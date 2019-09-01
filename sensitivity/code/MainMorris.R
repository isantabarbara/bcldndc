
#Clean memory
rm(list=ls())

# Importing R packages
library(XML)
library(yaml)
library(data.table)
library(stringr)
require("RSQLite")
library(ggplot2)
tryCatch({
args=(commandArgs(T))
config.file <- "config.yml"
if (length(args) == 0) {
	print("No arguments supplied")
} else {
	config.file <- args[[1]]
	print(paste("Config file:",config.file))
}

config <- yaml.load_file(config.file)

projects.common <- config$path$projectsCommon #"~/projects/common/"
#Importing sources
source(file.path(projects.common,'Logging.R'))
Logging(logPath=config$log$path,
        logFile=config$log$file,
        logLevel=config$log$level)$ini()  

source(file.path(projects.common,'Measurements.R'))
source(file.path(projects.common,'MeasurementParser.R'))
source(file.path(projects.common,'Output.R'))
source(file.path(projects.common,'Ldndc.R'))
source(file.path(projects.common,'ProjectFile.R'))
source(file.path(projects.common,'Aggregator.R'))

#Importing sources
source('PriorReader.R')
source('SensitivityIndexAnalisys.R')
#source(config$morris$scoreFile)
source('ScoreD.R')
source('ScoreDTW.R')
source('ScoreLik.R')
source('ScoreNSE.R')
source('ScoreR2.R')
source('ScoreRMSE.R')


#We analyse site by site
siteName <- names(config$sites)[1]

#
#
# Secure function to send sqlQuerys with error handling
#
#
sqlQuery <- function(query,...)
{
  
  logdebug("METHOD IN: sqlQuery")
  loginfo(paste("Query: ", query))
  
  tryCatch(
    
    return(dbSendQuery(conn, query))
    , error=function(e) { 
      print(paste("Caught RSQLite Error: dbSendQuery() Failed with query", query))
      stop(e)
    })
  
  logdebug("METHOD OUT: sqlQuery")
}

#
# Check if the morris algorithm include configuration for measurements
#
includeMeasurementScore <- function() {
  return((!is.null(config$meas)))
}

initializeMeasurements <- function() {
  
  logdebug("METHOD IN: initializeMeasurements")
  
  all.meas <- list()
  
  for (siteName in names(config$sites)) {
    logdebug(paste("Reading measurements for site",siteName))
    
    parser <- MeasurementParser(site=config$sites[[siteName]],
                                unknownSD = config$meas$unknownSDValuePercent,
                                fixSDValue = config$meas$fixSDValue,
                                forceSDValue = config$meas$forceSDValue)
    
    parser$modifyFiles()
    meas <- Measurements(site=config$sites[[siteName]])
    
    meas$readMeasurements()
    
    if (!is.null(config$meas$years)) {
      meas$removeYears(config$meas$years[1],config$meas$years[2])
    }
    
    #TODO: create specific files for measurement filter
    if (!is.null(config$meas$mod)) {
    if (config$meas$mod[1] == 1) {
      meas$peaks(probs.num=as.numeric(config$meas$mod[2]))
    } else if (config$meas$mod[1] == 2) {
      meas$discretize(probs.num=as.numeric(config$meas$mod[2]))
    } else if (config$meas$mod[1] == 3) {
      meas$highValues(probs.num=as.numeric(config$meas$mod[2]))
    } else if (config$meas$mod[1] == 4) {
      meas$proportionate(probs.num=as.numeric(config$meas$mod[2]))
    }
    }
    
    meas$aggregate()
        
    all.meas[siteName] <- meas
    
  }
  
  logdebug("METHOD OUT: initializeMeasurements")
  return(all.meas)
}

insert.quantile <- function(path, run, col, D, DTW, Lik, NSE, R2, RMSE, quan) {
  
  logdebug("METHOD IN: insert.quantile")
  
  min <- quan[1]
  Q1 <- quan[2]
  Q2 <- quan[3]
  Q3 <- quan[4]
  max <- quan[5]
  

  if (is.na(Lik) | is.infinite(Lik)) {
    Lik <- -99
  }
  if (is.na(NSE) | is.infinite(NSE)) {
    NSE <- -99
  }
  if (is.na(DTW) | is.infinite(DTW)) {
    DTW <- -99
  }
  if (is.na(D) | is.infinite(D)) {
    D <- -99
  }
  if (is.na(R2) | is.infinite(R2)) {
    R2 <- -99
  }
  sqlQuery(paste0("INSERT INTO sensitivity VALUES ('", col, "',", path, ",", run, ",", D, ",", DTW, ",", Lik, ",", NSE, ",", R2, ",", RMSE, ",", min, ",", Q1, ",", Q2, ",", Q3, ",", max,");"))
  
  
  logdebug("METHOD OUT: insert.quantile")
}

if (includeMeasurementScore()) {
  measurements <- initializeMeasurements()[[1]]
}

logdebug("starts levels")
#Starts levels
l.levels <- c()
lev <- config$morris$levels
for (i in c(1:lev-1)) {
  l.levels <- c(l.levels,i/(lev-1))
}

file <- paste0("morris",siteName,".db")
dbFile <- file.path(config$path$common,file)

if (!config$morris$onlyPlots) {
  unlink(dbFile, recursive = F, force = T)
}


logdebug("connect db")
conn  <- dbConnect(dbDriver("SQLite"), dbname = dbFile)
logdebug("connected db")

if (!config$morris$onlyPlots) {
  
  logdebug("Create db table")
tryCatch({
  dbSendQuery(conn, "CREATE TABLE Sensitivity (col TEXT, path INTEGER, run INTEGER, scoreD FLOAT, scoreDTW FLOAT, scoreLik FLOAT, scoreNSE FLOAT, scoreR2 FLOAT, scoreRMSE FLOAT, min FLOAT, Q1 FLOAT, Q2 FLOAT, Q3 FLOAT, max FLOAT);")
},
  error= function(err) {
    logerror(err)
    logerror("Error creating tables. They probably exists")
})

prior <- PriorReader()
priorProbabilityDistribution <- prior$priorProbabilityDistribution()
priorProbabilityDistribution <- priorProbabilityDistribution[!priorProbabilityDistribution$name == "site.parameter.LIQDON.value",]
priorProbabilityDistribution <- priorProbabilityDistribution[!priorProbabilityDistribution$name == "site.parameter.LIQNH4.value",]
priorProbabilityDistribution <- priorProbabilityDistribution[!priorProbabilityDistribution$name == "site.parameter.LIQNO3.value",]
priorProbabilityDistribution <- priorProbabilityDistribution[!priorProbabilityDistribution$name == "site.parameter.LIQUREA.value",]

output.subfolder <- "outtemp"
dir.create(file.path(config$path$common,output.subfolder))

areInputs <- function() {
  areInputs <- (!is.null(config$ldndc$inputsprior))
  loginfo(paste("INPUTS INCLUDED?", areInputs))
  return(areInputs)
}

#
# Prepare the config_inputs path for ldndc class
#
getConfigXpathsInputs <- function() {
  logdebug("METHOD IN: Main$getConfigXpathsInputs")
  
  path <- ""
  
  if (areInputs()) {
    path <- config$ldndc$inputsprior
  }
  
  logdebug("METHOD OUT: Main$getConfigXpathsInputs")
  
  return(path)
}



for (pathNum in c(1:config$morris$pathNumber)) {
  
  loginfo(paste("Start path",pathNum))
  #Get random value from l.levels for each parameter
  rnd.levels <- c()
  for (i in c(1:nrow(priorProbabilityDistribution))) {
    index <- as.integer(runif(1, 1, length(l.levels)))
    rnd.levels <- c(rnd.levels, l.levels[index])
    #logdebug("Rnd levels")
    #logdebug(rnd.levels)
  }
  
  loginfo("start Ldndc")
  path <- file.path(config$sites[[siteName]]$inputPath, config$sites[[siteName]]$projectFile)
  output.sinksource <- ProjectFile(project.file=path)$getOutputSinksource()
  
  config.xpaths.inputs <- getConfigXpathsInputs()
  ldndc <- Ldndc(subfolder = output.subfolder,
              site=config$sites[[siteName]],
              confInputPath=config$sites[[siteName]]$inputPath,
              commonPath = config$path$common,
              loglevel=config$log$level,
              inputsPriorPath=config.xpaths.inputs)
              
  ldndc$ini()
  
  #Get the paremeters for this norm.values
  default.Values <- data.frame(priorProbabilityDistribution$init, row.names=priorProbabilityDistribution$name)
  #logdebug("INIT Param Values")
  #logdebug(capture.output(default.Values))
  
  for (i in c(1:nrow(priorProbabilityDistribution))) {
    l.level <- rnd.levels[i]
    param.name <- priorProbabilityDistribution$name[i]
    #logdebug(paste("oldValue",priorProbabilityDistribution$init[i]))
    
    value <- prior$denormalize(param.name, l.level)
    #logdebug(paste("new Value",value))
    
    
    #logdebug(paste("1df pre value",default.Values[as.character(param.name),]))
    default.Values[as.character(param.name),] <- value
    #logdebug(paste("1df post value",default.Values[as.character(param.name),]))
  }
  
 #logdebug("MOD Param Values")
#  logdebug(capture.output(default.Values))
  
  #Run simulation
  tryCatch({
    
    ldndc$execute(default.Values)
    output <- Output(commonPath=config$path$common,
                     subfolder = output.subfolder,
                     site=config$sites[[siteName]],
                     output.sinksource = output.sinksource,
                     matchCols = config$match)
    
    compounds <- names(config$match)
    output$process(compounds)
    
    if (includeMeasurementScore()) {
      aggr <- Aggregator(dataDaysRange=output$dataDaysRange, matchCols=output$matchCols, outputs=output$output, measurements=measurements$values)
      aggr.comb <- aggr$getCombination()
      scoreD <- ScoreD(data.sets=aggr.comb)
      D.values <- scoreD$score()
      scoreDTW <- ScoreDTW(data.sets=aggr.comb)
      DTW.values <- scoreDTW$score()
      scoreLik <- ScoreLik(data.sets=aggr.comb)
      Lik.values <- scoreLik$score()
      scoreNSE <- ScoreNSE(data.sets=aggr.comb)
      NSE.values <- scoreNSE$score()
      scoreR2 <- ScoreR2(data.sets=aggr.comb)
      R2.values <- scoreR2$score()
      scoreRMSE <- ScoreRMSE(data.sets=aggr.comb)
      RMSE.values <- scoreRMSE$score()
    }
   
    output.1 <- output$output
    for (compound in names(config$match)) {
      col <- config$match[[compound]][[1]]
      score.val <- 1
      
      if (includeMeasurementScore()) {
        D.val <- D.values[[compound]]
        DTW.val <- DTW.values[[compound]]
        Lik.val <- Lik.values[[compound]]
        NSE.val <- NSE.values[[compound]]
        R2.val <- R2.values[[compound]]
        RMSE.val <- 0#RMSE.values[[compound]]
      }
      
      insert.quantile(pathNum, 0, col, D.val, DTW.val, Lik.val, NSE.val, R2.val, RMSE.val, quantile(output.1[col][,1]))
      
    }
    loginfo(paste("Correct execution model for init/default random Parameters set"))
  },
  error= function(err) {
    logerror(err)
    logerror(paste("Error executing model for init random Parameters set"))

  })

  step.size <- lev/(2*(lev-1))
  
  #for each param
  for (i in c(1:nrow(priorProbabilityDistribution))) {
    
    #move l.value step.size
    l.level <- rnd.levels[i]
    l.level <- l.level + step.size
    if (l.level > 1) {
      l.level <- l.level - step.size*2
    }
    
    param.name <- priorProbabilityDistribution$name[i]
    value <- prior$denormalize(param.name, l.level)
    default.Values[as.character(param.name),] <- value
    
    #Set new param value and Run simulation
    tryCatch({
    #  logdebug("New default.Values")
     # logdebug(capture.output(default.Values))
      ldndc$execute(default.Values)
      
      output <- Output(commonPath=config$path$common,
                       subfolder = output.subfolder,
                       site=config$sites[[siteName]],
                       output.sinksource = output.sinksource,
                       matchCols = config$match)
      
      compounds <- names(config$match)
      output$process(compounds)
      
      if (includeMeasurementScore()) {
        aggr <- Aggregator(dataDaysRange=output$dataDaysRange, matchCols=output$matchCols, outputs=output$output, measurements=measurements$values)
        #logdebug("AGGREGATION")
        aggr.comb <- aggr$getCombination()
        scoreD <- ScoreD(data.sets=aggr.comb)
        D.values <- scoreD$score()
        scoreDTW <- ScoreDTW(data.sets=aggr.comb)
        DTW.values <- scoreDTW$score()
        scoreLik <- ScoreLik(data.sets=aggr.comb)
        Lik.values <- scoreLik$score()
        scoreNSE <- ScoreNSE(data.sets=aggr.comb)
        NSE.values <- scoreNSE$score()
        scoreR2 <- ScoreR2(data.sets=aggr.comb)
        R2.values <- scoreR2$score()
        scoreRMSE <- ScoreRMSE(data.sets=aggr.comb)
        RMSE.values <- scoreRMSE$score()
      }
      
      output.2 <- output$output

      for (compound in names(config$match)) {
        col <- config$match[[compound]][[1]]
        
        score.val <- 1
        
        if (includeMeasurementScore()) {
          D.val <- D.values[[compound]]
          DTW.val <- DTW.values[[compound]]
          Lik.val <- Lik.values[[compound]]
          NSE.val <- NSE.values[[compound]]
          R2.val <- R2.values[[compound]]
          RMSE.val <- 0#RMSE.values[[compound]]
          
        }
        
        insert.quantile(pathNum, i, col, D.val, DTW.val, Lik.val, NSE.val, R2.val, RMSE.val, quantile(output.2[col][,1]))
      }
      
      loginfo(paste("Correct execution model for Parameter",param.name,"with value:",value))
    },
    error = function(err) {
      logerror(err)
      logerror(paste("Error executing model for Parameter",param.name,"with value:",value))
    })
  
  }

}

}

# Standarize the quantiles with global max and min to all are betwen 0 and 1.
# Average all the quantiles to get the elementary difference from there.
#

#comps <- c("dN_n2o_emis.kgNha.1.","dN_no_emis.kgNha.1.","dN_nh4_leach.kgNha.1.","dN_no3_leach.kgNha.1.","temp_10cm.oC.","soilwater_10cm...","dC_co2_emis_auto.kgCha.1.")#"DW_above.kgDWm.2.",
#comps <- c("N_nh4.kgNha.1.","N_no3.kgNha.1.")
for (compound in config$match) {
#for (col in comps) {
  col <- compound[[1]]
  
  sqlCmd <- paste0("SELECT path, run, min, Q1, Q2, Q3, max, scoreD, scoreDTW, scoreLik, scoreNSE, scoreR2, scoreRMSE  FROM sensitivity WHERE col = '", col, "';")
  logdebug(sqlCmd)
  elements <- dbGetQuery(conn, sqlCmd)
 # logdebug(capture.output(elements))
  
  #normalize all the values between 0 and 1
  standarize <- function(vect) {
    Max <- max(vect)
    Min <- min(vect)
    if (Max-Min != 0) {
      return(((vect - Min)/(Max - Min)))
    } else {
      return(rep(0,length(vect)))
    }
  }
  
  #We can play here with different options: give more or less weight to different statistics
  elements$min <- standarize(elements$min) #Index 3 (path and run are 1 and 2)
  
  elements$Q1 <- standarize(elements$Q1) #Index 4
  elements$Q2 <- standarize(elements$Q2)
  elements$Q3 <- standarize(elements$Q3)
  elements$max <- standarize(elements$max) #Index 7
  elements$scoreD <- standarize(elements$scoreD) #Index 8
  elements$scoreDTW <- standarize(elements$scoreDTW) #Index 9
  elements$scoreLik <- standarize(elements$scoreLik) #Index 10
  elements$scoreNSE <- standarize(elements$scoreNSE) #Index 11
  elements$scoreR2 <- standarize(elements$scoreR2) #Index 12
  elements$scoreRMSE <- standarize(elements$scoreRMSE) #Index 13
  
  elem.idx <- list(c(4:6), c(3:7), c(8), c(9), c(10), c(11), c(12), c(13))
  
  for (idx in elem.idx) {
    logdebug("elem.idx:")
    logdebug(idx)
    logdebug(capture.output(elements))
  elements$y <- apply(elements[idx], 1, mean)
  elements$d <- apply(elements[idx], 1, mean)
  
  #TODO: doubt what to do with run == 0
  elements[elements$run  == 0,]$d <- 0
  error.pathrun <- list()
  for (path in 1:max(elements$path)) {
    for (run in 1:max(elements$run)) {
      tryCatch(
          
          elements[elements$path == path & elements$run == run,]$d  <- elements[elements$path == path & elements$run == run,]$y - elements[elements$path == path & elements$run == (run-1),]$y
        , error=function(e) { 
          #logerror(e)
         # logerror(paste("Path and run:",path, run, "cannot be calculated."))
          error.pathrun <<- append(error.pathrun,list(c(path,run)))
        })
      
    }
  }
  
  for (path.run in error.pathrun) {
    logerror(paste("Removing elements without correct d. Path and run:",path.run[1], path.run[2]))
    elements <- elements[!(elements$path == path.run[1] & elements$run == path.run[2]),]
  }
  logdebug("Start plotting")
  plotData <- data.frame(param=c(1:max(elements$run)),u=rep(0,max(elements$run)),teta=rep(0,max(elements$run)))
  max.path <- max(elements$path)
  for (run in 1:max(elements$run)) {

    number.of.runs <- nrow(elements[elements$run == run,])
    if (number.of.runs == 0) {
      logerror(paste("There are no elements to include RUN: ", run))
    }
    if (number.of.runs < max.path) {
      logerror(paste("Some executions of run", run,"broke the model. Number of runs - MaxPath:", number.of.runs, "-", max.path))
    }
    plotData[plotData$param == run,]$u <- mean(abs(elements[elements$run == run,]$d))
    plotData[plotData$param == run,]$teta <- sd(elements[elements$run == run,]$d)
  }
  
  logdebug("Saving csv")
  idx.str <- paste0(idx,collapse="")
  write.csv2(plotData, file = file.path(config$path$common,paste0(siteName,col,idx.str,".csv")), quote=F, row.names=F, col.names=T)
  plt <- ggplot(plotData, aes(u,teta,label=as.character(as.factor(param)))) + geom_point() + geom_text(aes(label=as.character(as.factor(param))),hjust=0, vjust=0)
  ggsave(filename = file.path(config$path$common,paste0(siteName,col,idx.str,".png")), plot = plt, width = 10, dpi = 100)
  
  }
}
},
error= function(err) {
  logerror(err)
  logerror("Error unk")
})


