#Check likelihood directly.


#Clean memory
rm(list=ls())

# Importing R packages
library(XML)
library(stringr)
library(logging)
library(yaml)
library(ggplot2)
print("hallo")
args=(commandArgs(T))
config.file <- "config.yml"
if (length(args) == 0) {
  print("No arguments supplied. We use config.yml as default config file")
} else {
  config.file <- args[[1]]
  print(paste("Config file:",config.file))
}

config <- yaml.load_file(config.file)

priorProbabilityDistribution <- data.frame()

#TODO: use Sys.getenv
projects.common <- config$path$projectsCommon

#Importing sources
source(file.path(projects.common,'Logging.R'))
Logging(logPath=config$log$path,
        logFile=config$log$file,
        logLevel=config$log$level)$ini()  
source('ParametersGenerator.R')
source(file.path(projects.common,'Measurements.R'))
source(file.path(projects.common,'MeasurementParser.R'))
source(file.path(projects.common,'Output.R'))
source(file.path(projects.common,'Ldndc.R'))
source(file.path(projects.common,'ProjectFile.R'))
source(file.path(projects.common,'Aggregator.R'))

source(config$mcmc$likelihoodFile)
source('OutputDB.R')

# Make a morris parameter iterations to execute the model and calculate the likelihood for each.

copyInputFolder <- function(siteName, i) {
  
  logdebug("METHOD IN: tasksFactory$copyInputFolder")
  
  
  #TODO: change hardcoded 1,2
  siteFolders <- str_split(config$sites[[siteName]]$projectFile,"/")[[1]][c(1,2)]
  source.path <- file.path(config$sites[[siteName]]$inputPath, siteFolders[1], siteFolders[2])
  
  
  dest.path <- file.path(paste0(confInputPath,i),siteFolders[1])
  dir.create(dest.path, recursive=T)
  
  logdebug(paste("copy",source.path,"to",dest.path))
  file.copy(source.path, dest.path, recursive= TRUE)
  
  logdebug("METHOD OUT: tasksFactory$copyInputFolder")
}

plotData <- function(data.sets,  fileId, title) {
  
  for (compound in names(data.sets)) {
    data <- data.sets[[compound]]
    data$date <-  strptime(paste(data$year, data$julianday), "%Y %j")
    
    g <- ggplot(data=data, aes_string(x="date", y=names(data)[5])) + geom_line()
    g <- g + geom_point(data=data, aes_string(x="date", y=names(data)[3]), colour="red", shape=1)
    g <- g + ggtitle(paste(title,compound))
    filename <- paste0("testLik",fileId,compound,".png")
    ggsave(file.path(config$path$common,filename), width=15, height=5)
    
  }
}
#
# Create the output database file and its structure
#
initializeOutputDB <- function() {
  logdebug("METHOD IN: governance$initializeOutputDB")
  
  out.db <- OutputDB()
  out.db$delete()
  out.db$connect()
  out.db$create()
  
  
  logdebug("METHOD OUT: governance$initializeOutputDB")
  
  return(out.db)
}

initializePrior <- function() {
    
    logdebug("METHOD IN: Main$initializePrior")
    
    folder <- config$path$common
    file <- config$params$prior
    priorProbabilityDistribution <- read.table(file.path(folder,file), header=T)
    
    logdebug("METHOD OUT: Main$initializePrior")
    
    return(priorProbabilityDistribution)
}

#
#
# Return the parameter value 
#
#
denormalize <- function(name, norm.value) {
  logdebug("METHOD IN: denormalize")
  
  min <- priorProbabilityDistribution[priorProbabilityDistribution$name == name,]$min
  max <- priorProbabilityDistribution[priorProbabilityDistribution$name == name,]$max
  logdebug(name)
  logdebug(min)
  logdebug(max)
  logdebug(norm.value)
  
  value <- norm.value * (max - min) + min
  logdebug(value)
  
  logdebug("METHOD OUT: denormalize")
  
  return(value)
  
}

execute <- function(foldNum, siteName, input.path, output.sinksource, default.Values, title) {
  
  ldndc <- Ldndc(subfolder = as.character(foldNum),
                 site=config$sites[[siteName]],
                 commonPath = config$path$common,
                 confInputPath = input.path,
                 inputsPriorPath = config$params$inputsprior,
                 loglevel=config$log$level)
  ldndc$ini()
  
  output <- Output(commonPath=config$path$common,
                   subfolder = as.character(foldNum),
                   site=config$sites[[siteName]],
                   dataDaysRange=config$dataDaysRange,
                   output.sinksource = output.sinksource,
                   matchCols = config$match)
  
  
  ldndc$execute(default.Values)
  
  output$process(meas$getMeasurementsNames())
  
  aggr <- Aggregator(dataDaysRange=output$dataDaysRange, matchCols=output$matchCols, outputs=output$output, measurements=meas$values)
  logdebug("AGGREGATION")
  aggr.comb <- aggr$getCombination()
 
  
  #calculate likelihood with output and measurements
  lik <- Likelihood(data.sets=aggr.comb)
  
  newLikelihood <- lik$sumLikelihood()
  
  if (grepl("Default",title)) {
    out.db$createOutputTable(output$output)
  }
  out.db$writeOutput(0, 0, newLikelihood, output$output)
  
  plotData(aggr.comb, paste0(title,newLikelihood), paste("Lik",title,config$mcmc$likelihoodFile, newLikelihood))
  
  
}

out.db <- initializeOutputDB()
priorProbabilityDistribution <- initializePrior()
default.Values <- data.frame(priorProbabilityDistribution$init, row.names=priorProbabilityDistribution$name)

#Starts levels
l.levels <- c()
lev <- 8
for (i in c(1:lev-1)) {
  l.levels <- c(l.levels,i/(lev-1))
}
step.size <- lev/(2*(lev-1))

for (siteName in names(config$sites)) {
  
    parser <- MeasurementParser(site=config$sites[[siteName]],
                                unknownSD = config$meas$unknownSDValuePercent,
                                fixSDValue = config$meas$fixSDValue,
                                forceSDValue = config$meas$forceSDValue)
    parser$modifyFiles()
    meas <- Measurements(site=config$sites[[siteName]], dataDaysRange = config$dataDaysRange)
    meas$readMeasurements()
    meas$aggregate()
    if (config$meas$mod == 1) {
      meas$peaks()
    }
    out.db$insertMeasurements("0", meas$values)
    
    path <- file.path(config$sites[[siteName]]$inputPath, config$sites[[siteName]]$projectFile)
    output.sinksource <- ProjectFile(project.file=path)$getOutputSinksource()
    
    confInputPath <- config$sitesDefault$confInputPath
    foldNum <- 1
    copyInputFolder(siteName, foldNum)
    input.path <- paste0(confInputPath,foldNum)
    
    fail.params <- default.Values
    rownames(fail.params) <- paste0(rownames(fail.params),"FAIL")
    execute(foldNum, siteName, input.path, output.sinksource, fail.params, "0Default")
    
    for (pathNum in c(1:10)) {
      
      logdebug(paste("Start path",pathNum))
      
      rnd.levels <- c()
      for (i in c(1:nrow(priorProbabilityDistribution))) {
        index <- as.integer(runif(1, 1, length(l.levels)))
        rnd.levels <- c(rnd.levels, l.levels[index])
      }
      
      #for each param
      for (i in c(1:nrow(priorProbabilityDistribution))) {
        
        #move l.value step.size
        l.level <- rnd.levels[i]
        l.level <- l.level + step.size
        if (l.level > 1) {
          l.level <- l.level - step.size*2
        }
        
        param.name <- priorProbabilityDistribution$name[i]
        value <- denormalize(param.name, l.level)
        default.Values[as.character(param.name),] <- value
      
        tryCatch({
        
          execute(foldNum, siteName, input.path, output.sinksource, default.Values, paste0(pathNum,i))
          
        }, error = function(e) {
          logerror(paste("Error",e))
          logerror(default.Values)
        })
      }
    }
}




