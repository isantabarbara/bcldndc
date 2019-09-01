#Check likelihood directly.


#Clean memory
rm(list=ls())

# Importing R packages
library(XML)
library(stringr)
library(logging)
library(yaml)
library(ggplot2)

args=(commandArgs(T))
config.file <- "config.yml"
if (length(args) == 0) {
  print("No arguments supplied. We use config.yml as default config file")
} else {
  config.file <- args[[1]]
  print(paste("Config file:",config.file))
}

config <- yaml.load_file(config.file)

#TODO: use Sys.getenv
projects.common <- config$path$projectsCommon

#Importing sources
source(file.path(projects.common,'Logging.R'))
Logging(logPath=config$log$path,
        logFile=config$log$file,
        logLevel=config$log$level)$ini()  

source(file.path(projects.common,'Measurements.R'))
source(file.path(projects.common,'MeasurementParser.R'))
source(file.path(projects.common,'Output.R'))



source(file.path(projects.common,'Aggregator.R'))

source(config$mcmc$likelihoodFile)
source('OutputDB.R')

plotData <- function(data.sets, outputs, fileId, title) {
  
  for (compound in names(data.sets)) {
    data <- data.sets[[compound]]
    data$date <-  strptime(paste(data$year, data$julianday), "%Y %j")
    
    output.name <- config$match[[compound]][[1]]
    output.values <- outputs[,c("year","julianday",output.name)]
    output.values$date <-  strptime(paste(output.values$year, output.values$julianday), "%Y %j")
    
    g <- ggplot(data=output.values, aes_string(x="date", y=names(data)[5])) + geom_line()
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
  out.db$connect()
  
  logdebug("METHOD OUT: governance$initializeOutputDB")
  
  return(out.db)
}


execute <- function(output, title) {
  
  aggr <- Aggregator(dataDaysRange=config$dataDaysRange, matchCols=config$match, outputs=output, measurements=meas$values)
  logdebug("AGGREGATION")
  aggr.comb <- aggr$getCombination()
  
  
  #calculate likelihood with output and measurements
  lik <- Likelihood(data.sets=aggr.comb)
  newLikelihood <- 0
  logerror(paste("bevor NRMSE fuera:", newLikelihood, sep=""))
  newLikelihood <- lik$sumLikelihood()
  logerror(paste("Mean NRMSE fuera:", newLikelihood, sep=""))
 
  
  
  plotData(aggr.comb, output, paste0(title,newLikelihood), paste("Lik",title,config$mcmc$likelihoodFile, newLikelihood))
  
}

out.db <- initializeOutputDB()


for (siteName in names(config$sites)) {
  
  parser <- MeasurementParser(site=config$sites[[siteName]],
                              unknownSD = config$meas$unknownSDValuePercent,
                              fixSDValue = config$meas$fixSDValue,
                              forceSDValue = config$meas$forceSDValue)
  parser$modifyFiles()
  meas <- Measurements(site=config$sites[[siteName]], dataDaysRange = config$dataDaysRange)
  meas$readMeasurements()
  meas$aggregate()
  meas$peaks()
  
  
  likelihoods <- out.db$selectLikelihood()

  for (likeli in likelihoods[,1]) {
    
    logerror(paste("likelihood", likeli))
    output <- out.db$selectOutputByLikelihood(likeli)
    execute(output, paste0("oldLik",likeli,"_"))
    
  }
  
 
}



