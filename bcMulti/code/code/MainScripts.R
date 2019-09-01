

#Clean memory
rm(list=ls())

library(stringr)
library(logging)
library(ggplot2)
library(Hmisc)
library(reshape)
#Importing sources
library(yaml)

args=(commandArgs(T))
config.file <- "config.yml"
if (length(args) == 0) {
  print("No arguments supplied. We use config.yml as default config file")
} else {
  config.file <- args[[1]]
  print(paste("Config file:",config.file))
}

config <- yaml.load_file(config.file)

projects.common <- config$path$projectsCommon
#Importing sources
source(file.path(projects.common,'Logging.R'))
Logging(logPath=config$log$path,
        logFile=config$log$file,
        logLevel=config$log$level)$ini()  

source('ModelUtils.R')
source('Database.R')
source('PlotTool.R')
source('OutputDB.R')

db <- Database()
db$connect()

# Initialize the prior information for the parameters
logdebug("initializePrior")
folder <- config$path$common
file <- config$params$prior
priorProbabilityDistribution <- read.table(file.path(folder,file), header=T)

plots <- PlotTool(prior=priorProbabilityDistribution)

tryCatch({
  plots$scriptPosteriorsSD()
}, error = function(e) { 
  logerror(paste("plotParametersBoxPlot",e))
})

tryCatch({
  #plots$plotAcceptedParametersSampleSpace()
}, error = function(e) { 
  logerror(paste("plotAcceptedParametersSampleSpace",e))
})

tryCatch({
  #plots$plotFinalParametersHistogram()
}, error = function(e) { 
  logerror(paste("plotFinalParametersHistogram",e))
})

tryCatch({
  #plots$plotSampledParametersHistogram()
}, error = function(e) { 
  logerror(paste("plotSubsetParametersHistogram",e))
})

tryCatch({
  # plots$plotSubsetParametersHistogram()
}, error = function(e) { 
  logerror(paste("plotFinalParametersHistogram",e))
})

tryCatch({
  #plots$plotParametersLevelPlotCorrelation()
}, error = function(e) { 
  logerror(paste("plotParametersLevelPlotCorrelation",e))
})



tryCatch({
  #   plots$plotBestParameterLines()
}, error = function(e) { 
  logerror(paste("plotParametersLevelPlotCorrelation",e))
})

tryCatch({
  out.db <- OutputDB()
  out.db$connect()
  #plots$plotOutputDBUncertainties(out.db)
  out.db$close()
  
}, error = function(e) { 
  logerror(paste("plotAcceptedParametersSampleSpace",e))
})

tryCatch({
  out.db <- OutputDB()
  out.db$connect()
  # plots$plotOutputDB(out.db)
  out.db$close()
  
}, error = function(e) { 
  logerror(paste("plotAcceptedParametersSampleSpace",e))
})

db$close()
