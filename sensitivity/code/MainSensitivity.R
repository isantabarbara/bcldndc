
#Clean memory
rm(list=ls())

# Importing R packages
library(XML)
library(yaml)
library(data.table)
library(stringr)
library(logging)

args=(commandArgs(T))
config.file <- "config.yml"
if (length(args) == 0) {
	print("No arguments supplied")
} else {
	config.file <- args[[1]]
	print(paste("Config file:",config.file))
}

config <- yaml.load_file(config.file)  

#Importing sources

projects.common <- config$path$projectsCommon #"~/projects/common/"
#Importing sources
source(file.path(projects.common,'Logging.R'))
Logging(logPath=config$log$path,
        logFile=config$log$file,
        logLevel=config$log$level)$ini()  

source(file.path(projects.common,'Output.R'))
source(file.path(projects.common,'Ldndc.R'))
source(file.path(projects.common,'ProjectFile.R'))

source('PriorReader.R')
source('SensitivityIndexAnalisys.R')






#TODO: workaround
#site.names <- c("hoeglwald","tharand","wetzstein")
#site.names <- c("collelongo","hesse","soroe")
#
site.names <- c("brasschaat","hyytiaela","loobos","sodanklya")

prior <- PriorReader()
priorProbabilityDistribution <- prior$priorProbabilityDistribution()

for (site.name in site.names) {
  
  #TODO: ugly workaround
  config$sitename <- site.name
  
  project.file <- ProjectFile()$search(file.path(config$ldndc$folder, config$sitename))
  ProjectFile()$cleanPrefix(project.file)
  
  model=Ldndc(projectFile=project.file)
  
  model$createLdndcConf()
  
  sensitivity = SensitivityIndexAnalisys(priorProbabilityDistribution=priorProbabilityDistribution,model=model)
  
  sensitivity$executeModelChangingParameters()
  
  sortedData <- sensitivity$sortParametersSensitivity()
  logdebug(capture.output(sortedData))
  comp <- paste0(config$compounds,collapse='' )
  write.table(sortedData, file=file.path(config$path$common, paste0(config$sitename,comp,config$path$sensitivityFile)), col.names=F, sep=";")


}
