#Check likelihood directly.


#Clean memory
rm(list=ls())

# Importing R packages
library(XML)
library(stringr)
library(logging)
library(yaml)


config = yaml.load_file("configPruebas.yml")

#Importing sources
source('Logging.R')
source('ParametersGenerator.R')
source('Measurements.R')
source('MeasurementParser.R')
source('ModelUtils.R')
source('Output.R')
source('SlavesFeeder.R')
source('TasksFactory.R')
source('Ldndc.R')
source('LikelihoodWithCum.R')
source('Posterior.R')
source('Task.R')
source('Database.R')
source('OutputDB.R')
source('MpiMaster.R')
source('GelmanRubin.R')
source('Validator.R')

Logging()$ini()  

all.meas <- list()

for (siteName in names(config$sites)) {
  #Meas file
  logdebug(paste("Reading measurements for site",siteName))
  meas <- Measurements(siteName=siteName)
  #TODO: check that it works
  first.year <- config$sites[[siteName]]$years[1]
  number.years <- config$sites[[siteName]]$years[2]
  years.serie <-  seq(from=first.year,to=first.year+number.years-1)
  meas$readMeasurements(years.serie)
  logdebug("Measurement values agg")
  logdebug(head(meas$valuesAggregated))
  all.meas[siteName] <- meas


  #Output file
  projectPath <- ""#file.path("chain1",siteName)
  output <- Output(projectPath=projectPath,config$sites[[siteName]])
  
  output$process(meas$getMeasurementsNames())
 
 
  
  #calculate likelihood with output and measurements
  lik <- Likelihood(outputs=output$outputAggregated, measurements=meas$valuesAggregated)
  
  newLikelihood <- lik$sumLikelihood()
  
  loginfo(paste("newLikelihood", newLikelihood))
  #QUES: why we do not have the prior in the posterior
  #QUES ANSW: I think that because we use uniform distribution. So the propability of the selected parameters
  #QUES ANSW: is always the same between min and max values and out there is 0.
  
  logdebug(capture.output(output$outputAggregated))
  logdebug("MEAS")
  logdebug(capture.output(meas$valuesAggregated))
  
  loginfo(paste("newLikelihood", newLikelihood))
}


