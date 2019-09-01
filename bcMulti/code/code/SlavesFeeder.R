# Responsability
# Send to the slaves all the files, functions and data they need for execute any existed task

SlavesFeeder <- setRefClass(    
  "slavesfeeder"
  , fields = list(
      measurements="list"
      )
  , methods = list(
    #
    # Constructor
    #
    initialize = function(..., measurements=list()) { 

      callSuper(..., measurements = measurements)
    },
    #
    # Send objects to the slaves
    #
    feedSlaves = function() {

      logdebug("METHOD IN: feedSlaves")
      
      mpi.bcast.Robj2slave(config)
      mpi.bcast.Robj2slave(measurements)
      
      logdebug("METHOD OUT: feedSlaves")
      
    },

    whipSlaves = function() {
      #
      # Execute code in the slaves
      # Call the function in all the slaves to get them ready to
      # undertake tasks
      #
      logdebug("METHOD IN: executeSlaves")
      
      mpi.bcast.cmd(library(logging))
      mpi.bcast.cmd(library(stats))
      mpi.bcast.cmd(library(XML))
      mpi.bcast.cmd(library(data.table))
      projects.common <- config$path$projectsCommon
      mpi.bcast.cmd(source(file.path(projects.common,'Logging.R')))
      mpi.bcast.cmd(source(file.path(projects.common,'Measurements.R')))
      mpi.bcast.cmd(source(file.path(projects.common,'Output.R')))
      mpi.bcast.cmd(source(file.path(projects.common,'Aggregator.R')))
      mpi.bcast.cmd(source("PlotOutputAndMeasurements.R"))
      mpi.bcast.cmd(source("Slave.R"))
      
      mpi.bcast.cmd(source(file.path(projects.common,'Ldndc.R')))
      mpi.bcast.cmd(source(config$mcmc$likelihoodFile))
      mpi.bcast.cmd(source('Task.R'))

      mpi.bcast.cmd(Logging(logPath=config$log$path,
                            logFile=config$log$file,
                            logLevel=config$log$level)$ini())
      mpi.bcast.cmd(Slave()$foldslave())
      
      logdebug("METHOD OUT: executeSlaves")
    }
    
  )#End methods List 
  
)#End RefClass