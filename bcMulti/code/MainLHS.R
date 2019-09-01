
#Clean memory
rm(list=ls())

# Importing R packages
#library(Rmpi)
library(XML)
library(stringr)
library(logging)
library(yaml)
library(data.table)
library(lhs)
library(hydroGOF)

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
source('ParametersGeneratorLHS.R')
source(file.path(projects.common,'Measurements.R'))
source(file.path(projects.common,'MeasurementParser.R'))
source(file.path(projects.common,'Output.R'))
source(file.path(projects.common,'Ldndc.R'))
source(file.path(projects.common,'ProjectFile.R'))
source(file.path(projects.common,'Aggregator.R'))
logdebug(config$randomNumberFile)
source(config$randomNumberFile)
source('ModelUtils.R')
source('SlavesFeeder.R')
source('TasksFactory.R')

source('PosteriorLHS.R')

source('Task.R')

source('Database.R')
source('OutputDB.R')
source('MpiMasterLHS.R')

#source('GelmanRubin.R')
source('Validator.R')

folder <- config$path$common
file <- config$params$prior
priorDistribution <- read.table(file.path(folder,file), header=T)
parametersGenerator <- ParametersGenerator(priorParameterDistribution=priorDistribution)
  
Main <- setRefClass(
  "main"
  , fields = list(
    
  )
  , methods = list(
    #
    # Constructor
    #
    initialize = function(...
    ) {
      
      callSuper(...)
    },
    #
    #
    #    
    recovery = function() {
      return(!is.null(config$recovery) && config$recovery)
    },
    #
    # Check that all the configuration is correct
    #
    validate = function() {
      
      logdebug("METHOD IN: governance$validate")
      
      # validator <- Validator()
      
      #validator$climateCheck()
      #validator$measurements()
      #validator$manaEventsDates()
      #validator$siteFolderAndFiles()
      #validator$checkLresourcesLocalPath()
      
      #TODO: uncomment or remove
      #validator$ldndc()
      
      logdebug("METHOD OUT: governance$validate")
      
    },
    #
    # Get number of chain. And copy 
    #
    prepareCopyFolders = function() {
      
      #TODO:
      #Creo que lo mejor es modificar el input-path
      #Creo cuatro input-paths, uno para cada conf. Y creo dichas carpetas y copio ahi todo el contenido del model-input del site.
      #[input-path][chainnum]/arable/...
      
      input.path <- config$path$input_path
      
      
    },
    #
    # Input calibrations, due the folder structures, is only prepared for one chain calibration.
    #
    validateInputCalibration = function() {
      logdebug("METHOD IN: main$validateInputCalibration")
      
      if (!is.null(config$params$inputsprior)) {
        if (config$parallelThreads > 1) {
          logerror("Sorry, input calibrations is prepared only for one chain.")
          config$parallelThreads <<- 1
        }
      }
      
      logdebug("METHOD OUT: main$validateInputCalibration")
    },
    
    #
    # Parse and read the measurement files
    # Insert measurement in database
    # Return: measurements data
    #
    initializeMeasurements = function(out.db) {
      
      logdebug("METHOD IN: governance$initializeMeasurements")
      
      all.meas <- list()
      
      for (siteName in names(config$sites)) {
        logdebug(paste("Reading measurements for site",siteName))
        
        parser <- MeasurementParser(site=config$sites[[siteName]],
                                    unknownSD = config$meas$unknownSDValuePercent,
                                    fixSDValue = config$meas$fixSDValue,
                                    forceSDValue = config$meas$forceSDValue,
                                    years = config$meas$years)
        parser$modifyFiles()
        meas <- Measurements(site=config$sites[[siteName]], dataDaysRange = config$dataDaysRange)
        for (name in names(meas$values)) {
          logerror(paste("Measurement values", name))
          logerror(capture.output(head(meas$values[[name]])))
        }
        
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
            meas$proportionateByYear(probs.num=as.numeric(config$meas$mod[2]))
          }
        }
        
        #RECOVERY: not insert when recovery
        if (!recovery()) { 
          out.db$insertMeasurements(siteName, meas$values)
        }
        
        meas$aggregate()
        
        #RECOVERY: not insert when recovery
        # if (!recovery()) { 
        #    out.db$insertMeasurements(siteName, meas$values)
        #  }
        
        all.meas[siteName] <- meas
        
      }
      
      logdebug("METHOD OUT: governance$initializeMeasurements")
      return(all.meas)
    },
    
    #
    # Initialize the prior information for the parameters
    #
    initializePrior = function() {
      
      logdebug("METHOD IN: Main$initializePrior")
      
      folder <- config$path$common
      file <- config$params$prior
      priorProbabilityDistribution <- read.table(file.path(folder,file), header=T)
      
      logdebug("METHOD OUT: Main$initializePrior")
      
      return(priorProbabilityDistribution)
    },
    
    #
    # Initialize the parameter samples
    #
    initializeParameters = function(priorDistribution) {
      logdebug("METHOD IN: Main$initializeParameters")
      
      #parametersGenerator <- ParametersGenerator(priorParameterDistribution=priorDistribution)
      ini.paramenters <- list()
      
      for (i in 1:config$parallelThreads) {
        startingParameters <- parametersGenerator$getLHSParameters()
        ini.paramenters[[i]] <- startingParameters
        
      }
      
      logdebug("METHOD OUT: Main$initializeParameters")
      
      return(ini.paramenters)
    },
    #
    # Create the database file and its structure
    #
    initializeDatabase = function(parameters) {
      logdebug("METHOD IN: governance$initializeDatabase")
      
      db <- Database()
      
      #RECOVERY: 
      if (recovery()) {
        db$connect()
      } else {
        db$delete()
        db$connect()
        logdebug(parameters)
        db$create(parametersNames=rownames(parameters))        
      }
      
      
      logdebug("METHOD OUT: governance$initializeDatabase")
      
      return(db)
    },
    #
    # Create the output database file and its structure
    #
    initializeOutputDB = function(parameters) {
      logdebug("METHOD IN: governance$initializeOutputDB")
      
      out.db <- OutputDB()
      
      #RECOVERY: 
      if (is.null(config$recovery) || !config$recovery) { 
        out.db$delete()
        out.db$connect()
        out.db$create()
      } else {
        out.db$connect()
      }
      
      
      logdebug("METHOD OUT: governance$initializeOutputDB")
      
      return(out.db)
    },
    
    #
    # Create the tasks needed for the calibration
    #
    initializeTasks = function(parameters, meas,out.db) {
      
      logdebug("METHOD IN: governance$initializeTasks")
      
      #Factory creates tasks with chainStates and the model instances
      tasksFac <- TasksFactory()
      tasks <- tasksFac$generateTasks(lastParameters=parameters, meas, out.db)
      
      logdebug("METHOD OUT: governance$initializeTasks")
      
      return(tasks)
    },
    
    #
    # Insert the posterior information (used parameters and values) into the database
    #
    insertParameters = function(posterior, db) {
      
      logdebug(paste("Posterior result for chain ", posterior$chain, sep=""))
      logdebug(paste("Posterior lastValue: ",posterior$lastValue))
      
      logdebug("Candidate Parameters (the ones used in the last model execution)")
      logdebug(capture.output(posterior$candidateParameters))
      logdebug("Last Parameters (the ones that have been already selected)")
      logdebug(capture.output(posterior$lastParameters))
     
      db$insertParameters(posterior$chain, posterior$iteration, posterior$lastValue, posterior$lastParameters)
     
    },
    
    #
    # Initialize summaries in database
    #
    initializeSummariesInDatabase = function(db) {
      
      for (chain in 1:config$parallelThreads) {
        db$insertSummary(list(chain=chain, 
                              bestValue=0,
                              bestCount=0,
                              acceptedCount=0,
                              acceptanceRate=0,
                              iterations=0))
      }
    },
    #
    # Process all the bayesian calibration
    #
    calibrate = function() {
      # Initialize everything: logs, database, measurements, priors...
      
      priorDistribution <- initializePrior()
      
      #RECOVERY
      likeliValues <- c()
      startingParameters <- NULL
      iteration <- 0
      
      if (!recovery()) { 
        startingParameters <- initializeParameters(priorDistribution)
        db <- initializeDatabase(startingParameters[[1]])
        out.db <- initializeOutputDB()
        initializeSummariesInDatabase(db)
        db$insertConfiguration(config)
      } else {
        #RECOVERY:
        db <- initializeDatabase(NULL)
        out.db <- initializeOutputDB()
        iteration <- db$getMaxAcceptedIteration() 
        parameters.db.info <<- db$getParametersByIteration(iteration)
        iteration <- iteration + 1
        
        #Initialize parameters with prior to get correct names (in db there are not points)
        priorParameters <- initializeParameters(priorDistribution)
        startingParameters <- list()
        for (chain.num in parameters.db.info$chain) {
          params <- parameters.db.info[parameters.db.info$chain == chain.num, ]
          likeliValues[chain.num] <- params$likelihood
          startingParameters[[chain.num]] <- data.frame(values=as.vector(as.matrix(params[1,][-c(1:3)])))
          rownames(startingParameters[[chain.num]]) <- rownames(priorParameters[[chain.num]])#colnames(params[-c(1:3)])
        }
      }
      
      meas <- initializeMeasurements(out.db)
      tasks <- initializeTasks(startingParameters, meas)
      
      # Start Metropolis Algorithm
      
      results <- NULL
      
      feeder <- SlavesFeeder(measurements=meas)
      mpi <- MpiMaster(feeder=feeder, prior=priorDistribution, output.db=out.db)
      #mpi$iniPosteriors(startingParameters)
      #RECOVERY
      mpi$iniPosteriors(startingParameters, iteration=iteration, likeliValues = likeliValues) 
      mpi$open()
      parametersNames <- db$getParametersNames()

      
      while (iteration <= config$mcmc$gelmanAfterConvergenceLoops) {
        
        loginfo(paste("Iteration: ",iteration,sep=""))
        
        #WARNING: is very important to treat carefully the results data type.
        #Posteriors result is a list with four lists inside. Each one have: a dataframe call parameters and a number call likelihood
        posteriors <- mpi$executeParallelTasks(tasks)
        
        #Now we process the results Gelman Rubin process with likelihoods and parameters
        iteration <- iteration + 1
        
        loginfo(paste("Start posteriors processing for iteration ",iteration,sep=""))
        
        for (posterior in posteriors) {
          
          tryCatch({
            
            insertParameters(posterior, db)
            
            mpi$posteriors[[posterior$chain]] <- posterior
            
            mpi$posteriors[[posterior$chain]]$iteration <- iteration
            
            #gelmanRubin$setIteration(posterior$iteration)
            
            db$updateSummary(posterior$getSummary())
            
          }, error=function(e) { 
            logerror(paste("Error  with database during parameter and summary saving.", e))
            #Restart database connection
            db$close()
            db$connect()
          })
        }
        
        loginfo(paste("End results processing for iteration ",iteration,sep=""))
        
        
        loginfo("Memory info")
        loginfo(sum(apply(as.array(ls()),MARGIN=1,FUN=function(x) object.size(x))))
        
      }
      
      db$close()
      out.db$close()
      
      #Close mpi environment and detach Rmpi
      #mpi.exit()
      mpi$close()
      
    }
    
  ) #End methods
) #End Class

main <- Main()


main$validate()
main$calibrate()
