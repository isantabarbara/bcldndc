
  
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
   # plots$plotParametersMultiBoxPlot()
  }, error = function(e) { 
    logerror(paste("plotParametersMultiBoxPlot",e))
  })

  tryCatch({
   # plots$plotParametersViolin()
  }, error = function(e) { 
    logerror(paste("plotParametersViolin",e))
  })
  
  
  tryCatch({
  #  plots$plotParametersBoxPlot()
  }, error = function(e) { 
    logerror(paste("plotParametersBoxPlot",e))
  })
  
  tryCatch({
    plots$plotAcceptedParametersSampleSpace()
  }, error = function(e) { 
    logerror(paste("plotAcceptedParametersSampleSpace",e))
  })
  
  tryCatch({
    plots$plotFinalParametersHistogram()
  }, error = function(e) { 
    logerror(paste("plotFinalParametersHistogram",e))
  })
  
  tryCatch({
    #plots$plotSampledParametersHistogram()
  }, error = function(e) { 
    logerror(paste("plotSubsetParametersHistogram",e))
  })
  
  tryCatch({
    #plots$plotSubsetParametersHistogram()
  }, error = function(e) { 
    logerror(paste("plotSubsetParametersHistogram",e))
  })
  
  tryCatch({
  #  plots$plotParametersLevelPlotCorrelation()
  }, error = function(e) { 
    logerror(paste("plotParametersLevelPlotCorrelation",e))
  })
  

  
  tryCatch({
  #  plots$plotBestParameterLines()
  }, error = function(e) { 
    logerror(paste("plotParametersLevelPlotCorrelation",e))
  })

   
  tryCatch({
    out.db <- OutputDB()
    out.db$connect()
    avg.lik <- round(as.numeric(out.db$selectMeanLikelihood()),2)
    Q75.lik <- round(as.numeric(out.db$selectQLikelihood(0.75)),2)
    Q95.lik <- round(as.numeric(out.db$selectQLikelihood(0.95)),2)
    
    
  #  plots$plotOutputDBUncertainties(out.db,filter=paste0(avg.lik,""),title="AVG")
  #  plots$plotOutputDBUncertainties(out.db,filter=paste0(Q75.lik,""),title="Q75")
  #  plots$plotOutputDBUncertainties(out.db,filter=paste0(Q95.lik,""),title="Q95")
  #  plots$plotOutputDBUncertainties(out.db,filter="0",title="ALL")

    out.db$close()
    
  }, error = function(e) { 
    logerror(paste("plotOutputDBUncertainties",e))
  })

 
  tryCatch({
    out.db <- OutputDB()
    out.db$connect()
    plots$plotOutputDB(out.db)
    out.db$close()
    
  }, error = function(e) { 
    logerror(paste("plotOutputDB",e))
  })
  
  tryCatch({
    out.db <- OutputDB()
    out.db$connect()
    #Trzing to identify parameters that reach some missing points and destroy the rest (I have to use likelihood as outputs do not have iteration value :S)
    #Hmetrx peak: iterations <- out.db$selectIterationsWithLikelihoodDetails("n_n2o_2002_NMAEMeasPeaks", 0.1)
 #   likelihoods <- out.db$selectIterationsWithLikelihoodDetails("n_n2o_2007_NMAEMeasPeaks", 0.01)
 #   logerror(paste("FILTERED ITERATIONS:",length(unique(likelihoods))))
 #   plots$plotOutputDBUncertaintiesByIterations(out.db,likelihoods,"n_n2o_2007_NMAEMeasPeaks")    
    
      
    out.db$close()
    
  }, error = function(e) { 
    logerror(paste("plotOutputDBUncertaintiesByIterations",e))
  })
  
 
  db$close()

