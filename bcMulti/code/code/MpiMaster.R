# Responsability
# Control the slaves creation, destruction and execute tasks in them

MpiMaster <- setRefClass(    
  "mpimaster"
  
  , fields = list(
      feeder="slavesfeeder",
      posteriors="list",
      prior = "data.frame",
      output.db = "outputdb"
    )
  , methods = list(
    #
    # Constructor
    #
    initialize = function(..., feeder=NULL, 
                          posteriors=list(),
                          prior = data.frame(),
                          output.db = NULL) {
      
      callSuper(..., feeder=feeder, 
                posteriors=posteriors,
                prior = prior,
                output.db = output.db)
    },
    #
    # Initialize the posteriors
    #
    iniPosteriors = function(parameters, iteration = 0, likeliValues = NULL) {
      #RECOVERY
      logdebug("METHOD IN: mpiMaster$iniPosteriors")

      for (i in 1:config$parallelThreads) {
        if (is.null(likeliValues) || (length(likeliValues) < config$parallelThreads)) {
          posteriors[[i]] <<- Posterior(chain=i, iteration=iteration, lastParameters=parameters[[i]])
        } else {
          posteriors[[i]] <<- Posterior(chain=i, iteration=iteration, lastValue=likeliValues[i], candidateValue=likeliValues[i]
                                      , bestValue=likeliValues[i], lastParameters=parameters[[i]])
        }
      }
      logdebug("METHOD OUT: mpiMaster$iniPosteriors")
     
    },
    #
    # Get mpi slaves
    #
    open = function() {

      logdebug("METHOD IN: mpiMaster$open")
      reset()
      
      # Notice we just say "give us all the slaves you've got."
      mpi.slaves <- 12
      if (!is.null(config$mpiSlaves)) {
		    mpi.slaves <- config$mpiSlaves
      }
		
      mpi.spawn.Rslaves(nslaves = mpi.slaves, comm = 1, needlog = FALSE, quiet = FALSE)
      #mpi.universe.size
      if (mpi.comm.size(comm = 1) < 2) {
        mpi.quit()
        stop("More slave processes are required.")
      }
      
      #QUES: I Don't really understad this function (copied from web tutorial)
      .Last <- function() {
        if (is.loaded("mpi_initialize")) {
          if (mpi.comm.size(1) > 0) {
            logdebug("Please use mpi.close.Rslaves() to close slaves.")
            mpi.close.Rslaves()
          }
          logdebug("Please use mpi.quit() to quit R")
          .Call("mpi_finalize")
        }
      }
      
      logdebug("METHOD OUT: mpiMaster$open")
      
    },
    
    #
    #
    #
    reset = function() {
      tryCatch({
        mpi.close.Rslaves()
      }, error = function(e) {
        print(e)
      })
    },
    #
    # Close mpi slaves
    #
    close = function() {

      logdebug("METHOD IN: mpiMaster$close")
      
      if (mpi.close.Rslaves(comm = 1, dellog = FALSE) == 0)
       logerror("SLAVES ARE NOT CORRECTLY CLOSE")
      
      #Terminates MPI execution environment.
      #mpi.finalize()
      #Close mpi environment and detach Rmpi
      mpi.exit()
      #Quit close all R environment
      #mpi.quit(save="no")
      
      logdebug("METHOD OUT: mpiMaster$close")
    },
    
    executeParallelTasks = function(tasks) {
      #
      # Execute parallel tasks
      #
      logdebug("METHOD IN: mpiMaster$executeParallelTasks")
      
      feedSlaves()

      moveParameters()      
      changeAcceptanceRandomNumber()
      
      tasksResults = list()
      junk <- 0 
      closedSlaves <- 0 
      totalSlaves <- mpi.comm.size()-1 
      
      loopTimes <- 0
      while (closedSlaves < totalSlaves) { 
        
        loopTimes <- loopTimes + 1
        
        message <- getSlaveMessage()
        
        #Slave source info
        slaveInfo <- getSlaveSourceInfo()
        slave_id <- slaveInfo[1]
        slaveStatus <- slaveInfo[2]
        
        taskNumber <- 1
        
        if (slaveIsReady(slaveStatus)) {
          #logdebug("Slave Status == 1: Slave ready")
          # slave is ready for a task. Give it the next task, or tell it tasks 
          # are done if there are none. 
          if (length(tasks) > 0) {

            # Send a task, and then remove it from the task list
            giveSlaveNextTask(slave_id, tasks[[taskNumber]])
            tasks[[taskNumber]] <- NULL   #Delete task
            taskNumber <- taskNumber + 1
          
          } else { 
            #logdebug("No more tasks")
            mpi.send.Robj(junk, slave_id, 2) 
          } 
        
        } else if (slaveHasBeenCorrectlyExecuted(slaveStatus)) {
          #logdebug("Slave Status == 2: A Slave has been correctly executed")
          
          tasksResults[[length(tasksResults)+1]] <- message
        
        } else if (slaveHasClosedDown(slaveStatus)) {
          #logdebug("Slave Status == 3: A Slave has closed down")
          
          closedSlaves <- closedSlaves + 1
        } 
      }
      
      
      #Create output data
      if (posteriors[[1]]$iteration == 0) {
        if (tasksResults[[1]]$ok) {
          output.db$createOutputTable(tasksResults[[1]]$output)
          output.db.created <- TRUE
        } else if (tasksResults[[2]]$ok) {
          output.db$createOutputTable(tasksResults[[2]]$output)
          output.db.created <- TRUE
        } else if (tasksResults[[3]]$ok) {
          output.db$createOutputTable(tasksResults[[3]]$output)
          output.db.created <- TRUE
        }
      }
      

      mixed.results <- mixResults(tasksResults)
      updatePosteriors(mixed.results,tasksResults)
      
      logdebug(paste("Loop times ", loopTimes))
      
      logdebug("METHOD OUT: mpiMaster$executeParallelTasks")
      return(posteriors)
    },
    
    giveSlaveNextTask = function(slave_id, task) {
        
        #logdebug("METHOD IN: mpiMaster$giveSlaveNextTask")
        
        #Change parameters of the task
        task$parameters <- getCandidateParameters(task$chain)
        
        #Send task to slave
        mpi.send.Robj(task, slave_id, 1) 
      
        #logdebug("METHOD OUT: mpiMaster$giveSlaveNextTask")
    },
    
    getSlaveMessage = function() {
      # Receive a message from a slave 
      #logdebug("METHOD IN: mpiMaster$getSlaveMessage")
      
      message <- mpi.recv.Robj(mpi.any.source(),mpi.any.tag()) 
      
      #logdebug("METHOD OUT: mpiMaster$getSlaveMessage")
      
      return(message)
    },
    
    getSlaveSourceInfo = function() {
      #Get the slave info that just sent a message
      #logdebug("METHOD IN: getSlaveSourceInfo")
      
      slave_info <- mpi.get.sourcetag()
      
      #logdebug("METHOD OUT: getSlaveSourceInfo")
      
      return(slave_info)
    },
    
    feedSlaves = function() {
      # Prepare slaves for working
      feeder$feedSlaves()
      feeder$whipSlaves()
    },
    
    slaveIsReady = function(status) {
      #Check the status of the slave (1 == Slave ready)
      isReady <- (status == 1)
      isReady
    },
    
    slaveHasBeenCorrectlyExecuted = function(status) {
      #Check the status of the slave (2 == Slave has been correctly executed)
      isReady <- (status == 2)
      isReady
    },
    
    slaveHasClosedDown = function(status) {
      #Check the status of the slave (3 == Slave has closed down)
      isReady <- (status == 3)
      isReady
    },
    #
    # Generate random value for the posterior acceptance
    #
    changeAcceptanceRandomNumber = function() {
      for (i in 1:config$parallelThreads) {
        rnd.num <- sample(seq(from=0.1, to=0.99, by=0.1), size=1, replace=T, prob=seq(from=0.1, to=0.99, by=0.1))
      # rnd.num <- runif(1)
        posteriors[[i]]$randomWorkaround <<- rnd.num
      }
      
    },
    #
    # Change the parameters for the new task process
    #
    moveParameters = function() {
      for (chain in 1:config$parallelThreads) {
        #Move parameters
        lastParameters <- posteriors[[chain]]$lastParameters
        
        parametersGenerator <- ParametersGenerator(priorParameterDistribution=prior)
        posteriors[[chain]]$candidateParameters <<- parametersGenerator$newRandomParameters(lastParameters)
      }
    },
    
    getCandidateParameters = function(chain) {
      return(posteriors[[chain]]$candidateParameters)
    },
    #
    # Mix the likelihood for each chain and update the posteriors
    #
    updatePosteriors = function(mixed.results, results) {
      
      for (chain in 1:config$parallelThreads) {
        
        likelihood <- mixed.results[[chain]]
        
        accepted <- posteriors[[chain]]$acceptNew(likelihood)
       
        if (accepted) {
          saveOutput(posteriors[[chain]]$iteration, results[[chain]])
        }
      }
    },
  
    #
    # Mix the likelihood by chain
    # When some site fails in one chain we will take the worst likelihood of all chains to complete the gap 
    # (#TODO: think better what to do with multisite fails)
    #
    mixResults = function(results) {
      
      logdebug("METHOD IN: MpiMaster$mixResults")
      
      chain.likeli <- list()
      worst.likelihood <- 0
      chain.fails <- list()
      chain.oks <- list()
      
      #TODO: decide if we want a penalize algorithm or not
      for (chain in 1:config$parallelThreads) {
        chain.likeli[[chain]] <- -Inf
        chain.fails[[chain]] <- 0
        chain.oks[[chain]] <- 0
      }
      output.db.created <- FALSE
      for (result in results) {

        if (!result$ok) {
          logerror(paste("Chain", result$chain, "site", result$site, "fails"))
          chain.fails[[result$chain]] <-  chain.fails[[result$chain]] + 1
          
        } else {
          
          chain.oks[[result$chain]] <- chain.oks[[result$chain]] + 1
          
         
          if (chain.likeli[[result$chain]] == -Inf) {
            print(paste("likeliMPIMaster Primero",chain.likeli[[result$chain]]))
            chain.likeli[[result$chain]] <- result$likelihood
            worst.likelihood <- min(worst.likelihood, result$likelihood)
            
          } else {
            print(paste("likeliMPIMaster Bueno",chain.likeli[[result$chain]]))
            chain.likeli[[result$chain]] <- result$likelihood + chain.likeli[[result$chain]]
            worst.likelihood <- min(worst.likelihood, result$likelihood)
          }
          
        }
      }
      
      
      for (chain in 1:config$parallelThreads) {
        if (chain.oks[[chain]] < ((chain.oks[[chain]] + chain.fails[[chain]])*0.8)) {
          #Remove chains with not enought correct results
          logerror(paste(chain.fails[[chain]],"are to many sites fails for chain",chain, "with", chain.oks[[chain]], "oks"))
          chain.likeli[[chain]] <- -Inf
        } else {
          #Correct the fails of different sites by setting the worst likelihood value for them
          chain.likeli[[chain]] <- chain.likeli[[chain]] + (chain.fails[[chain]] * worst.likelihood)
        }
      }
      
      logdebug("METHOD OUT: MpiMaster$mixResults")
      return(chain.likeli)
    }, 
    #
    # Save output in database
    #
    saveOutput = function(iteration, result) {
      
      output.save.step <- 1
      
      if (iteration > 100) {
        output.save.step <- 100
      }
      
      if ((iteration %% output.save.step) == 0) {
        if (result$ok) {
            output.db$writeOutput(result$chain, result$site, result$likelihood, result$output)
        }
      }
      
    }
    
  )
)




