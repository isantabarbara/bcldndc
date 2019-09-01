# Responsability
# Save and update the Posterior of a MonteCarlo markov chain

Posterior <- setRefClass(    
  "posterior"
  , fields = list(
    chain="numeric", #Chain identification
    acceptedCount="numeric",  #Parameters accepted so far
    bestCount="numeric", #How many times does it change the best posterior
    isBest="logical",
    bestValue="numeric", 
    lastValue="numeric", 
    lastParameters="data.frame",
    candidateValue="numeric",
    candidateParameters="data.frame",
    iteration="numeric",
    acceptanceRate="numeric",
    randomWorkaround = "numeric" #There is a problem creating random numbers inside MPI slaves, so we generate it in MpiMaster
  )
  , methods = list(
    initialize = function(..., chain=0, acceptedCount=0, bestCount=0,isBest=F, iteration=0,lastParameters=data.frame(),
                          acceptanceRate=100.0, lastValue=1e-07, candidateValue=-Inf, bestValue=-Inf, randomWorkaround = 0) {
      callSuper(chain=chain,
                acceptedCount=acceptedCount, 
                bestCount=bestCount,
                isBest=isBest,
                iteration=iteration,
                acceptanceRate=acceptanceRate,
                lastValue=lastValue,
                candidateValue=candidateValue,
                bestValue=bestValue,
                lastParameters=lastParameters,
                randomWorkaround = randomWorkaround,
                ...)
    },
    #
    # Check if the new posterior value is accepted and update the state
    # Return: logical value with the acceptance result
    #
    acceptNew = function(newValue) {
      
      logdebug("METHOD IN: Posterior$acceptNew")
      
      if (newValue >= config$mcmc$gelmanConvergenceConst) {
        accepted <- TRUE
        acceptedCount <<- acceptedCount + 1.0
      } else {
        accepted <- FALSE
      }
      
      candidateValue <<- newValue
      loginfo(paste("newValue for chain ", chain , ":", newValue))
      loginfo(paste("lastValue for chain ", chain , ":", lastValue))

        
      lastParameters <<- candidateParameters
      lastValue <<- newValue
        
        if (newValue > bestValue) {
          isBest <<- TRUE
          bestValue <<-  newValue
          bestCount <<- bestCount + 1
          
          loginfo(paste("New best posterior for chain ", chain, ". bestValue: ", bestValue, sep=""))
        } else {
          isBest <<- FALSE
        }
       
      
      if (iteration > 0) {
        acceptanceRate <<- acceptedCount / iteration
      }
      
      
      logdebug("METHOD OUT: Posterior$acceptNew")
      
      return(accepted)
      
    },
    #
    # Makes sure that new posterior values are being correctly processed.
    #
    validatePosteriorValue = function(newValue) {
      
      # During the first iterations we check that likelihoods change, so we make sure that parameters
      # are changing during model execution (its easy to put parameters from incorrect species or make
      # a wrong configuration of the Lresources file).
    #  if ((iteration > 3) && (iteration < 10) && (newValue == lastValue))
     #   stop("Posterior values are not changing. Are paremeters correctly configured")
      
    #  if (is.na(newValue) | is.na(lastValue))
    #    stop("Posterior values should be available and are NA")
      
    },
    #
    # Print a summary with the chain information
    #
    printSummary = function() {
      logdebug("METHOD IN: Posterior$printSummary")
      
      summary <- c(paste("Summary Chain ", chain, ". BestValue: ", round(bestValue, digits=3), sep=""),
                   paste("Summary Chain ", chain, ". BestCount: ", bestCount, sep=""),
                   paste("Summary Chain ", chain, ". AcceptedCount: ", acceptedCount, sep=""),
                   paste("Summary Chain ", chain, ". AcceptanceRate: ", round(acceptanceRate*100, digits=2), sep=""),
                   paste("Summary Chain ", chain, ". Iterations: ", iteration, sep=""))
      
      summaryFile <- paste(config$path$commonFolder,"summary.txt")
      cat(summary, file=summaryFile, sep="\n", append=T)
      
      loginfo(summary)
      
      logdebug("METHOD OUT: Posterior$printSummary")
    },
    #
    # Create a summary of the process
    #
    getSummary = function() {
      logdebug("METHOD IN: Posterior$getSummary")
      
      summary <- list(chain=chain, bestValue=round(bestValue, digits=3),
                      bestCount=bestCount,
                      acceptedCount=acceptedCount,
                      acceptanceRate=round(acceptanceRate*100, digits=2),
                      iterations=iteration)
      
      
      logdebug("METHOD OUT: Posterior$getSummary")
      
      return(summary)
    }
    
    
  )#End methods List 
  
)#End RefClass

