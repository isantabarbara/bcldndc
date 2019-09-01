# Responsability
# Gelman Rubin statistics and storage of the GR markov chain data

GelmanRubin <- setRefClass(    
  "gelmanrubin"
  
  , fields = list(
      parametersNames = "character",
      iteration="numeric",
      convergenceConst = "numeric",
      convergedParamIndex = "numeric",
      burnPhaseIterations="numeric",
      converged="logical",
      afterConvergenceLoops="numeric" # After convergence of chains, is reduced by 1 in each iteration until it becomes 0

  )
  , methods = list(
    #
    #
    # Constructor
    #
    #
    initialize = function(..., iteration=0, 
                          convergenceConst=config$mcmc$gelmanConvergenceConst, 
                          burnPhaseIterations=config$mcmc$gelmanBurnPhaseIterations, 
                          converged=FALSE,
                          convergedParamIndex = 0,
                          afterConvergenceLoops=config$mcmc$gelmanAfterConvergenceLoops, 
                          parametersNames="")
    {
      logdebug("Initialize GelmanRubin")
      
      callSuper(..., 
                iteration=iteration,
                convergenceConst=convergenceConst,
                burnPhaseIterations=burnPhaseIterations,
                converged=converged,
                convergedParamIndex= convergedParamIndex,
                afterConvergenceLoops=afterConvergenceLoops,
                parametersNames=parametersNames)
      
    },
    #
    # New Iteration. Update iteration counter
    #
    setIteration = function(newIteration) {

        logdebug("METHOD IN: GelmanRubin$setIteration")
      
        iteration <<- newIteration
        
        logdebug("METHOD OUT: GelmanRubin$setIteration")
    },
   
    #
    #
    # Gelman Rubin convergence diagnostic. Return TRUE when the chains converge after the burn phase
    #
    #
    convergenceDiagnostic = function(db) {
      
      loginfo("METHOD IN: GelmanRubin$convergenceDiagnostic")
      
      R_hat <- potentialScaleReductionFactors(db)
      
      convergence <- FALSE
      
      new.converged.param <- c()
      
      #With only one chain R_hat will be NA, so we can not use length, instead we use sum(!is.na)
      if ((iteration > burnPhaseIterations) && (sum(!is.na(R_hat)) > 0)) {
        convergence <- all(R_hat <= convergenceConst)
        
        all.converged <- which(R_hat <= convergenceConst)
        new.converged.param <- setdiff(all.converged,convergedParamIndex)
        
        convergedParamIndex <<- all.converged
      }
      

      if (length(R_hat) == length(parametersNames)) {
        loginfo("R_hat values:")
        loginfo(capture.output(data.frame(parametersNames,R_hat)))
      }
      
      if (length(new.converged.param) > 0) {
        converged.param.name <- parametersNames[new.converged.param]
        loginfo(paste("CONVERGENCE REACHED",converged.param.name))
        db$insertConvergenceIteration(paste0("ConvergedIteration",converged.param.name),iteration)
      }
      
      loginfo("METHOD OUT: GelmanRubin$convergenceDiagnostic")
      
      return(convergence)
      
    },
    #
    # Make the calculations to check if the MCMC has reached the end
    #
    reachTheEnd = function(db) {
      
      logdebug("METHOD IN: GelmanRubin$reachTheEnd")
      
      if (converged) {
        afterConvergenceLoops <<- afterConvergenceLoops - 1
      } else {
        converged <<- convergenceDiagnostic(db)
      }
      
      loginfo(paste("Iteration ", iteration, " converged: ", converged, sep=""))
      logdebug(paste("afterConvergenceLoops: ",afterConvergenceLoops, sep=""))
      
      logdebug("METHOD OUT: GelmanRubin$reachTheEnd")
      
      enoughLoops()
    },
    #
    # Check if the MCMC method has make enought loops
    #
    enoughLoops = function() {
      enoughLoops <- FALSE
      
      if (config$debugForceStop) {
        
        enoughLoops <- (afterConvergenceLoops <= iteration)
      
      } else {
      
        enoughLoops <- (afterConvergenceLoops <= 0)
      }
      
      enoughLoops
    },
    calculateWithinChainVariances = function(db) {

      logdebug("METHOD IN: calculateWithinChainVariances")
      
      Ws <- as.vector(t(db$getChainVariances(1)))
      
      for (chain in 2:config$parallelThreads) {
        Ws <- Ws + as.vector(t(db$getChainVariances(chain)))
      }
     
      Ws <- Ws / config$parallelThreads
      
      logdebug("METHOD OUT: calculateWithinChainVariances")
      
      Ws
      
    },
    calculateBetweenChainVariances = function(db) {
      
      logdebug("METHOD IN: calculateBetweenChainVariances")
      
      chainMeansArray <- cbind(as.vector(t(db$getChainAverages(1))))
            
      for (chain in 2:config$parallelThreads) {
          chainMeansArray <- cbind(chainMeansArray,as.vector(t(db$getChainAverages(chain))))
      }
      
      variancesBetweenChains <- apply(chainMeansArray, 1, var)
      
      Bs <- iteration * variancesBetweenChains
      
      logdebug("METHOD OUT: calculateBetweenChainVariances")
      
      Bs
      
    },
    calculateTotalMeanFromChainsMean = function() {
      
      logdebug("METHOD IN: calculateTotalMeanFromChainsMean")
      
      totalMean <- 0
      
      for (chain in 1:config$parallelThreads) {
        db$getChainAverage(chain)
      }
      
      totalMean <- totalMean / config$parallelThreads
      
      logdebug("METHOD OUT: calculateTotalMeanFromChainsMean")
      
      totalMean
    },
    estimatedVariances = function(Ws, Bs) {
      
      logdebug("METHOD IN: estimatedVariances")
      
      vars <- (1-1/iteration)*Ws + (1/iteration)*Bs
     
      logdebug("METHOD OUT: estimatedVariances")
      
      vars
    },
    potentialScaleReductionFactors = function(db) {
      
      logdebug("METHOD IN: potentialScaleReductionFactors")
      
      Rs <- 99999
      
      tryCatch({
        Ws <- calculateWithinChainVariances(db)
        Bs <- calculateBetweenChainVariances(db)
    
        #Bs y Ws when the parameters are equal is always 0, so we exclude them.
        Rs <- sqrt(estimatedVariances(Ws[Ws>0],Bs[Bs>0])/Ws[Ws>0])
      }, 
      error = function(e) {
        logerror(paste("Error in Potential Scale Reduction Factors", e))  
      })
      
      logdebug("METHOD OUT: potentialScaleReductionFactors")
      
      return(Rs)
    }
    
  )
)
