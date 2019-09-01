#Responsability:
#Move the parameters for the Ldndc model
#TODO: Add prior not uniform parameter generator.
library("lhs")
require("MASS")

ParametersGenerator <- setRefClass(    
  "parametersgenerator"
  , fields = list(
      priorParameterDistribution="data.frame"
    )
  , methods = list(
    initialize = function(...) {
      
      callSuper(...)
      
    },

    getLHSParameters = function() {
      
      logdebug("METHOD IN: ParametersGenerator$getLHSParameters")

      firstParameters <- data.frame(priorParameterDistribution$min ,row.names=priorParameterDistribution$name)
      
      logdebug("First parameters")
      logdebug(firstParameters)
      min <- priorParameterDistribution$min
      max <- priorParameterDistribution$max
      
      #n=1 only gives 1s
      lhs_matrix <- optimumLHS(n=2,k=nrow(firstParameters))
      
      # move random number from (0,1) to given intervall
      firstParameters[,1] <- lhs_matrix[1,] * (max - min) + min 
      
      logdebug("LHS First parameters")
      logdebug(firstParameters)
      
      logdebug("METHOD OUT: ParametersGenerator$getLHSParameters")
      
      firstParameters
    },
    #
    #
    # Get the fixed initialization values of the parameter
    #
    #
    getInitParameters = function() {
      
      logdebug("METHOD IN: ParametersGenerator$getInitParameters")
     
      firstParameters <- data.frame(priorParameterDistribution$init ,row.names=priorParameterDistribution$name)
      
      logdebug("METHOD OUT: ParametersGenerator$getInitParameters")
      
      firstParameters
    },
    #
    # Generate new set of parameters for the model
    # - Parameter: table with the last accepted parameters
    #
    newRandomParameters = function(parameter) {
      
        logdebug("METHOD IN: ParametersGenerator$newRandomParameters")
        
        #TODO: Ruediger idea add the posibility of correlation between parameters. ej: MAX and MIN
        min <- priorParameterDistribution$min
        max <- priorParameterDistribution$max
        
        cand <- moveParameters(parameter[,1], min, max)
        
        #QUES: Oijen 2005: not worry about jumping outside the range. [I am worring!]
        
        #cand doesn't have the column names
        parameter[,1] <- cand
        
        logdebug("New Parameters Generated:")
        logdebug(parameter)
        
        logdebug("METHOD OUT: ParametersGenerator$newRandomParameters")
        
        return(parameter)
    },
    #
    # Check if at least one candidate is out of bounds
    # Return: logical value
    #
    outOfBounds = function(cand, min, max) {
      
      logdebug("METHOD IN: ParametersGenerator$outOfBounds")
      
      outofmins <- length(which(cand < min)) > 0
      outofmaxs <- length(which(cand > max)) > 0
      outofbounds <- outofmins || outofmaxs
      
      logdebug("METHOD OUT: ParametersGenerator$outOfBounds")
      
      return(outofbounds)

    },
    #
    # Give a logical vector with the candidates out of bounds
    #
    outOfBoundsVector = function(cand, min, max) {
      
      logdebug("METHOD IN: ParametersGenerator$outOfBoundsVector")
      
      outofmins <- cand < min
      outofmaxs <- cand > max
      outofbounds <- outofmins | outofmaxs
      
      logdebug("METHOD OUT: ParametersGenerator$outOfBoundsVector")
      
      return(outofbounds)
      
    },
    #
    # Bounce with min or max if candidate is not in parameter space 
    #
    bounceCandidatesOutOfParameterSpace = function(cand, min, max) {
      
      # 
      # if Cand is out of boundarys bounce into boundarys (shouldnt be done with normal dist)
      # if Cand is TOO small or TOO large bounce to maximum or to minimum value respectively 
      
      outofbounds <- which(cand < min  & ( min - cand < max - min) ) 
      cand[ outofbounds ] <- (min[outofbounds] - cand[outofbounds] ) + min[outofbounds]
      
      outofbounds <- which(cand < min  & ( min - cand >= max - min) )
      cand[ outofbounds ] <-  max[outofbounds]
      
      outofbounds <- which(cand > max  & ( cand - max < max - min) )
      cand[ outofbounds ] <- (max[outofbounds] - cand[outofbounds] ) + max[outofbounds]
      
      outofbounds <- which(cand > max  & ( cand - max >= max - min) )
      cand[ outofbounds ] <- min[outofbounds] 
    
      cand
    },
    #
    # Generate a proposal for a new candidate value for Teta based on our existing value:
    # Teta1 = Teta + Epsilon
    #
    moveParameters = function(parameterValues, min, max) {
      
      logdebug("METHOD IN: moveParameters")
     
      epsilon <- generateEpsilonMatrix()
      
      if (!(is.null(config$params$montecarlo)) && config$params$montecarlo) {
        logdebug("Montecarlo move parameters")
        proposal <- runif(length(parameterValues), min, max)
      } else {
        #mu=parameterValues: a vector giving the means of the variables.
        #Sigma=epsilon: a positive-definite symmetric matrix specifying the covariance matrix of the variables.
        proposal <- mvrnorm(n=1,parameterValues, epsilon)
      }
      #We do not accept parameters out of bounds. Loop until all are inside bounds
      whoIsOutOfBounds <- outOfBoundsVector(proposal, min, max)
      
      while (!fixedInBoundaries(whoIsOutOfBounds)) {
      
        newProposal <- mvrnorm(n=1,parameterValues, epsilon)
        proposal[whoIsOutOfBounds] <- newProposal[whoIsOutOfBounds]
        
        whoIsOutOfBounds <- outOfBoundsVector(proposal, min, max)
      }
      
      
      logdebug("METHOD OUT: moveParameters")
      
      return(proposal)
    },
    #
    # Check if all parameters are inside bounds from the logical vector
    #
    fixedInBoundaries = function(whoIsOutOfBounds) {
      
      fixed <- length(which(whoIsOutOfBounds)) == 0
      
      return(fixed)
      
    },
    #
    # (Van Oijen et al. 2005):
    # "For Epsilon... We found that variances equal to the square
    # of 5% of the prior parameter range and 0 covariances gave good results"
    # 
    generateEpsilonMatrix = function() {
      
      range <-  getPriorParameterRange()
      
      #Play with stepWidth to make Epsilon bigger and create more differences between iterations outputs.
      stepWidth <- config$params$stepWidth
      epsilonMatrix <- diag((stepWidth * range)^2)
      
      return(epsilonMatrix)
    },
    #
    # Get the range of prior Parameters
    #
    getPriorParameterRange = function() {
      
      min <- priorParameterDistribution$min
      max <- priorParameterDistribution$max
      
      range <- max - min
      
      return(range)
    }
    
                                
    )#End methods List 
  
)#End RefClass
