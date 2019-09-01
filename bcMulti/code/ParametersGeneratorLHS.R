#Responsability:
#Move the parameters for the Ldndc model
#TODO: Add prior not uniform parameter generator.
library("lhs")
require("MASS")

ParametersGenerator <- setRefClass(    
  "parametersgenerator"
  , fields = list(
      priorParameterDistribution="data.frame",
      parameterSamples="data.frame"
  )
  , methods = list(
    initialize = function(...,
                          priorParameterDistribution=data.frame(),
                          parameterSamples=data.frame()) {
      
      callSuper(priorParameterDistribution=priorParameterDistribution,
                parameterSamples = parameterSamples,
                ...)
      
    },
    
    getLHSParameters = function() {
      
      logdebug("METHOD IN: ParametersGenerator$getLHSParameters")
      logdebug(config$mcmc$gelmanAfterConvergenceLoops)
      logdebug(capture.output(head(priorParameterDistribution)))
      logdebug(nrow(priorParameterDistribution))
      samples <-randomLHS(n=config$mcmc$gelmanAfterConvergenceLoops, k=nrow(priorParameterDistribution))
      min <- priorParameterDistribution$min
      max <- priorParameterDistribution$max
      parameterSamples <<- data.frame(t(t(samples) * (max - min) + min))
      colnames(parameterSamples) <<- priorParameterDistribution$name
      logdebug(capture.output(head(parameterSamples)))
      
      firstParameters <- parameterSamples[1,]
      logdebug("First parameters")
      logdebug(capture.output(t(firstParameters)))
     
      
      logdebug("METHOD OUT: ParametersGenerator$getLHSParameters")
      
      return(data.frame(t(firstParameters)))
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
    newRandomParameters = function(iteration) {
      
      logdebug("METHOD IN: ParametersGenerator$newRandomParameters")
      logdebug(capture.output(head(parameterSamples)))
      
      if (iteration+1 <= nrow(parameterSamples)) {
        parameter <- parameterSamples[iteration+1,]
      } else {
        parameter <- parameterSamples[1,]
      }
     
      logdebug(capture.output(t(parameter)))
      logdebug("METHOD OUT: ParametersGenerator$newRandomParameters")
      
      return(data.frame(t(parameter)))
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
