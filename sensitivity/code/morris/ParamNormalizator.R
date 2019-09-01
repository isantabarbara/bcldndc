#Responsability:
#Read the prior configuration file and handle the normalization and denormalization process of the parameters

ParamNormalizator <- setRefClass(    
  "paramnormalizator"
  
  , fields = list(
    folder="character",
    file="character"
  )
  , methods = list(
    #
    #
    # Constructor
    #
    #
    initialize = function(folder = config$path$common,
                          file = config$ldndc$priorsFile,
                          prior = data.frame()
                          ...)
    {
      
      callSuper(..., 
                folder = folder,
                file = file,
                prior = prior
      )
    },
    #
    #
    # Read the file with the prior probability distribution configuration
    # Return: the prior probability distribution
    #
    #
    priorProbabilityDistribution = function() {
      
      logdebug("METHOD IN: priorProbabilityDistribution")
      logdebug(file.path(folder,file))
      priorProbabilityDistribution <- read.table(file.path(folder,file), header=T)
      
      logdebug("METHOD OUT: priorProbabilityDistribution")
      
      return(priorProbabilityDistribution)
    },
    #
    #
    # REturn a value between 0 and 1 for the parameter value 
    #
    #
    normalize = function(name, value) {
      
      min <- prior[prior$name == name,]$min
      max <- prior[prior$name == name,]$max
      
      norm.value <- (value - min)/(max - min)
      
      return(norm.value)
    },
    #
    #
    # Return the parameter value 
    #
    #
    denormalize = function(name, norm.value) {
      
      min <- prior[prior$name == name,]$min
      max <- prior[prior$name == name,]$max
      
      value <- norm.value * (max - min) + min
      
      return(value)
      
    }
    
    
    
    
    
  )#End methods List 
  
)#End RefClass
