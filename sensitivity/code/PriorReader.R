#Responsability:
#Read the prior configuration file

PriorReader <- setRefClass(    
  "priorreader"
  
  , fields = list(
      folder="character",
      file="character",
      prior = "data.frame"
  )
  , methods = list(
    #
    #
    # Constructor
    #
    #
    initialize = function(folder = config$path$common,
                          file = config$ldndc$priorsFile, 
                          prior = data.frame(),
                          ...)
    {
     logdebug("METHOD IN: Initialize PriorReader") 
      callSuper(..., 
                folder = folder,
                file = file,
                prior = prior
               )
     logdebug("METHOD OUT: Initialize PriorReader") 
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
      prior <<- read.table(file.path(folder,file), header=T)
      
      logdebug("METHOD OUT: priorProbabilityDistribution")
      
      prior
    },
    #
    #
    # Return a value between 0 and 1 for the parameter value 
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
      logdebug("METHOD IN: denormalize")
      
      min <- prior[prior$name == name,]$min
      max <- prior[prior$name == name,]$max
      logdebug(name)
      logdebug(min)
      logdebug(max)
      logdebug(norm.value)
      
      value <- norm.value * (max - min) + min
      logdebug(value)
      
      logdebug("METHOD OUT: denormalize")
      
      return(value)
      
    }
    
    
  )#End methods List 
  
)#End RefClass
