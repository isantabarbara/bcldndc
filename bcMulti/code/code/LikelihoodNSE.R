#Responsability:
#Calculate the model efficiency comparing outputs with measurements (Minimazing deviation)

library(stats)

Likelihood <- setRefClass(    
  "likelihood"
  
  , fields = list(
    data.sets="list"
  )
  , methods = list(
    #
    # Constructor
    # 
    initialize = function(..., 
                          data.sets = data.sets)
    {
      logdebug("Initialize Likelihood")
      callSuper(...,
                data.sets = data.sets)
      
    },
    sumLikelihood = function() {
      
      logdebug("METHOD IN: likelihood$sumLikelihood")
      
      nse.set <- c()
      
      for (compound in names(data.sets)) {
        
        loginfo(paste("Likelihood for compound ", compound, sep=""))
        
          
        nse <- modelEfficiency(compound)
        
      
        nse.set <- c(nse.set, nse)
        
      }
      
      
      total.nse <- mean(nse.set)
      
      loginfo(paste("Mean NSE:", total.nse, sep=""))
      
      logdebug("METHOD OUT: likelihood$sumLikelihood")
      
      return(total.nse)
      
    },

    #
    # Model Efficiency
    # Nash–Sutcliffe model efficiency coefficient
    #
    modelEfficiency = function(compound) {
      
      
      logdebug(paste("METHOD IN: likelihood$modelEfficiency: -Compound:", compound, sep=" "))
      
      
          
      mean.meas <-  mean(measurements[[compound]][,3])
      numerator <- sum((measurements[[compound]][,5] - measurements[[compound]][,3])^2)
      denominator <- sum((measurements[[compound]][,3] - mean.meas)^2)
      model.efficiency <- 1 -  (numerator/denominator)
      
      logdebug("METHOD OUT: likelihood$modelEfficiency")
      
      return(model.efficiency)
    }
    
    
    
    
    
  )#End methods List 
  
)#End RefClass