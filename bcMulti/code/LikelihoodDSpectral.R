#Responsability:
#Calculate the NRMSE

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
      
      error.set <- c()
      
      for (compound in names(data.sets)) {
        
        loginfo(paste("Likelihood for compound ", compound, sep=""))
        
        
        d75 <- index.agreement.d(compound, probs=0.75)
        d25 <- index.agreement.d(compound, probs=0.25)
        
        
        error.set <- c(error.set, mean(c(d25,d75)))
        
      }
      
      
      total.error <- mean(error.set)
      
      loginfo(paste("Mean d:", total.error, sep=""))
      
      logdebug("METHOD OUT: likelihood$sumLikelihood")
      
      return(total.error)
      
    },
    
    index.agreement.d = function(compound, probs=0.75) {
      
      logdebug(paste("METHOD IN: likelihood$index.agreement.d: -Compound:", compound, sep=" "))
      
      quant <- quantile(data.sets[[compound]][,5], probs=probs)
      idx <- which(data.sets[[compound]][,5] > quant)
      
      numerator <- sum((data.sets[[compound]][,3][idx]-data.sets[[compound]][,5][idx])^2)
      meas.mean <-  mean(data.sets[[compound]][,3][idx])
      denominator <- sum((abs(data.sets[[compound]][,5][idx]-meas.mean) + abs(data.sets[[compound]][,3][idx]-meas.mean))^2)
      
      d <- 1 - numerator/denominator
      
      logdebug("METHOD OUT: likelihood$index.agreement.d")
      
      return(d)
    }
    
    
    
    
    
  )#End methods List 
  
)#End RefClass