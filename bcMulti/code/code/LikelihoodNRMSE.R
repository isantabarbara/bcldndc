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
        
        
        error <- nrmse(compound)
        
        
        error.set <- c(error.set, error)
        
      }
      
      
      total.error <- mean(error.set)
      
      loginfo(paste("Mean NRMSE:", total.error, sep=""))
      
      logdebug("METHOD OUT: likelihood$sumLikelihood")
      
      return(1/total.error)
      
    },
    
    nrmse = function(compound) {
      
      logdebug(paste0("METHOD IN: likelihood$nrmse: -Compound:", compound))
      
   
      logerror(paste("length",length(data.sets[[compound]][,3])))
      
      rmse <- sqrt(sum((data.sets[[compound]][,3]-data.sets[[compound]][,5])^2)/length(data.sets[[compound]][,3]))

      nrmse.val <- rmse/(max(data.sets[[compound]][,3])-min(data.sets[[compound]][,3]))
      logerror(paste("rmse",nrmse.val))
      
      logdebug("METHOD OUT: likelihood$nrmse")
      
      return(nrmse.val)
    }
    
    
    
    
    
  )#End methods List 
  
)#End RefClass