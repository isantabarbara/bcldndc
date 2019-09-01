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
        logerror(paste("er",error))
        diff <- diff.dispersion(compound)
        error <- error + error*diff
        #error.cum <- cumulative(compound)
        #logerror(paste("ercum",error.cum))
        
        
        error.set <- c(error.set, error)#,error.cum)
        
      }
      
      
      total.error <- mean(error.set)
      
      logerror(paste("Mean NRMSE:", total.error, sep=""))
      
      logdebug("METHOD OUT: likelihood$sumLikelihood")
      
      return(1/total.error)
      
    },
    
    nrmse = function(compound) {
      
      logdebug(paste0("METHOD IN: likelihood$nrmse: -Compound:", compound))
      
      
      rmse <- sqrt(sum((data.sets[[compound]][,3]-data.sets[[compound]][,5])^2)/length(data.sets[[compound]][,3]))
      
      nrmse.val <- rmse/(max(data.sets[[compound]][,3])-min(data.sets[[compound]][,3]))
      
      
      logdebug("METHOD OUT: likelihood$nrmse")
      
      return(nrmse.val)
    },
    
    #
    # Get the likelihood of the cumulative values
    #
    cumulative= function(compound) {
      #logdebug("METHOD IN: likelihood$cumulativeLikelihood")
      
      
      cum.outputs <- data.sets[[compound]][,5]
      
      new.cum.outputs <- c()
      spls <- split(cum.outputs, ceiling(seq_along(cum.outputs)/5))
      for (spl in spls) {
        new.cum.outputs <- c(new.cum.outputs,cumsum(spl))  
      }
      
      cum.meas <- data.sets[[compound]][,3]
      
      new.cum.meas <- c()
      spls <- split(cum.meas, ceiling(seq_along(cum.meas)/5))
      for (spl in spls) {
        new.cum.meas <- c(new.cum.meas,cumsum(spl))
      }
      
      rmse <- sqrt(sum((new.cum.meas-new.cum.outputs)^2)/length(new.cum.meas))
      
      nrmse.val <- rmse/(max(new.cum.meas)-min(new.cum.meas))
      
     
      
      #logdebug("METHOD OUT: likelihood$cumulativeLikelihood")
      
      return(nrmse.val)
      
    },
    
    diff.dispersion = function(compound) {
      
      outputs.disp <- sd(data.sets[[compound]][,5])/mean(data.sets[[compound]][,5])
      meas.disp <- sd(data.sets[[compound]][,3])/mean(data.sets[[compound]][,3])
      
      diff <- abs(meas.disp-outputs.disp)
      
      return(diff)
    }
    
    
    
    
    
  )#End methods List 
  
)#End RefClass