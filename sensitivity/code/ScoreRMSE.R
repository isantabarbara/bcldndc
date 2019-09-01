#Responsability:
#Calculate likelihood and log different statistic scores
library(stats)

#QUES: pensar en lo que dijo Christian del espectrum algo

ScoreRMSE <- setRefClass(    
  "scoreRMSE"
  
  , fields = list(
    data.sets="list"
  )
  , methods = list(
    #
    # Constructor
    # 
    initialize = function(..., 
                          data.sets = "")
    {
      logdebug("Initialize Likelihood")
      callSuper(...,
                data.sets = data.sets)
      
    },
    
    score = function() {
      
      logdebug("METHOD IN: likelihood$sumLikelihood")
      
      likeli <- list()
      
      #Workaround: id.log is just to identify the different compound values in log file.
      id.log <- ceiling(runif(1,1,1000))
      
      for (compound in names(data.sets)) {
        
        loginfo(paste("Likelihood for compound ", compound, sep=""))
        
        nrmse.val <- nrmse.f(compound)
        
        loginfo(paste("LIKELIHOOD TEST VALUES", id.log, ":compound", compound, "nrmse", nrmse.val))
        
        likeli[[compound]] <- nrmse.val
      }
      
      logdebug("METHOD OUT: likelihood$sumLikelihood")
      
      return(likeli)
      
    },
    nrmse.f = function(compound) {
      
      logdebug(paste0("METHOD IN: likelihood$nrmse: -Compound:", compound))
      
      rmse <- sqrt(sum((data.sets[[compound]][,3]-data.sets[[compound]][,5])^2)/nrow(data.sets[[compound]][,3]))
      loginfo(paste("RMSE",rmse))
      nrmse.val <- rmse/(max(data.sets[[compound]][,3])-min(data.sets[[compound]][,3]))
      loginfo(paste("RMSE.val",nrmse.val))
      
      logdebug("METHOD OUT: likelihood$nrmse")
      
      return(nrmse.val)
    },
    cvRmse = function(compound) {
      
      logdebug(paste0("METHOD IN: likelihood$nrmse: -Compound:", compound))
      
      rmse <- sqrt(sum((data.sets[[compound]][,3]-data.sets[[compound]][,5])^2)/nrow(data.sets[[compound]][,3]))
      
      cvRmse.val <- rmse/mean(data.sets[[compound]][,3])
      
      logdebug("METHOD OUT: likelihood$nrmse")
      
      return(cvRmse.val)
    }
    
  )#End methods List 
  
)#End RefClass