#Responsability:
#Calculate likelihood and log different statistic scores
library(stats)

#QUES: pensar en lo que dijo Christian del espectrum algo

ScoreNSE <- setRefClass(    
  "scoreNSE"
  
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
        
        nse <- modelEfficiency(compound)
        
        loginfo(paste("LIKELIHOOD TEST VALUES", id.log, ":compound", compound, "nse", nse))
        
        likeli[[compound]] <- nse
      }
      
      
      logdebug("METHOD OUT: likelihood$sumLikelihood")
      
      return(likeli)
      
    },
    
    modelEfficiency = function(compound) {
      
      logdebug(paste("METHOD IN: likelihood$modelEfficiency: -Compound:", compound, sep=" "))
      
      logdebug(capture.output(data.sets))
      mean.meas <-  mean(data.sets[[compound]][,3])
      logdebug(mean.meas)
      numerator <- sum((data.sets[[compound]][,5] - data.sets[[compound]][,3])^2)
      logdebug(numerator)
      denominator <- sum((data.sets[[compound]][,3] - mean.meas)^2)
      logdebug(denominator)
      model.efficiency <- 1 -  (numerator/denominator)
      
      
      logdebug("METHOD OUT: likelihood$modelEfficiency")
      
      return(model.efficiency)
    }
    
  )#End methods List 
  
)#End RefClass