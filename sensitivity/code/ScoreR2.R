#Responsability:
#Calculate likelihood and log different statistic scores
library(stats)

#QUES: pensar en lo que dijo Christian del espectrum algo

ScoreR2 <- setRefClass(    
  "scoreR2"
  
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
        
        
        loginfo(capture.output(data.sets[[compound]]))
   
        
        r2 <- coefficient.determination.r2(compound)
      
        
        loginfo(paste("LIKELIHOOD TEST VALUES", id.log, ":compound", compound, "r2", r2))
        
        likeli[[compound]] <- r2
      }
      
      
      logdebug("METHOD OUT: likelihood$sumLikelihood")
      
      return(likeli)
      
    },
 
    coefficient.determination.r2 = function(compound) {
      
      logdebug(paste("METHOD IN: likelihood$coefficient.determination.r2: -Compound:", compound, sep=" "))
      meas.mean <-  mean(data.sets[[compound]][,3])
      sim.mean <-  mean(data.sets[[compound]][,5])
      
      sim.coff <- (data.sets[[compound]][,5]-sim.mean)
      meas.coff <- (data.sets[[compound]][,3]-meas.mean)
      numerator <- sum(sim.coff*meas.coff)
      denominator <- (sqrt(sum(sim.coff^2))*sqrt(sum(meas.coff^2)))
      r2 <- numerator/denominator
      
      logdebug("METHOD OUT: likelihood$coefficient.determination.r2")
      
      return(r2)
    }
    
  )#End methods List 
  
)#End RefClass