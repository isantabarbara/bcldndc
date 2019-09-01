#Responsability:
#Calculate likelihood and log different statistic scores
library(stats)

#QUES: pensar en lo que dijo Christian del espectrum algo

ScoreD <- setRefClass(    
  "scoreD"
  
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
        

        d <- index.agreement.d(compound)
        d.cum <- index.agreement.d.cum(compound)
        
        loginfo(paste("LIKELIHOOD TEST VALUES", id.log, ":compound", compound, "d.cum", d.cum, "d", d))
        
        d <- mean(c(d,d.cum))
        
        likeli[[compound]] <- d
      }
      
     
      logdebug("METHOD OUT: likelihood$sumLikelihood")
      
      return(likeli)
      
    },
    index.agreement.d = function(compound) {
      logdebug(paste("METHOD IN: likelihood$index.agreement.d: -Compound:", compound, sep=" "))
      
      numerator <- sum((data.sets[[compound]][,3]-data.sets[[compound]][,5])^2)
      meas.mean <-  mean(data.sets[[compound]][,3])
      denominator <- sum((abs(data.sets[[compound]][,5]-meas.mean) + abs(data.sets[[compound]][,3]-meas.mean))^2)
      
      d <- 1 - numerator/denominator
      
      logdebug("METHOD OUT: likelihood$index.agreement.d")
      
      return(d)
      
    },
    index.agreement.d.cum = function(compound) {
      logdebug(paste("METHOD IN: likelihood$index.agreement.d.cum: -Compound:", compound, sep=" "))
      
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
      
      numerator <- sum((new.cum.meas-new.cum.outputs)^2)
      meas.mean <-  mean(new.cum.meas)
      denominator <- sum((abs(new.cum.outputs-meas.mean) + abs(new.cum.meas-meas.mean))^2)
      
      d.cum <- 1 - numerator/denominator
      
      logdebug("METHOD OUT: likelihood$index.agreement.d.cum")
      
      return(d.cum)
      
    }
    
  )#End methods List 
  
)#End RefClass