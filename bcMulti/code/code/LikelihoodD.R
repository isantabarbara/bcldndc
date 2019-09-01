#Responsability:
#Calculate likelihood and log different statistic scores
library(stats)

#QUES: pensar en lo que dijo Christian del espectrum algo

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
                          data.sets = "")
    {
      logdebug("Initialize Likelihood")
      callSuper(...,
                data.sets = data.sets)
      
    },
    
    sumLikelihood = function() {
      
      logdebug("METHOD IN: likelihood$sumLikelihood")
      
      measurementsLikeli <- c()
      
      #Workaround: id.log is just to identify the different compound values in log file.
      id.log <- ceiling(runif(1,1,1000))
      
      for (compound in names(data.sets)) {
        
        loginfo(paste("Likelihood for compound ", compound, sep=""))
      
        
        d <- index.agreement.d(compound)
 
            
        measurementsLikeli <- c(measurementsLikeli, d)
        
      }
      
      #loginfo("Likelihoods")
      #loginfo(capture.output(measurementsLikeli))
      
      totalLikelihood <- mean(measurementsLikeli)
      
      loginfo(paste("Mean likelihood:", totalLikelihood, sep=""))
      
      logdebug("METHOD OUT: likelihood$sumLikelihood")
      
      return(totalLikelihood)
      
    },
   
   
    
    index.agreement.d = function(compound) {
      logdebug(paste("METHOD IN: likelihood$index.agreement.d: -Compound:", compound, sep=" "))
     # d = 1 - [ ( sum( (obs - sim)^2 ) ] / sum( ( abs(sim - mean(obs)) + abs(obs - mean(obs)) )^2 ) 
      
      out <- data.sets[[compound]][,5]#getOutputMaxs(compound)
      
      
      numerator <- sum((data.sets[[compound]][,3]-out)^2)
      meas.mean <-  mean(data.sets[[compound]][,3])
      denominator <- sum((abs(out-meas.mean) + abs(data.sets[[compound]][,3]-meas.mean))^2)
      
      d <- 1 - numerator/denominator
      
      logdebug("METHOD OUT: likelihood$index.agreement.d")
      
      return(d)
      
    },
    
    getOutputMaxs = function(compound) {
      
      x <- c()
      for (idx in 1:length(data.sets[[compound]][,5])) {
        logerror(paste(compound,"index",idx))
        if (idx > 2) {
          logerror(data.sets[[compound]][,5][c((idx-2):(idx+2))])
          #In the same year
          if (length(unique(data.sets[[compound]][,1][c((idx-2):(idx+2))])) == 1) {
             x <- c(x,max(data.sets[[compound]][,5][c((idx-2):(idx+2))]))
          }
        }
        else {
          x <- c(x,data.sets[[compound]][,5][idx])
        }
      }
      
      return(x)
      
    }
  )#End methods List 
  
)#End RefClass