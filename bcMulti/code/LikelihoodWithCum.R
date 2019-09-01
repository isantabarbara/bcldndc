#Responsability:
#Calculate the likelihood comparing outputs with measurements
library(stats)

#QUES: pensar en lo que dijo Christian del espectrum algo


#TODO: make the measurement and output independent and make available in a likelihood the mix of different aggregation likelihoods.
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
      
      measurementsLikeli <- c()
      
      for (compound in names(data.sets)) {
        
        loginfo(paste("Likelihood for compound ", compound, sep=""))
        
               
        #Using log because with log=F we cannot sum, we muss multiply, and the values can be really small: log(A*B) = logA + logB.
        likeli <- likelihood(compound, log=TRUE)
        
        #loginfo(capture.output(likeli))
        
        #We calculate the mean because if not the compound with more measurements can have bigger weight
        #TODO: If I remove the -Inf I have the problem that the lik mean can be better for a simulation with many -Inf
        sumLikelihood <- mean(likeli)
        
        measurementsLikeli <- c(measurementsLikeli, sumLikelihood)
        
      }
      
      #loginfo("Likelihoods")
      #loginfo(capture.output(measurementsLikeli))
      
    #  totalLikelihood <- mean(measurementsLikeli)
      totalLikelihood <- abs(1/mean(measurementsLikeli))
      
      loginfo(paste("Mean likelihood:", totalLikelihood, sep=""))
      
      logdebug("METHOD OUT: likelihood$sumLikelihood")
      
      return(totalLikelihood)
      
    },
    #
    # Get an identifier based in the actual time
    #
    getTimestamp = function() {
      
      options("digits.secs"=6)
      now <- Sys.time()
      as.numeric(now)
      
    },
    # loglikelihood with normal distribution (not sivia function)
    #- modelOutput: outputs of the models with aggregated means
    #- meas: measurements: column with data in the measurement file (ex: co2)
    #- measurementsSD: measurements Standard Deviation: column with the sd in the measurement file.
    likelihood = function(compound,log=FALSE) {
      
      #logdebug(paste("METHOD IN: likelihood$likelihood: -Compound:", compound, sep=" "))

      
      #We calculate likelihood just to compare (we standarize likelihoods between compound by creating z)
      z <- (data.sets[[compound]][,5] - data.sets[[compound]][,3])/data.sets[[compound]][,4]
      likeli <- dnorm(z, log=log)
      #Without standarizationg it would be like this.
      #likeli <- dnorm(outputs[[compound]], mean=measurements[[compound]]$meas, sd=measurements[[compound]]$measSD, log=FALSE)
      likeli <- rectifyLikelihood(likeli)
      
      likeli.mean <- mean(likeli)
      
      cum <- cumulativeLikelihood(compound, log=log)
      
      logerror(paste("Compound:",compound,"Likelihood:",likeli.mean,"Likelihood for cumulative:",cum$likeli))
           
      #logdebug("METHOD OUT: likelihood$likelihood")
      
      return(c(likeli.mean,cum$likeli))
      
    },
    #
    # Get the likelihood of the cumulative values
    #
    cumulativeLikelihood = function(compound, log=FALSE) {
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
      
      z <- (cum.outputs - new.cum.meas)/rep(sd(new.cum.meas,na.rm=T),length(new.cum.meas))
      
      likeli <- dnorm(z, log=log)
      likeli <- rectifyLikelihood(likeli)
      
      #logdebug("METHOD OUT: likelihood$cumulativeLikelihood")
      
      return(list(likeli=mean(likeli)))
      
    },
    #
    # We admit percent of outliers and replace them with the worst likelihood value 
    #
    rectifyLikelihood= function(likeli, percent = 0.05) {
      invalid.cnt <- sum(likeli == -Inf, na.rm=T)
      na.cnt <- sum(is.na(likeli))
      total.cnt <- length(likeli)
      
      if ((total.cnt * percent) < (invalid.cnt + na.cnt)) {
        min.value <- min(likeli[likeli != -Inf], na.rm=T)
        likeli[likeli == -Inf] <- min.value
        likeli[is.na(likeli)] <- min.value
      }
      
      return(likeli)
    }
    
  )#End methods List 
  
)#End RefClass