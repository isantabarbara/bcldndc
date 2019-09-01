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
      
      for (compound in names(data.sets)) {
        
        loginfo(paste("Likelihood for compound ", compound, sep=""))
        
       
        loginfo(capture.output(data.sets[[compound]]))
        
        #Using log because with log=F we cannot sum, we muss multiply, and the values can be really small: log(A*B) = logA + logB.
        gaussian.val <- gaussian.likelihood(compound, log=TRUE)
        
        nse <- modelEfficiency(compound)
        r2 <- coefficient.determination.r2(compound)
        d <- index.agreement.d(compound)
        nrmse.val <- nrmse(compound)
        cvRmse.val <- cvRmse(compound)
        
        logerror(paste("LIKELOHOOD TEST VALUES: compound", compound, "nse", nse, "nrmse", nrmse.val, "cvRmse", cvRmse.val, "r2", r2, "d", d, "sumGaussian", sum(gaussian.val)))
        
        #We calculate the mean because if not the compound with more measurements can have bigger weight
        #TODO: If I remove the -Inf I have the problem that the lik mean can be better for a simulation with many -Inf
        sumLikelihood <- mean(gaussian.val)
        
        measurementsLikeli <- c(measurementsLikeli, d)
        
      }
      
      #loginfo("Likelihoods")
      #loginfo(capture.output(measurementsLikeli))
      
      totalLikelihood <- mean(measurementsLikeli)
      
      loginfo(paste("Mean likelihood:", totalLikelihood, sep=""))
      
      logdebug("METHOD OUT: likelihood$sumLikelihood")
      
      return(totalLikelihood)
      
    },
    # loglikelihood with normal distribution (not sivia function)
    #- modelOutput: outputs of the models with aggregated means
    #- meas: measurements: column with data in the measurement file (ex: co2)
    #- measurementsSD: measurements Standard Deviation: column with the sd in the measurement file.
    gaussian.likelihood = function(compound, log=FALSE) {
      
      logdebug(paste("METHOD IN: likelihood$likelihood: -Compound:", compound, sep=" "))

      #We calculate likelihood just to compare (we standarize likelihoods between compound by creating z)
      z <- (data.sets[[compound]][,5] - data.sets[[compound]][,3])/data.sets[[compound]][,4]
      likeli <- dnorm(z, log=log)
      #Without standarizationg it would be like this.
      #likeli <- dnorm(data.sets[[compound]][,5], mean=data.sets[[compound]][,3], sd=data.sets[[compound]][,4], log=FALSE)
      likeli <- rectifyLikelihood(likeli)
      
      likeli.mean <- mean(likeli)
      
      logdebug("METHOD OUT: likelihood$likelihood")
      
      return(likeli.mean)
      
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
    nrmse = function(compound) {
      
      logdebug(paste0("METHOD IN: likelihood$nrmse: -Compound:", compound))
      
      rmse <- sqrt(sum((data.sets[[compound]][,3]-data.sets[[compound]][,5])^2)/nrow(data.sets[[compound]][,3]))
      
      nrmse.val <- rmse/(max(data.sets[[compound]][,3])-min(data.sets[[compound]][,3]))
      
      logdebug("METHOD OUT: likelihood$nrmse")
      
      return(nrmse.val)
    },
    cvRmse = function(compound) {
      
      logdebug(paste0("METHOD IN: likelihood$nrmse: -Compound:", compound))
      
      rmse <- sqrt(sum((data.sets[[compound]][,3]-data.sets[[compound]][,5])^2)/nrow(data.sets[[compound]][,3]))
      
      cvRmse.val <- rmse/mean(data.sets[[compound]][,3])
      
      logdebug("METHOD OUT: likelihood$nrmse")
      
      return(cvRmse.val)
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