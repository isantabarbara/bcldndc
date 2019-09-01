#Responsability:
#Calculate the likelihood comparing outputs with measurements
library(hydroGOF)
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
      rnd.num <- sample(1:100,1)
      for (compound in names(data.sets)) {
        
        loginfo(paste("Likelihood for compound ", compound, sep=""))
        
        #Using log because with log=F we cannot sum, we muss multiply, and the values can be really small: log(A*B) = logA + logB.
        likeli <- likelihood(compound, log=TRUE)
        
        
        peaks.likeli <- peaksLikelihood(compound, log=TRUE)
        
        
        likeli <- rectifyLikelihood(likeli)
        peaks.likeli <- rectifyLikelihood(peaks.likeli)
        
        likeli.mean <- mean(likeli)
        peaks.likeli.mean <- mean(peaks.likeli)
        
        logerror(paste(rnd.num, "Mean Likelihood for compound", compound, "is", likeli.mean))
        measurementsLikeli <- c(measurementsLikeli, peaks.likeli.mean)
        
      }
      
      loginfo("Likelihoods")
      loginfo(capture.output(measurementsLikeli))
      
      totalLikelihood <- abs(1/mean(measurementsLikeli))
      
      logerror(paste(rnd.num, "Mean likelihood:", totalLikelihood, sep=""))
      
      logdebug("METHOD OUT: likelihood$sumLikelihood")
      
      return(totalLikelihood)
      
    },
    #
    # We admit percent of outliers and replace them with the worst likelihood value 
    #
    rectifyLikelihood= function(likeli, percent = 0.0005) {
      invalid.cnt <- sum(likeli == -Inf, na.rm=T)
      na.cnt <- sum(is.na(likeli))
      total.cnt <- length(likeli)
      
      min.value <- -99999
      if ((total.cnt * percent) < (invalid.cnt + na.cnt)) {
        #min.value <- min(likeli[likeli != -Inf], na.rm=T)
        likeli[likeli == -Inf] <- min.value
        likeli[is.na(likeli)] <- min.value
      }
      
      return(likeli)
    },
    # loglikelihood with normal distribution (not sivia function)
    #- modelOutput: outputs of the models with aggregated means
    #- meas: measurements: column with data in the measurement file (ex: co2)
    #- measurementsSD: measurements Standard Deviation: column with the sd in the measurement file.
    likelihood = function(compound,log=FALSE) {
      
      logdebug(paste("METHOD IN: likelihood$likelihood: -Compound:", compound, sep=" "))
      
      #The normal distribution has density: f(x) = 1/(√(2 π) σ) e^-((Meas - Output(Params))^2/(2 σ^2))
      
      #QUES: why do we use the dnorm function and not other way
      z <- (data.sets[[compound]][,5] - data.sets[[compound]][,3])/data.sets[[compound]][,4]
      likeli <- dnorm(z, log=log)
      
      
      logdebug("METHOD OUT: likelihood$likelihood")
      
      return(likeli)
    },
    #
    # Get the index of the peak in measurements
    #
    getPeakInMeasurements = function(compound) {
      meas <- data.sets[[compound]][,3]
      med <- median(meas)
      logerror(paste("median",med))
      high.indexs <- which(meas > med)
      meas[-high.indexs] <- 0
      index <- peakIndexes(meas)
      return(index)
    },
    
    peaksLikelihood = function(compound,log=FALSE) {
      
      logdebug(paste("METHOD IN: likelihood$peaksLikelihood: -Compound:", compound, sep=" "))
      
      #The normal distribution has density: f(x) = 1/(√(2 π) σ) e^-((Meas - Output(Params))^2/(2 σ^2))
      indexes <- getPeakInMeasurements(compound)

      logerror(indexes)
      logerror(capture.output(data.sets[[compound]][,3][indexes]))
      out <- getOutputMaxs(compound, indexes)
      logerror(out)
      #QUES: why do we use the dnorm function and not other way
      z <- (out - data.sets[[compound]][,3][indexes])/data.sets[[compound]][,4][indexes]
      likeli <- dnorm(z, log=log)
      
      
      logdebug("METHOD OUT: likelihood$peaksLikelihood")
      
      return(likeli)
    },
    
    getOutputMaxs = function(compound, indexes) {
      
      x <- c()
      for (idx in indexes) {
        logerror(paste(compound,"index",idx))
        if (idx > 5) {
          logerror(data.sets[[compound]][,5][c((idx-5):(idx+5))])
          x <- c(x,max(data.sets[[compound]][,5][c((idx-5):(idx+5))]))
        }
        else {
          x <- c(x,data.sets[[compound]][,5][idx])
        }
      }
      
      return(x)
      
    },
    
    peakIndexes = function(x, thresh = 0) {
      
      pks <- which(diff(sign(diff(x, na.pad=F)), na.pad=F) < 0) + 2
      if (!missing(thresh)) {
        pks[x[pks -1] -x[pks] > thresh]
      }
      else pks
    }
    
  )#End methods List 
  
)#End RefClass