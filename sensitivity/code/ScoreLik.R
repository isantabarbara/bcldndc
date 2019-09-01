#Responsability:
#Calculate likelihood and log different statistic scores
library(stats)

#QUES: pensar en lo que dijo Christian del espectrum algo

ScoreLik <- setRefClass(    
  "scoreLik"
  
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
      totalLikelihood <- NA

      result <- list()

      rnd.num <- sample(1:100,1)
      for (compound in names(data.sets)) {
        
        logdebug(paste("Likelihood for compound ", compound, sep=""))
        
        #Using log because with log=F we cannot sum, we muss multiply, and the values can be really small: log(A*B) = logA + logB.
        likeli <- likelihood(compound, log=TRUE)
        
        
        likeli <- rectifyLikelihood(likeli)
        
        likeli.prod <- median(likeli)

        logdebug(paste(rnd.num, "Prod Likelihood for compound", compound, "is", likeli.prod))
        
       
		result[[compound]] <- abs(1/likeli.prod)
      }
      
      logdebug("METHOD OUT: likelihood$sumLikelihood")
      
      return(result)
      
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
    }
    
  )#End methods List 
  
)#End RefClass
