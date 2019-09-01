#Responsability:
#Calculate likelihood and log different statistic scores
library(stats)
library(dtw)
#QUES: pensar en lo que dijo Christian del espectrum algo

ScoreDTW <- setRefClass(    
  "scoreDTW"
  
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
      
      logdebug("METHOD IN: likelihood$score")
      
      likeli <- list()
      
      #Workaround: id.log is just to identify the different compound values in log file.
      id.log <- ceiling(runif(1,1,1000))
      
      for (compound in names(data.sets)) {
        
        loginfo(paste("Likelihood for compound ", compound, sep=""))
        
        
        loginfo(capture.output(data.sets[[compound]]))
        
        dtw.value.inv <- dtw.inv(compound)
        
        loginfo(paste("DTW TEST VALUES", dtw.value.inv, ":compound", compound))
        
        likeli[[compound]] <- dtw.value.inv
      }
      
      logdebug("METHOD OUT: likelihood$score")
      
      return(likeli)
      
    },
    
    dtw.inv = function(compound) {
      
      logdebug(paste("METHOD IN: likelihood$dtw: -Compound:", compound, sep=" "))
      
      dtw <- dtw(dist(data.sets[[compound]][,3],data.sets[[compound]][,5]), distance.only=T)$distance
      
      logdebug("METHOD OUT: likelihood$dtw")
      
      return(1/dtw)
    }
    
  )#End methods List 
  
)#End RefClass