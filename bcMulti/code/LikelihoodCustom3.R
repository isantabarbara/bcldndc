#Responsability:
#Calculate the likelihood comparing outputs with measurements
library(stats)

#QUES: pensar en lo que dijo Christian del espectrum algo


#TODO: make the measurement and output independent and make available in a likelihood the mix of different aggregation likelihoods.
Likelihood <- setRefClass(    
  "likelihood"
  
  , fields = list(
    data.sets="list",
    summary="data.frame"
  )
  , methods = list(
    #
    # Constructor
    # 
    initialize = function(..., 
                          data.sets = data.sets,
                          summary = data.frame(matrix(ncol=3)))
    {
      logdebug("Initialize Likelihood")
      callSuper(...,
                data.sets = data.sets,
                summary = summary)
      
    },
    
    sumLikelihood = function() {
      
      logdebug("METHOD IN: likelihood$sumLikelihood")
      
      compounds.likeli <- c()
      
      #Penalization if the nitrogen balance is affected
      balance.pen <- c(1)
      
      for (compound in names(data.sets)) {
        
        if (!grepl("fake",compound)) {
        
          loginfo(paste("Likelihood for compound ", compound, sep=""))
          
          
          #Using log because with log=F we cannot sum, we muss multiply, and the values can be really small: log(A*B) = logA + logB.
          likeli <- likelihood(compound)
          
          #We calculate the mean because if not the compound with more measurements can have bigger weight
          #TODO: If I remove the -Inf I have the problem that the lik mean can be better for a simulation with many -Inf
          likeli <- mean(likeli)
          #NO and N2O has double influence
          if (grepl("n_n",compound)) {
            likeli <- likeli*2
          } else if (grepl("agb",compound)) {
            likeli <- likeli*5
          }
          
          loginfo(paste("Compound:", compound, "Mean value:",likeli))
          colnames(summary) <<- c("compound","value","detail")
          summary <<- rbind(summary,c(compound,likeli,paste0(compound,"L")))
          
          
          compounds.likeli <- c(compounds.likeli, likeli)
      } else {
        balance.pen <- c(balance.pen,penalization(compound))
      }
      }
      
      summary <<- summary[-1,]
      #loginfo("Likelihoods")
      #loginfo(capture.output(measurementsLikeli))
      
      mean.likeli <- mean(compounds.likeli)
      totalLikelihood <- 1/mean.likeli
      loginfo(paste("Mean total likelihood:", mean.likeli, "Inverse:", totalLikelihood))
      
      logdebug("METHOD OUT: likelihood$sumLikelihood")
      
      totalLikelihood <- totalLikelihood/(2*median(balance.pen))
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
    
    peakIndexes = function(x, thresh = 0) {
      #logerror("METHOD IN: measurementParer$peakIndexes")
      pks <- which(diff(sign(diff(x, na.pad=F)), na.pad=F) < 0) + 2
      if (!missing(thresh)) {
        pks <- pks[x[pks -1] -x[pks] > thresh]
      }
      else pks
      
      return(pks-1)
    },

    # loglikelihood with normal distribution (not sivia function)
    #- modelOutput: outputs of the models with aggregated means
    #- meas: measurements: column with data in the measurement file (ex: co2)
    #- measurementsSD: measurements Standard Deviation: column with the sd in the measurement file.
    likelihood = function(compound) {
      
      logdebug(paste("METHOD IN: likelihood$likelihood: -Compound:", compound, sep=" "))
      #logdebug(capture.output(head(data.sets[[compound]])))
      #dt <- data.table(data.sets[[compound]], key='year')
      dt <- data.sets[[compound]]
      logdebug(capture.output(head(dt)))
      
      meas.mean <- mean(dt[,3])
      dayR <- config$dataDaysRange
      seasons <- list(c(0,78),c(79,170),c(171,262),c(263,366))
      mae.values <- c()
      magnitude.penalization <- c()
      
      for (year in unique(dt$year)) {
        logdebug(paste("Analyzing year:", year, compound))
        meas.median <- median(dt[dt$year == year,][,3])
        logdebug(paste("MEDIAN",meas.median))
        
        dt.year <- dt[dt$year == year,]
        
        #We analize peaks for each season
        mae.seasons.pks <- c()
        for (season in seasons) {
          season <- round(season/dayR)
          # loginfo(capture.output(head(dt.year)))
          dt.season <- dt.year[dt.year$julianday >= season[1] & dt.year$julianday <= season[2],]
          loginfo(paste("Analyzing year and season:", year, season[1], compound))
          if (nrow(dt.season) > 0) {
            idx.meas.season.pks <- which(dt.season[,3] >= quantile(dt.season[,3], 0.999))
            dt.meas.season.peaks <- dt.season[idx.meas.season.pks,]
            mae.dt.meas.season.peaks <-  mae(dt.meas.season.peaks[,3],dt.meas.season.peaks[,5])
            mae.seasons.pks <- c(mae.seasons.pks, (mae.dt.meas.season.peaks/meas.mean))
            
            #We analize the magnitud of meas and out peaks without matching
            max.season.meas <- max(dt.season[,3])
            max.season.output <- max(dt.season[,5])
            if ((max.season.output > max.season.meas*2) | (max.season.output < max.season.meas/2)) {
              #msg <- paste(compound, "There is a year with DATASET Max Output:", max.season.output, " 2 times different than MAX measurement:", max.season.meas, ". General Penalization")
              #logerror(msg)
              magnitude.penalization <- c(magnitude.penalization, 1.3)
            } else {
              magnitude.penalization <- c(magnitude.penalization, 1)
            }
          }
        }
        mae.seasons.pks <- mean(mae.seasons.pks)
        
        
        
        #idx.meas.pks <- peakIndexes(dt[dt$year == year & dt[,3] >= meas.median,][,3]) #We get the indexes of the peaks for measurements
        idx.meas.pks <- which(dt.year[,3] >= quantile(dt.year[,3], 0.99))
        #idx.meas.pks <- peakIndexes(dt.year[,3]) #We get the indexes of the peaks for measurements
        #idx.out.pks <- peakIndexes(dt.year[,5]) #We get the indexes of the peaks for outputs
        idx.out.pks <- which(dt.year[,5] >= quantile(dt.year[,5], 0.99))
        
        same.peaks <- (sum(idx.meas.pks == idx.out.pks) == length(idx.out.pks))
        dt.meas.peaks <- dt.year[idx.meas.pks,]
        dt.out.peaks <- dt.year[idx.out.pks,]
        
        
        max.dt.meas.peaks <- max(dt.meas.peaks[,3])
        min.dt.meas.peaks <- min(dt.meas.peaks[,3])
        max.dt.out.peaks <- max(dt.out.peaks[,3])
        min.dt.out.peaks <-  min(dt.out.peaks[,3])
        max.dt.year <- max(dt.year[,3])
        min.dt.year <- min(dt.year[,3])
        mae.dt.meas.peaks <-  mae(dt.meas.peaks[,3],dt.meas.peaks[,5])
        mae.dt.out.peaks <-   mae(dt.out.peaks[,3],dt.out.peaks[,5])
        mae.dt.year <-  mae(dt.year[,3],dt.year[,5])
        
        
        
        norm.dt.meas.peaks <- (max.dt.meas.peaks - min.dt.meas.peaks)
        if (norm.dt.meas.peaks == 0) {
          norm.dt.meas.peaks <- max.dt.meas.peaks
        }
        
        norm.dt.out.peaks <- (max.dt.out.peaks - min.dt.out.peaks)
        if (norm.dt.out.peaks == 0) {
          norm.dt.out.peaks <- max.dt.out.peaks
        }
        
        norm.dt.year <- (max.dt.year - min.dt.year)
        if (norm.dt.year == 0) {
          norm.dt.year <- max.dt.year
        }
        
        
        
        loginfo(paste("Compound:",compound,"Year:",year,"Max - Min meas in meas peak",max.dt.meas.peaks,min.dt.meas.peaks))
        loginfo(paste("Compound:",compound,"Year:",year,"mae meas peaks",mae.dt.meas.peaks))
        loginfo(paste("Compound:",compound,"Year:",year,"Max - Min meas in out peak",max.dt.out.peaks,min.dt.out.peaks))
        loginfo(paste("Compound:",compound,"Year:",year,"mae out peaks",mae.dt.out.peaks))
        loginfo(paste("Compound:",compound,"Year:",year,"Same peaks in out and meas:",same.peaks))
        loginfo(paste("Compound:",compound,"Year:",year,"Max - Min meas",max.dt.year,min.dt.year))
        loginfo(paste("Compound:",compound,"Year:",year,"mae year",mae.dt.year))
        loginfo(paste("Compound:",compound,"Mean of all measurements", meas.mean))
        
        
        mae.values <- c(mae.values, mae.seasons.pks)
        
        
        idx.meas.max <- which(dt.year[,3] == max(dt.year[,3]))
        dt.meas.max <- dt.year[idx.meas.max,]
        meas.magnitud.dev <- 1
        if ((dt.meas.max[,5] > dt.meas.max[,3]*2) | (dt.meas.max[,5] < dt.meas.max[,3]/2)) {
          logerror(paste("Compound:",compound,"Year:",year,"Max measurement is 5 time different than output. We penalize it doubling the Nmae meas peaks"))
          meas.magnitud.dev <- 2
        }
        
        idx.out.max <- which(dt.year[,5] == max(dt.year[,5]))
        dt.out.max <- dt.year[idx.out.max,]
        out.magnitud.dev <- 1
        if ((dt.out.max[,5] > dt.out.max[,3]*2) | (dt.out.max[,5] < dt.out.max[,3]/2)) {
          logerror(paste("Compound:",compound,"Year:",year,"Max output is 5 time different than measurement. We penalize it doubling the Nmae out peaks"))
          out.magnitud.dev <- 2
        }
        
        mae.values <- c(mae.values, (mae.dt.meas.peaks/meas.mean)*meas.magnitud.dev)
        if (!same.peaks) {
          #If output and measurements peaks are not the same we count the two errors
          mae.values <- c(mae.values, (mae.dt.out.peaks/meas.mean)*out.magnitud.dev)
        }
        mae.values <- c(mae.values, mae.dt.year/meas.mean)
        
        loginfo(paste("Compound:",compound,"Year:",year,"Nmae Seasons peaks",mae.seasons.pks))
        loginfo(paste("Compound:",compound,"Year:",year,"Nmae meas peaks",(mae.dt.meas.peaks/meas.mean)*meas.magnitud.dev))
        loginfo(paste("Compound:",compound,"Year:",year,"Nmae out peaks",(mae.dt.out.peaks/meas.mean)*out.magnitud.dev))
        loginfo(paste("Compound:",compound,"Year:",year,"Nmae year",mae.dt.year/meas.mean))
        
        sum.row <- c(compound,mae.seasons.pks,paste0(compound,"_",year,"_NMAESeasonsPeaks"))
        summary <<- rbind(summary,sum.row)
        sum.row <-  c(compound,(mae.dt.meas.peaks/meas.mean)*meas.magnitud.dev,paste0(compound,"_",year,"_NMAEMeasPeaks"))
        summary <<- rbind(summary,sum.row)
        sum.row <- c(compound,(mae.dt.out.peaks/meas.mean)*out.magnitud.dev,paste0(compound,"_",year,"_NMAEOutPeaks"))
        summary <<- rbind(summary,sum.row)
        sum.row <-  c(compound,mae.dt.year/meas.mean,paste0(compound,"_",year,"_NMAEYear"))
        summary <<- rbind(summary,sum.row)
      }
      
      #For likelihood summary
      sum.rmse <- rmse(dt[,5],dt[,3])
      sum.row <-  c(compound,sum.rmse,paste0(compound,"_RMSE"))
      summary <<- rbind(summary,sum.row)
      sum.nrmse <- nrmse(dt[,5],dt[,3])
      sum.row <-  c(compound,sum.nrmse,paste0(compound,"_NRMSE"))
      summary <<- rbind(summary,sum.row)
      sum.mae <- mae(dt[,5],dt[,3])
      sum.row <-  c(compound,sum.mae,paste0(compound,"_MAE"))
      summary <<- rbind(summary,sum.row)
      tryCatch({
        data.lm <- lm(dt[,colnames(dt)[5]] ~ dt[,colnames(dt)[3]], data=dt)
        sum.r.squared <- summary(data.lm)$r.squared
        sum.row <-  c(compound,sum.r.squared,paste0(compound,"_R2"))
        summary <<- rbind(summary,sum.row)
      }, error=function(e) {
        logerror(paste("Error calculating r.squared", compound, e))
        
      })
      
      #No queremos cumulativo, para evitar que el desfase en una zona compense otra.
      #Dividimos por anios.
      
      #Cada anio se divide en tres partes: picos, medianas y valles
      #Buscamos para cada parte los coefficientes R2 y su S que nos indica cuan mierda es
      #(Vale la pena hacer todos los anios juntos? Para evitar que se empiecen a aceptar parametros que mejoren mucho un anio y jodan el resto)
      
      #We apply the magnitude penalizations for outliers
      magnitud.dev <- mean(magnitude.penalization)
      logerror(paste("Season magnitud penalization:", magnitud.dev))
      mae.values <- mae.values * magnitud.dev
      #logdebug("METHOD OUT: likelihood$likelihood")
      
      return(mae.values)
      
    },
    
    # Analyze the year sum or mean of nitrogen balance compounds and set a penalization
    #
    #
    #
    penalization = function(compound) {
      
      logdebug(paste("METHOD IN: likelihood$likelihood: -Compound:", compound, sep=" "))
      #logdebug(capture.output(head(data.sets[[compound]])))
      #dt <- data.table(data.sets[[compound]], key='year')
      dt <- data.sets[[compound]]
      logdebug(capture.output(head(dt)))
      
      year.pen <- c(1)
      for (year in unique(dt$year)) {
        logdebug(paste("Analyzing year:", year, compound))
        meas.sum.min <- sum(dt[dt$year == year,][,3])
        meas.sum.max <- sum(dt[dt$year == year,][,4])
        output.sum <- sum(dt[dt$year == year,][,5])
        loginfo(paste("Balance numbers for year and comp:", year,compound,". Min:",meas.sum.min, ". Max:",meas.sum.max,". Outsum:",output.sum))
        if ((output.sum < meas.sum.min) | (output.sum > meas.sum.max)) {
          year.pen <- c(year.pen,10)
        }
      }
      
      pena <- median(year.pen)
      loginfo(paste("Penalization", year, compound, pena))
      return(pena)
      
    },
    
    likSummary = function() {
      logdebug("METHOD IN: likelihood$summary")
      loginfo(capture.output(summary))
      return(summary)
      
    },
    #
    # Get the likelihood of the cumulative values
    #
    cumulativeLikelihood = function(compound, log=FALSE) {
      #logdebug("METHOD IN: likelihood$cumulativeLikelihood")
      
      
      cum.outputs <- data.sets[[compound]][,5]
      years <- unique(data.sets[[compound]][,1])
      
      new.cum.outputs <- c()
      spls <- split(cum.outputs, ceiling(seq_along(cum.outputs)/5))
      for (spl in spls) {
        new.cum.outputs <- c(new.cum.outputs,cumsum(spl))  
      }
      for (year in years) {
        data.sets[[compound]][data.sets[[compound]][,1] == year,][,5]
        
        #sort(c(2323,231,123123,2323,4,5,31231,6,7,212))[1:6]
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
    
    
    
    peaks = function(sub.data.set) {
      
      
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
