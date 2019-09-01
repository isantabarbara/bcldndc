#Responsability:
#Read the measurements

# Scheme of how are the measurements structuredin the values variable (a list with data.frames each one):
#
# 
# 
# siteValues (list)          
# +------------+------------+
# |  n_n2o     |  n_no      |
# |------------|------------|
# |meas (d.f)  |meas (d.f)  |
# |+--+--+--+-+|+--+--+--+-+|
# ||  |  |  | |||  |  |  | ||
# ||--|--|--|-|||--|--|--|-||
# ||1 |65|98|9|||44|98|33|7||
# ||34|68|89|9|||65|34|67|7||
# |+--+--+--+-+|+--+--+--+-+|
# +------------+------------+
# 

Measurements <- setRefClass(    
  "measurements"
  
  , fields = list(
      site = "list",
      dataDaysRange = "numeric",
      values = "list",
      valuesAggregated = "list",
      valuesColumnIndex = "numeric"
  )
  , methods = list(
    #
    # Constructor
    #
    initialize = function(site = list(),
						              dataDaysRange = 1,
                          values=list(),
                          valuesAggregated=list(),
                          valuesColumnIndex = 3,
                          ...)
    {
      callSuper(..., 
                site = site,
                dataDaysRange = dataDaysRange,
                values = values,
                valuesAggregated = valuesAggregated,
                valuesColumnIndex = valuesColumnIndex)
      
      
    },
    getFilesToRead = function() {
      logdebug("METHOD IN: measurements$getFilesToRead")
      
      meas.path <- file.path(site$measFolder, site$measSubfolder)
      
      files <- c()
      
      for (file.name in site$measFiles) {
        files <- c(files, list.files(path = meas.path, pattern = file.name, full.names = T, recursive = T,
                                     ignore.case = T, include.dirs = F))
      }
      
      logdebug(files)
      files <- files[grepl("PARSED", files)]
      
      logdebug("METHOD OUT: measurements$getFilesToRead")
      
      return(files)
      
    },
    #
    # Read the configured measurements files
    # Return: the measurements
    #
    readMeasurements = function() {
      
      logdebug("METHOD IN: measurements$readMeasurements")
      
      files <- getFilesToRead()
          
      for (file in files) {
  
            logdebug(paste("Read measurement file: ", file, sep=" "))
            meas <- read.table(file, header=T, sep="\t",dec=".")
            
            measName <- colnames(meas)[[valuesColumnIndex]]

            values[[measName]] <<- meas

            #CHECKED!: Measurements are aggregated correctly
            #write.table(valuesAggregated[[measName]], file = paste0("meas",measName,proc.time()[3],".csv"), row.names=F)

      }
      
      logdebug("METHOD OUT: measurements$readMeasurements")
     
      #including all years and all measurements
      return(values)
      
    },
    #
    # Overwrite the measurements with average values
    #
    aggregate = function() {
      #years <- getMeasurementsYears()
      logdebug("METHOD IN: measurements$aggregate")
      
      for (measName in names(values)) {
        
        meas <- values[[measName]]
        years <- as.numeric(unique(meas$year))
        values[[measName]] <<- weeklyAverage(meas)
        logdebug(head(values[[measName]]))
      }
      

     
      logdebug("METHOD OUT: measurements$aggregate")
    },
    #
    # Get the peaks, ignoring the values under the prob quantile
    #
    peaks = function(probs.num=0.50) {
      logdebug("METHOD IN: measurements$peaks")
      
      for (measName in names(values)) {
        
        meas <- values[[measName]]
        idx <- getHighPeaks(meas[,3], probs.num=probs.num)
        meas <- meas[idx,]
        values[[measName]] <<- meas
      }
      
      logdebug("METHOD OUT: measurements$peaks")
    },
    
    highValues = function(probs.num=0.50) {
      logdebug("METHOD IN: measurements$highValues")
      
      for (measName in names(values)) {
        logdebug(paste("high values for",measName, probs.num))
        logdebug(capture.output(head(values)))
        meas <- values[[measName]]
        logdebug(capture.output(meas))
        med <- quantile(meas[,3], probs=probs.num)
        logdebug(paste("quantile", med))
        high.indexs <- which(meas[,3] > med)
        logdebug(paste("here1"))
        meas <- meas[high.indexs,]
        logdebug(paste("here2"))
        values[[measName]] <<- meas
      }
      
      logdebug("METHOD OUT: measurements$highValues")
    },
    #
    # We set the same number of peaks than valley values
    #
    proportionate = function(probs.num=0.50) {
      logdebug("METHOD IN: measurements$highValues")
      
      for (measName in names(values)) {
        logdebug(paste("high values for",measName, probs.num))
        logdebug(capture.output(head(values)))
        is.fake <- grepl("fake",measName)
        meas <- values[[measName]]
        logdebug(capture.output(meas))
        med <- quantile(meas[,3], probs=probs.num)
        logerror(paste("quantile", med))
        high.indexs <- which(meas[,3] > med)
        low.indexs <- which(meas[,3] <= med)
       
        
        while ((length(low.indexs)/length(high.indexs) > 1.3) && !is.fake) {
          
          if (length(low.indexs)/length(high.indexs) < 2) {
            low.indexs <- low.indexs[which(which(low.indexs==low.indexs) %% 5 != 0)]
          } else {
            #Remove half of the low indexes
            low.indexs <- low.indexs[which(which(low.indexs==low.indexs) %% 2 != 0)]
          }
        }
        
        logdebug(paste("Number of aggregated measurements: Low:", length(low.indexs), "High:", length(high.indexs)))
         
        indexs <- c(high.indexs, low.indexs)
        
        meas <- meas[indexs,]
        
        values[[measName]] <<- meas
      }
      
      logdebug("METHOD OUT: measurements$highValues")
    },
    
    #
    # We set the same number of peaks than valley values
    #
    proportionateByYear = function(probs.num=0.50) {
      logdebug("METHOD IN: measurements$proportionateByYear")
      
      for (measName in names(values)) {
        logdebug(paste("high values for",measName, probs.num))
        
        all.meas <- values[[measName]]
        logdebug(capture.output(head(all.meas)))
        
        all.index <- c()
        new.all.meas <- NULL
        for (year in unique(all.meas$year)) {
          meas <- all.meas[all.meas$year == year,]
          med <- quantile(meas[,3], probs=probs.num)
          logerror(paste("quantile", med))
          high.indexs <- which(meas[,3] > med)
          low.indexs <- which(meas[,3] <= med)
          
          logdebug(paste("lowlen:",length(low.indexs),"highlen",length(high.indexs)))
          
          its <- 0
          proportion <- 1.3
          while ((length(low.indexs)/length(high.indexs) > proportion)) {
            
            if (length(low.indexs)/length(high.indexs) < 2) {
              low.indexs <- low.indexs[which(which(low.indexs==low.indexs) %% 5 != 0)]
            } else {
              #Remove half of the low indexes
              low.indexs <- low.indexs[which(which(low.indexs==low.indexs) %% 2 != 0)]
            }
            
            logdebug(paste("lowlen:",length(low.indexs),"highlen",length(high.indexs)))
            if (its > 3) {
              proportion <- proportion + 0.1
            }
            its <- its + 1
          }
          
          
          logerror(paste("Number of aggregated measurements: Year:", year, "Low:", length(low.indexs), "High:", length(high.indexs)))  
          indexs <- c(high.indexs, low.indexs)
          if (is.null(new.all.meas)) {
            new.all.meas <- meas[indexs,]
          } else {
            new.all.meas <- rbind(new.all.meas, meas[indexs,])
          }
          #all.index <- c(all.index, indexs)
        }
        
        #all.meas <- all.meas[all.index,]
        
        values[[measName]] <<- new.all.meas
      }
      
      logdebug("METHOD OUT: measurements$proportionateByYear")
    },
    #
    # Remove from measurements years outside the selected interval
    #
    removeYears = function(ini, end) {
      
      logdebug("METHOD IN: measurements$removeYears")
      
      for (measName in names(values)) {
        
        meas <- values[[measName]]
        
        valid.indexs <- which(meas$year >= ini & meas$year <= end)
        meas <- meas[valid.indexs,]
        
        values[[measName]] <<- meas
        
      }
      
      logdebug("METHOD OUT: measurements$removeYears")
    },
    extremes = function() {
      logdebug("METHOD IN: measurements$extremes")
      
      for (measName in names(values)) {
        
        meas <- values[[measName]]
        idx <- getHighPeaks(meas[,3])
        idx.low <- getLowPeaks(meas[,3])
        
        meas <- meas[c(idx,idx.low),]
        values[[measName]] <<- meas
      }
      
      logdebug("METHOD OUT: measurements$extremes")
    },
    #
    # Get the index of the peak in measurements above 0.75 quantile
    #
    getHighPeaks = function(data, probs.num=0.75) {
      logerror("METHOD IN: measurementParer$getHighPeak")
      meas <- data
     
      med <- quantile(meas, probs=probs.num)
      logerror(paste("quantile",med))
      high.indexs <- which(meas > med)
  
      meas[-high.indexs] <- 0
      index <- peakIndexes(meas)

      logerror("METHOD OUT: measurementParer$getHighPeak")
      
      return(index)
    },
    
    
    peakIndexes = function(x, thresh = 0) {
      logerror("METHOD IN: measurementParer$peakIndexes")
      pks <- which(diff(sign(diff(x, na.pad=F)), na.pad=F) < 0) + 2
      if (!missing(thresh)) {
        pks[x[pks -1] -x[pks] > thresh]
      }
      else pks
      
      return(pks-1)
    },
    
    getLowPeaks = function(data, probs.num=0.50) {
      logdebug("METHOD IN: measurementParer$getLowPeaks")
      meas <- data
      
      med <- quantile(meas, probs=probs.num)
   
      
      high.indexs <- which(meas > med)
      meas[-high.indexs] <- 0
      index <- inv.peakIndexes(meas)
      
      logdebug("METHOD OUT: measurementParer$getLowPeaks")
      
      return(index)
    },
    
    inv.peakIndexes = function(x, thresh = 0) {
      
      pks <- which(diff(sign(diff(x, na.pad=F)), na.pad=F) > 0) + 2
      if (!missing(thresh)) {
        pks[x[pks -1] -x[pks] > thresh]
      }
      else pks
    },
    
    discretize = function(probs.num=0.50) {
      
     
      for (measName in names(values)) {
        
        meas <- values[[measName]]
        
        med <- quantile(meas[,3], probs=probs.num)
        
        period <- peaksMedianPeriod(meas, probs.num=0.75)
        logerror(paste("Removing all data but maximun and minimum from periods of",period,"for compound",measName))
        
        
        years <- as.numeric(unique(meas$year))
        
        indexes <- maxMinFilter(meas, years, period)
        
        meas <- meas[indexes,]
        meas <- meas[meas[,3] > med,]
        
        values[[measName]] <<- meas
        
      
      }
      
    },
     
    peaksMedianPeriod = function(meas, probs.num=0.50) {
      logerror("METHOD IN: measurementParer$peaksMedianPeriod")
      idx <- getHighPeaks(meas[,3], probs.num=probs.num)
      meas$date <- strptime(paste(meas$year, meas$day), "%Y %j")

      dates <- meas[idx,]$date
      periods.between.peaks <- c()
      for (i in 1:length(dates)) {

        if (i < length(dates)) {

          periods.between.peaks <- c(periods.between.peaks, (dates[i+1] - dates[i]))
        }
      }
      
      med.period <- median(periods.between.peaks)
      
      logerror("METHOD OUT: measurementParer$peaksMedianPeriod")
      
      
      
      return(med.period)
      
      
    },
    
    #
    # Get the years we have for all the measurements
    #
    getMeasurementsYears = function() {

      logdebug("METHOD IN: measurements$getMeasurementsYears")
      
     # years <- c()
      
    #  for (measurements in values) {
    #      years <- c(years, levels(factor(measurements$year)))
    #  }
      
     # years <- as.numeric(levels(factor(years)))
      #years <- as.numeric(unique(data$year))
      logdebug("METHOD OUT: measurements$getMeasurementsYears")
      
      return(years)
    }, 
    #
    # Get the unique names of the measurements values columns (ej: n_n2o, n_no...)
    #
    getMeasurementsNames = function() {

      logdebug("METHOD IN: measurements$getMeasurementsNames")
      
      names <- c()
      
      for (meas in values) {
          name <- names(meas)[[valuesColumnIndex]]
          
          names <- c(names, name)
      }
      
      logdebug("METHOD OUT: measurements$getMeasurementsNames")
      
      return(unique(names))
    }, 
    #TODO: We should save the aggregated measurements in a file, to check future errors
    aggregation = function(meas, years) {
      
      logdebug("METHOD IN: measurements$aggregation")
      
      aggMean <- c()
      aggSD <- c()
      
      
      sdColumnIndex <- valuesColumnIndex + 1
      
      for(year in years) {
        
        aggregationsInTheYear <- round(365/dataDaysRange)
        for (aggNum in 0:(aggregationsInTheYear-1)) {
          
          combinedMeasurements <- getAggregatedMeasurementsData(meas, aggNum, year)
          
          #na.rm=TRUE, if there is a lack some day we can ignore it
          aggmean <- mean(combinedMeasurements[[valuesColumnIndex]],na.rm=TRUE)
          #The NA sd should be replace by a value during the parsing process
          combinedVariancesMean <- mean((combinedMeasurements[[sdColumnIndex]])^2,na.rm=TRUE)
          aggsd <- sqrt(combinedVariancesMean)
          
          aggMean   <- c(aggMean, aggmean)
          aggSD <- c(aggSD, aggsd)
          
        }
      }
      
      logdebug("METHOD OUT: measurements$aggregation")
      
      return(list(meas=aggMean, measSD=aggSD))
      
    },
    #
    # Aggregate data with a moving average criteria
    #
    movingAverage= function(values, years) {
      
      logdebug("METHOD IN: measurements$movingAverage")

      aggMean <- c()
      aggSD <- c()
      aggDays <- c()
      aggYears <- c()
      
      sdColumnIndex <- valuesColumnIndex + 1
      
     
      for(year in years) {
        
        for (prevDay in c(0:364)) {
          
          combinedMeasurements <- movingAverageAux(values, prevDay, year)
          
          #na.rm=TRUE, if there is a lack some day we can ignore it
          aggmean <- mean(combinedMeasurements[[valuesColumnIndex]],na.rm=TRUE)
          #The NA sd should be replace by a value during the parsing process
          combinedVariancesMean <- mean((combinedMeasurements[[sdColumnIndex]])^2,na.rm=TRUE)
          aggsd <- sqrt(combinedVariancesMean)
          
          if (!is.na(aggmean)) {
            aggMean   <- c(aggMean, aggmean)
            aggSD <- c(aggSD, aggsd)
            aggDays <- c(aggDays,prevDay+1)
            aggYears <- c(aggYears, year)
          }
        }
      }
      
      
      new.values <- data.frame(year=aggYears, day=aggDays, value=aggMean, sd=aggSD)
      logdebug("finish aggregation")
      logdebug(capture.output(new.values))
      
      colnames(new.values) <- colnames(values)
      
      logdebug("METHOD OUT: measurements$getMovingAverage")
      
      return(new.values)
    },
    movingAverageAux = function(values, prevStartDay, year) {
     
      #logdebug("METHOD IN: measurements$movingAverageAux")
      
      startDay <- prevStartDay + 1
      endDay <- prevStartDay + dataDaysRange
      
      selectorCriteria <-(values$year == year
                          & values$day >= startDay 
                          & values$day <= endDay)
      
      aggMeasurements <- values[selectorCriteria,]
      
      #logdebug(capture.output(aggMeasurements))
      
     # logdebug("METHOD OUT: measurements$movingAverageAux")
      
      return(aggMeasurements)
    },
    
    getAggregatedMeasurementsData = function(meas, aggNumber, year) {
      
      #logdebug("METHOD IN: measurements$getAggregatedMeasurementsData")

      startDay <- 1 + aggNumber*dataDaysRange
      endDay <- dataDaysRange + aggNumber*dataDaysRange

      selectorCriteria <-(meas$year == year
                          & meas$day >= startDay 
                          & meas$day <= endDay)
  
      aggMeasurements <- meas[selectorCriteria,]

      #logdebug("METHOD OUT: measurements$getAggregatedMeasurementsData")
      
      return(aggMeasurements)
    },
    
    maxMinFilter = function(meas, years, period) {
      
      logdebug("METHOD IN: measurements$aggregation")
      
      indexes <- c()
    
      for(year in years) {
        
        aggregationsInTheYear <- round(365/period)
        for (aggNum in 0:(aggregationsInTheYear-1)) {
          
          indexes <- c(indexes,getMaxMinDataIndex(meas, aggNum, year))

        }
      }
      
      logdebug("METHOD OUT: measurements$aggregation")
      
      return(indexes)
      
    },
    
    getMaxMinDataIndex = function(meas, aggNumber, year) {
      
      #logdebug("METHOD IN: measurements$getAggregatedMeasurementsData")
      
      startDay <- 1 + aggNumber*dataDaysRange
      endDay <- dataDaysRange + aggNumber*dataDaysRange
      
      selectorCriteria <-(meas$year == year
                          & meas$day >= startDay 
                          & meas$day <= endDay)
      
      max.val <- max(meas[selectorCriteria,3])
      min.val <- median(meas[selectorCriteria,3])
      
      maxCriteria <-(meas$year == year
                          & meas$day >= startDay 
                          & meas$day <= endDay
                          & meas[,3] == max.val)
      
      minCriteria <-(meas$year == year
                     & meas$day >= startDay 
                     & meas$day <= endDay
                     & meas[,3] == min.val)
      
      max.idx <- which(maxCriteria)
      min.idx <- which(minCriteria)
      
      
      #logdebug("METHOD OUT: measurements$getAggregatedMeasurementsData")
      
      return(c(max.idx,min.idx))
    },
    #
    # Average without moving/smoothing
    #
    weeklyAverage = function(values) {

      logdebug("METHOD IN: measurements$weeklyAverage")
      
      values.dt <- data.table(values)
      
      agg.res <- data.frame()
      
      rangesNumr <- round(365/dataDaysRange)
      
      #Nested function used in the by clause of data.table
      #Returns a number for data aggregation by the day range configured.
      day <- function(day) {
        aggNum <- as.integer((day/dataDaysRange)-0.00001)+1
        aggNum[aggNum > rangesNumr] <- rangesNumr
        return(aggNum)
      }

      colIdxs <- c(valuesColumnIndex:length(colnames(values.dt)))
      
      agg.res <- values.dt[, lapply(.SD, mean), by=list(year,day(day)), .SDcols=colIdxs]
      
      logdebug("METHOD OUT: measurements$weeklyAverage")
  
      return(as.data.frame(agg.res))
    }
    
  
    
  )#End methods List 
  
)#End RefClass
