#Responsability:
#Recieve measurements and outputs and return a list of data.frames with all values perfectly formed and aggregated

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
#
# Output is just a data.frame with 

Aggregator <- setRefClass(    
  "aggregator"
  
  , fields = list(
    measurements = "list",
    outputs = "data.frame",
    dataDaysRange = "numeric",
    matchCols ="list"
  )
  , methods = list(
    #
    # Constructor
    #
    initialize = function(measurements = list(),
                          dataDaysRange = 1,
                          outputs = data.frame(),
                          matchCols ="list",
                          ...)
    {
      callSuper(..., 
                measurements = measurements,
                dataDaysRange = dataDaysRange,
                outputs = outputs,
                matchCols = matchCols)
    },
    #
    # n should be the data aggregation days
    #
    moving.avg = function(x, n=7) {
      filter(x, rep(1/n,n), sides=2)
    },
    
    #
    # Average without moving/smoothing
    #
    weekly.avg = function(values) {
      
      logdebug("METHOD IN: aggregator$weekly.avg")
      
      values.dt <- data.table(values)
      
      agg.res <- data.frame()
      
      rangesNumr <- round(365/dataDaysRange)
      
      #Nested function used in the by clause of data.table
      #Returns a number for data aggregation by the day range configured.
      julianday <- function(julianday) {
        aggNum <- as.integer((julianday/dataDaysRange)-0.00001)+1
        aggNum[aggNum > rangesNumr] <- rangesNumr
        return(aggNum)
      }
      
      colIdxs <- c(3:length(colnames(values.dt)))
      
      agg.res <- values.dt[, lapply(.SD, mean), by=list(year,julianday(julianday)), .SDcols=colIdxs]
      
      logdebug("METHOD OUT: aggregator$weekly.avg")
      
      return(as.data.frame(agg.res))
    },
    
    getCombination = function() {
      
      logdebug("METHOD IN: aggregator$getCombination")
      
      data <- list()
      
      for (compound in names(matchCols)) {
          
          meas.name <- compound
          output.name <- matchCols[[compound]][[1]]
          
          logdebug(paste("Aggregando",output.name))
          
          meas.values <- measurements[[meas.name]]
          
          colnames(meas.values)[2] <- "julianday"

          output.values <- outputs[,c("year","julianday",output.name)]
          
          logdebug("MEAS")
          logdebug(capture.output(head(meas.values)))
          logdebug("OUTPUT1")
          logdebug(capture.output(head(output.values)))
          #Weekly aggregation
          output.values <- weekly.avg(output.values)
          
          #Aggregation by moving average
          #output.values[,output.name] <- as.numeric(moving.avg(output.values[,output.name], n=dataDaysRange))
      
          #Remove new NA
          output.values <- output.values[!is.na(output.values[,output.name]),]
          logdebug("OUTPUT2")
          logdebug(capture.output(head(output.values)))
          merged.data <- merge(meas.values, output.values, by=c("year","julianday"))
          logdebug("AGGREGATED:")
          logdebug(capture.output(head(merged.data)))
          
          
          #merged.data: year, julianday, measV, measVstd, outputV
          data[[meas.name]] <- merged.data
        
      }
      
      logdebug("METHOD OUT: aggregator$getCombination")
      
      return(data)
      
    }
    
    
    
    
    

    
    
    
    
  )#End methods List 
  
)#End RefClass
