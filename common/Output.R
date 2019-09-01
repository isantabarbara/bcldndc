#Responsability:
#Read and transform the model output
#TODO: add the output in database would be great

Output <- setRefClass(    
  "output"
  , fields = list(
	commonPath="character",
    subfolder="character",
    site="list",
    dataDaysRange="numeric",
    output.sinksource = "character",
    output.files="character",
    output = "data.frame",
    outputAggregated = "data.frame",
    matchCols ="list"
    
  )
  , methods = list(
    #
    # Constructor
    #
    initialize = function(commonPath="",
						  subfolder = "",
                          site=list(),
                          dataDaysRange=1,
                          output.sinksource = "",
                          output.files="",
                          output = data.frame(),
                          outputAggregated = data.frame(),
                          matchCols = list(),
                          ...)
    {
    
      callSuper(..., 
				commonPath=commonPath,
                subfolder=subfolder,
                site=site,
                dataDaysRange=dataDaysRange,
                output.sinksource = output.sinksource,
                output.files=output.files,
                output = output,
                outputAggregated = outputAggregated,
                matchCols = matchCols)
      
      
    },
    #
    #Read the configured output file of the ldndc model
    #Return: the output of the model
    #
    process = function(compounds)
    {
      logdebug("METHOD IN: output$process")
     
      output <<- data.frame()
      tryCatch( {
        loginfo(paste("compounds in output process:", paste(compounds, collapse=",")))
        
        
        findOutputFiles()
        read()
        setCalibrationDays()
      
      },
      error= function(err) {
        logerror(err)
        logerror("Error reading model output.")
      })
      
      
      logdebug("METHOD OUT: output$process")

    },
    read = function() {
      logdebug("METHOD IN: Output$read")

      for (compound in matchCols) {  
        
  		  if (length(compound) > 1) {
  		    
          if (grepl("layer",compound[2])) {
  		      readLayerCompound(compound[1],compound[2])
  		    } else {
  		      readComplexCompound(compound)
  		    }
          
  		  } else {
          
  			readBasicCompound(compound)
  		  }
     
      }
      logdebug(capture.output(head(output)))
      logdebug("METHOD OUT: Output$read")
    },
    
    
    readComplexCompound = function(compound.list) {
      
      logdebug("METHOD IN: Output$readComplexCompound")
      
      first.output.name <- NULL
      temp.out <- NULL
      first <- T
      
      for (output.name in compound.list) {
        
        if (first) {
          
          first.output.name <- output.name
          temp.out <- getTempOutput(output.name)
          
          if (nrow(output) == 0) {
            output <<- temp.out[c("year","julianday")]
          }
          logdebug(head(output))
          output <<- cbind(output, temp.out[first.output.name])
          colnames(output)[ncol(output)] <<- first.output.name
          logdebug(head(output))
          first <- F
          
        } else {
          
          if (is.numeric(output.name)) {
            output[first.output.name] <<- output[first.output.name] * output.name
          } else {
            if (!(output.name %in% colnames(temp.out))) {
              temp.out <- getTempOutput(output.name)
            }
            logdebug(head(output))
            output[first.output.name] <<- output[first.output.name] + temp.out[output.name]
            logdebug(head(output))
          }
        }
        
      }
      
      logdebug("METHOD OUT: Output$readComplexCompound")
    },
    readBasicCompound = function(compound) {
      
		  logdebug("METHOD IN: Output$readBasicCompound")
      
		  output.name <- compound[1]
		
      temp.out <- getTempOutput(output.name)
        
      if (nrow(output) == 0) {
        output <<- temp.out[c("year","julianday")]
		  }
      
      output <<- cbind(output, temp.out[output.name])
		  logdebug(capture.output(head(output)))
      logdebug("METHOD OUT: Output$readBasicCompound")
    },
    readLayerCompound = function(compound,layerfilter) {
      
      logdebug("METHOD IN: Output$readLayerCompound")
      
      output.name <- compound[1]
      filter <- as.numeric(strsplit(layerfilter,"=")[[1]][2])
      
      
      temp.out <- getTempOutput(output.name)
      
      if (nrow(output) == 0) {
        output <<- temp.out[temp.out$layer == filter,][c("year","julianday")]
      }
      
      logdebug(paste("filter", filter))
      logdebug(capture.output(head(temp.out)))
      output <<- cbind(output, temp.out[temp.out$layer == filter,][output.name])
      logdebug(capture.output(head(output)))
      logdebug("METHOD OUT: Output$readLayerCompound")
    },
	getTempOutput = function(output.name) {
		file.name <- output.files[[output.name]]
		loginfo(paste("Reading output file:", file.name, "for compound", output.name))
		temp.out <- read.table(file.name, header=T, dec=".", sep="\t")
		return(temp.out)
	},
    #
    # Find the files with the output values
    #
    findOutputFiles = function() {
      
      logdebug("METHOD IN: Output$findOutputFiles")
      
      folder <- dirname(file.path(commonPath, subfolder, output.sinksource))
      logdebug(folder)
      files <- list.files(path = folder, full.names = TRUE, recursive = FALSE,
                          ignore.case = TRUE, include.dirs = FALSE)
      
      logdebug(files)

      for (compound in matchCols) {
        output.name <- compound[[1]]
        logdebug(paste("search output:",output.name))
        
        for (file in files) {
          if (length(grep(output.name, readLines(file, n=1))) >=  1) {
            logdebug(paste("Output file finded:",file))
            output.files[[output.name]] <<- file
          }
        }
        
        notUniqueOutputWorkaround(folder, output.name)
      }
      
      logdebug(output.files)
      
      logdebug("METHOD OUT: Output$findOutputFiles")
    },
    #
    # I make this workaround to specify output files when col name is not unique
    #
    notUniqueOutputWorkaround = function(folder, output.name) {
      tryCatch({
        if (!is.null(config$fixedoutputfiles[[output.name]])) {
          file <- file.path(folder, config$fixedoutputfiles[[output.name]])
          logdebug(paste("Renew Output file:",file))
          output.files[[output.name]] <<- file
        }
      }, error=function(e) {})
    },
    #
    # Remove the days not configured for calibration
    #
    setCalibrationDays = function() {
      logdebug("METHOD IN: output$setCalibrationDays")
      
      logdebug("output with removal days")
      logdebug(capture.output(head(output)))
      
      #TODO: Check that this works
      for (rm.days in site$removeDays) {
        
        year <- rm.days[1]
        ini.day <- rm.days[2]
        end.day <- rm.days[3]
        days <- c(ini.day:end.day)
        
        rem.index <- which(output$year == year & output$julianday %in% c(ini.day:end.day))
        if (length(rem.index) > 0) {
			    output <<- output[-rem.index,]
		    }
      }
      
      logdebug("output without removal days")
      logdebug(capture.output(head(output)))
      
      logdebug("METHOD OUT: output$setCalibrationDays")
            
    },
   
    
    getNewColumnNames = function(colNames) {
      
      logdebug("METHOD IN: output$getNewColumnNames")
      #Change the output col names with the measurements col names
      newColnames <- colNames
      logdebug(colNames)
      
      for (colindex in 3:length(colNames)) {
        newName <- getMeasurementName(colNames[[colindex]])
        newColnames[[colindex]] <- newName
      }
      
      logdebug("METHOD OUT: output$getNewColumnNames")
      
      return(newColnames)
      
    },
    
    getMeasurementName = function(outputCol) {
      
      index <- 1
      measName <- ""
      
      while (length(measName) == 0 || index <= length(matchCols)) {
        
        if (outputCol %in% matchCols[[index]]) {
          measName <- names(matchCols)[[index]]
        }
        
        index <- index + 1
      }
      
      return(measName)
    },
    
    #
    # In matchCols we have configured the matching between output and measurement names:
    # e.g: n_n2o -> nd_dN2O.kgNha.1.
    # From the measurement names (n_n20, n_no...) we get the output column names: (nd_dN2O.kgNha.1., "nd_dNO.kgNha.1.")
    #
    getOutputColNames = function(compounds) {
      
      logdebug("METHOD IN: Output$getOutputColNames")
      names <- c()
      
      for (compound in compounds) {
        colNames <- getOutputColName(compound)
        names <- c(names, colNames)
      }
      
      logdebug("METHOD OUT: Output$getOutputColNames")
      
      return(names)
    },
    
    getOutputColName = function(compound) {

      return(matchCols[[compound]])
    },
    #
    # Aggregate output data.
    # Returns: list with the output data grupped
    # 
    siteAggregation = function(siteOutput) {

      loginfo("METHOD IN: output$siteAggregation")
      
      siteOutputDT <- data.table(siteOutput)

      combinedOutput <- data.frame()
      
      rangesNumr <- round(365/dataDaysRange)
      
      #Nested function used in the by clause of data.table
      #Returns a number for data aggregation by the day range configured.
      julianday <- function(julianday) {
        aggNum <- as.integer((julianday/dataDaysRange)-0.00001)+1
        aggNum[aggNum > rangesNumr] <- rangesNumr
        return(aggNum)
      }
      
      logdebug("siteOutputDT")
      logdebug(capture.output(head(siteOutputDT)))
 
      meas.names <- names(matchCols)
      
      #TODO: check that it works with outputs not contiguos      
      colIdxs <- getOutputColumnsPositions(colnames(siteOutputDT))
      
      combinedOutput <- siteOutputDT[, lapply(.SD, mean), by=list(year,julianday(julianday)), .SDcols=colIdxs]
      
      output.col.names <- getMatchedOutputColNames(colnames(combinedOutput))
      
      #colnames(combinedOutput) <- output.col.names
      #TODO: check setnames
      setnames(combinedOutput,colnames(combinedOutput),output.col.names)
      
      logdebug("combinedOutput")
      logdebug(capture.output(head(combinedOutput)))
      
      loginfo("METHOD OUT: output$siteAggregation")
      
      #52*year values for each column
      return(combinedOutput)
      
    },
    getOutputColumnsPositions = function(colnames) {
      
      logdebug("METHOD IN: output$getOutputColumnsPositions")
      
      wantedOutputs <- as.character(lapply(matchCols, function(x) x))
      
      colsIdx <- which(colnames %in% wantedOutputs)
      logdebug(colsIdx)
      logdebug("METHOD OUT: output$getOutputColumnsPositions")
      
      return(colsIdx)
    },
    
   
    #
    # Get the measurement col names in correct order for replacing output col names
    #
    getMatchedOutputColNames = function(outcols) {
      loginfo("METHOD IN: output$getMatchedOutputColNames")
      
      logdebug(capture.output(matchCols))
      ordered.meas.cols <- c('id','year','julianday')
      mapply(function(x,idx) ordered.meas.cols[which(outcols == x)]<<-names(matchCols)[idx], matchCols, seq_along(matchCols))
      
      loginfo("METHOD OUT: output$getMatchedOutputColNames")
      return(ordered.meas.cols)
    }

    
    
  )#End methods List 
  
)#End RefClass



