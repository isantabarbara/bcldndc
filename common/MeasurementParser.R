#What this parser does?

#Take a measurement file with the format:

'%units
s_co2 = "umol:s-1:m-2"
s_co2_std = "umol:s-1:m-2"

%data
date time s_co2 s_co2_std  
2006-12-12  12:00	0.47	0.22
...
'

#And parse it to the format:

'year  day	n_co2	n_co2_std
2006	365	0.47	0.22
...
'
#TODO: Adapt to the new measurement files.
MeasurementParser <- setRefClass(    
  "measurementparser"
  
  , fields = list(
    site="list",
    forceSDValue = "logical",
    fixSDValue = "numeric",
    unknownSD="numeric",
    years="numeric"
  )
  , methods = list(
    #
    # Constructor
    #
    initialize = function(site=list(),
                          unknownSD = 0.3,
                          fixSDValue = 2,
                          forceSDValue = FALSE,
                          years = 0,
                          ...)
    {
      callSuper(..., 
                site = site,
                unknownSD = unknownSD,
                fixSDValue = fixSDValue,
                forceSDValue = forceSDValue,
                years = years)
    },
    getFilesToParse = function() {
      logdebug("METHOD IN: measurementParser$getFilesToParse")
      
      meas.path <- file.path(site$measFolder, site$measSubfolder)
      
      files <- c()
      
      for (file.name in site$measFiles) {
        files <- c(files, list.files(path = meas.path, pattern = file.name, full.names = T, recursive = T,
                 ignore.case = T, include.dirs = F))
      }
      
      files <- files[!grepl("PARSED", files)]
      logdebug("METHOD OUT: measurementParser$getFilesToParse")
      
      return(files)
      
    },
    #
    # 
    #
    modifyFiles = function() {
      
      logdebug("METHOD IN: measurementParser$execute")
      
      
      files <- getFilesToParse()
      logdebug(files)
      
      for (file in files) {
        
          logdebug(paste("Parsing file", file, sep=" "))
 
          convFactor <- getConversionFactor(file)
          
          skip.lines <- which(grepl("%data",readLines(con=file)))
          
          meas <- read.table(skip=skip.lines, file=file, header=T, sep = "")
          logdebug(meas)
          oldNames <- colnames(meas)
          parsed <- data.frame()
          
          #We get the min SD 
          minSD <- min(meas[meas[,4] != -99.99,4])
          #We get the sd of all meas values
          logdebug(paste("check"))
          logdebug(capture.output(head(meas)))
          logdebug(paste("check2"))
          date1 <- as.Date(paste0(years[1],"-01-01"),"%Y-%m-%d")
          date2 <- as.Date(paste0(years[2],"-01-01"),"%Y-%m-%d")
          logdebug(capture.output(head(meas[(meas$date >= date1) & (meas$date <= date2) & (meas[,3] != -99.99),3])))
         
          meas.SD <- sd(meas[as.Date(meas$date, "%Y-%m-%d") >= date1 & as.Date(meas$date, "%Y-%m-%d") <= date2 & meas[,3] != -99.99,3])
          
          #If there are not SD values at all, we disable the fixSDValue
          if (fixSDValue == 1 && minSD == Inf) {
            logerror("fixSDValue configured to set the minimum SD but it is Inf. Disabling fixSDValue")
            fixSDValue <<- 0
          }
          
          for (row in 1:nrow(meas)) {
            
            date <- as.Date(meas[row,1])
            year <- as.numeric(format(date, "%Y"))
            day <- as.numeric(format(date, "%j"))
            
            value <- meas[row,3]
            stdValue <- meas[row,4]

            if (value == -99.99) value <- NA
           #if (value < 0) value <- NA
            
            if (fixSDValue == 1) {
              stdValue <- minSD
            } else if (fixSDValue == 2) {
              stdValue <- meas.SD
            } else if (fixSDValue != 0) {
              stdValue <- fixSDValue
            } else if ((stdValue == -99.99) || forceSDValue) {
              stdValue <- as.numeric(value)*unknownSD
            }
            
            parsed = rbind(parsed,c(year,day,as.numeric(value)*convFactor[1],as.numeric(stdValue)*convFactor[2]))
            
          }
         
          colnames(parsed) <- c("year","day",oldNames[3],oldNames[4])
                    
         
          
          #Overwrite any file that exists
          #TODO: write in same folder, same name with PARSED prefix
          path <- file.path(dirname(file),"PARSED")
          dir.create(path)
          file <- file.path(dirname(file),"PARSED",basename(file))
          write.table(file=file, x=parsed, append=FALSE,  col.names=TRUE, row.names = FALSE, quote = FALSE, sep="\t", dec=".")
        }
      
      
      logdebug("METHOD OUT: measurementParser$execute")
    },
    
    
    #
    # Convert units to Kg per hectar per day
    #
    getConversionFactor = function(file) {
      lines <- readLines(con=file, n=3)
      units <- lines[2]
      stdUnits <- lines[3]
      
      fact <- getConversionFactorAux(units)
      factStd <- getConversionFactorAux(stdUnits)
      logdebug(paste("FActores conversion: ",c(fact,factStd)))
      return(c(fact,factStd))
    },
    
    getConversionFactorAux = function(units) {
      units <- tolower(strsplit(units, "=", fixed=T)[[1]][2])
      
      #Out spaces
      units <- str_replace_all(string=units, pattern="\"", repl="")
      units <- gsub("\\s","", units)
      
      #Kg ha-1
      factorForKg <- 1
      units <- strsplit(units, ":", fixed=T)[[1]]
      factorForKg <- factorForKg*updateFactor(units[1])
      factorForKg <- factorForKg*updateFactor(units[2])
      factorForKg <- factorForKg*updateFactor(units[3])
      
      return(factorForKg)
    },
    
    #
    # Get factor to convert value to Kg/ha-1
    #
    updateFactor = function(unit) {
      
      switch(unit, 
             g={
               return(0.001)
             },
             mg={
               return(0.000001)
             },
             ug={
               return(0.000000001)
             },
             dw={#Workaround for DW that is m-2 in the output file
               return(0.0001)
             }
             ,
             umol={
               umolTomol <- 0.000001
               molToCgrams <- 12
               gramsToKg <- 0.001
               #TODO: asumed C
               return(umolTomol*molToCgrams*gramsToKg)
             },
             cmol={
               umolTomol <- 0.000001
               molToCgrams <- 12
               gramsToKg <- 0.00001 #Workaround because Oensingen CO2 is wrong
               #TODO: asumed C
               return(umolTomol*molToCgrams*gramsToKg)
             },
             mmol={
               mmolTomol <- 0.001
               molToCgrams <- 12
               gramsToKg <- 0.001
               return(mmolTomol*molToCgrams*gramsToKg)
             },
             mol={

               return(0.000012)
             },
             gc={
               return(0.001)
             },
             'm-2'={
               return(10000)
             },
             'ha-1'={
               return(1)
             },
             's-1'={
               return(86400)
             },
             'm-1'={
               return(1440)
             },
             'h-1'={
               return(24)
             },
             cm={
               return(0.01)
             },
             #Default
      {
        return(1)
      })
      
    }
  )
)


