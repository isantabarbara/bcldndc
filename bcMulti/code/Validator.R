#Responsability:
#Validate the data for the calibration

Validator <- setRefClass(    
  "validator"
  
  , fields = list(
    commonFolder="character",
    chainFolders="character",
    sites="list"
    
  )
  , methods = list(
    #
    #
    # Constructor
    #
    #
    initialize = function(commonFolder = config$path$common,
                          chainFolders = "", 
                          sites = list(), ...)
    {
      
      callSuper(..., 
                commonFolder = commonFolder,
                chainFolders = chainFolders, 
                sites = sites)
    },
    #
    # Check that climate file is coherent with starting year
    #
    climateCheck = function() {

      logdebug("METHOD IN: Validator$climateCheck")
      
      for (siteName in names(config$sites)) {
        
        logdebug(paste("Checking climate for", siteName))
        
        year <- config$sites[[siteName]]$years[1]
      
        climateFile <- file.path(config$path$common, siteName, "climate.txt")
      
        error.msg <- paste(climateFile, "file has not been correctly configured")
        
        data.line <- 0
        search <- function(year, line) {
          
          if (length(grep("time", line))) {
            
            if (!length(grep(year,line))) {
              logerror(error.msg)
              #stop(error.msg)
            }
            
          } else if (data.line == 2) {
            
            data.line <<- 0
            if (!length(grep(year,line))) {
              logerror(error.msg)
              #stop(error.msg)
            }
            
          } else if (length(grep("%data", line)) || (data.line == 1)) {

            data.line <<- data.line + 1
          }
        }

        conn <- file(climateFile) 
        open(conn)
        lapply(readLines(conn), function(x) search(year,x))
        close(conn)
      }
      
      logdebug("METHOD OUT: Validator$climateCheck")
    },
    #
    # Checks that we have the correct measurement names that match the outputs.
    # Checks that at list one year from the measurements match the simulation years
    #
    measurements = function() {

      logdebug("METHOD IN: Validator$measurements")
      
      meas.names <- names(config$match)
      
      for (siteName in names(config$sites)) {
        
        logdebug(paste("Checking measurements for", siteName))
        
        meas.folder <- file.path(config$path$common, siteName, "measurements")
        files <- list.files(path = meas.folder, all.files = F,
                            full.names = T, include.dirs = FALSE)
        
        for (file in files) {

          skip.lines <- which(grepl("%data",readLines(con=file)))
          meas <- read.table(skip=skip.lines, file=file, header=T, sep = "", nrows = 1)

          matchs <- sum(colnames(meas) %in% meas.names)
          if (matchs == 0)
            stop(paste(file, "doesn't contain any configured column."))
          
          date <- as.Date(meas[1,1])
          year <- as.numeric(format(date, "%Y"))
          
          match.yr <- sum(year %in% config$sites[[siteName]]$years)
          if (match.yr == 0) {
            #TODO: uncomment or fix
            #stop(paste(file, "doesn't contain any configured year."))
          }
        }
        
        
      }
    
      logdebug("METHOD OUT: Validator$measurements")
      
    },
    
    #
    # Execute the model for each site configured
    #
    ldndc = function() {
    
      for (siteName in names(config$sites)) {
        logdebug(paste("Checking ldndc for", siteName))
        ldndc <- Ldndc(siteName,config$sites[[siteName]] )
        ldndc$execute()
      }
    },
    #
    # Checks that first events in mana.xml are in the configured years
    #
    manaEventsDates = function() {
      #TODO: eventDates
    },
    #
    # Checks that all the mandatory files for ldndc execution are there
    # We force the files names
    #
    ldndcFiles = function(site.path, site) {

      files <- list.files(path = site.path, all.files = T,
                full.names = FALSE, include.dirs = FALSE)
      
      contains.file <- function(file.name) {
        if (!(file.name %in% files)) {
          logerror(paste("VALIDATION: Cannot find",file.name,"in",site.path))
          stop(paste("VALIDATION: Cannot find",file.name,"in",site.path))
        }
      }
      
      #TODO: project file is not checked
      mandatory.files <- c("mana.xml","climate.txt","airchem.txt",
                           "setup.xml","site.xml")
      
      lapply(mandatory.files, function(file.name) contains.file(file.name))
             
    },
    #
    # Check if we have the correct folder and files for the configured files
    #
    siteFolderAndFiles = function() {
     
      for (siteName in names(config$sites)) {
     
        logdebug(paste("Checking if", siteName, "folder exists."))
     
        site.path <- file.path(config$path$common, siteName)
     
        if (!file.exists(site.path))
            stop(paste(site.path,"does not exist."))
        
        ldndcFiles(site.path, config$sites[[siteName]])
      }
    },
    #
    # Check that LResources contains %O/%o
    #
    checkLresourcesLocalPath = function() {
      
      conn <- file(file.path(commonFolder,"Lresources"),open="r")
      lines <- readLines(conn)
      close(conn)
      found <- FALSE
      Lresources.local.line <- ""
      for (line in lines){
          
        if (length(grep("%O",line)) > 0)
          found <- TRUE
        
        if (length(grep("Lresources-local",line)) > 0)
          Lresources.local.line <- line
      }
      
      if (!found) {
        msg <- paste("Lresources is not correctly configured. Line '", Lresources.local.line, "' should contains %O/%o as path.")
        logerror(msg)
        stop(msg)
      }
      
    }
    
    
    
  )#End methods List 
  
)#End RefClass




