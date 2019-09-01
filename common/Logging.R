#Responsability:
#Logging initialization
library(logging)

Logging <- setRefClass(    
  "logging"
  
  , fields = list(
      logPath="character",
      logFile="character",
      logLevel="character"
    )
  , methods = list(
    #
    # Constructor
    #
    initialize = function(..., logPath="",
                            logFile="",
                          logLevel="")
    {

      callSuper(..., logPath=logPath,
                logFile=logFile,
                logLevel=logLevel)
    },
    #
    #
    # Initialize logging
    #
    #
    ini = function() {
		print(logPath)
		print(logFile)
		print(logLevel)
      logReset()
      
      #Show milliseconds
      options(digits.secs = 3)
      file <- file.path(logPath,logFile)
      
      basicConfig(level='FINEST')
      addHandler(writeToFile, file=file, level=logLevel)
      with(getLogger(), names(handlers))
      
    },
    testLogging = function(text) {
      loginfo(text)
    }


  )
)

