#Responsability:
#Execute de model ldndc

#Load Needs
#Configuration object

Ldndc <- setRefClass(    
  "ldndc"
  , fields = list(
      subfolder = "character",
      site="list",
      commonPath="character",
      inputsPriorPath="character",
      confInputPath="character",
      loglevel="character",
      inputs.prior = "list"
  )
  , methods = list(
    #
    # Constructor
    #
    initialize = function(subfolder = "",
                          site=list(),
                          commonPath = "",
                          confInputPath = "",
                          loglevel="ERROR",
                          inputsPriorPath = "",
                          inputs.prior = list(),
                          ...)
    {
      logdebug("METHOD IN: Ldndc initialize")
      callSuper(..., 
                subfolder=subfolder,
                site=site,
                commonPath=commonPath,
                confInputPath=confInputPath,
                loglevel=loglevel,
                inputsPriorPath= inputsPriorPath,
                inputs.prior=inputs.prior
                )
      
      logdebug("METHOD OUT: Ldndc initialize")

    },
    ini = function() {
      createLdndcConf()
      readInputsPrior()
    },
    #
    # Execute the model
    # Input: data.frame with the parameters
    #
    execute = function(parameters=NULL) {
      
      logdebug("METHOD IN: ldndc$execute")

      
      if (!is.null(parameters)) {
        newResourceLocalFile(parameters)
        modifyInputs(parameters)
      }
      
      scheduleArg <- site$scheduleArg
      
      projectPathFile <- file.path(site$inputPath, site$projectFile)
      
      ldndc.exe <- file.path(commonPath,"./ldndc")
      ldndc.conf <- file.path(commonPath,subfolder,paste0("ldndc",basename(confInputPath),".conf"))
      cmd <- paste(ldndc.exe, "-c", ldndc.conf, projectPathFile, scheduleArg, sep=" ")
      loginfo(cmd)
      
      status <- system(cmd,intern=F)

      if (status != 0) {
        logerror(cmd)
        stop(paste(status, "code for ldndc"))
      }


      logdebug("METHOD OUT: ldndc$execute")
      
    },
    #
    # Creates new resource-local file with the modified parameters
    #
    newResourceLocalFile = function(parameters) {
      
      loginfo("METHOD IN: Ldndc$newResourceLocalFile")
      
      if (!is.data.frame(parameters))
        stop("The parameters should be stored in a data.frame")
      
      #logdebug(capture.output(parameters))
      parameters[,1] <- gsub("\\s", "", paste("\"",parameters[,1],"\"",sep=""))
      
      resource.local <-  file.path(commonPath, subfolder, site$paramFile)
      
      write.table(parameters, file = resource.local, append = F, quote = F, sep = "= ",
                  eol = "\n", dec = ".", row.names = T,
                  col.names = F)
      
      loginfo("METHOD OUT: Ldndc$newResourceLocalFile")
    },
    #
    # Create the configuration file with the correct data for execute ldndc.
    # Creates the ldndc.log file if it doesn't exists
    #
    createLdndcConf = function() {

      logdebug("METHOD IN: Ldndc$createLdndcConf")
      
      logdebug(paste("subfolder=",subfolder))
      logdebug(paste("commonPath=",commonPath))
      logdebug(paste("confInputPath=",confInputPath))
      logdebug(paste("inputsPriorPath=",inputsPriorPath))
      logdebug(paste("loglevel=",loglevel))
      global <- "[global]"
      
      ldndc.log <- file.path(commonPath,"ldndc.log")
      log.file <- "log_file = \"stderr\""
      if (loglevel == "DEBUG") 
        log.file <- paste("log_file = \"",ldndc.log,"\"",sep="")
      
      log.level <- "log_level = \"verbose\""
      log.mask <- "log_targets_mask = \"0\""
      
      resources.path <- paste0("resources_path = \"",commonPath,"\"")
      
      input.path <- paste("input_path = \"",site$inputPath,"\"",sep="")
      if (length(confInputPath) > 0) {
		    input.path <- paste("input_path = \"",confInputPath,"\"",sep="")
      }
      
      output.on <- "output = \"on\""
      output.path <- paste0("output_path = \"",file.path(commonPath,subfolder),"\"")
      
      #TODO: random_seed workaround
      random <- "random_seed = 1"
      
      lines <- c(global, log.file, log.level, log.mask, 
                 resources.path, input.path, output.on, output.path, random)
      
      ldndc.conf <- file.path(commonPath,subfolder,paste0("ldndc",basename(confInputPath),".conf"))
      logdebug(paste("ldndc.conf",ldndc.conf))
      file.create(ldndc.conf)
      fileConn <- file(ldndc.conf)
      writeLines(lines, fileConn)
      close(fileConn)
      
      file.create(ldndc.log)
      
      logdebug("METHOD OUT: Ldndc$createLdndcConf")
    },
    #
    # Modify the configured xml input files for executing the model.
    #
    modifyInputs = function(parameters) {
    
		logdebug("METHOD IN: Ldndc$modifyInputs")
    
    used.files <- c()
		
		if (sum(rownames(parameters) == "corg") > 0) {
		  parameters["norg",] <- parameters["corg",]
		}
		if (sum(rownames(parameters) == "wcmax") > 0) {
		  parameters["wcmin",] <- parameters["wcmax",]
		}
    #Workaround error ph
		#if (sum(rownames(parameters) == "ph") > 0) {
		#  parameters["ph",] <- parameters["ph",]*0.2
    #  logerror(paste("modified ph - new value:",parameters["ph",]))
		#}
    for (element in inputs.prior) {
      logdebug(element$paramkey)
			value <- parameters[element$paramkey,][[1]]
			
			if (!is.null(value)) {

        #If we need to modify more than ones a file, we can not open the original
        if (sum(grepl(element$file,used.files)) == 0) {
				  xmlOriginalFile <- file.path(site$inputPath,element$file)
				  used.files <- c(used.files,xmlOriginalFile)
        } else {
          xmlOriginalFile <- file.path(confInputPath,element$file)
        }
          
				xmlfile <- xmlTreeParse(xmlOriginalFile, useInternalNodes = TRUE)
				root <- xmlRoot(xmlfile, skip=FALSE)
				
				nodes <- getNodeSet(root, element$xpath)
				logdebug(paste('XPATH:',element$xpath))
				for (node in nodes) {
          logdebug("NEXT NODE!")
          if (!is.null(xmlAttrs(node)[element$attr][[1]])) {
            if (!is.na(xmlAttrs(node)[element$attr][[1]])) {
					old.value <- as.numeric(xmlAttrs(node)[element$attr][[1]])
					logdebug(paste("oldvalue:",old.value))
					logdebug(paste("attr:",element$attr))
          if (old.value != -99.99) {
					  new.value <- old.value + old.value * value
					  logdebug(paste("newvalue:",new.value))
            if (!is.na(new.value)) { 
              xmlAttrs(node)[element$attr] <- new.value
              logdebug("DONE att")
            } else {
              logerror("ERROR INPUTS, new value is NA, maybe there is no Attr!!!!!!!!!!!!!!!!!!")
              logerror(paste("oldvalue:",old.value))
              logerror(paste("param value:",value))
            }
          }
            }
          }
				}			
				
				xmlDestFile <- file.path(confInputPath,element$file)
        logdebug(paste('Save XML:',xmlDestFile))
				saveXML(root, xmlDestFile, indent=TRUE)
				logdebug('Saved XML')
				
			} else {
          logerror(paste("NULL Value modifying input:", element$paramkey))
			}
				
		}
		
		logdebug("METHOD OUT: Ldndc$modifyInputs")
    },
    #
    # Reads the input priors
    #
    readInputsPrior = function() {
  		logdebug("METHOD IN: Ldndc$readInputsPrior")
  		
  		tryCatch({
  			inputs.prior <<- yaml.load_file(file.path(commonPath, inputsPriorPath))
  		}, error=function(e) { 
  		  logerror(paste("Error loading inputs priors file. Maybe they are none.", e))
  		  inputs.prior <<- list()
  		}) 		
      
      if (is.null(inputs.prior)) {
        inputs.prior <<- list()
      }
      
  		logdebug("METHOD OUT: Ldndc$readInputsPrior")
    }
    
     
  )#End methods List 
  
)#End RefClass
