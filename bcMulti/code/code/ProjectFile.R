#Responsability:
#Read all the project file references

ProjectFile <- setRefClass(    
  "projectfile"
  
  , fields = list(
      project.file="character"
    
  )
  , methods = list(
    #
    # Constructor
    #
    initialize = function(project.file = "", ...)
    {
      
      callSuper(..., 
                project.file = project.file)
    },
    
    #
    # Search the tag <ldndcproject> in all the folder files, return the file name that contains it.
    # DEPRECATED
    searchProjectFile = function(folder) {
      
      logdebug("METHOD IN: ProjectFile$search")
      
      name <- ""
      
      lapply(list.files(folder, full.names=T), function(x) { 
        
        suppressWarnings(
          if (sum(grep("<ldndcproject>",readLines(x))) > 0) {
            print(basename(x))
            name <<- basename(x) 
          }
        )
      })
      
      logdebug("METHOD OUT: ProjectFile$search")
      return(name)
      
    },
    #
    # Copy all the xml and txt file from the source folder to the destiny folder.
    # It also create the destiny folder
    # DEPRECATED
    prepareProjectFolderAndFiles = function() {
      
      logdebug("METHOD IN: modelUtil$prepareProjectFolderAndFiles")
      
      
      #Getting the sites folders
      sites.dirs <- list.dirs(commonFolder, exclude=paste("chain",sep=""))
      
      mkdir(file.path(commonFolder, "plots"))
      
      for (chainFolder in chainFolders) {
        
        destFolder <- file.path(commonFolder, chainFolder)
        mkdir(destFolder)
        
        copy.res <- file.copy(sites.dirs, destFolder, overwrite = T, recursive = T, copy.mode = T)
        
        if (length(copy.res[copy.res==FALSE]) > 0)  {
          msg <- "unable to copy site basefiles"
          logerror(msg)
          stop(msg) 
        }
        
        modifyProjectFile(destFolder, chainFolder)
        
      }
      
      logdebug("METHOD OUT: modelUtil$prepareProjectFolderAndFiles")
      
    },
    #
    # List all the directories of a folder.
    # exclude: We can exclude any directory using string pattern here
    #
    list.dirs = function(path=".", pattern=NULL, all.dirs=FALSE,
                         full.names=TRUE, ignore.case=FALSE, exclude=NULL) {
      
      all <- list.files(path, pattern, all.dirs,
                        full.names, recursive=FALSE, ignore.case)
      all.dirs <- all[file.info(all)$isdir]
      
      if (is.null(exclude)) {
        return(all.dirs)
      } else {
        return(all.dirs[is.na(str_match(all.dirs,exclude)[,1])])
      }
    },
    #
    # Create a directory. If the directory exists it would be erased at first.
    #
    mkdir = function(destFolder)
    {
      logdebug("METHOD IN: modelUtil$mkdir")
      
      unlink(destFolder, recursive = T, force = T)
      
      file.remove(dir(destFolder, full.names=TRUE))
      dir.create(destFolder, recursive=T, showWarnings = T)
      
      logdebug("METHOD OUT: modelUtil$mkdir")
      
    },
    #
    # Add the working folder to the input and output in each project file
    #
    modifyProjectFile = function(destFolder, chainFolder) {
      
      logdebug("METHOD IN: modelUtil$modifyProjectFile")
      
      for (site.dir in names(sites)) {
        
        projectFile <- searchProjectFile(file.path(destFolder,site.dir))
        xmlFile <- file.path(destFolder,site.dir,projectFile)
        setupFile <- file.path(destFolder, site.dir, 'setup.xml')
        projectDir <- file.path(chainFolder,site.dir)
        
        modifyXmlAttribute(xmlFile, "/ldndcproject/input/sources", "sourceprefix", paste(projectDir,.Platform$file.sep,sep=""))
        modifyXmlAttribute(xmlFile, "/ldndcproject/output/sinks", "sinkprefix", paste(projectDir,.Platform$file.sep,sep=""))
        
        addBayesianCalibrationChildNode(xmlFile, "/ldndcproject/output/sinks")
        cleanOldParametersConfiguration(xmlFile)
        #TODO setSetupOutputs(setupFile)
      }
      
      logdebug("METHOD OUT: modelUtil$modifyProjectFile")
      
    },
    #
    # Clean all the outputs modules and set only the bayesiancalibration module
    #
    setSetupOutputs = function(setupFile) {
      
      logdebug("METHOD IN: modelUtil$setSetupOutputs")      
      
      fileContent <- readLines(setupFile)
      
      lineNums <- grep('output:',fileContent)
      fileContent[lineNums] <- ""
      
      lastLine <- grep('/modulelist', fileContent)
      print(lastLine)
      fileContent[lastLine-1] <- "<module id=\"output:bayesiancalibration:daily\" timemode=\"daily\" />"
      
      write(fileContent, setupFile)
      
      logdebug("METHOD OUT: modelUtil$setSetupOutputs")      
      
    },
    #
    #remove <speciesparameters  source="parameters-species.xml" />
    #remove <siteparameters  source="parameters-site.xml" />
    #remove <soilparameters  source="parameters-soil.xml" />
    #
    cleanOldParametersConfiguration = function(xmlFile) {
      
      logdebug("METHOD IN: modelUtil$cleanOldParametersConfiguration")      
      
      fileContent <- readLines(xmlFile)
      
      parameters <- c("speciesparameters","siteparameters","soilparameters")
      
      for (linePattern in parameters) {
        lineNums <- grep(linePattern,fileContent)
        if (length(lineNums) > 0)
          fileContent[lineNums] <- ""
      }
      
      write(fileContent, xmlFile)
      
      logdebug("METHOD OUT: modelUtil$cleanOldParametersConfiguration")      
    },
    #
    # Add child node in a node
    #
    addBayesianCalibrationChildNode = function(xmlFile, node, child) {
      
      
      xmlfile <- xmlTreeParse(xmlFile, useInternalNodes = TRUE)
      root <- xmlRoot(xmlfile, skip=FALSE)
      
      if (length(getNodeSet(root, paste0(node,"/bayesiancalibrationdaily"))) == 0) {
        
        nodes <- getNodeSet(root, node)
        
        bcNode = newXMLNode("bayesiancalibrationdaily", attrs = c(sink = config$commonOutputFile, format="txt"))
        
        addChildren(nodes[[1]],bcNode)
        
        saveXML(root, xmlFile, indent=TRUE)
        
        #Workaround
        restoreXMLSymbols(xmlFile)
      }
    },
    #
    # Modify the xml file
    #
    modifyXmlAttribute = function(xmlFile, node, attr, value) {
      xmlfile <- xmlTreeParse(xmlFile, useInternalNodes = TRUE)
      root <- xmlRoot(xmlfile, skip=FALSE)
      
      nodes <- getNodeSet(root, node)
      
      ## For each node, apply gsub on the content of the node
      xmlAttrs(nodes[[1]])[attr] = value
      
      saveXML(root, xmlFile, indent=TRUE)
      
      #Workaround
      restoreXMLSymbols(xmlFile)
    },
    #
    # Modify the xml file
    # node need to be Xpath:
    # Example:
    # "/ldndcspeciesparameters/speciesparameters[@id='0']/species[@mnemonic='ALFA']/par[@name='GRAINCN']"
    # "/ldndcspeciesparameters/speciesparameters/species[@mnemonic='ALFA']/par[@name='GRAINCN']"
    #
    modifyXmlXpaths = function(xmlFile, xpathsAndValues, attr) {
      
      logdebug("METHOD IN: ModelUtils$modifyXmlXpaths")
      
      xmlfile <- xmlTreeParse(xmlFile, useInternalNodes = TRUE)
      
      root <- xmlRoot(xmlfile, skip=FALSE)
      
      if (!is.matrix(xpathsAndValues))
        stop("XpathsAndValues muss be a matrix")
      
      apply(xpathsAndValues, 1, function(xpathValPair) {
        logdebug(xpathValPair)
        nodes <- getNodeSet(root, xpathValPair[1])
        xmlAttrs(nodes[[1]])[attr] = xpathValPair[2]
      })
      
      saveXML(root, xmlFile, indent=TRUE)
      
      #Workaround
      restoreXMLSymbols(xmlFile)
      
      logdebug("METHOD OUT: ModelUtils$modifyXmlXpaths")
      
    },
    #
    # Get the attribute and its value from a xml file
    #
    getXmlAttribute = function( xPath) {
      xmlfile <- xmlTreeParse(project.file, useInternalNodes = TRUE)
      root <- xmlRoot(xmlfile, skip=FALSE)
      attr <- getNodeSet(xmlfile, xPath)
      return(attr)
    },
    getInputSourceprefix = function() {
      xpath  <- "/ldndcproject/input/sources/@sourceprefix"
      node <- getXmlAttribute(xpath)
      prefix <- node[[1]]['sourceprefix'][[1]]
      return(prefix)
    },
    getInputSetup = function() {
      xpath  <- "/ldndcproject/input/sources/setup/@source"
      node <- getXmlAttribute(xpath)
      value <- node[[1]]['source'][[1]]
      return(value)
    },
    getOutputSinksource = function() {
      xpath  <- "/ldndcproject/output/sinks/@sinkprefix"
      node <- getXmlAttribute(xpath)
      value <- node[[1]]['sinkprefix'][[1]]
      return(value)
    },
    #
    # Workaround method: We lose the < and > symbols in some attributes, so we replace them
    #
    restoreXMLSymbols = function(xmlFile) {
      conn <- file(xmlFile)
      txt <- suppressWarnings(readLines(conn))
      txt <- gsub("&lt;", "<", txt)
      txt <- gsub("&gt;", ">", txt)
      txt <- append('<?xml version="1.0"?>',txt)
      writeLines(txt, conn)
      close(conn)
    }
    
    
    
    
  )#End methods List 
  
)#End RefClass




