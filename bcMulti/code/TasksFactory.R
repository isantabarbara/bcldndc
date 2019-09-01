# Responsability
# Create all the tasks that has to be done in mpi slaves.


TasksFactory <- setRefClass(    
  "tasksfactory"
  , fields = list(
      parallelThreads="numeric"
    )
  , methods = list(
    initialize = function(...,
                          parallelThreads = config$parallelThreads) {
      
      callSuper(..., 
                parallelThreads=parallelThreads)
                
    

    },
    
    copyInputFolder = function(siteName, i) {
      
      logdebug("METHOD IN: tasksFactory$copyInputFolder")
      
     
      #TODO: change hardcoded 1,2 
      siteFolders <- str_split(config$sites[[siteName]]$projectFile,"/")[[1]][c(1,2)]
      source.path <- file.path(config$sites[[siteName]]$inputPath, siteFolders[1], siteFolders[2])
      
      
      dest.path <- file.path(paste0(config$sites[[siteName]]$confInputPath,i),siteFolders[1])
      dir.create(dest.path, recursive=T)
      
      logdebug(paste("copy",source.path,"to",dest.path))
      file.copy(source.path, dest.path, recursive= TRUE)
      
      logdebug("METHOD OUT: tasksFactory$copyInputFolder")
    },
    #
    # Generate and prepare the set of tasks for been executed in MPI slaves.
    #
    generateTasks = function(lastParameters, measurements, out.db) {
     
      logdebug("METHOD IN: tasksFactory$generateTasks")
      
      for (i in 1:parallelThreads) {
        chainPath <- file.path(config$path$common, i)
        dir.create(chainPath)
      }
      
      tasks <- list()
      
      task.num <- 1
      for (i in 1:parallelThreads) {
        
        for (siteName in names(config$sites)) {
          
          path <- file.path(config$sites[[siteName]]$inputPath, config$sites[[siteName]]$projectFile)
          output.sinksource <- ProjectFile(project.file=path)$getOutputSinksource()
          
          copyInputFolder(siteName, i)
          input.path <- paste0(config$sites[[siteName]]$confInputPath,i)
     
          ldndc <- Ldndc(subfolder = as.character(i),
                         site=config$sites[[siteName]],
                         commonPath = config$path$common,
                         confInputPath = input.path,
                         inputsPriorPath = config$params$inputsprior,
                         loglevel=config$log$level)
          ldndc$ini()
          
          tasks[[task.num]] <- Task(chain=i,
                                    site.name=siteName,
                              id=task.num,
                             ldndc=ldndc,
                             output=Output(commonPath=config$path$common,
                                          subfolder = as.character(i),
                                          site=config$sites[[siteName]],
                                          dataDaysRange=config$dataDaysRange,
                                          output.sinksource = output.sinksource,
                                          matchCols = config$match),
                             parameters=lastParameters[[i]],
                             measurements=measurements[[siteName]])
          
          task.num <- task.num + 1
          
        }
        
        
      }
      logdebug("METHOD OUT: tasksFactory$generateTasks")
      
      return(tasks)
      
    },
    #
    # Prepare all the folders for the different chains
    # DEPRECATED
    prepareChainFolders = function() {
      
      
      dir.create(path)
      utils <- ModelUtils(chainFolders=chainFolders, sites=config$sites)
      utils$prepareProjectFolderAndFiles()
      
    },
    #
    # Get the list of tasks ids
    #
    getTasksIds = function(tasks) {
      
      logdebug("METHOD IN: tasksFactory$getTasksIds")
      
      ids <- list()
      
      for (i in 1:length(tasks)) {
        
        ids[[i]] <- tasks[[i]]$id
      }
      
      logdebug("METHOD OUT: tasksFactory$getTasksIds")
      
      return(ids)
      
    }
    
  )#End methods List 
  
)#End RefClass



