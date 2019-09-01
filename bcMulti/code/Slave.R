# Responsability:
# Slave that recieve and execute a task

Slave <- setRefClass(    
  "slave"

  , fields = list()
  , methods = list(
    #
    # Function the slaves will call to perform a validation on the
    # fold equal to their slave number.
    # Assumes: thedata,fold,foldNumber,p
    #
    foldslave = function() {

      logdebug("METHOD IN: foldslave")
      
      # Note the use of the tag for sent messages: 
      #     1=ready_for_task, 2=done_task, 3=exiting 
      # Note the use of the tag for received messages: 
      #     1=taskLdndc, 2=taskMetropolis, 3=done_tasks 
      junk <- 0 
      done <- FALSE
      
      while (!done) {
        
        # Signal being ready to receive a new task 
        mpi.send.Robj(junk,0,1) 
        
        # Receive a task 
        task <- mpi.recv.Robj(mpi.any.source(),mpi.any.tag()) 
        
        task_info <- mpi.get.sourcetag()
        tag <- task_info[2] 
        
        
        if (isNewTask(tag)) {
          
          message <- task$process()
          
          mpi.send.Robj(message,0,2)
          
        } else if (areAllTasksDone(tag)) {
          
          done <- TRUE
        }
        
      }
      
      logdebug("METHOD OUT: foldslave")
      
      #Finish
      mpi.send.Robj(junk,0,3)
      
    },
    
    #tag 1=task; tag 2=done_tasks
    areAllTasksDone = function(status) {
      
      done <- (status == 2) || (status == 3)
      
      return(done)
    },
    
    isNewTask = function(status) {
      
      done <- (status == 1)
      
      return(done)
    }
    
    
  )
)
