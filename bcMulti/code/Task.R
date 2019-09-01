# Responsability
# Execute parallelized task


Task <- setRefClass(    
  "task"
  , fields = list(
      chain = "numeric",
      id="numeric",
      site.name="character",
      ldndc = "ldndc",
      output = "output",
      parameters = "data.frame",
      measurements = "measurements"
  )
  , methods = list(
    initialize = function(..., chain = 0,
                          id=0,
                          site.name="",
                          ldndc = NULL,
                          output = NULL,
                          parameters = data.frame(),
                          measurements = NULL) {
      
      logdebug("initialize Task")
      logdebug(paste("Parameters in Task",id,"chain",chain))
      logdebug(capture.output(parameters))
      callSuper(..., 
                chain=chain,
                id=id,
                site.name=site.name,
                ldndc=ldndc,
                output=output,
                parameters=parameters,
                measurements = measurements)
      

    },
    
    #
    # Execute the task process
    #
    process = function(iteration = 0) {
     
      logdebug("METHOD IN: task$process")

      logdebug(paste("Tag 1 in Slave for chain ",chain,sep=""))

      ok <- FALSE
      newLikelihood <- 0
      like.sum <- NULL
      
      tryCatch( {
        loginfo(paste("Chain ",chain, " calling executeMoudel",sep=""))
       
        ldndc$execute(parameters)
        loginfo(paste("Chain ",chain, " out of executeModel. Calling output$read",sep=""))
        
        output$process(measurements$getMeasurementsNames())

        loginfo(paste("Chain ",chain, " out of output$read. Calling Likelihood$sumLikelihood",sep=""))
       
        
        aggr <- Aggregator(dataDaysRange=output$dataDaysRange, matchCols=output$matchCols, outputs=output$output, measurements=measurements$values)
        logdebug("AGGREGATION")
        aggr.comb <- aggr$getCombination()
        logdebug(capture.output(head(aggr.comb)))
        
          #calculate likelihood with output and measurements
        lik <- Likelihood(data.sets=aggr.comb)
        loginfo(paste0("Chain ",chain, " out of Likelihood$sumLikelihood."))
        newLikelihood <- lik$sumLikelihood()
       
        
        
        magnitud.dev <- magnitudValidation(matchCols=output$matchCols, outputs=output$output, measurements=measurements$values)
        if (magnitud.dev > 0) {
          #could happen that the measurements dates are ok, but outsite there are outliers. If this happens. Reduce probability half
          newLikelihood <- newLikelihood/(2*magnitud.dev)
        }
        
        #Summary of the likelihood
        like.sum <- lik$likSummary()
        like.sum$iteration <- iteration
        like.sum$chain <- chain
        like.sum$likelihood <- newLikelihood
        
        #QUES: why we do not have the prior in the posterior
        #QUES ANSW: I think that because we use uniform distribution. So the propability of the selected parameters
        #QUES ANSW: is always the same between min and max values and out there is 0.

        
        if (is.na(newLikelihood))
          stop(paste("New likelihood should be available and is NA or NULL. Site",site.name,"Chain",chain))
        if (newLikelihood == 0)
          stop(paste("New likelihood is 0. Check why #TODO. Site",site.name,"Chain",chain))
        if ((newLikelihood == Inf) || (newLikelihood == -Inf))
          stop(paste("New likelihood is Inf or -Inf. Check why #TODO. Site",site.name,"Chain",chain))
        
        ok <- TRUE
      },
      error= function(err) {
                  logerror(err)
                  logerror(paste("Error processing task: executing Site",site.name,"Chain", chain))
                })
      
      message <- list(ok=ok, chain=chain, site=site.name,
                      likelihood=newLikelihood, 
                      output=output$output,
                      likesummary=like.sum)
      
      logdebug("METHOD OUT: task$process")
      
      return(message)
    }, 
    #
    # Check that the output maximum is not X orders of magnitud bigger than the measurements maximum. 
    # No matter year or match
    #
    magnitudValidation = function(matchCols=output$matchCols, outputs=output$output, measurements=measurements$values) {
      logdebug("METHOD IN: task$magnitudValidation")
      magnitud.dev <- 0
      for (compound in names(matchCols)) {
        
        meas.name <- compound
        output.name <- matchCols[[compound]][[1]]
        
      
        max.meas <- max(measurements[[meas.name]][3])
        max.output <- max(outputs[,output.name])
                
        if ((max.output > max.meas*3) | (max.output < max.meas/3)) {
          msg <- paste(compound, "Max Output:", max.output, "is 5 times different than MAX measurement:", max.meas, ". Invalid parameters, they can not keep the order of magnitud")
          logerror(msg)
          magnitud.dev <- magnitud.dev + 1.5
        }
        
      }
      logdebug("METHOD OUT: task$magnitudValidation")
      
      return(magnitud.dev)
    }
    
    
    
    
    
  )#End methods List 
  
)#End RefClass


