# Responsability
# Execute the sensibility analysis

SensitivityIndexAnalisys <- setRefClass(    
  "sensitivityIndexAnalisys"
  
  , fields = list(
    parametersSensitivity="data.frame",
    compounds = "character",
    priorProbabilityDistribution="data.frame",
    model="ldndc"
  )
  , methods = list(
    #
    # Constructor
    #
    initialize = function(...,
                          parametersSensitivity=data.frame(),
                          priorProbabilityDistribution=data.frame(),
                          compounds = config$compounds,
                          model=Ldndc())
    {
      logdebug("Initialize SensitivityIndexAnalisys")
      callSuper(...,
                parametersSensitivity=parametersSensitivity,
                priorProbabilityDistribution=priorProbabilityDistribution,
                compounds=compounds,
                model=model)
      
    },
    #
    # Normally output1 = outputWithMinValueOfParameter and output2 = max
    #
    getSensitivityIndex = function(outputs, col) {

      logdebug("METHOD IN: sensitivityIndexAnalisys$getSensitivityIndex")
      # Sensitivity Index = (Dmax - Dmin)/Dmax
      #TODO: add NSE with measurements to check which simulation change more the differences
      Dmax <- 1
      Dmin <- 1
      
      if (length(col) == 1) {
        Dmax <- outputs[[1]][col]
        Dmin <- outputs[[1]][col]
        
        #TODO if we remove the outputs with errors maybe we ignore a sensitive site 
        for (output in outputs) {
          Dmax <- pmax(Dmax, output[col])
          Dmin <- pmin(Dmin, output[col])
        }
      } else {
        Dmax <- outputs[[1]][col[1]] + outputs[[1]][col[2]]
        Dmin <- outputs[[1]][col[1]] + outputs[[1]][col[2]]
        
        for (output in outputs) {
          Dmax <- pmax(Dmax, (output[col[1]] + output[col[2]]))
          Dmin <- pmin(Dmin, (output[col[1]] + output[col[2]]))
        }
      }
      
      senIdx <- ((Dmax-Dmin)/Dmax)
      senIdx[is.na(senIdx)] <- 0
      senIdx <- abs(senIdx)
      
      logdebug("METHOD IN: sensitivityIndexAnalisys$getSensitivityIndex")
      
      return(sum(senIdx))
      
    },
    
    changeParameterAndExecuteModel = function(parameterIndex) {
      
      logdebug("METHOD IN: sensitivityIndexAnalisys$executeModelWithParameterChange")
      
      outputs <- list()
      
      logdebug(priorProbabilityDistribution)
      param <- priorProbabilityDistribution[parameterIndex,][c('name','min','max')]
      
      logdebug("haber")
      logdebug(param)
      i <- 1
      
      #Execute several times the model
      for (value in seq(from=param$min, to=param$max, by=(param$max-param$min)/10)) {
       
        dfValues <- data.frame(value,row.names=param$name)
        
        tryCatch({
          model$execute(dfValues)
          outputs[[i]] <- model$output()
          loginfo(paste("Correct execution model for Parameter",param$name,"with value:",value))
        },
        error= function(err) {
          logerror(err)
          logerror(paste("Error executing model for Parameter",param$name,"with value:",value))
          #We do not want empty elements in the list
          #TODO: check how this affect when validating some sites with an error.
          i <- i - 1
        })
        
        i <- i + 1
      }
      
      if (length(outputs) == 0)
        logerror(paste("NO OUTPUTS FOR PARAMETER",param$name))
      
      logdebug("METHOD OUT: sensitivityIndexAnalisys$executeModelWithParameterChange")
      
      return(outputs)
    },
    executeModelChangingParameters = function() {
      
      logdebug("METHOD IN: executeModelChangingParameters")
     
      for (parameterIndex in 1:length(priorProbabilityDistribution[,1])) {
        
        logdebug(paste("Parameter number ", parameterIndex))
        name <- getParamName(parameterIndex)
            
        outputs <- changeParameterAndExecuteModel(parameterIndex)
        
        if (validOutput(outputs)) {
          
          diffs <- c()
          for (col in compounds) {
            if (length(col) == 1) {
              diff <- getSensitivityIndex(outputs, col)
            } else {
              #TODO: deprecated? Force to modify the cpp?
              diff <- getSensitivityIndex(outputMin[col[1]] + outputMin[col[2]], outputMax[col[1]] + outputMax[col[2]])
            }
            logdebug(paste("Sensitivity Index for ", col, sep=""))
            logdebug(capture.output(diff))
            diffs <- c(diffs, diff)
          }
          
          sensitivityIndex <- sum(diffs)
          parametersSensitivity <<- rbind(parametersSensitivity, data.frame(name=name, diff=sensitivityIndex))
        } else {
          logerror(paste("Output invalid for Parameter: ", name, sep=""))
        }
      }
      
      logdebug("METHOD OUT: executeModelChangingParameters")
      
      parametersSensitivity
    },
    
    
    getParamName = function(index) {
      name <- priorProbabilityDistribution[index,]$name
      name
    },
    validOutput = function(outputs)  {
       
      error <- (length(outputs) == 0)

      for (output in outputs) {
        error <- error || is.null(output) || nrow(output) == 0 
      }
      return(!error)
    },
    sortParametersSensitivity = function() {
      
      sorted <- parametersSensitivity[with(parametersSensitivity, order(parametersSensitivity[,2])), ]
      sorted
    }
    
    
    
  )#End methods List 
  
)#End RefClass
