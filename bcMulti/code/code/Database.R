# Responsability
# Initialize and insert data in sqlite database
require("RSQLite")


Database <- setRefClass(    
  "database"
  , fields = list(
     path="character",
     file="character",
     conn="SQLiteConnection",
     TABLE_PARAMETERS = "character",
     TABLE_SUMMARY = "character",
     TABLE_CONFIGURATION = "character"
  )
  , methods = list(
    #
    # Constructor
    #
    initialize = function(...,
                          path = config$db$path,
                          file = config$db$file,
                          TABLE_PARAMETERS = "Parameters",
                          TABLE_SUMMARY = "Summary",
                          TABLE_CONFIGURATION = "Configuration"
                          )
    {
      
      loginfo("Initialize Database")
      
      callSuper(...,
                path = path,
                file = file,
                TABLE_PARAMETERS = TABLE_PARAMETERS,
                TABLE_SUMMARY = TABLE_SUMMARY,
                TABLE_CONFIGURATION = TABLE_CONFIGURATION
                )
      
    },
    #
    # Connect to the data base in file
    #
    connect = function() {
      logdebug("METHOD IN: Database$connect")
      
      dbFile <- file.path(path,file)
      conn  <<- dbConnect(dbDriver("SQLite"), dbname = dbFile,loadable.extensions = TRUE)
      #Insert the Variance function in Sqlite3
      loadExtension = paste("select load_extension('", config$db$extension,"')",sep="")
      dbGetQuery(conn, loadExtension)
      logdebug("METHOD OUT: Database$connect")
      
    },
    #
    # Delete the database file
    #
    delete = function() {
      logdebug("METHOD IN: database$deleteFile")
      
      dbFile <- file.path(path,file)
      unlink(dbFile, recursive = F, force = T)
      
      logdebug("METHOD OUT: database$deleteFile")
    },
    #
    # Create tables if they do not exist 
    # parametersNames: a vector with the parameter names
    #      
    create = function(parametersNames) {

      logdebug("METHOD IN: Database$create")
 
      tryCatch({
        
        dbSendQuery(conn, paste("CREATE TABLE ", TABLE_PARAMETERS, "(chain INTEGER, iteration INTEGER, likelihood FLOAT",
                                getColNamesCreateQuery(parametersNames),");", sep=""))
        dbSendQuery(conn, paste("CREATE TABLE ", TABLE_SUMMARY, "(chain INTEGER, key TEXT, value NUMERIC);", sep=""))
        dbSendQuery(conn, paste("CREATE TABLE ", TABLE_CONFIGURATION, "(key TEXT, value TEXT);", sep=""))
      },
      error= function(err) {
           logerror(err)
           logerror("Error creating tables. They probably exists")
      })
      
      logdebug("METHOD OUT: Database$create")
      
    },
    #
    # Prepare the column names query
    #  
    getColNamesCreateQuery = function(colnames) {
      
      logdebug("METHOD IN: database$getColNamesCreateQuery")
      
      query <- ""
      
      for (i in 1:length(colnames)) {
        query <- paste(query,", ",gsub("\\.","",colnames[i])," FLOAT",sep="")
      }
      
      logdebug(paste("Created Columns: ",query))
      logdebug("METHOD OUT: database$getColNamesCreateQuery")
      
      return(query)
    },
    #
    # Close database connection
    #
    close =  function() {

      tryCatch({
        
        dbDisconnect(conn)
      },
      error= function(err) {
        logerror(err)
        logerror("Error closing database")
      })
    },
    #
    # Insert parameters
    # Parameters: data.frame, rownames has the name of the parameters and parameters[,1] are the values
    #
    insertParameters = function(chain, iteration, likelihood, parameters) {

      sqlQuery(paste("INSERT INTO ", TABLE_PARAMETERS ," (chain, iteration, likelihood, ",
                     getColNamesInsertQuery(rownames(parameters)),") VALUES (", chain, ",", iteration, ",", correctNumber(likelihood),
                     getColValuesInsertQuery(parameters[,1]),")", sep=""))
      
    },
    
    insertSummary = function(summary) {
      
      logdebug("METHOD IN: database$insertSummary")
    
      tryCatch({

        dbBeginTransaction(conn)
        
        query <- paste("INSERT INTO ",TABLE_SUMMARY, " VALUES (", summary$chain,",'iterations',", summary$iterations,");",sep="")
        dbSendQuery(conn, query)
        
        query <- paste("INSERT INTO ",TABLE_SUMMARY, " VALUES (", summary$chain,",'bestValue',", summary$bestValue,");",sep="")
        dbSendQuery(conn, query)
        
        query <- paste("INSERT INTO ",TABLE_SUMMARY, " VALUES (", summary$chain,",'bestCount',", summary$bestCount,");",sep="")
        dbSendQuery(conn, query)
        
        query <- paste("INSERT INTO ",TABLE_SUMMARY, " VALUES (", summary$chain,",'acceptedCount',", summary$acceptedCount,");",sep="")
        dbSendQuery(conn, query)
        
        query <- paste("INSERT INTO ",TABLE_SUMMARY, " VALUES (", summary$chain,",'acceptanceRate',", summary$acceptanceRate,");",sep="")
        dbSendQuery(conn, query)
        
        dbCommit(conn)
      
      }, error=function(e) { 
        
        logerror(paste("Inserting Summary. Caught RSQLite Error: dbSendQuery() Failed with error", e))
        dbRollback(conn)
        
      })
  
      logdebug("METHOD OUT: database$insertSummary")
    },
    
    updateSummary = function(summary) {
      
      logdebug("METHOD IN: database$updateSummary")
      
      tryCatch({
        
        dbBeginTransaction(conn)
        
        query <- paste("UPDATE ",TABLE_SUMMARY, " SET value = ", summary$iterations," WHERE key LIKE 'iterations' AND chain = ", summary$chain,";",sep="")
        dbSendQuery(conn, query)
        
        query <- paste("UPDATE ",TABLE_SUMMARY, " SET value = ", summary$bestValue," WHERE key LIKE 'bestValue' AND chain = ", summary$chain,";",sep="")
        dbSendQuery(conn, query)
        
        query <- paste("UPDATE ",TABLE_SUMMARY, " SET value = ", summary$bestCount," WHERE key LIKE 'bestCount' AND chain = ", summary$chain,";",sep="")
        dbSendQuery(conn, query)
        
        query <- paste("UPDATE ",TABLE_SUMMARY, " SET value = ", summary$acceptedCount," WHERE key LIKE 'acceptedCount' AND chain = ", summary$chain,";",sep="")
        dbSendQuery(conn, query)
        
        query <- paste("UPDATE ",TABLE_SUMMARY, " SET value = ", summary$acceptanceRate," WHERE key LIKE 'acceptanceRate' AND chain = ", summary$chain,";",sep="")
        dbSendQuery(conn, query)
        
        dbCommit(conn)
        
       } ,error=function(e) { 
          
          logerror(paste("Updating Summary. Caught RSQLite Error: dbSendQuery() Failed with error", e))
          dbRollback(conn)
          
       })
      
      logdebug("METHOD OUT: database$updateSummary")
    },
    #
    # Insert convergence iteration in the summary
    #
    insertConvergenceIteration = function(key.value, iteration) {
      query <- paste("INSERT INTO ",TABLE_SUMMARY, " VALUES (-1, '", key.value, "',", iteration,");",sep="")
      sqlQuery(query)
    },
    #
    # Insert configuration values
    #
    insertConfiguration = function(configuration) {
      
      logdebug("METHOD IN: database$insertConfiguration")
      
      query <- paste("INSERT INTO ",TABLE_CONFIGURATION, " VALUES ('dataDaysRange','", config$dataDaysRange,"');",sep="")
      dbSendQuery(conn, query)
      
      query <- paste("INSERT INTO ",TABLE_CONFIGURATION, " VALUES ('forceSDValue','", config$meas$forceSDValue,"');",sep="")
      dbSendQuery(conn, query)
      
      query <- paste("INSERT INTO ",TABLE_CONFIGURATION, " VALUES ('parameterGeneratorStepWidth','", config$params$stepWidth,"');",sep="")
      dbSendQuery(conn, query)
      
      query <- paste("INSERT INTO ",TABLE_CONFIGURATION, " VALUES ('gelmanAfterConvergenceLoops','", config$mcmc$gelmanAfterConvergenceLoops,"');",sep="")
      dbSendQuery(conn, query)
      
      query <- paste("INSERT INTO ",TABLE_CONFIGURATION, " VALUES ('gelmanBurnPhaseIterations','", config$mcmc$gelmanBurnPhaseIterations,"');",sep="")
      dbSendQuery(conn, query)
      
      query <- paste("INSERT INTO ",TABLE_CONFIGURATION, " VALUES ('gelmanConvergenceConst','", config$mcmc$gelmanConvergenceConst,"');",sep="")
      dbSendQuery(conn, query)
      
      query <- paste("INSERT INTO ",TABLE_CONFIGURATION, " VALUES ('debugForceStop','", config$debugForceStop,"');",sep="")
      dbSendQuery(conn, query)
      
      
      logdebug("METHOD OUT: database$insertConfiguration")
      
    },
    
    #
    # Get the parameters values.
    # maxIteration: for debug mode. We can, f.e, plot the evolution of a chain convergence.
    # return data.frame
    #
    getParameters = function(chain=NULL, maxIteration=NULL) {
      
      logdebug("METHOD IN: database$getParameters")
     
      whereChain <- ""
      if (is.null(chain) == FALSE) {
        whereChain <- paste(" WHERE chain IN (", chain, ")",sep="")
        if (is.null(maxIteration) == FALSE)
          whereChain <- paste(whereChain, " AND iteration <= ", maxIteration, sep="")
      } else if (is.null(maxIteration) == FALSE) {
        whereChain <- paste(" WHERE iteration <= ", maxIteration, sep="")
      }
      
      parametersNames <- getParametersNames()
      sqlCmd <- paste("SELECT iteration, ", getColNamesInsertQuery(parametersNames), " FROM ", TABLE_PARAMETERS, whereChain,  sep="")
      
      logdebug(sqlCmd)
      
      parameters.values <- dbGetQuery(conn, sqlCmd)
      
      logdebug("METHOD OUT: database$getParameters")
      
      parameters.values
    },
    #
    # Get the parameters values with best likelihood
    # 
    # return data.frame
    #
    selectParametersBestLikelihood = function(chain.param.set = 1) {
      
      logdebug("METHOD IN: database$selectParametersBestLikelihood")
      
      group.having <- paste(" GROUP BY chain HAVING likelihood = MAX(likelihood) ORDER BY likelihood DESC", sep="")
      
      from <- paste0("(SELECT * FROM ", TABLE_PARAMETERS, " WHERE likelihood <> 0 AND chain = ", chain.param.set, ")")
      parametersNames <- getParametersNames()
      sqlCmd <- paste("SELECT iteration,", getColNamesInsertQuery(parametersNames), " FROM ", from, group.having,  sep="")
      
      logdebug(sqlCmd)
      
      #Remember we can have 4 values because of the 4 chains
      parameters.values <- dbGetQuery(conn, sqlCmd)[1,]
      
      parameters.values <- melt(parameters.values, id.vars="iteration")[-1]
      rownames(parameters.values) <- parameters.values$variable
      parameters.values <- parameters.values[-1]
      
      logdebug("METHOD OUT: database$selectParametersBestLikelihood")
      
      parameters.values
    },
    #
    # Get the parameters values ordered by likelihood
    # 
    # return data.frame
    #
    selectParametersOrderByLikelihood = function(limit = 100, chain = 1) {
      
      logdebug("METHOD IN: database$selectParametersBestLikelihood")
      
      parametersNames <- getParametersNames()
      sqlCmd <- paste("SELECT iteration,", getColNamesInsertQuery(parametersNames), "FROM", TABLE_PARAMETERS, "WHERE likelihood <> 0 AND chain =", chain, "ORDER BY likelihood DESC LIMIT", limit)
      
      logdebug(sqlCmd)
      
      parameters.values <- dbGetQuery(conn, sqlCmd)
      
      logdebug("METHOD OUT: database$selectParametersBestLikelihood")
      
      parameters.values
    },
    #
    # Getting subset of parameters. Step decide the step from one to the next one.
    # Sqlite Example: Select iteration from Parameters where chain = 1 and ((iteration*1.0/1)-iteration/1) = 0;
    #
    getParametersSubset = function(chain=NULL, step=NULL) {
      
      
      logdebug("METHOD IN: database$getParametersSubset")
      
      
      whereChain <- ""
      if (is.null(chain) == FALSE) {
        whereChain <- paste(" WHERE chain IN (", chain, ")",sep="")
        if (is.null(step) == FALSE)
          whereChain <- paste0(whereChain, " AND ((iteration*1.0/",step,")-iteration/",step,") = 0")
      } else if (is.null(step) == FALSE) {
        whereChain <- paste0(" WHERE ((iteration*1.0/",step,")-iteration/",step,") = 0")
      }
      
      parametersNames <- getParametersNames()
      sqlCmd <- paste("SELECT iteration, ", getColNamesInsertQuery(parametersNames), " FROM ", TABLE_PARAMETERS, whereChain,  sep="")
      
      logdebug(sqlCmd)
      
      parameters.values <- dbGetQuery(conn, sqlCmd)
      
      logdebug("METHOD OUT: database$getParametersSubset")
      
      parameters.values
      
    },
    
    getChainVariances = function(chain) {
      #
      # Calculate the variance for one chain
      #
      logdebug("METHOD IN: database$getChainVariance")
      #WARNING variance method is not a sqlite3 native method.
      

      
      parametersNames <- getParametersNames()
      sqlCmd <- paste("SELECT ", getColNamesAggregateFunctionQuery(parametersNames, "variance"), " FROM ", TABLE_PARAMETERS, " WHERE chain IN (", chain, ")", sep="")
      
      logdebug(sqlCmd)
      
      data.variance <- dbGetQuery(conn, sqlCmd)
      logdebug(data.variance)
      
      logdebug("METHOD OUT: database$getChainVariance")
      
      data.variance
    },
    
    getChainAverages = function(chain) {
      
      logdebug("METHOD IN: database$getChainAverage")
      
      parametersNames <- getParametersNames()
      sqlCmd <- paste("SELECT ", getColNamesAggregateFunctionQuery(parametersNames, "avg"), " FROM ", TABLE_PARAMETERS, " WHERE chain IN (", chain, ")", sep="")
      
      logdebug(sqlCmd)
      
      data.avg <- dbGetQuery(conn, sqlCmd)
      
      logdebug("METHOD OUT: database$getChainAverage")
      
      data.avg
    },
    
    countChainSamples = function(chain) {
      
      logdebug("METHOD IN: database$countChainSamples")
      
      sqlCnt <- paste("SELECT count(iteration) FROM ", TABLE_PARAMETERS, " WHERE chain IN (", chain, ")", sep="")
      
      logdebug(sqlCnt)
      
      data.cnt <- dbGetQuery(conn, sqlCnt)
      
      logdebug("METHOD OUT: database$countChainSamples")
      
      data.cnt
    },
    
    correctNumber = function(number) {
      
      if (number == -Inf) {
        number <-  -99999
      }
      
      number
    },
    
    getColNamesAggregateFunctionQuery = function(colnames, funName) {
      
      query <- ""
      
      for (i in 1:length(colnames)) {
        if (i == 1) {
          query <- paste(" ", funName, "(", colnames[i], ") as ", colnames[i], sep="")
        } else {
          query <- paste(query,", ", funName, "(", colnames[i], ") as ", colnames[i], sep="")
        }
      }
      
      query
    },
    
    correctNaN = function(value) {
      
      if (is.na(value)) {
        value <- "null"      
      }
      return(value)
      
    },
    
      
    getColNamesInsertQuery = function(colnames) {
  
      query <- gsub("\\.","",colnames[1])

      for (i in 2:length(colnames)) {
        query <- paste(query,", ",gsub("\\.","",colnames[i]),sep="")
      }
      
      query
    },
    
    getColValuesInsertQuery = function(values) {
      
      query <- ""
      
      for (i in 1:length(values)) {
        query <- paste(query,", ",correctNaN(values[i]),sep="")
      }
      
      query
    },
    #
    #
    # Secure function to send sqlQuerys with error handling
    #
    #
    sqlQuery = function(query,...)
    {
      
      logdebug("METHOD IN: sqlQuery")
      logdebug(paste("Query: ", query))
      
      tryCatch(
        
        return(dbSendQuery(conn, query))
        , error=function(e) { 
          print(paste("Caught RSQLite Error: dbSendQuery() Failed with query", query))
          stop(e)
        })
      
      logdebug("METHOD OUT: sqlQuery")
    },
    #
    #
    # insert VALUES into table
    #
    # 
    insert = function(tabname, values)
    {
      sqlQuery(paste("INSERT INTO ",tabname," VALUES ", values, sep="") )
    },
    
    # to put invert commas (apostrophs) around strings for use in db value inserts
    ticks = function(X,mark="'")
    {
      paste(mark, X ,mark,sep="")
    },
    
    #
    # Select the name of all the tables in the database
    # Return type "data.frame"
    #
    tablesNames = function() {
      logdebug("METHOD IN: database$tablesNames")
      
      query <- "SELECT name FROM sqlite_master WHERE type='table'"
      
      tables <- dbGetQuery(conn,query)
      
      logdebug("METHOD OUT: database$tablesNames")
      
      return(tables)
    },
    #
    # Get the column names of a table
    # Return type "character"
    #
    tableColumns = function(table) {
      logdebug("METHOD IN: database$tableColumns")
      
      query <- paste("PRAGMA table_info(",table,")", sep="")
      
      table_info <- dbGetQuery(conn,query)
      
      cols <- table_info[,2]
      
      logdebug("METHOD OUT: database$tableColumns")
      
      return(cols)
    },
    #
    # Get a "character" vector with the name of the parameters
    #
    getParametersNames = function() {
      
      logdebug("METHOD IN: database$getParametersNames")
      
      ParametersCols <- tableColumns('Parameters')
      
      #Remove chain, iteration, likelihood col names
      paramNames <- ParametersCols[c(-1,-2,-3)]
      
      logdebug("METHOD OUT: database$getParametersNames")
      
      return(paramNames)
      
    },
    #
    # Get the chain ids from Parameters table
    # Return type "character"
    #
    getChainIds = function() {
      
      logdebug("METHOD IN: database$getChainIds")
      
      query <- paste("SELECT chain FROM ", TABLE_PARAMETERS, " GROUP BY chain ORDER BY chain ASC", sep="")
      chains <- dbGetQuery(conn,query)[,1]
      
      logdebug("METHOD OUT: database$getChainIds")
      
      return(chains)
    },
    
    #
    # Remove all symbols from the text
    #
    rmSym = function(text) {
      return(str_replace_all(text, "[^[:alnum:]]", ""))
    },
    
    getMaxAcceptedIteration = function() {
      logdebug("METHOD IN: database$getMaxAcceptedIteration")
      
      query <- paste("SELECT max(iteration) FROM ", TABLE_PARAMETERS, sep="")
      max <- dbGetQuery(conn,query)[1,1]
      
      logdebug("METHOD OUT: database$getMaxAcceptedIteration")
      
      return(max)
    },
    
    getMinAcceptedIteration = function() {
      logdebug("METHOD IN: database$getMinAcceptedIteration")
      
      query <- paste("SELECT min(iteration) FROM ", TABLE_PARAMETERS, sep="")
      min <- dbGetQuery(conn,query)[1,1]
      
      logdebug("METHOD OUT: database$getMinAcceptedIteration")
      
      return(min)
    },
    #
    # Get all the information of parameters of one iteration.
    # This is done because of the recovery function
    #
    getParametersByIteration = function(iteration) {
      #select * from Parameters where iteration = max.iteration;
      logdebug("METHOD IN: database$getParametersByIteration")
      
      query <- paste0("SELECT * FROM ", TABLE_PARAMETERS, " WHERE iteration = ", iteration)
      parameters.info <- dbGetQuery(conn,query)
      
      logdebug("METHOD OUT: database$getParametersByIteration")
      
      return(parameters.info)
    }
    
  )#End methods List 
  
)#End RefClass







