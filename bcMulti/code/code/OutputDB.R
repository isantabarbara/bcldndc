# Responsability
# Initialize and insert output data in sqlite database
require("RSQLite")
#TODO: How valuable is adding the output only for the calibration years??

OutputDB <- setRefClass(    
  "outputdb"
  , fields = list(
    path="character",
    file="character",
    conn="SQLiteConnection",
    TABLE_OUTPUTS = "character",
    TABLE_MEASUREMENTS = "character"
  )
  , methods = list(
    #
    # Constructor
    #
    initialize = function(...,
                          path = config$db$path,
                          file = paste0("Output",config$db$file),
                          TABLE_OUTPUTS = "Outputs",
                          TABLE_MEASUREMENTS = "Measurements"
                          
    )
    {
      
      loginfo("Initialize Output Database")
      
      callSuper(...,
                path = path,
                file = file,
                TABLE_OUTPUTS = TABLE_OUTPUTS,
                TABLE_MEASUREMENTS = TABLE_MEASUREMENTS
      )
      
    },
    #
    # Connect to the data base in file
    #
    connect = function() {
      logdebug("METHOD IN: OutputDB$connect")
      
      dbFile <- file.path(path,file)
      conn  <<- dbConnect(dbDriver("SQLite"), dbname = dbFile)
    
      logdebug("METHOD OUT: OutputDB$connect")
      
    },
    #
    # Delete the database file
    #
    delete = function() {
      logdebug("METHOD IN: OutputDB$deleteFile")
      
      dbFile <- file.path(path,file)
      unlink(dbFile, recursive = F, force = T)
      
      logdebug("METHOD OUT: OutputDB$deleteFile")
    },
    #
    # Create tables if they do not exist 
    # parametersNames: a vector with the parameter names
    #      
    create = function() {
      
      logdebug("METHOD IN: OutputDB$create")
      
      tryCatch({
        
        dbSendQuery(conn, paste("CREATE TABLE ", TABLE_MEASUREMENTS, 
                                "(site INTEGER, compound TEXT, year INTEGER, day INTEGER, value FLOAT, sd FLOAT);", sep=""))
        dbSendQuery(conn, paste("CREATE TABLE Likelihood (chain INTEGER, iteration INTEGER, compound TEXT, likelihood FLOAT, value FLOAT, detail TEXT);", sep=""))
      },
      error= function(err) {
        logerror(err)
        logerror("Error creating tables. They probably exists")
      })
      
      logdebug("METHOD OUT: OutputDB$create")
      
    },
    #
    # Create the outputs table
    #
    createOutputTable = function(output.data) {
      logdebug("METHOD IN: OutputDB$createOutputTable")
      
      colnames(output.data) <- sapply(colnames(output.data), function(x) rmSym(x))
      
      dbSendQuery(conn, paste0("CREATE TABLE ", TABLE_OUTPUTS, "(chain, site, likelihood",getColNamesCreateQuery(colnames(output.data)),");"))
      
      logdebug("METHOD OUT: OutputDB$createOutputTable")
    },
    
    #
    # Remove all symbols from the text
    #
    rmSym = function(text) {
      return(str_replace_all(text, "[^[:alnum:]]", ""))
    },
    #
    # We save the likelihood summary to check later details
    #
    writeLikelihoodSummary = function(summary) {
      logdebug("METHOD IN: OutputDB$writeLikelihoodSummary")
      logdebug(capture.output(summary))
      tryCatch({
      apply(summary, 1, function(row) {
        query <- paste0("INSERT INTO Likelihood VALUES (",row[5], ",",row[4], ",", paste0("'", row[1],"'"), ",", row[6], ",", row[2], ",",paste0("'", row[3],"'"),");")
        sqlQuery(query)
      })
      } ,
      error= function(err) {
        logerror(paste("Error inserting Likelihoods.", err))
      })
      logdebug("METHOD OUT: OutputDB$writeLikelihoodSummary")
    },
    #
    # Write the output data into the data base
    #
    writeOutput = function(chain, site, likelihood, output.data) {
      logdebug("METHOD IN: OutputDB$writeOutput")
      
      
      apply(output.data, 1, function(row) {
        values <- paste(row,collapse=", ")
        query <- paste("INSERT INTO ", TABLE_OUTPUTS, " VALUES (",chain, ",",paste0("'", site,"'"), ",", likelihood, ",", values,");", sep="")
        sqlQuery(query)
      })
      
      logdebug("METHOD OUT: OutputDB$writeOutput")
    },
    #
    # Iterate over all compound measurements and insert them.
    #
    insertMeasurements = function(site.name, measurements) {
      
      logdebug("METHOD IN: outputDB$insertMeasurements")
      
      logdebug(capture.output(measurements))
      site.id <- getSiteId(site.name)
      
      for (compound in names(measurements)) {
        apply(measurements[[compound]], 1, function(row) {
          query <- paste("INSERT INTO ", TABLE_MEASUREMENTS, " VALUES ('",site.id, "', '",compound, "',", row[['year']], ", ", row[['day']], ",", row[[3]], ", ", row[[4]], ");", sep="")
          sqlQuery(query)
        })
      }
      
      logdebug("METHOD OUT: outputDB$insertMeasurements")
    },
    #
    # Get the measurements
    #
    getMeasurements = function(site.name = NULL, compound = NULL) {
      
      logdebug("METHOD IN: database$getMesaurements")
      
      site.id <- getSiteId(site.name)
      
      order.by <- " ORDER BY compound, year, day ASC"
    #  where <- paste0(" WHERE year > 1999 and year < 2006 and site IN ('", site.id, "')")
      where <- paste0(" WHERE site IN ('", site.id, "')")
      if (is.null(compound) == FALSE) {
        where <- paste0(where, " AND compound IN ('", compound, "')")
      }
      
      sqlCmd <- paste0("SELECT * FROM ", TABLE_MEASUREMENTS,  where, order.by)
      
      logdebug(sqlCmd)
      
      measurements.values <- dbGetQuery(conn, sqlCmd)
      
      logdebug("METHOD OUT: database$getMesaurements")
      
      return(measurements.values)
    },
    #
    # Get the compound names from the Measurements table
    # Return type "character"
    #
    getCompoundsName = function() {
      
      logdebug("METHOD IN: database$getCompoundsName")
      
      sqlCmd <- paste("SELECT compound FROM ", TABLE_MEASUREMENTS, " GROUP BY compound", sep="")
      
      logdebug(sqlCmd)
      
      compoundsNames <- dbGetQuery(conn, sqlCmd)[,1]
      
      logdebug("METHOD OUT: database$getCompoundsName")
      
      return(compoundsNames)
    },
    #
    # Prepare the column names query
    #  
    getColNamesCreateQuery = function(colnames) {
      
      logdebug("METHOD IN: OutputDB$getColNamesCreateQuery")
      
      query <- ""
      
      for (i in 1:length(colnames)) {
        query <- paste(query,", ",rmSym(colnames[i])," FLOAT",sep="")
      }
      
      logdebug(paste("Created Columns: ",query))
      logdebug("METHOD OUT: OutputDB$getColNamesCreateQuery")
      
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
    selectMeasurementsCompounds = function() {
      
      logdebug("METHOD IN: outputDB$selectMeasurementsCompounds")
      
      sqlCmd <- paste("SELECT DISTINCT(compound) FROM", TABLE_MEASUREMENTS)
      
      data <- dbGetQuery(conn, sqlCmd)      

      logdebug("METHOD OUT: outputDB$selectMeasurementsCompounds")
      
      return(data)
    },
    selectMeasurementsSites = function() {
      
      logdebug("METHOD IN: outputDB$selectMeasurementsSites")
      
      sqlCmd <- paste("SELECT DISTINCT(site) FROM", TABLE_MEASUREMENTS)
      
      data <- dbGetQuery(conn, sqlCmd)      
      logerror(capture.output(data))
      logdebug("METHOD OUT: outputDB$selectMeasurementsSites")
      
      return(data)
    },
    selectMeanLikelihood = function() {
      sqlCmd <- paste("SELECT avg(likelihood) FROM Outputs")
      value <- as.numeric(dbGetQuery(conn, sqlCmd)[1])
      return(value) 
    },
    selectMaxLikelihood = function() {
        sqlCmd <- paste("SELECT max(likelihood) FROM Outputs")
        value <- as.numeric(dbGetQuery(conn, sqlCmd)[1])
        return(value)
    },
    selectQLikelihood = function(Q=0.5) {
      sqlCmd <- paste("SELECT likelihood FROM Outputs")
      value <- quantile(as.numeric(dbGetQuery(conn, sqlCmd)[,1]),Q)
      return(value)
    },
    selectIterationsWithLikelihoodDetails = function(detail, Q) {
      sqlCmd <- paste0("SELECT L.value FROM likelihood as L WHERE detail like '", detail, "'")
      logerror(sqlCmd)
      value <- quantile(as.numeric(dbGetQuery(conn, sqlCmd)[,1]),Q)
      sqlCmd <- paste0("SELECT L.likelihood FROM likelihood as L WHERE detail like '", detail, "' AND value <= ", value)
      logerror(sqlCmd)
      iterations <- as.numeric(dbGetQuery(conn, sqlCmd)[,1])
      return(iterations)
    },
    
    selectBestOutput = function() {
      
      logdebug("METHOD IN: outputDB$selectBestOutput")
      
      sqlCmd <- paste("SELECT * from", TABLE_OUTPUTS, "WHERE likelihood = (SELECT MAX(likelihood) from" , TABLE_OUTPUTS, ")")
         
      data <- dbGetQuery(conn, sqlCmd)      
         
      logdebug("METHOD OUT: outputDB$selectBestOutput")
      
      return(data)
    },
    selectWorstOutput = function() {
      
      logdebug("METHOD IN: outputDB$selectBestOutput")
      
      sqlCmd <- paste("SELECT * from", TABLE_OUTPUTS, "WHERE likelihood = (SELECT MIN(likelihood) from" , TABLE_OUTPUTS, ")")
      
      data <- dbGetQuery(conn, sqlCmd)      
      
      logdebug("METHOD OUT: outputDB$selectBestOutput")
      
      return(data)
    },
    selectOutputByIterations = function(iterations = c()) {
      logdebug("METHOD IN: outputDB$selectOutputByIterations")
      data <- NULL     
      for (iteration in unique(iterations)) {
        filter <- paste0(" WHERE cast(likelihood as text) like '",iteration,"%'")
        sqlCmd <- paste("SELECT * from", TABLE_OUTPUTS,filter)
        logerror(sqlCmd)
        data1 <- dbGetQuery(conn, sqlCmd)
        if (is.null(data)) {
          data <- data1
        } else {
          data <- rbind(data,data1)
        }
        #WHERE likelihood < 0.06630862 and likelihood > 0.06630861;
      }
      #iter <- paste0("(",paste(unique(iterations), collapse=","),")")
     # filter <- paste("WHERE likelihood > 0", iter)
    #  sqlCmd <- paste("SELECT * from", TABLE_OUTPUTS,filter)#, "WHERE year > 1999 AND year < 2005")#likelihood > 0.09")#0.47")
    #  logerror(sqlCmd)
    #  data <- dbGetQuery(conn, sqlCmd)
      
      logdebug("METHOD OUT: outputDB$selectOutputByIterations")
      
      return(data)
    },
    selectOutput = function(filter="") {
      
      logdebug("METHOD IN: outputDB$selectOutput")
      filter <- paste("WHERE likelihood >", filter)
      sqlCmd <- paste("SELECT * from", TABLE_OUTPUTS,filter)#, "WHERE year > 1999 AND year < 2005")#likelihood > 0.09")#0.47")
                            
      data <- dbGetQuery(conn, sqlCmd)
                            
      logdebug("METHOD OUT: outputDB$selectOutput")
                            
      return(data)
    },
    selectOutputByLikelihood = function(likelihood) {
      
      logdebug("METHOD IN: outputDB$selectOutputByLikelihood")
      
      sqlCmd <- paste("SELECT * from", TABLE_OUTPUTS, "WHERE likelihood =", likelihood)
      
      data <- dbGetQuery(conn, sqlCmd)
      
      logdebug("METHOD OUT: outputDB$selectOutputByLikelihood")
      
      return(data)
    },
    selectLikelihood = function() {
      
      logdebug("METHOD IN: outputDB$selectLikelihood")
      
      sqlCmd <- paste("SELECT DISTINCT likelihood from", TABLE_OUTPUTS)
      
      data <- dbGetQuery(conn, sqlCmd)
      
      logdebug("METHOD OUT: outputDB$selectLikelihood")
      
      return(data)
    },
    #
    # Secure function to send sqlQuerys with error handling
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
    # Get the id for the site name
    #
    getSiteId = function(site.name) {
      logdebug("METHOD IN: database$getSiteId")
      
      #TODO: workround for plotting
       # return(as.numeric(site.name))
      #Workaround ends
      
      #Fast way to get site.name as numeric
      #site.id <- as.numeric(capture.output(cat(utf8ToInt(site.name),sep="")))
      #TODO: Possible id repetition
      site.id <- sum(utf8ToInt(site.name))
      
      logdebug("METHOD OUT: database$getSiteId")
      return(site.id)
    }
  )#End methods List 
  
)#End RefClass






