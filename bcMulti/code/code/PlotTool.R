# Responsability
# Different plots for the data saved during MCMC algorithm
library(lattice)


PlotTool <- setRefClass(    
  "plottool"
  , fields = list(
    commonFolder="character",
    plotsFolder="character",
    match="list",
    sites="list",
    prior = "data.frame"
  )
  , methods = list(
    #
    # Constructor
    #
    initialize = function(...,
                          commonFolder=config$path$common,
                          plotsFolder="plots",
                          match=config$match,
                          sites=config$sites,
                          prior = data.frame()
                          )
    {
      logdebug("Initialize PlotTool")
      callSuper(..., 
                commonFolder=commonFolder,
                plotsFolder = plotsFolder,
                match=match,
                sites=sites,
                prior = prior)
    },
    #
    # Plot accepted parameter sample space throught iterations
    #
    plotAcceptedParametersSampleSpace = function() {
      
      logdebug("METHOD IN: plotTool$plotAcceptedParametersSampleSpace")
      folder <- functionName(match.call())
      mkdir(folder)
      
      values <- list()
      parametersNames <- db$getParametersNames()

      chains <- db$getChainIds()
      
      for (id in chains) {
        values[[id+1]]  <- db$getParameters(id)
      }
      for (param in parametersNames) {

        for (id in chains) { 
          filename <- paste(param,id,"accepted.png",sep="")
          png(filename=file.path(folder,filename))
          
          plot(values[[id+1]][['iteration']], values[[id+1]][[param]], xlab="Iterations", ylab=paste(param,"values"), main=paste(param,"sample space"))
          
          dev.off()
        }
      }
      
      logdebug("METHOD OUT: plotTool$plotAcceptedParametersSampleSpace")
      
    },
    #
    # Plot two parameters walk space throught iterations
    #
    plotTwoParametersWalkSpace = function(param1, param2) {
      
      logdebug("METHOD IN: plotTool$plotTwoParametersWalkSpace")
      folder <- functionName(match.call())
      mkdir(folder)
      
      values <- list()
   
      
      chains <- db$getChainIds()
      
      for (id in chains) {
        values1[[id+1]]  <- db$getParameters(id, maxIteration=2000)
        values2[[id+1]]  <- db$getParameters(id, maxIteration=5000)
        values3[[id+1]]  <- db$getParameters(id, maxIteration=10000)
        values4[[id+1]]  <- db$getParameters(id, maxIteration=15000)
        values5[[id+1]]  <- db$getParameters(id, maxIteration=20000)
        values6[[id+1]]  <- db$getParameters(id, maxIteration=30000)
        values7[[id+1]]  <- db$getParameters(id)
      }
        
        for (id in chains) { 
          filename <- paste(param1,"_",param2,id,"1accepted.png",sep="")
          png(filename=file.path(folder,filename))
          plot(values1[[id+1]][[param1]], values1[[id+1]][[param2]], xlab=param1, ylab=param2, main=paste(param1," vs ", param2))
          dev.off()
          
          filename <- paste(param1,"_",param2,id,"2accepted.png",sep="")
          png(filename=file.path(folder,filename))
          plot(values2[[id+1]][[param1]], values2[[id+1]][[param2]], xlab=param1, ylab=param2, main=paste(param1," vs ", param2))
          dev.off()
          
          filename <- paste(param1,"_",param2,id,"3accepted.png",sep="")
          png(filename=file.path(folder,filename))
          plot(values3[[id+1]][[param1]], values3[[id+1]][[param2]], xlab=param1, ylab=param2, main=paste(param1," vs ", param2))
          dev.off()
          
          filename <- paste(param1,"_",param2,id,"4accepted.png",sep="")
          png(filename=file.path(folder,filename))
          plot(values4[[id+1]][[param1]], values4[[id+1]][[param2]], xlab=param1, ylab=param2, main=paste(param1," vs ", param2))
          dev.off()
          
          filename <- paste(param1,"_",param2,id,"5accepted.png",sep="")
          png(filename=file.path(folder,filename))
          plot(values5[[id+1]][[param1]], values5[[id+1]][[param2]], xlab=param1, ylab=param2, main=paste(param1," vs ", param2))
          dev.off()
          
          filename <- paste(param1,"_",param2,id,"6accepted.png",sep="")
          png(filename=file.path(folder,filename))
          plot(values6[[id+1]][[param1]], values6[[id+1]][[param2]], xlab=param1, ylab=param2, main=paste(param1," vs ", param2))
          dev.off()
          
          filename <- paste(param1,"_",param2,id,"7accepted.png",sep="")
          png(filename=file.path(folder,filename))
          plot(values7[[id+1]][[param1]], values7[[id+1]][[param2]], xlab=param1, ylab=param2, main=paste(param1," vs ", param2))
          dev.off()
        }
     
      
      logdebug("METHOD OUT: plotTool$plotTwoParametersWalkSpace")
      
    },
    #
    # Plot the paremeters probability distributions
    #
    plotParametersKernelDensityByIteration = function() {
      
      logdebug("METHOD IN: plotTool$plotParametersDistributionByIteration")
      
      
      parametersNames <- db$getParametersNames()
      
      chains <- db$getChainIds()
      
      minIt <- db$getMinAcceptedIteration()
      maxIt <- db$getMaxAcceptedIteration()
      
      for (it in seq(minIt+100, maxIt, by=1000)) {
        plotParametersKernelDensity(chains,parametersNames, it, filePrefix="evolution")
      }
      
      plotParametersKernelDensity(chains,parametersNames, maxIt, "final")
      plotParametersHistogram(chains,parametersNames, maxIt, "final")
      
      logdebug("METHOD OUT: plotTool$plotParametersDistributionByIteration")
      
    },
    #
    # Plot the probability distribution of all the parameters for the given iteration
    # Iteration: we can choose to plot the distribution state in a specific iteration
    # filePrefix: we can play with the name of the file.
    #
    plotParametersKernelDensity = function(chains, parametersNames, iteration, filePrefix="") {
      
      logdebug("METHOD IN: plotTool$plotParametersDistribution")
      folder <- functionName(match.call())
      mkdir(folder)
      
      totalValues <-  db$getParameters(maxIteration=iteration)
      values <- list()
      
      for (id in chains) {
        values[[id]] <- db$getParameters(id,iteration)
      }
      
      for (param in parametersNames) {
        
        fileName <- paste(filePrefix,param,formatC(iteration, width=7, flag="0", format="fg"),".png",sep="_")
        png(filename=file.path(folder,fileName))
        
        plot (density(totalValues[[param]], bw = "nrd"), main = "", col="gray", type="p")
        title(main=param, sub="Parameter Kernel Density Estimation", ylab="Density")
        
        for (id in chains) {
          lines (density(values[[id]][[param]], bw = "nrd"), col=palette()[id]) 
        }
        dev.off()
        
      }
      
      logdebug("METHOD OUT: plotTool$plotParametersDistribution")
    },
    
    plotFinalParametersHistogram = function() {
      
      logdebug("METHOD IN: plotTool$plotFinalParametersHistogram")
      
      parametersNames <- db$getParametersNames()
      
      chains <- db$getChainIds()
      
      minIt <- db$getMinAcceptedIteration()
      maxIt <- db$getMaxAcceptedIteration()
      
      plotParametersHistogram(chains,parametersNames, maxIt, "final")
      
      logdebug("METHOD OUT: plotTool$plotFinalParametersHistogram")
      
    },
    
    plotParametersHistogramByIteration = function() {
      
      logdebug("METHOD IN: plotTool$plotParametersHistogramByIteration")
      
      
      parametersNames <- db$getParametersNames()
      
      chains <- db$getChainIds()
      
      minIt <- db$getMinAcceptedIteration()
      maxIt <- db$getMaxAcceptedIteration()
      
      
      for (it in seq(minIt+100, maxIt, by=1000)) {
        plotParametersHistogram(chains,parametersNames, it, filePrefix="evolution")
      }
      
      logdebug("METHOD OUT: plotTool$plotParametersHistogramByIteration")
      
    },
    getHighestDensityInterval = function(values) {
      logdebug("METHOD IN: plotTool$getHighestDensityInterval")
      
      dx <- density(values)
      dn <- cumsum(dx$y)/sum(dx$y)
      li <- which(dn >= 0.10)[1]
      ui <- which(dn >= 0.90)[1]
      interval <- dx$x[c(li,ui)]
      
      logdebug("METHOD OUT: plotTool$getHighestDensityInterval")
      
      return(interval)
    },
    #
    # Create plots with the density distribution of the parameters
    #
    plotParametersHistogram2 = function(chains, parametersNames, iteration, filePrefix="") {
      
      logdebug("METHOD IN: plotTool$plotParametersHistogram2")
      folder <- functionName(match.call())
      mkdir(folder)
      
      totalValues <-  db$getParameters(maxIteration=iteration)
      values <- list()
      
      for (id in chains) {
        values[[id]] <- db$getParameters(id,iteration)
      }
      
      best.params1 <- db$selectParametersBestLikelihood()
      best.params1$rowname <- rownames(best.params1)
      
      best.params2 <- db$selectParametersBestLikelihood(2)
      best.params2$rowname <- rownames(best.params2)
      
      best.params3 <- db$selectParametersBestLikelihood(3)
      best.params3$rowname <- rownames(best.params3)
      
      best.params4 <- db$selectParametersBestLikelihood(4)
      best.params4$rowname <- rownames(best.params4)
          
      for (param in parametersNames) {
        
        init.value <- prior[gsub("\\.","",prior$name) == param,]$init
        best.value1 <- best.params1[best.params1$rowname == param,]$value
        best.value2 <- best.params2[best.params2$rowname == param,]$value
        best.value3 <- best.params3[best.params3$rowname == param,]$value
        best.value4 <- best.params4[best.params4$rowname == param,]$value
        
        fileName <- paste("hist",filePrefix,param,formatC(iteration, width=7, flag="0", format="fg"),".png",sep="_")
        png(filename=file.path(folder,fileName))
        
        layout(rbind(1,2), heights=c(9,1))
        
        #TODO: set better the ymax
        x <- totalValues[[param]]
        HDI <- getHighestDensityInterval(x)
   
        h<-hist(x, breaks=50, col="white", xlab=paste(param,"values"),
                main=paste(param,"Histogram"), prob=TRUE)  # prob=TRUE for probabilities not counts
        
        used.col <- c()
        max.id <- 0
        for (id in chains) {
          
     #     lines(density(values[[id]][[param]]), col=palette()[id+1])
          used.col <- c(used.col, palette()[id+1])
          max.id <- id+1
        }
        
        used.col <- c(used.col, palette()[max.id+1])
        
    #    abline(v=init.value, col=palette()[max.id+1],lwd=2)
    #    abline(v=best.value1, col=palette()[2],lwd=3)
    #    abline(v=best.value2, col=palette()[3],lwd=3)
    #    abline(v=best.value3, col=palette()[4],lwd=3)
    #    abline(v=best.value4, col=palette()[5],lwd=3)
        abline(v=HDI[1], col="red",lwd=3)
        abline(v=HDI[2], col="red",lwd=3)
        
        segments(col="red", lwd=3, x0=HDI[1], y0=0, x1=HDI[2], y1=0)
        
        par(mar=c(0,0,0,0))
        plot.new()
      #  legend("center",horiz=T,c("chain1","chain2","chain3","chain4","Default"),fill=used.col)
        dev.off()
        
      }
      
      logdebug("METHOD OUT: plotTool$plotParametersHistogram2")
    },
    
    #
    # Create plots with the density distribution of the parameters
    #
    plotParametersBoxPlot = function() {
      
      logdebug("METHOD IN: plotTool$plotParametersBoxPlot")
      folder <- functionName(match.call())
      mkdir(folder)
      
      library(reshape2)
      parametersNames <- db$getParametersNames()
      
      maxIt <- db$getMaxAcceptedIteration()
      
      totalValues <-  db$getParameters(maxIteration=maxIt)
      
      totalValues <- totalValues[,c(-1)]
      logerror(capture.output(head(totalValues)))
      totalValues <- data.frame(apply(totalValues, 2, function(x) {((x - min(x))/(max(x)-min(x)))}))
      
      colnms <- colnames(totalValues)
      colnames(totalValues) <- gsub(".*parameter\\s*|value.*","",colnms)
      
      logerror(capture.output(head(totalValues)))
      
      g <- ggplot(data = melt(totalValues), aes(x=variable,y=value)) + geom_boxplot(aes(fill=variable))
      g <- g + theme(axis.text.x = element_text(angle=70, hjust = 1), axis.title.x = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(), legend.position="none")
      
      filename <- paste0("plotBoxPlot",".png")
      ggsave(file.path(folder,filename), width=15, height=5)
      
      logdebug("METHOD OUT: plotTool$plotParametersBoxPlot")
    },
    
    #
    # Create plots with the density distribution of the parameters
    #
    plotParametersMultiBoxPlot = function() {
      
      logdebug("METHOD IN: plotTool$plotParametersMultiBoxPlot")
      folder <- functionName(match.call())
      mkdir(folder)
      
      db1a <- Database(path="~/projects/calibrations/proj2Paper/Hoeglwald")
      db1a$connect()
      db2a <- Database(path="~/projects/calibrations/proj2Paper/Tharand")
      db2a$connect()
      db3a <- Database(path="~/projects/calibrations/proj2Paper/Wetzstein")
      db3a$connect()
      db5a <- Database(path="~/projects/calibrations/proj2Paper/TharandMulti")
      db5a$connect()
    
    db1b <- Database(path="~/projects/calibrations/proj2Paper/FASY/Collelongo")
    db1b$connect()
    db2b <- Database(path="~/projects/calibrations/proj2Paper/FASY/Hesse")
    db2b$connect()
    db3b <- Database(path="~/projects/calibrations/proj2Paper/FASY/Soroe")
    db3b$connect()
    db5b <- Database(path="~/projects/calibrations/proj2Paper/FASY/SoroeMulti")
    db5b$connect()
    
    db1c <- Database(path="~/projects/calibrations/proj2Paper/PISY/Brasschaat")
    db1c$connect()
    db2c <- Database(path="~/projects/calibrations/proj2Paper/PISY/Hyytiaela")
    db2c$connect()
    db3c <- Database(path="~/projects/calibrations/proj2Paper/PISY/Loobos")
    db3c$connect()
    db4c <- Database(path="~/projects/calibrations/proj2Paper/PISY/Sodanklya")
    db4c$connect()
    db5c <- Database(path="~/projects/calibrations/proj2Paper/PISY/SodanklyaMulti")
    db5c$connect()
      
      library(reshape2)
    #  parametersNames <- db$getParametersNames()
    
  #  paramsfi <- "CalloAndResp"
  #  selectParams <- c("BASEFOLRESPFRAC","FRTALLOC_BASE","FRTLOSS_SCALE","GRESPFRAC","MFOLOPT","QWODFOLMIN","RESPQ10","ROOTMRESPFRAC","WOODMRESPA")
    
 #   paramsfi <- "Nitrogen"
 #   selectParams <- c("AMAXB","EXPL_NH4","EXPL_NO3","FRET_N","NCFOLOPT","NCFRTOPT","NCSAPOPT","SENESCSTART")
    
 #   paramsfi <- "Temperature"
 #   selectParams <- c("GDDFOLEND","GDDFOLSTART","GDDWODEND","GDDWODSTART","PSNTMAX","PSNTMIN","PSNTOPT")
    
    paramsfi <- "Water"
    selectParams <- c("EXT","H2OREF_A","WUECMAX")
      
      aux <- function(data, names) {
        maxItAux <- data$getMaxAcceptedIteration()
        totalValuesAux <-  data$getParameters(maxIteration=maxItAux)
        
        matches <- grep(paste(names,collapse="|"),colnames(totalValuesAux))
        totalValuesAux <- totalValuesAux[,matches]
        totalValuesAux <- data.frame(apply(totalValuesAux, 2, function(x) {((x - min(x))/(max(x)-min(x)))}))
        colnmsAux <- colnames(totalValuesAux)
        colnames(totalValuesAux) <- gsub(".*parameter\\s*|value.*","",colnmsAux)
        return(totalValuesAux)
        
      }
    
    totalValues1a <- aux(db1a,selectParams)
    totalValues2a <- aux(db2a,selectParams)
    totalValues3a <- aux(db3a,selectParams)
    #  totalValues4a <- aux(db4a)
    totalValues5a <- aux(db5a,selectParams)
    totalValues1b <- aux(db1b,selectParams)
    totalValues2b <- aux(db2b,selectParams)
    totalValues3b <- aux(db3b,selectParams)
    #  totalValues4a <- aux(db4b)
    totalValues5b <- aux(db5b,selectParams)
    totalValues1c <- aux(db1c,selectParams)
    totalValues2c <- aux(db2c,selectParams)
    totalValues3c <- aux(db3c,selectParams)
    totalValues4c <- aux(db4c,selectParams)
    totalValues5c <- aux(db5c,selectParams)
      
      
     # g <- ggplot(data = melt(totalValues1a), aes(x=variable,y=value)) + geom_boxplot(fill=3, color=3, alpha=0.1)
  #    g <- g + geom_boxplot(data=melt(totalValues2a), aes(x=variable,y=value), fill=3, color=3, alpha=0.1)
  #    g <- g + geom_boxplot(data=melt(totalValues3a), aes(x=variable,y=value), fill=3, color=3, alpha=0.1)
     # g <- g + geom_boxplot(data=melt(totalValues4a), aes(x=variable,y=value), fill=3, color=3, alpha=0.3)
      
#g <- ggplot(data = melt(totalValues1b), aes(x=variable,y=value)) + geom_boxplot(fill=3, color=3, alpha=0.1)
#    g <- g + geom_boxplot(data=melt(totalValues1b), aes(x=variable,y=value), fill=3, color=3, alpha=0.1)
  #  g <- g + geom_boxplot(data=melt(totalValues2b), aes(x=variable,y=value), fill=3, color=3, alpha=0.1)
  #  g <- g + geom_boxplot(data=melt(totalValues3b), aes(x=variable,y=value), fill=3, color=3, alpha=0.1)
    # g <- g + geom_boxplot(data=melt(totalValues4b), aes(x=variable,y=value), fill=3, color=3, alpha=0.3)
  
g <- ggplot(data = melt(totalValues1c), aes(x=variable,y=value)) + geom_boxplot(fill=3, color=3, alpha=0.1)
#    g <- g + geom_boxplot(data=melt(totalValues1c), aes(x=variable,y=value), fill=3, color=3, alpha=0.1)
    g <- g + geom_boxplot(data=melt(totalValues2c), aes(x=variable,y=value), fill=3, color=3, alpha=0.1)
    g <- g + geom_boxplot(data=melt(totalValues3c), aes(x=variable,y=value), fill=3, color=3, alpha=0.1)
    g <- g + geom_boxplot(data=melt(totalValues4c), aes(x=variable,y=value), fill=3, color=3, alpha=0.1)
    g <- g + geom_boxplot(data=melt(totalValues5c), aes(x=variable,y=value), color=1, alpha=0.1)
   # g <- g + geom_boxplot(data=melt(totalValues5b), aes(x=variable,y=value), color=1, alpha=0.1)  
 #   g <- g + geom_boxplot(data=melt(totalValues5a), aes(x=variable,y=value), color=1, alpha=0.1)
      g <- g + theme(axis.text.x = element_text(angle=70, hjust = 1), axis.title.x = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(), legend.position="none")
      
      filename <- paste0(paramsfi,"AllplotBoxPlotPISY",".png")
      ggsave(file.path(folder,filename), width=15, height=5)
      
      logdebug("METHOD OUT: plotTool$plotParametersMultiBoxPlot")
    },
    #
    # Create plots with the density distribution of the parameters
    #
    plotParametersViolin = function() {
      
      logdebug("METHOD IN: plotTool$plotParametersViolin")
      folder <- functionName(match.call())
      mkdir(folder)
      
      library(reshape2)
      parametersNames <- db$getParametersNames()
      
      maxIt <- db$getMaxAcceptedIteration()
      
      totalValues <-  db$getParameters(maxIteration=maxIt)
      
      totalValues <- totalValues[,c(-1)]
      logerror(capture.output(head(totalValues)))
      totalValues <- data.frame(apply(totalValues, 2, function(x) {((x - min(x))/(max(x)-min(x)))}))
      
      colnms <- colnames(totalValues)
      colnames(totalValues) <- gsub(".*parameter\\s*|value.*","",colnms)
      
      logerror(capture.output(head(totalValues)))
      
      g <- ggplot(data = melt(totalValues), aes(x=variable,y=value)) + geom_violin(aes(fill=variable))
      g <- g + theme(axis.text.x = element_text(angle=70, hjust = 1), axis.title.x = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(), legend.position="none")
      
      filename <- paste0("plotViolin",".png")
      ggsave(file.path(folder,filename), width=15, height=5)
      
      logdebug("METHOD OUT: plotTool$plotParametersViolin")
    },
    #
    # Create plots with the density distribution of the parameters
    #
    scriptPosteriorsSD = function() {
      
      logdebug("METHOD IN: plotTool$scriptPosteriorsSD")
      
      file <- config$params$prior
      
      priors <- read.table(file.path(commonFolder,file), header=T)
      
      priors$sd <- (priors$max - priors$min)/sqrt(12)
      
      
      library(reshape2)
      parametersNames <- db$getParametersNames()
      
      maxIt <- db$getMaxAcceptedIteration()
      
      totalValues <-  db$getParameters(maxIteration=maxIt)
      
      totalValues <- totalValues[,c(-1)]
      logerror(capture.output(head(totalValues)))
      totalValues <- data.frame(apply(totalValues, 2, function(x) {sd(x)}))
      
      priors$sdPost <- totalValues[,1]
      priors$postdivprior <- priors$sdPost/priors$sd
      logerror("StandardDeviations")
      logerror(capture.output(totalValues))
      logerror("StandardDeviationsPosteriors")
      logerror(capture.output(priors))
      
     
      logdebug("METHOD OUT: plotTool$scriptPosteriorsSD")
    },
    #
    # Create plots with the density distribution of the parameters
    #
    plotParametersHistogram = function(chains, parametersNames, iteration, filePrefix="") {
      
      logdebug("METHOD IN: plotTool$plotParametersHistogram")
      folder <- functionName(match.call())
      mkdir(folder)
      
      totalValues <-  db$getParameters(maxIteration=iteration)
      values <- list()
      
      for (id in chains) {
        values[[id]] <- db$getParameters(id,iteration)
      }
      
      best.params1 <- db$selectParametersBestLikelihood()
      best.params1$rowname <- rownames(best.params1)
      
      best.params2 <- db$selectParametersBestLikelihood(2)
      best.params2$rowname <- rownames(best.params2)
      
      best.params3 <- db$selectParametersBestLikelihood(3)
      best.params3$rowname <- rownames(best.params3)
      
      best.params4 <- db$selectParametersBestLikelihood(4)
      best.params4$rowname <- rownames(best.params4)
      
      for (param in parametersNames) {
        
        init.value <- prior[gsub("\\.","",prior$name) == param,]$init
        best.value1 <- best.params1[best.params1$rowname == param,]$value
        best.value2 <- best.params2[best.params2$rowname == param,]$value
        best.value3 <- best.params3[best.params3$rowname == param,]$value
        best.value4 <- best.params4[best.params4$rowname == param,]$value
        
        fileName <- paste("hist",filePrefix,param,formatC(iteration, width=7, flag="0", format="fg"),".png",sep="_")
        png(filename=file.path(folder,fileName))
        
        layout(rbind(1,2), heights=c(9,1))
        
        #TODO: set better the ymax
        x <- totalValues[[param]]
        HDI <- getHighestDensityInterval(x)
        
        max.val <- 0
        for (id in chains) {
          max.val.loc <- max(density(values[[id]][[param]])$y)
          if (max.val.loc > max.val) {
            max.val <- max.val.loc
          }
        }
        h<-hist(x, ylim=c(0,max.val), breaks=50, col="white", xlab=paste(param,"values"),
                main=paste(param,"Histogram"), prob=TRUE)  # prob=TRUE for probabilities not counts
        
        used.col <- c()
        max.id <- 0
        max.val <- 0
        for (id in chains) {
          max.val.loc <- max(density(values[[id]][[param]])$y)
          if (max.val.loc > max.val) {
            max.val <- max.val.loc
          }
          
          lines(density(values[[id]][[param]]), col=palette()[id+1])
          used.col <- c(used.col, palette()[id+1])
          max.id <- id+1
        }
      
        
       # h<-hist(x, ylim=c(0,max.val),breaks=50, col="white", xlab=paste(param,"values"),
        #      main=paste(param,"Histogram"), prob=TRUE)  # prob=TRUE for probabilities not counts
      
        
        used.col <- c(used.col, palette()[max.id+1])
        
        abline(v=init.value, col=palette()[max.id+1],lwd=2)
        abline(v=best.value1, col=palette()[2],lwd=1.5, lty=2)
        abline(v=best.value2, col=palette()[3],lwd=1.5, lty=2)
        abline(v=best.value3, col=palette()[4],lwd=1.5, lty=2)
        abline(v=best.value4, col=palette()[5],lwd=1.5, lty=2)
       # abline(v=HDI[1], col="red",lwd=3)
       # abline(v=HDI[2], col="red",lwd=3)
        
       # segments(col="red", lwd=3, x0=HDI[1], y0=0, x1=HDI[2], y1=0)
        
        par(mar=c(0,0,0,0))
        plot.new()
        legend("center",horiz=T,c("chain1","chain2","chain3","chain4","Default"),fill=used.col)
        dev.off()
        
      }
      
      logdebug("METHOD OUT: plotTool$plotParametersHistogram")
    },
    #
    # Create plots with the density distribution of the parameters and best parameters
    #
    plotBestParameterLines = function(filePrefix="") {
      
      logdebug("METHOD IN: plotTool$plotBestParameterLines")
      folder <- functionName(match.call())
      mkdir(folder)
      
      parametersNames <- db$getParametersNames()

      best.params1 <- db$selectParametersOrderByLikelihood(limit = 100, chain = 1)
      best.params2 <- db$selectParametersOrderByLikelihood(limit = 100, chain = 2)
      best.params3 <- db$selectParametersOrderByLikelihood(limit = 100, chain = 3)
      best.params4 <- db$selectParametersOrderByLikelihood(limit = 100, chain = 4)
         
      for (param in parametersNames) {
        
        fileName <- paste("BestParams",filePrefix,param,".png",sep="_")
        png(filename=file.path(folder,fileName))
        
        plot.new()
        for (param.value in best.params1[,param]) {
          abline(v=param.value, col=palette()[2],lwd=1)
        }
        for (param.value in best.params2[,param]) {
          abline(v=param.value, col=palette()[3],lwd=1)
        }
        for (param.value in best.params3[,param]) {
          abline(v=param.value, col=palette()[4],lwd=1)
        }
        for (param.value in best.params4[,param]) {
          abline(v=param.value, col=palette()[5],lwd=1)
        }

        dev.off()
        
      }
      
      
      logdebug("METHOD OUT: plotTool$plotBestParameterLines")
    },
    #
    # Histogram of a sampled parameter population
    #
    plotSampledParametersHistogram = function() {
      
      logdebug("METHOD IN: plotTool$plotSubsetParametersHistogram")
      
      
      folder <- functionName(match.call())
      mkdir(folder)
      
      parametersNames <- db$getParametersNames()
      
      chains <- db$getChainIds()
      
      maxIteration <- db$getMaxAcceptedIteration()
      sample.n <- sample.size(maxIteration)
      step <- as.integer(maxIteration/sample.n)
            
      totalValues <-  db$getParametersSubset(step=step)
      values <- list()
      
      for (id in chains) {
        values[[id]] <- db$getParametersSubset(id,step=step)
      }
      
      for (param in parametersNames) {
        
        fileName <- paste("subhist",param,".png",sep="_")
        png(filename=file.path(folder,fileName))
        
        
        x <- totalValues[[param]]
        h<-hist(x, breaks=50, col="white", xlab=paste(param,"values"), 
                main=paste(param,"Histogram"), prob=TRUE)  # prob=TRUE for probabilities not counts
        
        for (id in chains) {
          lines(density(values[[id]][[param]]), col=palette()[id+1])
        }
        
        dev.off()
        
      }
      
      logdebug("METHOD OUT: plotTool$plotSubsetParametersHistogram")
      
    },
    
    #
    # Calculate a valid sample size to take the parameters with the population properties
    #
    sample.size = function(N, k=1.96, e=0.05, p=0.5, q=0.5) {
      
      logdebug("METHOD IN: PlotTool$sample.size")
      
      n <- (k^2*N*p*q)/(e^2*(N-1)+k^2*p*q)
      
      logdebug("METHOD OUT: PlotTool$sample.size")
      
      return(1)
      #return(n)
    },
    #
    # Create plots with parameters correlation in a level plot
    #
    plotParametersLevelPlotCorrelation = function() {
      
      logdebug("METHOD IN: plotTool$plotParametersLevelPlotCorrelation")
      folder <- functionName(match.call())
      mkdir(folder)
      
      parametersNames <- db$getParametersNames()
      
      chains <- db$getChainIds()
      
      maxIt <- db$getMaxAcceptedIteration()
      values <- list()
      for (id in chains) {
        values[[id]] <- db$getParameters(id,maxIt)
        
        filename <- paste("levelplot_chain",id,".png",sep="")
        png(filename=file.path(folder,filename))
        
        print(levelplot(cor(as.matrix(values[[id]])),scale=list(x=list(rot=45)),xlab="",ylab="",main="Parameters Correlations"))
        
        dev.off()
        
      }
      
      values[[99]] <- db$getParameters(NULL,maxIt)
      
      filename <- paste("levelplot.png",sep="")
      png(filename=file.path(folder,filename))
      
      print(levelplot(cor(as.matrix(values[[99]])),scale=list(x=list(rot=45)),xlab="",ylab="",main="Parameters Correlations"))
      
      dev.off()
      
      logdebug("METHOD OUT: plotTool$plotParametersLevelPlotCorrelation")
    },
    #
    # Create plots with each two parameters correlation 
    #
    plotParametersTwoByTwoCorrelations = function() {
      
      logdebug("METHOD IN: plotTool$plotParametersTwoByTwoCorrelations")
      folder <- functionName(match.call())
      mkdir(folder)
      
      parametersNames <- db$getParametersNames()
      
      chains <- db$getChainIds()
      
      maxIt <- db$getMaxAcceptedIteration()
      values <- list()
      
      for (id in chains) {
        values[[id]] <- db$getParameters(id,maxIt)
        
        for (param1 in parametersNames) {
          startPlotting <- FALSE
          
          for (param2 in parametersNames) {
            
            if (param1 == param2)
              startPlotting <- TRUE
            
            if (startPlotting) {
              fileName <- paste("correlation",param1,param2,"chain",id,".png",sep="_")
              png(filename=file.path(folder,fileName))
             
              x <- values[[id]][[param1]]
              y <- values[[id]][[param2]]
              
              plot(x,y, col="grey", main="Correlation and regresion line", xlab=param1, ylab=param2)
              abline(lm(y~x), col="red", lwd = 3)
              
              dev.off()
            }
          }
        }
      }
      
      logdebug("METHOD OUT: plotTool$plotParametersTwoByTwoCorrelations")
    },
    coefficient.determination.r2 = function(output, meas) {
      
      logdebug("METHOD IN: likelihood$coefficient.determination.r2")
  
      meas.mean <-  mean(meas)
      sim.mean <-  mean(output)
      
      sim.coff <- (output-sim.mean)
      meas.coff <- (meas-meas.mean)
      numerator <- sum(sim.coff*meas.coff)
      denominator <- (sqrt(sum(sim.coff^2))*sqrt(sum(meas.coff^2)))
      r2 <- numerator/denominator
      
      loginfo(r2)
      logdebug("METHOD OUT: likelihood$coefficient.determination.r2")
      
    },
    
    plotOutputDB = function(out.db) {
      logdebug("METHOD IN: plotTool$plotOutputDB")
      folder <- functionName(match.call())
      mkdir(folder)
      
      outData <- out.db$selectBestOutput()
      compounds <- out.db$getCompoundsName()
      logerror(capture.output(compounds))
     # sites <- out.db$selectMeasurementsSites()
      
      for (compound in compounds) {
        logerror(paste("compound",compound))
        for (site in names(sites)) {
          logerror(paste("site",site))
         
          meas <- out.db$getMeasurements(site.name=site, compound=compound)
      
          meas$date <- strptime(paste(meas$year, meas$day), "%Y %j")
          logerror(capture.output(head(meas)))
   
          #colnames(chain    site likelihood year julianday dNn2oemiskgNha1 dNnoemiskgNha1 dCco2emisautoKgCha1 DWbudkgDWm2)
          outData$date <- strptime(paste(outData$year, outData$julianday), "%Y %j")
          logerror(capture.output(head(outData)))
    # library(sqldf)
     # sql.stats.data <- paste0("SELECT outData.", compound, ", meas.value FROM meas, outData where meas.date = outData.date")
    #  stats.data <- sqldf(sql.stats.data)
    #  logerror(head(stats.data))
     # coefficient.determination.r2(out.data$DWbudkgDWm2, )
    
    #logerror(capture.output(match))
    logerror(paste("compo",compound))
          out.name <- match[[compound]][[1]]
          logerror(out.name)
          logerror(class(out.name))
          out.name <- out.db$rmSym(out.name)
          logerror(out.name)
    
      
          g <- ggplot(data=outData, aes_string(x="date", y=out.name)) + geom_line()
          g <- g + geom_point(data=meas, aes(x=date, y=value, color="red"), alpha=0.9)
      
      
        filename <- paste0("plot",site,compound,".png")
        ggsave(file.path(folder,filename), width=15, height=5)
        }
      }
      
      
      
      
      logdebug("METHOD OUT: plotTool$plotOutputDB")
    },
    getManagements = function() {
      logdebug("METHOD IN: plots$getManagements")
      library(XML)
      #years <- config$years[[1]]
      file <- config$managementFile
      
      mana <- matrix(ncol=2)
      
      #xpath: //event[@type='fertilize']//fertilize
      #attr: amount
      # <event  type="plant" time="2004-09-13" >
      
      
      xmlfile <- xmlTreeParse(file, useInternalNodes = TRUE)
      root <- xmlRoot(xmlfile, skip=FALSE)
      
      nodes <- getNodeSet(root, "//event//event")
      
      for (node in nodes) {
        time <- xmlAttrs(node)["time"][[1]]
        type <- xmlAttrs(node)["type"][[1]]
        # print(type)
        #print(time)
        mana <- rbind(mana, c(time,type))
        
      }      
      
      mana <- as.data.frame(mana)
      mana <- mana[-1,]
      colnames(mana) <- c("date","type")
      mana$date <- strptime(mana$date, "%Y-%m-%d")
      
      logdebug("METHOD OUT: plots$getManagements")
      
      return(mana)
    },
    plotOutputDBUncertainties = function(out.db,filter="0",title="ALL") {
      
      logdebug("METHOD IN: plotTool$plotOutputDBUncertainties")
      folder <- functionName(match.call())
      mkdir(folder)
      
      outData <- out.db$selectOutput(filter)
      
      mana <- matrix(ncol=2)
      if (!is.null(config$managementFile)) {
        mana <- getManagements()
      }
      colnames(mana) <- c("date","type")
      
      outData$date <- strptime(paste(outData$year, outData$julianday), "%Y %j")
      logerror(capture.output(head(outData)))
      
      iqr <- function(x, ...) {
        qs <- quantile(as.numeric(x), c(0.25, 0.75), na.rm = T)
        names(qs) <- c("ymin","ymax")
        qs
      }
      
      quan3 <- function(x, ...) {
        qs <- quantile(as.numeric(x), 0.75, na.rm = T)
        names(qs) <- "ymax"
        qs
      }
      
      quan1 <- function(x, ...) {
        qs <- quantile(as.numeric(x), 0.25, na.rm = T)
        names(qs) <- "ymax"
        qs
      }
      
      quan05 <- function(x, ...) {
        qs <- quantile(as.numeric(x), 0.05, na.rm = T)
        names(qs) <- "ymax"
        qs
      }
      
      quan95 <- function(x, ...) {
        qs <- quantile(as.numeric(x), 0.95, na.rm = T)
        names(qs) <- "ymax"
        qs
      }
      
      getmode <- function(v) {
        v <- round(v,3)
        uniqv <- unique(v)
        uniqv[which.max(tabulate(match(v, uniqv)))]
      }
      
      compounds <- out.db$getCompoundsName()
      logerror(capture.output(compounds))
      # sites <- out.db$selectMeasurementsSites()
      
      for (compound in compounds) {
        
        logdebug(paste("compound", compound))
        
        for (site in names(sites)) {
          logerror(paste("site",site))
          
          meas <- out.db$getMeasurements(site.name=site, compound=compound)
          
          meas$date <- strptime(paste(meas$year, meas$day), "%Y %j")
          logdebug(capture.output(head(meas)))
          
          #colnames(chain    site likelihood year julianday dNn2oemiskgNha1 dNnoemiskgNha1 dCco2emisautoKgCha1 DWbudkgDWm2)

          # library(sqldf)
          # sql.stats.data <- paste0("SELECT outData.", compound, ", meas.value FROM meas, outData where meas.date = outData.date")
          #  stats.data <- sqldf(sql.stats.data)
          #  logerror(head(stats.data))
          # coefficient.determination.r2(out.data$DWbudkgDWm2, )
          logdebug("MANAGEMENT")
          logdebug(capture.output(mana))#mana[mana$type == "fertilize",]$date
          #logerror(capture.output(match))
        #  logerror(paste("compo",compound))
          out.name <- match[[compound]][[1]]
         # logerror(out.name)
       #   logerror(class(out.name))
          out.name <- out.db$rmSym(out.name)
        #  logerror(out.name)
       #dNn2oemiskgNha1
       #stat_summary(fun.data = "iqr", geom="ribbon") +
          g <- ggplot(outData, aes_string(x="date", y=out.name)) +  stat_summary(fun.y = getmode, geom="line", colour="pink")  + stat_summary(fun.y = median, geom="line", colour="blue")  + stat_summary(fun.y=quan3, geom="line", colour="green") + stat_summary(fun.y=quan1, geom="line", colour="green") #+ stat_summary(fun.y = mean, fun.ymin = min, fun.ymax = max, colour="grey") 
          g <- g + geom_point(data=meas, aes(x=date, y=value), colour="red", shape=1) +  geom_pointrange(data=meas, colour="red", size=0.1, alpha=0.5, aes(x=date, y=value,ymax = value + sd, ymin=value - sd))
          g <- g +  ylab(paste("Kg",compound,"/ha")) + xlab("Date") + ggtitle(paste(compound,"simulations"))
       if (sum(is.na(mana)) == 0) {   
          g <- g + geom_vline(xintercept=as.numeric(mana[mana$type == "fertilize",]$date), colour="red", linetype="solid")   
          g <- g + geom_vline(xintercept=as.numeric(mana[mana$type == "harvest",]$date), colour="blue", linetype="solid")   
          g <- g + geom_vline(xintercept=as.numeric(mana[mana$type == "till",]$date), colour="black", linetype="solid")   
         # g <- g + geom_vline(xintercept=as.numeric(mana[mana$type == "plant",]$date), colour="blue", linetype="dotted")   
       }
       #ggsave(file=file.path(folder,"AllSample1UncIQR1.png"), width=10, height=5)
          
         # g <- ggplot(data=outData, aes_string(x="date", y=out.name)) + geom_line()
         # g <- g + geom_point(data=meas, aes(x=date, y=value, color="red"), alpha=0.9)
          
          
          filename <- paste0("plotUnc",site,compound,title,"filter",filter,"_25_75.png")
          ggsave(file.path(folder,filename), width=15, height=5)
       
       
       g <- ggplot(outData, aes_string(x="date", y=out.name)) +  stat_summary(fun.y = median, geom="line", colour="blue")  + stat_summary(fun.y=quan05, geom="line", colour="green") + stat_summary(fun.y=quan95, geom="line", colour="green") #+ stat_summary(fun.y = mean, fun.ymin = min, fun.ymax = max, colour="grey") 
       g <- g + geom_point(data=meas, aes(x=date, y=value), colour="red", shape=1) +  geom_pointrange(data=meas, colour="red", size=0.1, alpha=0.5, aes(x=date, y=value,ymax = value + sd, ymin=value - sd))
       g <- g +  ylab(paste("Kg",compound,"/ha")) + xlab("Date") + ggtitle(paste(compound,"simulations"))
       if (sum(is.na(mana)) == 0) {   
         g <- g + geom_vline(xintercept=as.numeric(mana[mana$type == "fertilize",]$date), colour="red", linetype="solid")   
         g <- g + geom_vline(xintercept=as.numeric(mana[mana$type == "harvest",]$date), colour="blue", linetype="solid")   
         g <- g + geom_vline(xintercept=as.numeric(mana[mana$type == "till",]$date), colour="black", linetype="solid")   
       }
       filename <- paste0("plotUnc",site,compound,title,"filter",filter,"_05_95.png")
       ggsave(file.path(folder,filename), width=15, height=5)
        }
      }
      
      
      
      
      logdebug("METHOD OUT: plotTool$plotOutputDBUncertainties")
    },
    #
# Pre getted iterations for output
#
plotOutputDBUncertaintiesByIterations = function(out.db,iterations=1,detail="") {
  
  logdebug("METHOD IN: plotTool$plotOutputDBUncertaintiesByIterations")
  folder <- functionName(match.call())
  mkdir(folder)
  
  outData <- out.db$selectOutputByIterations(iterations)
  
  mana <- matrix(ncol=2)
  if (!is.null(config$managementFile)) {
    mana <- getManagements()
  }
  colnames(mana) <- c("date","type")
  
  outData$date <- strptime(paste(outData$year, outData$julianday), "%Y %j")
  logerror(capture.output(head(outData)))
  
  iqr <- function(x, ...) {
    qs <- quantile(as.numeric(x), c(0.25, 0.75), na.rm = T)
    names(qs) <- c("ymin","ymax")
    qs
  }
  
  quan3 <- function(x, ...) {
    qs <- quantile(as.numeric(x), 0.75, na.rm = T)
    names(qs) <- "ymax"
    qs
  }
  
  quan1 <- function(x, ...) {
    qs <- quantile(as.numeric(x), 0.25, na.rm = T)
    names(qs) <- "ymax"
    qs
  }
  
  quan05 <- function(x, ...) {
    qs <- quantile(as.numeric(x), 0.05, na.rm = T)
    names(qs) <- "ymax"
    qs
  }
  
  quan95 <- function(x, ...) {
    qs <- quantile(as.numeric(x), 0.95, na.rm = T)
    names(qs) <- "ymax"
    qs
  }
  
  getmode <- function(v) {
    v <- round(v,3)
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  
  compounds <- out.db$getCompoundsName()
  logerror(capture.output(compounds))
    
  for (compound in compounds) {
    
    logdebug(paste("compound", compound))
    
    for (site in names(sites)) {
      logerror(paste("site",site))
      
      meas <- out.db$getMeasurements(site.name=site, compound=compound)
      
      meas$date <- strptime(paste(meas$year, meas$day), "%Y %j")
      logdebug(capture.output(head(meas)))
      
      out.name <- match[[compound]][[1]]
      out.name <- out.db$rmSym(out.name)
      
      
      g <- ggplot(outData, aes_string(x="date", y=out.name)) +  stat_summary(fun.y = median, geom="line", colour="blue")  + stat_summary(fun.y=quan05, geom="line", colour="green") + stat_summary(fun.y=quan95, geom="line", colour="green") #+ stat_summary(fun.y = mean, fun.ymin = min, fun.ymax = max, colour="grey") 
      g <- g + geom_point(data=meas, aes(x=date, y=value), colour="red", shape=1) +  geom_pointrange(data=meas, colour="red", size=0.1, alpha=0.5, aes(x=date, y=value,ymax = value + sd, ymin=value - sd))
      g <- g +  ylab(paste("Kg",compound,"/ha")) + xlab("Date") + ggtitle(paste(compound,"simulations"))
      if (sum(is.na(mana)) == 0) {   
        g <- g + geom_vline(xintercept=as.numeric(mana[mana$type == "fertilize",]$date), colour="red", linetype="solid")   
        g <- g + geom_vline(xintercept=as.numeric(mana[mana$type == "harvest",]$date), colour="blue", linetype="solid")   
        g <- g + geom_vline(xintercept=as.numeric(mana[mana$type == "till",]$date), colour="black", linetype="solid")   
      }
      filename <- paste0("plotUnc",site,compound,"Detail_",detail,"_05_95.png")
      ggsave(file.path(folder,filename), width=15, height=5)
    }
  }
  
  
  
  
  logdebug("METHOD OUT: plotTool$plotOutputDBUncertaintiesByIterations")
},
    #
    # Create a directory. If the directory exists it would be erased at first.
    #
    mkdir = function(destFolder)
    {
      logdebug("METHOD IN: plotTool$mkdir")
      
      dir.create(destFolder, recursive=T, showWarnings = F)
      
      logdebug("METHOD OUT: plotTool$mkdir")
      
    },
    
    functionName = function(call) {
      namef <- as.character(call[[1]])
      folder <- file.path(commonFolder,plotsFolder,namef[length(namef)])
      return(folder)
    }
    
   
    
  
    
  )#End methods List 
  
)#End RefClass


