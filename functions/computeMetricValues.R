# Compare netqorks
computeMetricValues <- function(datasetDir, datasetName, extension, methodSet, metricSet, iterationSet, anonSet) {
  # values: "metric" x "k-value"
  values <- array(data=NA, dim=c(length(methodSet), length(metricSet), length(iterationSet), length(anonSet))
                  , dimnames=list(methodSet, metricSet, iterationSet, anonSet));
  
  # load all metric data into "values"
  for(method in methodSet) {
    for(metric in metricSet) {
      for(iteration in iterationSet) {
        for(anon in anonSet) {
          logdebug("*** Method: %s, Metric: %s, Iteration: %s, %%Anon: %s", method, metric, iteration, anon);
          
          # load graph
          filenameAbsolut <- paste(datasetDir, "graphs/", method, "/", datasetName, "-", iteration, "-", anon, ".", extension, sep="");
          print(filenameAbsolut);
          g <- loadDataset(filenameAbsolut, "gml");
          
          # compute or load metric
          filenameAbsolut <- paste(datasetDir, "data/", method, "/", sep="")
          createPath(filenameAbsolut);
          filenameMetric <- paste(filenameAbsolut, datasetName, "-", metric, "-", iteration, "-", anon, ".RData", sep="");
          values[method, metric, iteration, as.character(anon)] <- getMetricValue(g, filenameMetric, metric);
        }
      }
    }
  }
  
  # save results
  dirResults <- paste(datasetDir, "results/", sep="");
  createPath(dirResults);
  save(values, file=paste(dirResults, datasetName, "-values.RData", sep=""));
  
  # summary
  meanValues <- array(data=NA, dim=c(length(methodSet), length(metricSet), length(anonSet))
                      , dimnames=list(methodSet, metricSet, anonSet));
  for(method in methodSet) { 
    for(metric in metricSet) {
      logdebug("*** Summary: Method: %s, Metric: %s", method, metric);
      
      meanValues[method, metric, ] <- colMeans(values[method, metric, , ]);
      
      # export data
      write.table(meanValues[method, , ], file=paste(datasetDir, "results/", datasetName, "-", method, ".csv", sep="")
                  , append=FALSE, sep = ",", row.names=TRUE, col.names=TRUE);
    }
  }
  
  # save results
  save(meanValues, file=paste(dirResults, datasetName, "-meanValues.RData", sep=""));
  
  # plot
  for(metric in metricSet) {
    logdebug("*** Plotting metric: %s", metric);
    
    # export plot
    dirPlots <- paste(datasetDir, "plots/", sep="");
    createPath(dirPlots);
    plotMeasureValues(dirPlots, datasetName, metric, meanValues[, metric, ]);
  }
}

#############
# FUNCTIONS #
#############
getMetricValue <- function(g, filenameMetric, metric) {
  
  if(file.exists(filenameMetric)) {
    # metric exists, load file
    logdebug("+++ Loading %s...", metric);
    
    load(filenameMetric);
    
  } else {
    # metric does not exist, compute it
    logdebug("+++ Computing %s...", metric);
    
    if(metric == 'lambda1') {
      # lambda_1
      value <- getLambda1(g);
    } else if(metric == 'mu2') {
      # mu_2
      value <- getMu2(g);
    } else if(metric == 'AD') {
      # Average distance
      value <- average.path.length(g, directed=FALSE);
    } else if(metric == 'D') {
      # Diameter
      value <- diameter(g, directed=FALSE);
    } else if(metric == 'T') {
      # Transitivity
      value <- transitivity(g, type="global", isolates=NaN);
    } else if(metric == 'EI') {
      # Edge intersection
      value <- edgeIntersection(G_ORIGINAL, g);
    } else if(metric == 'SC') {
      # Subgraph centrality
      value <- getSubgraphCentrality(g);
    } else if(metric == 'h') {
      # Harmonic
      value <- getHarmonic(g);
    } else if(metric == 'Q') {
      # Modularity
      value <- modularity(g, communities);
    } else if(metric == 'Core') {
      # k-core
      value <- (sum(CORENESS == graph.coreness(g)) / vcount(g));
    } else if(metric == 'BC') {
      # Betweenness centrality
      value <- getBetweennessRMS(g);
    } else if(metric == 'CC') {
      # Closeness centrality
      value <- getClosenessRMS(g);
    } else if(metric == 'DC') {
      # Degree centrality
      value <- getDegreeRMS(g);
    }
    
    # save data
    save(value, file=filenameMetric);
  }
  
  return(value);
}
