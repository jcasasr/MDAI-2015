########################
# Create Random Graphs #
########################
createRandomGraph <- function(path, datasetName, datasetExt, datasetType, g0, method, iterationSet, anonSet) {
  
  # check directory for saving graphs
  subdir <- paste(path, datasetName, "/graphs/", method, "/", sep="");
  createPath(subdir);
  
  # create structure for verifying the EI
  ei <- array(data=NA, dim=c(length(iterationSet), length(anonSet)), dimnames=c(iterationSet, anonSet));
  en <- array(data=NA, dim=c(length(iterationSet), length(anonSet)), dimnames=c(iterationSet, anonSet));
  
  # create edge's number set
  if(method=="Add") {
    anonNumEdgesSet <- rep.int(ecount(g0), length(anonSet)) + round(anonSet * ecount(g0) / 100);
  } else if(method=="Del") {
    anonNumEdgesSet <- rep.int(ecount(g0), length(anonSet)) - round(anonSet * ecount(g0) / 100);
  } else if(method=="AddDel" | method=="Switch") {
    anonNumEdgesSet <- round(anonSet * ecount(g0) / 100);
  } else {
    logerror("Method not defined '%s'", method);
    stop();
  }
  
  for(iteration in iterationSet) {
    loginfo("Iteration %d / %d", iteration, max(iterationSet));
    g <- g0;
    
    # iteration
    for(i in 1:length(anonSet)) {
      anon <- anonSet[i];
      anonNumEdges <- anonNumEdgesSet[i];
      loginfo("Creating random graph [%d %%, %d edges]", anon, anonNumEdges);
      
      # create random graph
      if(method=="Add") {
        g <- createGraphAdd(g0, g, anonNumEdges);
        
      } else if(method=="Del") {
        g <- createGraphDel(g0, g, anonNumEdges);
        
      } else if(method=="AddDel") {
        g <- createGraphAddDel(g0, g, anonNumEdges);
        
      } else if(method=="Switch") {
        g <- createGraphSwitch(g0, g, anonNumEdges);
        
      } else {
        logerror("Method not defined '%s'", method);
        stop();
      }
      summary(g);
      # save
      filename <- paste(subdir, datasetName, "-", iteration, "-", anon, ".", datasetExt, sep="");
      print(filename);
      print(datasetType);
      write.graph(g, file=filename, format=datasetType);
      loginfo("File saved: %s", filename);
      
      # compute EI
      ei[iteration, anon] <- edgeIntersection(g0, g) / ecount(g0);
      en[iteration, anon] <- ecount(g);
    }
  }
  
  ################################### 
  # verify the results by EI metric #
  ###################################
  # params
  dirPlots <- paste(path, datasetName, "/plots/", sep="");
  createPath(dirPlots);
  # plots
  plotMeasureValues(dirPlots, paste("EI-VER", datasetName, method, sep="-"), "EI", colMeans(ei));
  plotMeasureValues(dirPlots, paste("EN-VER", datasetName, method, sep="-"), "m", colMeans(en));
}
