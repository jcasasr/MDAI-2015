source('globals.R');

# redefine interesting methods, metric ans networks
methodSet <- methodSet;
metricSet <- c("AD", "T", "BC", "CC", "DC", "lambda1");
networkSet <- c("er-n1000-p10", "barabasi-n1000-p1-m5", "karate", "jazz", "URV-email", "polblogs");

# create array
finalValues <- array(data=NA, dim=c(length(networkSet), length(methodSet), length(metricSet))
                , dimnames=list(networkSet, methodSet, metricSet));

# compute the mean error values
for(network in networkSet) {
  # load mean values
  load(paste(path, network, "/results/", network, "-meanValues.RData", sep=""));
  
  for(method in methodSet) {
    for(metric in metricSet) {
      finalValues[network, method, metric] <- getAverageError(meanValues[method, metric, ], numDec=4);
    }
  }
  
  # Average error
  print(paste("***", network));
  print("+++++++++++++++++++++");
  print("+++ Average error +++");
  print(finalValues[network, , ]);
  
  # export data
  dirResults <- paste("results/", sep="");
  createPath(dirResults);
  write.table(finalValues[network, , ], file=paste(dirResults, datasetName, ".csv", sep="")
              , append=FALSE, sep = ",", row.names=TRUE, col.names=TRUE);
  
  # normalized total error
  # duplicate structure
  normalizedValues <- finalValues[network, ,];
    
  for(metric in metricSet) {
    normalizedValues[, metric] <- normalizedValues[, metric] - min(normalizedValues[, metric], na.rm=TRUE);
    normalizedValues[, metric] <- normalizedValues[, metric] / max(normalizedValues[, metric], na.rm=TRUE);
  }
    
  # print
  #print("++++++++++++++++++++++++");
  #print("+++ Normalized error +++");
  #print(normalizedValues[, ]);
  print("++++++++++++++++++++++++++++++++++++");
  print("+++ Cummulative normalized error +++");
  print(rowSums(normalizedValues[, ], na.rm=TRUE));
}
