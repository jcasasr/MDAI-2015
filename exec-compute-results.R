source('globals.R');

# create array
finalValues <- array(data=NA, dim=c(length(methodSet), length(metricSet))
                , dimnames=list(methodSet, metricSet));

#################################
# compute the mean error values #
#################################
# load mean values
load(paste(path, datasetName, "/results/", datasetName, "-meanValues.RData", sep=""));
  
for(method in methodSet) {
  for(metric in metricSet) {
    finalValues[method, metric] <- getAverageError(meanValues[method, metric, ]);
  }
}

#################
# Average error #
#################
print(paste("***", datasetName));
print("+++++++++++++++++++++");
print("+++ Average error +++");
print(finalValues);
  
# export data
dirResults <- paste("results/", sep="");
createPath(dirResults);
write.table(finalValues, file=paste(dirResults, datasetName, ".csv", sep="")
            , append=FALSE, sep = ",", row.names=TRUE, col.names=TRUE);

##########################
# normalized total error #
##########################
# duplicate structure
normalizedValues <- finalValues;
    
for(metric in metricSet) {
  normalizedValues[, metric] <- normalizedValues[, metric] - min(normalizedValues[, metric], na.rm=TRUE);
  normalizedValues[, metric] <- normalizedValues[, metric] / max(normalizedValues[, metric], na.rm=TRUE);
}
    
# print
print("++++++++++++++++++++++++");
print("+++ Normalized error +++");
print(normalizedValues);
print("++++++++++++++++++++++++++++++++++++");
print("+++ Cummulative normalized error +++");
print(rowSums(normalizedValues, na.rm=TRUE));
