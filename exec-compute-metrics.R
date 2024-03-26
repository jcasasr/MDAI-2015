# sources
source('globals.R');

# NYAP!!!!
G_ORIGINAL <- g0;
CORENESS <- graph.coreness(g0);
BETWEENNESS <- betweenness(g0, directed=FALSE, normalized=TRUE);
CLOSENESS <- closeness(g0, mode="all", normalized=TRUE);
DEGREE <- degree(g0, mode="all", normalized=TRUE);

# params
datasetDir <- paste(path, datasetName, "/", sep="");

computeMetricValues(datasetDir, datasetName, datasetExt, methodSet, metricSet, iterationSet, anonSet);
