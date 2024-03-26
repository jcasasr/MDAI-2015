# libraries
library(igraph);

# sources
source('functions/computeMetricValues.R');
source('functions/createRandomGraphs.R');
source('functions/k-anonymity.R');
source('functions/load.R');
source('functions/log.R');
source('functions/metrics.R');
source('functions/plot.R');
source('functions/random-graphs.R');
source('functions/utils.R');
source('functions/utils-filesystem.R');
source('functions/utils-matrix.R');
source('functions/utils-statistics.R');
# Add
source('functions-Add/graphsAdd.R');
# Del
source('functions-Del/graphsDel.R');
# AddDel
source('functions-AddDel/graphsAddDel.R');
# Switch
source('functions-Switch/graphsSwitch.R');

# dataset params
path <- "/Volumes/DATA/MDAI-2015-datasets/";
datasetName <- "polblogs";
datasetExt <- "gml";
datasetType <- "gml";

# Anonymization methods
methodSet <- c("Add", "Del", "AddDel", "Switch");
metricSet <- c("lambda1", "mu2", "AD", "h", "T", "SC", "EI", "D", "Core", "BC", "CC", "DC");
iterationSet <- 1:10;
anonSet <- 0:25;

# info
loginfo("***********");
loginfo("*** PARAMS:");
loginfo("***********");
loginfo("Dataset: %s", datasetName);

# original graph
g0 <- loadDataset(paste(path, datasetName, "/graphs/", datasetName, ".", datasetExt, sep=""), datasetType);
