# sources
source('globals.R');

for(method in methodSet) {
  createRandomGraph(path, datasetName, datasetExt, datasetType, g0, method, iterationSet, anonSet);
}