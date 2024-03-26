source('globals.R');

plotDegreeDist <- function(g, filename) {
  setEPS();
  postscript(filename, width=8.0, height=6.0, pointsize=26);
  par(mar=c(2, 2, 0.5, 0.5));
  # plot values
  plot(sort(degree(g)), col="red", xlab="Vertices", ylab="Frequency");
  dev.off();
}

# Degree distribution
ba1000 <- loadDataset(paste(path, "barabasi-n1000-p1-m5/graphs/barabasi-n1000-p1-m5.gml", sep=""), "gml");
plotDegreeDist(ba1000, "plots-final/barabasi-n1000-p1-m5-Hist.eps");
er1000 <- loadDataset(paste(path, "er-n1000-p10/graphs/er-n1000-p10.gml", sep=""), "gml");
plotDegreeDist(er1000, "plots-final/er-n1000-p10-Hist.eps");

# karate
dataset <- "karate";
load(paste(path, dataset, "/results/", dataset, "-meanValues.RData", sep=""));
finalPlots(dataset, 'lambda1', meanValues[,'lambda1',], legendPosition="bottomleft");

# jazz
dataset <- "jazz";
load(paste(path, dataset, "/results/", dataset, "-meanValues.RData", sep=""));
finalPlots(dataset, 'AD', meanValues[,'AD',], legendPosition="right");

# URV-email
dataset <- "URV-email";
load(paste(path, dataset, "/results/", dataset, "-meanValues.RData", sep=""));
finalPlots(dataset, 'CC', meanValues[,'CC',], legendPosition="right");

# polblogs
dataset <- "polblogs";
load(paste(path, dataset, "/results/", dataset, "-meanValues.RData", sep=""));
finalPlots(dataset, 'T', meanValues[,'T',], legendPosition="bottomleft");
