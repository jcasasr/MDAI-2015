###########################
# Load and clean datasets #
###########################

loadDataset <- function(path, type) {
  g <- read.graph(path, format=type);
  g <- simplify(g, remove.loops=TRUE, remove.multiple=TRUE);
  loginfo("Network '%s' loaded!", path);
  summary(g);
  
  return(g);
}

loadAndCorrectDataset <- function(path, type) {
  # load
  g <- read.graph(path, format=type);
  show("*** Original dataset:");
  summary(g);
  
  # to undirected
  g <- as.undirected(g);
  # simplify
  g <- simplify(g, remove.loops=TRUE, remove.multiple=TRUE);
  # remove isolated nodes
  #g <- delete.vertices(g, which(degree(g) < 1));
  
  show("*** Cleaned dataset:");
  summary(g);
  
  filename <- paste(path, ".gml", sep="");
  write.graph(g, file=filename, format="gml");
  show(paste("*** GML file saved at: ", filename, sep=""));
  
  return(g);
}