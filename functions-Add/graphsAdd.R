#################
# Random Graphs #
#################

createGraphAdd <- function(g0, g, edNum) {
  logdebug("Computing adjacency matrices...");
  A0 <- get.adjacency(g0);
  A <- get.adjacency(g);
  
  logdebug("Computing 'getEdgeDifferenceNum'...");
  ed <- ecount(g);
  
  logdebug("Starting edge modification process...");
  while(ed < edNum) {
    # create fake edge
    e <- findValueInMatrix(A, 0);
    A[e[1], e[2]] <- TRUE;
    A[e[2], e[1]] <- TRUE;
      
    ed <- ed + 1;
  }
  
  # create graph from A
  logdebug("Creating anonymous graph from adjacency matrix...");
  g <- graph.adjacency(A, mode="undirected", weighted=NULL);
  
  return(g);
}
