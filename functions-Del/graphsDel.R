#################
# Random Graphs #
#################

createGraphDel <- function(g0, g, edNum) {
  logdebug("Computing adjacency matrices...");
  A0 <- get.adjacency(g0);
  A <- get.adjacency(g);
  
  logdebug("Computing 'getEdgeDifferenceNum'...");
  ed <- ecount(g);
  
  logdebug("Starting edge modification process...");
  while(ed > edNum) {
    # remove edge
    e <- findValueInMatrix(A, 1);
    A[e[1], e[2]] <- FALSE;
    A[e[2], e[1]] <- FALSE;
      
    ed <- ed - 1;
  }
  
  # create graph from A
  logdebug("Creating anonymous graph from adjacency matrix...");
  g <- graph.adjacency(A, mode="undirected", weighted=NULL);
  
  return(g);
}
