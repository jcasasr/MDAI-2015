#################
# Random Graphs #
#################

createGraphAddDel <- function(g0, g, edNum) {
  logdebug("Computing adjacency matrices...");
  A0 <- get.adjacency(g0);
  A <- get.adjacency(g);
  
  logdebug("Computing 'getEdgeDifferenceNum'...");
  ed <- getEdgeDifferenceNum(A0, A);
  logdebug("ED=%s, must be %s [%s edges]", ed, edNum, edNum-ed);
  
  logdebug("Starting edge modification process...");
  while(ed < edNum) {
    ##################
    # create fake edge
    e <- findValueInMatrix(A, 0);
    while(A0[e[1], e[2]]==TRUE) {
      # it means that 'e' exists in A0, therefore 'e' was removed in previous perturbation steps
      e <- findValueInMatrix(A, 0);
    }
    A[e[1], e[2]] <- TRUE;
    A[e[2], e[1]] <- TRUE;
    
    #############
    # remove edge
    e <- findValueInMatrix(A, 1);
    while(A0[e[1], e[2]]==FALSE) {
      # it means that 'e' does not exist in A0, therefore 'e' was added in previous perturbation steps
      e <- findValueInMatrix(A, 1);
    }
    A[e[1], e[2]] <- FALSE;
    A[e[2], e[1]] <- FALSE;
    
    ed <- ed + 1;
  }
  
  # create graph from A
  logdebug("Creating anonymous graph from adjacency matrix...");
  g <- graph.adjacency(A, mode="undirected", weighted=NULL);
  
  return(g);
}
