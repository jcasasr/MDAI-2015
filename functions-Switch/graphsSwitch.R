#####################
# Switch Graphs #
#####################
createGraphSwitch <- function(g0, g, edNum) {
  logdebug("Computing adjacency matrices...");
  A0 <- get.adjacency(g0);
  A <- get.adjacency(g);
  
  logdebug("Computing 'getEdgeDifferenceNum'...");
  ed <- getEdgeDifferenceNum(A0, A);
  logdebug("ED=%s, must be %s [%s edges]", ed, edNum, edNum-ed);
  
  logdebug("Starting edge modification process...");
  while(ed < edNum) {
    # e1 is an existing edge
    e1 <- findValueInMatrix(A, 1);
    s <- e1[1];
    t1 <- e1[2];
    
    # (s,t1) exists in G0
    if(A0[s,t1]==TRUE) {
      # e2 is a non-existing edge with the same source vertex
      t2 <- findValueInVector(A[s, ], 0);

      # (s,t2) doesn't exist in G0
      if(A0[s,t2]==FALSE && s!=t2 && t1!=t2) {
        # remove e1
        A[s, t1] <- FALSE;
        A[t1, s] <- FALSE;
        # create e2
        A[s, t2] <- TRUE;
        A[t2, s] <- TRUE;
        
        ed <- ed + 1;
      }
    }
  }
  
  # create graph from A
  logdebug("Creating anonymous graph from adjacency matrix...");
  g <- graph.adjacency(A, mode="undirected", weighted=NULL);
  
  return(g);
}
