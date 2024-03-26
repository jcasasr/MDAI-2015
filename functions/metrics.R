# Get the subgraph centrality
getSubgraphCentrality <- function(g) {
  return(sum(subgraph.centrality(g)) / vcount(g));
}

# Get the largest eigenvalue of the adjacency matrix
getLambda1 <- function(g) {
  e <- eigen(get.adjacency(g));
  return(max(e$values));
}

# Get the second smallest eigenvalue of the Laplacian Matrix
getMu2 <- function(g, index=2) {
  L <- graph.laplacian(g);
  e <- eigen(L);
  eigenvalues <- sort(e$values, decreasing=FALSE);
  
  return(eigenvalues[index]);
}

# Get the harmonic value
# ChesterEtAl:2012
# TODO
getHarmonic <- function(g) {
  n <- vcount(g);
  sp <- shortest.paths(g);
  
  value <- 0;
  for(i in 1:length(sp[,1])) {
    for(j in 1:length(sp[i,])) {
      if(sp[i,j] > 0) {
        value <- value + (1/sp[i,j]);
      }
    }
  }
  term <- (1/(n*(n-1)));
  res <- (1 / (term * value));
  
  return(res);
}

#
edgeIntersection <- function(g1, g2) {
  logdebug("edgeIntersection: Starting...");
  numNodesG1 <- length(V(g1));
  numNodesG2 <- length(V(g2));
  
  if(numNodesG1 != numNodesG2) {
    logerror("edgeIntersection: ERROR: Different number of nodes G1=%d and G2=%d",numNodesG1, numNodesG2);
    
    retutrn(NA);
  } else {
    numEdgesG1 <- length(E(g1));
    numEdgesG2 <- length(E(g2));
    total <- max(numEdgesG1,numEdgesG2);
    inter <- 0;
    
    # use matrix
    mG1 <- get.adjacency(g1);
    mG2 <- get.adjacency(g2);
    
    # count number of equal edges
    if(is.directed(g1) && is.directed(g2)) {
      stop("NOT YET IMPLEMENTED!");
      
    } else {
      mA <- mG1 * mG2;
      inter <- sum(mA[mA>0]);
      # undirected
      inter <- inter/2;
    }
    
    loginfo("edge_intersection: datasets G1=%d and G2=%d", numEdgesG1, numEdgesG2);
    loginfo("edge_intersection: Edge intersection(G1,G2) = %d/%d [%.2f %%]", inter, total, ((inter/total)*100),"%");
    
    return(inter);
  }
}

getBetweennessRMS <- function(g) {
  return(computeDiff(BETWEENNESS, betweenness(g, directed=FALSE, normalized=TRUE)));
}

getClosenessRMS <- function(g) {
  return(computeDiff(CLOSENESS, closeness(g, mode="all", normalized=TRUE)));
}

getDegreeRMS <- function(g) {
  return(computeDiff(DEGREE, degree(g, mode="all", normalized=TRUE)));
}
