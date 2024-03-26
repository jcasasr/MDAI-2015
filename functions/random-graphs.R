# n: number of vertices
# power: power of probability density function (power=1 linear preferential attachment)
# m: number of edges added at each step (total number ~ m*n)
createBarabasiGraph <- function(n, power, m, save=FALSE) {
  # create a power-law network
  g <- barabasi.game(n=n, power=power, m=m, directed=FALSE);
  # plot
  plot(sort(degree(g)), col="red");
  # save
  if(save) {
    name <- paste(path, "barabasi-n", n, "-p", power, "-m", m, ".gml", sep="");
    write.graph(g, file=name, format="gml");
  }
  
  return(g);
}

createERGraph <- function(n, p, save=FALSE) {
  # create
  g <- erdos.renyi.game(n, p/n, type="gnp", directed=FALSE, loops=FALSE);
  # plot
  plot(sort(degree(g)), col="red");
  # save
  if(save) {
    name <- paste("er-n", n, "-p", p, sep="");
    write.graph(g, file=paste(path, name, ".gml", sep=""), format="gml");
  }
  
  return(g);
}
