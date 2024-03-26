getDegreeHistogramFromDegreeSequence <- function(d) {
  labels <- 0:max(d);
  h <- rep.int(0,length(labels));
  for (i in 1:length(h)) {
    h[i] <- sum(d==labels[i]);
  }
  
  # return
  return(h);
}

# Get k-anonymity value
# -h: degree histogram
# @return: k-anonymity value
getKAnonymityValueFromHistogram <- function(h) {
  return(min(h[h>0]));
}

getKAnonymityValueFromDegreeSequence <- function(d) {
  return(getKAnonymityValueFromHistogram(getDegreeHistogramFromDegreeSequence(d)));
}

getKAnonymityValueFromGraph <- function(g) {
  return(getKAnonymityValueFromDegreeSequence(degree(g)));
}
