plotMeasureValues <- function(path, datasetName, measure, m) {
  # params
  if(is.array(m)) {
    ymin <- min(m[,], na.rm=TRUE);
    ymax <- max(m[,], na.rm=TRUE);
    nrows <- nrow(m);
    ncols <- ncol(m);
  } else {
    ymin <- min(m, na.rm=TRUE);
    ymax <- max(m, na.rm=TRUE);
    nrows <- 1;
    ncols <- length(m);
  }
  # colors and lines
  colors <- rainbow(nrows);
  lty <- 1:nrows; # line type
  lwd <- 2 # line width
  # X values
  xs <- 0:(ncols-1);
  
  #######
  # PNG #
  #######
  filename <- paste(path, datasetName, "-", measure, ".png", sep="");
  png(file=filename, width=640, height=480, units='px', pointsize=16);
  par(mar=c(4, 4, 1, 1));
  # plot values
  for(i in 1:nrows) {
    if(i==1) {
      if(is.array(m)) {
        ys <- m[i,];
      } else {
        ys <- m;
      }
      plot(x=xs, y=ys, type="l", col=colors[i], lty=lty[i], lwd=lwd, ylim=c(ymin, ymax), main=datasetName, xlab="Anon. (%)", ylab=measure);
    } else {
      lines(x=xs, y=m[i,], type="l",  col=colors[i], lty=lty[i], lwd=lwd);
    }
  }
  # plot original value line
  if(is.array(m)) {
    ys <- m[1,1];
  } else {
    ys <- m[1];
  }
  lines(x=xs, y=rep(x=ys, times=ncols), type="l",  col="gray", lty=1, lwd=1);
  # plot legend
  if(is.array(m)) {
    legendNames <- dimnames(m)[[1]];
  } else {
    legendNames <- "-verify-";
  }
  legend(x="topright", legend=legendNames, ncol=1, bty="n", col=colors, lty=lty, lwd=lwd);
  dev.off();
  
  #######
  # EPS #
  #######
  lwd <- 3 # line width
  
  setEPS();
  filename <- paste(path, datasetName, "-", measure, ".eps", sep="");
  postscript(filename, width=8.0, height=6.0, pointsize=20);
  par(mar=c(4, 4, 1, 1));
  # plot values
  for(i in 1:nrows) {
    if(i==1) {
      if(is.array(m)) {
        ys <- m[i,];
      } else {
        ys <- m;
      }
      plot(x=xs, y=ys, type="l", col=colors[i], lty=lty[i], lwd=lwd, ylim=c(ymin, ymax), xlab="", ylab="");
    } else {
      lines(x=xs, y=m[i,], type="l",  col=colors[i], lty=lty[i], lwd=lwd);
    }
  }
  # plot original value line
  if(is.array(m)) {
    ys <- m[1,1];
  } else {
    ys <- m[1];
  }
  lines(x=xs, y=rep(x=ys, times=ncols), type="l",  col="gray", lty=1, lwd=1);
  # plot legend
  if(is.array(m)) {
    legendNames <- dimnames(m)[[1]];
  } else {
    legendNames <- "-verify-";
  }
  legend(x="topright", legend=legendNames, ncol=1, bty="n", col=colors, lty=lty, lwd=lwd);
  dev.off();
}

finalPlots <- function(datasetName, measure, m, legendPosition="topright") {
  # params
  ymin <- min(m[,], na.rm=TRUE);
  ymax <- max(m[,], na.rm=TRUE);
  nrows <- nrow(m);
  ncols <- ncol(m);
  # colors and lines
  colors <- c('red', 'blue', 'black', 'green', 'gray');
  lty <- 1:nrows; # line type
  lwd <- 3 # line width
  # X values
  xs <- 0:(ncols-1);
  
  #######
  # EPS #
  #######
  setEPS();
  filename <- paste("plots-final/", datasetName, "-", measure, ".eps", sep="");
  postscript(filename, width=8.0, height=6.0, pointsize=26);
  par(mar=c(2, 2, 0.5, 0.5));
  # plot values
  for(i in 1:nrows) {
    if(i==1) {
      plot(x=xs, y=m[i,], type="l", col=colors[i], lty=lty[i], lwd=lwd, ylim=c(ymin, ymax), xlab="", ylab="");
    } else {
      lines(x=xs, y=m[i,], type="l",  col=colors[i], lty=lty[i], lwd=lwd);
    }
  }
  # plot legend
  legendNames <- dimnames(m)[[1]];
  legend(x=legendPosition, legend=legendNames, ncol=1, bty="n", col=colors, lty=lty, lwd=lwd);
  dev.off();
}
