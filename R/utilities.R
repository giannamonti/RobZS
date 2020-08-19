prepara0 <- function(x,y,family,index=NULL,robu=NULL){
  # default classical scale, robust scale for robu=1

  if (is.null(robu)) robu=0
  if (is.null(index)) {
    if (robu>0){
      muy <- median(y)
      mux <- apply(x,2,median)
      sigx <- apply(x,2,mad)
    } else {
      muy <- mean(y)
      mux <- apply(x,2,mean)
      sigx <- apply(x,2,sd)
    }
  } else {
    if (robu>0){
      muy <- median(y[index])
      mux <- apply(x[index,],2,median)
      sigx <- apply(x[index,],2,mad)
    } else {
        muy <- mean(y[index])
      mux <- apply(x[index,],2,mean)
      sigx <- apply(x[index,],2,sd)
    }
  }
  xnor <- scale(x,mux,sigx)
  ycen <- scale(y,muy,FALSE)
  return(list(xnor=xnor,ycen=ycen,mux=mux,sigx=sigx,muy=muy))
}


weight.gaussian <- function(resi,ind,del){
  if(is.logical(ind)){
    h <- length(which(ind==TRUE))
  }else{
    h <- length(ind)
  }
  n <- length(resi)
  mu <- mean(resi[ind])
  rc <- (resi - mu)
  qn <- qnorm((h+n)/ (2*n))   # required quantile
  cdelta <- 1 / sqrt(1 - (2*n)/(h/qn) * dnorm(qn))
  s <- sqrt(mean(rc[ind]^2)) * cdelta
  we <- as.integer(abs(rc/s) <= qnorm(1-del))
  out <- list(we=we,mu=mu,s=s)

  return(out)
}


addColnames <- function(x) {
  # 'x' needs to be a matrix
  if(is.null(colnames(x))) colnames(x) <- paste("x", seq_len(ncol(x)), sep="")
  x
}


addIntercept <- function(x, check = FALSE) {
  # add intercept column to design matrix
  if(!check || is.na(match("(Intercept)", colnames(x)))) {
    cbind("(Intercept)"=rep.int(1, nrow(x)), x)
  } else x
}


uptrimMSE<- function(x,trim=0.1){
  # computes trim% upper trimmed mean
  return(mean(x[x<quantile(x,1-trim)]))
}




