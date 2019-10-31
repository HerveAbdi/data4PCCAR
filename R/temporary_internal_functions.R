# Utilities for GSVD. All from Derek Beaton
# Github DerekBeaton
# Will be soon moved to CRAN.
# Put together on October 31, 2019. by Vincent Guillemot and Hervé


#' GSVD, authored by Derek Beaton. Cf. package GSVD on GitHub, soon available on the CRAN
#'
#' @keywords internal

gsvd <- function(DAT, # Data
                 LW,  # Left weights
                 RW,  # Right weights
                 k = 0, # Number of factors to keep
                 tol = .Machine$double.eps) {
  # preliminaries
  DAT.dims <- dim(DAT)
  if (length(DAT.dims) != 2) {
    stop("gsvd: DAT must have dim length of 2 (i.e., rows and columns)")
  }
  DAT <- as.matrix(DAT)
  RW.is.vector <-
    LW.is.vector <-
    RW.is.missing <-
    LW.is.missing <- F ##asuming everything is a matrix.
  
  ### These are here out of convenience for the tests below. They started to get too long.
  if (!missing(LW)) {
    if (is.empty.matrix(LW)) {
      stop("gsvd: LW is empty (i.e., all 0s")
    }
  }
  if (!missing(RW)) {
    if (is.empty.matrix(RW)) {
      stop("gsvd: RW is empty (i.e., all 0s")
    }
  }
  
  # check if LW and RW are missing, if they are vectors, or if they are diagonal matrices.
  if (missing(LW)) {
    LW.is.missing <- T
  } else{
    # it's here and we have to check!
    
    if (is.vector(LW)) {
      LW.is.vector <- T
    } else if (!LW.is.vector) {
      if (is.identity.matrix(LW)) {
        LW.is.missing <- T
        # warning("gsvd: LW was an identity matrix. LW will not be used in the GSVD.")
      } else if (is.diagonal.matrix(LW)) {
        LW <- diag(LW)
        
        if (length(LW) != DAT.dims[1]) {
          stop("gsvd:length(LW) does not equal nrow(DAT)")
        } else{
          LW.is.vector <- T  #now it's a vector
        }
        
      } else if (nrow(LW) != ncol(LW) | nrow(LW) != DAT.dims[1]) {
        stop("gsvd:nrow(LW) does not equal ncol(LW) or nrow(DAT)")
      }
    }
  }
  
  
  if (missing(RW)) {
    RW.is.missing <- T
  } else{
    # it's here and we have to check!
    
    if (is.vector(RW)) {
      RW.is.vector <- T
    } else if (!RW.is.vector) {
      if (is.identity.matrix(RW)) {
        RW.is.missing <- T
        # warning("gsvd: RW was an identity matrix. RW will not be used in the GSVD.")
      } else if (is.diagonal.matrix(RW)) {
        RW <- diag(RW)
        
        if (length(RW) != DAT.dims[2]) {
          stop("gsvd:length(RW) does not equal ncol(DAT)")
        } else{
          RW.is.vector <- T  #now it's a vector
        }
        
      } else if (nrow(RW) != ncol(RW) | nrow(RW) != DAT.dims[2]) {
        stop("gsvd:nrow(RW) does not equal ncol(RW) or ncol(DAT)")
      }
    }
  }
  
  
  
  if (!LW.is.missing) {
    if (LW.is.vector) {
      ## replace with sweep
      DAT <- sweep(DAT, 1, sqrt(LW), "*")
    } else{
      DAT <- (LW %^% (1 / 2)) %*% DAT
    }
  }
  
  if (!RW.is.missing) {
    if (RW.is.vector) {
      ## replace with sweep
      DAT <- sweep(DAT, 2, sqrt(RW), "*")
    } else{
      DAT <- DAT %*% (RW %^% (1 / 2))
    }
  }
  
  
  if (k <= 0) {
    k <- min(nrow(DAT), ncol(DAT))
  }
  res <- tolerance.svd(DAT, nu = k, nv = k, tol = tol)
  res$d.orig <- res$d
  res$l.orig <- res$d.orig ^ 2
  res$tau <- (res$l.orig / sum(res$l.orig)) * 100
  components.to.return <- min(length(res$d.orig), k) #a safety check
  ## u and v should already be k vectors but: be safe.
  res$d <- res$d.orig[1:components.to.return]
  res$l <- res$d ^ 2
  res$u <- as.matrix(res$u[, 1:components.to.return])
  res$v <- as.matrix(res$v[, 1:components.to.return])
  
  
  
  ## the logic here should match the one from above
  if (!LW.is.missing) {
    if (LW.is.vector) {
      res$p <- sweep(res$u, 1, 1 / sqrt(LW), "*")
      res$fi <- sweep(sweep(res$p, 1, LW, "*"), 2, res$d, "*")
    } else{
      res$p <- (LW %^% (-1 / 2)) %*% res$u
      res$fi <- sweep((LW %*% res$p), 2, res$d, "*")
    }
  } else{
    res$p <- res$u
    res$fi <- sweep(res$p, 2, res$d, "*")
  }
  
  if (!RW.is.missing) {
    if (RW.is.vector) {
      res$q <- sweep(res$v, 1, 1 / sqrt(RW), "*")
      res$fj <- sweep(sweep(res$q, 1, RW, "*"), 2, res$d, "*")
    } else{
      res$q <- (RW %^% (-1 / 2)) %*% res$v
      res$fj <- sweep((RW %*% res$q), 2, res$d, "*")
    }
  } else{
    res$q <- res$v
    res$fj <- sweep(res$q, 2, res$d, "*")
  }
  
  
  rownames(res$fi) <-
    rownames(res$u) <- rownames(res$p) <- rownames(DAT)
  rownames(res$fj) <-
    rownames(res$v) <- rownames(res$q) <- colnames(DAT)
  
  class(res) <- c("list", "GSVD", "gsvd")
  return(res)
  #return(list(fi = fi, fj = fj, p = p, q = q, u = res$u, v = res$v, d = d, d.orig = d.orig, tau = tau))
}


#' matrix.exponent, authored by Derek Beaton. Cf. package GSVD on GitHub, soon available on the CRAN
#'
#' @keywords internal

matrix.exponent <- function(x, power = 1, k = 0, ...){
  
  ##stolen from MASS::ginv()
  if (length(dim(x)) > 2L || !(is.numeric(x) || is.complex(x)))
    stop("matrix.exponent: 'x' must be a numeric or complex matrix")
  if (!is.matrix(x))
    x <- as.matrix(x)
  
  k <- round(k)
  if(k<=0){
    k <- min(nrow(x),ncol(x))
  }
  
  ## should be tested for speed.
  
  #res <- tolerance.svd(x,...)
  #comp.ret <- 1:min(length(res$d),k)
  #return( (res$u[,comp.ret] * matrix(res$d[comp.ret]^power,nrow(res$u[,comp.ret]),ncol(res$u[,comp.ret]),byrow=T)) %*% t(res$v[,comp.ret]) )
  
  
  ## the special cases:
  ## power = 0
  if(power==0){
    x <- diag(1,nrow(x),ncol(x))
    attributes(x)$message.to.user = "https://www.youtube.com/watch?v=9w1y-kMPNcM"
    return( x )
  }
  ## is diagonal
  if(is.diagonal.matrix(x)){
    return( diag( diag(x)^power ) )
    
  }
  ## is vector
  if( any(dim(x)==1) ){
    return( x^power )
  }
  
  res <- tolerance.svd(x, nu = k, nv = k, ...)
  if(k > length(res$d)){
    k <- length(res$d)
  }
  return( sweep(res$u,2,res$d[1:k]^power,"*") %*% t(res$v) )
  
}

#' tolerance.svd, authored by Derek Beaton. Cf. package GSVD on GitHub, soon available on the CRAN
#'
#' @keywords internal

tolerance.svd <- function(x, nu=min(dim(x)), nv=min(dim(x)), tol = .Machine$double.eps) {
  
  ## the R SVD is much faster/happier when there are more rows than columns in a matrix
  ## however, even though a transpose can speed up the SVD, there is a slow down to then set the U and V back to where it was
  ## so I will remove this for now. I just need to keep it in mind.
  
  # x.dims <- dim(x)
  # x.is.transposed <- F
  # if( (x.dims[1]*10) < x.dims[2]){ # * 10 to make it worth the transpose.
  #   x.is.transposed <- T
  #   x <- t(x)
  # }
  
  ## nu and nv are pass through values.
  svd_res <- svd(x, nu = nu, nv = nv)
  
  # if tolerance is any of these values, just do nothing; send back the SVD results as is.
  if( (is.null(tol) | is.infinite(tol) | is.na(tol) | is.nan(tol) | tol < 0) ){
    
    return(svd_res)
    
  }
  ## once you go past this point you *want* the tolerance features.
  
  
  if(any(unlist(lapply(svd_res$d,is.complex)))){
    stop("tolerance.svd: Singular values ($d) are complex.")
  }
  # if( (any(abs(svd_res$d) > tol) ) & (any(sign(svd_res$d) != 1)) ){
  if( any( (svd_res$d^2 > tol) & (sign(svd_res$d)==-1) ) ){
    stop("tolerance.svd: Singular values ($d) are negative with a magnitude above 'tol'.")
  }
  
  svs.to.keep <- which(!(svd_res$d^2 < tol))
  if(length(svs.to.keep)==0){
    stop("tolerance.svd: All (squared) singular values were below 'tol'")
  }
  
  svd_res$d <- svd_res$d[svs.to.keep]
  
  ## are these checks necessary? problably...
  if(nu >= length(svs.to.keep)){
    svd_res$u <- as.matrix(svd_res$u[,svs.to.keep])
  }else{
    svd_res$u <- as.matrix(svd_res$u[,1:nu])
  }
  
  if(nv >= length(svs.to.keep)){
    svd_res$v <- as.matrix(svd_res$v[,svs.to.keep])
  }else{
    svd_res$v <- as.matrix(svd_res$v[,1:nv])
  }
  
  rownames(svd_res$u) <- rownames(x)
  rownames(svd_res$v) <- colnames(x)
  
  ## force consistent directions as best as possible:
  if( sign(svd_res$u[1]) == -1 ){
    svd_res$u <- svd_res$u * -1
    svd_res$v <- svd_res$v * -1
  }
  
  class(svd_res) <- c("list", "GSVD", "svd")
  return(svd_res)
}

#' is.diagonal.matrix, authored by Derek Beaton. Cf. package GSVD on GitHub, soon available on the CRAN
#'
#' @keywords internal

is.diagonal.matrix <- function(x,tol=.Machine$double.eps){
  if(is.null(dim(x))){
    stop("is.diagonal.matrix: X is not a matrix.")
  }
  x[ x^2 < tol ] <- 0
  return(all(x[lower.tri(x)] == 0, x[upper.tri(x)] == 0))
}

#' power, authored by Derek Beaton. Cf. package GSVD on GitHub, soon available on the CRAN
#'
#' @keywords internal

`%^%` <- function(x,power){
  matrix.exponent(x,power=power)
}

#' is.empty.matrix, authored by Derek Beaton. Cf. package GSVD on GitHub, soon available on the CRAN
#'
#' @keywords internal
is.empty.matrix <- function(x,tol=.Machine$double.eps){
  
  x <- as.matrix(x)
  x[abs(x) < tol] <- 0
  
  if(sum(abs(x))==0){
    return(TRUE)
  }else{
    return(FALSE)
  }
  
}

#' is.identical.matrix, authored by Derek Beaton. Cf. package GSVD on GitHub, soon available on the CRAN
#'
#' @keywords internal

is.identical.matrix <- function(x,tol=.Machine$double.eps, round.digits = 12){
  
  x <- as.matrix(x)
  x[abs(x) < tol] <- 0
  x <- round(x,digits=round.digits)
  
  if(length(unique(c(x)))==1){
    return(TRUE)
  }else{
    return(FALSE)
  }
  
}


#' is.identity.matrix, authored by Derek Beaton. 
#' Cf. package GSVD on GitHub, soon available on the CRAN
#'
#' @keywords internal
is.identity.matrix <- function(x,tol=.Machine$double.eps){
  
  if(is.null(dim(x))){
    stop("is.identity.matrix: x is not a matrix.")
  }
  
  x <- as.matrix(x)
  x[abs(x) < tol] <- 0
  
  if(is.diagonal.matrix(x)){
    if( all(diag(x)==1) ){
      return(TRUE)
    }else{
      return(FALSE)
    }
  }else{
    return(FALSE)
  }
  
}

