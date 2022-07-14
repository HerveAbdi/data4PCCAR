# Utilities for GSVD. All from Derek Beaton
# Github DerekBeaton
# Will be soon moved to CRAN.
# Put together on October 31, 2019. by Vincent Guillemot and Hervé
# Last Modification. Hervé 11/03/2019.

#' GSVD, authored by Derek Beaton. Cf. package GSVD on GitHub, soon available on the CRAN
#'
#' @keywords internal

gsvd <- function(DAT,   # Data
                 LW,    # Left weights
                 RW,    # Right weights
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
  
  ### These are here out of convenience for the tests below. 
  ### They started to get too long.
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
  
  # check if LW and RW are missing, if they are vectors, 
  # or if they are diagonal matrices.
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
  
  ## stolen from MASS::ginv()
  if (length(dim(x)) > 2L || !(is.numeric(x) || is.complex(x)))
    stop("matrix.exponent: 'x' must be a numeric or complex matrix")
  if (!is.matrix(x))
    x <- as.matrix(x)
  
  k <- round(k)
  if(k <= 0){
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
  
  ## are these checks necessary? probably...
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

#' epGPCA2, authored by Derek Beaton. 
#' Cf. package GSVD on GitHub, soon available on the CRAN
#'
#' @keywords internal
epGPCA2 <- function (DATA, scale = TRUE, center = TRUE, DESIGN = NULL, 
                     make_design_nominal = TRUE, 
                     masses = NULL, weights = NULL, graphs = TRUE, k = 0) {
  main <- deparse(substitute(DATA))
  DESIGN <- designCheck(DATA, DESIGN, make_design_nominal)
  DATA <- as.matrix(DATA)
  DATA <- expo.scale(DATA, scale = scale, center = center)
  this.center <- attributes(DATA)$`scaled:center`
  this.scale <- attributes(DATA)$`scaled:scale`
  MW <- computeMW(DATA, masses = masses, weights = weights)
  #print('In epGPCA. MS')
  #print(MW$M)
  #print(MW$W)
  #print('In epGPCA. masses & weights')
  #print(masses)
  #print(weights)
  #res <- corePCA(DATA, M = MW$M, W = MW$W, k = k)
  #MW <- computeMW(DATA, masses = masses, weights = weights)
  res <- corePCA2(DATA, M = masses, W = weights, k = k)
  res$center <- this.center
  res$scale <- this.scale
  class(res) <- c("epGPCA", "list")
  epPlotInfo <- epGraphs(res = res, DESIGN = DESIGN, main = main, 
                         graphs = graphs)
  return(epOutputHandler(res = res, epPlotInfo = epPlotInfo))
}

#' corePCA2, authored by Derek Beaton. 
#' Cf. package GSVD on GitHub, soon available on the CRAN
#'
#' @keywords internal
corePCA2 <- function (DATA, M = NULL, W = NULL, 
                      decomp.approach = "svd", 
                      k = 0)  {
  DATA_dims <- dim(DATA)
  # if (is.null(M)) {
  #   M <- rep(1, nrow(DATA))
  # }
  # if (is.null(W)) {
  #   W <- rep(1, ncol(DATA))
  # }
  # if ((!is.null(dim(M))) && (length(M) == (nrow(M) * ncol(M)))) {
  #   M <- diag(M)
  # }
  # if ((!is.null(dim(W))) && (length(W) == (nrow(W) * ncol(W)))) {
  #   W <- diag(W)
  # }
  #print('In corePCA')
  #print(M)
  #print(W)
  pdq_results <- genPDQ2(datain = DATA, M = M, W = W, is.mds = FALSE, 
                        decomp.approach = decomp.approach, k = k)
  #print('in corePCA')
  #print(pdq_results$Dv)
  #print(pdq_results$p)
  
  if (is.diagMat(M)){
    fi <- matrix(M, nrow(pdq_results$p), ncol(pdq_results$p)) * 
      pdq_results$p * matrix(pdq_results$Dv, nrow(pdq_results$p), 
                             ncol(pdq_results$p), byrow = TRUE)
    ci <- matrix(1/M, nrow(fi), ncol(fi), byrow = FALSE) *
      (fi^2)/matrix(pdq_results$Dv^2,
                    nrow(fi), ncol(fi), byrow = TRUE)
  } else {
    fi <- M %*%  pdq_results$p *
      matrix(pdq_results$Dv, nrow(pdq_results$p), 
             ncol(pdq_results$p), byrow = TRUE)
    ci <- matrix.exponent(M, power = -1) %*% 
      (fi^2)/matrix(pdq_results$Dv^2, 
                    nrow(fi), ncol(fi), byrow = TRUE)
  }
  rownames(fi) <- rownames(DATA)
  di <- rowSums(fi^2)
  ri <- matrix(1/di, nrow(fi), ncol(fi)) * (fi^2)
  ri <- replace(ri, is.nan(ri), 0)
  ci <- replace(ci, is.nan(ci), 0)
  di <- as.matrix(di)
  if (is.diagMat(W)){
    fj <- matrix(W, nrow(pdq_results$q), ncol(pdq_results$q)) * 
      pdq_results$q * matrix(pdq_results$Dv, nrow(pdq_results$q), 
                             ncol(pdq_results$q), byrow = TRUE)
    cj <- matrix(1/W, nrow(pdq_results$q), ncol(pdq_results$q), 
                 byrow = FALSE) * (fj^2)/matrix(pdq_results$Dv^2, nrow(fj), 
                                                ncol(fj), byrow = TRUE)
  } else {
    fj <- W %*% pdq_results$q * 
      matrix(pdq_results$Dv, nrow(pdq_results$q), 
             ncol(pdq_results$q), byrow = TRUE)
    cj <- matrix.exponent(W, power = -1) %*% 
      (fj^2)/matrix(pdq_results$Dv^2, nrow(fj), 
                    ncol(fj), byrow = TRUE)         
  }
  rownames(fj) <- colnames(DATA)
  dj <- rowSums(fj^2)
  rj <- matrix(1/dj, nrow(fj), ncol(fj)) * (fj^2)
  rj <- replace(rj, is.nan(rj), 0)
  cj <- replace(cj, is.nan(cj), 0)
  dj <- as.matrix(dj)
  #print("Checking the values for tau in corePCA")
  res <- list(fi = fi, di = di, ci = ci, ri = ri, fj = fj, 
              cj = cj, rj = rj, dj = dj, 
              t = pdq_results$tau, 
              eigs = pdq_results$eigs, 
              pdq = pdq_results, 
              X = DATA, M = M, W = W)
}

#' epOutputHandler, authored by Derek Beaton. 
#' Cf. package GSVD on GitHub, soon available on the CRAN
#'
#' @keywords internal
epOutputHandler <- function (res = NULL, epPlotInfo = NULL) 
{
  if (!is.null(res) && !is.null(epPlotInfo)) {
    final.output <- list(ExPosition.Data = res, Plotting.Data = epPlotInfo)
    class(final.output) <- c("expoOutput", "list")
    return(final.output)
  }
  else {
    print("Unknown inputs. epOutputHandler must exit.")
    return(0)
  }
  print("It is unknown how this was executed. epOutputHandler must exit.")
  return(0)
}


#' genPDQ2, authored by Derek Beaton. 
#' Cf. package GSVD on GitHub, soon available on the CRAN
#'
#' @keywords internal
genPDQ2 <- function (datain, M = NULL, W = NULL, is.mds = FALSE, decomp.approach = "svd", 
                     k = 0) {
  na.check <- is.na(datain)
  nan.check <- is.nan(datain)
  inf.check <- is.infinite(datain)
  if (sum(na.check | nan.check | inf.check) > 0) {
    stop("ExPosition must stop. There are NA, NaN, or Infinite values.")
  }
  if (is.null(M)) {
    M <- rep(1, nrow(datain))
  }
  if (is.null(W)) {
    W <- rep(1, ncol(datain))
  }
  if ((is.null(dim(M)) &&  is.null(dim(W))) && 
      (length(W) >  0 && length(M) > 0)) {
    vectorflag <- TRUE
    M <- sqrt(M)
    W <- sqrt(W)
  }
  else if (length(dim(M)) == 2 && length(dim(W)) == 2) {
    vectorflag <- FALSE
    M <-  sqrt.mat(M)
    W <-  sqrt.mat(W)
  } else {
    stop("There is an error in the formatting of your masses or weights")
  }
  if (vectorflag) {
    datain <- matrix(M, length(M), dim(datain)[2], byrow = FALSE) * 
      datain * matrix(W, dim(datain)[1], length(W), byrow = TRUE)
  }
  else {
    datain <- M %*% datain %*% W
  }
  svdOUT <- pickSVD(datain, is.mds = is.mds, decomp.approach = decomp.approach,
                    k = k)
  # svdOUT <- svd(datain)
  P <- svdOUT$u
  d <- as.vector(svdOUT$d)
  D <- diag(d)
  Q <- svdOUT$v
  tau <- svdOUT$tau
  eigs <- svdOUT$eigs
  rank <- length(eigs)
  if (vectorflag) {
    P <- matrix(1/M, dim(P)[1], dim(P)[2], byrow = FALSE) * 
      P
    Q <- matrix(1/W, dim(Q)[1], dim(Q)[2], byrow = FALSE) * 
      Q
  }
  else {
    if (!is.diagMat(P)) {
      P <- solve(M) %*% P
    } else {
      P <- (1/M) %*% P
    }
    if (!is.diagMat(P)) {
      Q <- solve(W) %*% Q
    } else {
      Q <- (1/W) %*% Q
    }
  }
  res <- list(p = P, q = Q, Dv = d, Dd = D, ng = length(d), 
              rank = rank, tau = tau, eigs = eigs)
  class(res) <- c("epSVD", "list")
  return(res)
}


#' sqrt.mat, authored by Derek Beaton. 
#' Cf. ExPosition CRAN
#'
#' @keywords internal
# sqrt.mat from ExPosition 
sqrt.mat <- function (X) 
{
  if (is.null(dim(X))) stop("X should be a matrix")
  if (!isSymmetric.matrix(X)) {
    stop("Weight/Mass matrix is not symmetric")
  }
  if (isDiagonal.matrix(X)) {
    # return(sqrt(diag(X))) # This did not work
    return(diag(sqrt(diag(X))))
  }
  else {
    A <- eigen(X)
    A$values[which(A$values < .Machine$double.eps)] <- 0
    if (sum(A$values < 0) > 0) {
      stop("Weight/Mass matrix not positive definite. Some eigenvalues are less than 0")
    }
    else {
      return(A$vectors %*% diag(sqrt(A$values)) %*% t(A$vectors))
    }
  }
}

#' sqrt.mat, authored by Derek Beaton. 
#' Cf. ExPosition CRAN
#'
#' @keywords internal
isDiagonal.matrix <-function (X) 
{
  if (is.null(dim(X))) {
    stop("X is not a matrix.")
  }
  return(all(X[lower.tri(X)] == 0, X[upper.tri(X)] == 0))
}