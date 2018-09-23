#
#
#
#_____________________________________________________________________
# function monteCarlo.eigen
# to create eigenvalue from cross-product matrix
# generated from random data matrix
#
#____________________________________________________________________
# Preamble monteCarlo.eigen ----
#'
#' @title Monte Carlo generation of (random) eigenvalues to match
#' a data matrix.
#'
#' @description \code{monteCarlo.eigen}:
#' generates
#' Monte Carlo random eigenvalues to match
#' a data given matrix. The random numbers can be generated from
#' any of the random number generators in \code{R}.
#' (of course, the default is a standard normal distribution).
#' Note that the specific parameters for the random gnerator
#' need to be passed as additional arguments to the function
#' (i.e., with the  "..." procedure).
#'
#' @details \code{monteCarlo.eigen} can be used
#' to implement a \emph{parallel} test for the number
#' of reliable components. Note that the \emph{parallel} test
#' becomes equivalent to the Kaiser test (i.e., eigenvalues
#' larger than the average inertia) when the number of rows
#' of the data matrix is large enough.
#'
#' @param X The data matrix to match.
#' @param nIter how many random set of eigenvalues
#' to generate; Default: \code{100}.
#' @param center (Default = \code{TRUE}) if
#' \code{TRUE}: center the data (by columns).
#' @param scale the type of scaling of the
#' data. Can be \code{FALSE} (no scaling),
#' \code{TRUE} (scale as \eqn{Z}-score),
#' \code{'NONE'} (no scaling), or \code{'SS1'} (all columns
#' of the data matrix have norm 1, and so the eigen-values
#' come from a correlation matrix).
#' Default: \code{'SS1'}.
#' @param FUN the function to generate
#' random numbers; Default: \code{rnorm} (normal distribution).
#' Could be any of the functions provided by \code{R}, such as
#' \code{rexp, rlogis,}, etc. Most of these functions require
#' additional parameters to be passed via ... (see below).
#' @param ... Stuff (i.e., parameters)
#' to pass the \code{FUN} if needed
#' (e.g., mean and standard deviation). To find these parameters,
#'  check the help of the function used for the random number
#'  generator.
#' @rdname monteCarlo.eigen
#' @importFrom stats rnorm
#' @return a list with 3 elements
#' \enumerate{
#' \item \code{$fixed.eigs}: the eigen-values of \eqn{X},
#' \item \code{$rand.eigs}: an \code{nIter} by rank(\eqn{X}) matrix
#' of the eigenvalues of the bootstrapped samples,
#' \item \code{$rand.eigs.sorted}:
#' an \code{nIter} by rank(\eqn{X}) matrix
#' of the eigenvalues of the bootstrapped samples.
#' }
#' @examples
#' data(iris)
#' random.eigen <- monteCarlo.eigen(iris[,1:4], nIter = 10)
#' @seealso
#'  \code{\link[stats]{rnorm}} \code{\link{scale0}}
#'  \code{\link{boot.eigen}}
#' @rdname monteCarlo.eigen
#' @export
#'
#____________________________________________________________________
monteCarlo.eigen <- function(X, nIter = 100,
                             center = TRUE,
                             scale = 'SS1',
                             FUN = rnorm,
                             ...){
  # Fixed effect first
  Xcent <- apply(X, 2, scale0, center, scale)
  fixed.eigs <- eigen(t(Xcent) %*% Xcent, symmetric = TRUE,
                      only.values = TRUE)$values
  #
  leRang    <- min(dim(X)) # max Rank of X
  randEigen <- matrix(0, nrow = nIter, ncol = leRang)
  nI <- NROW(X)
  nJ <- NCOL(X)
  nIJ <- nI*nJ
  # The i-loop
  for (i in 1:nIter){ # begin loop in i
    Xrand <-  apply(matrix(FUN(nIJ), nrow = nI),
                    2, scale0, center, scale)
    eigRand <- eigen(t(Xrand) %*% Xrand,
                     symmetric = TRUE, only.values = TRUE)$values
    randEigen[i,1:length(eigRand)] <- eigRand
  } # End loop in i
  return.list <- structure(list(fixed.eigs = fixed.eigs,
                                rand.eigs  = randEigen,
                                rand.eigs.sorted = apply(randEigen, 2, sort)),
                           class = "randEigen")
  return(return.list)
} # end of monteCarlo
#********************************************************************
#********************************************************************
# print.randEigen ----
#' Change the print function for the class \code{randEigen}.
#'
#' \code{randEigen} Change the print function for
#' object of the class \code{randEigen} (ouput of monteCarlo.eigen).
#'
#' @param x a list: output of monteCarlo.eigen (class: randEigen)
#' @param ... everything else for the functions
#' @author Hervé Abdi
#' @export
print.randEigen <- function (x, ...) {
  ndash = 78 # How many dashes for separation lines
  cat(rep("-", ndash), sep = "")
  cat("\n monteCarlo (random) PCA-eigenvalues \n")
  # cat("\n List name: ",deparse(eval(substitute(substitute(x)))),"\n")
  cat(rep("-", ndash), sep = "")
  cat("\n$fixed.eigs       ", "The eigenvalues of the original matrix.")
  cat("\n$rand.eigs        ", "The eigenvalues of the random samples. ")
  cat("\n$rand.eigs.sorted ", "The sorted (by dimension) eigenvalues of")
  cat("\n$                 ", " the random samples (e.g., for confidence intervals).")
  cat("\n",rep("-", ndash), sep = "")
  cat("\n")
  invisible(x)
} # end of function print.bootEigen
#_____________________________________________________________________


#_____________________________________________________________________
# Preamble boot.eigen
#
#'
#' @title generate a bootstrap distribution of eigenvalues
#' for a given matrix
#'
#' @description \code{boot.eigen}:
#' generates
#'  a bootstrap distribution of eigenvalues
#' for a given (rectangular) matrix.
#' @details
#' \code{boot.eigen} uses \code{scale0} to
#' normalize the data and so will not create \code{NaN}
#' when the variance of a column is 0 (it will create
#' a vector of 0s).
#' @param X The original data matrix.
#' @param nIter how many bootstrapped sets of eigenvalues
#' to generate; Default: \code{100}.
#' @param center (Default = \code{TRUE}) if
#' \code{TRUE}: center the data (by column).
#' @param scale the type of scaling of the columns of
#' data. Can be \code{FALSE} (no scaling),
#' \code{TRUE} (scale as \eqn{Z}-scores
#' with squared norm = \eqn{I - }1),
#'  or \code{'SS1'} (all columns
#' of the data matrix have norm 1, and so the eigenvalues
#' come from a correlation matrix).
#' Default: \code{'SS1'}.
#' @rdname boot.eigen
#' @return a list with 3 elements
#' \enumerate{
#' \item \code{$fixed.eigs}: the eigen-values of \eqn{X},
#' \item \code{$boot.eigs}: an \code{nIter} by rank(\eqn{X}) matrix
#' of the eigenvalues of the bootstrapped samples,
#' \item \code{$boot.eigs.sorted}: an \code{nIter} by rank(\eqn{X}) matrix
#' of the eigenvalues of the bootstrapped samples.
#' }
#' @seealso \code{\link{scale0}} \code{\link{monteCarlo.eigen}}
#' @author Hervé Abdi
#' @examples
#' data(iris)
#' bootstapped.Eigenvalues <- boot.eigen(iris[,1:4], nIter = 10)
#' @rdname boot.eigen
#' @export
#'
boot.eigen <- function(X, nIter = 100, center = TRUE, scale = 'SS1'){
  Xcent <- apply(X, 2, scale0, center, scale)
  fixed.eigs <- eigen(t(Xcent) %*% Xcent, symmetric = TRUE,
                      only.values = TRUE)$values
  leRang    <- min(dim(X)) # max Rank of X
  bootEigen <- matrix(0, nrow = nIter, ncol = leRang)
  nI <- NROW(X)
  nJ <- NCOL(X)
  # The i-loop
  for (i in 1:nIter){ # begin loop in i
    # generate Xboot
    Xboot <- apply(X[sample(nI, replace = TRUE),],
                   2, scale0, center, scale)
    eigBoot <- eigen(t(Xboot) %*% Xboot,
                     symmetric = TRUE, only.values = TRUE)$values
    bootEigen[i,1:length(eigBoot)] <- eigBoot
  } # End loop in i
  return.list <- structure(list(fixed.eigs = fixed.eigs,
                                boot.eigs  = bootEigen,
                                boot.eigs.sorted = apply(bootEigen, 2, sort)),
                           class = "bootEigen")
  return(return.list)
}
#_____________________________________________________________________
#********************************************************************
# print.bootEigen ----
#' Change the print function for the class \code{bootEigen}.
#'
#' \code{print.bootEigen} Change the print function for
#' objects ot the class \code{bootEigen}
#' (output of \code{boot.eigen}).
#'
#' @param x a list: output of boot.eigen (class: bootEigen)
#' @param ... everything else for the functions
#' @author Hervé Abdi
#' @export
print.bootEigen <- function (x, ...) {
  ndash = 78 # How many dashes for separation lines
  cat(rep("-", ndash), sep = "")
  cat("\n Bootstraped PCA-eigenvalues \n")
  # cat("\n List name: ",deparse(eval(substitute(substitute(x)))),"\n")
  cat(rep("-", ndash), sep = "")
  cat("\n$fixed.eigs       ", "The eigenvalues of the original matrix.")
  cat("\n$boot.eigs        ", "The eigenvalues of the bootstrapped samples. ")
  cat("\n$boot.eigs.sorted ", "The sorted (by dimension) eigenvalues of")
  cat("\n$                 ", " the bootstrapped samples (e.g., for confidence intervals).")
  cat("\n",rep("-", ndash), sep = "")
  cat("\n")
  invisible(x)
} # end of function print.bootEigen
#_____________________________________________________________________
#_____________________________________________________________________
# function scale0 ----
# scale can be FALSE, TRUE, or 'SS1' to be compatible with scale
#' @title center and normalize a vector to 1 or to (\eqn{N} - 1)
#' @description center and normalize a vector to 1 or to (\eqn{N} - 1)
#' @param x a vector with \eqn{N} elements.
#' @param center if \code{TRUE} (Default) center if not do nothing.
#' @param scale if \code{FALSE} do nothing,
#' if \code{TRUE} normalize like \eqn{Z}-scores
#' [i.e., norm = \eqn{N} - 1], if \code{'SS1'} (Default) normalise
#' to 1 (i.e., norm = 1).
#' @return a scaled version of \code{x}.
#' @details
#' When \code{x} comprises different values
#' \code{scale0} gives the same results as \code{scale}
#' except for the option \code{'SS1'}; when \code{x}
#' comprises all identical values and when the
#' paramater \code{scale = TRUE}, \code{scale} gives back
#' a vector of \code{NaN} (because of a division by 0 error),
#' whereas \code{scale0} gives back a vector of \code{0}.
#' @author Hervé Abdi
#' @seealso scale
#' @examples
#' y <- scale0(c(1,2,5,10))
#'
#' @rdname scale0
#' @export
scale0 <- function(x, center = TRUE, scale = 'SS1'){
  # canibalized form dplyr
  .near <- function(z, y,
                    tol = .Machine$double.eps^0.5) {abs(z - y) < tol}
  if (!is.logical(center)) center <- TRUE
  if (center) x <- x - mean(x) # center
  if (!is.logical(scale)) scale = 'SS1'
  if ((isTRUE(scale)) | (scale == 'SS1')){
    ss2 = sum(x^2)
    if (isTRUE(scale)){ ss2 <- ss2 / (length(x) - 1) } # variance
    if  (!(.near(ss2,0))) {x <- x / sqrt(ss2)} else {x = rep(0,length(x))}
  } # end long if
  return(x)
} # end of function scale0
#_____________________________________________________________________
#
