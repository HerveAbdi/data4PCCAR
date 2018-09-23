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
#' any of the random nymber generators in \code{R}.
#' (of course the default is a standard normal distribution).
#' Note that the specific parameters for the random gnerator
#' need to be passed as additional arguments to the function
#' (i.e., with the  ... procedure).
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
#' @param scale the type of scaling of the
#' data. Can be \code{FALSE} (no scaling),
#' \code{TRUE} (scale as \eqn{Z}-score),
#' \code{'NONE'} (no scaling), or \code{'SS1'} (all columns
#' of the data matrix have norm 1, and so the eigen-values
#' come from a correlation matrix).
#' Default: \code{'SS1'}.
#' @param FUN the function Default: rnorm (normal distribution).
#' Could be any of the functions provided by \code{R}, such as
#' \code{rexp, rlogis,}, etc. Most of these functions require
#' additional parameters to be passed via ... (see below).
#' @param ... Stuff (i.e., parameters)
#' to pass the the \code{FUN} if needed
#' (e.g., mean and standard deviation). To find these parameters,
#'  check the help of the function used for the random number
#'  generator.
#' @rdname monteCarlo.eigen
#' @importFrom stats rnorm
#' @return a \code{nIter} by \code{min(dim(X))}
#' (i.e., full rank of \code{X}) matrix with the random
#' eigen-values
#' @examples
#' \dontrun{
#' data(iris)
#' random.eigen <- monteCarlo.eigen(iris[,1:4])
#' }
#' @seealso
#'  \code{\link[stats]{rnorm}}
#' @rdname monteCarlo.eigen
#' @export
#'
monteCarlo.eigen <- function(X, nIter = 100,
                             scale = 'SS1',
                             FUN = rnorm,
                             ...){
leRang    <- min(dim(X)) # max Rank of X
randEigen <- matrix(0, nrow = nIter, ncol = leRang)
# laFUN = function(n, FUN2 = FUN, ...){return(y = FUN2(n,...))}
nI <- NROW(X)
nJ <- NCOL(X)
nIJ <- nI*nJ
# get the normalization options
if (is.logical(scale)){
  if (!scale) {scale <- 'NONE'} else {scale <- 'Z'}} else {
    scale <- 'SS1'}
# normalizing to 1
if(scale == 'SS1') normFact = sqrt(nI - 1)
# The i-loop
for (i in 1:nIter){ # begin loop in i
  # Xrand    <- matrix(laFUN(nIJ), nrow = nI)
  if (scale == 'NONE'){Xrand.sc <-  matrix(FUN(nIJ,...),nrow = nI)}
  if (scale == 'Z'){ Xrand.sc   <- apply(matrix(FUN(nIJ,...), nrow = nI) , 2, base::scale) }
  if (scale == 'SS1'){
    Xrand.sc <- apply(matrix(FUN(nIJ,...), nrow = nI) , 2,
                      base::scale) / normFact}
  eigRand <- eigen(t(Xrand.sc) %*% Xrand.sc,
                   symmetric = TRUE, only.values = TRUE)$values
  randEigen[i,1:length(eigRand) ] <- eigRand
                  } # End loop in i
  return(randEigen)
}
