# Functions in this file:
# getRandomFS [includes insieder function rdiag]
# recode2Integer
# buildRandomImage4PCA
#_____________________________________________________________________
# A set of functions to create PCA-based multidimensional samples
#   matching a given factorial structure (from the loadings)
# Main idea: Create (random) simulated data
# whose loadings will match the loadings
# of a given matrix recording likert scales
# and have the same distribution of scores
# Herve Abdi. October 10, 2018.
#-


#_____________________________________________________________________
# getRansomFS: Preamble ----
# generate random observations
# according to a multidimensional space
# default is uniform distribution
#_____________________________________________________________________
# Helper for roxygen2 ----
#  install.packages('sinew')
#  sinew::makeOxygen(getRandomFS)
#
#_____________________________________________________________________
#' @title generate multivariate random observations (i.e.,
#' factor scores)
#' that match the variance/eigenvalues of the dimensions.
#'
#' @description \code{getRandomFS}:
#' generate random observations
#' such as factors scores from a PCA wjose variance
#' match the variance of the dimensions.
#' .
#'
#' @details
#' \code{getRandomFS}: Simulates a set
#' multivariate coordinates of random observations
#' whose variance per dimension
#' (i.e.,
#' eigenvalues in PCA) will match a given set of variances.
#' The function generating the set of number ca be any such function
#' (default is \code{runif}).
#'
#' @param ev The variance / eigenvalues per dimension (no default).
#' @param nObs the number of observationz to generate, Default: 100
#' @param FUN  the random number generating.
#' function. Default: \code{runif} (uniform distribution).
#' @param center center the numbers.  Default: \code{FALSE}.
#' @param scale normalization per dimension prior to
#' re-nromalize the factor scores with \code{ev}, best to
#' stick with the default.
#' if \code{FALSE} do not normalize
#' the numbers, if \code{TRUE} normalize the numbers to Z-scores
#' per dimension, if \code{'SS1'} (default) normalize to one
#' @param sv  if \code{TRUE}, \code{ev} gives singular values or
#' standard deviation, if
#'   \code{FALSE} (default)
#'   \code{ev} gives eigenvalues or
#' variance.
#' @return A matrix
#' of multidimensional factor scores
#' with nObs rows (observations) and number of columns
#' (variables / components)
#' equal to
#'  the length of \code{ev}.
#'
#' @author Hervé Abdi
#' @examples
#' # generates 10 factors scores from a 4-dimensional space
#' # whose dimensions have variances of 16, 9, 4, and 1
#' randFS <- getRandomFS(ev = c(16, 9, 4, 1), nObs = 10)
#' @seealso
#'  \code{\link[data4PCCAR]{scale0}}
#'  @importFrom stats runif
#' @rdname getRandomFS
#' @export
getRandomFS <- function(ev,
                        nObs = 100,
                        FUN = stats::runif,
                        center = FALSE,
                        scale  = 'SS1',
                        sv = FALSE){
  # Insider function rdiag
  rdiag <- function (X, y)
  {
    nC <- length(y)
    nR <- nrow(X)
    return(X * matrix(y, nrow = nR, ncol = nC, byrow = TRUE))
  }
  if (!(sv)) {sv <- sqrt(ev)} else {sv <- ev}
  nDim <- length(sv)
  randomObs.preP <-  t(replicate(nObs,  FUN(nDim)))
  P <-  scale0(randomObs.preP,
                          center = center, scale = scale)
  randomObs <- rdiag(P, sv)
  colnames(randomObs) <- paste0("Dimension ", 1:nDim)
  rownames(randomObs) <- paste0("Obs_", 1:nObs)
  return(randomObs)
} # End of getRandomFS ----
#_____________________________________________________________________
#_____________________________________________________________________
#  sinew::makeOxygen(recode2Integer)
#_____________________________________________________________________
# recode2Integer preamble ----
#' @title Recode a vector of real values to integers amtching
#' a given distribution for the integers
#' @description \code{recode2Integer}:
#'  Recodes a vector of real values to integers amtching
#' a given distribution for the integers
#' @param original.var A vector of integers
#' (e.g., Likert scale), used to dereive the distribution of the
#' integers.
#' @param toBeScaled.var a vector of real values to be recoded
#' as integers (with the same distribution as \code{original.var}).
#' @return A vector of integers matching the distribution
#' of the integers in \code{original.var}.
#' @details \code{recode2Integer}: can be used to
#' transform a vector or real numbers to a vector of integers
#' such as, for example, the score obtained from a Likert scale.
#' Note that the more elements the original vector has, the better
#' the quality of the transformation, because ties can create problem
#' wih small samples.
#' @author Hervé Abdi
#' @examples
#' set.seed(42)
#' toto <- rnorm(1:10)
#' toto.scaled <- recode2Integer(c(1,1,1,3,4,4,1,2,2,4), toto)
#' @rdname recode2Integer
#' @export
recode2Integer <- function(original.var, toBeScaled.var){
  nOri <- length(original.var)
  nRec <- length(toBeScaled.var)
  var.ordered <- sort(toBeScaled.var)
  # force original.var to be integers
  original.var <-  round(original.var)
  # get the distribution
  nPerBin.ori <-  summary(as.factor(original.var))
  nPerBin <- round(nPerBin.ori * (nRec / nOri) )
  # to handle unequal number of observations
  # between original.var and scaled.var
  toto <- cumsum(nPerBin)
  toto[length(toto)] <- nRec # max sure that n are correct
  cuts = c(min(var.ordered) - 1, var.ordered[toto])
  nBins <- length(toto)
  rec.var <- rep(NA, length(toBeScaled.var))
  for (i in 1:nBins){
    indices <-  ((toBeScaled.var >  cuts[i]) &
                   (toBeScaled.var <= cuts[i + 1]) )
    rec.var[indices] <- i
  } # end loop in i
  return(rec.var)
} # end recode2Integer ----
#_____________________________________________________________________
#

#_____________________________________________________________________
#  sinew::makeOxygen(buildRandomImage4PCA)
#_____________________________________________________________________
# buildRandomImage4PCA: Preamble ----
#' @title Make a random image (with similar PCA loadings) of a
#' matrix of integers data.
#'
#' @description \code{buildRandomImage4PCA}:
#' Makes a random image (with similar PCA loadings) of a
#' matrix of integer data such as Likert scales data..
#' @param X The original data
#' @param nObs  how many observations for the random data
#'  (Default: \code{NROW(X)}.
#' @param center if \code{TRUE} (Default) perform a
#' centered PCA of \code{X}.
#' @param scale if \code{FALSE}
#' (Default)  do not scale the data for the PCA of \code{X}.
#' Other options are \code{TRUE} (use \eqn{Z}-scores), \code{'SS1'},
#' normalize each variable to length 1.
#' @return A matrix of (somewhat) random integers with
#' \code{nObs} observations whose
#' factorial structure roughly matches the original matrix.
#' @details The procedure starts with a PCA of the original matrix
#' \code{X} and gets loadings and factor scores.
#' From the eigenvalues,
#'  \code{buildRandomImage4PCA} generates random observations in
#'  the factor scores spaces. These observations are then used
#'  to recreate (via the \emph{reconstitution formula})
#'  a new matrix \code{X.rand} that will store the observations
#'  that will recreatae the random factor scores. The
#'  observation in \code{X.rand} are then re-scaled
#'  to match the distribution of the integers in the original matrix.
#'  Doing so gives a matrix of scores with the same basic statistics
#'  (i.e., mean and standard deviation) per column whose PCA will give
#'  roughly the same loadings (not the same factor score of course)
#'  as the original matrix.
#' @author Hervé Abdi
#' @examples
#' library(ExPosition)
#' data("twentyWines")
#' test4build <- buildRandomImage4PCA(twentyWines$df.active)
#' @seealso
#'  \code{\link[ExPosition]{epPCA}}
#' @rdname buildRandomImage4PCA
#' @import ExPosition
#' @export
# Build the random matrix
buildRandomImage4PCA <-  function(X, nObs = NROW(X),
                                  center = TRUE,
                                  scale = FALSE){
  nJ <- NCOL(X) # How many columns
  resPCA <- epPCA(X, center = center,
                  scale = scale, graphs = FALSE)
  Q    <- resPCA$ExPosition.Data$pdq$q
  eigs <- resPCA$ExPosition.Data$eigs
  test.rand <- getRandomFS(ev = eigs, nObs = nObs)
  X.rand <-  test.rand %*% t(Q)
  colnames(X.rand) <- rownames(Q)
  X.rand.round <- as.data.frame(matrix(NA, nrow = nObs,
                                       ncol = nJ,
                                       dimnames = dimnames(X.rand) ) )

  for (j in 1:nJ){ # a Horrible loop!
    X.rand.round[,j] <- recode2Integer(original.var = X[,j],
                                       toBeScaled.var = X.rand[,j] )
  }
  return( X.rand.round)
} # end of buildRandomImage4PCA
#_____________________________________________________________________
# End of file.
#
