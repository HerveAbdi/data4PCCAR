# perm4CCA ----
#_____________________________________________________________________
# Preamble perm4CCA ----
#_____________________________________________________________________
#' @title Permutation for canonical correlation analysis (CCA);
#'as implemented
#' in \code{tepCCA}) (a \code{TExPosition} compatible version).
#' 
##' @description \code{perm4CCA}: 
##' Permutation for CCA (as implemented
#' in \code{tepCCA}).
#' Computes an omnibus permutation test and
#' specific tests for the eigenvalues when
#' performing a CCA from
#' 2 matrices \eqn{X} and \eqn{Y}.
#' Several possible
#' combinations of centering and normalizing
#' are possible (see paramater \code{scale1,
#' scale2, center2, scale2}).
#' Used for functions related to CCA.
#' The different types of normalization are
#' based on the \code{ExPosition::expo.scale}
#' function. Two different permutation schemes
#' are currently available (see parameter
#' \code{permType}).
#' @param DATA1 an \eqn{N*I} matrix of quantitative data
#' @param DATA2 an \eqn{N*J} matrix of quantitative data
#' @param center1 when \code{TRUE} (default) \code{DATA1}
#' will be centered
#' @param center2 when \code{TRUE} (default) \code{DATA2}
#' will be centered
#' @param scale1 when \code{TRUE} (default) \code{DATA1}
#' will be normalized. Depends upon \code{ExPosition}
#' function \code{expo.scale} whose description is:
#'boolean, text, or (numeric) vector.
#'If boolean or vector,
#'it works just as scale.
#'The following text options are available:
#' \code{'z'}: z-score normalization,
#' \code{'sd'}: standard deviation normalization,
#' \code{'rms'}: root mean square normalization,
#'  \code{'ss1'}: sum of squares
#'  (of columns) equals 1
#'  (i.e., column vector of length of 1).
#' @param scale2 when \code{TRUE} (default) \code{DATA2}
#' will be normalized
#'  (same options as for \code{scale1}).
#' @param nIter (Default = \code{1000}). Number of Iterations
#' (i.e., number of permuted samples computed).
#' @param permType what type of permutation is used
#' if \code{'byMat'} (default) only the labels of the observations
#' are permuted, other option is \code{'byColumns'} then
#' all columns of each matrix are independently
#' permuted.
#' @param compact if \code{TRUE}
#' (Default) return only \eqn{p}-values for omnibus test.
#' @return a list with
#' \code{fixedInertia}: the inertia of
#' \eqn{X'Y} data matrix (i.e., sums of squares)
#' \code{fixedEigenvalues}: the  eigenvalues of
#' the \eqn{X'Y}  matrix;
#' \code{pOmnibus}: the probability associated
#' to the inertia.
#' If \code{compact} is \code{FALSE}, return also
#' \code{permInertia}:
#' an \code{nIter} * 1 vector containing the
#' permuted inertia;
#' \code{pEigenvalues}: The probabilites
#' associated to each eigenvalue;
#' If \code{compact} is \code{FALSE}, returns also
#' \code{permEigenvalues}: an
#' \code{nIter} * \code{L} matrix giving
#' the permuted eigenvalues.
#' @author Vincent Guillemot & Hervé Abdi
#' @rdname perm4CCA
#' @seealso  compS
#' @export
#'
#' @examples
#' \dontrun{ # Some examples here sometimes ****}
#' 
perm4CCA <- function (DATA1, DATA2, center1 = TRUE, 
                      center2 = TRUE, scale1 = "ss1", 
          scale2 = "ss1", nIter = 1000, 
          permType = "byMat", compact = FALSE) 
{
  if (permType != "byColumns") 
    permType <- "byMat"
  DATA1 <- as.matrix(DATA1)
  DATA2 <- as.matrix(DATA2)
  X = DATA1
  Y = DATA2
  if (NCOL(X) > NCOL(Y)) {
    X = DATA2
    Y = DATA1
  }
  nN <- NROW(X)
  nI <- NCOL(X)
  nJ <- NCOL(Y)
  if (!(nN == NROW(Y))) {
    stop("DATA1 and DATA2 non-conformable")
  }
  maxRank <- min(nI, nJ)
  Sfixed = compS(DATA1, DATA2, center1 = center1, center2 = center2, 
                 scale1 = scale1, scale2 = scale2)
  fixedEigenvalues <- rep(0, maxRank)
  fixedEV <- sv2(Sfixed)
  if (length(fixedEV) > maxRank) {
    fixedEigenvalues <- fixedEV[1:maxRank]
  }
  if (length(fixedEV) == maxRank) {
    fixedEigenvalues <- fixedEV
  }
  if (length(fixedEV) < maxRank) {
    fixedEigenvalues[1:length(fixedEV)] <- fixedEV
  }
  fixedInertia <- sum(fixedEigenvalues)
  permInertia <- rep(NA, nIter)
  permEigenvalues <- matrix(NA, nrow = nIter, ncol = maxRank)
  .truc <- function(X, Y, longueur = min(c(dim(X), NCOL(Y))), 
                    permType = permType) {
    valP <- rep(0, longueur)
    if (permType == "byMat") {
      Xrand <- X[sample(nN), ]
      Yrand <- Y
    }
    if (permType == "byColumns") {
      Xrand <- apply(X, 2, sample)
      Yrand <- apply(Y, 2, sample)
    }
    Srand <- compS(Xrand, Yrand)
    resvp <- sv2(Srand)
    valP[1:length(resvp)] <- resvp
    return(valP)
  }
  laLongueur <- maxRank + 1
  permEigenvalues <- replicate(nIter, .truc(X, Y, laLongueur, 
                                            permType))
  permEigenvalues <- t(permEigenvalues[1:maxRank, ])
  permInertia = rowSums(permEigenvalues)
  pOmnibus = sum(permInertia > fixedInertia)/nIter
  if (pOmnibus == 0) 
    pOmnibus <- 1/nIter
  pEigenvalues <- rowSums(t(permEigenvalues) > (fixedEigenvalues))/nIter
  pEigenvalues[pEigenvalues == 0] <- 1/nIter
  return.list <- structure(list(fixedInertia     = fixedInertia, 
                                fixedEigenvalues = fixedEigenvalues, 
                                pOmnibus         = pOmnibus, 
                                pEigenvalues     = pEigenvalues), 
                           class = "perm4PLSC")
  if (!compact) {
    return.list$permInertia     = permInertia
    return.list$permEigenvalues = permEigenvalues
  }
  return(return.list)
} # End of function
#_____________________________________________________________________
