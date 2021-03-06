#_____________________________________________________________________
# Bootstrap for CCA  ----
# Last version October 31, 2019. Vincent Guillemot + Hervé Abdi
#_____________________________________________________________________
# Boot4CCA Preamble ----
# function Boot4CCA. Derived from Boot4PLSC
#' @title  Bootstrap for Canonical Correlation Analysis (CCA).
#' @description  \code{Boot4CCA}:
#' Creates "Bootstrap Bricks" and other bootstrap statistics
#' for the \eqn{I} and \eqn{J} sets
#' of a CCA.
#' The bricks are
#' obtained from bootstrapping the rows
#' of the two data-tables used for the  CCA.
#'  \code{Boot4CCA} uses the "transition formula" to get
#' the values of the row and column loadings
#' from multiplication of the latent variables.
#' Gives also the bootstrapped eigenvalues
#' (if \code{eigen = TRUE}).
#' @details
#' \emph{Note}: \code{Boot4CCA} gives the
#' \emph{eigenvalues} of the matrix
#' \eqn{X'Y} even though CCA
#' works with the \emph{singular values}
#' (i.e., the square roots of the eigenvalues) of
#' \eqn{X'Y}.
#'
#' @param DATA1 an \eqn{N*I}  data matrix
#' @param DATA2 an \eqn{N*J}  data matrix
#' (measured on the same observations as \code{DATA2})
#' @param center1 when \code{TRUE} (default) \code{DATA1}
#' will be centered
#' @param center2 when \code{TRUE} (default) \code{DATA2}
#' will be centered
#' @param scale1 when \code{TRUE} (default) \code{DATA1}
#' will be normalized. Depends upon
#' function \code{scale0} whose description is:
#' boolean, text, or (numeric) vector.
#'If boolean or vector,
#'it works just as scale.
#'The following text options are available:
#' \code{'z'}: \eqn{z}-score normalization,
#' \code{'sd'}: standard deviation normalization,
#' \code{'rms'}: root mean square normalization,
#'  \code{'ss1'}: sum of squares
#'  (of columns) equals 1
#'  (i.e., each column vector has length of 1).
#' @param scale2 when \code{TRUE} (default) \code{DATA2}
#' will be normalized
#'  (same options as for \code{scale1}).
#' @param Fi (Default = \code{NULL}), the \eqn{I} 
#' factor scores
#' for the columns of \code{DATA1}.
#' if \code{NULL},  \code{Boot4RowCA} computes them..
#' @param Fj = (Default = \code{NULL}, the \eqn{J} 
#' factor scores
#' for the columns of \code{DATA2}.
#' if \code{NULL} the function
#' \code{Boot4RowCA} computes them.
#' @param nf2keep How many factors to
#' keep for the analysis (Default = \code{3}).
#' @param nIter (Default = \code{1000}). Number of Iterations
#' (i.e., number of Bootstrtap samples).
#' @param critical.value (Default = \code{2}).
#' The critical value for a \code{BR} to be considered
#' significant.
#' @param eig if \code{TRUE} compute bootstraped
#' confidence intervals (CIs) for the eigenvalues
#' (default is \code{FALSE}).
#' @param alphaLevel the alpha level used to compute
#' the confidence intervals for the eigenvalues
#' (with CIS at 1-alpha). Default is \code{.05}
#' @return a list with
#' \itemize{
#' \item{\code{bootstrapBrick.i}: }{the
#'  the \code{I * Dimensions * Iterations} Brick of
#'      Bootstraped factor scores  for the \eqn{I}-set;}
#'  \item{\code{bootRatios.i}: }{the bootstrap ratios
#'      for the \eqn{I}-set;}
#' \item{\code{bootRatiosSignificant.i}: }{the Significant
#'    BRs for the \eqn{I}-set;}
#' \item{\code{bootstrapBrick.j}: }{
#'     the \code{J * Dimensions * Iterations} Brick of
#'     Bootstraped factor scores for the \eqn{J}-set;}
#' \item{\code{bootRatios.j}: }{the bootstrap ratios for the \eqn{J}-set;}
#' \item{\code{bootRatiosSignificant.j}: }{the Significant
#'    BRs for the \eqn{J}-set;}
#'    }
#'    In addition if \code{eig = TRUE}, the list includes:
#'\itemize{
#' \item{\code{eigenValues}: }{the \code{nIter * nL} table
#'  of eigenvalues;}
#'\item{\code{fixedEigenvalues}: }{the eigenvalues of
#'   matrix n\eqn{X'Y}.}
#'  \item{\code{eigenCIs}: }{the CIs for the
#'  eigenvalues.}
#'  }
#' @seealso \code{\link{scale0}}
#' @export
#' @author Vincent Guillemot & Hervé Abdi
#' @rdname BootCCA
#' @examples
#' \dontrun{
#' # Some examples here sometimes in the next future ****}
Boot4CCA <- function (
          DATA1, DATA2, 
          center1 = TRUE,  center2 = TRUE, 
          scale1  = "SS1", scale2  = "SS1", 
          Fi = NULL, Fj = NULL, 
          nf2keep = 3, nIter = 1000, 
          critical.value = 2, eig = FALSE, 
          alphaLevel = 0.05) 
{
  .boot.ratio.test <- function(boot.cube, critical.value = 2) {
    boot.cube.mean <- apply(boot.cube, c(1, 2), mean)
    boot.cube.mean_repeat <- array(boot.cube.mean, 
                                    dim = c(dim(boot.cube)))
    boot.cube.dev <- (boot.cube - boot.cube.mean_repeat)^2
    s.boot <- (apply(boot.cube.dev, c(1, 2), mean))^(1/2)
    boot.ratios <- boot.cube.mean/s.boot
    significant.boot.ratios <- (abs(boot.ratios) > critical.value)
    rownames(boot.ratios) <- rownames(boot.cube)
    rownames(significant.boot.ratios) <- rownames(boot.cube)
    return(list(sig.boot.ratios = significant.boot.ratios, 
                boot.ratios = boot.ratios))
  }
  nN = NROW(DATA1)
  if (nN != NROW(DATA2)) {
    stop("input matrices not conformable")
  }
  X <- apply(DATA1, 2, scale0, center = center1, scale = scale1)
  Y <- apply(DATA2, 2, scale0, center = center2, scale = scale2)
  nI = NCOL(X)
  nJ = NCOL(Y)
  maxRank <- min(nI, nJ)
  if (maxRank < nf2keep) 
    nf2keep = maxRank
  if (is.null(Fi) | is.null(Fj)) {
    S <- t(X) %*% Y
    M <- t(X) %*% X
    W <- t(Y) %*% Y
    Mm12 <- matrix.exponent(M, power = -1/2)
    Wm12 <- matrix.exponent(W, power = -1/2)
    svd.S <- gsvd(DAT = S, LW = Mm12, RW = Wm12, k = nf2keep)
    if (nf2keep > length(svd.S$d)) 
      nf2keep = length(svd.S$d)
    Lx <- X %*% svd.S$u
    Ly <- Y %*% svd.S$v
    Fi <- svd.S$u * matrix(svd.S$d, nI, nf2keep, byrow = TRUE)
    Fj <- svd.S$v * matrix(svd.S$d, nJ, nf2keep, byrow = TRUE)
  }
  else {
    nL = min(NCOL(Fi), NCOL(Fj))
    if (nL < nf2keep) 
      nf2keep = nL
    Fi = Fi[, 1:nf2keep]
    Fj = Fj[, 1:nf2keep]
    delta.inv <- 1/sqrt(colSums(Fi^2))
    Lx <- X %*% (Fi * matrix(delta.inv, nI, nf2keep, byrow = TRUE))
    Ly <- Y %*% (Fj * matrix(delta.inv, nJ, nf2keep, byrow = TRUE))
  }
  fj.boot <- array(NA, dim = c(nJ, nf2keep, nIter))
  dimnames(fj.boot)[1] <- list(colnames(Y))
  dimnames(fj.boot)[2] <- list(paste0("Dimension ", 1:nf2keep))
  dimnames(fj.boot)[3] <- list(paste0("Iteration ", 1:nIter))
  fi.boot <- array(NA, dim = c(nI, nf2keep, nIter))
  dimnames(fi.boot)[1] <- list(colnames(X))
  dimnames(fi.boot)[2] <- list(paste0("Dimension ", 1:nf2keep))
  dimnames(fi.boot)[3] <- list(paste0("Iteration ", 1:nIter))
  if (eig) {
    eigenValues <- matrix(0, nrow = nIter, ncol = maxRank)
    colnames(eigenValues) <- paste0("Dimension ", 1:maxRank)
    rownames(eigenValues) <- paste0("Iteration ", 1:nIter)
    fixedEigenvalues <- sv2(compS(X, center1 = center1, 
                                  scale1 = scale1, 
                                  Y, center2 = center2, 
                                  scale2 = scale2))
    names(fixedEigenvalues) <- paste0("Dimension ", 1:length(fixedEigenvalues))
  }
  for (ell in 1:nIter) {
    boot.index <- sample(nN, replace = TRUE)
    fi.boot[, , ell] <- t(X[boot.index, ]) %*% Ly[boot.index, ]
    fj.boot[, , ell] <- t(Y[boot.index, ]) %*% Lx[boot.index, ]
    if (eig) {
      eigenS <- sv2(compS(X[boot.index, ], center1 = center1, 
                          scale1 = scale1, Y[boot.index, ], center2 = center2, 
                          scale2 = scale2))
      index <- min(maxRank, length(eigenS))
      eigenValues[ell, 1:index] <- eigenS
    }
  }
  BR.j <- .boot.ratio.test(fj.boot, critical.value)
  BR.i <- .boot.ratio.test(fi.boot, critical.value)
  return.list <- structure(list(bootstrapBrick.i = fi.boot, 
                bootRatios.i = BR.i$boot.ratios,
                bootRatiosSignificant.i = BR.i$sig.boot.ratios, 
                bootstrapBrick.j = fj.boot, 
                bootRatios.j = BR.j$boot.ratios, 
                bootRatiosSignificant.j = BR.j$sig.boot.ratios), 
                class = "bootBrick.ij4plsc")
  if (eig) {
    eigenValues <- eigenValues[, colSums(eigenValues) > 0]
    return.list$eigenValues = eigenValues
    sortedEigenValues <- apply(eigenValues, 2, sort)
    index = round(nIter * (alphaLevel/2))
    if (index == 0)  index <- 1
    eigenCI = sortedEigenValues[c(index, nIter - (index - 1)), ]
    minCI <- as.character(alphaLevel/2)
    substr(minCI, 1, 2) <- "_"
    minCI <- paste0("LB", minCI)
    maxCI <- as.character(1 - (alphaLevel/2))
    substr(maxCI, 1, 2) <- "_"
    maxCI <- paste0("UB", maxCI)
    rownames(eigenCI) <- c(minCI, maxCI)
    return.list$fixedEigenvalues <- fixedEigenvalues
    return.list$eigenCI <- eigenCI
    class(return.list) <- "bootBrick.ij.eig4plsc"
  }
  return(return.list)
} # End of Function ----
#_____________________________________________________________________