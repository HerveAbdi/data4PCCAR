# Varimax function de luxe to be stored in data4PCCAR
# HA: September 21, 2018.
# function in this file: epVari print.epVari
#_____________________________________________________________________
# What about a rotation -----
# make it a function
#_____________________________________________________________________
# Helper for roxygen2 ----
#  install.packages('sinew')
#  sinew::makeOxygen(epVari)
#_____________________________________________________________________

#' @title Varimax rotation on loadings and factor scores.
#' @description \code{epVari}: a
#' Varimax rotation on loadings and factor scores.
#' \code{epVari} is based on \code{stats::varimax}
#' (with parameters \code{normalize = TRUE, eps = 1e-6}), but
#' gives also the rotated factor scores, pseudo eigenvalues
#' and explained percentage of inertia for the rotated dimensions.
#' @param resExpo the results of a principal component
#' analysis performed
#' by  \code{ExPosition::epPCA}.
#' @param dim2keep \code{(Default = 2)}
#' the number of dimensions to keep.
#' @param ... stuff to pass to \code{varimax}.
#' @return A list with 6 elements:
#' \enumerate{
#' \item \code{rotationMatrix:} the rotation matrix
#' \item \code{rotated.I:} the rotated factor scores (I-set).
#' \item \code{rotated.J:} the loadings (J-set).
#' \item \code{rotated.eigs:} the rotated pseudo-eigenvalues.
#' \item \code{rotated.t:} the rotated percentage of explained inertia.
#' }
#' @details \code{epVar} is a wraper around stats::varimax.
#' @import ExPosition
#' @importFrom stats varimax
#' @examples
#' data(iris)
#' library(ExPosition)
#' resPCA <- ExPosition::epPCA(iris[,1:4], graphs = FALSE)
#' iris.Vari <-  epVari(resPCA)
#' @seealso \code{\link[stats]{varimax}}
#' @author Hervé Abdi
#' @rdname epVari
#' @export
epVari <- function(resExpo, dim2keep = 2,...){
  nL <- NCOL(resExpo$ExPosition.Data$fj)
  if ((nL <= 1)){stop('Not enough dimensions (1 or 0) for a rotation')}
  if (nL < dim2keep){dim2keep = nL
  warning(paste('dim2keep too large: shrunk to: ',nL))} # end if
  resVar <- varimax(resExpo$ExPosition.Data$fj[,1:dim2keep],
                    normalize = TRUE, eps = 1e-6)
  rotMat <- resVar$rotmat
  rownames(rotMat) <- paste0('Dimension ',1:dim2keep)
  colnames(rotMat) <- paste0('Dimension ',1:dim2keep)

  rotated.J <- resExpo$ExPosition.Data$fj[,1:dim2keep] %*% rotMat
  rotated.I <- resExpo$ExPosition.Data$fi[,1:dim2keep] %*% rotMat
  vari.eig  <- colSums(rotated.I^2)
  # vari.tau.1 <- 100*vari.eig / sum(resPCA$ExPosition.Data$eigs)
  # formula 1 will work only when all eig have been returned
  #  the formula below always works
  vari.tau = (vari.eig / resExpo$ExPosition.Data$eigs[1:dim2keep]) *
                                 resExpo$ExPosition.Data$t[1:dim2keep]
  # return a list
  return.list <- structure(list(rotationMatrix = rotMat,
                                rotated.I      = rotated.I,
                                rotated.J      = rotated.J,
                                rotated.eigs   = vari.eig,
                                rotated.t      = vari.tau),
                           class = 'epVari')
  return(return.list)
} # end epVari()
#_____________________________________________________________________
# print.epVari ----
#
#' Change the print function for createFactorMap
#'
#'  Change the print function for createFactorMap
#'
#' @param x a list: output of createFactorMap
#' @param ... everything else for the functions
#' @author Hervé Abdi
#' @export
print.epVari <- function(x, ...) {
  ndash = 78 # How many dashes for separation lines
  cat(rep("-", ndash), sep = "")
  cat("\nResults of a Varimax Rotation \n")
  # cat("\n List name: ",deparse(eval(substitute(substitute(x)))),"\n")
  cat(rep("-", ndash), sep = "")
  cat("\n$rotationMatrix ", "The Rotation Matrix")
  cat("\n$rotated.I      ", "The rotated factor scores (I-set)")
  cat("\n$rotated.J      ", "The rotated loadings (J-set)")
  cat("\n$rotated.eigs   ", "The rotated (pseudo) eigenvalues")
  cat("\n$rotated.t      ", "The rotated (pseudo) percentages of inertia")
  cat("\n",rep("-", ndash), sep = "")
  cat("\n")
  invisible(x)
} # end of function print.epVari ----
# _____________________________________________________________________

