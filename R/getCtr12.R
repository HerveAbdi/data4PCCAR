# Functions in this file
#  getImportantCtr.12
#  print.ctr12
#
# Hervé Abdi. Created 11/06/2018.
#             last modified: 11/07/2018.
#_____________________________________________________________________
# sinew::makeOxygen(getImportantCtr.12) # helper for CTR
#_____________________________________________________________________
# Preamble:  getImportantCtr -----
#' @title Get
#' the items with important contributions for a factorial plane.
#'
#' @description \code{getImportantCtr}: 
#'  Gets the items whose contribution is
#' important for a factorial plane.
#' \code{getImportantCtr.12} gives
#' the contributions of the items to the plane, the items
#' important for the plane, and the items important for the plane
#' or for at least one dimension of the plane.
#' 
#' @details \code{getImportantCtr} is mostly used to create clean
#' factorial maps by plotting only the important items.
#' \code{getImportantCtr.12}:
#' can be used for row or columns of PCA, CA, MCA, etc.
#' @param ctr The contributions of the items (e.g.,
#' the output of \code{ExPosition}
#' such as \code{$ExPosition.Data$ci}).
#' @param eig the eigenvalues of the analysis
#' (e.g.,
#' the output of \code{ExPosition}
#'  such as \code{$ExPosition.Data$eigs}).
#' @param axis1 the horizontal axis (Default: 1).
#' @param axis2 the vertical axis (Default: 2).
#' @return a list with
#' \itemize{
#' \item{ctr.12: }{The contributions of the items to the plane;}
#' \item{importantCtr.12: }{A logical vector identifying
#' the items whose contribution to the plane
#' is larger than the average;}
#' \item{importantCtr.1or2: }{A logical vector identifying
#' the items whose contribution to the plane is larger than
#' the  average, or
#' is larger than the average for at least one of the components.}
#' }
#' @author Hervé Abdi
#' @rdname getImportantCtr
#' @export
getImportantCtr <- function(ctr, eig, axis1 = 1, axis2 = 2){
  nI <- NROW(ctr)
  le.ctr <- ctr[, c(axis1, axis2)]
  le.eig <- eig[c(axis1, axis2)]
  # use sweep to multiply by a diagonal matrix
  absCtrVar <- sweep(le.ctr,2,le.eig, FUN = '*')
  # absCtrVar <- as.matrix(varCtr) %*% diag(resMCA$ExPosition.Data$eigs)
  varCtr12  <- (absCtrVar[,1] + absCtrVar[,2]) /
    (le.eig[1] + le.eig[2])
  names(varCtr12)   <- rownames(ctr)
  importantVar.12   <- (varCtr12 >=  1/nI)
  importantVar.1or2 <- importantVar.12 | (le.ctr[,1 ] >= 1/nI) | (le.ctr[,2 ] >= 1/nI)

  return.list = structure(list(ctr.12 = varCtr12,
                      importantCtr.12 = importantVar.12,
                    importantCtr.1or2 = importantVar.1or2 ),
                          class = 'ctr12')
  return(return.list)
} # end getImportantCtr
# ********************************************************************
#' Change the print function for objects of class \code{ctr12}
#'
#'  Change the print function for objects of class \code{ctr12}
#'
#' @param x a list: output of \code{getImportantCtr.12}.
#' @param ... everything else for the function.
#' @author Hervé Abdi
#' @export
print.ctr12 <- function (x, ...) {
  ndash = 78 # How many dashes for separation lines
  cat(rep("-", ndash), sep = "")
  cat("\n Important Contributions of I Items to a Plane \n")
  # cat("\n List name: ",deparse(eval(substitute(substitute(x)))),"\n")
  cat(rep("-", ndash), sep = "")
  cat("\n$ctr.12             ", "Contributions of the items to the plane")
  cat("\n$importantCtr.12    ", "Items with contribution > 1/I to the plane")
  cat("\n$ importantCtr.1or2 ", "Items with contribution > 1/I to the plane") 
  cat("\n$ importantCtr.1or2 ", "        or contribution > 1/I to at least one dimension.")
  cat("\n",rep("-", ndash), sep = "")
  cat("\n")
  invisible(x)
} # end of function print.ctr12
#_____________________________________________________________________
