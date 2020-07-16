# Head ----
# File: Supplementary4Vari 
# Projection of Supplementary ELements after a Varimax Rotation
# functions in this file: Supplementary4Vari 
#                         print.epVariSup.i,   print.epVariSup.j
# Hervé Abdi: July 16, 2020. 
#_____________________________________________________________________
#_____________________________________________________________________
# Helper for roxygen2 ----
#  install.packages('sinew')
#  sinew::makeOxygen(Supplementary4Vari)
#_____________________________________________________________________
## Preamble ----
#' @title Compute the projection of supplementary elements 
#' (rows or columns) for a PCA followed by a \code{Varimax}
#' rotation.
#' @description \code{Supplementary4Vari} 
#' Computes the projection of supplementary elements 
#' (rows or columns) for a PCA (computed with
#' \code{ExPosition::epPCA()})
#' followed by a \code{Varimax} rotation 
#' (computed with
#' \code{data4PCCAR::epVari()}).
#' Supplementary elements can be rows or columns:
#' If the original data set had dimensions \eqn{I} by \eqn{J},
#' supplementary rows should have dimensions \eqn{Isup} by \eqn{J},
#' and 
#' supplementary columns should have dimensions \eqn{I} by \eqn{Jsup}.
#' Note that for  supplementary columns, the parameters
#' \code{scale} and \code{center} need to be specified in the call.  
#' @param SUP.DATA the supplementary data set. 
#' Can be rows (dimensions \eqn{Isup} by \eqn{J}) 
#' or columns (dimensions \eqn{I} by \eqn{Jsup}). 
#' @param resPCA the results of a PCA performed by 
#' \code{ExPosition::epPCA()}.
#' @param resVari the results of a \code{Varimax} rotation
#' performed by \code{data4PCCAR::epiVari} on the same data set
#' as \code{resPCA}.
#' @param set what set (\code{'rows'} or \code{'columns'}) is projected 
#' (Default: \code{'rows'})
#' @param center value of the \code{center} parameter 
#' that was used for the original  PCA analysis
#' for columns (Default: \code{TRUE}).
#' @param scale value of the \code{scale} parameter 
#' that was used for the original  PCA analysis
#' for columns (Default: \code{'SS1'}; 
#' look at \code{ExPosition::epPCA} for possible values).
#' @return a list with the coordinates of the elements in
#' the \code{Varimax} space. Coordinates are denoted \code{$fii} for
#' the rows and \code{$fjj} for
#' the  columns.
#' @importFrom ExPosition supplementaryRows supplementaryCols
#' @details 
#' The computation of the coordinates
#'  is obtained by first projecting the data
#' as supplementary elements 
#' (using \code{supplementaryCols} or
#' \code{supplementaryRows} from \code{ExPosition}) in the 
#' unrotated space and then rotating in the \code{Varimax}
#' space using the rotation matrix from \code{epiVari()}.
#' @author Hervé Abdi
#' @examples 
#' \dontrun{
#' if(interactive()){
#' library(ExPosition) # for epPCA()
#' resPCA       <- epPCA(iris[1:4], scale = 'SS1', graphs = FALSE)
#' resVari      <- epVari(resPCA)
#' resVariSup.i <- Supplementary4Vari(matrix(c(5,4,2,.5), nrow =1),
#'                                    resPCA, resVari)
#'  }
#' }
#' @rdname Supplementary4Vari
#' @export 
# *** 1. test sup I 4 vari ----
Supplementary4Vari <- function(SUP.DATA, 
                               resPCA, resVari, set = 'rows',
                               center = TRUE, scale = 'SS1'){
  # *** 2. test sup J 4 vari
  #  this one should be TRUE
  if( (class(resPCA$ExPosition.Data)[1]) != 'epPCA'){
  stop('Current version of Supplementary4Vari works only with epPCA')
  }
  if (!(set %in% c('rows',   'Rows',   'I','Iset',
                   'columns','Columns','J','Jset'))){
    stop('Incorrect option for parameter "set"')
  }
  if (set %in% c('rows',   'Rows',   'I','Iset')){ # Iset
    nJ <- nrow(resPCA$ExPosition.Data$fj)
    if (nJ != ncol(SUP.DATA)){
      stop('Incorrect number of columns for the supplementary rows')
    }
    Supfi      <- ExPosition::supplementaryRows(SUP.DATA, resPCA) 
    SupVarifi <- Supfi$fii[, 1:nrow(resVari$rotationMatrix)] %*% 
      resVari$rotationMatrix
    return.list <- structure(list(rotatedfii      =  SupVarifi),
                             class = 'epVariSup.i')
  }  else # end I go for J
  {nI <- nrow(resPCA$ExPosition.Data$fi)
  if (nI != nrow(SUP.DATA)){
    stop('Incorrect number of rows for the supplementary columns')
  } # end if
  Supfj      <- ExPosition::supplementaryCols(SUP.DATA, 
                                  resPCA, center = center,
                                  scale = scale) 
  # print(Supfj$fjj)
  SupVarifj <- Supfj$fjj[, 1:nrow(resVari$rotationMatrix)] %*% 
    resVari$rotationMatrix
  #print(SupVarifj)
  # add correlation. To be done later
  # 
  return.list <- structure(list(rotatedfjj   =  SupVarifj),
                           class = 'epVariSup.j')
  }
  return(return.list)
} # End of Supplementary4Vari
#_____________________________________________________________________
# print routines ----
# #_____________________________________________________________________
# print.epVariSup.i ----
#
#' Change the print function for createFactorMap
#'
#'  Change the print function for createFactorMap
#'
#' @param x a list: output of createFactorMap
#' @param ... everything else for the functions
#' @author Hervé Abdi
#' @export
print.epVariSup.i <- function(x, ...) {
  ndash = 78 # How many dashes for separation lines
  cat(rep("-", ndash), sep = "")
  cat("\nSupplementary rows projected in the rotated Varimax space \n")
  # cat("\n List name: ",deparse(eval(substitute(substitute(x)))),"\n")
  cat(rep("-", ndash), sep = "")
  cat("\n$rotatedfii: ", "Rotated coordinates of the supplementary rows")
  cat("\n",rep("-", ndash), sep = "")
  cat("\n")
  invisible(x)
} # end of function print.epVariSup.i ----
# _____________________________________________________________________
# print.epVariSup.j ----
#
#' Change the print function for createFactorMap
#'
#'  Change the print function for createFactorMap
#'
#' @param x a list: output of createFactorMap
#' @param ... everything else for the functions
#' @author Hervé Abdi
#' @export
print.epVariSup.j <- function(x, ...) {
  ndash = 78 # How many dashes for separation lines
  cat(rep("-", ndash), sep = "")
  cat("\nSupplementary columns projected in the rotated Varimax space \n")
  # cat("\n List name: ",deparse(eval(substitute(substitute(x)))),"\n")
  cat(rep("-", ndash), sep = "")
  cat("\n$rotatedfjj: ", "Rotated coordinates of the supplementary columns")
  cat("\n",rep("-", ndash), sep = "")
  cat("\n")
  invisible(x)
} # end of function print.epVariSup.j ----
# _____________________________________________________________________


