# Head ----
# File: supplementary4PLSC 
# Projection of supplementary elements for a PLSC analysis with tepPLS
# functions in this file: projSupplementaryObservations4PLSC
#                         supplementaryObservations4PLSC
# Hervé Abdi: first version July 19, 2020/
# Current July 19, 2020. ----
#_____________________________________________________________________
#_____________________________________________________________________
# Helper for roxygen2 ----
#  install.packages('sinew')
#  sinew::makeOxygen(projSupplementaryObservations4PLSC)
#   sinew::makeOxygen(supplementaryObservations4PLSC)
#_____________________________________________________________________
## Preamble ----
#' @title Compute latent variables / factor scores for supplementary
#' observations in a \code{PLSC} analysis 
#' computed with \code{\link[TExPosition]{tepPLS}}.

#' 
#' @description \code{projSupplementaryObservations4PLSC}
#' computes latent variables / factor scores for supplementary
#' observations in a \code{PLSC} analysis 
#' computed from \code{TExPosition} \code{\link[TExPosition]{tepPLS}}. 
#' \code{projSupplementaryObservations4PLSC}
#' preprocesses and projects supplementary observations for a PLSC
#' by first preprocessing the variables (i.e., centering and scaling)
#' and then projecting the preprocessed observations 
#' onto the loadings (a.k.a., saliences) from the original analysis.
#'  \code{projSupplementaryObservations4PLSC} is mostly used
#'  as a preparatory step for
#'   \code{supplementaryObservations4PLSC}.
#' @param mat4sup The observations to be projected
#' @param center_fixed The barycenter vector (i.e., mean)
#' used to center the original variables.
#' @param scale_fixed the normalizing vector
#' used to scale the original variables.
#' @param loadings_fixed the loadings 
#' used to project the supplementary observations on the 
#' variables (obtained from the original analysis).
#' 
#' @return the supplementary latent variable values.
#' @details \code{projSupplementaryObservations4PLSC}
#' is mostly a subroutine for
#' \code{\link{supplementaryObservations4PLSC}}.
#'
#' @author Hervé Abdi
#' @seealso  \code{\link[TExPosition]{tepPLS}} 
#' \code{\link{supplementaryObservations4PLSC}}
# # @examples 
# # \dontrun{
# #if(interactive()){
#  #EXAMPLE1
#  #}
# #}
#' @rdname projSupplementaryObservations4PLSC
#' @export 
## # Project
projSupplementaryObservations4PLSC <- function(mat4sup, 
                                              # data to be projected
           center_fixed,  # centering factor
           scale_fixed,   # scaling factor
           loadings_fixed # loadings out of PLSC
){
  # Preprocess
  # 
  mat4sup <- as.matrix(mat4sup) # make sure this is a matrix
  mat4sup.center <- t(apply(mat4sup,        1,'-', center_fixed))
  Zsup           <- t(apply(mat4sup.center,1,'/', scale_fixed)) 
  # Project
  l.sup <- Zsup %*% loadings_fixed
  colnames(l.sup) <- paste0('Dimension ',1:ncol(l.sup))
  return(l.sup)
} # end of projsupplementaryObservations4PLSC ----
#_____________________________________________________________________

#Preamble supplementaryObservations4PLSC ----
# function supplementaryObservations4PLSC
#' @title compute the latent variables for
#' supplementary observations for a PLSC model 
#' computed with  \code{\link[TExPosition]{tepPLS}}.
#' 
#' @description \code{supplementaryObservations4PLSC}:
#' computes the latent variables for
#' supplementary observations for a PLSC model 
#' computed with  \code{\link[TExPosition]{tepPLS}}.
#' 
#' @param resPLSC the results of a \code{PLSC} analysis
#' from  \code{\link[TExPosition]{tepPLS}}.
#' @param Xsup an \eqn{N}sup by \eqn{I} matrix of
#' supplementary observations matching the **X** matrix
#' (see \code{description for details}).
#'  When \code{NULL} (Default) nothing is computed for \code{Xsup}.
#' @param Ysup 
#'  an \eqn{N}sup by \eqn{J} matrix of
#' supplementary observations matching the **Y** matrix
#' (see \code{description for details}).
#'  When \code{NULL} (Default) nothing is computed for \code{Ysup}.
#'@param dimNames Names for the
#' dimensions (i.e., factors) for the
#' supplementary loadings (Default: \code{'Dimension '}).
#' @return A list with \code{lx.sup} and \code{ly.sup} 
#' giving the latent variables values 
#' of the supplementary observations
#' for (respectively) \eqn{X} and \eqn{Y}.
#' @details The original analysis is performed with 
#' \code{\link[TExPosition]{tepPLS}} on the original data matrices
#' **X** (\eqn{N} by \eqn{I}) and **Y** (\eqn{N} by \eqn{J}).
#' The supplementary data matrices should have \eqn{I}
#' columns for \eqn{X}sup and \eqn{J}
#' columns for \eqn{Y}sup. 
#' 
# #@examples 
# #\dontrun{
#if(interactive()){
#  #EXAMPLE1
# #    }
# #}
#' @rdname supplementaryObservations4PLSC
#' @export 

supplementaryObservations4PLSC <- function(resPLSC, 
                                           Xsup = NULL, 
                                           Ysup = NULL,
                                           dimNames = 'Dimension '){
  # Check parameters around here
  return.list <- structure(list(),
                           class = 'supElementsPLS'
  )
  noms2col <- paste0(dimNames, 1:ncol(resPLSC$TExPosition.Data$lx))
  # Look at Xsup
  if(!is.null(Xsup)){
    nI <- nrow(resPLSC$TExPosition.Data$fi)
    if(nI != ncol(Xsup)){
      stop('Xsup should have the same number of columns as X in epPLS')
    }# End if 
    lx.sup <- projSupplementaryObservations4PLSC(Xsup,# data
           center_fixed = resPLSC$TExPosition.Data$data1.norm$center,
           # centering factor
           scale_fixed = resPLSC$TExPosition.Data$data1.norm$scale,  
           # scaling factor
           loadings_fixed = resPLSC$TExPosition.Data$pdq$p 
           # loadings out of PLSC
    )
    colnames(lx.sup)   <- noms2col 
    return.list$lx.sup <- lx.sup
  }
  if(!is.null(Ysup)){
    nJ <- nrow(resPLSC$TExPosition.Data$fj)
    if(nJ != ncol(Ysup)){
      stop('Ysup should have the same number of columns as Y in epPLS')
    }# End if 
    ly.sup <- projSupplementaryObservations4PLSC(Ysup,# data
          center_fixed = resPLSC$TExPosition.Data$data2.norm$center,
          # centering factor
          scale_fixed =  resPLSC$TExPosition.Data$data2.norm$scale,  
          # scaling factor
          loadings_fixed = resPLSC$TExPosition.Data$pdq$q 
          # loadings out of PLSC
    )
    colnames(ly.sup)   <- noms2col 
    return.list$ly.sup <-  ly.sup
  }
  return(return.list)
}

# _____________________________________________________________________
# print.supElementsPLS ----
#
#' Change the print function for supElementsPLS
#'
#'  Change the print function for supElementsPLS
#'
#' @param x a list: output of supplementary4Vari
#' @param ... everything else for the functions
#' @author Hervé Abdi
#' @export
print.supElementsPLS <- function(x, ...) {
  ndash = 78 # How many dashes for separation lines
  cat(rep("-", ndash), sep = "")
  cat("\nLatent Variables for Supplementary Observations for epPLS/epPLSCA  \n")
  # cat("\n List name: ", deparse(eval(substitute(substitute(x)))),"\n")
  cat(rep("-", ndash), sep = "")
  if (!is.null(x$lx.sup)){
  cat("\n$lx.sup: ", "Latent variables for the X-set (if any)")}
  if (!is.null(x$ly.sup)){
  cat("\n$ly.sup: ", "Latent variables for the Y-set (af any)")
  cat("\n",rep("-", ndash), sep = "") }
  cat("\n")
  invisible(x)
} # end of function print.supElementsPLS ----
# _____________________________________________________________________