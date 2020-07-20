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
#' computed from \code{TExPosition::tepPLS}.

#' 
#' @description \code{projSupplementaryObservations4PLSC}
#' computes latent variables / factor scores for supplementary
#' observations in a \code{PLSC} analysis 
#' computed from \code{TExPosition::tepPLS}. 
#' \code{projSupplementaryObservations4PLSC}
#' preprocesses and projects supplementary observations for a PLSC
#' by first preprocessing the variables (i.e., centering and scaling)
#' and then projecting the preprocess observations 
#' onto the loadings (a.k.a., saliences) from the original analysis.
#'  \code{projSupplementaryObservations4PLSC} is mostly used
#'  as a preparatory step for
#'   \code{supplementaryObservations4PLSC}.
#' @param mat4sup The observations to be projected
#' @param center_fixed The barycenter vector (i.e., mean)
#' used to center the original variables.
#' @param scale_fixed the normlizing vector
#' used to scale the original variables.
#' @param loadings_fixed the loadings 
#' used to project the supplementary observations on the 
#' variables (obtaiined from the original analysis).
#' 
#' @return the supplementary latent variable values.
#' @details \code{projSupplementaryObservations4PLSC}
#' is mostly a subroutine of
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
#' from \code{TExPosition::PLS}.
#' 
#' @description \code{supplementaryObservations4PLSC}:
#' computes the latent variables for
#' supplementary observations for a PLSC model 
#' from \code{TExPsoition::PLS}.
#' 
#' @param resPLSC the results of a \code{PLSC} analysis
#' from \code{TExPosition::tepPLSC}.
#' @param Xsup an \eqn{Nsup} by \eqn{I} matrix of
#' supplementary observations matching the \eqn{X} matrix
#' (see \code{description for details}).
#'  When \code{NULL} (Default) noting is computed for \code{Xsup}.
#' @param Ysup 
#'  an \eqn{Nsup} by \eqn{J} matrix of
#' supplementary observations matching the \eqn{Y} matrix
#' (see \code{description for details}).
#'  When \code{NULL} (Default) noting is computed for \code{Ysup}.
#' @return A list with \code{lx.sup} and \code{ly.sup} 
#' giving the latent variables values 
#' of the supplementary observations
#' for (respectively) \eqn{X} and \eqn{Y}.
#' @details The original analysis is performed with 
#' \code{TExPosition::epPLS} on the original data matrices
#' \eqn{X} (\eqn{N} by \eqn{I}) and \eqn{Y} (\eqn{N} by \eqn{J}).
#' The supplementary data matrices should have \eqn{I}
#' columns for \eqn{Xsup} and \eqn{J}
#' columns for \eqn{Ysup}. 
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
                                           Ysup = NULL){
  # Check parameters around here
  return.list <- structure(list(),
                           class = 'supElementsPLS'
  )
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
  cat("\nLatent Variables for Supplementary Observations for epPLS  \n")
  # cat("\n List name: ",deparse(eval(substitute(substitute(x)))),"\n")
  cat(rep("-", ndash), sep = "")
  cat("\n$lx.sup: ", "Latent variables for the X-set (if any)")
  cat("\n$ly.sup: ", "Latent variables for the Y-set (af any)")
  cat("\n",rep("-", ndash), sep = "")
  cat("\n")
  invisible(x)
} # end of function print.supElementsPLS ----
# _____________________________________________________________________