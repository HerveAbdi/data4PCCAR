# Head ----
# File: supplementary4PLSCA 
# Projection of supplementary elements 
#     for a PLSCA analysis with tepPLSCA
# function(s) in this file: supplementaryObservations4PLSC
# Hervé Abdi: first version July 31, 2020.
# Current July 31, 2020. ----
#_____________________________________________________________________
#_____________________________________________________________________
# Helper for roxygen2 ----
#  install.packages('sinew')
#  sinew::makeOxygen(projSupplementaryObservations4PLSCA)
#_____________________________________________________________________
## Preamble ----
#_____________________________________________________________________

#Preamble supplementaryObservations4PLSCA ----
# function supplementaryObservations4PLSCA
#' @title compute the latent variables for
#' supplementary observations for a PLSC model 
#' computed with  \code{\link[TExPosition]{tepPLSCA}}.
#' 
#' @description \code{supplementaryObservations4PLSCA}:
#' computes the latent variables for
#' supplementary observations for a PLSCA model 
#' computed with  \code{\link[TExPosition]{tepPLSCA}}.
#' 
#' @param resPLSCA the results of a \code{PLSCA} analysis
#' from  \code{\link[TExPosition]{tepPLSCA}}.
#' @param Xsup an \eqn{N}sup by \eqn{I} matrix of
#' supplementary observations matching the **X** matrix
#' (see \code{description} for details).
#'  When \code{NULL} (Default) nothing is computed for \code{Xsup}.
#' @param Ysup 
#'  an \eqn{N}sup by \eqn{J} matrix of
#' supplementary observations matching the **Y** matrix
#' (see \code{description} for details).
#'  When \code{NULL} (Default) nothing is computed for \code{Ysup}.
#' @return A list with \code{lx.sup} and \code{ly.sup} 
#' giving the latent variables values 
#' of the supplementary observations
#' for (respectively) \eqn{X} and \eqn{Y}.
#' @details The original analysis is performed with 
#' \code{\link[TExPosition]{tepPLSCA}} on the original data matrices
#' **X** (\eqn{N} by \eqn{I}) and **Y** (\eqn{N} by \eqn{J}).
#' The supplementary data matrices should have \eqn{I}
#' columns for \eqn{X}sup and \eqn{J}
#' columns for \eqn{Y}sup. Note that \code{PLSCA} is used
#' with qualitative variables (i.e., factors) recoded
#' as 0/1 variables with disjunctive coding
#' (i.e., with  \code{\link[ExPosition]{makeNominalData}}), 
#' the supplementary 
#' variables need to be recoded in the same way. 
#' 
#' ## Implementation
#' 
#' For \code{PLSCA} the observations needs to be pre-processed in
#' the same way as the original observations. Often in \code{PLSCA}
#' the observations are described by qualitative variables
#' (in general coded as \emph{factors}) by \code{R}.
#' When this is the case the supplementary observations should coded
#' as factors too with the same levels (aka modalities) as
#' the active observations
#'  (see also  \code{\link[ExPosition]{makeNominalData}}). 
#' 
#' ## Computation
#' 
#' The projections of supplementary observations in \code{PLSC}
#' is obtained using the standard \emph{transition formulas}
#' from correspondence analysis.
#' 
#' @author Hervé Abdi
#' #' @references 
#' See:
#' 
#'  Beaton, D., Dunlop, J., ADNI, & Abdi, H. (2016).
#'  Partial Least Squares-Correspondence Analysis (PLSCA): 
#'  A framework to simultaneously analyze behavioral and genetic data. 
#'  \emph{Psychological Methods, 21}, 621-651. 
#' 
#' Abdi H. & Béra, M. (2018). 
#' Correspondence analysis. 
#' In R. Alhajj and J. Rokne (Eds.), 
#' \emph{Encyclopedia of Social Networks and Mining (2nd Edition)}. 
#' New York: Springer Verlag. 
#'  
#'   Abdi, H. (2007). Singular Value Decomposition (SVD) 
#'   and Generalized Singular Value Decomposition (GSVD). 
#'   In N.J. Salkind (Ed.): 
#'   \emph{Encyclopedia of Measurement and Statistics}. 
#'   Thousand Oaks (CA): Sage. pp. 907-912. 
#' @seealso 
#'   \code{\link[TExPosition]{tepPLSCA}} 
#'  \code{\link[ExPosition]{makeRowProfiles}} 
#'  \code{\link{supplementaryVariables4PLSCA}} 
#'  \code{\link{supplementaryObservations4PLSC}} 
#' @importFrom ExPosition makeRowProfiles 
# #@examples 
# #\dontrun{
#if(interactive()){
#  #EXAMPLE1
# #    }
# #}
#' @rdname supplementaryObservations4PLSCA
#' @export 

supplementaryObservations4PLSCA <- function(resPLSCA, 
                                           Xsup = NULL, 
                                           Ysup = NULL){
  # Check parameters around here
  return.list <- structure(list(),
                           class = 'supElementsPLS'
  )
  nN  <- nrow(resPLSCA$TExPosition.Data$lx)
  scaling.factor <- 1 / sqrt(nN)
  Dv   <-  resPLSCA$TExPosition.Data$pdq$Dv
  Dv_1 <-  1/ Dv
  Fi <-  resPLSCA$TExPosition.Data$fi
  Fj <- resPLSCA$TExPosition.Data$fj
  # Look at Xsup
  if(!is.null(Xsup)){
    nI <- nrow(Fi)
    if(nI != ncol(Xsup)){
      stop('Xsup should have the same number of columns as X in epPLS')
    }# End if 
     L.sup <-  ExPosition::makeRowProfiles(Xsup)$rowProfiles
     lx.sup  <- scaling.factor * L.sup %*% 
       (Fi  * matrix((Dv_1), nrow(Fi),
                    ncol = length(Dv_1), byrow = TRUE))
     # Fi %*% Dd_1 less efficient
    return.list$lx.sup <- lx.sup
  }
  if(!is.null(Ysup)){
    nJ <- nrow(Fj)
    if(nJ != ncol(Ysup)){
      stop('Ysup should have the same number of columns as Y in epPLS')
    }# End if 
    C.sup <-  ExPosition::makeRowProfiles(Ysup)$rowProfiles
    ly.sup  <- scaling.factor * C.sup %*% 
      (Fj  * matrix((Dv_1), nrow(Fj),
                    ncol = length(Dv_1), byrow = TRUE))
    return.list$ly.sup <-  ly.sup
  }
  return(return.list)
}

