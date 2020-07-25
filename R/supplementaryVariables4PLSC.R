# Head ----
# File: supplementary4PLSC 
# Projection of supplementary variables for a PLSC analysis with tepPLS
# functions in this file: 
#       supplementaryVariables4PLSC
#       print.print.supVar4PLSC
#   + an internal function
#       projOnDualSet4PLS    
# Hervé Abdi: first version July 21, 2020/
# Current July 23, 2020. ----
#_____________________________________________________________________
#_____________________________________________________________________
#_____________________________________________________________________
# Helper for roxygen2 ----
#  install.packages('sinew')
#  sinew::makeOxygen(supplementaryVariables4PLSC)
#_____________________________________________________________________
#
# supplementaryVariables4PLSC ----
#preamble supplementaryVariables4PLSC ----
#_____________________________________________________________________
##' @title Project supplementary variables (columns) 
##' for a PLSC analysis (from  \code{\link[TExPosition]{tepPLS}}).
##' 
#' @description \code{supplementaryVariables4PLSC}: 
#' Projects supplementary variables (columns) 
#' for a PLSC analysis 
#' (from  \code{\link[TExPosition]{tepPLS}}).
#' The variables should be measured on the same observations
#' as the observations measured on the original analysis.
#' The original data consisted in 2 matrices denoted
#' **X** (dimensions \eqn{N} by \eqn{I})  and 
#' **Y** (\eqn{N} by \eqn{J}).
#' The supplementary data denoted **V**sup is a
#' \eqn{N} by \eqn{K} matrix, that can be considered 
#' as originating either from **X**
#' (and then denoted **X**sup) or **Y**
#' (and then denoted **Y**sup) .
#' If originating from **X** 
#' (resp, **Y**) matrix **Y** (resp, **X**) 
#' is the \emph{dual} matrix. 
#' Note that \emph{only the dual matrix} 
#' is needed to project supplementary 
#' variables.
#' See \code{details} for more.
#' 
#' 
#' @param var.sup **V**sup: The \eqn{N} by \eqn{K}
#'  matrix of \eqn{K} supplementary
#' variables.
#' @param resPLSC the results of 
#' a PLSC analysis performed with 
#' \code{\link[TExPosition]{tepPLS}}.
#' @param Xset the original **X** (\eqn{N} by \eqn{I})
#' data matrix. If \code{NULL}, the supplementary data
#' are projected on the dual set (i.e., **Y**).
#' See also \code{details} for more.
#' 
#' @param Yset the original **Y** (\eqn{N} by \eqn{J})
#' data matrix. If \code{NULL}, the supplementary data
#' are projected on the dual set (i.e., **X**).
#' See also \code{details} for more.
#' @param X.center centering parameter for **X** 
#' (Default: \code{TRUE}). 
#' See \code{\link[ExPosition]{expo.scale}} for details.
#' @param X.scale scaling parameter for **X** 
#' (Default: \code{'SS1'}). 
#' See \code{\link[ExPosition]{expo.scale}} for details.
#' @param Y.center 
#' centering parameter for **Y** 
#' (Default: \code{TRUE}). 
#' See \code{\link[ExPosition]{expo.scale}} for details.
#' @param Y.scale  scaling parameter for **Y** 
#' (Default: \code{'SS1'}). 
#' See \code{\link[ExPosition]{expo.scale}} for details.
#' @param dimNames Names for the
#' dimensions (i.e., factors) for the
#' supplementary loadings (Default: \code{'Dimension '}).
#' @return a list with the following elements:
#' \itemize{
#'  \item{"\code{cor.lx}": }{The correlations between the supplementary variables
#'  and the **X** set.}
#'   \item{"\code{cor.ly}": }{The correlations between the supplementary variables
#'  and the **Y** set.}
#'   \item{"\code{loadings.sup.X}": }{The loadings of the supplementary variables
#'  as originating from the \code{Xset} (needs to
#'  have the dual \code{Yset} to be computed).}
#'  \item{"\code{loadings.sup.Y}": }{The loadings of the supplementary variables
#'  as originating from the \code{Yset} (needs to
#'  have the dual \code{Xset} to be computed).}
#' }
#' 
#' @details The computation relies on the SVD
#' of the correlation matrix between **X** and **Y**,
#' computed as **R** = **X**'**Y** 
#' (where **X** **Y** are the original data matrices that
#' have been preprocessed,
#' with, e.g., centering and scaling) 
#' and decomposed with the
#' SVD as **R** = **PDQ**', with the usual constraints that
#' **P**'**P** = **Q**'**Q** = **I**. 
#' 
#' ## Active loadings
#' The active loadings are **P** (**X**-loadings) 
#' and **Q** (**Y**-loadings). These loadings come
#' from the SVD of matrix **R** = **X**'**Y** = **PDQ**'.
#' 
#' ## Transition formulas
#' The loadings of one set can be obtained from the correlation matrix
#' **R** and the loadings from the dual set. For example:
#' 
#' **P** = **X**'**YQ** inv(**D**) =  **RQ** inv(**D**) 
#' (with inv(**D**) being the inverse of **D**). Eq. 1
#' 
#' ## Projection of supplementary variables
#' 
#' Supplementary variable loadings are obtained by first computing
#' their correlation with their dual set and then projecting these
#' on the singular vector of their dual set. So, for example,
#' the loadings denoted **P**sup for 
#' an  \eqn{N}  by \eqn{K} matrix of 
#' \eqn{K} 
#' supplementary variables considered
#' as belonging to the **X**-set will be projected by first computing
#' the correlation matrix between these variables and
#' **Y** (the dual set) as: **R**sup = **X**sup' **Y** (note that
#' we assume here that **X**sup has been pre-processed in the same way as
#' **X**). The supplementary loadings are now computed
#' by replacing in Eq.1 **X** by **X**sup to obtain:
#' 
#'  **P**sup = **X**sup' * **Y** * **Q** * inv(**D**) 
#'  =  **R**sup * **Q** *  inv(**D**) 
#' . Eq.2. 
#' 
#' 
#' @author  Hervé Abdi
#' @references 
#' See:
#' 
#'  Abdi, H., & Williams, L.J. (2013). Partial least squares methods: 
#'  Partial least squares correlation 
#'  and partial least square regression. 
#'  In: B. Reisfeld & A. Mayeno (Eds.), 
#'  \emph{Methods in Molecular Biology: 
#'  Computational Toxicology}. New York: Springer Verlag. 
#'  pp. 549-579.
#'  
#'   Abdi, H. (2007). Singular Value Decomposition (SVD) 
#'   and Generalized Singular Value Decomposition (GSVD). 
#'   In N.J. Salkind (Ed.): 
#'   \emph{Encyclopedia of Measurement and Statistics}. 
#'   Thousand Oaks (CA): Sage. pp. 907-912. 
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[TExPosition]{tepPLS}} 
#'  \code{\link[ExPosition]{expo.scale}} 
#'  \code{\link{supplementaryObservations4PLSC}} 
#' @rdname supplementaryVariables4PLSC
#' @export 
#' @importFrom ExPosition expo.scale
#_____________________________________________________________________
supplementaryVariables4PLSC <- function(var.sup, 
                                        resPLSC,
                                        Xset     = NULL,
                                        Yset     = NULL,
                                        X.center = TRUE,
                                        X.scale  = 'SS1',
                                        Y.center = TRUE,
                                        Y.scale  = 'SS1',
                                        dimNames = 'Dimension '
){
  if ((is.null(Xset)) & (is.null(Yset))){
    warning(
'supplementaryVariables4PLSC needs at least one of "Xset" or "Yset"')
    print('Results will give only correlations with latent variables')      
  }
  # an internal function ----
  # subfunction projOnDualSet4PLS ----
  projOnDualSet4PLS <- function(mat2proj, 
                                mat2proj.center,
                                mat2proj.scale,
                                dual.set, 
                                dual.center,
                                dual.scale,
                                dual.singVectors,
                                delta,
                                dimNames = dimNames){
    Z.sup <- ExPosition::expo.scale(mat2proj, 
                                    center = mat2proj.center, 
                                    scale  = mat2proj.scale)
    Z.active <- ExPosition::expo.scale(dual.set, 
                                       center = dual.center, 
                                       scale  = dual.scale)
    Rsup <- t(Z.sup) %*% Z.active
    loadings.sup <- Rsup %*% 
      (dual.singVectors * 
         matrix(1/delta, ncol = length(delta), 
                nrow = nrow(dual.singVectors), byrow = TRUE))
    noms2col <- paste0(dimNames, 1 : ncol(loadings.sup))
    colnames(loadings.sup) <- noms2col
    return(loadings.sup)
  } # end of internal function
  #___________________________________________________________________
  if (nrow(var.sup) != nrow(resPLSC$TExPosition.Data$lx)){
    stop('"var.sup" and "original data matrices" should have same number of rows')
  }
  return.list <- structure(list(),
                           class = 'supVar4PLSC')
  # 1. Project X-variables as sup
  # loadings.sup.x
  delta <- resPLSC$TExPosition.Data$pdq$Dv
  noms2col <- paste0(dimNames, 1 : length(delta))
  # to be moved at the end
  # cor.lx - ly ----
  # loadings4Xsup ----
  if (!is.null(Yset)){# Get projection as Xset
    loadings.sup.X <-  projOnDualSet4PLS(mat2proj = var.sup, 
                                         mat2proj.center = X.center,
                                         mat2proj.scale  = X.scale,
                                         dual.set = Yset, 
                                         dual.center = Y.center,
                                         dual.scale  = Y.scale,
                                         dual.singVector = resPLSC$TExPosition.Data$pdq$q,
                                         delta = delta,
                                         dimNames = dimNames)
    return.list$loadings.sup.X <- loadings.sup.X
  }
  
  if (!is.null(Xset)){# Get projection as Xset
    loadings.sup.Y <-  projOnDualSet4PLS(mat2proj = var.sup, 
                                         mat2proj.center = Y.center,
                                         mat2proj.scale = Y.scale,
                                         dual.set = Xset, 
                                         dual.center = X.center,
                                         dual.scale = X.scale,
                                         dual.singVector = resPLSC$TExPosition.Data$pdq$p,
                                         delta = delta,
                                         dimNames = dimNames)
    return.list$loadings.sup.Y <- loadings.sup.Y
  }
  
  cor.lx <- cor(var.sup, resPLSC$TExPosition.Data$lx)
  cor.ly <- cor(var.sup, resPLSC$TExPosition.Data$ly)
  colnames(cor.lx) <- noms2col -> colnames(cor.ly)
  return.list$cor.lx <- cor.lx
  return.list$cor.ly <- cor.ly
  
  # if (!is.null(Yset)){# Get projection as Xset
  #   
  #     dual.singularVectors <- resPLSC$TExPosition.Data$pdq$q
  #     Z.active <- ExPosition::expo.scale(Yset, 
  #                               center = Y.center, 
  #                               scale  = Y.scaled)
  #     Z.sup <- ExPosition::expo.scale(var.sup, 
  #                                     center = Y.center, 
  #                                     scale  = Y.scaled)
  #     
  #      } else {
  #     dual.singularVectors <- resPLSC$TExPosition.Data$pdq$p  
  #      }
  # 
  # delta <- resPLSC$TExPosition.Data$pdq$Dv
  # Zsup <- ExPosition::expo.scale(var.sup, 
  #                                center = active.center, 
  #                                scale  = active.scaled )
  # 
  # X.active <- ExPosition::expo.scale(, 
  #                          center = active.center, 
  #                          scale  = active.scaled)
  # 
  # first do it right ----
  # Rsup <- t(Zsup) %*% dual.Zactive
  # just to check here that defauls gives correlation
  #  all(near(Rsup, cor(var.sup, dualSet))) # works
  # Comprehension formula for loadings.sup 
  #loadings.sup.1 <- Rsup %*% dual.singularVectors %*% diag(1/delta)
  # Computational formula for loadings.sup
  # loadings.sup <- Rsup %*% 
  #        (dual.singularVectors * 
  #        matrix(1/delta, ncol = length(delta), 
  #               nrow = nrow(dual.singularVectors), byrow = TRUE))
  # noms2col <- paste0('Dimension ', 1 : ncol(loadings.sup))
  # colnames(loadings.sup) <- noms2col
  # cor.lx <- cor(var.sup, resPLSC$TExPosition.Data$lx)
  # cor.ly <- cor(var.sup, resPLSC$TExPosition.Data$ly)
  # colnames(cor.lx) <- noms2col -> colnames(cor.ly)
  # return.list <- structure(list(loadings.sup      =  loadings.sup,
  #                               cor.lx = cor.lx,
  #                               cor.ly = cor.ly),
  #                          class = 'supVar4PLSC')
  return(return.list)
  #check
  #all(near(loadings.sup,resPLSC$TExPosition.Data$pdq$p[1:8,]))
  # works
} # End of supplementaryVariables4PLSC() ----


# print function for class 'supVar4PLSC'
#_____________________________________________________________________
#_____________________________________________________________________
# print routines ----
# #_____________________________________________________________________
# print.supVar4PLSC ----
#
#' Change the print function for supVar4PLSC
#'
#'  Change the print function for supVar4PLSC
#'
#' @param x a list: output of supplementaryVariables4PLSC
#' @param ... everything else for the functions
#' @author Hervé Abdi
#' @export
print.supVar4PLSC <- function(x, ...) {
  ndash = 78 # How many dashes for separation lines
  cat(rep("-", ndash), sep = "")
  cat("\nSupplementary columns for PLSC \n")
  # cat("\n List name: ", deparse(eval(substitute(substitute(x)))),"\n")
  cat(rep("-", ndash), sep = "")
  cat("\n$cor.lx        : ", "Supplementary Loadings as correlation with lx")
  cat("\n$cor.ly        : ", "Supplementary Loadings as correlation with ly")
  if (!is.null(x$loadings.sup.X)){
  cat("\n$loadings.sup.X: ", "Supplementary Loadings for the X-set") }
  if (!is.null(x$loadings.sup.Y)){
  cat("\n$loadings.sup.Y: ", "Supplementary Loadings for the Y-set") }
  cat("\n",rep("-", ndash), sep = "")
  cat("\n")
  invisible(x)
} # end of function print. supVar4PLSC ----
# ____________________________________________________________________
#
#_____________________________________________________________________