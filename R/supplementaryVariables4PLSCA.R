# Head ----
# File: supplementary4PLSCA 
# Projection of supplementary variables for a PLSC analysis with tepPLS
# functions in this file: 
#       supplementaryVariables4PLSC
#       print.print.supVar4PLSC
#   + an internal function
#       projOnDualSet4PLS    
# Hervé Abdi: first version July 21, 2020/
# Current July 26, 2020. ----
# print('Test 07/26/2020.  16:19')
#_____________________________________________________________________
#_____________________________________________________________________
#_____________________________________________________________________
# Helper for roxygen2 ----
#  install.packages('sinew')
#  sinew::makeOxygen(supplementaryVariables4PLSC)
#_____________________________________________________________________
#
# supplementaryVariables4PLSCA ----
#preamble supplementaryVariables4PLSCA ----
#_____________________________________________________________________
##' @title Project supplementary variables (columns) 
##' for a PLSCA analysis (from  \code{\link[TExPosition]{tepPLSCA}}).
##' **Beta Version. Current Version 07/30/2020. **
##' 
#' @description \code{supplementaryVariables4PLSCA}: 
#' Projects supplementary variables (columns) 
#' for a \code{PLSCA} analysis 
#' (from  \code{\link[TExPosition]{tepPLSCA}}).
#' The variables should be measured on the same observations
#' as the observations used in the original analysis.
#' The original data consisted in 2 
#' matrices (containing non-negative numbers such as count, as in
#' correspondece analysis,
#' or often simply 0/1 as in multiple correspondence analysis) denoted
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
#' @param make.var.sup.nominal 
#' logical, when  \code{TRUE} (default)
#' transforms each column of \code{var.sup} 
#' from a factor with \eqn{M} levels
#' to a set of \eqn{M} 0/1 vectors (to create a group coding,
#' also called \emph{complete disjonctive coding}). 
#' @param resPLSCA the results of 
#' a PLSCA analysis performed with 
#' \code{\link[TExPosition]{tepPLSCA}}.
#' @param Xset the original **X** (\eqn{N} by \eqn{I})
#' data matrix. If \code{NULL}, the supplementary data
#' are projected on the dual set (i.e., **Y**).
#' See also \code{details} for more.
#' @param make.Xset.nominal logical, when  \code{TRUE} (default)
#' transforms each column of 
#' \code{Xset} from a factor with \eqn{M} levels
#' to a set of \eqn{M} 0/1 vectors (to create a group coding,
#' also called \emph{complete disjonctive coding}). 
#' @param Yset the original **Y** (\eqn{N} by \eqn{J})
#' data matrix. If \code{NULL}, the supplementary data
#' are projected on the dual set (i.e., **X**).
#' See also \code{details} for more.
#' @param make.Yset.nominal logical, when  \code{TRUE} (default)
#' transforms each column of \code{Yset} 
#' from a factor with \eqn{M} levels
#' to a set of \eqn{M} 0/1 vectors (to create a group coding,
#' also called \emph{complete disjonctive coding}). 
#' @param dimNames Names for the
#' dimensions (i.e., factors) for the
#' supplementary loadings (Default: \code{'Dimension '}).
#' @return a list with the following elements:
#' \itemize{
#' 
#'   \item{"\code{loadings.sup.X}": }{The loadings 
#'   of the supplementary variables
#'  as originating from the \code{Xset} (needs to
#'  have the dual \code{Yset} to be computed).}
#'  \item{"\code{sup.fi}": }{The singular-value-normalized 
#'  loadings of the supplementary variables
#'  as originating from the \code{Xset} (needs to
#'  have the dual \code{Yset} to be computed).}
#'  \item{"\code{loadings.sup.Y}": }{The loadings 
#'  of the supplementary variables
#'  as originating from the \code{Yset} (needs to
#'  have the dual \code{Xset} to be computed).}
#'  \item{"\code{sup.fj}": }{The singular-value-normalized 
#'  loadings of the supplementary variables
#'  as originating from the \code{Yset} (needs to
#'  have the dual \code{Xset} to be computed).}
#'  \item{"\code{cor.lx}": }{The correlations 
#'  between the supplementary variables
#'  and the **X** set.}
#'   \item{"\code{cor.ly}": }{The correlations 
#'   between the supplementary variables
#'  and the **Y** set.}
#' }
#' 
#' @details The computation relies on the Generalized
#' singular decomposition (GSVD)
#' of the contingency between **X** and **Y**,
#' computed as **R** = **X**'**Y** 
#' (where **X** **Y** are the original data matrices that
#' have been preprocessed as for the original \code{PLSCA} analysis,
#'  e.g., transformed into 0/1 vectors) 
#' and decomposed with the
#' GSVD as **R** = **PDQ**', with the (metrics) constraints that
#' **P**'inv(**Dr**) **P** = **Q**'inv(**Dc**)**Q** = **I** where
#' inv() denotes the inverse matrix and where **Dr** (resp **Dc**)
#' are the diagonal matrices of the barycenters of (respectively)
#' **X** and **Y**.
#' 
#' 
#' ## Transition formulas
#' The loadings of one set can be obtained 
#' from the cross-product matrix
#' **R** and the loadings from the dual set. For example:
#' if we denote **Delta** the diagonal matrix of
#' the singular values,
#' **F**  (resp. **G**) the singular value normalized 
#' factor scores (denoted \code{fi}, resp. \code{fj}, 
#' in \code{PLSCA}),
#' and **L** (resp. **C**) the row (resp. column) profiles
#' the loadings of one set are derived from the other set as:
#'  
#' **F** = **LG** inv(**Delta**)  and 
#' **G** = **CF** inv(**Delta**) 
#' (with inv(**Dc**) being the inverse of **Dc**). Eq. 1
#' 
#' ## Projection of supplementary variables
#' 
#' Supplementary variable loadings are obtained by first computing
#' the cross-product matrix with their dual set 
#' and then using the transition formulas from
#' correspondence analysis to 
#' compute one set of loadings from the loadings of the other set.
#' So, for example the loadings denoted \code{fii} for the
#' variables of the \code{Xset} can be obtained from the
#' row profiles of the **R**sup matrix by replacing in Eq.1
#' **L** by **L**sup.
#' 
#' @author  Hervé Abdi
#' @references 
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
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[TExPosition]{tepPLSCA}} 
#'  \code{\link[ExPosition]{makeNominalData}} 
#'  \code{\link[ExPosition]{makeRowProfiles}} 
#'  \code{\link{supplementaryObservations4PLSC}} 
#' @rdname supplementaryVariables4PLSCA
#' @export 
#' @importFrom ExPosition makeRowProfiles makeNominalData
#_____________________________________________________________________
supplementaryVariables4PLSCA <- function(var.sup, 
                                        make.var.sup.nominal = TRUE,
                                        resPLSCA,
                                        Xset     = NULL,
                                        make.Xset.nominal = TRUE,
                                        Yset     = NULL,
                                        make.Yset.nominal = TRUE,
                                        dimNames = 'Dimension '
){
  # To solve the problem of trying to project only one variable
  # because it will be treated as a row vector instead of
  # a column vector
  var.sup <- as.matrix(var.sup)
  if (make.var.sup.nominal) {
    var.sup <- ExPosition::makeNominalData(var.sup) }
  if ((is.null(Xset)) & (is.null(Yset))){
    warning(
'supplementaryVariables4PLSC needs at least one of "Xset" or "Yset"')
    print('Results will give only correlations with latent variables')      
  }
  
  #___________________________________________________________________
  if (nrow(var.sup) != nrow(resPLSCA$TExPosition.Data$lx)){
    stop('"var.sup" and "original data matrices" should have same number of rows')
  }
  return.list <- structure(list(),
                           class = 'supVar4PLSC')
  # 1. Project X-variables as sup
  # loadings.sup.x
  delta <- resPLSCA$TExPosition.Data$pdq$Dv
  Dd_1  <- diag(1/as.vector(delta))
  noms2col <- paste0(dimNames, 1:length(delta))
  # Currently not needed but may be in neext version
  #r <- resPLSCA$TExPosition.Data$W1
  #c <- 1/resPLSCA$TExPosition.Data$W2
  # loadings4Xsup ----
  if (!is.null(Yset)){# Get projection as Xset
    if (make.Yset.nominal){ 
       Yset <-  ExPosition::makeNominalData(Yset) }
    Fj <- resPLSCA$TExPosition.Data$fj 
    Ri_sup <- t(var.sup) %*% as.matrix(Yset)
    L_sup <- makeRowProfiles(Ri_sup)$rowProfiles
     # Reconstitution formula for Fi
     fii <-  L_sup    %*% Fj %*% Dd_1 
     # fii <- L_sup %*% # resPLSCA$TExPosition.Data$fj %*% diag(1/delta)
     #   (resPLSCA$TExPosition.Data$fj * # more efficient
     #      matrix(1/delta, nrow = nrow(resPLSCA$TExPosition.Data$fj),
     #      ncol = length(delta), byrow = TRUE) )
        # P & Q will need to be rewritten for the correct GSVD
        # with P %*% Dr_1 %*% P = I
        # here   P %*% Dr %*% P = I
      Psup <-  # fii %*% diag(1 / delta) # more efficient
                       fii * matrix(1/delta, nrow = nrow(fii),
                        ncol = length(delta), byrow = TRUE) 
    # Psup <- R_sup %*% diag(1/c) %*% 
    colnames(fii) <- noms2col
    colnames(Psup) <- noms2col
    return.list$loadings.sup.X <- Psup
    return.list$sup.fi         <- fii
  }
  
  if (!is.null(Xset)){# Get projection as Xset
    if (make.Xset.nominal){
      Xset <-  ExPosition::makeNominalData(Xset) }
    Fi <- resPLSCA$TExPosition.Data$fi 
    Rj_sup <-  t(as.matrix(Xset)) %*% var.sup
    C_sup <- makeRowProfiles(t(Rj_sup))$rowProfiles
    # Reconstitution formula for Fi
    fjj <-  C_sup    %*% Fi %*% Dd_1 
    #fjj <- C_sup %*% # resPLSCA$TExPosition.Data$fj %*% diag(1/delta)
    #  (resPLSCA$TExPosition.Data$fi * # more efficient
    #     matrix(1/delta, nrow = nrow(resPLSCA$TExPosition.Data$fi),
    #            ncol = length(delta), byrow = TRUE) )
    # P & Q will need to be rewritten for the correct GSVD
    # with P %*% Dr_1 %*% P = I
    # here   P %*% Dr %*% P = I
    Qsup <- # fjj %*% diag(1 / delta)
              fjj * matrix(1/delta, nrow = nrow(fjj),
                  ncol = length(delta), byrow = TRUE) 
    # Psup <- R_sup %*% diag(1/c) %*% 
    colnames(fjj) <- noms2col
    colnames(Qsup) <- noms2col
    return.list$loadings.sup.Y <- Qsup
    return.list$sup.fj         <- fjj
  }
  # cor.lx - ly ----
  cor.lx <- cor(var.sup, resPLSCA$TExPosition.Data$lx)
  cor.ly <- cor(var.sup, resPLSCA$TExPosition.Data$ly)
  colnames(cor.lx) <- noms2col -> colnames(cor.ly)
  return.list$cor.lx <- cor.lx
  return.list$cor.ly <- cor.ly
  
  return(return.list)
  #check
  #all(near(loadings.sup,resPLSC$TExPosition.Data$pdq$p[1:8,]))
  # works
} # End of supplementaryVariables4PLSC() ----


