# A work file for the rename dimension function
# for ExPosition
# HA September 15, 2019.
# Add names to the columns of the outputs of ExPosition
#

# # a clean start
# rm(list = ls())
# graphics.off()
# #
# library(ExPosition)
# library(data4PCCAR)
# # 


# expoRes   <- resPCA$ExPosition.Data
#_____________________________________________________________________
# function nameExpositionResults
#_____________________________________________________________________
# Helper for roxygen2 ----
#  install.packages('sinew')
#  install.packages('httpuv')
#  sinew::makeOxygen(nameExpositionResults)
#
#_____________________________________________________________________
#' @title add dimension names to the results from \code{ExPosition}
#' or \code{InPosition}
#' (for use with \code{ggplot2}).
#' @description \code{nameExpositionResults}:
#' adds dimension names to the results of analyses performed with 
#' \code{ExPosition} (column names are needed for plots
#' created with \code{ggplot2}).
#' @param resExpo the output of an analysis performed
#' by \code{ExPosition} with the functions:
#' \code{epCA, epPCA, epGPCA, epMCA, epMDS}.
#' @param names4Dimensions names for
#' the dimensions, Default: 'Dimension '
#' @return the original results with their dimensions 
#' named or renamed.
#' @seealso ExPosition
#' @details 
#' \code{ExPosition} returns the results of its analysis in a list
#' of matrices
#' that comprises the factor scores, the contributions, 
#' the cosines (etc. see documentation for \code{\link{ExPosition}}).
#' The columns of these statistics 
#' do not have names and this creates
#' a problem when using \code{ggplot2} whose
#' syntax  requires names for variables in \code{aes()}.
#' 
#' @author Hervé Abdi
#' @examples 
#' \dontrun{
#' if(interactive()){
#' # Get the data from package data4PCCAR
#' df <- twentyWines$df.active
#' resPCA <- epPCA(df, scale = FALSE, graphs = FALSE, k = 1)
#' resPCA.named <- nameExpositionResults(resPCA)
#'  }
#' }
#' @rdname nameExpositionResults
#' @export 
nameExpositionResults <- function(resExpo, 
              names4Dimensions  = 'Dimension '){
    expoRes   <- resExpo$ExPosition.Data
    if (!is.null(resExpo$Fixed.Data)){# results from InPosition
      resFromInPo <- TRUE
      expoRes <- resExpo$Fixed.Data$ExPosition.Data
    }
    nf      <- NCOL(expoRes$fi)
    nom2dim <- names4Dimensions
    noms2add <- paste0(nom2dim, 1:nf)
# Get the standard ones
colnames(expoRes$fi)     <- noms2add
colnames(expoRes$ci)     <- noms2add
colnames(expoRes$ri)     <- noms2add
colnames(expoRes$pdq$p)  <- noms2add
colnames(expoRes$pdq$q)  <- noms2add
colnames(expoRes$pdq$Dd) <- noms2add
rownames(expoRes$pdq$Dd) <- noms2add
names(expoRes$pdq$Dv)    <- noms2add
# special case get all dimensions
nf.eig        <- NCOL(expoRes$eigs)
noms2add.eig  <- paste0(nom2dim, 1:nf)
names(expoRes$eigs) <- noms2add.eig
names(expoRes$t)    <- noms2add.eig
# check that fj exists (mds special case)
if (!is.null(expoRes$fj)){
  colnames(expoRes$fj) <- noms2add
  colnames(expoRes$cj) <- noms2add
  colnames(expoRes$rj) <- noms2add
                         } # end of if
     if (isTRUE(resFromInPo)){
    names(resExpo$Inference.Data$components$p.vals)    <- noms2add
    colnames(resExpo$Inference.Data$components$eigs.perm) <- noms2add
    names(resExpo$Inference.Data$components$eigs) <- noms2add
    colnames(resExpo$Inference.Data$fj.boots$tests$sig.boot.ratios) <- noms2add
    colnames(resExpo$Inference.Data$fj.boots$tests$boot.ratios) <- noms2add
    dimnames(resExpo$Inference.Data$fj.boots$boots)[[2]] <- noms2add
       
    resExpo$Fixed.Data$ExPosition.Data <- expoRes 
       
     } else {
     resExpo$ExPosition.Data <- expoRes 
     }
     return(resExpo)
 } # end of function
#_____________________________________________________________________

# test ----
# # Test of the function below
# #Get the data
# # library(InPosition)
# # library(data4PCCAR)
# df <- twentyWines$df.active
# resPCA <- epPCA(df, scale = FALSE, graphs = FALSE, k = 1)
# resPCA.named <- nameExpositionResults(resPCA)
# resPCA.inf <- epPCA.inference.battery(df, scale = FALSE)
# toto <- nameExpositionResults(resPCA.inf)
# data(jocn.2005.fmri)
# #by default, components 1 and 2 will be plotted.
# mds.res.images <- epMDS(jocn.2005.fmri$images$data)
# mds.res.images.named <- nameExpositionResults(mds.res.images)
# # 
# data("foodInFrance")
# foodInFrance$df.active
# resPCA2 <- nameExpositionResults(
#          epPCA(foodInFrance$df.active, k = 2, graphs = FALSE))
#_____________________________________________________________________