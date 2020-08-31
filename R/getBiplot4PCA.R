# function getBiplot ----
# Entête ----
# Compute the Biplot coordinates (as inverse rotation)
# for the ExPosition::epPCA function
# File created 08/30/2020. Hervé Abdi
# Current Version 08/30/2020. Hervé Abdi
#__________________________________________________________
# sinew ----
# install.packages('remotes')
# remotes::install_github('metrumresearchgroup/sinew')
# sinew::makeOxygen('getBiplotCoor')
#__________________________________________________________
#
#
# res.epPCA # the output of ExPosition::epPCA()
# The length of the variables
#' @title Compute the coordinates of variables
#' for a PCA computed with \code{ExPosition::epPCA()}.
#' Temporary version
#' @description \code{getBiplotCoor}
#' Compute the coordinates of variables
#' for a PCA computed with \code{ExPosition::epPCA()}.
#' @param res.epPCA  the output of \code{ExPosition::epPCA()}
#' @param varLength PARAM_DESCRIPTION, (\code{Default: NULL})
#' @param axis1 horizontal axis (\code{Default: 1})
#' (used to compute the constraints).
#' @param axis2 vertical axis (\code{Default: 2})
#' (used to compute the constraints).
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @seealso \code{\link[prettyGraphs]{minmaxHelper}}
#' @importFrom prettyGraphs minmaxHelper
#' @author Hervé Abdi
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname getBiplotCoor
#' @export
# function starts here
getBiplotCoor <-   function(res.epPCA, varLength = NULL,
                            axis1 = 1, axis2 = 2){
# res.epPCA   <- pca.res 
oriData <- res.epPCA$ExPosition.Data$X # preprocessed data
getLength <- function(x){# compute the length of the var
  sqrt( (sum((x*(x > 0))^2)) ) / 
     sqrt(length(x) / sum(x > 0))  }
if (!(is.null(varLength))){
  if (!is.vector(varLength)){
      stop('varLength should be a vector of length J')}
  if(length(varLength != ncol(oriData))){
    stop('varLength should be a vector of length J')}  
  varLength <- diag(varLength)
}
if (is.null(varLength)){
varLength <- diag(apply(oriData, 2, getLength )) } 
coordArrow <- varLength %*% res.epPCA$ExPosition.Data$pdq$q
rownames(coordArrow) <- rownames(res.epPCA$ExPosition.Data$fj)
colnames(coordArrow) <- paste0('Dimension ', 1:ncol(coordArrow))
constraints4Biplot <- prettyGraphs::minmaxHelper(
                  res.epPCA$ExPosition.Data$fi[,c(axis1,axis2)], 
                  coordArrow[,c(axis1, axis2)])
return.list = structure(
         list(coordArrow = coordArrow,
              constraints4Biplot = constraints4Biplot),
         class = 'coor4biplot')
return(return.list)
}

#       #should be a vector of Dimension J
# makeArrow = TRUE  # create the arrows for
#                   #  createFactorMap
#color4Arrow = "darkOliveGreen"# # c('#ffb3ff', '#ff884d') # default olivegreen
#axis1 = 1
#axis2 = 2
##' @title Add arrows and names
##' to a factorial graph created by \code{createFactorMap}
##' and friends. 
#' @description \code{addArrowsAndNames}:
#' Add arrows and names
#' to a factorial graph created by \code{createFactorMap}
#' and friends
#' @param DATA a matrix or data frame with coordinates
#' of the items to be plotted
#'  (NB \emph{need to have row names}).
#' @param axis1 horizontal axis (\code{Default: 1}).
#' @param axis2 vertical axis (\code{Default: 2}).
#' @param fontface font face for the names
#'  (\code{Default: 'bold.italic'}).
#' @param color an element or
#' a vector of color (\code{Default: 'darkolivegreen'}).
#' @return A list with two elements
#' 1) \code{arrows}  (\code{ggplot} code to add the arrows)
#' 2) \code{labels4Biplot} 
#' (\code{ggplot} code to add the names of the
#' items): Uses \code{ggplot2::annotate()}.
#' @details More here later
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @author Hervé Abdi
#' @rdname addArrowsAndNames
#' @export
addArrowsAndNames <- function(DATA,
                              axis1 = 1,
                              axis2 = 2,
                              fontface = 'bold.italic',
                              color = 'darkolivegreen'){ 
coordArrow <- DATA    
color4Arrow <- color
arrows4Biplot <- addArrows(coordArrow[,c(axis1, axis2)],
                            color = color4Arrow)
#names4Biplot <- 
#
labels4Biplot = annotate("text", 
           x = coordArrow[,axis1]*1.1,
           y = coordArrow[,axis2]*1.1,
           label = rownames(coordArrow),
           color = color4Arrow ,
           fontface = fontface)
# Strange problem with ggplot
# It accepts a list but not a structure
return.list = # structure(
          list(
          arrows = arrows4Biplot,
          labels4Biplot = labels4Biplot) #,
         #class = 'arrow4biplot')
return(return.list)
} # end makeArrow
#