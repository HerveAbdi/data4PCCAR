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
# Preamble getBiplotCoor ----
#' @title Compute the biplot coordinates of variables
#' for a PCA computed with \code{ExPosition::epPCA()}.
#' 
#' @description \code{getBiplotCoor}
#' Computes the biplot coordinates of variables
#' for a PCA computed with \code{ExPosition::epPCA()}.
#' @param res.epPCA  the output of \code{ExPosition::epPCA()}
#' @param varLength the length of the variables.
#' When \code{NULL} (\code{Default}) \code{getBiplot} uses
#' the norm of the positive part of the vector.
#' @param axis1 horizontal axis (\code{Default: 1})
#' (used to compute the constraints).
#' @param axis2 vertical axis (\code{Default: 2})
#' (used to compute the constraints).
#' @return a list (of class \code{ 'coor4biplot'}) 
#' with 1)  \code{coordArrow} the coordinates of the arrows.
#' These coordinates can be used, for example, to 
#' plot these arrows on a graph created by 
#' \code{\link[PTCA4CATA]{createFactorMap}} 
#' and 2) \code{constraints4Biplot} the \code{constraints}
#' to be used for the maps (created, e.g., by 
#' \code{\link[prettyGraphs]{prettyPlot}}
#'  or \code{\link[PTCA4CATA]{createFactorMap}}). See also
#'  \code{\link[prettyGraphs]{minmaxHelper}}. 
#' 
#' @details \code{getBiplotCoor} projects the "biploted"
#' variables on the singular vectors (equivalent to a projection
#' as supplementary elements) and rescale the output
#' so that the variables have the correct length.
#' Note that only the positive part of the variables is kept.
#' 
#' 
#' @seealso
#' \code{\link{addArrowsAndNames}}
#' \code{\link[ExPosition]{epPCA}} 
#'  \code{\link[prettyGraphs]{minmaxHelper}} 
#' \code{\link[prettyGraphs]{prettyPlot}}  
#' \code{\link[PTCA4CATA]{createFactorMap}}
#' @importFrom prettyGraphs minmaxHelper
#' @author Hervé Abdi
#' @examples 
#' \dontrun{
#' data("twentyWines") # get the 20 wines data set
#' resPCA <- ExPosition::epPCA(twentyWines$df.active, 
#'                          scale = FALSE, graphs = FALSE) # PCA
#' getCoordinates <- getBiplotCoor(resPCA) # get biplot coordinates
#' # to plot them see help for addArrowsAndNames
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
constraints4Biplot <- lapply(
                 prettyGraphs::minmaxHelper(
                  res.epPCA$ExPosition.Data$fi[,c(axis1,axis2)], 
                  coordArrow[,c(axis1, axis2)]), '*', 1.11)
return.list = structure(
         list(coordArrow = coordArrow,
              constraints4Biplot = constraints4Biplot),
         class = 'coor4biplot')
return(return.list)
} # end of getBiplotCoor ----
#_____________________________________________________________________
# print coor4biplot ----
#
#' Change the print function for coor4biplot
#'
#'  Change the print function for coor4biplot
#'
#' @param x a list: output of \code{\link{getBiplotCoor}}
#' @param ... everything else for the functions
#' @author Hervé Abdi
#' @export
print.coor4biplot <- function(x, ...) {
  ndash = 78 # How many dashes for separation lines
  cat(rep("-", ndash), sep = "")
  cat("\n Projection of the (active) variables for a biplot PCA map \n")
  # cat("\n List name: ", deparse(eval(substitute(substitute(x)))),"\n")
  cat(rep("-", ndash), sep = "")
    cat("\n$coordArrow        : ", "The coordinates of the variables (arrows) for a biplot")
    cat("\n$constraints4Biplot: ", "The plotting constraints for the biplot map")
    cat("\n",rep("-", ndash), sep = "")
  cat("\n")
  invisible(x)
} # end of function print.coor4biplot ----
# _____________________________________________________________________

#
#       #should be a vector of Dimension J
# makeArrow = TRUE  # create the arrows for
#                   #  createFactorMap
#color4Arrow = "darkOliveGreen"# # c('#ffb3ff', '#ff884d') # default olivegreen
#axis1 = 1
#axis2 = 2
# Preamble addArrowsAndNames ----
#' @title Add arrows and names
#' to a factorial graph created by 
#' \code{\link[PTCA4CATA]{createFactorMap}}
#' and friends. 
#' @description \code{addArrowsAndNames}:
#' Add arrows and names
#' to a factorial graph created by  
#' \code{\link[PTCA4CATA]{createFactorMap}}
#' and friends. Use, for example, to create 
#' a \emph{biplot} by adding variables
#' to the observation map 
#' @param DATA a matrix or data frame with coordinates
#' of the items to be plotted
#'  (NB \emph{needs to have row names}).
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
#' @details To be used as an add-on for map
#' created by  \code{\link[PTCA4CATA]{createFactorMap}}.
#' Note that for strange reasons (i.e., \code{ggplot2} internals ....),
#' the returned list cannot be provided with a \code{class}
#' (and so the \code{print} function cannot give a help)
#' @seealso \code{\link{getBiplotCoor}}
#' @examples 
#' \dontrun{
#' data("twentyWines") # get the wine data set
#' resPCA <- ExPosition::epPCA(twentyWines$df.active, 
#'                             scale = FALSE, graphs = FALSE)
#' getCoordinates <- getBiplotCoor(resPCA) # get biplot coordinates
#' aMap <- PTCA4CATA::createFactorMap(resPCA$ExPosition.Data$fi, 
#'          constraints = getCoordinates$constraints4Biplot) # create base map
#' add2Map <- addArrowsAndNames(getCoordinates$coordArrow) # get the arrows
#' print(aMap$zeMap + add2Map) # print the biplot map
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
} # end addArrowAndNames ----
#
