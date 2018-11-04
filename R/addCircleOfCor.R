# functions in this file:
#  addCirlceOfCor
#  addArrows
#_____________________________________________________________________
# Helper for roxygen2 ----
#  install.packages('sinew')
#  sinew::makeOxygen(addCircleOfCor)
#
#_____________________________________________________________________
# addCircleOfCor()  -----
#' @title Add a Circle of Correlation to a PCA-like map
#' of correlation produced by \code{createFactorMap()} \emph{et alia.}
#' @description
#' \code{addCircleOfCor}:
#' Add a Circle of Correlation to a PCA-like map
#' of correlation produced by \code{createFactorMap} \emph{et alia}.
#'
#' @param color  (\code{Default: 'darkorchid'}) color for the circle.
#' @param alpha  (\code{Default: 0.3}) transparency for the circle,
#' should be between 0 (completely transparent) and 1
#' (no transparent).
#' @param size (\code{Default: 1}) thickness of the line of the circle.
#' @param center (\code{Default: c(0, 0)}) center of the circle.
#' @param radius (\code{Default: 1}) radius of the circle.
#' @param nPoints (\code{Default: 100}) the number of points used to
#' draw the circle.
#' @return nothing
#' @details The map should should be first created by, for example,
#' \code{createFactorMap()} (or equivalent functions from
#' \code{PTCA4CATA}), and then the circle of correlation is added
#' (see example).
#' @import ggplot2
#' @author Herve Abdi
#' @seealso \code{\link[PTCA4CATA]{createFactorMap}}
#' @examples
#' \dontrun{
#' # Some PCA-like correlations
#' corXY <- matrix(c(.5,-.5, .1,.7, .8,.5, -.1,.9,  -.6,-.6),
#'                ncol = 2, byrow = TRUE )
#' # create a map of correlation
#' MapCor <- PTCA4CATA::createFactorMap(corXY,
#'          constraints = list(minx = -1, miny = -1,
#'                             maxx = 1 , maxy = 1) )
#' # Add a circle to the base Map
#' ggMapWithCircle <- MapCor$zeMap  + addCircleOfCor()
#' # To print the map with the circle:
#' # print(ggMapWithCircle)
#' }
#' @rdname addCircleOfCor
#' @export
#_____________________________________________________________________
# end addCirlceOfCor
#_____________________________________________________________________
addCircleOfCor <- function(color = 'darkorchid', # color of the circle
                           alpha = .3,
                           size  = 1,
                           center = c(0,0),
                           radius = 1,
                           nPoints = 100){
  # first an internal function
  #___________________________________________________________________
  # begin circleFun here
  .circleFun <- function(center,
                        radius,
                        npoints){
    tt <- seq(0,2*pi,length.out = npoints)
    xx <- center[1] + radius * cos(tt)
    yy <- center[2] + radius * sin(tt)
    return(data.frame(x = xx, y = yy))
  } # end private circleFun
  #___________________________________________________________________
  dat <- .circleFun(center, radius, npoints = nPoints)
  x <- y <- NULL # needed to appease the parser that
                 # thinks that x and y are glocal undefined variables
  aCircle <- geom_path(data = dat, mapping = aes(x = x, y = y),
                       color = color , alpha = alpha, size = size)
  return(aCircle)

} # end addCircleOfCor ----
#_____________________________________________________________________

#_____________________________________________________________________
# addArrows Preamble -----
#_____________________________________________________________________
# addArrows: How to add lines/arrows to a circle of correlation
# Add arrows to a plot
#  install.packages('sinew')
#  sinew::makeOxygen(addArrows)
#
#' @title Add arrows to a factorial map
#' (best with a circle of Correlation).
#'
#' @description  \code{addArrows}: Add arrows to a factorial map
#' (i.e., a PCA-like map),
#' most likely  a plot of correlation produced by
#' \code{createFactorMap} \emph{et alia}.
#'
#' @param X a data frame with the coordinates of the points
#' (e.g., from \code{ExPosition::epCA()}).
#' @param axis1  (\code{default} = 1)
#'    The number of the column of \code{X} for the horizontal axis.
#' @param axis2 (\code{default} = 2)
#' The number of the column of \code{X} for the vertical  axis.
#' @param color
#' the color of the arrows. Can be one color or a vector of colors.
#' If \code{color} is a vector, it needs to have exactly as many
#' elements as
#' the number of arrows  to plot.
#' \code{Default = 'darkorchid'}.
#' @param alpha (Default: 0.6),
#' the alpha (transparency) for the arrows, should be between 1
#' (no transparency) and 0 (completely transparent).
#' @param center  (\code{Default: c(0, 0)}) the center of the graph
#' @param arrowLength (\code{Default: 0.3}), the lenth (in cm.) of
#' the terminal arrow.
#' @param size (\code{Default = 1}), the thickness of the
#' segment
#' @return a \code{ggplot2} component to be added to a
#' scatterplot / map (typically created by
#' \code{createFactorMap()}).
#' @details The map should should first be created by, for example,
#' \code{createFactorMap()} (or equivalent functions from
#' \code{PTCA4CATA}), and then the arrows are added
#' (see example).  \code{addArrows} is typically used
#' with \code{\link{addCircleOfCor}}.
#' @seealso
#'  \code{\link[ggplot2]{annotate}} \code{\link{addCircleOfCor}} \code{\link[PTCA4CATA]{createFactorMap}}
#' @examples
#' \dontrun{
#' # Some PCA-like correlations
#' corXY <- matrix(c(.5,-.5, .1,.7, .8,.5, -.1,.9,  -.6,-.6),
#'                ncol = 2, byrow = TRUE )
#' # create a map of correlation
#' MapCor <- createFactorMap(corXY,
#'          constraints = list(minx = -1, miny = -1,
#'                             maxx = 1 , maxy = 1) )
#' # Add arrows and a circle to the base Map
#' ggMapWithCircle <- MapCor$zeMap +  addArrows(corXY) + addCircleOfCor()
#' # To print the map with arrows and circle:
#' # print(ggMapWithCircle)
#' }
#' @rdname addArrows
#' @export
#' @importFrom ggplot2 annotate
addArrows <- function(X, axis1 = 1, axis2 = 2,
                      color  = 'darkorchid',
                      alpha  = .6,
                      size = 1,
                      center = c(0,0),
                      arrowLength = .3){
  X = data.frame(X)
  zeArrows <- ggplot2::annotate("segment", x = center[1], y = center[2],
                                xend = X[,axis1],
                                yend = X[,axis2],
                                color = color,
                                alpha = alpha,
                                size = size,
                                arrow = arrow(length = unit(.3, "cm") ) )
  return(zeArrows)
}
# End addArrows ----
#_____________________________________________________________________
