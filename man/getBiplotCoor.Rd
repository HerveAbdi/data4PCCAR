% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getBiplot4PCA.R
\name{getBiplotCoor}
\alias{getBiplotCoor}
\title{Compute the biplot coordinates of variables
for a PCA computed with \code{ExPosition::epPCA()}.}
\usage{
getBiplotCoor(res.epPCA, varLength = NULL, axis1 = 1, axis2 = 2)
}
\arguments{
\item{res.epPCA}{the output of \code{ExPosition::epPCA()}}

\item{varLength}{the length of the variables.
When \code{NULL} (\code{Default}) \code{getBiplot} uses
the norm of the positive part of the vector.}

\item{axis1}{horizontal axis (\code{Default: 1})
(used to compute the constraints).}

\item{axis2}{vertical axis (\code{Default: 2})
(used to compute the constraints).}
}
\value{
a list (of class \code{ 'coor4biplot'})
with 1)  \code{coordArrow} the coordinates of the arrows.
These coordinates can be used, for example, to
plot these arrows on a graph created by
\code{\link[PTCA4CATA]{createFactorMap}}
and 2) \code{constraints4Biplot} the \code{constraints}
to be used for the maps (created, e.g., by
\code{\link[prettyGraphs]{prettyPlot}}
or \code{\link[PTCA4CATA]{createFactorMap}}). See also
\code{\link[prettyGraphs]{minmaxHelper}}.
}
\description{
\code{getBiplotCoor}
Computes the biplot coordinates of variables
for a PCA computed with \code{ExPosition::epPCA()}.
}
\details{
\code{getBiplotCoor} projects the "biploted"
variables on the singular vectors (equivalent to a projection
as supplementary elements) and rescale the output
so that the variables have the correct length.
Note that only the positive part of the variables is kept.
}
\examples{
\dontrun{
data("twentyWines") # get the 20 wines data set
resPCA <- ExPosition::epPCA(twentyWines$df.active, 
                         scale = FALSE, graphs = FALSE) # PCA
getCoordinates <- getBiplotCoor(resPCA) # get biplot coordinates
# to plot them see help for addArrowsAndNames
}
}
\seealso{
\code{\link{addArrowsAndNames}}
\code{\link[ExPosition]{epPCA}}
\code{\link[prettyGraphs]{minmaxHelper}}
\code{\link[prettyGraphs]{prettyPlot}}
\code{\link[PTCA4CATA]{createFactorMap}}
}
\author{
Hervé Abdi
}
