#_____________________________________________________________________
# firstpos4ExPo function to be stored in data4PCCAR
# HA: February 08, 2020.
# function in this file: firstpos4ExPo
#_____________________________________________________________________
#
#_____________________________________________________________________
# Helper for roxygen2 ----
#  install.packages('sinew')
#  sinew::makeOxygen(firstpos4ExPo)
#_____________________________________________________________________
# firstpos4ExPo
# A function to straighten the sign for ExPosition Results
# Based on `data4PCCAR::firstpos`
# now moved to `data4PCCAR`
#_____________________________________________________________________
#' @title Makes positive the first value of factor scores and
#' singular vectors from the output of functions from
#' the \code{ExPosition} package.
#'
#' @description \code{firstpos4ExPo}: makes positive the first element
#' of the column factor scores (i.e., \code{fj})
#' and column singular vectors (i.e., pdq$q) of  analyses
#' performed with the  \code{ExPosition} package.
#' \code{firstpos4ExPo} also insures that the sign of the column
#' factor scores and singular vectors agree with
#'   the row factors scores (i.e., \code{fi})
#' and singular vectors (i.e., pdq$p).
#'
#' The transformation implemented by \code{firstpos4ExPo}
#' makes it easier to compare results from different analysis
#' because the sign of the factor scores
#' and singular vectors is arbitrary (i.e., a singular vector
#' multiplied by -1 is still a singular vector).
#'
#' @param resFile The result of an analysis of a function
#' from the \code{ExPosition} package (e.g., \code{epPCA}, \code{epCA})
#' @return An \code{ExPosition} object with the
#' corrected results.
#' @details Note that only the results of the
#' computation are affected by the \code{firstpos4ExPo},
#' the graphics are *not* affected. So when using
#' \code{firstpos4ExPo} it is wise to set the option
#' \code{graphs = FALSE}.
#' Also, a similar
#' version of \code{firstpos4ExPo} for the \code{InPosition}
#' family is not yet available.
#' @author Hervé Abdi
#' @seealso  \code{\link{firstpos}},  \code{\link{renormInertiaExPo}}
#' @examples
#' \dontrun{
#' data("mtcars") # use the mtcars data set 
#' resPCA.mtcars <- firstpos4ExPo(epPCA(mtcars, graphs = FALSE, k = 3))
#' # Check that the first element of the column vectors is positive
#' resPCA.mtcars$resPCA.mtcars$ExPosition.Data$fj
#' }
#' @rdname firstpos4ExPo
#' @export


firstpos4ExPo <- function(resFile){# Based on data4PCCAR::firstpos
  if (attr(resFile,"class")[1] != 'expoOutput'){
    stop('firstpos4ExPo works only with ExPosition objects')
  }
  # Check for the specific problem with MCA
  # attr(resFile$ExPosition.Data, 'class')[1] == 'epMCA'
  #
  fjfi <- firstpos(resFile$ExPosition.Data$fj,
                   resFile$ExPosition.Data$fi)
  resFile$ExPosition.Data$fj <- fjfi$P
  resFile$ExPosition.Data$fi <- fjfi$Q
  QP  <- firstpos(resFile$ExPosition.Data$pdq$q,
                  resFile$ExPosition.Data$pdq$p)
  resFile$ExPosition.Data$pdq$q <- QP$P
  resFile$ExPosition.Data$pdq$p <- QP$Q

  return(resFile)
   } # End of function firstpos4ExPo
#_____________________________________________________________________
