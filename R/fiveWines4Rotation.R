# Description for file fiveWines4Rotation example for data4PCCAR
# Hervé Abdi: April 14, 2018.
# Revisited July 26, 2020. (add mysteryWine)
#
#  fiveWines4Rotation Preamble ----
#' @title 5 wines are described by sensory and chemistry variables.
#'
#' @description
#'  \code{fiveWines4Rotation}: Five
#' wines are described by two blocks of variables:
#' Sensory and Chemistry.
#'
#'
#' @details The five (rather fictitious) wines 
#' are described by two blocks
#' of variables corresponding (respectively) to
#' their  sensory and chemistry properties.
#' This data set has been used to illustrate (Varimax) rotation
#' for factor analytic methods and also in several papers 
#' about Partial Least Square Regression (PLSR).
#'
#' @name  fiveWines4Rotation
#' @usage data("fiveWines4Rotation")
#' @docType data
#' @format
#' A list with two matrices:
#' * \code{Xmat.Chemistry} The chemistry properties (4 variables)
#' * \code{Ymat.Sensory} The sensory properties (3 variables).
#' @md
#' @keywords datasets data4PCCAR
#' @author Hervé Abdi 
#' @references These data have been used to illustrate
#' PLSR and
#' rotations in factor analytic methods See, for example,
#' the papers available from 
#' \url{https://personal.utdallas.edu/~herve/}:
#' 
#' Abdi, H. (2010). Partial least square regression, 
#' projection on latent structure regression, PLS-Regression. 
#' \emph{Wiley Interdisciplinary Reviews: 
#' Computational Statistics, 2}, 97-106. 
#'   
#' Abdi, H. (2003). Factor rotations. 
#' In M. Lewis-Beck, A. Bryman, T. Futing (Eds): 
#' \emph{Encyclopedia for research methods for the social sciences}. 
#' Thousand Oaks (CA): Sage. 978-982.
#'
#'
NULL
# End of 
#_____________________________________________________________________
# Print function fiveWines4Rotation  ----
#_____________________________________________________________________
#_____________________________________________________________________
#' Change the print function for the data set:
#' \code{fiveWines4Rotation}
#' #'
#' Change the print function for the data set:
#' \code{fiveWines4Rotation}.
#'
#' @param x a list: the data set: \code{fiveWines4Rotation}
#' @param ... the rest
#' @author Hervé Abdi
#' @export
print.fiveWines4Rotation <- function(x, ...){
  ndash = 78 # How many dashes for separation lines
  cat(rep("-", ndash), sep = "")
  cat("\n A list: 5 wines are evaluated by their chemical and sensory properties.\n")
  # cat("\n List name: ", deparse(eval(substitute(substitute(x)))),"\n")
  cat(rep("-", ndash), sep = "")
  cat("\n$Xmat.Chemistry: ","The chemical data.")
  cat("\n$Ymat.Sensory  : ","The sensory data.")
  cat("\n",rep("-", ndash), sep = "")
  cat("\n")
  invisible(x)
} # end of function print.twentyWines
# end print.twentyWines ----
#_____________________________________________________________________

#'
#'
#'
#'
