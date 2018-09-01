# Description for file foodInFrance example for data4PCCAR
# Hervé Abdi: September 2, 2018.
#
#   foodInFrance
#  Preambule ----
#' @title How much typical French families spent on different types
#' of food in the 1950's.
#'
#' @description
#' \code{foodInFrance}: How much typical French families spent for food
#' in the 1950's. The families are categorized by social class
#' (manual workers, employees, and executives)
#' and by number of children
#' (from 2 to 5).
#' Interestingly (for historians of economics), at this
#' time, \code{Wine} was considered a food.
#'
#' @details The measurements give the amount in \emph{Francs}
#' spend by a typical (i.e., average) family on 7 items:
#' \code{ Bread, Vegetables, Fruits, Meat, Poultry, Milk, Wine}
#'
#' Because the unit (i.e., Francs spent) is the same all accross the table
#' (and a Franc is a Franc no matter what you buy),
#' this example illustrates the
#' case when the data should \emph{not} be normalized when
#' performing a PCA.
#'
#'@name foodInFrance
#' @usage data("foodInFrance")
#' @docType data
#' @format
#' A list containing 2 data frames:
#' * \code{df.active:} the data per se
#' * \code{supplementary.variables:} Additional information
#' long and short names and factors to expliciely identify
#' social class and number of children.
#' @md
#' @keywords datasets data4PCCAR
#' @author Hervé Abdi from Nicole Tabard via  Ludovic Lebart et al.
#' @references
#'  The original data were published in a (now unavailable)
#'  report:
#'
#'   Tabard, N. (1967). \emph{Les Condition de Vie des Familles}.
#'   Paris: UNCAF-CREDOC.
#'
#'   The concatenated data are given in
#'
#'    Lebart, L., Morinreau,, A., & Fenelon, J.P. (1982).
#'    \emph{Traitement des Données Statistiques}. Paris: Dunod.
#'
#'    These data are also used in:
#'
#'    Abdi, H, & Willaims, L.A. (2010).
#'    Principal component analysis.
#'    \emph{Wiley Interdisciplinary Reviews: Computational Statistics, 2},
#'    433-459.
#'
#'    and in:
#'
#'    Hardle, W., K., & Simar, L.  (2015) \emph{Applied Multivariate
#'    Statistical Analysis}. New York: Springer.
#'
NULL
# End of foodInFrance
#_____________________________________________________________________
# Print function foodInFrance  ----
#_____________________________________________________________________
#_____________________________________________________________________
#_____________________________________________________________________
#' Change the print function for the data set:
#' \code{foodInFrance}
#' #'
#' Change the print function for the data set:
#' \code{foodInFrance}.
#'
#' @param x a list: the data set: {foodInFrance}
#' @param ... the rest
#' @author Herve Abdi
#' @export
print.foodInFrance <- function(x, ...){
  ndash = 78 # How many dashes for separation lines
  cat(rep("-", ndash), sep = "")
  cat("\n A list: How much French families (from the 1950's) spent (in Francs) on food.\n")
  # cat("\n List name: ",deparse(eval(substitute(substitute(x)))),"\n")
  cat(rep("-", ndash), sep = "")
  cat("\n$df.active               ","A data frame: 12 typical families by 7 types of food.")
  cat("\n$supplementary.variables ","Long names and")
  cat("\n                         ","factors for social class and number of children.")
  cat("\n",rep("-", ndash), sep = "")
  cat("\n")
  invisible(x)
} # end of function print.foodInFrance
# end print.foodInFrance ----
#_____________________________________________________________________

#'
#'
#'
#'
