# Documentation file for 
# data set blindSortingWines
# for data4PCCAR
# Hervé Abdi: April 06, 2021.
# Data to be used for Chapter MDS for SAGE
# 
#  Préambule ----
# blindSortingWines
#' @title Wine experts and panelists
#' (blind) sorted red, rosé, and white wines.
#' These data can be used for 
#' multidimensional scaling (MDS or DISTATIS).
#' 
#' @description
#' \code{blindSortingWines}
#' 26 wine Experts and 19 Panelists
#' blind sorted 18 wines 
#' (6 red, 6 rosé, and 6 white).
#' 
#' The 26 Experts were told to sort the
#' wines into three groups whereas the
#' Panelists just told to make as many groups 
#' as they wished but less the 18 and more than 1.
#' 
#' @name blindSortingWines
#' @usage data("blindSortingWines")
#' @docType data
#' @format
#' A list containing 
#' five data frames
#' * \code{wineInformation: } a df
#' describing the 18 wines. The colors
#' are coded \code{P, R, W} 
#' (Rosé, Red, and White).
#' * \code{distance.Experts:} the 18 by  18
#' distance matrix derived from the
#' sorting of the 26 wine Experts. 
#' * \code{distance.Panelists:} the 18 by  18
#' distance matrix derived from the
#' sorting of the 26 wine Panelists.
#' * \code{df.Experts:} The
#' 18 wines (rows) by 
#' 26 wine Experts (columns)
#' data frame storing the results of the
#' sorting task by the wine
#' Experts (from \code{S1} to \code{S26}).
#' Here two wines (rows) 
#' with the same number were
#' placed in the same group by the 
#' Expert
#' (columns).
#' * \code{df.Experts:} The
#' 18 wines (rows) by 19
#'  wine Panelists (columns)
#' data frame storing the results of the
#' sorting task by the wine
#' Panelists (from \code{J1} to \code{J19}).
#' Here two wines (rows) 
#' with the same number were
#' placed in the same group by the 
#' Panelist
#' (columns).
#' @md
#' @keywords datasets data4PCCAR 
#' @author Jordi Ballester, Dominique Valentin, &
#' Hervé Abdi 
#' @references
#' These data (and the story around) 
#' are described in more details in
#' Ballester, J., Abdi, H., 
#' Langlois, J., Peyron, D., 
#' & Valentin, D. (2009). 
#' The odor of colors: 
#' Can wine experts and novices
#'  distinguish the odors of 
#'  white, red, and rosé wines?
#'  *Chemosensory Perception*, **2**, 203-213. 
#' 
NULL
# End of blindSortingWines ----
#_____________________________________________________________________
# Print function blindSortingWines----
#_____________________________________________________________________
#_____________________________________________________________________
#' Change the print function for the data set:
#' \code{blindSortingWines}
#' #'
#' Change the print function for the data set:
#' \code{blindSortingWines}.
#'
#' @param x a list:
#'  the data set \code{blindSortingWines}.
#' @param ... the rest.
#' @author Hervé Abdi
#' @export
print.blindSortingWines <- function(x, ...){
  ndash = 78 # How many dashes for separation lines
  cat(rep("-", ndash), sep = "")
  cat("\n A list: wine experts and novices sorted red, rose, and white wines.\n")
  # cat("\n List name: ",deparse(eval(substitute(substitute(x)))),"\n")
  cat(rep("-", ndash), sep = "")
  cat("\n$wineInformation    ","The names and description of the 18 wines.")
  cat("\n$distance.Experts   ","The distance matrix for the 26 experts.")
  cat("\n$distance.Panelists ","The distance matrix for the 19 novices.")
  cat("\n$df.Experts         ","The actual sorting of the 26 experts.")
  cat("\n$df.Panelists       ","The actual sorting of the 19 novices.")
  cat("\n",rep("-", ndash), sep = "")
  cat("\n")
  invisible(x)
} # end of function print.blindSortingWines
# end print.blindSortingWines----
#_____________________________________________________________________


