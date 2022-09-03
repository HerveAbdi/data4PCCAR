# Description for file twentyWines example for data4PCCAR
# Hervé Abdi: April 14, 2018.
# Revisited July 26, 2020. (add mysteryWine)
#
#  winesOf3Colors Preambule ----
#' @title 36 plus 1 wines are evaluated on several dimensions organized in 4 blocks.
#'
#' @description
#'  \code{winesOf3Colors}:
#' 36 wines are evaluated on several dimensions organized in 4 blocks.
#' An additional mystery wine is provided 
#' (to be projected as supplementary observation).
#'
#' @details The (fictitious) wines come from 3 countries
#' Argentina, Canada, and the USA. They also come in three different
#' colors: Red, Rosé, and White. They are described by
#' their characteristics, chemistry, and sensory properties.
#' The mystery wine is a French red pinot noir that
#' could be though as a "\emph{Bourgogne Rouge}".

#'
#' @name winesOf3Colors
#' @usage data("winesOf3Colors")
#' @docType data
#' @format
#' A list with
#' one data frame with four blocks of data:
#' * \code{1} The descriptors (origin, color, varietal): columns  1 to 3.
#' * \code{2} The price (in Dollars): column 4.
#' * \code{3} The chemistry properties: columns 5 to 8
#' * \code{4} The sensory properties: columns 9 to 17
#' @md
#' @keywords datasets data4PCCAR
#' @author Hervé Abdi and Dominique Valentin
#' @references These data have been used to illustrate
#' PLSC and CCA. See, e.g.,
#' (papers available from \url{https://personal.utdallas.edu/~herve/})
#' 
#'  Abdi H., Eslami, A., & Guillemot, V. (2018). 
#'  Canonical correlation analysis (CCA). 
#'  In R. Alhajj and J. Rokne (Eds.), 
#'  \emph{Encyclopedia of Social Networks and Mining (2nd Edition)}.
#'   New York: Springer Verlag.
#'   
#' Abdi, H., & Williams, L.J. (2013). 
#' Partial least squares methods: 
#' Partial least squares correlation 
#' and partial least square regression.
#'  In: B. Reisfeld & A. Mayeno (Eds.), 
#'  \emph{Methods in Molecular Biology: Computational Toxicology}. 
#' New York: Springer Verlag. pp. 549-579.
#'
#'
NULL
# End of winesOf3Colors
#_____________________________________________________________________
# Print function winesOf3Colors  ----
#_____________________________________________________________________
#_____________________________________________________________________
#' Change the print function for the data set:
#' \code{wines3Colors}
#' #'
#' Change the print function for the data set:
#' \code{wines3Colors}.
#'
#' @param x a list: the data set: {wines3Colors}
#' @param ... the rest
#' @author Herve Abdi
#' @export
print.wines3Colors <- function(x, ...){
  ndash = 78 # How many dashes for separation lines
  cat(rep("-", ndash), sep = "")
  cat("\n A list: 36 wines  (Argentina, Canada, USA) + 1 (French wine) are evaluated.\n")
  # cat("\n List name: ",deparse(eval(substitute(substitute(x)))),"\n")
  cat(rep("-", ndash), sep = "")
  cat("\n$winesDescriptors                ","The data.")
  cat("\n$mysteryWine                     ","supplementary wine.")
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
