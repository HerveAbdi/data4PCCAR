# Description for file twentyWines example for data4PCCAR
# Hervé Abdi: Apri 14, 2018.
#
#  winesOf3Colors Preambule ----
#' @title 36 wines are evaluated on several dimensions orgainzed in 3 blocks.
#'
#'
#' @description
#'  \code{winesOf3Colors}:
#' 36 wines are evaluated on several dimensions orgainzed in 3 blocks.
#'
#' @details The (fictitious) wines come form 3 countries
#' Argentina, Canada and the USA. They also come in three different
#' colors: Red, Rosé, and White. They are decribed by
#' they characteristic, chemistry and sensory properties

#'
#' @name winesOf3Colors
#' @usage data("winesOf3Colors")
#' @docType data
#' @format
#' A list with
#' one data frame with three blocks of data:
#' * \code{1} The descriptors.
#' * \code{2} The chemistry
#' * \code{3} The sensory
#' @md
#' @keywords datasets data4PCCAR
#' @author Hervé Abdi and Dominique Valentin
#' @references Thesedata have been used to illustrate
#' PLSC and CCA
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
  cat("\n A list: 36 wines  (Argentina, Canada, USA) are evaluated.\n")
  # cat("\n List name: ",deparse(eval(substitute(substitute(x)))),"\n")
  cat(rep("-", ndash), sep = "")
  cat("\n$winesDescriptors                ","The data.")
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
