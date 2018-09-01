# Description for file alcoholInEurope example for data4PCCAR
# Hervé Abdi: Septemeber 1, 2018.
#
#  alcoholInEurope
#  Preambule ----
#' @title The average consumption of 4 types of alcoholic beverages
#' in 22 European countries and 3 neighboring countries.
#'
#' @description
#' These data
#' give the amount in liters per year and per person older than 15
#' for 4 categories of alcoholic beverages:
#' (\code{Beer, Wine, Spirit, Other}) in 22 European countries and 3
#' neighboring countries.
#' These data were obtained from an article originally published
#'  in the journal
#' \emph{le Monde} dated September 25, 2018.
#'
#' @name alcoholInEurope
#' @usage data("alcoholInEurope")
#' @docType data
#' @format
#' A list containing 2 data frames:
#' * \code{df.active:} the 22 countries by
#' 4 types of alcoholic beverages active data set.
#' * \code{supplementary.observations:}
#' the 3 (non-EU but neighoring) countries by
#' 4 types of alcoholic beverages supplementary data set.
#' @md
#' @keywords datasets data4PCCAR
#' @author Hervé Abdi from Le Monde
#' @references
#' These data were obtained from the \emph{World Health Organization}
#' and were collected and presented in an article from the journal
#' \emph{le Monde} dated September 25, 2018. See the following link
#' for this article and details:
#'
#' \url{https://www.lemonde.fr/les-decodeurs/article/2018/08/24/quels-alcools-preferent-boire-les-europeens_5345933_4355770.html}
#
NULL
# End of alcoholInEurope

#_____________________________________________________________________
# Print function alcoholInEU  ----
#_____________________________________________________________________
#_____________________________________________________________________
#' Change the print function for the data set:
#' \code{alcoholInEU}
#' #'
#' Change the print function for the data set:
#' \code{alcoholInEU}.
#'
#' @param x a list: the data set: \code{alcoholInEU}
#' @param ... the rest
#' @author Herve Abdi
#' @export
print.alcoholInEU <- function(x, ...){
  ndash = 78 # How many dashes for separation lines
  cat(rep("-", ndash), sep = "")
  cat("\n A list: The alcohol consumption  (in liters per year per adult)")
  cat("\n         in 22 (EU) + 3 (non-EU)  countries.\n")
   # cat("\n List name: ",deparse(eval(substitute(substitute(x)))),"\n")
  cat(rep("-", ndash), sep = "")
  cat("\n$df.active                  ","The alcohol consumption in 22     (EU) countries.")
  cat("\n$supplementary.observations ","The alcohol consumption in  3 (non-EU) countries.")
  cat("\n",rep("-", ndash), sep = "")
  cat("\n")
  invisible(x)
} # end of function print.alcoholInEU
# end print.alcoholInEU ----
#_____________________________________________________________________
