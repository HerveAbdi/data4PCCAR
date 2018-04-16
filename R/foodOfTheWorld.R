# Description for file foodOfTheWorld example for data4PCCAR
# Hervé Abdi: April 14, 2018.
#
#  sortingSpices Preambule ----
#' @title 26 world cuisines are described by
#' their cooking ingredients.
#'
#' @description
#' \code{foodOfTheWorld}: 26 world cuisines are described by
#' their cooking ingredients. The long form of the data comprises
#' 159 ingredients. A shorter form includes the 68 most relevant
#' ingredients.
#'
#'@details
#' These data sets are derived from the work of blogger
#' \emph{Jyothi}
#' (see \code{https://www.r-bloggers.com/a-visualization-of-world-cuisines/}).
#' Here the main data
#' consist in a list with three main data frames:
#'  1) \code{CT}: a contingency table listing
#' the number of recipes (from the publicly available
#' data base \code{Epicurious}) that mention a given ingredient
#' for a given cuisine;
#' 2) \code{small.CT}
#' A short version is also provided that is
#' using only 68
#' (most discriminant) ingredients;
#' and 3) \code{small.CT}
#' a compact version
#' (that was given in the original post) with only 22
#' (most informative) ingredients.
#' Because the counts vary greatly,
#' the contingency tables are also re-scaled by taking the
#' logarithm of the contingency table [with log(0) set to 0].
#' The log tables are also available as a pseudo-integer version
#' (obtained by multiplying by 10 and rounding the "logged"
#' contingency table). The log rescaled tables are preceded
#' by the prefix \code{log}, the "log-rounded" tables
#' are, in addition,  followed by the suffix \code{.interger}
#'
#' The original post used these data  (the compact version)
#' to illustrate
#' principal component analysis, but because these data are count
#' \emph{correspondence analysis} is more appropriate.
#'
#' The large, small, and compact data sets give roughly the
#' same first dimension(s), whereas versions
#' with more deescriptors give
#' more signfificant/relevant dimensions than data sets with fewer
#' variables.
#'
#' @name foodOfTheWorld
#' @usage data("foodOfTheWorld")
#' @docType data
#' @format
#' A list containing 9 objects:
#'  1) \code{CT}: a 26 cuisines by 159 ingredients data frame
#'  containing the contingency table;
#'  2) \code{logCT}: a 26 cuisines by 159 ingredients
#'  data frame containing the "logged" contingency table;
#'  3) \code{logCT.integer}: a 26 cuisines by 159 ingredients
#'  data frame containing the rounded"logged" contingency table;
#'  4) \code{small.CT}: a 26 cuisines by 68 ingredients data frame
#'  containing the "small" contingency table;
#'  5) \code{small.logCT}: a 26 cuisines by 68 ingredients data frame
#'  containing the "small" "logged" contingency table;
#'  6) \code{small.logCT.integer}:
#'  a 26 cuisines by 68 ingredients data frame
#'  containing the "small" rounded "logged" contingency table;
#'  7) \code{compact.CT}: a 26 cuisines by 22 ingredients data frame
#'  containing the "compact" contingency table;
#'  8) \code{compact.logCT}: a 26 cuisines by 22 ingredients data frame
#'  containing the "compact" "logged" contingency table; and
#'  9) \code{compact.logCT.integer}:
#'  a 26 cuisines by 22 ingredients data frame
#'  containing the "compact" rounded "logged" contingency table;
#'  @keywords datasets data4PCCAR
#'  @author Herve Abdi & Jyothi
NULL
# End of foodOfTheWorld ----

#_____________________________________________________________________
# Print function foodOfTheWorld ----
#_____________________________________________________________________
#_____________________________________________________________________
#' Change the print function for the data set:
#' \code{foodOfTheWorld}
#'
#' Change the print function for the data set:
#' \code{foodOfTheWorld}
#'
#' @param x a list: the data set: {foodOfTheWorld}
#' @param ... the rest
#' @author Herve Abdi
#' @export
print.foodOfTheWorld <- function(x, ...){
  ndash = 78 # How many dashes for separation lines
  cat(rep("-", ndash), sep = "")
  cat("\n A list: 26 cuisines are described by the ingredients they use.  \n")
  # cat("\n List name: ",deparse(eval(substitute(substitute(x)))),"\n")
  cat(rep("-", ndash), sep = "")
  cat("\n$CT                    ","26 by 159 ingredients contingency table. ")
  cat("\nlogCT                  ","26 by 159 ingredients logged contingency table.")
  cat("\n$logCT.integer         ","26 by 159 ingredients rounded logged contingency table.")
  cat("\n$small.CT              ","26 by  68 ingredients contingency table. ")
  cat("\n$small.logCT           ","26 by  68 ingredients logged contingency table.")
  cat("\n$small.logCT.integer   ","26 by  68 ingredients rounded logged contingency table.")
  cat("\n$compact.CT            ","26 by  22 ingredients contingency table.")
  cat("\n$compact.logCT         ","26 by  22 ingredients logged contingency table.")
  cat("\n$compact.logCT.integer ","26 by  22 ingredients rounded logged contingency table.")
  cat("\n",rep("-", ndash), sep = "")
  cat("\n")
  invisible(x)
} # end of function print.foodOfTheWorld
# end print.foodOfTheWorld ----
#_____________________________________________________________________

#'
