# Description for file deadInTheUSA example for data4PCCAR
# Hervé Abdi: Apri 14, 2018.
#
#   deadInTheUSA
#  Preambule ----
#' @title The causes of death as a function of age in the USA in 2001.
#'
#' @description
#'  \code{deadInTheUSA}: provides a
#'  19 causes of death by 11 age-bins contingency table
#'  storing the number of persons who died for each
#'  cause for each age in the USA in 2001.
#'  This type of data is typically
#'  analyzed with correspondence analysis.
#'
#' @details The data have been cleaned and compacted
#' from the original data to make the data table more readable.
#' The cause \code{AD_PD} is the concatenation of
#' Alzheimer and Parkinson diseases.
#'
#' @name deadInTheUSA
#' @usage data("deadInTheUSA")
#' @docType data
#' @format
#' A list containing 1 data frame:
#' * \code{CT:} the contingency table
#' giving the number of death per age and cause.
#' @md
#' @keywords datasets data4PCCAR
#' @author Hervé Abdi
#' @references
#' Original data from
#' \emph{National Vital Stat Reports, 52(3)}, Sept 18.
NULL
# End of deadInTheUSA
#________________________________________
#_____________________________________________________________________
# Print function   ----
#_____________________________________________________________________
#_____________________________________________________________________
#' Change the print function for the data set:
#' \code{deadUSA}
#'
#' Change the print function for the data set:
#' \code{deadUSA}.
#'
#' @param x a list: the data set: \code{deadUSA}
#' @param ... the rest
#' @author Herve Abdi
#' @export
print.deadUSA <- function(x, ...){
  ndash = 78 # How many dashes for separation lines
  cat(rep("-", ndash), sep = "")
  cat("\n A list: The Causes of Death by Ages in the USA in 2001.\n")
  # cat("\n List name: ",deparse(eval(substitute(substitute(x)))),"\n")
  cat(rep("-", ndash), sep = "")
  cat("\n$CT ","A contingency table (df) of dimensions 19 causes of death by 11 age-bins")
  cat("\n",rep("-", ndash), sep = "")
  cat("\n")
  invisible(x)
} # end of function print.deadUSA
# end print.deadUSA ----
#_____________________________________________________________________
