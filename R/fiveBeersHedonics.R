# Description for file  fiveBeersHedonics example for data4PCCAR
# Hervé Abdi: September 1, 2018.
#
#  fiveBeersHedonics
#  Preamble ----
#' @title  Five tasters evaluated
#' how much they liked  (using a 10 point rating scale) five beers.
#'
#' @description
#'  \code{fiveBeersHedonics}:
#' five tasters evaluated how much they liked five beers
#' using a 10 point rating scale, with "1" meaning
#' \emph{completely disklike}
#' and "10" \emph{completely like}.
#'
#' In order to be analyzed with correspondence analysis,
#' these rating scale data
#' need to be recoded with what is called
#' \emph{fuzzy coding, doubling}, or
#' \emph{thermometer} coding.
#' With this code, each rating is re-expressed
#' as two variables: The first one expresses how far the rating
#' was from the low value of the scale and
#' the second one expresses how far the rating was
#' from the high value of the scale.
#' This way,
#' the original variable is now represented by a line
#' linking the low anchor of the scale to the high
#' anchor of the scale.
#'
#'  This data set provides the original data  and the "fuzzy-recoded"
#'  data.
#'
#' @name fiveBeersHedonics
#' @usage data("fiveBeersHedonics")
#' @docType data
#' @format
#' A list containing 2 data frames:
#' * \code{ratings:} A 5 by 5 data frame with
#' the original ratings performed using a 10 point rating scale.
#' * \code{fuzzyRatings:}
#' A 5 by 10 data frame with
#' the doubled ratings (with values between 0 and 1).
#' @md
#' @keywords datasets data4PCCAR
#' @author Hervé Abdi
#' @references
#' These data were obtained from a beer ratings article
#' published sometimes in the early 21st century by
#' the \emph{Dallas Morning News}.
#'
NULL
# End of fiveBeersHedonics
#_____________________________________________________________________
# Print function fiveBeersRank----
#_____________________________________________________________________
#_____________________________________________________________________
#' Change the print function for the data set:
#' \code{fiveBeersRank}
#' #'
#' Change the print function for the data set:
#' \code{fiveBeersRank}.
#'
#' @param x a list: the data set: \code{fiveBeersRank}
#' @param ... the rest
#' @author Herve Abdi
#' @export
print.fiveBeersRank <- function(x, ...){
  ndash = 78 # How many dashes for separation lines
  cat(rep("-", ndash), sep = "")
  cat("\n A list: 5 tasters rated (on a 10 point scale) how much they liked 5 beers.\n")
  # cat("\n List name: ",deparse(eval(substitute(substitute(x)))),"\n")
  cat(rep("-", ndash), sep = "")
  cat("\n$ratings      ","The 5*5 data frame with the original ratings.")
  cat("\n$fuzzyRatings ","The 5*10 data frame with the doubled ratings.")
  cat("\n",rep("-", ndash), sep = "")
  cat("\n")
  invisible(x)
} # end of function print.fiveBeersRank
# end print.fiveBeersRank----
#_____________________________________________________________________

