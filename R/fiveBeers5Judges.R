# Entête ----
#  File for: fiveBeers5Judges example 
# for data4PCCAR
# Hervé Abdi: November 15, 2020.
#
# 
#  Preamble ----
# fiveBeers5Judges
#' @title  Five tasters evaluated
#' five beers on dimensions 
#' that they  had previously chosen.
#' These data can be used to illustrate,
#' for example, 
#' Multiple Factor Analysis (MFA) or Distatis.
#'
#' @description
#'  \code{fiveBeers5Judges}:
#' Five tasters evaluated
#' five beers on dimensions 
#' that they had previously chosen.
#' These data can be used to illustrate,
#' among others, 
#' Multiple Factor Analysis (MFA),
#' STATIS,  or Distatis.
#' 
#' The judges are named \code{J1}
#' to \code{J5}
#'
#' @name fiveBeers5Judges
#' @usage data("fiveBeers5Judges")
#' @docType data
#' @format
#' A list containing one data frame and several vectors:
#' * \code{ratings:} A 5 by 26 data frame with
#' the original ratings performed using a 
#' 5 point Likert rating scale with
#' the beers being rows and the evaluations
#' by the judges being the columns.
#' * \code{judgeVector: }
#' A vector indicating what judge was rating what variable.
#' A 5 by 26 data frame with
#' * \code{Var4Judges: } a vector storing
#' the number of variables per judge.
#' * \code{namesOfJudges: } A vector storing
#' the names of the judges.
#'  * \code{color4Products: } A vector storing
#'  the color names for the beers.
#'  * \code{color4Judges: } A vector storing
#'  the color names for the judges. 
#' @md
#' @keywords datasets data4PCCAR
#' @author Hervé Abdi
#' @references
#' These data were used in a couple of workshops
#' for the SPISE meeting.
#'
NULL
# End of fiveBeers5Judges ----
#_____________________________________________________________________
# Print function fiveBeers5Judges----
#_____________________________________________________________________
#_____________________________________________________________________
#' Change the print function for the data set:
#' \code{fiveBeers5Judges}
#' #'
#' Change the print function for the data set:
#' \code{fiveBeers5Judges}.
#'
#' @param x a list: the data set \code{fiveBeers5Judges}.
#' @param ... the rest.
#' @author Hervé Abdi
#' @export
print.fiveBeers5Judges <- function(x, ...){
  ndash = 78 # How many dashes for separation lines
  cat(rep("-", ndash), sep = "")
  cat("\n A list: 5 Judges evaluated  5 beers with scales of their choice.\n")
  # cat("\n List name: ",deparse(eval(substitute(substitute(x)))),"\n")
  cat(rep("-", ndash), sep = "")
  cat("\n$ratings        ","The 5 beers * 26 scales data frame with the ratings.")
  cat("\n$judgeVector    ","What judge was rating what scale.")
  cat("\n$nVar4Judges    ","Number of variables per judge.")
  cat("\n$namesOfJudges  ","Names of the judges.")
  cat("\n$color4Products ","Colors for the beers.")
  cat("\n$color4Judges   ","Colors for the judges.")
  cat("\n",rep("-", ndash), sep = "")
  cat("\n")
  invisible(x)
} # end of function print.fiveBeersRank
# end print.fiveBeersRank----
#_____________________________________________________________________

