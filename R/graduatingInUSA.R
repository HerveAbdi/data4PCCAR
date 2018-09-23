# Description for file  graduatingInUSA example for data4PCCAR
# Hervé Abdi: September 3, 2018.
#
#  graduatingInUSA
#  Preambule ----
#' @title Number of University Graduates in the USA from 1966 to 2001.
#'
#' @description
#'  \code{graduatingInUSA}: A series of contingency tables
#'  giving the number of graudates in the USA from 1966 to 2001.
#'  The grand contingency tables gives all the graduates by fields
#'  and by year. the partial tables subset the grand tables
#'  by gender (women vs men) and by lvel of graduation
#'  (bachelor, master, and PH.D.).
#'
#' @name graduatingInUSA
#' @usage data("graduatingInUSA")
#' @docType data
#' @format
#' A list containing 7 data frames:
#' * \code{df.all} the grand contingency table
#'  * \code{menBachelor}: the contingency table for men bachelors
#'   * \code{menMaster:} the contingency table for men maters
#'    * \code{menPhd} the contingency table for men Ph.D.
#'  * \code{womenBachelor}: the contingency table for women bachelors
#'   * \code{womenMaster:} the contingency table for women maters
#'    * \code{womenPhd} the contingency table for women Ph.D.
#' @md
#' @keywords datasets data4PCCAR
#' @author Hervé Abdi
#' @references
#' I could not find the exact source of these data but they came
#' from some official agency.
NULL
# End of graduatingInUSA
#_____________________________________________________________________
# Print function graduatingUSA----
#_____________________________________________________________________
#_____________________________________________________________________
#' Change the print function for the data set:
#' \code{graduatingUSA}
#' #'
#' Change the print function for the data set:
#' \code{graduatingUSA}.
#'
#' @param x a list: the data set: \code{graduatingUSA}
#' @param ... the rest
#' @author Herve Abdi
#' @export
print.graduatingUSA <- function(x, ...){
  ndash = 78 # How many dashes for separation lines
  cat(rep("-", ndash), sep = "")
  cat("\n A list: Number of University Graduates in the USA from 1966 to 2001 \n")
  # cat("\n List name: ",deparse(eval(substitute(substitute(x)))),"\n")
  cat(rep("-", ndash), sep = "")
  cat("\n$df.all        ","The 34 years * 8 fields grand contingemcy table.")
  cat("\n$menBachelor   ","The 34 years * 8 fields contingency table: men Bachelors.")
  cat("\n$menMaster     ","The 34 years * 8 fields contingency table: men Masters.")
  cat("\n$menPhd        ","The 34 years * 8 fields contingency table: men Ph.D.")
  cat("\n$womenBachelor ","The 34 years * 8 fields contingency table: women Bachelors.")
  cat("\n$womenMaster   ","The 34 years * 8 fields contingency table: women Masters.")
  cat("\n$womenPh.D.    ","The 34 years * 8 fields contingency table: women Ph.D.")

  cat("\n",rep("-", ndash), sep = "")
  cat("\n")
  invisible(x)
} # end of function print.graduatingUSA
# end print.graduatingUSA'----
#_____________________________________________________________________

