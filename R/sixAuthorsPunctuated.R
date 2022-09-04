# Description for file sixAuthorsPunctuated example for data4PCCAR
# Hervé Abdi: April 14, 2018.
#
#   sixAuthorsPunctuated
#  Preambule ----
#' @title The punctuation used by six classical French Writers
#'
#' @description
#'  \code{sixAuthorsPunctuated}:
#'  The punctuation marks (comma, period, and "other marks")
#'  used by six French authors Rousseau,
#'  Chateaubrian, Hugo, Zola, Proust, Giraudoux.
#'  This set is used to illustrate Correspondence Analysis.
#'
#' @details
#' The punctuation by the six authors constitutes the "active set"
#' in correspondence analysis: it is stored
#' in the data frame: \code{df.active}.
#'
#' The category \code{Other} is the sum of 4 punctation marks:
#' \code{Exclam, Question, SemiCol, Colon} whose distribution
#' is stored in the data frame   \code{supplementary.variables}.
#'
#' To illustrate the projection of supplementary onservations,
#' the data set also contains the punctutation from
#' Chapter 1 of \emph{Les Réseaux de Neurones} written
#' by Hervé Abdi. These data are stored in the data frame:
#' \code{supplementary.observation}.
#'
#' @name sixAuthorsPunctuated
#' @usage data("sixAuthorsPunctuated")
#' @docType data
#' @format
#' A list containing 3 data frames:
#' * \code{df.active:} the punctuation by the six authors
#' * \code{supplementary.variables:} gives the details of the punctuation
#' marks included the \code{Other} category:
#' ' \code{Exclam, Question, SemiCol, Colon}
#' * \code{supplementary.observation:}
#' The punctuation marks used in
#' Chapter 1 of \emph{Les Réseaux de Neurones} written
#' by Hervé Abdi
#' @md
#' @keywords datasets data4PCCAR
#' @author Hervé Abdi from E. Brunet
#' @references
#' The active data and supplementary variables come form:
#'
#' Brunet E. (1989). Faut-il pondérer les données linguistiques?.
#' \emph{CUMFID}, 1989, 16:39–50.
#'
#' These data are used in several papers illustrating
#' correspondence analysis such as:
#'
#' Abdi H, Williams LJ. (2010). Correspondence analysis. In:
#' Salkind NJ, ed.
#' \emph{Encyclopedia  of  Research  Design}
#' Thousand Oaks: Sage Publications.
#'
NULL
# End of sixAuthorsPunctuated
#_____________________________________________________________________
# Print function twentyWines  ----
#_____________________________________________________________________
#_____________________________________________________________________
#' Change the print function for the data set:
#' \code{sixAuthors}
#' #'
#' Change the print function for the data set:
#' \code{sixAuthors}.
#'
#' @param x a list: the data set: \code{sixAuthors}
#' @param ... the rest
#' @author Herve Abdi
#' @keywords internal
#' @export
print.sixAuthors <- function(x, ...){
  ndash = 78 # How many dashes for separation lines
  cat(rep("-", ndash), sep = "")
  cat("\n A list: The punctuation of six authors.\n")
  # cat("\n List name: ",deparse(eval(substitute(substitute(x)))),"\n")
  cat(rep("-", ndash), sep = "")
  cat("\n$df.active                 ","The number of punctuation marks in the oeuvre of 6 writers")
  cat("\n$supplementary.variables   ","The detail of the category OTHER.")
  cat("\n$supplementary.observation ","The number of punctuation marks in a Chapter of H. Abdi")
  cat("\n",rep("-", ndash), sep = "")
  cat("\n")
  invisible(x)
} # end of function print.sixAuthors
# end print.sixAuthors ----
#_____________________________________________________________________
