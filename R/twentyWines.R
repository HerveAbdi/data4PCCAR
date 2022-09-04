# Description for file twentyWines example for data4PCCAR
# Hervé Abdi: Apri 14, 2018.
#
#   twentyWines Preambule ----
#' @title 20 red wines (10 French, 10 American) were rated by an expert on multiple dimensions.
#'
#' @description
#'  \code{twentyWines}:
#' 20 (imaginary) red wines (10 French, 10 American) are rated 
#' (by a semi-fictitious expert)
#' from 1 to 20,
#' on multiple sensory dimensions..
#'
#' @details The data are derived from real data but have been
#' "massaged"
#' to be nice (e.g., means, variances, eigenvalues, are integers).
#' The 10 French wines were a mix of
#' Carbernet-Sauvignon and Merlot grapes and the 10 American wines
#' were made from Zinfandel grapes.
#'
#' The wines were first evaluated on \code{Sugar} and
#' \code{Astringent}; these 2 dimensions are stored in the
#' data frame \code{df.active}.
#'
#' After the evaluation of the 20 wines was performed,
#' the expert was also
#' asked to evaluate (on the same dimensions) another red wine
#' (\emph{Trius}: a Cabernet-Merlot from Ontario, Canada). The results
#' for this wine are stored in the data frame:
#' \code{supplementary.observation}
#'
#' After the first evaluation of the 21 wines,
#'  the (semi-fictitious) expert rated the
#' wine on 2 other dimensions: \code{Acid} and \code{Bitter}.
#'
#' Incidently, the expert (even though semi-fictitious) spontaneously
#' commented that some wines were \code{Fruity} or \code{Woody}.
#'
#' The long names of the wines, the origin of the wines
#' (\code{F} for French and \code{U} for USA), the additional ratings,
#' and the comments are stored in the data frame
#' \code{supplementary.variables}.
#'
#' @name twentyWines
#' @usage data("twentyWines")
#' @docType data
#' @format
#' A list containing 3 data frames:
#' * \code{df.active} the ratings of the 20 wines for
#'    \code{Sugar} and \code{Astringent}.
#' * \code{supplementary.variables} stores the long names of the wines,
#' and the ratings of the 20 wines
#' for the *supplementary* variables
#' \code{Acid, Bitter, Fruity, Woody},
#' and the \code{Origin} of the wines (French vs USA).
#' * \code{supplementary.observation} the ratings for the
#' mystery wine (Trius Red: a wine from Ontario).
#' @md
#' @keywords datasets data4PCCAR
#' @author Hervé Abdi and Dominique Valentin
#' @references These ratings were (strongly) inspired by the
#' \emph{Words} example from:
#'
#'  Abdi, H., Edelman, B., Valentin, D., & Dowling, W.J. (2009).
#' \emph{Experimental Design and Analysis for Psychology}.
#' Oxford: Oxford University Press.
NULL
# End of twentyWines
#_____________________________________________________________________
# Print function twentyWines  ----
#_____________________________________________________________________
#_____________________________________________________________________
#' Change the print function for the data set:
#' \code{twentyWines}
#' #'
#' Change the print function for the data set:
#' \code{twentyWines}.
#'
#' @param x a list: the data set: {twentyWines}
#' @param ... the rest
#' @author Herve Abdi
#' @keywords internal
#' @export
print.twentyWines <- function(x, ...){
  ndash = 78 # How many dashes for separation lines
  cat(rep("-", ndash), sep = "")
  cat("\n A list: 20 red wines  (10 French, 10 USA) are rated by an expert.\n")
  # cat("\n List name: ",deparse(eval(substitute(substitute(x)))),"\n")
  cat(rep("-", ndash), sep = "")
  cat("\n$df.active                 ","Ratings for Sugar and Astringent.")
  cat("\n$supplementary.variables   ","Additional information on the wines.")
  cat("\n$supplementary.observation ","Ratings (Sugar and Astringent) for a new wine.")
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
