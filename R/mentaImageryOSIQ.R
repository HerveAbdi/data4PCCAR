# Description for file mentalImageryOSIQ example for data4PCCAR
#  and for csvd paper
# Hervé Abdi: October 1, 2018.
#
#  mentalImageryOSIQ
#  Preambule ----
#' @title (simulated) results of 2,100 participants answering
#' a Likert scale like questionnaire about mental imagery
#' (called the Object-Spatial Imagery Questionnaire: OSIQ).
#'
#' @description
#' \code{mentalImageryOSIQ} a data set with
#' (simulated) results of 2,100 participants answering
#' a Likert scale like questionnaire comprising
#' 30 questions about mental imagery
#' (called the Object-Spatial Imagery Questionnaire: OSIQ).
#' Half of the questions concern mental imagery for object and
#' the other half of the questions
#' concern mental imagery for spatial locations.
#'
#' @details
#' These data are simulated data that roughly match some real data.
#' Details about the questionnaire and for the actual questions
#' see the paper by Blajenkova et al. (2006).
#' The order of the questions
#'  is the same as in the original paper.
#'
#'  The answers to the questions
#'  were given on a Likert type scale from 1 to 5. One question
#'  (\code{s27}) has been reversed coded so that a large number
#'  indicates a good memory.
#'
#'  These data were created to mimic the structure of real data and
#'  were created with
#'  the function \code{\link{buildRandomImage4PCA}}.
#'
#'
#' @name mentalImageryOSIQ
#' @usage data("mentalImageryOSIQ")
#' @docType data
#' @format
#' a list containing one
#' data frame (\code{mentalImageryOSIQ$OSIQ})
#' with 2,100 rows (observations)
#' and 30 columns (questions). The first letter
#' of a column denotes the type of imagery
#' (\code{s} for spatial and \code{o} for object).
#' The questions are
#' in the same order as in the original publication
#' by Blajenkova et al. (2006). The first letter
#' of the name of a row can be \code{H, M, L}.
#' This letter indicates that the participants identified
#' themselves as having \code{H}igh or superior episodic memory,
#' \code{M}edium (when they did not mention high nor low), or
#' \code{L}ow episodic memory.
#' @md
#' @keywords datasets data4PCCAR
#' @author Hervé Abdi & Brian Levine
#' @references
#' The original OSIQ questionnaire can be found in:
#'
#' Blajenkova, O.,
#' Kozhevnikov, M., & Motes, M.A. (2006).
#' Object-spatial imagery: a new self-report imagery questionnaire.
#' \emph{Applied Cognitive Psychology, 20}, 239–263.
#'
#' These (simulated) data have been also used in
#' Guillemot et al. (in press, 2019).
#'
NULL
# End of mentalImageryOSIQ

#_____________________________________________________________________
# Print function alcoholInEU  ----
#_____________________________________________________________________
#_____________________________________________________________________
#' Change the print function for the data set:
#' \code{OSIQ}
#' #'
#' Change the print function for the data set:
#' \code{OSIQ}.
#'
#' @param x a list: the data set: \code{OSIQ}
#' @param ... the rest
#' @author Herve Abdi
#' @export
print.OSIQ <- function(x, ...){
  ndash = 78 # How many dashes for separation lines
  cat(rep("-", ndash), sep = "")
  cat("\n A list with one data.frame (OSIQ): ")
   cat("\n    the responses of 2,100 participants to the 30 questions of the OSIQ")
  cat("\n First letter of row (H,M,L) denotes the memory group of the participant.")
  cat("\n First letter of column (s,o) denotes the type of mental imagery.")
  cat("\n Responses are integers from 1 to 5.",deparse(eval(substitute(substitute()))),"\n")
  cat(rep("-", ndash), sep = "")
  cat("\nOSIQ: ","A 2,100 by 30 data frame with values ranging from 1 to 5.")
  cat("\n",rep("-", ndash), sep = "")
  cat("\n")
  invisible(x)
} # end of function print.OSIQ
# end of print.OSIQ ----
#_____________________________________________________________________
