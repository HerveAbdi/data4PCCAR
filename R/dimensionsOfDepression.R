# head ----
# Description for file  dimensionsOfDepression example for data4PCCAR
# Hervé Abdi: September 20, 2019.
# the Dimensions of Depression 
# Data from the sapiens lab see
# https://sapienlabs.org/quantitative-similarity-of-depression-tools/
# https://sapienlabs.org/15-depression-assessment-tools-and-their-differences/
# And also following references for more:
#[1] Fried, E., & Nesse, R. (2015). 
# Depression is not a consistent syndrome: 
#   An investigation of unique symptom patterns in the STAR*D study. 
# Journal Of Affective Disorders, 
# 172, 96-102. doi: 10.1016/j.jad.2014.10.010
# [2] Fried, E. (2017). The 52 symptoms of major depression:
#   Lack of content overlap among seven common depression scales. 
# Journal Of Affective Disorders, 208, 191-197.
# doi: 10.1016/j.jad.2016.10.019
#
# The similarities of depression scales
#
#
#   dimensionsOfDepression
#  Preambule ----
#' @title 
#' The similarity of 15 psychometric instruments 
#' measuring depression.
#' 
#' @description
#'  \code{dimensionsOfDepression}: gives the 
#'  the similarity  (measured on a scale from 0 to 1000, 
#'  with bigger numbers
#' meaning more similar)
#'  between 15 psychometric instruments (i.e. scales)
#' measuring depression.
#' 
#' @source These data come from a post of the sapiens lab,
#' for more details and references see
#' \url{https://sapienlabs.org/quantitative-similarity-of-depression-tools/}
#' 
#' @name  dimensionsOfDepression
#' @usage data("dimensionsOfDepression") 
#' @docType data
#' @format
#' A list containing 2 data frames:
#' * \code{df.scaleSimilarity}:
#' a 15 * 15 matrix giving the similarity on a scale from 0 to 1000
#' between the scales (the similarity can be seen as akind
#' to a percentage 
#' of common variance rescaled from 0 to 1000).
#' * \code{supplementary.information}: a data frame with the
#' whole name of the scales 
#' and one factor variable indicating if he
#' scale is targeted toward adults 
#' (including the geriatric population) 
#' or for children or adolescents.
#' #' @md
#' @keywords datasets data4PCCAR
#' @author Hervé Abdi 
#' @references 
#' for details and more references, see the \code{source} url.
#' \url{https://sapienlabs.org/quantitative-similarity-of-depression-tools/}
NULL
# End of dimensionsOfDepression
#_____________________________________________________________________
# Print function dimDepression  ----
#_____________________________________________________________________
#_____________________________________________________________________
#' Change the print function for the data set:
#' \code{dimDepression}
#' #'
#' Change the print function for the data set:
#' \code{dimDepression}.
#'
#' @param x a list: the data set: \code{dimDepression}
#' @param ... the rest
#' @author Herve Abdi
#' @keywords internal
#' @export
print.dimDepression <- function(x, ...){
  ndash = 78 # How many dashes for separation lines
  cat(rep("-", ndash), sep = "")
  cat("\n A list: The punctuation of six authors.\n")
  # cat("\n List name: ",deparse(eval(substitute(substitute(x)))),"\n")
  cat(rep("-", ndash), sep = "")
  cat("\n$scaleSimilarity           ","the inter-similarity (measured on a scale from 0")
  cat("\n                           " ,"to 1000) of 15 instruments measuring depression. ")
   cat("\n$supplementary.information ","The names of the instruments and more information.")
  cat("\n",rep("-", ndash), sep = "")
  cat("\n")
  invisible(x)
} # end of function print.dimDepression
# end print.dimDepression ----
#_____________________________________________________________________
