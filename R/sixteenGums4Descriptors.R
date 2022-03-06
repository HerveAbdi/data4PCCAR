# Entête ----
#  File for: sixteenGums4Descriptors example 
# for data4PCCAR
# Hervé Abdi: March 6, 2022.
#
# 
#  Preamble ----
# sixteenGums4Descriptors
#' @title  the average data
#' from ten tasters who evaluated
#' the intensity of four descriptors
#' for 
#' sixteen chewing-gums.
#' These data can be used to illustrate:
#' normed 
#' Principal Component Analysis (PCA) 
#' or Multiple Correspondence Analysis (MCA).
#'
#' @description
#' \code{sixteenGums4Descriptors}: 
#'  the average data obtained 
#' from ten tasters who evaluated
#' sixteen chewing gums on
#' the intensity of four descriptors.
#' These data can be used to illustrate:
#' un-normed  (or normed) 
#' Principal Component Analysis (PCA) 
#' and Correspondence Analysis (MCA).
#' The data are the averages 
#' (computed
#' over the 10 judges) of the 
#' intensity ratings of the 4 descriptors for
#' the 16 gums.
#' 
#' 
#'
#' @name sixteenGums4Descriptors
#' @usage data("sixteenGums4Descriptors", 
#'        package = 'data4PCCAR')
#' @docType data
#' @format
#' A list 
#' (of class: \code{sixteenGums})
#' containing 
#' one data frame,
#' and several vectors:
#' * \code{ratingsIntensity: } 
#' A data frame of dimension 
#' \eqn{I = }16 rows (Beers) by \eqn{J = }4 columns
#' (descriptors) with
#' the average intensity ratings  performed using 
#' different scales per variables 
#'  (respectively: \code{10, 5, 5, 100} points).
#' * \code{longNames: } 
#' a 16 element vector with the long names of the gums. 
#'  \code{color4Products}
#'  a 16 element vector with the
#'  color names of the gums.
#'  \code{color4Descriptors}
#' a 4 element vector with the
#'  color names of the descriptors.
#'  \code{scale}
#' a 4 element vector with the
#'  number of points of the scales used
#'  to describe the gums.
#' @md
#' @keywords datasets data4PCCAR
#' @author Hervé Abdi & Carlos Gomez.
#' @references
#' These data are used in 
#' Abdi H, Gomez, C., & Delmas, M. (2022).
#' Méthodes Statistiques Multivariées pour l'Analyse 
#' Sensorielle et les Etudes Consommateurs.
#'
NULL
# End of sixteenGums4Descriptors ----

#___________________________________________________________
# Print function sixteenGums----
#___________________________________________________________
#___________________________________________________________
#' Change the print function for the data set:
#' \code{sixteenGums}
#' #'
#' Change the print function for the data set:
#' \code{sixteenGums}.
#'
#' @param x a list: the data set 
#' \code{sixteenGums}.
#' @param ... the rest.
#' @author Hervé Abdi
#' @export
print.sixteenGums <- function(x, ...){
  ndash = 78 # How many dashes for separation lines
  cat(rep("-", ndash), sep = "")
  cat("\n A list: Sixteen chewing gums are evaluated for the intensity of 4 descriptors.\n")
  # cat("\n List name: ",deparse(eval(substitute(substitute(x)))),"\n")
  cat(rep("-", ndash), sep = "")
  cat("\n$ratingsIntensity  ","The 16 gums by 4 scales df with the intensity ratings.")
  cat("\n$longNames         ","A vector with the full names of the gums.")
  cat("\n$color4Products    ","A vector with colornames for the beers.")
  cat("\n$color4Descriptors ","A vector with colornames for the descriptors.")
  cat("\n$scale             ","A vector with the number of points of the 4 scales.")
  cat("\n",rep("-", ndash), sep = "")
  cat("\n")
  invisible(x)
} # end of function print.sixteenGums
# end printsixteenGums----
#___________________________________________________________

