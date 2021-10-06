# Entête ----
#  File for: sixBeers12Descriptors10Judges example 
# for data4PCCAR
# Hervé Abdi: October 05, 2021.
#
# 
#  Preamble ----
# sixBeers12Descriptors10Judges
#' @title  Ten tasters evaluated
#' the intensity of twelve descriptors
#' for 
#' six beers.
#' These data can be used to illustrate:
#' un-normed  (or normed) 
#' Principal Component Analysis (PCA) 
#' or Correspondence Analysis (CA).
#'
#' @description
#' \code{sixBeers12Descriptors10Judges}:
#'  tasters evaluated
#' the intensity of twelve descriptors
#' for 
#' six beers.
#' These data can be used to illustrate:
#' un-normed  (or normed) 
#' Principal Component Analysis (PCA) 
#' and Correspondence Analysis (CA).
#' The data are the averages 
#' (computed
#' over the 10 judges) of the 
#' intensity ratings of the descriptors for
#' the beers.
#' 
#' 
#'
#' @name sixBeers12Descriptors10Judges
#' @usage data("sixBeers12Descriptors10Judges", 
#'        package = 'data4PCCAR')
#' @docType data
#' @format
#' A list 
#' (of class: \code{sixBeers12Descriptors})
#' containing 
#' one data frame,
#' one tibble, and several vectors:
#' * \code{ratingsIntensity: } 
#' A data frame of dimension 
#' \eqn{I = }6 rows (Beers) by \eqn{J = }12 columns
#' (descriptors) with
#' the average intensity ratings  performed using a 
#'  0 to 7  Likert rating scale.
#' * \code{tb: } A tibble with the same
#'  data as the data frame \code{ratingsIntensity},
#'  but
#'  with in addition three (character) columns describing
#'  the beers (see below \code{shortNames},
#'  \code{brewedIn}, \code{color4I}).
#' * \code{longNamesBeers: }
#' A 6 element vector with the full names of the beers.
#' \code{brewedIn},
#' A 6 element vector with the name
#' of the country of origin of the
#' beers.
#' * \code{color4Products: }
#' A 6 element vector with color names matching the
#' origin of the beers.
#' * \code{descripteurs: } 
#' A
#'  12 element vector with the French name of the
#'  descriptors.
#' * \code{color4Descriptors: } 
#' A
#'  12 element vector with color names for the
#'  descriptors.
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
# End of sixBeers12Descriptors10Judges ----

#___________________________________________________________
# Print function sixBeers12Descriptorss----
#___________________________________________________________
#___________________________________________________________
#' Change the print function for the data set:
#' \code{sixBeers12Descriptors}
#' #'
#' Change the print function for the data set:
#' \code{sixBeers12Descriptors}.
#'
#' @param x a list: the data set 
#' \code{sixBeers12Descriptors}.
#' @param ... the rest.
#' @author Hervé Abdi
#' @export
print.sixBeers12Descriptors <- function(x, ...){
  ndash = 78 # How many dashes for separation lines
  cat(rep("-", ndash), sep = "")
  cat("\n A list: Six Beers are evaluated for the intensity of 12 descriptors.\n")
  # cat("\n List name: ",deparse(eval(substitute(substitute(x)))),"\n")
  cat(rep("-", ndash), sep = "")
  cat("\n$ratingsIntensity  ","The 6 beers * 12 scales df with the intensity ratings.")
  cat("\n$tb                ","A tibble with in addition some information on the beers.")
  cat("\n$longNamesBeers    ","A vector with the full names of the beers.")
  cat("\n$brewedIn          ","A vector with the country of origin of the beers.")
  cat("\n$color4Products    ","A vector with colornames for the beers.")
  cat("\n$color4Descriptors ","A vector with colornames for the descriptors")
  cat("\n",rep("-", ndash), sep = "")
  cat("\n")
  invisible(x)
} # end of function print.fiveBeersRank
# end print.fiveBeersRank----
#___________________________________________________________

