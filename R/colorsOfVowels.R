# Description for file colorOfVowels example for data4PCCAR
# Hervé Abdi: April 14, 2018.
#
#  colorsOfVowels  Preambule ----
#' @title 133 (French) participants
#'  associated 6 colors to 6 vowels (data from
#' Chastaing, 1961).
#'
#' @description
#' \code{colorsOfVowels}
#'133 French
#'participants (106 with synesthesia and 27 without synesthesia)
#'associated 6 colors to 6 vowels (data from
#' Chastaing, 1961).
#'
#' @details The participants were asked if they
#' had previously spontaneously associated a color to a vowel.
#' The 106 participants who responded "Yes" are called
#' with \emph{synesthesia} and the 27
#' participants who responded "No" are called
#' with \emph{without synesthesia}.
#'
#' The question asked to the participants was:
#' ``Here are six colors: red, orange, yellow, blue, and violet.
#' What vowel do you think match each color?".
#' So, the color are the stimuli
#' (i.e., the \emph{independent} variable)
#' and the vowels are the responses
#' (i.e., the \emph{dependent} variable).
#'
#' The data were obtained by concatenation of the data from
#' Tables 1 and 2 of Chastaing (1961).
#' The vowels correspond to the following sound(s)
#' 1) \emph{i} as in prett\strong{y},
#' 2) \emph{y} the French "u"-sound as in t\strong{u}t\strong{u},
#' 3) \emph{e} this column concatenate all the
#' accented e-sounds in French such as é, è and ê
#' (e.g., as in m\strong{ay}),
#' 4) \emph{a} like P\strong{a}p\strong{a},
#' 5) \emph{o} the o and ô sounds, and
#' 6) \emph{u} the "ou" sound (like in y\strong{ou}).
#'
#' @name colorsOfVowels
#' @usage data("colorsOfVowels")
#' @docType data
#' @format
#' A list containing 5 objects:
#' 1) \code{CT}: a 6 vowels by 6 colors (pseudo) contingency table
#' storing the responses of all 133 participants;
#' 2) \code{CT.noSynesthesia}: a 6 vowels by 6 colors (pseudo)
#' contingency table
#' storing the responses of the 27 non synesthesia participants;
#' 3) \code{CT.Synesthesia}: a 6 vowels by 6 colors (pseudo)
#' contingency table
#' storing the responses of the 106 synesthesia participants;
#' 4) \code{vowelsDescription} a data frame describing the
#' vowels (from their place of articulation as anterior or posterior);
#' and
#' 5) \code{colorsDescription} a data frame
#' giving the R-color code for the colors.
#'  @keywords datasets data4PCCAR
#'  @author Maxime Chastaing and Hervé Abdi
#'  @references
#'  The data reported here were obtained from Tables 1 and 2 from
#'   Chastaing, M (1961). des sons et des couleurs.
#'   \emph{Vie et Language, 112}, 358-365.
NULL
# End of colorsOfVowels ----
#_____________________________________________________________________
# Print function colorsOfVowels  ----
#_____________________________________________________________________
#_____________________________________________________________________
#' Change the print function for the data set:
#' \code{colorsOfVowels}
#'
#' Change the print function for the data set:
#' \code{colorsOfVowels}.
#'
#' @param x a list: the data set: {colorsOfVowels}
#' @param ... the rest
#' @author Herve Abdi
#' @export
print.colorsOfVowels <- function(x, ...){
  ndash = 78 # How many dashes for separation lines
  cat(rep("-", ndash), sep = "")
  cat("\n A list: 133 French participants associated 6 colors to 6 vowels.\n")
  # cat("\n List name: ",deparse(eval(substitute(substitute(x)))),"\n")
  cat(rep("-", ndash), sep = "")
  cat("\n$CT                 ","6 vowels by 6 colors contingency table:")
  cat("\n$CT                 " ,"       133 (all)  participants.")
  cat("\n$CT.noSynesthesia   ","6 vowels by 6 colors contingency table:")
  cat("\n$CT                 " ,"       27 no synesthesia participants.")
  cat("\n$CT.Synesthesia     ","6 vowels by 6 colors contingency table:")
  cat("\n$CT                 " ,"       106 synesthesia participants.")
  cat("\n$vowelsDescription  ","Place of articulation of the vowels.")
  cat("\n$colorsDescription ","R-color code for the colors.")
  cat("\n",rep("-", ndash), sep = "")
  cat("\n")
  invisible(x)
} # end of function print.colorsOfVowels
# end print.colorsOfVowels ----
#_____________________________________________________________________

#'

