#_____________________________________________________________________
# Preamble ----
#_____________________________________________________________________
#' Change the print function for the data set: 'str_colorsOfMusic'
#'
#' Change the print function for the data set: 'str_colorsOfMusic'
#'
#' @param x a list: the data set: 'str_colorsOfMusic'
#' @param ... the rest
#' @author Hervé Abdi
#' @keywords internal
#' @export
print.str_colorsOfMusic <- function (x, ...) {
  ndash = 78 # How many dashes for separation lines
  cat(rep("-", ndash), sep = "")
  cat("\n 12 children and 10 Adults pick one color (out of 10) to describe 9 pieces of music  \n")
  # cat("\n List name: ",deparse(eval(substitute(substitute(x)))),"\n")
  cat(rep("-", ndash), sep = "")
  cat("\n$participantsChoice      ", "A df. Rows: participants, Columns: music pieces")
  cat("\n$participantsDescription ", "Age (Child/Adult) and Gender (F/M) of participants")
  cat("\n$colorInformation        ", "Name and code of the colors")
  cat("\n$cubeOfData              ", "The 10-Color*9-Music*22-Participant cube of 0/1 data")
  cat("\n$contingencyTable        ", "The 10-Color*9-Music (pseudo) contingency table")
  cat("\n",rep("-", ndash), sep = "")
  cat("\n")
  invisible(x)
} # end of function print.str_colorsOfMusic
#_____________________________________________________________________
# description ----
# Description for colorOfMusic
#' @title 12 Children and 10 Adults picked up
#' the color that best describes 9 pieces of music.
#'
#' @description \code{colorOfMusic}. A data set:
#'  12 Children and 10 Adults picked up
#' one color to describe 9 pieces of music (one at a time).
#' The participants are described by their
#' age (children vs adults) and by their gender
#' (F vs M). This data set is used to illustrate
#' correspondence analysis in Abdi and Bera (2018).
#' @name colorOfMusic
#' @usage data("colorOfMusic")
#' @docType data
#' @format A list with 5 data frames or arrays
#' describing respectively
#' 1) the check mark data, 2) the description of the participants,
#' 3) the names and color codes of the 10 colors used,
#' 4) the binary cube of data  (color by music by participants),
#' and 5)
#' the (pseudo) contingency table of data (color by music).
#' \describe{
#' \item{\code{participantsChoice}}{A 22 rows (Participants) by 9
#' columns (Pieces of Music) data frame. The number
#' at the intersection of
#' of a row and a column gives the number-code
#' (from 1 to 10) of the chosen color
#' for the music (column) by the participant (row).
#' The name of the colors are given in the data frame stored in
#' \code{$colorInformation}.}
#'\item{\code{participantsDescription}}{A 22 by 2
#'data frame  describing the
#'participants according to \code{Age} (Child vs Adult) and
#' \code{Gender} (F vs M).}
#' \item{\code{colorInformation}}{The name
#' of the colors and their color code (useful when plotting the
#' data).}
#' \item{\code{cubeOfData}}{The 10-Colors by 9-Music pieces
#'  by 22-Participants
#'  cube of 0/1 data. A value of 1 (resp. 0) means that the participant
#'  chose (resp. did not choose)
#'  the color to match the piece of music}
#' \item{\code{contingencyTable}}{The 10-Colors
#'  by 9-Music Pieces (pseudo) contingency table.
#'  The value at the intersection of a row (color) and a column
#'  (piece of music) is the number of participants who chose this
#'  color to match this piece of music. This contingency
#'  table is typically analyzed with correspondence analysis.
#'  (e.g., see Abdi and Bera, 2018). }
#' }
#' @keywords datasets data4PCCAR
#' @source Abdi, H. and Bera, M. (2018).  Correspondence Analysis.
#' \emph{Encyclopedia of Social Network Analysis and Mining} (2nd Edition).
#' New York: Springer Verlag.
#' \url{www.utdallas.edu/~herve}.
#' @author Herve Abdi
"colorOfMusic"
