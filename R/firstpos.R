#_____________________________________________________________________
# File for function firstpos
# December 18 2019. VG
# 

# Preamble ----
#' @title ensures that the first element of all the columns
#' of one/two matrix/matrices
#' is positive.
#' 
#' @description \code{firspos}: 
#' Changes the sign of the values of the columns 
#' in one or two matrices 
#' according to the sign of the elements in the first line 
#' of the first matrix. 
#' After transformation the first element of every column of the
#' matric or matrices will be positive.
#' 
#' This is used to get reproducible 
#' results when performing singular value decompositions 
#' or eigenvalue decompositions.
#' 
#' Note that when provided tow matrices the signs of the
#' matrices will match: This is used to unsures that
#' singular vectors of a given matrix match.
#' 
#' @param P A matrix of numeric values
#' @param Q If not \code{NULL}, 
#' a matrix of numeric values with the 
#' same number of columns as \code{P}
#' @return If \code{Q} is \code{NULL}, 
#' then the function returns a only the modified P matrix.
#' Otherwise, the functions returns a list with two elements:
#' \itemize{
#'   \item \code{P} A matrix with the same values as in \code{P}, 
#'   except that the sign of all the values in each column
#'    is flipped if the first value of this column is negative.
#'   \item \code{Q} A matrix with the same values as in 
#'   \code{Q}, 
#'   except that the sign of all the values in each column 
#'   is flipped the same way that it is flipped for\code{P}.
#' }
#' 
#' @examples 
#' \dontrun{
#'  A <- matrix(rnorm(4), 2, 2)
#'  B <- matrix(rnorm(6), 3, 2)
#'  firstpos(A, B)
#' }
#' @rdname firstpos
#' @export

firstpos <- function(P, Q = NULL) {
  if (!is.numeric(P)) stop("P must contain only numeric values")
  if (is.null(Q)) {
    row1sign <- ifelse(P[1,] < 0, -1, 1)
    P <- sweep(x = P, MARGIN = 2, STATS = row1sign, FUN = "*")
    return(P)
  }
  if (!is.numeric(Q)) stop("Q must contain only numeric values")
  if (NCOL(P) != NCOL(Q)) stop("P and Q must have the same number of columns.")
  row1sign <- ifelse(P[1,] < 0, -1, 1)
  P <- sweep(x = P, MARGIN = 2, STATS = row1sign, FUN = "*")
  Q <- sweep(x = Q, MARGIN = 2, STATS = row1sign, FUN = "*")
  return(list(P = P, Q = Q))
}
