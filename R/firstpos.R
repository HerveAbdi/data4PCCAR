#' @title firstpos
#' @description Changes the sign of the values in two matrices according to the sign of the elements in the first line of the first matrix. This is used to get reproducible results when performing singular value decompositions or eigenvalue decompositions.
#' @param P A matrix of numeric values
#' @param Q If not NULL, a matrix of numeric values with the same number of columns as \code{P}
#' @return 
#' \code{P} A matrix with the same values as in P, except that the sign of all the values in each column is flipped if the first value of this column is negative.
#' \code{Q} A matrix with the same values as in Q, except that the sign of all the values in each column is flipped the same way that it is flipped for P.
#' If Q is NULL, then the function returns only a list with only one element (P)
#' 
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  A <- matrix(rnorm(4), 2, 2)
#'  B <- matrix(rnorm(6), 3, 2)
#'  firstpos(A, B)
#'  }
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
