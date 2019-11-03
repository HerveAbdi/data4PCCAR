#' @title Test if an object is a diagonal matrix
#'
#' @description \code{is.diagMat} Test
#' if an object is a diagonal matrix
#'
#' @param DATA an object to be tested.
#'
#' @return a logical value \code{TRUE} if the object is a matrix.
#' @export
#' @author Hervé Abdi
#'
#' @examples
#' is.diagMat(c(1,2,3))
#'
is.diagMat <- function(DATA){
  return(isTRUE( identical(diag(diag(DATA)),  DATA) &
                   (dim(DATA)[1] == dim(DATA)[2]) ) )
}    


# 
# 
# isDiag <- function(M) {
#   dimz <- dim(M)
#   if (length(dimz) > 2) {
#     stop("Too many dimensions")
#   }
#   if (is.null(dimz)) {
#     return(FALSE)
#   } else if (length(M) == dimz[1]) {
#     return(FALSE)
#   } else if (length(M) == dimz[2]) {
#     return(FALSE)
#   } else if (dimz[1] != dimz[2]) {
#     stop("M is not a square matrix")
#   } else if (length(M) == prod(dimz)) {
#     if (!isSymmetric(M)) {
#       return(FALSE)
#     } else if (all(M[upper.tri(M)] == 0)) {
#       return(TRUE)
#     } else {
#       return(FALSE)
#     }
#   } else {
#     stop("Something went horribly wrong")
#   }
# }
# isDiag(1:3)
# isDiag(diag(5))
# isDiag(toeplitz(5:1))
# isDiag(data.frame(toeplitz(5:1)))
