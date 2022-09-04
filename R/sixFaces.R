# Description for file sixFaces example for data4PCCAR
# Hervé Abdi: April 14, 2018.
#
#  sixFaces   Preambule ----
#' @title The pictures of six faces (3 men and 3 women).
#' To be used to illustrate PCA or PLS on images.
#'
#' @description \code{sixFaces}:
#' The 230*240 = 55,200 pixels pictures of six faces (3 men and 3 women).
#' To be used to illustrate PCA or PLS on images.
#'
#' @details \code{sixFaces} contains the original pictures
#' (as a 230 * 240 * 6 array),
#' the normalized pictures of the faces (such that the sum of squares
#' of the pixels of a face is equal to 1), the
#' 6*55,200 matrix of the faces.
#'
#' @note To get back a 230*240 image-matrix from the vectorized
#' version of the face use, for example:
#'
#' \code{face1 <- matrix(face.matrix[1,],230,240,byrow = FALSE)}
#'
#' To visualize an image use, for example:
#'
#' \code{image(sixFaces$face.array.normed[,,1])}
#'
#' To fold back an image from the vectorized and vizualize it, use:
#'
#' \code{image(matrix(sixFaces$face.matrix[1,],230,240,byrow = FALSE))}
#'
#' @name sixFaces
#' @usage data("sixFaces")
#' @docType data
#' @format
#' A list containing 3 objects:
#' * \code{face.array.raw} a 230 * 240 * 6 array storing the 6
#'  (230 by 240 pixels) raw images of the faces.
#'  Values of the pixels
#'  go from 0 to 253.
#' * \code{face.array.normed} a 230 * 240 * 6 array storing the 6
#'  (230 by 240 pixels) \emph{normalized} images of the faces.
#'  The sum of the squared values of a normalized image is now equal to 1.
#' * \code{face.matrix} a matrix of dimensions 6 by 55,220 ( = 230*240)
#' storing the vectorized (normalized) pictures of the faces.
#' @md
#' @keywords datasets data4PCCAR
#' @author Hervé Abdi
#' @references
#'  The faces have been used in a few publications including:
#'
#'  Abdi, H., Valentin, D., O'Toole, A.J., & Edelman, B. (2005).
#'  DISTATIS: The analysis of multiple distance matrices.
#'  \emph{Proceedings of the IEEE Computer Society:
#'  International Conference on Computer Vision
#'  and Pattern Recognition.} (San Diego, CA, USA). pp. 42-47.
#'
#' Abdi, H., & Valentin, D. (2006).
#' \emph{Mathématiques pour les Sciences Cognitives,
#'  (Mathematics for Cognitive Sciences).}
#' Grenoble (France): Presses Universitaires de Grenoble.
NULL
# End of sixFaces ----
#_____________________________________________________________________
# Print function sixFaces  ----
#_____________________________________________________________________
#' Change the print function for the data set:
#' \code{sixFaces}
#'
#' Change the print function for the data set:
#' \code{sixFaces}.
#'
#' @param x a list: the data set: \code{sixFaces}
#' @param ... the rest
#' @author Herve Abdi
#' @keywords internal
#' @export
print.sixFaces <- function(x, ...){
  ndash = 78 # How many dashes for separation lines
  cat(rep("-", ndash), sep = "")
  cat("\n A list: 6 230*240 grey images of faces (3 Men & 3 Women). For PCA & PLS. \n")
  # cat("\n List name: ",deparse(eval(substitute(substitute(x)))),"\n")
  cat(rep("-", ndash), sep = "")
  cat("\n$face.array.raw    ","A 230*240*6 array. Pixel values from 0 to 253.")
  cat("\n$face.array.normed ","A 230*240*6 array. Faces have sum of squares of 1.")
  cat("\n$face.matrix       ","The 6*55,200 (= 230*240) vectorized normalized face matrix.")
  cat("\n",rep("-", ndash), sep = "")
  cat("\n")
  invisible(x)
} # end of function print.sixFaces
# end print.sixFaces ----
#_____________________________________________________________________

#'
#'
#'
#'

