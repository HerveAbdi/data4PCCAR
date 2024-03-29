#' Change the print function for the data set:
#' \code{foodOfTheWorld}
#'
#' Change the print function for the data set:
#' \code{foodOfTheWorld}
#'
#' @param x a list 
#' that contains items 
#' from the \code{epGPCA2} class.
#' @param ... the rest (i.e., 
#' inherited/passed arguments for
#' \code{S3} print method(s).)
#' @author Derek Beaton, Cerise Chin-Fatt, Herve Abdi
#' @keywords internal
#' @export
print.epGPCA2 <- function (x, ...) 
{
  res.epGPCA <- x
  if (!inherits(res.epGPCA, "epGPCA")) 
    stop("no convenient data")
  cat("**Results for Generalized Principal Component Analysis**\n")
  cat("The analysis was performed on ", nrow(res.epGPCA$fi), 
      "individuals, described by", nrow(res.epGPCA$fj), "variables\n")
  cat("*The results are available in the following objects:\n\n")
  res <- array("", c(16, 2), list(1:16, c("name", "description")))
  res[1, ]  <- c("$fi     ", "Factor scores of the rows")
  res[2, ]  <- c("$di     ", "Squared distances of the rows")
  res[3, ]  <- c("$ci     ", "Contributions of the rows")
  res[4, ]  <- c("$ri     ", "Cosines of the rows")
  res[5, ]  <- c("$fj     ",  "Factor scores of the columns")
  res[6, ]  <- c("$dj     ", "square distances of the columns")
  res[7, ]  <- c("$cj     ", "Contributions for the columns")
  res[8, ]  <- c("$rj     ", "Cosines of the columns")
  res[9, ]  <- c("$t      ", "Explained Variance")
  res[10, ] <- c("$eigs   ", "Eigenvalues")
  res[11, ] <- c("$M.     ", "masses")
  res[12, ] <- c("$W.     ", "weights")
  res[13, ] <- c("$pdq.   ", "GSVD data")
  res[14, ] <- c("$X      ", "X matrix to decompose")
  res[15, ] <- c("$center ", "Center of X")
  res[16, ] <- c("$scale  ", "Scale factor of X")
  print(res)
}