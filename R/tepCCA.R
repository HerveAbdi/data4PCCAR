# tepCCA -----
#' @title A \code{TExPosition}-type version of Canonical Correlation
#' Analysis (CCA).\emph{Temporary Version (11-04-2019)}.
#' 
#' @description \code{tepCCA}:
#'  A \code{TExPosition}-type version of Canonical Correlation
#' Analysis (CCA). \emph{Temporary Version. 
#' This version will soon be revised to take into account 
#' the new \code{GSVD}-package from Derek Beaton}.
#'
#' @param DATA1  an \eqn{N*I} matrix of quantitative data.
#' @param DATA2  an \eqn{N*J} matrix of quantitative data.
#' @param center1 when \code{TRUE} (default) \code{DATA1}
#' will be centered.
#' @param center2 when \code{TRUE} (default) \code{DATA2}
#' will be centered.
#' @param scale1 when \code{TRUE} (default) \code{DATA1}
#' will be normalized. Depends upon \code{ExPosition}
#' function \code{expo.scale} whose description is:
#'boolean, text, or (numeric) vector.
#'If boolean or vector,
#'it works just like \code{scale}.
#'The following text options are available:
#' \code{'z'}: z-score normalization,
#' \code{'sd'}: standard deviation normalization,
#' \code{'rms'}: root mean square normalization,
#'  \code{'ss1'}: sum of squares
#'  (of columns) equals 1
#'  (i.e., column vector of length of 1).
#' @param scale2 when \code{TRUE} (default) \code{DATA2}
#' will be normalized
#'  (same options as for \code{scale1}).
#' @param DESIGN a design matrix 
#' to indicate if the rows comprise several groups.
#' @param make_design_nominal 
#' a boolean. If \code{TRUE} (default), 
#' DESIGN is a vector that indicates groups 
#' (and will be dummy-coded). 
#' If \code{FALSE}, \code{DESIGN} is a dummy-coded matrix.
#' @param graphs 
#' a boolean. If \code{TRUE}, 
#' graphs and plots are provided 
#' (via \code{TExPosition::tepGraphs}).
#' @param k 	number of components to return.
#' @author Vincent Guillemot, Derek Beaton, Hervé Abdi
#' @return
#' See \code{ExPosition::epGPCA} (and also \code{ExPosition::corePCA}) 
#' for details on what is returned. 
#' In addition to the values returned:
#' \code{tepCCA} returns  
#' 
#' \code{lx}: 
#' the latent variables for \code{DATA1}, and 
#'  \code{ly}: 
#' the latent variables for \code{DATA2}'
#' 
#' \code{data1.norm}:	the
#' center and scale information for \code{DATA1}. and 
#' \code{data2.norm}:	the
#' center and scale information for \code{DATA2}.
#' @references 
#' Abdi H., Eslami, A., & Guillemot, V. (2018). 
#' Canonical correlation analysis (CCA). 
#' In R. Alhajj and J. Rokne (Eds.), 
#' \emph{Encyclopedia of Social Networks and Mining (2nd Edition)}. 
#' New York: Springer Verlag. 
#' @importFrom ExPosition epGPCA
#' @import TExPosition
# #' @importFrom TExPosition tepGraphs 
#' @export
#' @examples
#' \dontrun{
#' # Sime example here ***}
tepCCA <- function (DATA1, DATA2, 
                    center1 = TRUE, scale1 = "SS1", 
                    center2 = TRUE, scale2 = "SS1", 
                    DESIGN = NULL, make_design_nominal = TRUE, 
          graphs = TRUE, k = 0) {
  if (nrow(DATA1) != nrow(DATA2)) {
    stop("DATA1 and DATA2 must have the same number of rows.")
  }
  # Internal function ----
  tepOutputHandler <-  function (res = NULL, tepPlotInfo = NULL) {
    if (!is.null(res) && !is.null(tepPlotInfo)) {
      final.output <- list(TExPosition.Data = res, 
                               Plotting.Data = tepPlotInfo)
      class(final.output) <- c("texpoOutput", "list")
      return(final.output)
    }
    else if (!is.null(res) && is.null(tepPlotInfo)) {
      return(res)
    }
    else {
      print("Unknown inputs. tepOutputHandler must exit.")
      return(0)
    }
    print("It is unknown how this was executed. tepOutputHandler must exit.")
    return(0)
  }
  #___________________________________________________________________
  main <- paste("CCA: ", deparse(substitute(DATA1)), " & ", 
                deparse(substitute(DATA2)), sep = "")
  DESIGN <- texpoDesignCheck(DATA1, DESIGN,
                               make_design_nominal = make_design_nominal)
  DESIGN <- texpoDesignCheck(DATA2, DESIGN, 
                             make_design_nominal = FALSE)
  DATA1 <- as.matrix(DATA1)
  DATA2 <- as.matrix(DATA2)
  DATA1 <- expo.scale(DATA1, scale = scale1, center = center1)
  DATA2 <- expo.scale(DATA2, scale = scale2, center = center2)
  R <- t(DATA1) %*% DATA2
  # R <- cor(DATA1, DATA2)
  M <- t(DATA1) %*% DATA1
  # M <- cor(DATA1)
  W <- t(DATA2) %*% DATA2
  # W <- cor(DATA2)
  Mm1 <- matrix.exponent(M, power = -1)
  Wm1 <- matrix.exponent(W, power = -1)
  # print(R[1:3, 1:3])
  # print(cor(DATA1, DATA2)[1:3, 1:3])
  # print(M)
  res1 <- epGPCA2(DATA = R,
                 k = k,
                 graphs = FALSE,
                 masses = Mm1,
                 weights = Wm1,
                 scale = FALSE,
                 center = FALSE)
  res <- list()
  #res <- res$ExPosition.Data
  res$ExPosition.Data <- res1
  res$center <- NULL
  res$scale <- NULL
  res$W1 <- res$M
  res$W2 <- res$W
  res$M <- res$W <- NULL
  res$data1.norm <- list(center = attributes(DATA1)$`scaled:center`, 
                         scale = attributes(DATA1)$`scaled:scale`)
  res$data2.norm <- list(center = attributes(DATA2)$`scaled:center`, 
                         scale = attributes(DATA2)$`scaled:scale`)
  res$lx <- ExPosition::supplementalProjection(DATA1, res$fi, Dv = res$pdq$Dv)$f.out
  res$ly <- ExPosition::supplementalProjection(DATA2, res$fj, Dv = res$pdq$Dv)$f.out
  class(res) <- c("tepPLS", "list")
# # res <- ExPosition::epGPCA(DATA = R, k = k,
# # graphs = FALSE, scale = FALSE, 
# # center = FALSE, masses = Mm12, weights = Wm12)
#   res <- list()
#   res$TExPosition.Data <- list()
#   res$TExPosition.Data <- res1
#   res$TExPosition.Data$center <- NULL
#   res$TExPosition.Data$scale <- NULL
#   res$TExPosition.Data$W1 <- res$M
#   res$TExPosition.Data$W2 <- res$W
#   res$TExPosition.Data$M <- res$W <- NULL
#   res$TExPosition.Data$data1.norm <- list(center = attributes(DATA1)$`scaled:center`, 
#                          scale = attributes(DATA1)$`scaled:scale`)
#   res$ExPosition.Data$data2.norm <- list(center = attributes(DATA2)$`scaled:center`, 
#                          scale = attributes(DATA2)$`scaled:scale`)
#   res$ExPosition.Data$lx <- ExPosition::supplementalProjection(DATA1, res$TExPosition.Data$fi, 
#                                              Dv = res$ExPosition.Data$pdq$Dv)$f.out
#   res$ExPosition.Data$ly <- ExPosition::supplementalProjection(DATA2, res$TExPosition.Data$fj, 
#                                              Dv = res$ExPosition.Data$pdq$Dv)$f.out
#   class(res) <- c("tepPLS", "list")
  # tepPlotInfo <- TExPosition::tepGraphs(res = res, 
  #                                       DESIGN = DESIGN, main = main, 
  #                                       graphs = graphs)
  # return(tepOutputHandler(res = res, tepPlotInfo = tepPlotInfo))
  return(tepOutputHandler(res = res, tepPlotInfo = NULL))
}