# tepRA -----
#' @title A \code{TExPosition}-type version of Redundancy
#' Analysis (RA).\emph{Temporary Version (14-07-2019)}.
#'
#' @description \code{tepRA}:
#'  A \code{TExPosition}-type version of Redundancy
#' Analysis (RA). \emph{Temporary Version.
#' This version will soon be revised to take into account
#' the new \code{GSVD}-package from Derek Beaton}.
#' \emph{Note: This is a temporary version}.
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
#' boolean, text, or (numeric) vector.
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
#' @param k number of components to return.
#' @author Vincent Guillemot, Derek Beaton, Hervé Abdi
#' @return
#' See #\code{ExPosition::epGPCA} 
#'  \code{ExPosition::corePCA} ad
#'  \code{TExPosition}
#' for details on what is returned.
#' In addition to the values returned:
#' \code{tepRA} returns
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
#' Abdi H., Eslami, A., Guillemot, V., & Beaton D. (2018).
#' Canonical correlation analysis (CCA).
#' In R. Alhajj and J. Rokne (Eds.),
#' \emph{Encyclopedia of Social Networks and Mining (2nd Edition)}.
#' New York: Springer Verlag.
# @importFrom ExPosition epGPCA # not there anymore
#' @import TExPosition
# #' @importFrom TExPosition tepGraphs
#' @export
#' @examples
#' \dontrun{
#' # *** Some example here at some point ***}
tepRA <- function (DATA1, DATA2,
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
    main <- paste("RA: ", deparse(substitute(DATA1)), " & ",
                  deparse(substitute(DATA2)), sep = "")
    DESIGN <- TExPosition::texpoDesignCheck(DATA1, DESIGN,
                               make_design_nominal = make_design_nominal)
    DESIGN <- TExPosition::texpoDesignCheck(DATA2, DESIGN,
                               make_design_nominal = FALSE)
    DATA1 <- as.matrix(DATA1)
    DATA2 <- as.matrix(DATA2)
    DATA1 <- ExPosition::expo.scale(DATA1, scale = scale1, center = center1)
    DATA2 <- ExPosition::expo.scale(DATA2, scale = scale2, center = center2)
    R <- t(DATA1) %*% DATA2
    M <- t(DATA1) %*% DATA1
    Mm1 <-  matrix.exponent(M, power = -1)
    W <- Wm1 <- diag(ncol(DATA2))
    res <-  epGPCA2(DATA = R,
                   k = k,
                   graphs = FALSE,
                   masses = Mm1,
                   weights = Wm1,
                   scale = FALSE,
                   center = FALSE)
    res <- res$ExPosition.Data
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
    #
    res$u <- Mm1 %*% res$pdq$p
    res$v <- Wm1 %*% res$pdq$q
    tepPlotInfo <- TExPosition::tepGraphs(res = res,
                                          DESIGN = DESIGN, main = main,
                                          graphs = graphs)
    #
    return(tepOutputHandler(res = res, tepPlotInfo = tepPlotInfo))
}
