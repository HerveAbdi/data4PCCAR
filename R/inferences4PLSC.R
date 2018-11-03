#
# Inference battery for PLSC
# 1. Permutation
# 2. Bootstrap
# Hervé Abdi. Current version November 23, 2016.
# Last edit HA. November 3, 2018.
#_____________________________________________________________________
#_____________________________________________________________________
#'@title  Compute an SCP matrix with several possible
#' combinations of centering and normalizing.
#'
#' @description \code{compS}: Computes an SCP matrix from 2 matrices X and Y
#'  with several possible
#' combinations of centering and normalizing.
#' Both \code{X} and \code{Y} are pre-processed
#' (i.e., centered / normalized)
#' Used for functions related to PLSC /
#' inter-battery analysis / co-inertia analysis...
#' Allows different types of normalization
#' based on the \code{ExPosition} function
#' \code{expo.scale}.
#'
#' @details The options of centering and scaling are almost
#' always used with the defaults, so change them only
#' if you know what you are doing.
#' @param DATA1 an \eqn{N*I} matrix of quantitative data
#' @param DATA2 an \eqn{N*J} matrix of quantitative data
#' @param center1 when \code{TRUE} (default) \code{DATA1}
#' will be centered
#' @param center2 when \code{TRUE} (default) \code{DATA2}
#' will be centered
#' @param scale1 when \code{TRUE} (default) \code{DATA1}
#' will be normalized. Depends upon \code{ExPosition}
#' function \code{expo.scale} whose description is:
#'boolean, text, or (numeric) vector.
#'If boolean or vector,
#'it works just like  \code{scale}.
#'The following text options are available:
#' \code{'z'}: z-score normalization,
#' \code{'sd'}: standard deviation normalization,
#' \code{'rms'}: root mean square normalization,
#'  \code{'ss1'}: sum of squares
#'  (of columns) equals 1
#'  (i.e., column vector of length of 1).
#' @param scale2 when \code{TRUE} (default) \code{DATA2}
#' will be normalized
#' (same options as for \code{scale1}).
#' @author Hervé Abdi
#' @return \code{S} the cross-product matrix from
#' \code{X} and \code{Y}.
#'@importFrom stats cor
#'@importFrom ExPosition expo.scale
#' @examples
#' S <- compS(matrix(stats::runif(10), nrow = 5), matrix(stats::runif(15), nrow = 5))
#' @rdname compS
#' @export
#'
compS <- function(DATA1,
                  DATA2,
                  center1 = TRUE,
                  center2 = TRUE,
                  scale1 =  'ss1',
                  scale2 =  'ss1'
                   ){
   X <- DATA1
   Y <- DATA2
   if (center1 & center2
            & (scale1 == 'ss1')
            & (scale2 == 'ss1') ){
                  S = stats::cor(X,Y) } else {
              Xc <- ExPosition::expo.scale(X, center = center1,
                                               scale = scale1)
              Yc <- ExPosition::expo.scale(Y, center = center2,
                                               scale = scale2)
              S <- t(Xc) %*% Yc
                  }

       return(S)
} # end of function compS
#_____________________________________________________________________
#_____________________________________________________________________
#_____________________________________________________________________
# sv2 preamble ----
#' @title Compute the squared singular values of a matrix.
#' @description \code{sv2}: computes the squared singular values
#' of a matrix.
#' @param X a rectangular matrix (or dataframe)
#' @return a vector of the squared singular values.
#' @details \code{sv2} is wraper around \code{eigen}, it is used
#' for permutation and boostsrap procedures.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  squared.ev <- sv2(matrix(runif(15), nrow = 3))
#'  }
#' }
#' @author Hervé Abdi
#' @rdname sv2
#' @export
sv2 <- function(X){
  X <- as.matrix(X)
  if (NROW(X) > NCOL(X)) {X = t(X)}
  return(eigen(X %*% t(X), symmetric = TRUE, only.values = TRUE)$values)
}

#_____________________________________________________________________
#' @title Permutation for PLSC (as implemented
#' in \code{TExPosition::tepPLS})
#'
#' @description \code{perm4PLSC}: Permutation for PLSC (as implemented
#' in \code{TExPosition::tepPLS}).
#' Computes an omnibus permutation test and
#' specific tests for the eigenvalues when
#' performing a PLSC from
#' 2 matrices \eqn{X} and \eqn{Y}.
#' Several possible
#' combinations of centering and normalizing
#' are possible (see paramater \code{scale1,
#' scale2, center2, scale2}).
#' Used for functions related to PLSC /
#' inter-battery analysis / co-inertia...
#' The different types of normalization are
#' based on the \code{ExPosition::expo.scale}
#' function. Two different permutation schemes
#' are currently available (see parameter
#' \code{permType}).
#' @param DATA1 an \eqn{N*I} matrix of quantitative data
#' @param DATA2 an \eqn{N*J} matrix of quantitative data
#' @param center1 when \code{TRUE} (default) \code{DATA1}
#' will be centered
#' @param center2 when \code{TRUE} (default) \code{DATA2}
#' will be centered
#' @param scale1 when \code{TRUE} (default) \code{DATA1}
#' will be normalized. Depends upon \code{ExPosition}
#' function \code{expo.scale} whose description is:
#'boolean, text, or (numeric) vector.
#'If boolean or vector,
#'it works just as scale.
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
#' @param nIter (Default = \code{1000}). Number of Iterations
#' (i.e., number of permuted samples computed).
#' @param permType what type of permutation is used
#' if \code{'byMat'} (default) only the labels of the observations
#' are permuted, other option is \code{'byColumns'} then
#' all columns of each matrix are independently
#' permuted.
#' @param compact if \code{TRUE}
#' (Default) return only \eqn{p}-values for omnibus test.
#' @return a list with
#' \code{fixedInertia}: the inertia of
#' \eqn{X'Y} data matrix (i.e., sums of squares)
#' \code{fixedEigenvalues}: the  eigenvalues of
#' the \eqn{X'Y}  matrix;
#' \code{pOmnibus}: the probability associated
#' to the inertia.
#' If \code{compact} is \code{FALSE}, return also
#' \code{permInertia}:
#' an \code{nIter} * 1 vector containing the
#' permuted inertia;
#' \code{pEigenvalues}: The probabilites
#' associated to each eigenvalue;
#' If \code{compact} is \code{FALSE}, returns also
#' \code{permEigenvalues}: an
#' \code{nIter} * \code{L} matrix giving
#' the permuted eigenvalues.
#' @author Hervé Abdi
#' @rdname perm4PLSC
#' @seealso  compS
#' @export

perm4PLSC <- function(DATA1,
                  DATA2,
                  center1 = TRUE,
                  center2 = TRUE,
                  scale1 =  'ss1',
                  scale2 =  'ss1',
                  nIter = 1000,
                  permType = 'byMat' , # 'byColumns
                  compact = FALSE
){
  if (permType != 'byColumns') permType <- 'byMat'
  DATA1 <- as.matrix(DATA1)
  DATA2 <- as.matrix(DATA2)
  X = DATA1
  Y = DATA2
  if (NCOL(X) > NCOL(Y)){
      X = DATA2
      Y = DATA1
      }

  nN <- NROW(X)
  nI <- NCOL(X)
  nJ <- NCOL(Y)
  if( !(nN == NROW(Y))){stop('DATA1 and DATA2 non-conformable')}
  maxRank <- min(nI,nJ)
  # Compute fixed SCP matrix for X & Y
  Sfixed = compS(DATA1,
            DATA2,
            center1 = center1,
            center2 = center2,
            scale1 =  scale1, #   'ss1' ,
            scale2 =  scale2)
  fixedEigenvalues <- rep(0,maxRank)
  fixedEV <- sv2(Sfixed)
  # fixedEV <- eigen(t(Sfixed) %*% (Sfixed),
  #                  symmetric   = TRUE,
  #                  only.values = TRUE)$values
  # Make sure that the length fit
  if (length(fixedEV) > maxRank){
    fixedEigenvalues <- fixedEV[1:maxRank]
  }
  if (length(fixedEV) == maxRank){fixedEigenvalues <- fixedEV}
  if (length(fixedEV) < maxRank){
    fixedEigenvalues[1:length(fixedEV)] <- fixedEV
  }
  fixedInertia <- sum(fixedEigenvalues)
  # The random permutations below
  # Initialize
  permInertia     <- rep(NA,nIter)
  permEigenvalues <- matrix(NA, nrow = nIter, ncol = maxRank)
  #
  # Use replicate
  # first define the function
  .truc <- function(X,Y,
                    longueur = min(c(dim(X),NCOL(Y))),
                    permType = permType){
     valP   <- rep(0, longueur)
     #resvp <- .eig4CA( apply(X,2,sample ))
     if ( permType == 'byMat'){
       Xrand <- X[sample(nN),]
       Yrand <- Y
     }
     if ( permType == 'byColumns'){
       Xrand <- apply(X,2,sample )
       Yrand <- apply(Y,2,sample )
     }
     Srand <- compS(Xrand,Yrand)
     resvp <- sv2(Srand)
     # resvp <-   eigen(t(Srand) %*% Srand,
     #                 symmetric = TRUE,
     #                 only.values = TRUE)$values
    valP[1:length(resvp)] <- resvp
    return(valP)
          }
  laLongueur <- maxRank + 1 # to fix rounding error for ev
  permEigenvalues <- replicate(nIter,
                               .truc(X,Y,laLongueur,permType) )
  permEigenvalues <- t(permEigenvalues[1:maxRank,])
  # Done without a loop!
  permInertia = rowSums(permEigenvalues)
  #
  pOmnibus = sum(permInertia > fixedInertia) / nIter
  if (pOmnibus == 0) pOmnibus <- 1/nIter # no 0
  pEigenvalues <- rowSums( t(permEigenvalues) >
                             (fixedEigenvalues)) / nIter
  pEigenvalues[pEigenvalues == 0 ] <- 1/nIter
  return.list <- structure(
    list(fixedInertia     = fixedInertia,
         fixedEigenvalues = fixedEigenvalues,
         pOmnibus         = pOmnibus,
         pEigenvalues     = pEigenvalues
    ),
    class = 'perm4PLSC')
  if (!compact){
    return.list$permInertia =  permInertia
    return.list$permEigenvalues = permEigenvalues
  }
  return(return.list)
} # End of function perm4PLSC

# *******************************************************************************
#' Change the print function for \code{perm4PLSC} class
#'
#'  Change the print function for \code{perm4PLSC} class
#'  objects
#'  (output of Perm4PLSC).
#'
#' @param x a list: output of perm4PLSC
#' @param ... everything else for the functions
#' @author Hervé Abdi
#' @export
print.perm4PLSC <- function(x, ...){
  ndash = 78 # How many dashes for separation lines
  cat(rep("-", ndash), sep = "")
  cat("\n Results of Permutation Test for PLSC of X'*Y = R \n")
  cat(" for Omnibus Inertia and Eigenvalues \n")
  # cat("\n List name: ",deparse(eval(substitute(substitute(x)))),"\n")
  cat(rep("-", ndash), sep = "")
  cat("\n$ fixedInertia     ", "the Inertia of Matrix X")
  cat("\n$ fixedEigenvalues ", "an L*1 vector of the eigenvalues of X")
  cat("\n$ pOmnibus         ", "the probablity associated to the Inertia")
  cat("\n$ pEigenvalues     ", "an L* 1 matrix of p for the eigenvalues of X")
  cat("\n$ permInertia      ", "vector of the permuted Inertia of X")
  cat("\n$ permEigenvalues  ", "matrix of the permuted eigenvalues of X")
  cat("\n",rep("-", ndash), sep = "")
  cat("\n")
  invisible(x)
} # end of function print.perm4PLSC
#_____________________________________________________________________

#_____________________________________________________________________
# Bootstrap here
#

#_____________________________________________________________________
# function Boot4PLSC
#' @title  Create a Bootstrap Cube for PLSC
#'
#' @description  \code{Boot4PLSC}:
#' Creates Bootstrap Bricks for the \eqn{I} and \eqn{J} sets
#' of a PLSC. The bricks are
#' obtained from bootstraping the rows
#' of the two data-tables used for PLSC.
#' Uses the "transition formula" to get
#' the values of the rows and columns loadings
#' from multiplication of the latent variables.
#' Gives also the bootstraped eigenvalues
#' (if \code{eigen = TRUE}).
#'
#' @details
#' \emph{Note}:  \code{Boot4PLSC} gives the
#' \emph{eigenvalues} of the matrix
#' \eqn{X'Y} even though PLSC
#' works with the \emph{singular values}
#' (i.e., the square roots of the eigenvalues) of
#' \eqn{X'Y}. The eigenvalues were chosen because their sum is the
#' sum of squares (i.e., Inertia) of \eqn{X'Y}.
#'
#' \emph{Planned development:} A compact version that gives only
#' bootstrap ratios (not BootstrapBricks).
#'
#' @param DATA1 an \eqn{N*I}  data matrix
#' @param DATA2 an \eqn{N*J}  data matrix
#' (measured on the same observations as \code{DATA2})
#' @param center1 when \code{TRUE} (default) \code{DATA1}
#' will be centered
#' @param center2 when \code{TRUE} (default) \code{DATA2}
#' will be centered
#' @param scale1 when \code{TRUE} (default) \code{DATA1}
#' will be normalized. Depends upon
#' function \code{scale0} whose description is:
#' boolean, text, or (numeric) vector.
#'If boolean or vector,
#'it works just as scale.
#'The following text options are available:
#' \code{'z'}: \eqn{z}-score normalization,
#' \code{'sd'}: standard deviation normalization,
#' \code{'rms'}: root mean square normalization,
#'  \code{'ss1'}: sum of squares
#'  (of columns) equals 1
#'  (i.e., each column vector has length of 1).
#' @param scale2 when \code{TRUE} (default) \code{DATA2}
#' will be normalized
#'  (same options as for \code{scale1}).
#' @param Fi (Default = \code{NULL}), the \eqn{I} factor scores
#' for the columns of \code{DATA1}.
#' if \code{NULL},  \code{Boot4RowCA} computes them..
#' @param Fj = (Default = \code{NULL}, the \eqn{J} factor scores
#' for the columns of \code{DATA2}.
#' if \code{NULL} the function
#' \code{Boot4RowCA} computes them.
#' @param nf2keep How many factors to
#' keep for the analysis (Default = \code{3}).
#' @param nIter (Default = \code{1000}). Number of Iterations
#' (i.e., number of Bootstrtap samples).
#' @param critical.value (Default = \code{2}).
#' The critical value for a \code{BR} to be considered
#' significant.
#' @param eig if \code{TRUE} compute bootstraped
#' confidence intervals (CIs) for the eigenvalues
#' (default is \code{FALSE}).
#' @param alphaLevel the alpha level used to compute
#' the confidence intervals for the eigenvalues
#' (with CIS at 1-alpha). Default is \code{.05}
#' @return a list with
#' \itemize{
#' \item{\code{bootstrapBrick.i}: }{the
#'  the \code{I * Dimensions * Iterations} Brick of
#'      Bootstraped factor scores  for the \eqn{I}-set;}
#'  \item{\code{bootRatios.i}: }{the bootstrap ratios
#'      for the \eqn{I}-set;}
#' \item{\code{bootRatiosSignificant.i}: }{the Significant
#'    BRs for the \eqn{I}-set;}
#' \item{\code{bootstrapBrick.j}: }{
#'     the \code{J * Dimensions * Iterations} Brick of
#'     Bootstraped factor scores for the \eqn{J}-set;}
#' \item{\code{bootRatios.j}: }{the bootstrap ratios for the \eqn{J}-set;}
#' \item{\code{bootRatiosSignificant.j}: }{the Significant
#'    BRs for the \eqn{J}-set;}
#'    }
#'    In addition if \code{eig = TRUE}, the list includes:
#'\itemize{
#' \item{\code{eigenValues}: }{the \code{nIter * nL} table
#'  of eigenvalues;}
#'\item{\code{fixedEigenvalues}: }{the eigenvalues of
#'   matrix n\eqn{X'Y}.}
#'  \item{\code{eigenCIs}: }{the CIs for the
#'  eigenvalues.}
#'  }
#' @author Hervé Abdi
#' @rdname Boot4PLSC
#' @export
#'
Boot4PLSC <- function(DATA1, DATA2,
                     center1 = TRUE,
                     center2 = TRUE,
                      scale1 = 'ss1',
                      scale2 = 'ss1',
                          Fi = NULL,
                          Fj = NULL,
                     nf2keep = 3,
                       nIter = 1000,
              critical.value = 2,
                         eig = FALSE,
                  alphaLevel = .05){# start function BootPLSC
  # NB Internal functions here for coherence
  .boot.ratio.test <- function(boot.cube,
                               critical.value=2){
    boot.cube.mean <- apply(boot.cube,c(1,2),mean)
    boot.cube.mean_repeat <- array(boot.cube.mean,
                                dim=c(dim(boot.cube)))
    boot.cube.dev <- (boot.cube - boot.cube.mean_repeat)^2
    s.boot<-(apply(boot.cube.dev,c(1,2),mean))^(1/2)
    boot.ratios <- boot.cube.mean / s.boot
    significant.boot.ratios <- (abs(boot.ratios) > critical.value)
    rownames(boot.ratios) <- rownames(boot.cube)
    rownames(significant.boot.ratios) <- rownames(boot.cube)
    return(list(sig.boot.ratios=significant.boot.ratios,
                boot.ratios=boot.ratios))
      } # end of boot.ratio.test
  #
  # End of .boot.ratio.test
  nN = NROW(DATA1)
  if (nN != NROW(DATA2)){stop('input matrices not conformable')}
  # below is replaced by scale0
  # X <- ExPosition::expo.scale(DATA1, center = center1,
  #                              scale = scale1)
  # Y <- ExPosition::expo.scale(DATA2, center = center2,
  #                              scale = scale2)
  X <- apply(DATA1,2, scale0, center = center1, scale = scale1)
  Y <- apply(DATA2,2, scale0, center = center2, scale = scale2)
  nI = NCOL(X)
  nJ = NCOL(Y)
  maxRank <- min(nI,nJ)
  if (maxRank < nf2keep) nf2keep = maxRank
  if  ( is.null(Fi) | is.null(Fj) ){
  # compute Fi and Fj
    S <- t(X) %*% Y
    svd.S <- svd(S, nu = nf2keep, nv = nf2keep)
    if (nf2keep > length(svd.S$d)) nf2keep = length(svd.S$d)
    Lx <- X %*% svd.S$u
    Ly <- Y %*% svd.S$v
    Fi <- svd.S$u * matrix(svd.S$d,nI,nf2keep,byrow = TRUE)
    Fj <- svd.S$v * matrix(svd.S$d,nJ,nf2keep,byrow = TRUE)
  } else { # Compute lx and ly from Fi and Fj
    nL = min(NCOL(Fi),NCOL(Fj))
    if (nL < nf2keep) nf2keep = nL
    Fi = Fi[,1:nf2keep]
    Fj = Fj[,1:nf2keep]
    delta.inv <- 1 / sqrt(colSums(Fi^2))
    Lx <-  X %*% (Fi * matrix(delta.inv,nI,nf2keep,byrow = TRUE) )
    Ly <-  Y %*% (Fj * matrix(delta.inv,nJ,nf2keep,byrow = TRUE) )
  }
  # Now we have Lx Ly Fi and Fj
  #
  # J-set
  fj.boot    <- array(NA, dim = c(nJ,nf2keep,nIter))
  # Name.
  dimnames(fj.boot)[1] <- list(colnames(Y))
  dimnames(fj.boot)[2] <- list(paste0("Dimension ", 1: nf2keep))
  dimnames(fj.boot)[3] <- list(paste0("Iteration ", 1:nIter))
  # I-set
  fi.boot    <- array(NA, dim = c(nI,nf2keep,nIter))
  # Name.
  dimnames(fi.boot)[1] <- list(colnames(X))
  dimnames(fi.boot)[2] <- list(paste0("Dimension ", 1:nf2keep))
  dimnames(fi.boot)[3] <- list(paste0("Iteration ", 1:nIter))
  if (eig){# if
            eigenValues <- matrix(0, nrow = nIter, ncol = maxRank )
            colnames(eigenValues) <- paste0("Dimension ",1: maxRank)
            rownames(eigenValues) <- paste0("Iteration ", 1:nIter)
            fixedEigenvalues <-  sv2(compS(
                X, center1 = center1, scale1 = scale1,
                Y, center2 = center2, scale2 = scale2) )
            names(fixedEigenvalues) <- paste0('Dimension ',
                                          1:length(fixedEigenvalues))
           } # end if
  for (ell in 1:nIter){# ell loop
   boot.index <- sample(nN, replace = TRUE)
   fi.boot[,,ell] <- t(X[boot.index,]) %*% Ly[boot.index,]
   fj.boot[,,ell] <- t(Y[boot.index,]) %*% Lx[boot.index,]
   # eigenvalues
   if (eig){# get the eigenvalues ----
     eigenS <- sv2(compS(
                X[boot.index,], center1 = center1, scale1 = scale1,
                Y[boot.index,], center2 = center2, scale2 = scale2) )
   #   # Trick here for the rank of the eigenvalues
    index <- min(maxRank,length(eigenS))
    eigenValues[ell,1:index] <- eigenS
       } # end if eig
  } # end ell loop
  # Boot-ratios
  BR.j <- .boot.ratio.test(fj.boot,critical.value)
  BR.i <- .boot.ratio.test(fi.boot,critical.value)
  #
  return.list <- structure(
    list(
      bootstrapBrick.i =     fi.boot,
      bootRatios.i =  BR.i$boot.ratios,
      bootRatiosSignificant.i =
        BR.i$sig.boot.ratios,
      bootstrapBrick.j =     fj.boot,
      bootRatios.j =  BR.j$boot.ratios,
      bootRatiosSignificant.j =
        BR.j$sig.boot.ratios),
      class = "bootBrick.ij4plsc")
## Code Below taken from BOOTCA. To be used
## to implement the eig option later
if (eig){#add eig
   # eliminate empty eigenvalues
   eigenValues <- eigenValues[, colSums(eigenValues) > 0]
   return.list$eigenValues = eigenValues
   # Get the CI
   #  order the eigenvalues to get the CIs
   sortedEigenValues <- apply(eigenValues,2,sort)
   index  =  round(nIter * (alphaLevel / 2))
   if (index == 0) index <- 1
   eigenCI = sortedEigenValues[c(index,nIter - (index - 1)),]
   minCI <- as.character(alphaLevel / 2)
   substr(minCI,1,2) <- "_"
   minCI <- paste0("LB",minCI)
   maxCI <- as.character(1 - (alphaLevel / 2))
   substr(maxCI,1,2) <- "_"
   maxCI <- paste0("UB",maxCI)
   rownames(eigenCI) <- c(minCI,maxCI)
   return.list$fixedEigenvalues <- fixedEigenvalues
   return.list$eigenCI <- eigenCI
   class(return.list) <- "bootBrick.ij.eig4plsc"
      } # end if eigen
  return(return.list)
} # End of Function

#_____________________________________________________________________
#' Change the print function for class bootBrick.ij4plsc
#'
#'  Change the print function for bootBrick.ij4plsc
#'  (output of Boot4MultCA)
#'
#' @param x a list: output of Boot4PLSC
#' @param ... everything else for the function
#' @author Herve Abdi
#' @export
print.bootBrick.ij4plsc <- function(x, ...) {
  ndash = 78 # How many dashes for separation lines
  cat(rep("-", ndash), sep = "")
  cat("\n Bootstraped Factor Scores (BFS) and Bootstrap Ratios  (BR) \n")
  cat(" for the I and J-sets of a PLSC (obtained from multinomial resampling of X & Y) \n")
  # cat("\n List name: ",deparse(eval(substitute(substitute(x)))),"\n")
  cat(rep("-", ndash), sep = "")
  cat("\n$ bootstrapBrick.i         ", "an I*L*nIter Brick of BFSs  for the I-Set")
  cat("\n$ bootRatios.i             ", "an I*L matrix of BRs for the I-Set")
  cat("\n$ bootRatiosSignificant.i  ", "an I*L logical matrix for significance of the I-Set")
  cat("\n$ bootstrapBrick.j         ", "a  J*L*nIter Brick of BFSs  for the J-Set")
  cat("\n$ bootRatios.j             ", "a  J*L matrix of BRs for the J-Set")
  cat("\n$ bootRatiosSignificant.j  ", "a  J*L logical matrix for significance of the J-Set")
 #  cat("\n$ eigenValues          ", "a  nIter*L matrix of the bootstraped CA eigenvalues")
 #  cat("\n$ eigenCI              ", "a  2*L with min and max CI for the eigenvalues")
  cat("\n",rep("-", ndash), sep = "")
  cat("\n")
  invisible(x)
} # end of function print.bootBrick.ij
#_____________________________________________________________________
#_____________________________________________________________________
#' Change the print function for class bootBrick.ij.eig4plsc
#'
#'  Change the print function for objects
#'  of the class \code{bootBrick.ij.eig4plsc}
#'  (output of \code{Boot4PLSC})
#'
#' @param x a list: output of \code{Boot4PLSC}
#' @param ... everything else for the function
#' @author Hervé Abdi
#' @export
print.bootBrick.ij.eig4plsc <- function(x, ...) {
  ndash = 78 # How many dashes for separation lines
  cat(rep("-", ndash), sep = "")
  cat("\n Bootstraped Factor Scores (BFS), Bootstrap Ratios (BR), eigenvalues \n")
  cat(" for the I and J-sets of a PLSC (obtained from bootstrapping X & Y) \n")
  # cat("\n List name: ",deparse(eval(substitute(substitute(x)))),"\n")
  cat(rep("-", ndash), sep = "")
  cat("\n$ bootstrapBrick.i        ", "an I*L*nIter Brick of BFSs  for the I-Set")
  cat("\n$ bootRatios.i            ", "an I*L matrix of BRs for the I-Set")
  cat("\n$ bootRatiosSignificant.i ", "an I*L logical matrix for significance of the I-Set")
  cat("\n$ bootstrapBrick.j        ", "a  J*L*nIter Brick of BFSs  for the J-Set")
  cat("\n$ bootRatios.j            ", "a  J*L matrix of BRs for the J-Set")
  cat("\n$ bootRatiosSignificant.j ", "a  J*L logical matrix for significance of the J-Set")
  cat("\n$ eigenValues             ", "a  nIter*L matrix of the bootstraped CA eigenvalues")
  cat("\n$ fixedEigenvalues        ", "a  1*L vector of the fixed eigenvalues")
  cat("\n$ eigenCI                 ", "a  2*L matrix with min and max CI for the eigenvalues")
  cat("\n",rep("-", ndash), sep = "")
  cat("\n")
  invisible(x)
} # end of function print.bootBrick.ij
#_____________________________________________________________________


