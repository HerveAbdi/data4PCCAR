# Entête ----
#  File for fast inferences for bootstrap ratios
#  for I and J set for plain correspoondence analysis.
#  Based on multinomial resampling
#  Functions in this file:
#  fastBoot4CA  print.BootBrick.ij
#  fastPerm4CA  print.permCA
# Current version is 
# September 27, 2020.
# Hervé Abdi.
#______________________----
# InferenceMultinom4CA
# Functions for Inferences for CA based 
# on the multinomial distribution.
# To be used only
# when the data consists in a real contingency table 
# with a large \eqn{N}.
# Bootstrap estimates are obtained by creating 
# bootstrap contingency tables
# from a multinomial distribution.
# Permutation tests are obtained by creating
# contingency tables matching H0
# (i.e., multinomial with \eqn{Pij = Pi*Pj})
# Permutation tests will not work with MCA though
# because in MCA a variable is coded 
# with a set of 0/1 the columns which are not
# independent (but come in blocks).
#___________________________________________________________
#_____________________-------------------
#___________________________________________________________
# Preamble ----
# Hervé Abdi
# Last Update: November 3, 2016. 
# Revisited October 14, 2017.
# New revision to port to data4PCCAR
# HA: September 27, 2020.
#___________________________________________________________

#___________________________________________________________
# Functions below
#___________________________________________________________
# Help fastBoot4CA ----
# function fastBoot4CA
#' \code{fastBoot4CA}: 
#' create for a Correspondence Analysis 
#' (CA)  a Bootstrap Cube 
#' obtained from bootstrapping the observations
#' from a contingency table.
#' 
#' 
#' \code{fastBoot4CA}:
#' Creates Bootstrap cubes for the *I* and *J* sets 
#' of a CA. The Bootstrap cubes are
#' obtained from bootstrapping the entries/cells
#' of the contingency table. 
#' \code{fastBoot4CA} uses the multinomial distribution 
#' to generate the 
#' Bootstrap samples (function \code{rmultinom})
#' \code{fastBoot4CA} uses the \emph{transition formula} 
#' to get
#' the values of the column factors.
#' Gives also the bootstrap eigenvalues
#' (if \code{eigen = TRUE}).
#' 
#' @details 
#' Note: the \code{rmultinom()} function
#' cannot handle numbers of observations that are too high
#' (i.e., roughly larger than 10^9), so if the table total
#' is larger than 10^8, the table is recoded so that
#' its sum is roughly equal to 10^8. 
#' Planned development: A compact version that gives only
#' bootstrap ratios (not the whole brick  
#' \code{BootstrapBricks}).
#' 
#'  \code{fastBoot4CA} should be used only
#' when the data consists in a real 
#' contingency table with a
#' relatively large \eqn{N}.
#' Bootstrap estimates are obtained by creating 
#' bootstrap contingency tables
#' from a multinomial distribution.
#' Permutation tests are obtained by creating
#' contingency tables matching H0
#' (i.e., multinomial with \eqn{Pij = Pi*Pj})
#' Permutation tests will not work with MCA though
#' because in MCA a variable is coded 
#' with a set of 0/1 columns (complete disjonctive 
#' coding scheme)---A coding scheme which implies that
#' the columns are not
#' independent (because they come in blocks).
#' @param X the data matrix
#' @param Fi (default = \code{NULL}) 
#' the \eqn{I}-set factor scores
#' (for the rows) from the analysis of **X**.
#' if NULL, \code{Boot4RowCA} will compute them.
#' @param Fj (default = \code{NULL}), 
#' the  \eqn{J}-set factor scores
#' (for the columns) from the analysis of **X**.
#' if NULL, \code{Boot4RowCA} will compute them.
#' @param delta (default = \code{NULL}), 
#' the singular values
#' from the CA of **X**. If \code{NULL} (default),
#' \code{Boot4RowCA} will compute them.
#' @param nf2keep How many factors to 
#' keep for the analysis (\code{Default = 3}). 
#' @param nIter (Default = 1000). Number of Iterations 
#' (i.e. number of Bootstrap samples).
#' @param critical.value (\code{Default = 2}).
#' The critical value for a BR to be considered
#' significant. 
#' @param eig if TRUE compute bootstrapped
#' confidence intervals (CIs) for the eigenvalues
#' (default is FALSE).
#' @param alphaLevel the alpha level to compute
#' confidence intervals for the eigenvalues
#' (with CIS at 1-alpha). Default is .05 
#' @return a list with 1) \code{bootCube.i} of
#' Bootstrapped factor scores (*I*-set)
#' 2) 
#'  \code{bootRatios.i}: the bootstrap ratios
#'  (BR)
#'    for
#'  \code{bootRatiosSignificant.i}: the Significant 
#'  BRs; 
#'  a list with \code{bootCube.j}: 
#' An Items * Dimension * Iteration Brick of
#' Bootstrapped factor scores (*J*-set);
#'  \code{bootRatios.j}: the bootstrap ratios (BR);
#'  \code{bootRatiosSignificant.j}: the Significant 
#'  BRs; 
#'  \code{eigenValues} the \code{nIter} * \code{nL} table
#'  of eigenvalues; \code{eigenCIs}: the CIs for the
#'  eigenvalues.
#' @author Hervé Abdi
#' @import ExPosition
#' @importFrom stats rmultinom
#' @export
fastBoot4CA <- function(X,
                       Fi = NULL,
                       Fj = NULL,
                       delta = NULL,
                       nf2keep = 3,
                       nIter = 1000,
                       critical.value = 2,
                       eig = FALSE,
                       alphaLevel = .05){
   # NB Internal function here for coherence
   .boot.ratio.test <- function(boot.cube, critical.value = 2){
    boot.cube.mean <- apply(boot.cube, c(1,2),mean)
    boot.cube.mean_repeat <- array(boot.cube.mean, 
                                   dim =c (dim(boot.cube)))
    boot.cube.dev <- (boot.cube - boot.cube.mean_repeat)^2
    s.boot<-(apply(boot.cube.dev,c(1,2),mean))^(1/2)
    boot.ratios <- boot.cube.mean / s.boot
    significant.boot.ratios <- (abs(boot.ratios) > critical.value)
    rownames(boot.ratios) <- rownames(boot.cube)
    rownames(significant.boot.ratios) <- rownames(boot.cube)
    return(list(
      sig.boot.ratios = significant.boot.ratios,
      boot.ratios     = boot.ratios))
  }
  # 
  # End of .boot.ratio.test
  X <- as.matrix(X)
  #
  # fix the size problem for multinomial
  ndigits <- round(log(sum(X),10))
  if (ndigits > 8 ) {X <- round(X/( 10^(ndigits - 8 ) )) }
  
  
  if (is.null(Fi) | is.null(Fj) | is.null(delta)){
  
    resCA <- ExPosition::epCA(X,graphs = FALSE)
    Fi    <- resCA$ExPosition.Data$fi
    Fj    <- resCA$ExPosition.Data$fj
    delta <- resCA$ExPosition.Data$pdq$Dv
  }
  
  #
  nL <- min(c(length(delta),ncol(Fi)))
  # make sure that dimensions fit
  if ( nf2keep > nL){ nf2keep <-  nL}
     Fi <-  Fi[,1:nf2keep]
     Fj <-  Fj[,1:nf2keep]
  delta <- delta[1:nf2keep]
  #
  # Compute the BootstrapBrick
  #
  # Initialize
  nJ <-  NCOL(X)
  nN <-  sum(X)
  nI <-  NROW(X)
  # J-set
  fj.boot    <- array(NA, dim = c(nJ,nf2keep,nIter)) 
  # Name.
  dimnames(fj.boot)[1] <- list(colnames(X))
  dimnames(fj.boot)[2] <- list(paste0("Dimension ",1:nf2keep))
  dimnames(fj.boot)[3] <- list(paste0("Iteration ", 1:nIter))
  # I-set
  fi.boot    <- array(NA, dim = c(nI,nf2keep,nIter)) 
  # Name.
  dimnames(fi.boot)[1] <- list(rownames(X))
  dimnames(fi.boot)[2] <- list(paste0("Dimension ", 1:nf2keep))
  dimnames(fi.boot)[3] <- list(paste0("Iteration ", 1:nIter))
  
  # Create the matrix of Column Profiles 
  # C <- t(apply(X,2,function(x) x / sum(x)))
  # R <- t(apply(X,1,function(x) x / sum(x)))
  
  # Now Create Fj from Fi and delta
  MatExpansion.j <- matrix(1/delta, nJ,length(delta),
                         byrow = TRUE) 
  MatExpansion.i <- matrix(1/delta, nI,length(delta),
                           byrow = TRUE) 
  # Test the transition formula
  # test.fj <- (C %*%  fi) *  MatExpansion
  # A Bootstrapped version
  # 
  # Loop
  if (eig){# Initialize eigenValues
    maxrank     <- min(NROW(X),NCOL(X)) - 1 
    eigenValues <- matrix(0,nIter, maxrank)
  }
  for (ell in 1:nIter){# ell loop
    # BootIndex <-  sample(nI, replace = TRUE)
    # boot.fj   
    #fj.boot[,,ell] <-  (C[,BootIndex] %*%  
    #                      Fi[BootIndex,]) * MatExpansion
    # BootIndex <-  sample(nI, replace = TRUE)
    # boot.fj   
    Xboot <- matrix(
      as.vector( stats::rmultinom(1, nN,
                           X) ),
      nrow = nI, ncol = nJ, byrow = FALSE)
    # Column Profiles
    C <-  t(apply(Xboot, 2, function(x) x / sum(x)))
        # row Profiles
    R <-  t(apply(Xboot, 1, function(x) x / sum(x)))
    #
    fj.boot[,,ell] <-  (C  %*%  
                          Fi) * MatExpansion.j
    #
    fi.boot[,,ell] <-  (R  %*%  
                          Fj) * MatExpansion.i
    if (eig){
      # Xboot <- X[BootIndex,]
      # Check that there are no zero columns
      Xboot   <- Xboot[,colSums(Xboot) > 0]
      eigenCA <- eigCA(Xboot) 
      # Trick here for the rank of the eigenvalues
      index <- min(maxrank,length(eigenCA))
      eigenValues[ell, 1:index] <- eigenCA[1:index ]
    }
  }# end of ell loop
  # Boot-ratios
  BR.j <- .boot.ratio.test(fj.boot,critical.value)
  BR.i <- .boot.ratio.test(fi.boot,critical.value)
  #
  return.list <- structure(
    list(
      bootstrapBrick.i = fi.boot,
      bootRatios.i =  BR.i$boot.ratios,
      bootRatiosSignificant.i = BR.i$sig.boot.ratios,
      bootstrapBrick.j = fj.boot,
      bootRatios.j = BR.j$boot.ratios,
      bootRatiosSignificant.j = BR.j$sig.boot.ratios
    ),
    class = "bootBrick.ij")
  if (eig){
    # eliminate empty eigenvalues
    eigenValues <- eigenValues[, colSums(eigenValues) > 0]
    return.list$eigenValues <-  eigenValues
    # Get the CI
    # order the eigenvalues to get the CIs
    sortedEigenValues <- apply(eigenValues,2,sort)
    index  <-   round(nIter * (alphaLevel /2))
    if (index == 0) index <- 1
    eigenCI <-  sortedEigenValues[c(index,nIter-(index-1)),]
    return.list$eigenCI <- eigenCI
  } # end if eigen
  return(return.list)
} # eof Boot4RowCA ----
# End of function Boot4RowCA
# *******************************************************************************
#_____________________-------------------
# Help print.bootBrick.ij ----
#' Change the print function for class bootBrick.ij 
#' 
#'  Change the print function for bootBrick.ij
#'  (output of fastBoot4CA)
#'  
#' @param x a list: output of fastBoot4CA
#' @param ... everything else for the function
#' @author Herve Abdi
#' @export
print.bootBrick.ij <- function (x, ...){ 
  ndash = 78 # How many dashes for separation lines
  cat(rep("-", ndash), sep = "")
  cat("\n Bootstraped Factor Scores (BFS) and Bootstrap Ratios  (BR) \n")
  cat(" for the I and J-sets of a CA (obtained from multinomial resampling of X) \n")
  # cat("\n List name: ",deparse(eval(substitute(substitute(x)))),"\n")
  cat(rep("-", ndash), sep = "")
  cat("\n$ bootstrapBrick.i  ", "an I*L*nIter Brick of BFSs  for the I-Set")
  cat("\n$ bootRatios.i      ", "an I*L matrix of BRs for the I-Set")
  cat("\n$ bootRatiosSignificant.i      ", "an I*L logical matrix for significance of the I-Set")
  cat("\n$ bootstrapBrick.j  ", "a  J*L*nIter Brick of BFSs  for the J-Set")
  cat("\n$ bootRatios.j      ", "a  J*L matrix of BRs for the J-Set")
  cat("\n$ bootRatiosSignificant.j      ", "a  J*L logical matrix for significance of the J-Set")
  cat("\n$ eigenValues       ", "an nIter*L matrix of the bootstraped CA eigenvalues")
  cat("\n$ eigenCI           ", "a  2*L with min and max CI for the eigenvalues")
  cat("\n",rep("-", ndash), sep = "")
  cat("\n")
  invisible(x)
} # end of function print.bootBrick.ij 
# eof print.bootBrick.ij ----
#___________________________________________________________

#_____________________-------------------
#___________________________________________________________
# Permutation test for CA
# To be used with matrices analyzed by CA
# based on the multinomial distribution.
# Idea from Greenacre 2016.
# November 1st, 2016
# Hervé Abdi
#********************************************************************
# 
#'Permutation test for CA
#'on real contingency tables
#'obtained from a multinomial distribution.
#'
#' \code{fastPerm4CA} computes a permutation test
#' for CA when CA is performed
#' on a real contingency table.
#' The resampling is obtained 
#' from a multinomial distribution.
#' \code{fastPerm4CA}
#' can be used for big tables
#' for the omnibus test (i.e., inertia) and for 
#' the test on the eigenvalues.
#' @param X the data matrix (non-negative integers)
#' @param compact if \code{TRUE} return only 
#' \emph{p}-values for omnibus test
#' default is \code{FALSE}.
#' @param nIter (Default = 1000). Number of Iterations 
#' (i.e. number of permuted samples computed).
#' @return a list with 
#' \code{fixedInertia}: the CA-inertia of the data matrix;
#' \code{fixedEigenvalues}: the CA-eigenvalues of
#' the data matrix;
#' \code{pOmnibus}: the probability associated
#' to the inertia.
#' If \code{compact} is \code{FALSE}, return also
#' \code{permInertia}: 
#' an \code{nIter} * 1 vector containing the 
#' permuted inertia; 
#' \code{pEigenvalues}: The probabilities 
#' associated to each eigenvalue;
#' If \code{compact} is  \code{FALSE}, return also
#' \code{permEigenvalues}: an
#' \code{nIter} * \code{L} matrix giving
#' the permuted eigenvalues.  
#' @author Hervé Abdi
#' @importFrom stats rmultinom
#' @export

fastPerm4CA <- function(X,
                       nIter = 1000,
                       compact = FALSE){
  nI <- NROW(X)
  nJ <- NCOL(X)
  # End of .boot.ratio.test
  X <- as.matrix(X)
  #
  # fix the size problem for multinomial
  ndigits <- round(log(sum(X),10))
  if (ndigits > 8 ) {X <- round(X/( 10^(ndigits - 8 ) )) }
  #
  maxRank <- min(nI,nJ) - 1
  if (maxRank == 0){stop('X should be a matrix of rank > 1')}
  # First compute the Fixed Effect Inertia
  fixedEigenvalues <- rep(0,maxRank) 
  fixedEV <- eigCA(X)
  # Make sure that the length fit
  if (length(fixedEV) > maxRank){
    fixedEigenvalues <- fixedEV[1:maxRank] 
  }
  if (length(fixedEV) == maxRank){fixedEigenvalues <- fixedEV}
  if (length(fixedEV) < maxRank){
    fixedEigenvalues[1:length(fixedEV)] <- fixedEV 
  }
  
  fixedInertia <- sum(fixedEigenvalues)
  #
  # Initialize
  permInertia     <- rep(NA,nIter)
  permEigenvalues <- matrix(NA, nrow = nIter, ncol = maxRank)
  #
  # Go for the loop
  # Use replicate instead
  # first define the function
  # First X under the null
  nN = sum(X)
  X4H0 <- (1/sum(X)^2) *
    (as.matrix(rowSums(X)) %*% t(as.matrix(colSums(X))))
  .truc <- function(X, longueur = min(dim(X)), nN){
    valP   <- rep(0, longueur)
    #resvp <- .eigCA( apply(X,2,sample ))
    resvp <-    eigCA(matrix(
      as.vector( stats::rmultinom(1, nN,
                           X) ),
      nrow = nI, ncol = nJ, byrow = FALSE))
    valP[1:length(resvp)] <- resvp
    return(valP)
  }
  laLongueur <- maxRank + 1 # to fix rounding error for ev
  permEigenvalues <- replicate(nIter, 
                               .truc(X4H0,laLongueur,nN) )
  
  # print(c('maxRank =',maxRank))
  #  print(head(permEigenvalues))
  #  print(c('Dimension permEigenValues ',
  #         nrow(permEigenvalues),' - ',ncol(permEigenvalues) ) )
  
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
    list(fixedInertia = fixedInertia,
         fixedEigenvalues = fixedEigenvalues,
         pOmnibus = pOmnibus,
         pEigenvalues = pEigenvalues
    ), 
    class = 'perm4CA')
  if (!compact){
    return.list$permInertia     <-  permInertia
    return.list$permEigenvalues <-  permEigenvalues
  }
  return(return.list)
} # End of fastPerm4CA
# eof fastPerm4CA ----
#___________________________________________________________
#_____________________-------------------
# *******************************************************************************
# Help print.perm4CA ----
#' Change the print function for perm4CA class
#' 
#'  Change the print function for perm4CA class
#'  objects
#'  (output of astPerm4CA)
#'  
#' @param x a list: output of perm4RowCA 
#' @param ... everything else for the functions
#' @author Hervé Abdi
#' @export
print.perm4CA <- function (x, ...) { 
  ndash = 78 # How many dashes for separation lines
  cat(rep("-", ndash), sep = "")
  cat("\n Results of Permutation Tests for CA of Matrix X \n")
  cat(" for Omnibus Inertia and Eigenvalues \n")
  # cat("\n List name: ",deparse(eval(substitute(substitute(x)))),"\n")
  cat(rep("-", ndash), sep = "")
  cat("\n$ fixedInertia     ", "the Inertia of Matrix X")
  cat("\n$ fixedEigenvalues ", "a L*1 vector of the eigenvalues of X")
  cat("\n$ pOmnibus         ",  "the probablity associated to the Inertia")
  cat("\n$ pEigenvalues     ", "an L* 1 matrix of p for the eigenvalues of X")
  cat("\n$ permInertia      ", "vector of the permuted Inertia of X")
  cat("\n$ permEigenvalues  ", "matrix of the permuted eigenvalues of X")
  cat("\n",rep("-", ndash), sep = "")
  cat("\n")
  invisible(x)
} # end of function print.perm4CA 
#_____________________-------------------
