#___________________________________________________________
# file fastInferences4CA.R
# Created 09/27/2020
# Fast inferences for CA based on multinomial distribution
# includes bootstrap and permutation tests.
#  Current Version 09/27/2020
# __________________________________________________________
# Preamble ----
# File for fast Inferences in CA 
# An example on how to compute (fast)
# permuted and bootstrap values 
# for the CA of a data table
# Functions to be ported to data4PCCAR
# Current functions ----
# eigCA
# malinvaudQ4CA.perm
# eigCA4Multinom
# multinomCV4CA
# print.Inference4CA
#
# **********************************************************
# The functions starts here ----
# __________________________________________________________
# Preamble eigCA ----
# First a nice function to get only the CA eigenvalues 
# from a data matrix
# 
#___________________________________________________________ 
# eigCA a function to compute 
#  the CA-eigenvalues for a matrix
#  if eig.only is FALSE, eigCA will also give Fi and Fj
#     (Useful for Booststraping Fi & Fj)
# help starts here 
# ________________
# eigCA
#' @title A bare-bone function to compute  the
#' eigen-values (and if asked row and column factor scores)
#' of a matrix suitable for correspondence analysis.
#' @description \code{eigCA}:
#' A very fast and bare-bone function that computes
#' the eigenvalues 
#' (and possibly the row and column factor scores)
#' of the Correspondence Analysis (CA)
#' of a data matrix suitable for CA
#' (i.e., a matrix whose all entries are non-negative).
#' \code{eigCA} is mainly used for cross-validation
#' and resampling methods (e.g., permutation and
#' bootstrap tests).
#' @param Xdata a data matrix 
#' (whose all entries are non-negative) suitable
#' for correspondence analysis.
#' @param eig.only when \code{TRUE} (Default) 
#' compute only the CA- eigen-values of \code{X}.
#' Otherwise compute also the row and column
#' CA factor scores (i.e., \code{fi} and \code{fj}).
#' @return if \code{eig.only} is \code{TRUE}
#' \code{eigCA} returns the CA-eigenvalues of
#' \code{X}; 
#' if \code{eig.only} is \code{FALSE}
#' \code{eigCA} returns 
#' a list with: \code{$eigen}: the CA-eigenvalues of
#' \code{X}, \code{$fi}: the CA row factor scores,
#' and \code{$fi}: the CA column factor scores.
#' @details As a fast bare-bones CA based computations
#' \code{eigCA} is mainly used for 
#' cross-validation for CA methods.
#' @author  Hervé Abdi
#' @examples 
#' \dontrun{
#' set.seed(87) # set the seed
#' X <- matrix(round(runif(21)*20), ncol = 3) # good for CA
#' eigenOfX <- eigCA(X)
#' }
#' @rdname eigCA
#' @export 
eigCA <- function(Xdata, # data matrix
            eig.only = TRUE # we just want the eigenvalues
                   ){# function starts here
  nI <-  nrow(Xdata)
  nJ <-  ncol(Xdata)
  Fliped = FALSE # Did we flip the matrix?
  # make sure that we work on the smallest side
  if (nI > nJ){Xdata <-  t(Xdata)
               truc <-  nI
               nI <-  nJ
               nJ <-  truc
               Fliped <-  TRUE}
  Y <-  Xdata / sum(Xdata) # stochastic matrix
  y.ip <-  rowSums(Y)
  y.pj <-  colSums(Y)
  # long way with diag to check computation     
  # Y4eig      = diag(1/sqrt(y.ip))%*% Y %*% diag(1/sqrt(y.pj))
  # rewrite Y4eig in a better way a la repmat!
  Y4eig <-   matrix((1/sqrt(y.ip)),nI,nJ) * Y *
    matrix((1/sqrt(y.pj)),nI,nJ,byrow = TRUE)
  # ^ this could be done faster I think
  if (eig.only){
    S4eig <-  Y4eig %*% t(Y4eig)
    Y.eig <-  eigen(S4eig, symmetric = TRUE, 
                       only.values = TRUE)
    # check.svd = svd(Y4eig) to double check
    return(Y.eig$values[-1]) # ignore the first trivial eigenvalue
  } else {# go for ze svd            
    Y.svd <- svd(Y4eig,nu = min(c(nI,nJ)))
    Y.eig <- Y.svd$d^2 # eigenvalue
    nL <- length(Y.eig)
    Fi <- matrix((1/sqrt(y.ip)),nI,nL) * Y.svd$u[,1:nL] * 
      matrix(Y.svd$d,nI,nL,byrow = TRUE)
    Fj <- matrix((1/sqrt(y.pj)),nJ,nL) * Y.svd$v[,1:nL] *
      matrix(Y.svd$d,nJ,nL,byrow = TRUE)
    if (Fliped){ truc <-  Fj
                 Fj   <-  Fi
                 Fi   <-  truc} # if Fliped, unFliped 
    ShortCA = list(eigen = Y.eig[-1],
                      Fi = Fi[,-1],
                      Fj = Fj[,-1]) # drop first dim
    return(ShortCA)
  }
} # end of function eigCA
# eof eigCA ----
#___________________________________________________________
# Preamble malinvaudQ4CA.perm  ----
# malinvaudQ4CA.perm computes the Malinvaud/Saporta test for CA
#                   with asymptotic and permutation derived p-values
# Help here 
# ___________
# 
#' @title Compute the Malinvaud/Saporta test for
#' the omnibus and dimensions of a correspondence 
#' analysis.
#' @description \code{malinvaudQ4CA.perm}:
#' Computes the Malinvaud / Saporta test for
#' the omnibus and dimensions of a correspondence 
#' analysis. \code{malinvaudQ4CA.perm} gives
#' the asymptotic Chi2 values and their associated 
#' \emph{p}-value under the usual assumptions.
#' If provided with permuted 
#' CA-eigenvalues, \code{malinvaudQ4CA.perm}
#' will report the \emph{p}-value obtained from
#' the permutation test.
#' @param Data A matrix suitable for 
#' correspondence analysis 
#' (i.e., with all non-negative elements).
#' @param LesEigPerm 
#' the permuted eigenvalues
#' as a \code{niteration} times rank of the
#' data  matrix.
#' if  \code{LesEigPerm} is \code{NULL}
#' (Default), \code{malinvaudQ4CA.perm}
#' will only compute the normal 
#' (i.e., chi2 based) approximation.
#' @param ndigit4print (Default: 4), 
#' number of significant 
#' digits to use to report the results.
#' @return A (self-explanatory) 
#' table with the results of the test
#' @details DETAILS
#' @author  Hervé Abdi
#' @references 
#' The original work is described in a rather
#' hard to find paper (published in the proceeding
#' of a meeting) by Malinvaud:
#' 
#' Malinvaud, E. (1987). 
#' Data Analysis in applied socio-economic statistics
#' with special considerations of correspondence analysis.
#' \emph{Marketing Science Conference Proceedings},
#'  HEC-ISA, Jouy-en-Josas.
#'  
#' A synthesis of the method with additional information 
#' can be found  
#' in 
#' 
#' Saporta (2011). \emph{Probabilité 
#' et Analyse des Données (3rd Ed)}.
#'    Technip, Paris. p. 209.  
#' @examples 
#' \dontrun{
#' set.seed(87) # set the seed
#' X <- matrix(round(runif(21)*20), ncol = 3) # good for CA
#' resMalin <- malinvaudQ4CA.perm(X)
#' }
#' @importFrom stats pchisq
#' @rdname malinvaudQ4CA.perm
#' @export 
malinvaudQ4CA.perm <- function(
          Data, # The original Data Table
          # Val.P = NULL, # fixed eigenvalues
          #  e.g. resFromExposition$ExPosition.Data$eigs
          # Output from ExPosition 
          LesEigPerm = NULL, # the permuted eigenvalues
          # as a niteration * rank of X matrix
          # -> to add if LesEigPerm is NULL skip
          # the permutation part 
          # and compute normal approximation
          ndigit4print = 4 # how many digits for printing
){# Function begins here
  # References:
  # Malinvaud, E. (1987). Data Analysis in applied socio-economic statistics
  # with special considerations of correspondence analysis.
  # Marketing Science Conference Proceedings, HEC-ISA, Jouy-en-Josas.
  # Also cited in Saporta (2011). Probabilité et Analyse des Données (3rd Ed).
  #    Technip, Paris. p. 209.  
  #   Val.P <- ResFromExposition$ExPosition.Data$eigs
  N.pp  <- sum(Data) # Grand Total of the data table
  # if(is.null(Val.P)){Val.P = eigCA(Data)}
  # OLd Version
  # Compute eigenvalues if not given
  Val.P = eigCA(Data) # Now compute the eigenvalues
  nL <- length(Val.P) # how many eigenvalues
  nI <- nrow(Data)
  nJ <- ncol(Data)
  Q     <- N.pp * cumsum(Val.P[nL:1])[nL:1]
  Q.nu  <- (nI - 1:nL)*(nJ - 1:nL)  
  # Get the values from Chi Square 
  pQ = 1 - stats::pchisq(Q,Q.nu) 
  # Add NA to make clear that the last 
  #     dimension is not tested
  Le.Q = c(Q,NA)
  Le.pQ = c(pQ,NA)
  Le.Q.nu = c(Q.nu,0)
  #
  NamesOf.Q = c('Ho: Omnibus', paste0('Dim-',seq(1:nL)))
  names(Le.Q)    <- NamesOf.Q
  names(Le.pQ)   <- NamesOf.Q
  names(Le.Q.nu) <- NamesOf.Q
  # Now compute the probability from permutation test
  #     from InPosition
  # LesEigPerm <-  Eigen.perm
  # ^ to make life easier
  if (!is.null(LesEigPerm)){# If LesEigPerm exist get p-values
    Q.perm     <-  N.pp * t(apply( LesEigPerm[,nL:1], 1, cumsum) )[,nL:1]
    #
    Logical.Q.perm =  t(t(Q.perm) >  (Q))
   pQ.perm = colMeans(Logical.Q.perm)
   pQ.perm[pQ.perm == 0] = 1 / nrow(Q.perm)
   Le.pQ.perm = c(pQ.perm,NA)
   # return the table
   Malinvaud.Q = data.frame(matrix(c(
    c(round(c(sum(Val.P),Val.P),digits = ndigit4print) ),
    round(Le.Q,      digits = ndigit4print),
    round(Le.pQ,     digits = ndigit4print),
    round(Le.Q.nu,   digits = ndigit4print),
    round(Le.pQ.perm,digits = ndigit4print)),
    nrow = 5, byrow = TRUE))
  colnames(Malinvaud.Q) = NamesOf.Q 
  rownames(Malinvaud.Q) = c('Inertia / sum lambda',
                            'Chi2',
                            'p-Chi2','df',
                            'p-perm') 
  } else {
    Malinvaud.Q = data.frame(matrix(c(
      c(round(c(sum(Val.P),Val.P),digits = ndigit4print) ),
      round(Le.Q,      digits = ndigit4print),
      round(Le.pQ,     digits = ndigit4print),
      round(Le.Q.nu,   digits = ndigit4print)
      # ,round(Le.pQ.perm,digit = ndigit4print)
      ),
      nrow = 4, byrow = TRUE))
    colnames(Malinvaud.Q) = NamesOf.Q 
    rownames(Malinvaud.Q) = c('Inertia / sum lambda',
                              'Chi2',
                              'p-Chi2','df'
                              #,'p-perm'
                              ) 
  }
  # test p for chi2 to avoid 0
  for (k in 1:(ncol(Malinvaud.Q)-1)){
  if((Malinvaud.Q[3,k]) == 0){
    Malinvaud.Q[3, k] <- round( 
          1/(10^ndigit4print), ndigit4print) }
  }
    return(Malinvaud.Q)
} # End of function MalinvaudQ4CA here
# eof malinvaudQ4CA.perm ----
#___________________________________________________________
# Preamble eigCA4Multinom ----
# Function eigCA4ultinom starts here
# Sample from a multinomial distribution 
# and compute the eigenvalues
# of a CA. 
# NB needs the function eigCA
# Examples of calls
# 1. Bootstrap of X:   
#     Boot.eig <- eigCA4Multinom(sum(X),X,nrow(X),ncol(X))))
# 2. Permutation of X 
#      X4H0 <- (1/sum(X)^2)*(as.matrix(rowSums(X))%*%t(as.matrix(colSums(X))))
#      Perm.eig <- eigCA4Multinom(sum(X),X4H0,nrow(X),ncol(X))))
#___________________________________________________________
# Help for eigCA4Multinom starts here
#' @title 
#' Sample from a multinomial distribution 
#' (with a given probability distribution)
#' and compute the eigenvalues
#' of a correpondence analysis of the simulated
#' matrix.. 
#' @description \code{eigCA4Multinom}:
#' Sample from a multinomial distribution 
#' (with a given probability distribution)
#' and compute the eigenvalues
#' of the CA of an \code{nrow*ncol}
#'  (see below for these parameters).
#'  data matrix simulating correspondence analysis.  
#' 
#' @param nobs grand total of the table to be simulated.
#' @param prob probability distribution 
#'  for the cells. Should be length = \code{nrow*ncol}
#'  (see below for these parameters).
#'
#' @param nrow The number of rows of the matrix
#' to be simulated. 
#' @param ncol The number of columns of the matrix
#' to be simulated.
#' @return OUTPUT_DESCRIPTION
#' @details  \code{eigCA4Multinom}
#' is mostly used for computing eigenvalues
#' of
#'  created data matrices
#' simulating permutation and bootstrap procedures
#' for correspondence analysis.
#' 
#' @examples 
#' \dontrun{
#' set.seed(87) # set the seed
#' X <- matrix(round(runif(21)*20), ncol = 3) # good for CA
#' nobs <-  sum(X) # grand total
#' nI <- nrow(X)
#' nJ <- ncol(X)
#' pI <- as.matrix(rowSums(X) / nobs) # marginal I & J
#' pJ <- as.matrix(colSums(X) / nobs) # probabilites
#' p4Permutation <- pI %*% t(pJ) # Independence <=> permutation
#' # Simulated Permutation Probabilities
#' permEigen <- eigCA4Multinom(nobs, p4Permutation, nI, nJ)
#' p4Bootstrap   <- X / nobs # Actual prob <=> Bootstrap
#' permBoots <- eigCA4Multinom(nobs, p4Bootstrap, nI, nJ)
#' }
#' @importFrom stats rmultinom
#' @rdname eigCA4Multinom
#' @export 
#' @author  Hervé Abdi
eigCA4Multinom <- function(nobs, # grandtotal of the table
                   prob, # probability distribution 
                  # for the cells. Should be length = nI*nJ
                  nrow, ncol# nrow & ncol of the matrix
){ # function starts here
  CA.Valp  <- eigCA(matrix(
    as.vector(stats::rmultinom(1, nobs, prob)),
    nrow = nrow, ncol = ncol, byrow = FALSE))
  return(CA.Valp)
} # eof eigCA4Multinom ----
#___________________________________________________________
# Preamble multinomCV4CA ----
# function multinomCV4CA
#  Multinomial distribution Based
#      Cross Validation for Correspondence Analysis
#  Assumes a plain model for CA as a contingency table
#___________________________________________________________
# Help multinomCV4CA starts here
#' @title Compute the permuted and bootstrapped eigenvalues
#' of the correspondence analysis (CA) of a matrix suitable
#' for CA (i.e., a matrix with non negative elements).
#' 
#' @description \code{multinomCV4CA}: 
#' a very fast routine that
#' computes the permuted and bootstrapped eigenvalues
#' of the correspondence analysis (CA) of a matrix suitable
#' for CA (i.e., a matrix with non negative elements).
#' @param Data a matrix suitable
#' for CA (i.e., a matrix with non negative elements).
#' @param niter number of Bootstrapped/Permutations
#'  (Default: \code{1000}).
#' @return A list with two elements: 1)
#' \code{$Permuted.ValP}: The matrix of the 
#' \code{niter} by \code{rank(Data)} permuted
#' eigenvalues of the data matrix \code{Data}, and
#' \code{$Bootstraped.ValP}: The matrix of the 
#' \code{niter} by \code{rank(Data)} bootstrapped 
#' eigenvalues of the data matrix \code{Data}.
#' 
#' @details \code{multinomCV4CA}
#' uses the multinomial distribution to
#' simulate bootstrap and permutation resampling
#' for a correspondence analysis.
#' @examples 
#' \dontrun{
#' set.seed(87) # set the seed
#' X <- matrix(round(runif(21)*20), ncol = 3) # good for CA
#' ResCV <- multinomCV4CA(X)
#' }
#' @rdname multinomCV4CA
#' @export 

multinomCV4CA <- function(Data, # The contingency Table
                           # data frame or matrix
                  niter = 1000 # How Many Iterations
                          ){
          X = as.matrix(Data)
          nN = sum(X)
          nI = nrow(X)
          nJ = ncol(X)
 # Get permutated eigenvalues from multinomial distribution
 # Probability distribution under H0 for permutation
          X4H0 <- (1/sum(X)^2)*
                (as.matrix(rowSums(X))%*%t(as.matrix(colSums(X))))
          Perm.ValP <- t(replicate(niter,eigCA4Multinom(nN,X4H0,nI,nJ)))
# Get bootstraped eigenvalues from multinomial distribution
          Boot.ValP <- t(replicate(niter,
                            eigCA4Multinom(nN, X, nI, nJ)))
          nL <-  ncol(Boot.ValP)
   colnames(Perm.ValP) <- paste0('Dimension ',1:nL) -> colnames(Boot.ValP)
   return.list <- structure(
                     list(Permuted.ValP = Perm.ValP,
                        Bootstrapped.ValP = Boot.ValP),
                            class = 'Inference4CA')
   return(return.list)
       } # eof multinomCV4CA ----
# Print routines ----
# print routines ----
# #_____________________________________________________________________
# print.Inference4CA ----
#
#' Change the print function for Inference4CA
#'
#'  Change the print function for Inference4CA
#'
#' @param x a list: output of, e.g.,  multinomCV4CA
#' @param ... everything else for the functions
#' @author Hervé Abdi
#' @export
print.Inference4CA <- function(x, ...) {
  ndash = 78 # How many dashes for separation lines
  cat(rep("-", ndash), sep = "")
  cat("\n Inferences for the Eigenvalues of Correspondence Analysis \n")
  # cat("\n List name: ", deparse(eval(substitute(substitute(x)))),"\n")
    cat(rep("-", ndash), sep = "")
    cat("\n$Permuted.ValP     : ", "The matrix of permuted eigenvalues.") 
    cat("\n$ Bootstrapped.ValP: ", "The matrix of bootstrapped eigenvalues.")
  cat("\n",rep("-", ndash), sep = "")
  cat("\n")
  invisible(x)
} # end of function print.Inference4CA ----
# ____________________________________________________________________
#___________________________________________________________
#********************  End of Functions Here ***************
#***********************************************************


