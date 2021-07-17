# Various goodies for MCA
# A set of Helper Functions for MCA
# Hervé Abdi: October 18, 2018.
# Contains the following functions:
# BinQuant
# ctr4Variables
# coloringLevels
# addLines4MCA
# phi2CT
# phi2Mat4Burt
# BR4varMCA
# print.pseudo.BR
# Last Edit: HA. October 23.
#____________________________________________________________________
# New Functions Here ----
# to be moved to data4PCCAR
#_____________________________________________________________________
# BinQuant Preamble ----
# sinew::makeOxygen()
# sinew::makeOxygen(BinQuant)
#_____________________________________________________________________
#'
#' @title Recode a quantitative variable
#'   as a binned factor.
#'
#' @description
#' \code{BinQuant}: a function to recode a quantitative variable
#'   as a binned factor with (roughly) the same number of
#'   observations per bin.
#' @details Use to create roughly) balanced
#' factors from quantitative variables
#' in MCA.
#' @param x A vector (of numbers)
#' @param nClass = 4 number of bins for the recoded factor
#' @param   stem = '' Stem for the levels of the recoded factor
#' @param levelNames (default = \code{NULL}) the name
#' of the levels of the factor. If \code{NULL} use the values
#' 1 to \code{nClass}.
#' @return a vector factor with bin values.
#' @importFrom stats quantile
#' @author Hervé Abdi
#' @examples
#' aFactor <-  BinQuant(1:15)
#' @rdname BinQuant
#' @export
BinQuant <- function(x, nClass = 4, stem = '', levelNames = NULL){
  x <- as.numeric(x)
  qFact = stats::quantile(x, probs = seq(0, 1, 1/nClass))
  if (nClass != (length(qFact) - 1)){
    nClass <-  length(qFact) - 1
    levelNames <-  NULL}
  if (is.null(levelNames)){levelNames <- 1:nClass}
  CodedFact = cut(x, breaks= qFact,
                  include.lowest = TRUE,
                  labels = paste0(stem,levelNames))
  return(CodedFact)
}
# ```
#_____________________________________________________________________
# Helper for roxygen2
#  install.packages('sinew')
#  sinew::makeOxygen(ctr4Variables)
#_____________________________________________________________________
# ctr4Variables Preamble ----
#' @title Compute contributions (or squared cosines)
#' for (qualitatitve)  variables
#' in Multiple Correspondence Analysis (e.g., as performed by
#' \code{ExPosition::epMCA}).
#' @description \code{ctr4Variables}:
#' Computes contributions
#' (or squared cosines)  for (qualitatitve)  variables
#' in Multiple Correspondence Analysis (e.g., as performed by
#' \code{ExPosition::epMCA}).
#' @param ctrJ a matrix or data frame of contributions
#' or squared cosines
#' (e.g., from \code{ExPosition::epMCA}, the contributions
#' are in \code{cj}, the squared cosines are in \code{ri}).
#' @return A qualitative variables by dimensions data frame.
#' @details In MCA, the contribution
#' (resp. squared cosine) of a variable is
#' the sum of (resp. squared cosine) contributions
#' of all its levels.
#'
#' \code{ctr4Variables} finds the levels of a given variable
#' by stripping the contribution columns names of their extension
#' (e.g., \code{toto.1} and \code{toto.2} are two levels
#' of the qualitative variable \code{toto}). This
#' is performed with the function
#' \code{tools::file_path_sans_ext}.
#'
#'
#' @author Hervé Abdi
#' @examples
#' library(ExPosition)
#' data(mca.wine)
#' resMCA    <- epMCA(mca.wine$data, graphs = FALSE)
#' contriVar <- ctr4Variables(resMCA$ExPosition.Data$cj)
#' @seealso \code{\link{getVarNames}}
#' @rdname ctr4Variables
#' @export
#' @importFrom tools file_path_sans_ext
#' @importFrom stats aggregate
#' @import ExPosition
ctr4Variables <- function(ctrJ){
  # Compute the contributions per variable
  lesNoms      <- getVarNames(rownames(ctrJ))
  stripedNames <-  lesNoms$stripedNames
  varNames     <- lesNoms$variableNames
  # nVar <- length(varNames)
  # get the number  of levels per variables
  #varCtr.tmp <- aggregate(cJ ~ varNames, (cbind(varNames,cJ)),sum)
  varCtr.tmp <- stats::aggregate(ctrJ ~ stripedNames,
                          (cbind(stripedNames,ctrJ)),sum)
  varCtr <- varCtr.tmp[,-1]
  rownames(varCtr)    <- varCtr.tmp[,1]
  # we need to re-order varCtr to get back to the orginal order
  # varCtr <- varCtr[sort(varNames, index.return = TRUE)$ix,]
  index <- match(varNames, rownames(varCtr))
  varCtr <- varCtr[index,, drop = FALSE]
  colnames(varCtr) <- paste0('Dimension ', 1:ncol(varCtr))
  return(varCtr)
} # end of ctr4Variables
#_____________________________________________________________________
# test: See examples.
# data(mac.wine)
# lesRes <- epMCA(mca.wine$data, graphs = FALSE)
# contriVar <- ctr4Variables(lesRes$ExPosition.Data$cj)
#_____________________________________________________________________
# Helper for roxygen2
#  install.packages('sinew')
#  sinew::makeOxygen(getVarNames)
#_____________________________________________________________________
# getVarNames
#' @title Get the names of variables from the levels
#' of the columns in an MCA performed with \code{ExPosition::epMCA}.
#' @description  \code{getVarNames}
#' Get the names of variables from the levels
#' of the columns in an MCA performed with \code{ExPosition::epMCA}.
#' @param labelsNames The name of the labels,
#' typically from the output of \code{ExPosition::epMCA}
#' @return A list with 3 vectors of the  original factor names:
#' \code{variableNames}: The names of the variables,
#' \code{stripedNames}: The names of the levels of the variables
#' striped of their extensions,
#' \code{originalLabelsNames}: The original label names.
#' @details This is a helper function for
#' some other helper functions for \code{ExPosition::epMCA}.
#' @author Hervé Abdi
#' @examples
#' \dontrun{
#' if(interactive()){
#'  # an example from ExPosition
#' library(ExPosition)
#' data(mca.wine)
#' lesRes <- epMCA(mca.wine$data, graphs = FALSE)
#' nameOffactors <- getVarNames(rownames(lesRes$ExPosition.Data$fj) )
#'  }
#' }
#' @seealso
#'  \code{\link[tools]{fileutils}}
#' @rdname getVarNames
#' @export
#' @importFrom utils head
# @importFrom tools file_path_sans_ext
getVarNames <-  function(labelsNames){
  # file_path_sans_ext works fine
  # except when there are non standard characters (e.g., ?)
  # after the last .
  #stripedNames      <-  tools::file_path_sans_ext(labelsNames)[1,]
  stripedNames <- sapply(strsplit(labelsNames,"\\."), function(x) paste0(utils::head(x,-1),collapse=".") )
  variableNames     <- unique(stripedNames)
  if (!is.null(dim(labelsNames))) labelsNames <- labelsNames[1,]
  return.list <- list(variableNames = variableNames,
                      stripedNames = stripedNames,
                      originalLabelsNames = labelsNames)
  # NB changed the old originalLabelsNames = labelsNames[1,]
  # create problems with vectors
  return(return.list)
} # end of getVarNames
#_____________________________________________________________________
# Helper for roxygen2
#  install.packages('sinew')
#  sinew::makeOxygen()
#  sinew::makeOxygen(coloringLevels)
#___________________________________________________________________
# coloringLevels preamble ----
#' @title create a vector of color names for the levels
#' of an MCA from the color names of the variables.
#' @description \code{coloringLabels}:
#' creates a vector of color names for the levels
#' (i.e., columns)
#' of an MCA from the color names of the variables.
#' @param levelsNames The names of the levels of the
#' columns of an MCA as run, for example, by
#' \code{ExPosition::epcMCA}.
#' Could be, for example the names of the \code{fj}
#' factor scores.
#' @param colorOfVariables A vector of colors
#' for the variables (should match the values obtained from the
#' levels).
#' if \code{NULL} (default)
#' or if the numbers of variables do not match the
#' number derived from \code{levelsNames} then
#' get the colors from
#' \code{}
#' @return A list with 2 vectors:
#' \code{color4Levels} the colors of the labels
#' and \code{color4Variables} the colors of the
#' variables (in the order in which they appeared).
#' @details If \code{colorOfVariables} is provided
#' the colors are used in the order in which the  variables
#' appear in the list of the levels.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[prettyGraphs]{prettyGraphsColorSelection}}
#'  \code{\link{getVarNames}}
#' @rdname coloringLabels
#' @author Hervé Abdi
#' @export
#' @importFrom prettyGraphs prettyGraphsColorSelection
coloringLevels <- function(levelsNames, colorOfVariables = NULL){
  # function starts here ----
  lesNoms <- getVarNames(levelsNames)
  # how many variables
  nVar <- length(lesNoms$variableNames)
  #col4Var     = col4Var # the color for the factors
  # if NULL get them from prettyGraphs
  if (is.null(colorOfVariables) |
      ((!is.null(colorOfVariables)) &
       (nVar != length(colorOfVariables)))  ){
    colorOfVariables <- prettyGraphs::prettyGraphsColorSelection(nVar)
  } # end if
  col4Labels <- rep("", length(levelsNames))
  for (i in 1:nVar){ # loop in i
    index <- lesNoms$stripedNames  %in% lesNoms$variableNames[i]
    col4Labels[index] <-  colorOfVariables[i]
  } # end of loop in i
  return.list <- list(color4Levels = col4Labels,
                      color4Variables = colorOfVariables)
  return(return.list)
} # end of function  coloringLabels
# end coloringLabels ----
#_____________________________________________________________________
#_____________________________________________________________________
# #  sinew::makeOxygen(addLignes4MCA)
# addLines4MCA preamble----
#_____________________________________________________________________
#' @title    (\code{ggplot2}) add lines to the levels of
#' the qualitative variables in a (variable) factorial
#' map of an MCA (e.g., computed with
#' \code{ExPosition::epMCA}).
#' @description \code{addLines4MCA}
#' is a \code{ggplot2} based function that
#' adds lines to join the levels of
#' the qualitative variables in a factorial
#' map of an MCA (e.g., computed with
#' \code{ExPosition::epMCA} and created with
#' \code{createFactortMap}).
#' @param Fj the output for the column set of an
#' MCA (e.g., from \code{ExPosition::epMCA}, this would be
#' \code{$ExPosition.Data$fj}).
#' @param col4Var vector of colors for the variables.
#' if col4Var is not equal to the number
#' of qualitative variables \code{ExPosition::epMCA})
#' will create these colors from
#' \code{\link[prettyGraphs]{prettyGraphsColorSelection}}.
#' @param alpha (Default: 0.7) the alpha values for the
#' lines.
#' @param linetype (Default: 3, dotted) the linetype number for the
#' lines.
#' @param size  (Default: .5) the size of the lines.
#' @param axis_h (Default: 1) what is the horizontal axis.
#' @param axis_v (Default: 2) hat is the verical axis.
#' @param dimension.names (Default: \code{'Dimension'})
#' @param ... Everything else that can be passed to
#' \code{ggplot2::geom_path()}.
#' @return A list with the lines for each variables
#' @details The levels of a given variable
#' are first ordered by alphabetical order prior
#' to drawing the lines
#' (so, e.g., \code{truc.1}, \code{truc.2}
#' and \code{truc.3} will be correctly ordered).
#'
#' The name (i.e., stem) of a variable is obtained
#' by stripping the names of the levels (i.e., columns)
#' of the extension after the last "." of their names.
#' For example, the name \code{toto.1} for
#' a row of \code{Fj} corresponds to the qualitative
#' variable \code{toto}.
#'
#'
#' @section Important_Note:
#'
#' When creating multiple layers graphs,
#'  because of the way \code{ggplot2} creates graphs,
#'   all the matrices/dataframe should all
#'   the have the same column names
#'  (e.g., \code{colnames()}
#'   equal to \code{c("Dimension 1", "Dimension 2")}).
#'    When this is not the case,
#'    some strange and cryptic error may be produced
#' (e.g., \code{"cannot find Dimension"}).
#' @examples
#' \dontrun{
#' if(interactive()){
#' library{ExPosition}
#' data("mca.wine")
#' resMCA    <- epMCA(mca.wine$data, graphs = FALSE)
#' Fj <- resMCA$ExPosition.Data$fj
#' baseMap4J <- createFactorMap(Fj)$zeMap +
#'   addLines4MCA(Fj,col4Var = rep('red',10) )
#'  }
#' }
#' @seealso
#'  \code{\link[prettyGraphs]{prettyGraphsColorSelection}}
#'  \code{\link{getVarNames}}
#' @rdname addLines4MCA
#' @export
#' @importFrom prettyGraphs prettyGraphsColorSelection
#' @importFrom ggplot2 geom_path

addLines4MCA <- function(Fj,
                         col4Var,
                         alpha     = .7,
                         linetype  =  3,
                         size      = .5 ,
                         axis_h    = 1,
                         axis_v    = 2,
                         dimension.names = 'Dimension', ...){

  noms <-  getVarNames(rownames(Fj))
  nK <- length(noms$variableNames)
  if (!isFALSE(length(col4Var) != nK)){# check number of colors
    col4Var <- prettyGraphs::prettyGraphsColorSelection(nK)
  }
  colnames(Fj) <- paste0(dimension.names," ", 1:ncol(Fj))
  lesLignes <- list()
  for (k in 1:nK){
    pos.k <-  which(noms$stripedNames %in% noms$variableNames[k])
    leF <- Fj[pos.k,c(axis_h, axis_v)]
    leF <- as.data.frame(leF[sort(rownames(leF),
                                  index.return = TRUE)$ix,])
    # colnames(leF) <- paste0('Dimension ',1:ncol(leF))
    uneLigne <-  ggplot2::geom_path(data = leF, color = col4Var[k],
                           linetype = linetype,
                           size = size, alpha = alpha, ...)
    lesLignes[[k]] <- uneLigne
  }
  return(lesLignes)
} # end of addLignes4MCA ----
#_____________________________________________________________________
# fastPhi2
#_____________________________________________________________________
#sinew::makeOxygen(phi2CT)
# phi2CT Preamble ----
#_____________________________________________________________________
#' @title A fast function to compute the phi2 for a Contingency table.
#' @description \code{phi2CT}
#' A fast function to compute the phi2 for a Contingency table.
#' @param CT A contingency table.
#' @return The phi2 (coefficient of correlation)
#' associated with the Contingency table.
#' @details phi2 is the coefficient of correlation for a
#' contingency table. It is equal to  the inertia of
#' the contingency tables divided by the
#' min(number of rows, number of columns) - 1.
#' The inertia is also equal to the Chi2 of the table divided
#' by its total.
#' Phi2 could also be derived from the function
#' \code{chisq.test}.
#' @seealso phi2Mat4BurtTable
#' @examples
#' phi2CT(matrix(c(19,15,7,14,4,21,5,22,8,5,9,21), nrow = 4) )
#' @rdname phi2CT
#' @export
phi2CT <- function(CT){
  Z <- CT / sum(CT)
  r <- as.matrix(rowSums(Z))
  c <- as.matrix(colSums(Z))
  nI <- nrow(Z)
  nJ <- ncol(Z)
  Z_rc <-  matrix(as.vector(r^(-.5)),
                  nrow = nI, ncol = nJ, byrow = FALSE) *
    (Z - r %*% t(c)  ) *
    matrix( as.vector(c^(-.5)),
            nrow = nI, ncol = nJ, byrow = TRUE)
  phi2 <- sum(Z_rc^2) /  (min(nI, nJ ) - 1)
  return(phi2)
} # end of function phi2CT
#
# phi2Mat4BurtTable Preamble ----
#_____________________________________________________________________
#sinew::makeOxygen(phi2Mat4BurtTable)
#phi2Mat4BurtTable Preamble ----
#_____________________________________________________________________
#
#' @title Compute the phi2 (correlation) matrix
#' for data tables suitable for Multiple Correspondence Analysis
#' (MCA).
#' @description \code{phi2Mat4BurtTable}
#' Compute the phi2 (correlation) matrix
#' for data tables suitable for Multiple Correspondence Analysis
#' (MCA).
#' @param DATA A data table suitable for MCA.
#' Could be an \eqn{I} by \eqn{K} data frame / matrix
#' with columns being factors, or an already
#' \eqn{I} by \eqn{J}
#' expanded matrix with 0/1 (as created for example
#' with \code{ExPosition::makeNominalData}).
#' @param make_data_nominal (Default: TRUE)
#' when \code{TRUE} the data are factors,
#' otherwise, the data are already in 0/1 form.
#' @return A list with
#' 1) \code{phi2.mat}: The phi2 correlation matrix
#' and
#' 2) \code{burt.table}: the Burt table.
#' @details The phi2 matrix from \code{phi2Mat4BurtTable}
#' can be used to create correlation heat maps for MCA.
#' @examples
#' \dontrun{
#' if(interactive()){
#' library(ExPosition)
#' data("mca.wine")
#' phi2Mat4BurtTable(mca.wine$data)
#'   }
#' }
#' @rdname phi2Mat4BurtTable
#' @export
phi2Mat4BurtTable <- function(DATA,
                              make_data_nominal = TRUE){
  if (isTRUE(make_data_nominal)){X = makeNominalData(DATA)
  } else {X = DATA}
  noms <- getVarNames(colnames(X))
  nK <- length(noms$variableNames)
  phi2.mat <-matrix(0, nrow = nK, ncol = nK)
  # double loop
  for ( k in 1:(nK - 1)){
    for (m  in (k + 1):nK){
      pos.k <-  which(noms$stripedNames %in% noms$variableNames[k])
      pos.m <-  which(noms$stripedNames %in% noms$variableNames[m])
      phi2.mat[k, m] <- phi2CT(t(X[,pos.k]) %*% X[,pos.m])
    }
  }
  phi2.mat <- phi2.mat + t(phi2.mat)
  phi2.mat <- phi2.mat + diag(1, nrow = nrow(phi2.mat))
  # get var k & m
  rownames(phi2.mat) <- noms$variableNames ->  colnames(phi2.mat)
  return.list <- structure(list(
                            phi2.mat = as.data.frame(phi2.mat),
                            burt.table = as.data.frame(t(X) %*% X )),
                           class = 'phi2Mat')
  return( return.list)
} # end of phi2Mat4BurtTable -----
#_____________________________________________________________________
#_____________________________________________________________________
# Preamble BR4varMCA ----
# sinew::makeOxygen(BR4varMCA)
#_____________________________________________________________________
#' @title Computes pseudo Bootstrap ratios from pseudo-\eqn{F} for
#' variables in MCA.
#' @description \code{BR4varMCA}
#'  Computes pseudo Bootstrap ratios from pseudo-\eqn{F} for
#' variables in MCA.
#' @param BrLevels The bootstrap ratios for the variables
#' (i.e., the output from \code{InPosition::epMCA.inference.battery}).
#' @param wJ the masses (i.e., the center of gravity of the rows)
#' for the columns, typically obtained from
#' the output of \code{ExPosition} as
#' \code{wJ = 1 / resMCA$ExPosition.Data$W}.
#' @param nIter (Default: 1000) the number of bootstrapped iterations
#'  used to compute the original Bootstrap ratios.
#' @return A list with 6 elements
#' \itemize{
#'  \item{"pseudoBR.pos"}{ The positive pseudo BR ratios (i.e.,
#'  BRs indicating \emph{differences} between levels)}
#'  \item{"pseudoBR"}{ BR ratios matching the probability 
#'  of their \eqn{F},
#' could be positive (indicating \emph{differences} between levels), or
#' could be negative (indicating \emph{similarities} between levels)
#' }
#' \item{F4Var}{The \eqn{F} from the ANOVA testing
#' the differences between the levels of
#' the qualitative variable.}
#' \item{df4Var}{The degrees of freedom for the \eqn{F}
#' from the ANOVA for the differences between the levels of
#' the qualitative variable.}
#' \item{ pF4Var}{Probability associated the \eqn{F}'s}.
#' }
#' @author Hervé Abdi
#' @details The idea here is to get a statistics that is
#' commensurable across designs. So, the \eqn{BR}s from the levels
#' are re-combined to give a pseudo-\eqn{F} that tests
#' if the levels of the variables are reliably different
#' for a given factor.  The
#' probability associated to the pseudo-\eqn{F}
#' is then used to compute a pseudo-\eqn{BR} whose value
#' will then have the same probability as the pseudo-\eqn{F}.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @importFrom stats pf qnorm
#' @rdname BR4varMCA
#' @export
BR4varMCA <- function(BrLevels, wJ, nIter = 1000){
  # test.Br2wj <-  diag(wJ) %*% BrLevels^2
  Br2wj <-  matrix(wJ, nrow = length(wJ), ncol = ncol(BrLevels),
                   byrow = FALSE) * BrLevels^2
  rownames(Br2wj) <- rownames(BrLevels)
  colnames(Br2wj) <- paste0('Dimension ', 1:ncol(Br2wj))
  lesNoms <-  getVarNames(rownames(BrLevels)) # from data4PCCAR
  Jk.raw  <- table(as.factor(lesNoms$stripedNames))
  index <- match(lesNoms$variableNames, names(Jk.raw) )
  Jk <- Jk.raw[index,  drop = FALSE]
  df_k <- Jk - 1 # degrees of freedom
  BrSums     <-  ctr4Variables(Br2wj) # from data4PCCAR
  F_k <-  matrix(df_k, nrow = length(df_k), ncol = ncol(BrSums),
                 byrow = FALSE) *  BrSums
  # get p(F)
  pF <- 1 - pf(as.matrix(F_k), df_k, 100)
  # Now inverse to a Z score
  # note that a significant negative BR indicates a strawberry basket
  pseudoBR     <-  -qnorm(as.matrix(pF))
  # to look at significance as difference between levels:
  #    keep positive only
  pseudoBR.pos <- pseudoBR * ((pseudoBR) > 0) # positive only
  return.list <- structure(list(pseudoBR.pos = pseudoBR.pos,
                                pseudoBR =  pseudoBR,
                                F4Var    = F_k,
                                df4Var   = df_k,
                                pF4Var   = pF),
                           class = 'pseudoFBR')
  return(return.list)
} # end of  BR4varMCA ----
#_____________________________________________________________________
# print.pseudoFBR ----
# Print here:
# ********************************************************************
