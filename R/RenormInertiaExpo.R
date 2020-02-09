#_____________________________________________________________________
# renormInertiaExPo function to be stored in data4PCCAR
# HA: February 08, 2020.
# function in this file: renormInertiaExPo 
#_____________________________________________________________________
#
#_____________________________________________________________________
# Helper for roxygen2 ----
#  install.packages('sinew')
#  sinew::makeOxygen(renormInertiaExPo )
#_____________________________________________________________________
#_____________________________________________________________________
# A  normalization function
# Renormalize the factor scores of an ExPosition output-list
# to a constant or to the Inertia of another ExPosition analysis.
# 
#' @title Renorm the output of an \code{ExPosition analysis}.
#' 
#' @description \code{renormInertiaExPo}:renorms
#' the output of an n\code{ExPosition} function (e.g., 
#' \code{epCA, epPCA, epMCA}) to a given constant.
#' After normalization, all the objects whose inertia
#' depends upon the data will now have their inertia
#' equal to the new specified inertia.
#'
#'The normalization constant can be a scalar, or
#'can be the output of a previous 
#'\code{ExPosition}
#'analysis, in this last case
#' the inertia is the inertia 
#' of the table used in this previous analysis.
#'  
#' @param resExpo the output of an n\code{ExPosition} function (e.g., 
#' \code{epCA, epPCA, epMCA}). 
#' 
#' 
#' @param newNorm The new norm (Default: \code{NULL}),
#' if \code{NULL} the results are normed to a value of 1,
#' if \code{newNorm} is a scalar, the data will be nromed
#' @return The renormed \code{ExPosition} output.
#' @details 
#'  All the dataframes in \code{resExpo}
#'  whose norm depends upon the data
#'  (i.e., \code{fi, fj, di, dj, pdq$Dv, pdq$Dd, eigs}) 
#'   are renormed in the output.
#'  
#'   
#' @examples 
#' \dontrun{
#' data("mtcars") # use the mtcars data set 
#'  resPCA.normed  <- renormInertiaExPo(epPCA(mtcars, graphs = FALSE), ncol(mtcars) )
#' }
#' @rdname renormInertiaExPo
#' @export 


renormInertiaExPo <- function(resExpo, newNorm = NULL){
  # Check that newNorm 
  # is a scalar or a valid ExPosition output
  PopulationInertia <- NA
  if (is.numeric(newNorm)){PopulationInertia <- newNorm}
  if (is.null(newNorm)){PopulationInertia <- 1}
  if (is.object(newNorm)){
    if (attr(newNorm,"class")[1] == 'expoOutput'){
      PopulationInertia <- sum(newNorm$ExPosition.Data$eigs)}}
  if(is.na(PopulationInertia)){ 
    stop('Incompatible Value for Parameter newNorm')}  
  # if not check that it is 
  SampleInertia     <- sum(resExpo$ExPosition.Data$eigs)
  normFactor <- sqrt(PopulationInertia / SampleInertia)
  # renorm
  resExpo$ExPosition.Data$fi <- resExpo$ExPosition.Data$fi * normFactor 
  resExpo$ExPosition.Data$fj <- resExpo$ExPosition.Data$fj * normFactor 
  resExpo$ExPosition.Data$Dv <- resExpo$ExPosition.Data$Dv * normFactor 
  resExpo$ExPosition.Data$Dd <- resExpo$ExPosition.Data$Dd * normFactor
  resExpo$ExPosition.Data$di <- resExpo$ExPosition.Data$di * normFactor^2 
  resExpo$ExPosition.Data$dj <- resExpo$ExPosition.Data$dj * normFactor^2 
  resExpo$ExPosition.Data$eigs <- resExpo$ExPosition.Data$eigs * normFactor^2 
  return(resExpo)
}
