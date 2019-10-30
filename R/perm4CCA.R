#' Title
#'
#' @param DATA1 
#' @param DATA2 
#' @param center1 
#' @param center2 
#' @param scale1 
#' @param scale2 
#' @param nIter 
#' @param permType 
#' @param compact 
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{}
perm4CCA <- function (DATA1, DATA2, center1 = TRUE, center2 = TRUE, scale1 = "ss1", 
          scale2 = "ss1", nIter = 1000, permType = "byMat", compact = FALSE) 
{
  if (permType != "byColumns") 
    permType <- "byMat"
  DATA1 <- as.matrix(DATA1)
  DATA2 <- as.matrix(DATA2)
  X = DATA1
  Y = DATA2
  if (NCOL(X) > NCOL(Y)) {
    X = DATA2
    Y = DATA1
  }
  nN <- NROW(X)
  nI <- NCOL(X)
  nJ <- NCOL(Y)
  if (!(nN == NROW(Y))) {
    stop("DATA1 and DATA2 non-conformable")
  }
  maxRank <- min(nI, nJ)
  Sfixed = compS(DATA1, DATA2, center1 = center1, center2 = center2, 
                 scale1 = scale1, scale2 = scale2)
  fixedEigenvalues <- rep(0, maxRank)
  fixedEV <- sv2(Sfixed)
  if (length(fixedEV) > maxRank) {
    fixedEigenvalues <- fixedEV[1:maxRank]
  }
  if (length(fixedEV) == maxRank) {
    fixedEigenvalues <- fixedEV
  }
  if (length(fixedEV) < maxRank) {
    fixedEigenvalues[1:length(fixedEV)] <- fixedEV
  }
  fixedInertia <- sum(fixedEigenvalues)
  permInertia <- rep(NA, nIter)
  permEigenvalues <- matrix(NA, nrow = nIter, ncol = maxRank)
  .truc <- function(X, Y, longueur = min(c(dim(X), NCOL(Y))), 
                    permType = permType) {
    valP <- rep(0, longueur)
    if (permType == "byMat") {
      Xrand <- X[sample(nN), ]
      Yrand <- Y
    }
    if (permType == "byColumns") {
      Xrand <- apply(X, 2, sample)
      Yrand <- apply(Y, 2, sample)
    }
    Srand <- compS(Xrand, Yrand)
    resvp <- sv2(Srand)
    valP[1:length(resvp)] <- resvp
    return(valP)
  }
  laLongueur <- maxRank + 1
  permEigenvalues <- replicate(nIter, .truc(X, Y, laLongueur, 
                                            permType))
  permEigenvalues <- t(permEigenvalues[1:maxRank, ])
  permInertia = rowSums(permEigenvalues)
  pOmnibus = sum(permInertia > fixedInertia)/nIter
  if (pOmnibus == 0) 
    pOmnibus <- 1/nIter
  pEigenvalues <- rowSums(t(permEigenvalues) > (fixedEigenvalues))/nIter
  pEigenvalues[pEigenvalues == 0] <- 1/nIter
  return.list <- structure(list(fixedInertia = fixedInertia, 
                                fixedEigenvalues = fixedEigenvalues, pOmnibus = pOmnibus, 
                                pEigenvalues = pEigenvalues), class = "perm4PLSC")
  if (!compact) {
    return.list$permInertia = permInertia
    return.list$permEigenvalues = permEigenvalues
  }
  return(return.list)
}