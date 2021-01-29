# Preamble ----
# R-version/clone of HA's PLSR matlab program.
# Current Version HA: 08/04/2020.
# Note that this version is still a beta version
# Historic: ----
# # MATLAB
# Hervé Abdi original Matlab version 2003. Modifications: 
#   ->  June 2007  (minimize memory storage)
#   ->  July 2007 Add RESS and PRESS (not optimized for that!)
#   ->  September 2008 add svd instead of standard NIPALS
#                Rewrite Jackknife for faster results
#  WARNING:  Computation of RESS and PRESS 
#            have not been thoroughly checked
# # R
# First version  : Lei Xuan 2012
# Current Version: Hervé Abdi. 06/28/2016 / 
#       revisited: 08.04/2020 
#   fix problem with X and Y needed tobe df:
#       01/28/2021.
#

##------------------------------------------------------------##
#                PLSR_SVD.R                               #
##------------------------------------------------------------##
# OldHelp ----
# USAGE: PLSR_SVD(X,Y,nfact)
# RETURN: A list of T,P,W,Wstar,U,b,C,B_pls,Bpls_star,Xori_rec,
#         Yori_rec,Yjack,R2_X,R2_Y,RESS,PRESS,
#         Q2,r2_random,rv_random
#
# NIPALS version of PLS regression using svds instead of NIPALS per se
#       (faster for large data sets).
# X matrix of predictors, Y: matrix of dependent variables
# nfact = number of latent variables to keep 
# [current default rank(X)-1]
# NB: for large datasets keeping nfact small improves performance a lot!
# GOAL:
# Compute the PLS regression coefficients/decomposition
# X = T*P' Y = T*B*C' = X*Bpls  X and Y being Z-scores
#                          B = diag(b)
#    Y = X * Bpls_star with X being augmented with a col of ones
#                       and Y and X having their original units
#    Yjack is the jackknifed (LOO) estimation of Y
# T'*T = I (NB normalization <> than SAS)
# W'*W = I
# C is unit normalized,           
# U, P are not normalized 
# For notations: see Abdi (2003, 2007, 2010),
#         available from \code{\url{personal.utdallas.edu/~herve}}
# 
#  Xhat,Yhat: reconstituted matrices from PLSR 
#    with nfact latent variables (i.e., fixed effect)
#  Yjack: reconstituted Y from jackknife 
#   with nfact latent variables (i.e., random effect)
# R2x, R2y: Proportion of variance of X, Y explained by each latent variable 
# RESSy is the residual sum of squares:
#       RESSy=\sum_{i,k} (y_{i,k} - \hat{y}_{i.k})^2
# PRESSy is the PREDICTED residual sum of squares
#       RESSy=\sum_{i,k} (y_{i,k} - \hat{y}_{-(i.k)})^2
#       where \hat{y}_{-(i.k)} is the value obtained
#       without including y_{i,j} in the analysis
# Q2=1 - PRESSy(n)/(RESSy(n-1))
#   ->  Used to choose # of variable keep factor n if Q2_n > limit
#       rule of thumb:  limit =.05 for # observation<100, 0 otherwise
# r2y_random/rv_random: Vector of r2/rv between Y and Yjack 
#                       for each # of latent variables
# Yhat4Press: array of the nfactor Y Jackknifed matrices used to compute PRESS
# Yhat4Ress : array of the nfactor Y (fixed effect) matrices used to compute RESS
# 
#
# Hervé Abdi original Matlab version 2003. Modifications: 
#   ->  June 2007  (minimize memory storage)
#   ->  July 2007 Add RESS and PRESS (not optimized for that!)
#   ->  September 2008 add svds instead of standard NIPALS
#                Rewrite jackknife for faster results
#  WARNING:  Computation of RESS and PRESS 
#                     have not been thoroughly checked
# Prelude to PLSR_SVD ----
#' @title  PLS regression  (PLSR) using the Singular
#' Value Decomposition instead of the original NIPALS
#       (faster for large data sets).
#' @description \code{PLSR_SVD}: 
#'  PLS regression (PLSR) 
#' computed using the Singular
#' Value Decomposition (SVD) instead of the original \code{NIPALS}.
#'       (faster for large data sets). This version is an \code{R}
#' version of the original \code{MATLAB} code used in Abdi (2010).

#' @param X The \eqn{N} observations by \eqn{I} variables
#' matrix (**X**) of the predictors.
#' @param Y The \eqn{N} observations by \eqn{J} variables
#' matrix (**Y**) to be predicted.
#' @param nfactor Number of factors (a.k.a., \emph{latent variables})
#' to be used for the prediction. 
#' Note that the solution in PLSR
#' is strongly dependent upon the number of factors to keep.
#' @param inference  when \code{TRUE} (default) 
#' run the jackknife based inference battery.
#' Note that this step can be very time consuming for large data sets.
#' @param displayJack if \code{TRUE} (default) 
#' display the current iteration when
#' performing jackknife, worth setting it to \code{FALSE}
#' for large data sets because it is quite time consuming.
#' 
#'
#' @return 
#' A (long) list with results for the fixed and random effects (if
#' \code{inference = TRUE})
#'  
#' Fixed effects results:  
#' \itemize{
#' \item{Xhat: }{reconstituted **X** matrix from PLSR 
#' (with \code{nfact} latent variables: fixed effect).} 
#' \item{Yhat: }{reconstituted **Y** matrix from PLSR
#' (with \code{nfact} latent variables: fixed effect).} 
#'  \item{Yjack: }{reconstituted Y from jackknife 
#'   (with \code{nfact} latent variables: fixed effect).}
#'  \item{R2x, R2y: }{Proportion of variance of **X**, **Y** 
#'   explained by each latent variable.} 
#' \item{RESSy: }{ the residual sum of squares:
#'       \eqn{RESSy = \sum_{i,k} (y_{i,k} - \hat{y}_{i.k})^2} }
#' \item{Yhat4Ress :}{
#'  array of the \code{nfactor} **Y** (fixed effect) 
#'  matrices used to compute \code{RESS}.}
#'}
#'If 
#' \code{inference = TRUE},
#'these random effect results are also returned:  
#' \itemize{
#' \item{PRESSy: }{the PREDICTED residual sum of squares: 
#'       \code{RESSy} = \eqn{\sum_{i,k} (y_{i,k} - \hat{y}_{-(i.k)})^2}
#'       where \eqn{\hat{y}_{-(i.k)}} is the value obtained
#'       without including \eqn{y_{i,j}} in the analysis}
#' \item{Q2: }{Values of the \eqn{Q^2} parameter
#'       \eqn{Q^2 = 1 - PRESSy(n)/(RESSy(n-1))}
#' \code{Q2}  is used to choose the number
#' of latent variables to  keep with the rule:
#' keep latent variable \emph{\eqn{n} if \eqn{Q2_n} > some limit}.
#'       Rule of thumb:  limit =.05 for 
#' number of observations < 100, 0 otherwise}
#' \item{r2y_random,  rv_random: }{Vector of \eqn{R^2} 
#' and \eqn{R_V} coefficients between **Y** and **Y**jack 
#'                       for each number of latent variable solutions.}
#' \item{Yhat4Press: }{
#' array of the \code{nfactor} **Y** Jackknifed matrices 
#' used to compute \code{PRESS}.
#' }
#' }
#' .
#' 
#' @details 
#' GOAL:
#' 
#' Compute the PLS regression coefficients/decomposition
#' \itemize{
#' \item{}{**Zx** = **T** * **P**'.}
#'\item{}{
#' **Zy** = **T** * **B** * **C**' =  **Zx** * **B**pls}  
#' \item{with}{}
#' \item{}{**Zx** 
#'     and **Zy** being matrices storing the \eqn{Z}-scores version of
#'     **X** and **Y**. }
#' \item{}{**B** = diag(**b**)}
#'\item{}{    **Y** = **X**  * **B**pls_star} 
#' \item{with}{
#'    **X** being augmented with a column of ones
#'                       and **Y** and **X**
#'               being measured in their original units}
#'               }
#'               
#' In addition
#'  we have:
#' \itemize{               
#'    \item{           
#'    **Y**jack:}{ the jackknifed (LOO) 
#'    random effect estimation of **Y**.
#' } 
#'}
#'    Also we have the following relationships:
#'   \itemize{            
#' \item{ **T**'* **T** = **I**  (NB normalization <> from SAS)}
#' \item{ **W**'* **W** = **I** }
#' \item{ **C** is unit normalized}           
#' \item{ **U** and **P** are not normalized.}
#'  } 
#' 
#' 
#' 
#' Xhat,Yhat: reconstituted matrices from PLSR 
#    with nfact latent variables (i.e., fixed effect)
#  Yjack: reconstituted Y from jackknife 
#   with nfact latent variables (i.e., random effect)
# R2x, R2y: Proportion of variance of X, Y explained by each latent variable 
# RESSy is the residual sum of squares:
#       RESSy=\sum_{i,k} (y_{i,k} - \hat{y}_{i.k})^2
# PRESSy is the PREDICTED residual sum of squares
#       RESSy=\sum_{i,k} (y_{i,k} - \hat{y}_{-(i.k)})^2
#       where \hat{y}_{-(i.k)} is the value obtained
#       without including y_{i,j} in the analysis
# Q2=1 - PRESSy(n)/(RESSy(n-1))
#   ->  Used to choose # of variable keep factor n if Q2_n > limit
#       rule of thumb:  limit =.05 for # observation < 100, 0 otherwise
# r2y_random/rv_random: Vector of r2/rv between Y and Yjack 
#                       for each # of latent variables
# Yhat4Press: array of the nfactor Y Jackknifed matrices used to compute PRESS
# Yhat4Ress : array of the nfactor Y (fixed effect) matrices used to compute RESS
# 
#
# Herve Abdi original Matlab version 2003. Modifications: 
#   ->  June 2007  (minimize memory storage)
#   ->  July 2007 Add RESS and PRESS (not optimized for that!)
#   ->  September 2008 add svds instead of standard NIPALS
#                Rewrite jackknife for faster results
#  WARNING:  Computation of RESS and PRESS 
#                     have not been thoroughly checked
#' 
#' For notations: see Abdi (2003, 2007, 2010),
#'      available from \code{personal.utdallas.edu/~herve}
#' @author Hervé Abdi, Lei Xuan 
#' @examples 
#' \dontrun{
#' # Run the wine example from Abdi (2010)
#' data("fiveWines4Rotation")
#' Xmat <- fiveWines4Rotation$Xmat.Chemistry
#' Ymat <- fiveWines4Rotation$Ymat.Sensory
#' resPLSR <- PLSR_SVD(Xmat, Ymat, 3)
#' }
#' @seealso \code{\link{PLS4jack}} \code{\link{corrcoef4mat}} 
#' \code{\link{normaliz}}
#' @importFrom MASS ginv
#' @importFrom stats sd
#' @rdname PLSR_SVD
#' @export 
#' @references 
# References 
#' (see also \code{https://personal.utdallas.edu/~herve/})
#' 1. Abdi, H. (2010). 
#'      Partial least square regression, 
#'     projection on latent structure regression, PLS-Regression. 
#'    \emph{Wiley Interdisciplinary Reviews: Computational Statistics, 
#'     2}, 97-106.
#'  2. Abdi, H. (2007). 
#'     Partial least square regression (PLS regression). 
#'     In N.J. Salkind (Ed.):  
#'     \emph{Encyclopedia of Measurement and Statistics}. 
#'     Thousand Oaks (CA): Sage. pp. 740-744.
#'  3. Abdi. H. (2003).
#'     Partial least squares regression (PLS-regression). 
#'     In M. Lewis-Beck, A. Bryman, T. Futing (Eds):  
#'     \emph{Encyclopedia 
#'     for Research Methods for the Social Sciences}. 
#'     Thousand Oaks (CA): Sage. pp. 792-795. 
#'
#
# Change for repmat call the local function


PLSR_SVD <- function(X, Y,
                     nfactor, 
                     inference = TRUE, 
                     displayJack = TRUE){
  # First: An internal function to mimic MATLAB's repmat
  #___________________________________________________________________
  repmat <-  function (a, n, m){kronecker(matrix(1, n, m), a)}
  #___________________________________________________________________
	X <- as.data.frame(X) # fixes a strange error
  Y <- as.data.frame(Y)
  X <- as.matrix(X) # Make sure that we are dealing with matrices
	Y <- as.matrix(Y)
	obs.names  <- rownames(X)
	Xvar.names <- colnames(X)
	Yvar.names <- colnames(Y)
	X_ori <- X
	Y_ori <- Y

	maxfac <- qr(X)$rank-1;
	if(!exists("nfactor")){	nfactor < -maxfac}
	if (nfactor > maxfac){  nfactor <- maxfac}
	M_X <- apply(X_ori,2,mean)
	M_Y <- apply(Y_ori,2,mean)
	S_X <- apply(X_ori,2,sd)
	S_Y <- apply(Y_ori,2,sd)
	X <- scale(X_ori)
	Y <- scale(Y_ori)
	n <- nrow(Y)
	nn <- nrow(X)
	np <- ncol(X)
	nq <- ncol(Y)
	if (nn != n){
		stop("Incompatible # of rows for X and Y")
		return(NULL)
	}
	# Precision for Convergence. not used in this version
  #	epsilon<-.Machine$double.eps
	# num of components kept
	# Initialization ----
	# The Y set
	U <- matrix(0,n,nfactor)
	C <- matrix(0,nq,nfactor)
	# The X set
	T <- matrix(0,n,nfactor)
	P <- matrix(0,np,nfactor)
	W <- matrix(0,np,nfactor)
	b <- matrix(0,1,nfactor)
	R2_X <- matrix(0,1,nfactor)
	R2_Y <- matrix(0,1,nfactor)
	RESS <- matrix(0,1,nfactor)
	PRESS <- matrix(0,1,nfactor)
	# Yhat4Press is a cube 
	# of the jackknifed reconstitution of Y
	# Needed to compute PRESS
	Yjack4Press  <- array(0,dim = c(n,nq,nfactor))
	Yhat4Res_rec <- array(0,dim = c(n,nq,nfactor))
	Xres <- X
	Yres <- Y
	SS_X <- sum(X^2)
	SS_Y <- sum(Y^2)
	for (i in 1:nfactor){
		myplsr <- svd(t(Xres) %*% Yres)
#		delta1 <- myplsr$d
		w <- myplsr$u[,1]
		c <- myplsr$v[,1]
		#tt <- Xres%*%w
      	#t <- tt/sqrt(sum(tt^2))
		t <- normaliz(Xres %*% w)
		u <- Yres %*% c
		p <- t(Xres) %*% t
		b_l <- t(u) %*% t	
		# Store in matrices
     	 	b[i] <- b_l
		P[,i] <- p
		W[,i] <- w
		#T[,i]<- t
		T[,i] <- as.matrix(t)
		U[,i] <- u
		C[,i] <- c
		# deflation of X and Y
		Xres <- Xres -t %*% t(p)
		Yres <- Yres - 
		   repmat(b[i], n, nq)*(t %*% t(c)) 
		#R2_X[i] <- (t(t) %*% t) * (t(p)%*%p)/SS_X  #T has an unit norm
		R2_X[i] <- (t(p) %*% p) / SS_X
		R2_Y[i] <- (b[i]^2) / SS_Y
	  #
		    if (nq == 1){ # Problem with how R handles vectors vs matrices
		      Yhat4Res_rec[,,i] <- (T[,1:i]*
		                             repmat(t(b[1:i]),n,1)) %*%
		          as.matrix(C[,1:i]) *
		                             repmat(t(S_Y),n,1)+
		                             repmat(t(M_Y),n,1)    
		    } else {
		Yhat4Res_rec[,,i] <- 
		     (T[,1:i] *
		               repmat(t(b[1:i]),n,1)) %*%
		                 t(C[,1:i]) *
		               repmat(t(S_Y),n,1) +
		              repmat(t(M_Y),n,1)
		    }
		RESS[i] <- sum((Y_ori - Yhat4Res_rec[,,i])^2)
	}
	X_rec<- T %*% t(P)
	Y_rec<- (T *
	          repmat(b,n,1)) %*% t(C)
	#Bring back X and Y to their original units
	Xori_rec <- X_rec*
	           repmat(t(S_X),n,1) +
	           repmat(t(M_X),n,1)
	Yori_rec <- Y_rec*
	            repmat(t(S_Y),n,1)+
	            repmat(t(M_Y),n,1)
	#The Wstar weights give T = X*Wstar
	Wstar <- W %*% (solve(t(P) %*% W))
	# B_pls=Wstar*diag(b)*C'
	B_pls <- (Wstar *
	            repmat(b,np,1)) %*% t(C)
	Bpls_star <- t(
	          repmat(t(S_X^-1),nq,1)) * B_pls *
	          repmat(t(S_Y), np, 1)
	Bpls_star <- rbind(-M_X %*% Bpls_star, Bpls_star)
	Bpls_star[1,] <- Bpls_star[1,] + M_Y
	#Y_pred <- cbind(1,X_ori)%*%Bpls_star
  if (isTRUE(inference)){
	# Now go to the jackknifed version (cross-validation)
	Yjack <- matrix(0,n,nq)
	print(paste0('Fixed Model Done. Start Jackknife for ', n, ' iterations.'))
	for (i in 1:n){# if inference ----
	  if (displayJack){
		print(c('Jackniffing row #:',toString(i))) }
		X4j <- X_ori
		Y4j <- Y_ori
		X4j <- X4j[-i,]
		Y4j <- Y4j[-i,]
		leyhat <- PLS4jack(X4j,Y4j,X_ori[i,],nfactor)
		Yjack[i,] <- leyhat[nfactor,]

		for (l in 1:nfactor){
			Yjack4Press[i,,l] <- leyhat[l,]
		}
		r2_random <- matrix(0,1,nfactor)
		rv_random <- matrix(0,1,nfactor)
		for (l in 1:nfactor){
			PRESS[l] <- sum((Y_ori - Yjack4Press[,,l])^2)
 			CORR <- corrcoef4mat(Y_ori, Yjack4Press[,,l])
			r2_random[l]<-CORR$r2_random
			rv_random[l]<-CORR$rv_random
		}
		Q2 <- 1 - PRESS[nfactor-1]/RESS[nfactor]
		# From Tenenhaus (1988), p83, 138 for Q2(l)
		Q2 <- c(PRESS[1] / (nq*(nn-1)),Q2)  	
	}
	# print('After Jackknife and Q')
  }
	if (isTRUE(inference)){	print('Jackknife done.') }
	#print('nfactor')
	#print(nfactor)
	
	Fact.names <- paste0("Factor ",1:nfactor)
	#print('dimT')
	#print(dim(T))
	rownames(T) <- obs.names
	colnames(T) <- Fact.names
	rownames(P) <- Xvar.names
	#print('dimP')
	#print(dim(P))
	colnames(P) <- Fact.names
	rownames(W) <- Xvar.names
	#print('dimW')
	#print(dim(W))
	colnames(W) <- Fact.names
	rownames(Wstar) <- Xvar.names
	#print('dim-Wstar')
	#print(dim(Wstar))
	colnames(Wstar) <- Fact.names
	rownames(U) <- obs.names
	colnames(U) <- Fact.names
	rownames(b) <- "b"
	colnames(b) <- Fact.names
	rownames(C) <- Yvar.names
	colnames(C) <- Fact.names
	rownames(B_pls) <- Xvar.names
	colnames(B_pls) <- Yvar.names
	rownames(Bpls_star) <- c('Intercept',Xvar.names)
	colnames(Bpls_star) <- Yvar.names
	rownames(Xori_rec)  <- obs.names
	colnames(Xori_rec)  <- Xvar.names
	rownames(Yori_rec)  <- obs.names
	colnames(Yori_rec)  <- Yvar.names

	rownames(R2_X) <- "R2x"
	colnames(R2_X) <- Fact.names
	rownames(R2_Y) <- "R2y"
	colnames(R2_Y) <- Fact.names
	rownames(RESS) <- "RESSy"
	colnames(RESS) <- Fact.names
	if (inference){ # if inference ----
	# if inference:
	#print('dim-PRESS')
	#print(dim(PRESS))
	rownames(Yjack)     <- obs.names
	colnames(Yjack)     <- Yvar.names
	rownames(PRESS) <- "PRESSy"
	#print(PRESS)
	colnames(PRESS) <- Fact.names
	#print(PRESS)
	Q2 <- t(as.matrix(Q2))
	#print('dim-Q2')
	#print(dim(Q2))
	rownames(Q2) <- "Q2"
	colnames(Q2) <- Fact.names[1:length(Q2)]
	rownames(r2_random) <- "Random-r2"
	colnames(r2_random) <- Fact.names
	#print('dim-rv_random')
	#print(dim(rv_random))
	rownames(rv_random) <- "Random-rv"
	colnames(rv_random) <- Fact.names
	dimnames(Yjack4Press)[[1]] <- obs.names
	dimnames(Yjack4Press)[[2]] <- Yvar.names
	dimnames(Yjack4Press)[[3]] <- paste0(1:nfactor,' Factor Solution')
	dimnames(Yhat4Res_rec)[[1]] <- obs.names
	dimnames(Yhat4Res_rec)[[2]] <- Yvar.names
	dimnames(Yhat4Res_rec)[[3]] <- paste0(1:nfactor,' Factor Solution')
	#
	} # end if inference ----
	
	# Return list 4 PLSR_SVD ----
	if (inference){
	  return.list=structure(list(T = T, P = P, W = W,
	                             Wstar = Wstar, U = U, B = b, C = C,
	                             Bpls = B_pls, Bpls_star = Bpls_star,
	                             Xhat = Xori_rec,
	                             Yhat = Yori_rec,
	                             R2x = R2_X, R2y = R2_Y,
	                             RESSy = RESS, 
	                             Yhat4Ress = Yhat4Res_rec,
	                             # Random part ----
	                             Yjack = Yjack, 
	                             PRESSy = PRESS,
	                             Q2 = Q2, 
	                             r2y_random = r2_random, 
	                             rv_random = rv_random,
	                             Yhat4Press = Yjack4Press 
                                 ), 
	                        class = 'PLSR_SVD' 
	  )    
	  
	} else {
	return.list=structure(list(T = T, P = P,
	                           W = W, Wstar = Wstar, U = U,
	                           B = b, C = C,
	                 Bpls = B_pls, Bpls_star = Bpls_star,
	                 Xhat = Xori_rec,
				           Yhat = Yori_rec, #,
				#
				#Yjack=Yjack,
				R2x = R2_X, R2y = R2_Y,
				RESSy=RESS,
				#PRESSy=PRESS,
				#Q2=Q2,r2y_random=r2_random, rv_random=rv_random,
				#Yhat4Press=Yjack4Press, 
				Yhat4Ress = Yhat4Res_rec 
				),
				class = 'PLSR_SVD' 
				)    
	}
	# Note the class "PLSR_SVD"
	# it is "linked" to the
	# function print.PLSR_SVD
	# that modifies the print function so that it will 
	# print the description of the results
	# when the print function is used
	return(return.list)
} # End of function PLSR_SVD

##------------------------------------------------------------##
#                       PLS4jack.R                             #
##------------------------------------------------------------##
# USAGE: PLS4jack(X,Y,xsup, nfactor)
# RETURN: Yhatsup
# PLS regression jackknifed for one supplementary element
# Compute the prediction for one supplementary element
# for 1 to nfactor latent variables
# X active IV matrix, Y active DV matrix
# xsup supplementary IV elements
# Yhatsup nfactor by number of col of Y
#         predicted value corresponding to xsup 
# X=T*P' Y=T*B*C'=X*Bpls  X and Y being Z-scores
#                          B=diag(b)
#    Y=X*Bpls_star with X being augmented with a col of ones
#                       and Y and X having their original units
# T'*T=I (NB normalization <> than SAS)
# W'*W=I
# C is unit normalized,           
# U, P are not normalized 
#  [Notations: see Abdi (2003). & Abdi (2007)
#               available from www.utd.edu/~herve]
# nfact=number of latent variables to keep

# Herve Abdi, Matlab version (2007)
# Lei Xuan, R version, 2012  


# References (see also www.utd.edu/~herve)
#  1. Abdi (2003).
#  Partial least squares regression (PLS-regression). 
#  In M. Lewis-Beck, A. Bryman, T. Futing (Eds):  
#  Encyclopedia for research methods for the social sciences. 
#  Thousand Oaks (CA): Sage. pp. 792-795. 
#  2. Abdi, H. (2007). 
#  Partial least square regression (PLS regression). 
#  In N.J. Salkind (Ed.):  
#  Encyclopedia of Measurement and Statistics. 
#  Thousand Oaks (CA): Sage. pp. 740-744.
#
#' @title in PLS regression (PLSR) compute a
#' supplementary projection for a jackknifed estimation of
#' one supplementary element.
#' The prediction is performed 
#' for 1 to \code{nfactor} latent variables.
#' @description 
#' in PLS regression (PLSR),\code{PLS4jack}:  computes a
#' supplementary projection for a jackknifed estimation of
#' one supplementary element.
#' The prediction is computed
#' for 1 to \code{nfactor} latent variables. 
#' \code{PLS4jack} is mainly used  by
#' \code{\link{PLSR_SVD}}
#' for computing the random effect prediction of jackknifed
#' observations in PLSR, but it can also be used to
#' project supplementary observation in PLSR. 
#' 
#' 
#' @param X the **X** matrix of predictors in the PLSR model.
#' @param Y the **Y** matrix to be predicted by tge PLSR model.
#' @param xsup the supplementary elements whose **Y** values
#' are to be predicted.
#' @param nfactor number of factors of the model.
#' @return  \code{Yhatsup} the matrix of the predicted values. 
#' @details see Abdi (2010) for details and examples.
#' @author Hervé Abdi, Lei Xuan 
#' #' @references 
# References 
#' (see also \code{https://personal.utdallas.edu/~herve/})
#' 1. Abdi, H. (2010). 
#'      Partial least square regression, 
#'     projection on latent structure regression, PLS-Regression. 
#'    \emph{Wiley Interdisciplinary Reviews: Computational Statistics, 
#'     2}, 97-106.
#'  2. Abdi, H. (2007). 
#'     Partial least square regression (PLS regression). 
#'     In N.J. Salkind (Ed.):  
#'     \emph{Encyclopedia of Measurement and Statistics}. 
#'     Thousand Oaks (CA): Sage. pp. 740-744.
#'  3. Abdi. H. (2003).
#'     Partial least squares regression (PLS-regression). 
#'     In M. Lewis-Beck, A. Bryman, T. Futing (Eds):  
#'     \emph{Encyclopedia 
#'     for Research Methods for the Social Sciences}. 
#'     Thousand Oaks (CA): Sage. pp. 792-795. 
#'
#' @seealso \code{\link{PLSR_SVD}}
#' @rdname PLS4jack
#' @export 
#' @importFrom MASS ginv
#' @importFrom stats sd
PLS4jack <- function(X, Y, xsup, nfactor){
  #___________________________________________________________________
  # repmat ----
  repmat <-  function (a, n, m){kronecker(matrix(1, n, m), a)}
  #___________________________________________________________________
	#X_ori<-X
	#Y_ori<-Y
	#if(!exists("nfactor")){
	#	nfactor<-qr$rank(X)-1
	#}    
	M_X <- apply(X,2,mean)
	S_X <- apply(X,2,sd)
	if (NCOL(Y)==1){
	     M_Y <- mean(Y)
	     S_Y <- sd(Y) } else {
	     M_Y <- apply(Y,2,mean)
   	   S_Y <- apply(Y,2,sd)
	     }
	X <- scale(X)
	Y <- scale(Y)
	n <- nrow(Y)
	nn <- nrow(X)
	np <- ncol(X)
	nq <- ncol(Y)
	if(nn != n){
		print("Incompatible # of rows for X and Y")
	}	
	Yhatsup <- matrix(0,nfactor, nq)	
	#Precision for convergence
	epsilon <- .Machine$double.eps
	# number of components kept
	# Initialization
	# The Y set
	U <- matrix(0,n,nfactor)
	C <- matrix(0,nq,nfactor)
	# The X set
	T <- matrix(0,n,nfactor)
	P <- matrix(0,np,nfactor)
	W <- matrix(0,np,nfactor)
	b <- matrix(0,1,nfactor)
	Xres <- X
	Yres <- Y
	for (j in 1:nfactor){
		plsr <- svd(t(Xres)%*%Yres)
#		delta1<-plsr$d
		w <- plsr$u[,1]
		c <- plsr$v[,1]
		#tt<-Xres%*%w	
		#t<-tt/sqrt(sum(tt^2))
	      t <- normaliz(Xres %*% w)
		u <- Yres %*% c
		p <- t(Xres) %*% t
		b_l <- t(u) %*% t	
		# Store in matrices
      	b[j] <- b_l
		P[,j] <- p
		W[,j] <- w
		T[,j] <- t
		U[,j] <- u
		C[,j] <- c
		# deflation of X and Y
		Xres <- Xres -t %*% t(p)
		Yres <- Yres - 
		         repmat(b[j], n, nq) * (t %*% t(c))
		Wstar<-W %*% (MASS::ginv(t(P) %*% W))
		B_pls<-(Wstar*
		          repmat(b, np, 1)) %*% t(C)
		Bpls_star <- t(
		         repmat(t(S_X^-1), nq, 1)) * B_pls*
		         repmat(t(S_Y), np, 1)
		Bpls_star <- rbind(-M_X %*% Bpls_star, Bpls_star)
		Bpls_star[1,] <- Bpls_star[1,] + M_Y
		Yhatsup[j,] <- c(1,xsup) %*% Bpls_star	
	}
	return(Yhatsup)
}


##------------------------------------------------------------##
#                     corrcoef4mat.R                           #
##------------------------------------------------------------##
# USAGE: corrcoef4mat(Y1,Y2)
# RETURN: A list of r2, rv
# Compute 1. the squared coefficient of correlation between matrices
#         2  Rv coefficient
#
#' @title Compute the squared correlation and 
#' \eqn{R_V} coefficients between two conformable matrices.
#' @description \code{corrcoef4mat: }
#' Computes the squared correlation and 
#' \eqn{R_V} coefficients between two conformable matrices.
#' @details \code{corrcoef4mat:} is mainly used by 
#' \code{\link{PLSR_SVD}}.
#' @param Y1 The first \eqn{I} by \eqn{J} matrix
#' @param Y2 The first \eqn{I} by \eqn{J} matrix
#' @return A list with \eqn{R^2} and the \eqn{RV} coefficient between
#' the matrices \code{Y1} and \code{Y2}.
#' @examples 
#' \dontrun{
#' X = matrix(c(4,2,8,7), nrow = 2)
#' Y = matrix(c(1,7,2,9), nrow = 2)
#' corrXY <- corrcoef4mat(X,Y)
#' }
#' @rdname corrcoef4mat
#' @export 
corrcoef4mat<-function(Y1,Y2){
	y1 <- c(Y1)
	y2 <- c(Y2)
	rv<-(y1 %*% y2)^2 / ((y1 %*% y1) * (y2 %*% y2))

	y1 <- y1-mean(y1)
	y2 <- y2-mean(y2)
	r2 <- (y1 %*% y2)^2 / ((y1 %*% y1)*(y2 %*% y2))

	return.list = list(rv_random = rv, r2_random = r2)
	return(return.list)

}

##------------------------------------------------------------##
#                     normaliz.R                               #
##------------------------------------------------------------##
# USAGE: normaliz(F)
# RETURN: f
# normalize send back a matrix normalized by column
# (i.e., each column vector has a norm of 1)
# Hervé Abdi: Matlab version June 2007
# Lei Xuan: R version November 2012. Revision 2020 HA.

#' @title normalize and send back a matrix normalized by column
#' (i.e., each column vector has a norm of 1).
#' @description \code{normaliz: }
#' normalizes and send back a matrix normalized by column
#' (i.e., each column vector has a norm of 1).
#' \code{normaliz} is used by \code{\link{PLSR_SVD}}.
#' @param F a matrix
#' @return the column normalized version of \code{F}
#' @author Hervé Abdi, Lei Xuan
#' @examples 
#' \dontrun{
#' X <-  matrix(c(4,2,8,7), nrow = 2)
#' Z <- normaliz(X)
#' }
#' @rdname normaliz
#' @export 

normaliz<-function(F){
  repmat <-  function (a, n, m){kronecker(matrix(1, n, m), a)}
	v <- matrix(0,1,ncol(F))
	for (j in 1:ncol(F)){
		v[j] <- sqrt(sum(F[,j]^2))
	}
	f <- F /  repmat(v,nrow(F),1)  
  return(f)
}
## testing ... normaliz(Xres%*%w)


#_____________________________________________________________________
# print.PLSR_SVD ----
# Change the print function for the PLSR_SVD
# environment
#

#' @title print function for \code{print.PLSR_SVD}
#' @description print function for \code{print.PLSR_SVD} objects.
#' @param x The input
#' @param ... everything else
#' @return printed help
#' @rdname print.PLSR_SVD
#' @author Hervé Abdi
#' @seealso \code{\link{PLSR_SVD}}
#' @export 

print.PLSR_SVD <- function (x, ...) { 
  ndash = 75 # How many dashes for separation lines
  cat(rep("-", ndash), sep = "")
  cat("\nPartial Least Square regression (PLSR)\n")
# cat("\n List name: ",deparse(eval(substitute(substitute(x)))),"\n")
  cat(rep("-", ndash), sep = "")
  cat('\n                        *** Fixed Effects ***')
  cat("\n$T          ", "X-scores (T-components, T'*T = I).")
  cat("\n$P          ", "X-loadings (X = T*P').")
  cat("\n$W          ", "X-weights (T = X*W' with W'*W = I).")
  cat("\n$Wstar      ", "X-*weights.")
  cat("\n$U          ", "Y-scores (U-components, U = Y*V).")
  cat("\n$B          ", "b-weight slopes vector for Y (Y = T*diag(B)*C').")
  cat("\n$C          ", "Y-loadings (Y = T*diag(b)*C').")
  cat("\n$Bpls       ", "B for Z(Y) prediction: Z(Y) = Z(X)*Bpls.")
  cat("\n$Bpls_star  ", "B for Y prediction: Y = X*Bpls_star.")
  cat("\n$Xhat       ", "Xhat (X predicted from T*P').")
  cat("\n$Yhat       ", "Yhat [Y predicted from T*diag(B)*C'].")
  cat("\n$R2x        ", "Correlation between X and Xhat.")
  cat("\n$R2y        ", "Correlation between Y and Yhat.")
  cat("\n$RESSy      ", "Y Residual Estimated Sum of Squares (fixed effect).")
  cat("\n$Yhat4Ress  ", "I*K*nfact array: Predicted Y for 1:nfact (fixed).")
  if (!is.null(x$Yjack)){
  cat('\n                      *** Random Effects ***')
  cat("\n$Yjack      ", "Yhat from Leave One Out,LOO (random effect).")
  cat("\n$PRESSy     ", "Y Predicted Residual Estimated Sum of Squares (random effect).")
  cat("\n$Q2         ", "Q2 index (random effect inference).")
  cat("\n$r2y_random ", "R2 Y and Yhat, random effect.")
  cat("\n$rv_random  ", "Rv coefficient random effect.")
  cat("\n$Yjack4Press", "I*K*nfact array: Predicted Y for 1 : nfact (random).")
  }
  cat("\n",rep("-", ndash), sep = "")
  cat("\n")
  invisible(x)
}
