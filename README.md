	
# data4PCCAR

An `R`-package containing a collection of data sets and 
`R`-functions to accompagny and complement:
Abdi, H., & Beaton, D. (to appear, 2022).
*Principal Component and Correspondence Analyses Using R*.
New York: Springer Verlag. 

In time, `data4PCCAR` will also contain the code used in the book 
and the resulting figures.

## Introduction

`data4PCCAR` is a collection of _data sets_  and `R`-_functions_ 
that are used (or could have been used, should have been used, or might be used) 
to illustrate Principal Component Analysis (PCA), 
Correspondence Analysis (CA), and related techniques 
such as Multiple Correspondence Analysis (MCA), 
and Multi-Dimensional Scaling (MDS).

## Installation

To install `data4PCCAR` from `Github` 
use the function `install_github()`  
from the package `remotes`:

```{r}
#install.packages("remotes") 
#decomment the line above if the package `remotes` is not yet intalled
remotes::install_github("HerveAbdi/data4PCCAR") # install data4PCCAR
```


## Current data sets

### alcoholInEurope

The average consumption of 4 types of alcoholic beverages in 22 European and neighboring countries.
To be used to illustrate the use of CA and Hellinger analysis.

### blindSortingWines
Wine experts and wine novices 
(blind) sorted red, rosé, and white wines.
Can they make the difference?
To be used for 
multidimensional scaling methods  (MDS, Procrustean,  or DISTATIS).

### colorsOfMusic

A set of 12 children and 10 adults picked up the color that best describes
each of 9 pieces of music.
To be used to illustrate CA and Partial Triadic CA (a variant of CA, a.k.a. PTCA).

### colorsOfVowels

A set of 133 (French) participants associated 6 colors to 6 vowels (data from Chastaing, 1961).
To be used to illustrate CA.

### deadInTheUSA

A contingency table storing the causes of death as a function of age in the USA in 2001.
To be used to illustrate CA.

### dimensionsOfDepression

The similarity (measured on a scale from 0 to 1000, with bigger numbers
meaning more similar)
between 15 psychometric instruments (i.e. scales)
 measuring depression.
 
 ### fiveBeers5Judges
 
 Five tasters evaluated
 five beers on dimensions 
 that they had previously chosen.
 These data can be used to illustrate,
 (among others) 
 Multiple Factor Analysis (MFA),
STATIS,  or Distatis.
 

### fiveBeersHedonics

Five tasters evaluated (using a 10 point rating scale)
how much they liked five beers.
To be used to illustrate 
how to use MCA with "fuzzy coding" with rating scales.

### fiveWines4Rotation
Five (rather fictitious) wines are described by their Sensory and Chemistry variables.
These wines are used to illustrate `Varimax` rotation and partiall least square regression (PLSR).

### foodInFrance

How much 12 typical French families spent on different types of food (including Wine!) in the 1950's.
To be used to illustrate non-normalized (a.k.a. covariance based) PCA.


### foodOfTheWorld

Twenty-six world cuisines 
are described by their (82) cooking ingredients.
To be used to illustrate CA. 

### graduatingInUSA

A set of contingency tables showing the number of university graduates in the USA from 1966 to 2001 by fields.
The grand contingency table gives the The 34 years * 8 fields results. The partial contingency tables
subset the grand contingency table by gender (women vs men) and by levels (Bachelor, Master, and Ph.D.).
To  be used to illustrate CA, PTCA, and variations of these methods.

### mentalImageryOSIQ

A data set storing the (simulated) results of 2,100 participants 
answering a Likert scale like questionnaire comprising 30 questions 
about mental imagery 
(called the Object-Spatial Imagery Questionnaire: OSIQ). 
Half of the questions concern mental imagery for object 
and the other half ot the questions concern mental imagery 
for spatial locations. 
To be used to illustrate PCA, Varimax rotation, and also MCA.


### sixAuthorsPunctuated

The punctuation used by 6 classical French Writers 
(Rousseau, Chateaubrian, Hugo, Zola, Proust, Giraudoux)
*plus* the punctuation of one chapter of Abdi's *Les Réseaux de Neurones*. 
A classic example for CA.

### sixBeers12Descriptors10Judges

   The average data obtained from ten tasters 
who evaluated the intensity of twelve descriptors for six wheat-based beers. 
These data can be used to illustrate: 
un-normed (or normed) Principal Component Analysis (PCA)
 or Correspondence Analysis (CA).


### sixFaces

The (230 by 240 = 55,200 pixels) pictures of six faces (3 men and 3 women). 
To be used to illustrate PCA or PLS on images.


### sixteenGums4Descriptors
  
  The average data obtained from ten tasters who evaluated
   the intensity of four descriptors
   for  sixteen chewing-gums. These data can be used to illustrate:
    normed Principal Component Analysis (PCA) or Multiple Correspondence Analysis (MCA).

### twentyWines

A baby example: 20 red wines (10 French, 10 American) are rated by an expert on multiple dimensions.
To be used to illustrate PCA.

### winesOf3Colors

A set of 36 wines (from Argentina, Canada, and the USA) 
are described by a variety of variables 
(descriptors, chemistry, and sensory). 
To be used to illustrate PCA with supplementary variables 
(could also be used for two table methods such as CCCA, PLSC),
PLSCA, and PLSR).

## Current functions 


### addArrows

Adds arrows to a factorial map
(best with a circle of correlation as created, e.g., by `addCircleOfCor`). 
Used for loadings for PCA maps.

### addArrowsAndNames
Computes the biplot coordinates of variables
for a PCA computed with `ExPosition::epPCA`.

### addCircleOfCor

Adds a *Circle of Correlation* to a PCA-like map
 of correlation produced by `createFactorMap` 
*et alia.* Used for loadings (when seen as correlation between factors and variables)
or cosines for PCA maps.

### addLines4MCA

 A `ggplot2` based function that
adds lines to join the levels of
the qualitative variables in a factorial
 map of an MCA (e.g., computed with
`ExPosition::epMCA` and created with
 `createFactortMap`). Used for MCA maps.

### BinQuant

 Creates (roughly) balanced
factors from quantitative variables.
Used to create nominal variables from
quantitative variables for MCA.

### boot.eigen

Generates
a bootstrap distribution of eigenvalues
for a given (rectangular) matrix.

### Boot4CCA

Creates a Bootstrap Cube for CCA (Canonical Correlation Analysis):
Creates Bootstrap Cubes for the I and J sets
of a CCA
obtained from bootstraping the rows
of the two data-tables used for CCA.
Companion function of  `tepCCA`.  Note: This function is still in the beta stage.

### Boot4PLSC 

Creates a Bootstrap Cube for PLSC (partial least square correlation):
Creates Bootstrap Cubes for the I and J sets
of a PLSC
obtained from bootstraping the rows
of the two data-tables used for PLSC.

### BR4varMCA 

Computes pseudo Bootstrap Ratios (BR) from pseudo-$F$ for variables in MCA.
Used to create BR-like histogram comparable to BR such as computed in PCA.

### buildRandomImage4PCA
 
Makes a random image (with similar PCA loadings) 
 of a matrix of integer data such as Likert scale data.
 Uses two helper functions: `getRandomFS` and `recode2Integer`:
 

### coloringLevels

Creates a vector of color names for the levels
 of an MCA from the color names of the variables.
 Used for creating mqps in MCA.


### compS

A helper function for the functions `perm4PLSC` and `Boot4PLSC`, 
computes the cross-product
of two comformable matrices
using various types of normalization and centering.

### corrcoef4mat

An efficient routine to compute squared correlation and
RV coefficient between 2 comformable matrices.

### ctr4Variables

 Computes contributions (or squared cosines)
for (qualitatitve)  variables from the contributions
of the levels of the qualitative variables.
Used to get contributions in MCA. 

### eig4CA

A very fast and bare-bone function that computes
the eigenvalues 
(and possibly the row and column factor scores)
of the Correspondence Analysis (CA)
of a data matrix suitable for CA
(i.e., a matrix whose all entries are non-negative).

### epVari

A Varimax rotation on loadings and factor scores.
Also gives the pseudo-eigenvalues and percentage of inertia.
Used post-PCA to get nice clean components.
`epiVari` requires an a-priori number of dimensions.

### fastBoot4CA

Used
for  Correspondence Analysis
(CA), creates   a Bootstrap Cube 
obtained from bootstrapping the observations
from a true contingency table. `fastBoot4CA` is based  on the base
`R` function `rmultinom`.

### fastPerm4CA 

Used
for  Correspondence Analysis
(CA),
computes a permutation test
for  CA when CA is performed
on a true contingency table.
`fastPerm4CA` is based  on the base
`R` function `rmultinom`.

### firstpos

 Changes the sign of the values of the columns 
 in one matrix or two matrices 
according to the sign of the elements in the first line 
 of the first matrix.  Used to standardize an eigenvector matrix 
 ortwo matched  singular vector matrices.
 
 ### firstpos4ExPo
  Makes positive the first value of factor scores and
 singular vectors from the output of functions from
  the `ExPosition` package such as 
  `epCA`  (see also `firstpos` 
  and `renormInertiaExPo` ).

### getBiplotCoor
Computes the biplot coordinates of variables
for a PCA computed with  `ExPosition::epPCA`.

### getImportantCtr.12

Gets the items with contributions important for a factorial plane.
`getImportantCtr.12` gives the contributions of the items 
to the plane, the items important for the plane, 
and the items important for the plane or 
for at least one dimension of the plane.

### getRandomFS

Generates multivariate random observations (i.e., factor scores)
    that match the variance/eigenvalues 
    of the dimensions (as defined by their loadings). 


### malinvaudQ4CA.perm 

Computes the Malinvaud / Saporta test for
the omnibus and dimensions of a correspondence 
analysis (CA). `malinvaudQ4CA.perm` gives
the asymptotic Chi2 values and their associated 
*p*-value under the usual assumptions and from a permutation test.

### monteCarlo.eigen

A Monte Carlo generation of (random) eigenvalues to match
a data matrix. 
Used to implement the *parallel* test
for the number of reliable eigenvalues.

###  multinomCV4CA 

A very fast function that computes the permuted and bootstrapped eigenvalues
of the correspondence analysis (CA) of a matrix suitable
for CA (i.e., a matrix with non negative elements).


### nameExpositionResults

Adds dimension names to the results of analyses performed with` ExPosition`
(recall that column names are needed for plots created with `ggplot2` when
using `aes()`). Note that if there is a space is a name (as is the case by default),
`aes_string()` may, in some contexts,  have to be used in lieu of `aes()`.

### normaliz

Normalizes a matrix  by column
(i.e., each column vector has a norm of 1).


###  perm4CCA
Computes a permutation test for Canonical Correlation Analysis 
(CCA,  (as implemented
 in `tepCCA`).
Computes an omnibus permutation test and
specific tests for the eigenvalues when
performing a CCA.

###  perm4PLSC
Computes a permutation test for PLSC (as implemented
 in `TExPosition::tepPLS`.
Computes an omnibus permutation test and
specific tests for the eigenvalues when
performing a PLSC.

### phi2CT

A fast function to compute the $\phi^2$  correlation 
coefficient for a contingency table.
Used for CA and MCA and
as a helper function
by function `phi2Mat4BurtTable`.
 
 
###  phi2Mat4BurtTable

Computes the $\phi^2$ (correlation) matrix
and Burt table
for data tables suitable for
MCA. 
Useful to create correlation heatmap 
for MCA.
Also a helper function,
 used for MCA.
 
 ### PLS4jack
 In PLS regression  `PLS4jack` compute a
 supplementary projection for a jackknifed estimation of
 one supplementary element.
 `PLS4jack` is mainly used  by
 `PLSR_SVD` for computing jackknifed estimates.
 
 ### PLSR_SVD
 
  PLS regression  (PLSR) using the Singular
 Value Decomposition (SVD) instead of the original NIPALS
 (faster for large data sets).

### projSupplementaryObservations4PLSC

Computes latent variables / factor scores for supplementary
observations in a  `PLSC` analysis. 
Note: `projSupplementaryObservations4PLSC` is mostly used
 as a preparatory step for
`supplementaryObservations4PLSC`.

### recode2Integer

Recodes a vector of real values to integers matching 
    a given distribution for the integers.
 
 ### renormInertiaExPo
 
 Renorms
 the output of an `ExPosition` function (e.g., 
`epCA, epPCA, epMCA`) to a given constant.
 
 ### scale0

Centers and normalizes a vector to norm 1 or to norm
$(N - 1)$. The function
`scale0` gives the same results as `scale`
except
1)  for the option `'SS1'` (`scale0` normalizes to 1); 
when `x`
comprises all identical values and 2) when the
parameter `scale = TRUE`, `scale` gives back
a vector of `NaN` (because of a division by 0 error),
whereas `scale0` gives back a vector of `0`.

### supplementary4Vari
Computes the projection of supplementary elements 
 (rows or columns) for a PCA (computed with
 `ExPosition::epPCA`)
 followed by a `Varimax` rotation 
 (computed with
 `data4PCCAR::epVari`).
 
 ### supplementaryObservations4PLSC
 
 Computes the value  of the latent variables for
  supplementary observations for a PLSC model 
  from `TExPosition::PLS`.
  
### supplementaryObservations4PLSCA
  
  Computes the value  of the latent variables for
   supplementary observations for a PLSCA model 
   from `TExPosition::PLSCA`.

### supplementaryVariables4PLSC

Projects supplementary variables (columns) 
 for a PLSC analysis 
 (computed with  `TExPosition::tepPLS`.
 Includes the internal function `projOnDualSet4PLSC`.

### supplementaryVariables4PLSCA

Projects supplementary variables (columns) 
 for a PLSCA  analysis 
 (computed with  `TExPosition::tepPLSCA)`.
 

### tepCCA

 A  `TExPosition`-like version of Canonical Correlation
 Analysis (CCA).  Inputs and outputs are the same as `TExPosition::tepPLS`.
 Current version uses a clone of `GSVD` from Derek Beaton's 
 soon-to-be-released-on-CRAN package `GSVD` (see `Github`: 
 `derekbeaton/GSVD` for details`). Note that this version is still under development.

### tepRA

 A  `TExPosition`-like version of Redundancy Analysis.  
 Inputs and outputs are the same as `TExPosition::tepPLS`.
 Current version uses a clone of `GSVD` from Derek Beaton's 
 soon-to-be-released-on-CRAN package `GSVD` (see `Github`: 
 `derekbeaton/GSVD` for details`). 
 Note that this version is still under development.

### theme_ptca2()
A `ggplot2` theme that matches the defaults of the factorial figures in `PTCA4CATA`.
`theme_ptca2` is, a (slightly smarter) cousin of `PTCA4CATA::theme_ptca` (which will be deprecated in the
near future).

 

