# data4PCCAR

An `R`-package containing a collection of data sets and 
\code{R}-functions to accompagny and complement:
Abdi, H., & Beaton, D. (to appear, 2019).
*Principal Component and Correspondence Analyses Using R*. New York: Springer Verlag. 

In time, `data4PCCAR` will also contain the code used in the book 
and the resulting figures.

## Introduction

`data4PCCAR` is a collection of data sets and functions that are used (or could have been used, or should have been used) to illustrate Principal Component Analysis (PCA),  Correspondence Analysis (CA), and related techniques such as Multiple Correspondence Analysis (MCA), and Multi-Dimensional Scaling (MDS).

## Installation

To install `data4PCCAR` from `Github` use the function `install_github()`  from the package `devtools`:

```{r}
#install.packages("devtools") #decomment this line if devtools is not yet intalled
devtools::install_github("HerveAbdi/data4PCCAR") # install data4PCCAR
```



## Current data sets

### alcoholInEurope

The average cunsumption of 4 types of alcoholic beverages in 22 European countries and neighboring countries.
To be used to illustrate the use of CA and Hellinger analysis.

### colorsOfMusic

12 children and 10 adults picked up the color that best describes
each of 9 pieces of music.
To be used to illustrate CA and Partial Triadic CA ( a variant of CA, a.k.a. PTCA).

### colorsOfVowels

133 (French) participants associated 6 colors to 6 vowels (data from Chastaing, 1961).
To be used to illustrate CA.

### deadInTheUSA

A contingency table storing the causes of death as a function of age in the USA in 2001.
To be used to illustrate CA.

### fiveBeersHedonics

Five tasters evaluated (using a 10 point rating scale)
how much they liked five beers.
To be used to illustrate how to use MCA with "fuzzy coding" with rating scales.

### foodInFrance

How much  12 typical French families spent on different types of food (including Wine) in the 1950's.
To be used to illustrate non-normalized (a.k.a. covariance based) PCA.


### foodOfTheWorld

26 world cuisines are described by their cooking ingredients.
To be used to illustrate CA. 

### graduatingInUSA

A set of contingency tables  showing the number of university graduates in the USA from 1966 to 2001 by fields.
The grand contingency table gives the The 34 years * 8 fields results. The partial contingency tables
subset the grand contigency table by gender (women vs men) and by levels (Bachelor, Master, and Ph.D.).
To  be used to illustrate CA, and PTCA, and variations of these methods.

### sixFaces

The (230 by 240 = 55,200 pixels) pictures of six faces (3 men and 3 women). To be used to illustrate PCA or PLS on images.

### sixAuthorsPunctuated

The punctuation used by 6 classical French Writers (Rousseau, Chateaubrian, Hugo, Zola, Proust, Giraudoux)
*plus* the punctuation of one chapter of Abdi's *Réseaux de Neurones*. A classic example for CA.

### twentyWines

A baby example: 20 red wines (10 French, 10 American) are rated by an expert on multiple dimensions. Use to illustrate PCA.

## Current functions 



### boot.eigen

Generates
a bootstrap distribution of eigenvalues
for a given (rectangular) matrix.


### epVari

Varimax rotation on loadings and factor scores.
Also give the pseudo-eigenvalues and percentage of inertia.


### monteCarlo.eigen

Monte Carlo generation of (random) eigenvalues to match
a data matrix. Use to implement the *parallel* test
for the number of reliable eigenvalue

### scale0

Center and normalize a vector to norm 1 or to norm ({_N_} - 1).
`scale0` gives the same results as `scale`
except for the option `'SS1'` (`scale0` normalizes to 1); 
when `x`
comprises all identical values and when the
parameter `scale = TRUE`, `scale` gives back
a vector of `NaN` (because of a division by 0 error),
whereas `scale0` gives back a vector of `0`.

### buildRandomImage4PCA
 
 Makes a random image (with similar PCA loadings) of a matrix of integer data such as Likert scales data.
 Uses two helper functions: `getRandomFS` and `recode2Integer`:
 
 #### getRandomFS
 Generate multivariate random observations (i.e., factor scores) that match the variance/eigenvalues of the dimensions.
 
 ####  recode2Integer
 Recode a vector of real values to integers amtching a given distribution for the integers
 
 

