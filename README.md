# data4PCCAR

An `R`-package containing a collection of data sets to accompagny:
Abdi, H., & Beaton, D. (to appear, 2019).
*Principal Component and Correspondence Analyses Using R*. New York: Springer Verlag. 

In time, `data4PCCAR` will also contain the code used in the book and the resulting figures.

## Introduction

`data4PCCAR` is a collection of data sets that are used (or could have been used, or should have been used) to illustrate Principal Component Analysis (PCA),  Correspondence Analysis (CA), and related techniques such as Multiplite Correspondence Analysis (MCA), and Multi-Dimensional Scaling (MDS).

## Installation

To install `data4PCCAR` from `Github` use the function `install_github()`  from the pacakge `devtools`:

```{r}
#install.packages("devtools") #decomment this line if devtools is not yet intalled
devtools::install_github('HerveAbdi/data4PCCAR') # install data4PCCAR
```



## Current data sets

### colorsOfMusic

12 Children and 10 Adults picked up the color that best describes
each of 9 pieces of music.
To be used to illustrate CA and Partial Triadic CA ( a variant of CA, a.k.a. PTCA).

### colorsOfVowels

133 (French) participants associated 6 colors to 6 vowels (data from Chastaing, 1961).
To be used to illustrate CA

### foodOfTheWorld

26 world cuisines are described by their cooking ingredients.
To be used to illustrate CA 

### sixFaces

The (230 by 240 = 55,200 pixels) pictures of six faces (3 men and 3 women). To be used to illustrate PCA or PLS on images.

### sixAuthorsPunctuated

The punctuation used by 6 classical French Writers (Rousseau, Chateaubrian, Hugo, Zola, Proust, Giraudoux)
plus the punction of one chapter of Abdi's *Réseaux de Neurones*. A classic examp[le for CA.

### twentyWines

A baby example: 20 red wines (10 French, 10 American) are rated by an expert on multiple dimensions. Use to illustrate PCA.

