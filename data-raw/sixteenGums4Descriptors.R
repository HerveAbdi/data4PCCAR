## code to prepare `sixteenGums4Descriptors` dataset goes here

# usethis::use_data(sixteenGums4Descriptors, overwrite = TRUE)
# Entête ----
# create the data set 
# sixBeeers12Descriptors10udges ----
# Example for a
#    PCA-unnormed + Biplot
# 
# to be pushed to data4PCCAR
# data from Carlos Gomez
# 
# Created March 05 2022.
# Carlos Data: used for the Chapter of the book
#________________________________________________
# Preamble ------
# Clean start
rm(list = ls())
graphics.off()
gc()
# Libraries ----
library(tidyverse) # wranglin'
library(prettyGraphs)
library(wrapr)
#_________________________________________________
## filenames ----
# Here we read the data from the xls file
## Read the data from the sheet
# NB make sure to use an Rproject 
#  to get correct directories
#dir2read <- paste0(getwd(),'/')
#file2read  = 'STAT Base de donnees pour les examples du chapitre Version XLSTAT. V6.xlsm'
#sheet = 'ACP non normée'
#tb <- readxl::read_excel(paste0(dir2read,file2read), sheet)
#_________________________________________________
# We need to enter the data by hand here


zeNames <-  matrix(c(
    "Hollywood Ice fresh", "H_fresh",
    "Oral B Hollywood verte","H_verte",
    "Oral B Hollywood forte","H_forte",
    "Hollywood Green fresh","H_green",
    "Freedent white","F_white",
    "Hollywood Power fresh","H_power",
    "Mentos Pure fresh","M_fresh",
    "Freedent verte","F_verte",
    "Trident verte","T_verte",
    "Trident blue","T_blue",
    "Airwaves extreme","A_extreme",
    "Arwaves verte","A_verte",
    "Freshman's Friend original","S_ori",
    "Bonsai methe forte","B_forte",
    "Bonsai chlorophylle","B_chloro",
    "Chicza mayan rainforest menthe sauvage","C_menthe"),
    ncol = 2, byrow = TRUE,
    dimnames = list(paste0('Gum_',1:16),
                    c("longNames","shortNames") )
) 
# descripteurs ----
descripteurs <- wrapr::qc(Menthol,
                          Spearmint,
                          Peppermint,
                          LongLasting)
# Data ----
laMatData <- matrix(c(7,	2,	4,	60,
                      5,	3,	4,	60,
                      7,	1,	5,	70,
                      6,	4,	5,	40,
                      7,	1,	4,	45,
                      8,	3,	3,	50,
                      7,	2,	4,	65,
                      4,	5,	1,	55,
                      3,	5,	1,	85,
                      4,	2,	4,	90,
                      8,	1,	3,	75,
                      5,	4,	2,	65,
                      10,	0,	3,	70,
                      9,	1,	4,	55,
                      6,	4,	4,	55,
                      1,	2,	1,	20),
                    ncol = 4, byrow = TRUE,
                    dimnames = list(
                        zeNames[,2], descripteurs)    
)

scale <- matrix(c(10, 5, 5, 100))


df <- data.frame(laMatData)
longNames <-  zeNames[,1]
shortNames <- zeNames[,2]
# col4I
col4I.cat <- prettyGraphsColorSelection(
    n.colors = nrow(df) , offset = NULL, 
    starting.color = 87)
color4I <- col4I.cat # maye have to change to reflect mints
color4J <-  prettyGraphsColorSelection(
    n.colors = ncol(df) , offset = NULL, 
    starting.color = 42)
#
sixteenGums4Descriptors <- structure(list(
    ratingsIntensity  = df,
    longNamesBeers    = longNames,
    color4Products    = color4I,
    color4Descriptors = color4J,
    scale  = scale  ),
    class = 'sixteenGums')
# Save the data with usethis
# An easy way to save the datafile
# NB: devtools does not work anymore
# use_data() syntax has changed so we need
# 1) proj_set() and 2) use_data()
# usethis::proj_set(path = '../../data4PCCAR/')
usethis::use_data(sixteenGums4Descriptors, 
                  overwrite = TRUE)
    
#save(sixteenGums4Descriptors, file = "sixteenGums4Descriptors.rda")
    
    
    
    