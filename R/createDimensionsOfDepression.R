# Create the Dimensions of Depresssion 
# Data from the sapiens lab see
# https://sapienlabs.org/quantitative-similarity-of-depression-tools/
# https://sapienlabs.org/15-depression-assessment-tools-and-their-differences/
# And also following references for more:
#[1] Fried, E., & Nesse, R. (2015). 
# Depression is not a consistent syndrome: 
#   An investigation of unique symptom patterns in the STAR*D study. 
# Journal Of Affective Disorders, 
# 172, 96-102. doi: 10.1016/j.jad.2014.10.010
# [2] Fried, E. (2017). The 52 symptoms of major depression:
#   Lack of content overlap among seven common depression scales. 
# Journal Of Affective Disorders, 208, 191-197.
# doi: 10.1016/j.jad.2016.10.019
#
# The similarities of depression scales
#

#installpackages('wrapr')
scaleNames <- wrapr::qc("BDI-II",CESDR,CESDC,DASS,CDI2,EPDS,"GDS-LF",
                        HAMD,IDS,MADRS,MFQ,PHQ9,QIDS,RADS2,ZDS)
nScales <-  length(scaleNames)
# Multiplied data by 10 and get rid of the decimal point
simScales <- matrix(
  c(1000, 406, 428, 313, 520, 360, 322, 369, 456, 399, 528, 464, 325, 499, 591, 
    406,1000, 513, 322, 399, 583, 375, 408, 516, 750, 455, 858, 563, 303, 646,
    428, 513,1000, 266, 476, 448, 538, 254, 333, 588, 446, 492, 274, 516, 467,
    313, 322, 266,1000, 284, 503, 395, 221, 297, 330, 248, 280, 191, 292, 458, 
    520, 399, 476, 284,1000, 358, 305, 297, 389, 397, 599, 401, 301, 646, 436, 
    360, 583, 448, 503, 358,1000, 414, 296, 307, 642, 363, 600, 263, 344, 444, 
    322, 375, 538, 395, 305, 414,1000, 222, 328, 538, 276, 342, 210, 303, 434, 
    369, 408, 254, 221, 297, 296, 222,1000, 555, 439, 305, 354, 347, 252, 429, 
    456, 516, 333, 297, 389, 307, 328, 555,1000, 453, 424, 461, 504, 437, 534, 
    399, 750, 588, 330, 397, 642, 538, 439, 453,1000, 420, 767, 420, 324, 517, 
    528, 455, 446, 248, 599, 363, 276, 305, 424, 420,1000, 481, 348, 540, 396, 
    464, 858, 492, 280, 401, 600, 342, 354, 461, 767, 481,1000, 474, 299, 588, 
    325, 563, 274, 191, 301, 263, 210, 347, 504, 420, 348, 474,1000, 243, 407, 
    499, 303, 516, 292, 646, 344, 303, 252, 437, 324, 540, 299, 243,1000, 384, 
    591, 646, 467, 458, 436, 444, 434, 429, 534, 517, 396, 588, 407, 384,1000),
  ncol = nScales)
colnames(simScales) <- scaleNames 
rownames(simScales) <- scaleNames 

longNames <- c(
"Beck Depression Inventory-II (BDI-II)",
"Center for Epidemiologic Studies Depression Scale Revised (CES-D-R)",
"Center for Epidemiological Studies Depression Scale for Children (CES-DC)",
"Depression Anxiety Stress Scales (DASS-42)",
"Children's Depression Inventory (CDI2)",
"Edinburgh Postnatal Depression Scale (EPDS)",
"Geriatric Depression Scale (GDS-LF)",
"Hamilton Depression Rating Scale (HAM-D-21)",
"Inventory of Depressive Symptomatology (IDS-SR)",	
"Montgomery-Asberg Depression Rating Scale (MADRS)",	
"Mood and Feelings Questionnaire (MFQ)",
"Patient Health Questionnaire-9 (PHQ-9)",	
"Quick Inventory of Depressive Symptomatology (QIDS SR-16)",	
"Reynolds Adolescent Depression Scale (RADS2)",
"Zung Self-Rating Depression Scale (ZDS)"
)
Age <- rep("Adults-Geriatric", nScales)
# "Children-Adolescents"
Age[c(11,5,14,3)] <- "Children-Adolescents"
df.supplementary <- data.frame(longNames, Age)
rownames(df.supplementary) <- scaleNames

dimensionsOfDepression <-  structure(list(df.scaleSimilarity = simScales,
                      supplementary.information = df.supplementary),
                           class = 'dimDepression')
# An easy way to save the datafile 
#devtools::use_data(DimensionsOfDepression, pkg = '../data4PCCAR/',
#                   overwrite = TRUE)
# use_data() has moved from devtools to usethis
usethis::use_data(dimensionsOfDepression, #pkg = '../data4PCCAR/',
                   overwrite = TRUE)
