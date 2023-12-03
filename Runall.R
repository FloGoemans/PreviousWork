#--------------------------------------------------------#
# Runall.R
#
# Master program to run project
# Author: Flo Goemans
#--------------------------------------------------------#

#--------------------------------------------------------#
# Setup
#--------------------------------------------------------#

# set project location
filepath <- getwd()
setwd(paste0(filepath))

source("Setup.R", echo=T)
source("functions.R", echo=T)

#--------------------------------------------------------#
# Analysis database creation
#--------------------------------------------------------#

source("./data/data_manipulation.R, echo=T)
#source("./data/ADSL.R", echo=T)
#source("./data/ADTTE.R", echo=T)

#--------------------------------------------------------#
# Exploratory analysis output
#--------------------------------------------------------#
#source("./analysis/distributions.R", echo=T)
source("./analysis/Response_alluvial.R", echo=T)

#report
#knitr_fromR(paste0(filepath, "/analysis/Exploratory_analysis.Rmd"))

#--------------------------------------------------------#
# Regression analysis output (not started yet)
#--------------------------------------------------------#

# replication of previous results
# multiple imputation


## End of Project