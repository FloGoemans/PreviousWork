#--------------------------------------------------------#
# Setup.R
#
# Setup locations and install/load packages needed
# Author: Flo Goemans
#--------------------------------------------------------#

#--------------------------------------------------------#
# Point to shared R package library
#--------------------------------------------------------#

.libPaths(new = "S:/R_packages_4.0.5")
.libPaths()

#--------------------------------------------------------#
# Point to shared R package library
#--------------------------------------------------------#

rawpath <- #redacted
excludepath <- #redacted

datapath <- paste0(filepath, "/data/")
analpath <- paste0(filepath, "/analysis/")

#--------------------------------------------------------#
# Packages for project
#--------------------------------------------------------#

#check if packages needed are installed and install only new packages
packages <- c('knitr', 'rmarkdown', 'readr', 'devtools', 'dplyr', 'tidyr', 'ggplot2', 'Hmisc', 'ggalluvial')
install.packages(setdiff(packages, rownames(installed.packages())))

#load packages and print versions
for (pkg in packages){
  library(pkg, character.only=TRUE)
  print(paste0(pkg, " version = ", packageVersion(pkg)))
}

#--------------------------------------------------------#
# Global objects - defined once in project
#--------------------------------------------------------#

# Visit Windows
vis3lower <- 43
vis3upper <- 241

vis12lower <- 300
vis12upper <- 600

### End of program
