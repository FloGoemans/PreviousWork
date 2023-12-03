#--------------------------------------------------------#
# functions.R
#
# Functions needed for project
# Author: Flo Goemans
#
# - knitr_fromR
#--------------------------------------------------------#

#--------------------------------------------------------#
# name:		Knitr_fromR
# description:	Knit R markdown from within other R script
# input:	file name (Rmd file)
# output: 	docx file with same name as inputfile, in outputs subfolder
# sample call:	knitr_fromR("test_markdown.Rmd")
#--------------------------------------------------------#

knitr_fromR <- function(inputFile)
{
  rmarkdown::render(
    inputFile, 
    output_file = sub("analysis", "analysis/output", paste0(substr(inputFile, 1, nchar(inputFile)-4), ".docx"))
  )
}


### end of program