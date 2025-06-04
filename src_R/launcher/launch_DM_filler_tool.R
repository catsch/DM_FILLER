setwd("src_R")
library(roxygen2)
roxygenise()

# get argument
uf <- commandArgs()
input_file  <- uf[2]