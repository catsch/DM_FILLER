setwd("src_R")
library(roxygen2)
roxygenise()

# get argument
uf <- commandArgs()
input_file  <- uf[2]

# launch DM filler tool
WRITE_DM_BP(input_file)