setwd("..")
library(roxygen2)
roxygenise()

# get argument
uf <- commandArgs()
input_file1  <- uf[2]
input_file2  <- uf[3]

# launch DM filler tool
WRITE_DM_BP(input_file1)

# launch orcid input
add_orcid(input_file2)