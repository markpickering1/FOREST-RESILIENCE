# ########################################################
# Title         : initialise_R.R
# Description   : This script initialises certain environmental variables in order to run the analysis R code. Script should be sourced at the start of the R files in order to set the working directory and standardise the location of the I/O area
# Date          : 26/01/23
# Authors       : Mark Pickering & Agata Elia
# Maintainer    : Mark Pickering 
# Notes         : 
# ########################################################

# dir root of analysis
root_project <- "/eos/jeodpp/data/projects/FOREST-RESILIENCE/"
# dir of input data
root_data_input <- paste0(root_project, "data/")
# dir of intermediate data
root_data_proce <- paste0(root_project, "data_processing/")
# dir of code
root_code <- paste0(root_project, "GIT-FOREST-RESILIENCE/FOREST-RESILIENCE/")
# dir of output figures
root_figures <- paste0(root_project, "figures/")

# initialise working directory
setwd(root_project)

