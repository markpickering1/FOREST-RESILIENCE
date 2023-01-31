# ########################################################
# Title         : initialise_R.R
# Description   : This script initialises certain environmental variables in order to run the analysis R code. Script should be sourced at the start of the R files in order to set the working directory and standardise the location of the I/O area
# Date          : 26/01/23
# Authors       : Mark Pickering & Agata Elia
# Maintainer    : Mark Pickering 
# Notes         : 
# ########################################################

###################################################
######     COMMON FILE STRUCTURE              #####
###################################################

# dir root of analysis
root_project <- "/eos/jeodpp/data/projects/FOREST-RESILIENCE/"

# dir of input data
root_data_input <- paste0(root_project, "data/")
# dir of intermediate data
root_data_proce <- paste0(root_project, "data_processing/")
# dir of git
root_git <- paste0(root_project, "GIT-FOREST-RESILIENCE/FOREST-RESILIENCE/")
# dir of output figures
root_figures <- paste0(root_project, "figures/")
# dir of main/master code
root_main <-  paste0(root_git, "code/main/")

# initialise working directory
setwd(root_project)

# set path of script to initialise plotting standards
path_figures_init <- paste0(root_main, 'initialise_figs.R')

###################################################
######     COMMON CODE                        #####
###################################################

# extract time and date
start_time <- Sys.time() ; print(start_time)      # initialise time
full_date <- as.character(Sys.time()) ; full_date <- strsplit(full_date, " ")
full_time <- full_date[[1]][2] ; full_date <- full_date[[1]][1]

