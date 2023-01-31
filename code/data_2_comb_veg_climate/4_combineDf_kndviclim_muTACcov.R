# ########################################################
# Title         : 4_combineDF_kndviclim_muTACcov.R
# Description   : combine the separate dfs of the different variables
# Aims          : merge dataframes of all data and RF predictors data for each X/Y
# Inputs	      : df_stats containing KNDVI CLIMATE (&FC) data (mu/CV/TAC)
# Outputs	      : single merged df
# Options	      : 
# Date          : 30/01/23
# Version       : 1
# Authors       : Mark Pickering & Agata Elia
# Maintainer    : 
# Notes		      : 
# Example use   : 
# ########################################################

###################################################
######     INITIALISE                         #####
###################################################
rm(list = ls())                                   # remove loaded objects

# initialise R and plotting environments
source('/eos/jeodpp/data/projects/FOREST-RESILIENCE/GIT-FOREST-RESILIENCE/FOREST-RESILIENCE/code/main/initialise_R.R')

######     GLOBAL VARS                        #####
script_info <- 'combDF_kndviclim_muTACcov'               # used in output name
script_info_input <- 'createDF_kndviclim_muTACcov'       # input data dir 
input_script_date <- '2023-01-26'                        # the 0.05 production after creating the GIT (first kndvi version)

######     SET LIBRARIES                      #####
# library(chron)   # useful for converting time values
library(dplyr)   # use %>%
# library(ncdf4)   # read ncdf
# library(reshape) # reshaping dataframes
# library(raster)  # package for raster manipulation
# library(terra)   # terra library for raster manip
# library(rasterVis)  # enables levelplots
# require(ggplot2)      # for plotting
# require(scales)       # for ggplot2 functions eg oob & trans
# library(ggpubr)       # for arranging ggplots together (ggarrange)
# library(ggExtra)      # more complex figures, marginal hists
# library(grid)       # for making multi-gird plots and tables
# library(gridExtra)  # for making multi-gird plots and tables
# library(lattice)  # for making multi-gird plots and tables
# library(ggplotify)  # plots within levelplots
# library(rgdal)        # 
# library(RColorBrewer) # colour palettes
# library(sf)           # utilise shapefiles etc


###################################################
######       I/O                              #####
###################################################


# initialise input file
input_dir <- paste0(root_data_proce, script_info_input, '/', script_info_input, '_', input_script_date,  '/')

# set/create output directory  - use same as before
output_path <- input_dir
print(paste0('output_path is : ', output_path ))
if(! dir.exists(output_path)) {dir.create(paste0(output_path),recursive=T)} # create directory if not present

#######################################
##### COMBINE DATAFRAMES          #####
#######################################

