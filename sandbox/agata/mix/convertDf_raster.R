# ########################################################
# Title         : convertDf_raster.R
# Description   : convert a variable in a dataframe in a raster
# Aims          : maps
# Inputs	      : dataframe
# Outputs	      : tif files
# Options	      : 
# Date          : 15/03/23
# Version       : 1
# Authors       : Agata Elia & Mark Pickering
# Maintainer    : Agata Elia 
# Notes		      : 
# Example use   : 
# ########################################################

###################################################
######     INITIALISE                         #####
###################################################
rm(list = ls())                                   # remove loaded objects

source('/eos/jeodpp/data/projects/FOREST-RESILIENCE/GIT-FOREST-RESILIENCE/FOREST-RESILIENCE/code/main/initialise_R.R')

######     GLOBAL VARS                        #####
script_info <- 'convertDf_raster'               # used in output name

######     SET LIBRARIES                      #####
library(chron)   # useful for converting time values
library(dplyr)   # use %>%
library(ncdf4)   # read ncdf
library(reshape) # reshaping dataframes
library(raster)  # package for raster manipulation
library(terra)   # terra library for raster manip
library(reshape) # melt dfs to wide-long
# library(rasterVis)  # enables levelplots
# require(ggplot2)      # for plotting
# library(ggpubr)       # for arranging ggplots together
# library(ggExtra)      # more complex figures, marginal hists
# library(grid)       # for making multi-gird plots and tables 
# library(gridExtra)  # for making multi-gird plots and tables 
# library(ggplotify)  # plots within levelplots
library(rgdal)        # 
library(RColorBrewer) # colour palettes
library(sf)           # utilise shapefiles etc

###################################################
######       I/O                              #####
###################################################
# this contains the hardcoded extensions of the individual datasets

# initialise input file
input_df   <- '/eos/jeodpp/data/projects/FOREST-RESILIENCE/data_processing/createDF_kndviclim_muTACcov/createDF_kndviclim_muTACcov_2023-03-14/df_kndvi_muTACcov.RData' 
    
# set/create output directory
output_path <- '/eos/jeodpp/data/projects/FOREST-RESILIENCE/GIT-FOREST-RESILIENCE/FOREST-RESILIENCE/sandbox/agata/'

# load dataframe
load(input_df)

# create tac and var dataframes
tac <- df_stats[c(1, 2, 8)]
var <- df_stats[c(1, 2, 5)]

# create rasters from dfs
tac_raster <- rasterFromXYZ(tac, res=c(0.05,0.05), crs="EPSG:4326")
writeRaster(tac_raster, '/eos/jeodpp/data/projects/FOREST-RESILIENCE/GIT-FOREST-RESILIENCE/FOREST-RESILIENCE/sandbox/agata/kndvi_tac_resid.tif', format = "GTiff")

var_raster <- rasterFromXYZ(var, res=c(0.05,0.05), crs="EPSG:4326")
writeRaster(var_raster, '/eos/jeodpp/data/projects/FOREST-RESILIENCE/GIT-FOREST-RESILIENCE/FOREST-RESILIENCE/sandbox/agata/kndvi_sd_var.tif', format = "GTiff")