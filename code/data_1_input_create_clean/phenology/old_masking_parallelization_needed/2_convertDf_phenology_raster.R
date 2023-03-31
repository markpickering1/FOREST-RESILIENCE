# ########################################################
# Title         : convertDf_raster.R
# Description   : convert a variable in a dataframe in a raster
# Aims          : maps
# Inputs	      : dataframe
# Outputs	      : tif files
# Options	      : 
# Date          : 21/03/23
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
script_info <- 'convertDf_raster'            # used in output name
script_info_input <- 'createDF_phenology_stats'
input_script_date <- '2023-03-21' 

######     SET LIBRARIES                      #####
library(chron)   # useful for converting time values
library(dplyr)   # use %>%
# library(ncdf4)   # read ncdf
library(reshape) # reshaping dataframes
# library(raster)  # package for raster manipulation
# library(terra)   # terra library for raster manip
# library(rasterVis)  # enables levelplots
require(ggplot2)      # for plotting
require(scales)       # for ggplot2 functions eg oob & trans
library(ggpubr)       # for arranging ggplots together (ggarrange)
library(ggExtra)      # more complex figures, marginal hists
library(grid)       # for making multi-gird plots and tables
library(gridExtra)  # for making multi-gird plots and tables
library(lattice)  # for making multi-gird plots and tables
# library(ggplotify)  # plots within levelplots
library(rgdal)        # 
library(RColorBrewer) # colour palettes
library(sf)           # utilise shapefiles etc
library(cowplot)      # for ggdraw

###################################################
######       SET THEME COMMON PLOTTING VARS   #####
###################################################

source(path_figures_init)

###################################################
######       I/O                              #####
###################################################
# this contains the hardcoded extensions of the individual datasets

# initialise input directory
input_dir <- paste0(root_data_proce, script_info_input, '/', script_info_input, '_', input_script_date,  '/')

# initialise input file name root
input_greenup <- 'greenup_doy_pos_median_mode.RData'
input_dormancy <- 'dormancy_doy_pos_median_mode.RData'

# set/create output directory
output_path   <- 'data/ancillary/phenology/modis_phenology_stats_pos/'
print(paste0('output_path is : ', output_path ))
if(! dir.exists(output_path)) {dir.create(paste0(output_path),recursive=T)} # create directory if not present

# load dataframe
load(paste0(input_dir, input_greenup))
load(paste0(input_dir, input_dormancy))

# create greenup and dormancy median dataframes
greenup_median <- greenup_df[c(1, 2, 3)]
dormancy_median <- dormancy_df[c(1, 2, 3)]

# create rasters from dfs
greenup_raster <- rasterFromXYZ(greenup_median, res=c(0.005,0.005), crs="EPSG:4326")
writeRaster(greenup_raster, file=paste0(output_path, 'modis_lcd_greenup_1_median_pos'), format = "GTiff")

dormancy_raster <- rasterFromXYZ(dormancy_median, res=c(0.005,0.005), crs="EPSG:4326")
writeRaster(dormancy_raster, file=paste0(output_path, 'modis_lcd_dormancy_1_median_pos'), format = "GTiff")