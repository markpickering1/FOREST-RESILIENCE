# ########################################################
# Title         : 1_createDf_phenology_stats.R
# Description   : convert yearly tif of greenup and dormancy in days since 1 Jan 1970 to df of greenup and dormancy in DOY
# Aims          : dataframe of yesrly greenup and dormancy
# Inputs	      : list of tif files of yearly greenup and dormancy
# Outputs	      : one dataframe for greenup and one dataframe for dormancy
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
script_info <- 'createDF_phenology_stats'            # used in output name

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

# initialise input directory
input_gs_folder   <- 'data/ancillary/phenology/modis_phenology_stats/'

# initialise input file name root
input_median_file_name <- 'modis_lcd_greenup_dormancy_1_median.tif'
input_mode_file_name <- 'modis_lcd_greenup_dormancy_1_mode.tif'

# set/create output directory
output_path <- paste0(root_data_proce, script_info, '/', script_info, '_', full_date,  '/')
print(paste0('output_path is : ', output_path ))
if(! dir.exists(output_path)) {dir.create(paste0(output_path),recursive=T)} # create directory if not present

#########################################################
######       extract greenness and dormancy df      #####
#########################################################

greenup_df <- data.frame()
dormancy_df <- data.frame()

# select phenology tif file
phenology_median <- paste0(input_gs_folder, input_median_file_name)
phenology_mode <- paste0(input_gs_folder, input_mode_file_name)
print(phenology_median)
print(phenology_mode)

# select greenup (band 1 in tif files)
greenup_median <- raster(phenology_median, band = 1)
greenup_mode <- raster(phenology_mode, band = 1)
# turn into df
greenup_median_df <- terra::as.data.frame(greenup_median, xy=T)
greenup_mode_df <- terra::as.data.frame(greenup_mode, xy=T)
# change column name
colnames(greenup_median_df)[3] <- 'median'
colnames(greenup_mode_df)[3] <- 'mode'
# adjust for negative doy
greenup_median_df$median[greenup_median_df$median<0] <- greenup_median_df$median[greenup_median_df$median<0] + 365
greenup_mode_df$mode[greenup_mode_df$mode<0] <- greenup_mode_df$mode[greenup_mode_df$mode<0] + 365
# set 0 to NA
greenup_median_df <- greenup_median_df %>%  mutate(median = na_if(median, 0))
greenup_mode_df <- greenup_mode_df %>%  mutate(mode = na_if(mode, 0))
# create df or join to existing dfs
greenup_df <- inner_join(greenup_median_df, greenup_mode_df)

# select dormancy (band 2 in tif files)
dormancy_median <- raster(phenology_median, band = 2)
dormancy_mode <- raster(phenology_mode, band = 2)
# turn into df
dormancy_median_df <- terra::as.data.frame(dormancy_median, xy=T)
dormancy_mode_df <- terra::as.data.frame(dormancy_mode, xy=T)
# change column name
colnames(dormancy_median_df)[3] <- 'median'
colnames(dormancy_mode_df)[3] <- 'mode'
# adjust for negative doy
dormancy_median_df$median[dormancy_median_df$median<0] <- dormancy_median_df$median[dormancy_median_df$median<0] + 365
dormancy_mode_df$mode[dormancy_mode_df$mode<0] <- dormancy_mode_df$mode[dormancy_mode_df$mode<0] + 365
# set 0 to NA
dormancy_median_df <- dormancy_median_df %>%  mutate(median = na_if(median, 0))
dormancy_mode_df <- dormancy_mode_df %>%  mutate(mode = na_if(mode, 0))
# create df or join to existing dfs
dormancy_df <- inner_join(dormancy_median_df, dormancy_mode_df)

# save merged output - avoid overwriting
if( ! file.exists(paste0(output_path, "greenup_doy_pos_median_mode.RData")) ){ save(greenup_df, file=paste0(output_path, "greenup_doy_pos_median_mode.RData"))}
if( ! file.exists(paste0(output_path, "dormancy_doy_pos_median_mode.RData")) ){ save(dormancy_df, file=paste0(output_path, "dormancy_doy_pos_median_mode.RData"))}