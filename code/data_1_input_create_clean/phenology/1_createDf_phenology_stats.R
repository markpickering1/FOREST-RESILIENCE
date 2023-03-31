# ########################################################
# Title         : 1_createDf_phenology_stats.R
# Description   : convert nc files of greenup and dormancy in DOY (adjusted for <0 and >365) in a df of greenup and dormancy dd-mm
# Aims          : dataframe of greenup and dormancy dd-mm
# Inputs	      : nc files of greenup and dormancy in DOY
# Outputs	      : dataframe of greenup and dormancy dd-mm
# Options	      : 
# Date          : 29/03/23
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
input_gs_folder   <- '/eos/jeodpp/data/projects/FOREST-RESILIENCE/data/ancillary/phenology/modis_phenology_stats_nc_masked_005/'

# initialise input file name root
input_greenup_file_name <- 'modis_lcd_greenup_1_p5_masked50_005_int.nc'
input_dormancy_file_name <- 'modis_lcd_dormancy_1_p95_masked50_005_int.nc'

# set/create output directory
output_path <- paste0(root_data_proce, script_info, '/', script_info, '_', full_date,  '/')
print(paste0('output_path is : ', output_path ))
if(! dir.exists(output_path)) {dir.create(paste0(output_path),recursive=T)} # create directory if not present

#########################################################
######       extract greenness and dormancy df      #####
#########################################################

phenology_df <- data.frame()

# select phenology tif file
phenology_greenup <- paste0(input_gs_folder, input_greenup_file_name)
phenology_dormancy <- paste0(input_gs_folder, input_dormancy_file_name)
print(phenology_greenup)
print(phenology_dormancy)

# select greenup (band 1 in tif files)
greenup <- rast(phenology_greenup)
dormancy <- rast(phenology_dormancy)

# turn into df
greenup_df <- terra::as.data.frame(greenup, xy=T)
dormancy_df <- terra::as.data.frame(dormancy, xy=T)

# change column name
colnames(greenup_df)[3] <- 'doy'
colnames(dormancy_df)[3] <- 'doy'

# adjust for <0 and >365 doy
greenup_df$doy[greenup_df$doy<0] <- greenup_df$doy[greenup_df$doy<0] + 365
greenup_df$doy[greenup_df$doy>365] <- greenup_df$doy[greenup_df$doy>365] - 365
dormancy_df$doy[dormancy_df$doy<0] <- dormancy_df$doy[dormancy_df$doy<0] + 365
dormancy_df$doy[dormancy_df$doy>365] <- dormancy_df$doy[dormancy_df$doy>365] - 365

# change column name
colnames(greenup_df)[3] <- 'doy_greenup'
colnames(dormancy_df)[3] <- 'doy_dormancy'

# create df or join to existing dfs
phenology_df <- inner_join(greenup_df, dormancy_df)

# save merged output - avoid overwriting
if( ! file.exists(paste0(output_path, "phenology_df.RData")) ){ save(phenology_df, file=paste0(output_path, "phenology_df.RData"))}