# ########################################################
# Title         : gs_mask_compute.R
# Description   : create a growing season mask from a list of yearly greenup and dormancy rasters
# Aims          : mask of growing season
# Inputs	      : list of tif files of yearly greenup and dormancy
# Outputs	      : one raster
# Options	      : 
# Date          : 20/03/23
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
script_info <- 'gs_df.R'            # used in output name

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

# initialise input folder
input_gs_folder   <- 'data/ancillary/phenology/modis_phenology/'

# initialise input file name root
input_file_name <- '_greenup_dormancy_1.tif'

# set/create output directory
output_path <- paste0(root_data_proce, script_info, '/', script_info, '_', full_date,  '/')
print(paste0('output_path is : ', output_path ))
if(! dir.exists(output_path)) {dir.create(paste0(output_path),recursive=T)} # create directory if not present

#########################################################
######       extract greenness and dormancy df      #####
#########################################################

start_year <- 2003 ; end_year <- 2021
greenup_df <- data.frame()
dormancy_df <- data.frame()

for(year_i in start_year:end_year){
  # print current year
  print(year_i)
  # select yearly phenology tif file
  phenology_i <- paste0(input_dir, year_i, input_file_name)
  print(phenology_i)
  
  # select greenup (band 1 in tif files)
  greenup_i <- raster(phenology_i, band = 1)
  # turn into df
  greenup_df_i <- terra::as.data.frame(greenup_i, xy=T)
  # change column name
  names(greenup_df_i)[3] <- paste0(year_i, '_greenup')
  # transform values from days since 1 Jan 1970 to DOY
  greenup_doy_df <- greenup_df_i %>%  mutate(names(greenup_df_i)[3] = na_if(names(greenup_df_i)[3], 0)) %>% mutate(names(greenup_df_i)[3] = as.Date(names(greenup_df_i)[3], origin = "1970-01-01")) %>% mutate(names(greenup_df_i)[3] = format(names(greenup_df_i)[3], format = "%j"))
  # create df or join to existing dfs
  if(i == start_year){greenup_df <-  greenup_doy_df
  } else{ greenup_df <- inner_join(greenup_df, greenup_doy_df)}
  
  # select dormancy (band 2 in tif files)
  dormancy_i <- raster(phenology_i, band = 2)
  # turn into df
  dormancy_df_i <- terra::as.data.frame(dormancy_i, xy=T)
  # change column name
  names(dormancy_df_i)[3] <- paste0(year_i, '_dormancy')
  # transform values from days since 1 Jan 1970 to DOY
  dormancy_doy_df <- dormancy_df_i %>%  mutate(names(dormancy_df_i)[3] = na_if(names(dormancy_df_i)[3], 0)) %>% mutate(names(dormancy_df_i)[3] = as.Date(names(dormancy_df_i)[3], origin = "1970-01-01")) %>% mutate(names(dormancy_df_i)[3] = format(names(dormancy_df_i)[3], format = "%j"))
  # create df or join to existing dfs
  if(i == start_year){dormancy_df <-  dormancy_doy_df
  } else{ dormancy_df <- inner_join(dormancy_df, dormancy_doy_df)}
}

# save merged output - avoid overwriting
if( ! file.exists(paste0(output_path, "greenup_doy.RData")) ){ save(greenup_df, file=paste0(output_path, "greenup_doy.RData"))}
if( ! file.exists(paste0(output_path, "dormancy_doy.RData")) ){ save(dormancy_df, file=paste0(output_path, "dormancy_doy.RData"))}