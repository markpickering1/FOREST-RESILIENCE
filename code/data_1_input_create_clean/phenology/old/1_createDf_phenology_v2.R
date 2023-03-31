# ########################################################
# Title         : 1_createDf_phenology.R
# Description   : convert yearly tif of greenup and dormancy in days since 1 Jan 1970 to df of greenup and dormancy in DOY
# Aims          : dataframe of yesrly greenup and dormancy
# Inputs	      : list of tif files of yearly greenup and dormancy
# Outputs	      : one dataframe for greenup and one dataframe for dormancy
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
script_info <- 'createDF_phenology_v2'            # used in output name

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
  phenology_i <- paste0(input_gs_folder, year_i, input_file_name)
  print(phenology_i)
  
  # select greenup (band 1 in tif files)
  greenup_i <- raster(phenology_i, band = 1)
  # turn into df
  greenup_df_i <- terra::as.data.frame(greenup_i, xy=T)
  # change column name
  colnames(greenup_df_i)[3] <- 'greenup'
  # transform values from days since 1 Jan 1970 to DOY
  greenup_doy_df <- greenup_df_i %>%  mutate(greenup = na_if(greenup, 0)) %>% mutate(greenup = as.Date(greenup, origin = "1970-01-01")) %>% mutate(greenup = format(greenup, format = "%j"))
  colnames(greenup_doy_df)[3] <- paste0('greenup_', year_i)
  # create df or join to existing dfs
  if(year_i == start_year){greenup_df <-  greenup_doy_df
  } else{ greenup_df <- inner_join(greenup_df, greenup_doy_df)}
  
  # select dormancy (band 2 in tif files)
  dormancy_i <- raster(phenology_i, band = 2)
  # turn into df
  dormancy_df_i <- terra::as.data.frame(dormancy_i, xy=T)
  # change column name
  colnames(dormancy_df_i)[3] <- 'dormancy'
  # transform values from days since 1 Jan 1970 to DOY
  dormancy_doy_df <- dormancy_df_i %>%  mutate(dormancy = na_if(dormancy, 0)) %>% mutate(dormancy = as.Date(dormancy, origin = "1970-01-01")) %>% mutate(dormancy = format(dormancy, format = "%j"))
  colnames(dormancy_doy_df)[3] <- paste0('dormancy_', year_i)
  # create df or join to existing dfs
  if(year_i == start_year){dormancy_df <-  dormancy_doy_df
  } else{ dormancy_df <- inner_join(dormancy_df, dormancy_doy_df)}
}

# melt data into long version and rename field
df_var <- melt(greenup_df, id=c("x","y"), variable_name = "year")
colnames(df_var)[4] <- 'greenup'

# save merged output - avoid overwriting
if( ! file.exists(paste0(output_path, "greenup_doy.RData")) ){ save(df_var, file=paste0(output_path, "greenup_doy.RData"))}

# melt data into long version and rename field
df_var <- melt(dormancy_df, id=c("x","y"), variable_name = "year")
colnames(df_var)[4] <- 'dormancy'

# save merged output - avoid overwriting
if( ! file.exists(paste0(output_path, "dormancy_doy.RData")) ){ save(df_var, file=paste0(output_path, "dormancy_doy.RData"))}