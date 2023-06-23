# ########################################################
# Title         : createDF_kndviclim.R
# Description   : create a df of X | Y | Date | KNDVI | T2M | VPD | SSR | SM | SPEI 
#                 for each point in the ncdf files
#                 create a df of mean, coeff of var, 1-lag TAC for each variable
#                 separate script can plot these variables
# Aims          : two dataframes of all data and RF predictor data for each X/Y
# Inputs	      : netcdf containing KNDVI and CLIMATE data
# Outputs	      : two dataframes
# Options	      : 
# Date          : 4/11/22
# Version       : 2 (26/01/23)
# Authors       : Mark Pickering & Agata Elia
# Maintainer    : Mark Pickering 
# Notes		      : 
# Example use   : 
# ########################################################

###################################################
######     INITIALISE                         #####
###################################################
rm(list = ls())                                   # remove loaded objects

source('/eos/jeodpp/data/projects/FOREST-RESILIENCE/GIT-FOREST-RESILIENCE/FOREST-RESILIENCE/code/main/initialise_R.R')

######     GLOBAL VARS                        #####
script_info <- 'createDF_kndviclim_fullTS'               # used in output name

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
input_kndvi   <- 'GIT-FOREST-RESILIENCE/FOREST-RESILIENCE/sandbox/agata/kndvi_sigma/sigma_mean_nc_rescaled10_masked_005/'

# set/create output directory
output_path <- 'GIT-FOREST-RESILIENCE/FOREST-RESILIENCE/sandbox/agata/kndvi_sigma/sigma_df/'
print(paste0('output_path is : ', output_path ))
if(! dir.exists(output_path)) {dir.create(paste0(output_path),recursive=T)} # create directory if not present

###################################################
######       extract ncdfs - rast             #####
###################################################

v_variable_i <- 'kndvi'
file_i_baseVar <- paste0(input_kndvi, 'modisKNDVISimplifiedBandMean_rescaled10_masked50_005.nc')            # 
r_i_baseVar    <- rast(file_i_baseVar)
df_var_simplified <- terra::as.data.frame(r_i_baseVar, xy = T, na.rm = NA) # set to NA for cells with NA in all layers removed
names(df_var_simplified)[3] <- 'kndvi_simplified'
df_var <- terra::as.data.frame(r_i_baseVar, xy = T, na.rm = NA) # set to NA for cells with NA in all layers removed
names(df_var)[3] <- v_variable_i
save(df_var, file=paste0(output_path, 'df_', v_variable_i, '_simplified_baseVar_full.RData' )    )

v_variable_i <- 'kndvi'
file_i_baseVar <- paste0(input_kndvi, 'modisKNDVIFixedSigmaBandMean_rescaled10_masked50_005.nc')            #
r_i_baseVar    <- rast(file_i_baseVar)
df_var_fixed <- terra::as.data.frame(r_i_baseVar, xy = T, na.rm = NA) # set to NA for cells with NA in all layers removed
names(df_var_fixed)[3] <- 'kndvi_fixed'
df_var <- terra::as.data.frame(r_i_baseVar, xy = T, na.rm = NA) # set to NA for cells with NA in all layers removed
names(df_var)[3] <- v_variable_i
save(df_var, file=paste0(output_path, 'df_',v_variable_i, '_fixed_baseVar_full.RData' )    )

df_input <- left_join(df_var_simplified, df_var_fixed)
df_input <- na.omit(df_input)
save(df_input, file=paste0(output_path, 'df_joined.RData' )    )
