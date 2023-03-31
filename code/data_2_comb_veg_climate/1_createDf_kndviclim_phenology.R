# ########################################################
# Title         : 1_createDf_kndviclim_phenology.R
# Description   : attach greenup and dormancy DOY to df and mask pixels out of growing season
# Aims          : mask out pixels out of growing season
# Inputs	      : full df of time series of kndvi and climate variables
# Outputs	      : dataframes
# Options	      : 
# Date          : 30/03/23
# Version       : 1
# Authors       : Mark Pickering & Agata Elia
# Maintainer    : Agata Elia
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
script_info <- 'createDF_kndviclim_phenology'               # used in output name
script_info_input <- 'createDF_kndviclim_fullTS'       # input data dir 
input_script_date <- '2023-03-14'
script_phenology_input <- 'createDF_phenology_stats'
input_phenology_script_date <- '2023-03-29'

######     SET LIBRARIES                      #####
# library(chron)   # useful for converting time values
library(dplyr)   # use %>%
library(stringr)
library(lubridate)
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


# initialise input directory
input_dir <- paste0(root_data_proce, script_info_input, '/', script_info_input, '_', input_script_date,  '/')
input_phenology_dir <- paste0(root_data_proce, script_phenology_input, '/', script_phenology_input, '_', input_phenology_script_date,  '/')

# set/create output directory  - use same as before
output_path <- paste0(root_data_proce, script_info, '/', script_info, '_', full_date,  '/')
print(paste0('output_path is : ', output_path ))
if(! dir.exists(output_path)) {dir.create(paste0(output_path),recursive=T)} # create directory if not present

####################################################################
##### RUN THROUG DATAFRAMES AND ATTACH GROWING SEASON          #####
####################################################################

# load phenology df
load( paste0(input_phenology_dir, 'phenology_df.RData'  ) )

v_variables <- c('kndvi', 't2m', 'VPD', 'spei', 'ssr', 'swvl1', 'tp') #select only variables that have an actual time-series
#rdata_files <- list.files(path=input_dir, pattern=paste0("*"), full.names=FALSE, recursive=FALSE) # view all files

for (i in 1:length(v_variables)){
  
  var_i <- v_variables[i] ; print(var_i)
  
  # open df
  load( paste0(input_dir, 'df_', var_i, '_baseVar' ,'_full.RData'  ) )
  
  # join greenup and dormancy DOY to the df
  df_var <- inner_join(df_var, phenology_df)

  # save joined df
  save(df_var, file=paste0(output_path, 'df_', var_i, '_baseVar' ,'_full_gs.RData'  ) )
  
  # open df
  load( paste0(input_dir, 'df_', var_i, '_deseason' ,'_full.RData'  ) )
  
  # join greenup and dormancy DOY to the df
  df_var <- inner_join(df_var, phenology_df)
  
  # save joined df
  save(df_var, file=paste0(output_path, 'df_', var_i, '_deseason' ,'_full_gs.RData'  ) )
  
} # end var loop

# copy across the forest, socc and heterogeneity data - as they have no time-series
# load(paste0(input_dir, 'df_forestcover_baseVar_full.RData'))
# df_stats <- df_var
# names(df_stats)[3] <- 'mu_var'
# save(df_stats, file=paste0(output_path, 'df_', 'forestcover' , '_muTACcov.RData' )    )
# 
# load(paste0(input_dir, 'df_socc30cm_baseVar_full.RData'))
# df_stats <- df_var
# names(df_stats)[3] <- 'mu_var'
# save(df_stats, file=paste0(output_path, 'df_', 'socc30cm' , '_muTACcov.RData' )    )
# 
# load(paste0(input_dir, 'df_dissimilarity_baseVar_full.RData'))
# df_stats <- df_var
# names(df_stats)[3] <- 'mu_var'
# save(df_stats, file=paste0(output_path, 'df_', 'dissimilarity' , '_muTACcov.RData' )    )
# 
# load(paste0(input_dir, 'df_diversity_baseVar_full.RData'))
# df_stats <- df_var
# # names(df_stats)[3] <- 'mu_var'
# save(df_stats, file=paste0(output_path, 'df_', 'diversity' , '_muTACcov.RData' )    )