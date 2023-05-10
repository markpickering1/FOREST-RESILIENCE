# ########################################################
# Title         : 5_createDf_kndviclim_phenology.R
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
#input_phenology_script_date <- '2023-03-29'
input_phenology_script_date <- '2023-04-12'            # new version with circular mean of greenup and dormancy and mean of length

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
#v_variables <- c('t2m', 'ssr') #select only variables that have an actual time-series

for (i in 1:length(v_variables)){
  
  var_i <- v_variables[i] ; print(var_i)
  
  if(! file.exists(paste0(output_path, 'df_', var_i, '_baseVar' ,'_full_gs_masked.RData'))){
    
    # open df of base variables data
    load( paste0(input_dir, 'df_', var_i, '_baseVar' ,'_full.RData'  ) )
    
    # join greenup and dormancy DOY to the df
    df_var <- inner_join(df_var, phenology_df)
  
    # save joined df
    #save(df_var, file=paste0(output_path, 'df_', var_i, '_baseVar' ,'_full_gs.RData'  ) )
    
    # create new columns with date in doy and checking if doy_greenup>doy_dormancy and if date is between greenup and dormancy
    df_var <- df_var %>% mutate(doy_date = as.numeric(format((as.Date(date, format = "%Y-%m-%d")), format = "%j"))) %>% 
      mutate(greenup_gt_dormancy = ifelse(doy_greenup<doy_dormancy,1,0)) %>% 
      mutate(date_within_gs = ifelse(((greenup_gt_dormancy==1 & (doy_date>=doy_greenup & doy_date<=doy_dormancy))|(greenup_gt_dormancy==0 & (doy_date>=doy_greenup | doy_date<=doy_dormancy))),1,0)) %>%
      mutate(var_gs = ifelse(date_within_gs == 1, !!as.symbol(var_i), NA)) # instead of masking make a new column
    
    # mask where date is between gs
    #df_var <- df_var[df_var$date_within_gs == 1,1:8]
    df_var <- df_var[,c(1:8, 11)]
    
    # save masked df
    save(df_var, file=paste0(output_path, 'df_', var_i, '_baseVar' ,'_full_gs_masked.RData'  ) )
  }
  
  if(! file.exists(paste0(output_path, 'df_', var_i, '_deseason' ,'_full_gs_masked.RData'))){
    
    # open df of deseasonalised data
    load( paste0(input_dir, 'df_', var_i, '_deseason' ,'_full.RData'  ) )
    
    # join greenup and dormancy DOY to the df
    df_var <- inner_join(df_var, phenology_df)
    
    # save joined df
    #save(df_var, file=paste0(output_path, 'df_', var_i, '_deseason' ,'_full_gs.RData'  ) )
    
    # create new columns with date in doy and checking if doy_greenup>doy_dormancyand if date is between greenup and dormancy
    df_var <- df_var %>% mutate(doy_date = as.numeric(format((as.Date(date, format = "%Y-%m-%d")), format = "%j"))) %>% 
      mutate(greenup_gt_dormancy = ifelse(doy_greenup<doy_dormancy,1,0)) %>% 
      mutate(date_within_gs = ifelse(((greenup_gt_dormancy==1 & (doy_date>=doy_greenup & doy_date<=doy_dormancy))|(greenup_gt_dormancy==0 & (doy_date>=doy_greenup | doy_date<=doy_dormancy))),1,0)) %>%
      mutate(var_gs = ifelse(date_within_gs == 1, !!as.symbol(var_i), NA)) # instead of masking make a new column
    
    # mask where date is between gs
    #df_var <- df_var[df_var$date_within_gs == 1,1:8]
    df_var <- df_var[,c(1:8, 11)]
    
    # save masked df
    save(df_var, file=paste0(output_path, 'df_', var_i, '_deseason' ,'_full_gs_masked.RData'  ) )
  }
} # end var loop

# copy across the forest, socc and heterogeneity (diversity and dissimilarity) data - as they have no time-series
load(paste0(input_dir, 'df_forestcover_baseVar_full.RData'))
save(df_var, file=paste0(output_path, 'df_forestcover_baseVar_full_gs_masked.RData' )    )

load(paste0(input_dir, 'df_socc30cm_baseVar_full.RData'))
save(df_var, file=paste0(output_path, 'df_socc30cm_baseVar_full_gs_masked.RData' )    )

load(paste0(input_dir, 'df_dissimilarity_baseVar_full.RData'))
save(df_var, file=paste0(output_path, 'df_dissimilarity_baseVar_full_gs_masked.RData' )    )

load(paste0(input_dir, 'df_diversity_baseVar_full.RData'))
save(df_var, file=paste0(output_path, 'df_diversity_baseVar_full_gs_masked.RData' )    )