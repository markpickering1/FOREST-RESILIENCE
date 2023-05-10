# ########################################################
# Title         : createDF_kndviclim.R
# Description   : create a df of X | Y | mu | TAC | CoV 
#                 for each variable at each point in the ncdf files
#                 create a df of mean, coeff of var, 1-lag TAC for each variable
# Aims          : dataframes of all data and RF predictor data for each X/Y
# Inputs	      : dfs containing KNDVI and CLIMATE data extracted from raster
# Outputs	      : separate and combined dfs of variables
# Options	      : 
# Date          : 11/11/22
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
script_info      <- 'createDF_kndviclim_muTACcov'               # used in output name
script_info_input <- 'createDF_kndviclim_fullTS'                 # load input dataframes containing full data and residuals

######     SET LIBRARIES                      #####
library(chron)   # useful for converting time values
library(dplyr)   # use %>%
# library(ncdf4)   # read ncdf
library(reshape) # reshaping dataframes
# library(raster)  # package for raster manipulation
# library(terra)   # terra library for raster manip
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

# initialise input file
# input_dir <- 'data_processing/createDF_kndviclim/createDF_kndviclim_2023-01-11/'
input_dir <- paste0(root_data_proce, script_info_input, '/', script_info_input, '_', full_date,  '/')

# initialise output
output_path <- paste0(root_data_proce, script_info, '/', script_info, '_', full_date,  '/')
print(paste0('output_path is : ', output_path ))
if(! dir.exists(output_path)) {dir.create(paste0(output_path),recursive=T)} # create directory if not present

###################################################
######     SET FUNCTIONS                      #####
###################################################

cacl_tac <- function(df_i, df_dates_t1, var_name ){
  # function takes a df, df_i, with x,y pixels, and values of column var_name for each date
  # creates a copy of the index of these dates shifted by 1 index
  # calculates the autocorrelation of the var_name value shifted by 1 index
  
  # take index of dates: clone and shift by one index
  names(df_dates_t1) <- c('date', 't_i')
  df_dates_t2 <- data.frame(v_dates, 2:(length(v_dates) + 1 ) )
  names(df_dates_t2) <- c('date', 't_i') 
  # head(df_dates_t1) ; tail(df_dates_t1) ; head(df_dates_t2) ; tail(df_dates_t2)
  
  # create two dfs of var_name with shifted index and values
  df_t1 <-  left_join(df_i, df_dates_t1)
  df_t2 <-  left_join(df_i, df_dates_t2)
  names(df_t1)[4] <- paste0(var_name, '_t1') ; names(df_t2)[4] <- paste0(var_name, '_t2') 
  df_t1$date <- NULL ; df_t2$date <- NULL
  
  # join the shifted
  df_o <- full_join(df_t1, df_t2)
  df_o <- na.omit(df_o)
  
  # calculate the 1-step lag
  df_o <- df_o %>% dplyr::group_by( x, y ) %>% 
    dplyr::summarise( tac_resid = cor( !!as.symbol(paste0(var_name, '_t1')) , !!as.symbol(paste0(var_name, '_t2') )) )
  
  return(df_o) 
}

###################################################
######     EXTRACT DATE INDEX                 #####
###################################################
# take all the dates and label indices for t1 item and t2 (1-lag) item for calculating autocorrel
load( paste0(input_dir, 'df_', 'kndvi_deseason' ,'_full.RData'  ) ) # head(df_var) ; # summary(df_var)
v_dates <-  unique(df_var$date) 
if(length(v_dates) != 874) {'Warning: check dates maybe something wrong'}
df_dates_index_t1 <- data.frame(v_dates, 1:length(v_dates) )

###################################################
######       extract statistics               #####
###################################################
# extract background mean, CoV and residual TAC for the different variabes.

v_variables <- c('kndvi', 't2m', 'VPD', 'spei', 'ssr', 'swvl1', 'tp')
rdata_files <- list.files(path=input_dir, pattern=paste0("*"), full.names=FALSE, recursive=FALSE) # view all files


for (i in 1:length(v_variables)){

  var_i <- v_variables[i] ; print(var_i)
    
  # open df
  load( paste0(input_dir, 'df_', var_i, '_baseVar' ,'_full.RData'  ) )
  # head(df_i) 
  
  # group by and calculate count & mean
  df_stats <- df_var %>% dplyr::group_by( x, y ) %>% 
    dplyr::summarise(
      n_var = n() ,
      mu_var  = mean( !!as.symbol(var_i), na.rm = T),
      sd_var  = sd( !!as.symbol(var_i), na.rm = T),
      cv_var  = raster::cv( !!as.symbol(var_i), na.rm = T),
      cv_calc_var = sd_var/mu_var *100    # calculate by hand - as seems a different result
      ) 
  # dim(df_stats) ; summary(df_stats)
  
  load( paste0(input_dir, 'df_', var_i, '_deseason' ,'_full.RData'  ) )
  
  df_stats_tac <- cacl_tac(df_var, df_dates_index_t1, var_i)
  
  df_stats <- full_join(df_stats, df_stats_tac)
  # head(df_stats) ; dim(df_stats) ; summary(df_stats)
  
  # save individual dataframes and join to single dataframe
  save(df_stats, file=paste0(output_path, 'df_', var_i , '_muTACcov.RData' )    )

} # end var loop


# copy across the forest data - as format is already useable
# file.copy(from=paste0(input_dir, 'df_treecover_baseVar_full.RData'), to=paste0(output_path, 'df_treecover_full.RData'), 
#           overwrite = F, recursive = FALSE, copy.mode = TRUE)
load(paste0(input_dir, 'df_treecover_baseVar_full.RData'))
df_stats <- df_var
names(df_stats)[3] <- 'mu_var'
save(df_stats, file=paste0(input_dir, 'df_', 'treecover' , '_muTACcov.RData' )    )


# # calculate the 1-lag tac
# # first replace the dates with the index
# df_t1 <-  left_join(df_var, df_dates_index_t1)
# df_t2 <-  left_join(df_var, df_dates_index_t2)
# names(df_t1)[4] <- paste(var_i, '_t1') ; names(df_t2)[4] <- paste(var_i, '_t2') 
# df_t1$date <- NULL ; df_t2$date <- NULL
# # head(df_t1) ; head(df_t2)
# df_var <- full_join(df_t1, df_t2)
# df_var <- na.omit(df_var)
# # head(df_i_deseason) ; summary(df_i_deseason) ; hist(df_i_deseason$t_i)
# 
# # calculate the 1-step lag
# df_stats_tac <- df_var %>% dplyr::group_by( x, y ) %>% 
#   dplyr::summarise( tac_resid = cor( !!as.symbol(paste(var_i, '_t1')) , !!as.symbol(paste(var_i, '_t2') )) )
# df_stats <- full_join(df_stats, df_stats_tac)
# # head(df_stats) ; dim(df_stats) ; summary(df_stats)

# save(df_kndviclim_muTACcov, file=paste0(output_path, 'df_all_muTACcov.RData' )    )
# head(df_kndviclim_muTACcov) ; dim(df_kndviclim_muTACcov) ; summary(df_kndviclim_muTACcov)
