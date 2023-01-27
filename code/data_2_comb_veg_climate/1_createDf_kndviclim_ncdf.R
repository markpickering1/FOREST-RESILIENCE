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
# Version       : 1.2
# Authors       : Mark Pickering & Agata Elia
# Maintainer    : Mark Pickering 
# Notes		      : 
# Example use   : 
# ########################################################

###################################################
######     INITIALISE                         #####
###################################################
rm(list = ls())                                   # remove loaded objects

source('/eos/jeodpp/data/projects/FOREST-RESILIENCE/GIT-FOREST-RESILIENCE/FOREST-RESILIENCE/initialise_R.R')

# set date time and extract
start_time <- Sys.time() ; print(start_time)      # initialise time
full_date <- as.character(Sys.time()) ; full_date <- strsplit(full_date, " ")
full_time <- full_date[[1]][2] ; full_date <- full_date[[1]][1]

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
input_kndvi   <- 'data/vegetation/8day/KNDVI/KNDVI_nc_deseasonalised_005/merged_kndvi_2003_2021_deseasonalised_005_con.nc' # '
input_kndvi_nodeseason <- 'data/vegetation/8day/KNDVI/KNDVI_nc_merged_005/merged_kndvi_2003_2021_005_con.nc'

input_t2m   <- 'data/climate/8day/T2M/T2M_3_europe/resolution/'   # t2m_allY_timsel8_europe3nn_deseason.nc
input_VPD   <- 'data/climate/8day/VPD/VPD_3_europe/resolution/'   # VPD_allY_timsel8_europe3nn_deseason.nc
input_spei  <- 'data/climate/8day/SPEI/SPEI_3_europe/resolution/' # SPEI_allY_timsel8_europe3nn_deseason.nc
input_ssr   <- 'data/climate/8day/SSR/SSR_3_europe/resolution/'   # SSR_allY_timsel8_europe3nn_deseason.nc
input_swvl1 <- 'data/climate/8day/SM/SM_3_europe/resolution/'     # SM_allY_timsel8_europe3nn_deseason.nc
input_tp    <- 'data/climate/8day/tp/tp_3_europe/resolution/'

input_hansen <- 'data/ancillary/hansen/hansen_nc_005/'            # hansenAverageTreecover2000MaskAtModisMean0005Scale4326_005_con.nc

# set/create output directory
output_path <- paste0(root_data_proce, script_info, '/', script_info, '_', full_date,  '/')
print(paste0('output_path is : ', output_path ))
if(! dir.exists(output_path)) {dir.create(paste0(output_path),recursive=T)} # create directory if not present

###################################################
######     SET FUNCTIONS                      #####
###################################################

# date_to_col <- function(df, inc_year=TRUE, inc_month=TRUE){
#   # function takes a dataframe with Z column a date and extracts the month and year component, inserting them as columns
#   if(inc_year)df <- df %>% mutate(year = lubridate::year(Z))
#   if(inc_month){ df <- df %>% mutate( month = lubridate::month(Z) ) }
#   df$Z <- NULL 
#   return(df)
# }

###################################################
######       extract ncdfs - rast             #####
###################################################

v_variables <- c('kndvi', 't2m', 'VPD', 'spei', 'ssr', 'swvl1', 'tp' ) #; v_variables <- c('kndvi')
v_files     <- c(NA,  input_t2m, input_VPD, input_spei, input_ssr, input_swvl1, input_tp)

# currently just run on the climate variables as we want 
for (i in 1:length(v_variables)){
  var_i <- v_variables[i] ; print(var_i)
  if(var_i =='kndvi'){
    file_i_deseason   <- input_kndvi
    file_i_baseVar    <- input_kndvi_nodeseason
  } 
  else {
    file_i_deseason   <- paste0( v_files[i] , var_i, '_allY_timsel8_europe3nn_deseason.nc' )
    file_i_baseVar    <- paste0( v_files[i] , var_i, '_allY_timsel8_europe3nn_baseVar.nc'  )
  }
  
  # first run on the non-deseasonalised data to get mean and cv
  # open raster and extract dates of entries for the deseasonalised data
  r_i_baseVar  <- rast(file_i_baseVar) #, varname = 'kndvi') ; plot(r_i_baseVar[[1]])
  times_i_baseVar <- list(time(r_i_baseVar)) # ; typeof(input_time[[1]]) # [[c(1,2)]]
  dates_i_baseVar <- sapply(  strsplit( as.character(times_i_baseVar[[1]]), ' ' ) , '[', 1 )
  
  # convert r to dataframe, reset titles, convert to long format
  df_i_baseVar <- terra::as.data.frame(r_i_baseVar, xy = T, na.rm = NA) # set to NA for cells with NA in all layers removed
  # head(df_i_baseVar[,1:6]) ; dim(df_i_baseVar)  # summary(df_i_baseVar) # hist(df_i_baseVar[[3]])
  names(df_i_baseVar)[3:length(names(df_i_baseVar))] <- dates_i_baseVar
  df_i_baseVar <- melt(df_i_baseVar, id=c("x","y"), variable_name = "date") # head(df_i_baseVar) , value.name = 'kndvi') # value_name = "kndvi" ) 
  names(df_i_baseVar)[4] <- var_i
  
  df_var <- df_i_baseVar
  
  # save individual dataframes and join to single dataframe
  save(df_var, file=paste0(output_path, 'df_',var_i, '_baseVar_full.RData' )    )

  # open raster and extract dates of entries for the deseasonalised data
  r_i_deseason  <- rast(file_i_deseason) #, varname = 'kndvi') ; plot(r_i_deseason[[1]])
  times_i_deseason <- list(time(r_i_deseason)) # ; typeof(input_time[[1]]) # [[c(1,2)]]
  dates_i_deseason <- sapply(  strsplit( as.character(times_i_deseason[[1]]), ' ' ) , '[', 1 )
  
  # convert r to dataframe, reset titles, convert to long format
  df_i_deseason <- terra::as.data.frame(r_i_deseason, xy = T, na.rm = NA)
  # head(df_i_deseason[,1:6]) ; dim(df_i_deseason)  # summary(df_i_deseason) # hist(df_i_deseason[[3]])
  names(df_i_deseason)[3:length(names(df_i_deseason))] <- dates_i_deseason
  df_i_deseason <- melt(df_i_deseason, id=c("x","y"), variable_name = "date") # head(df_i_deseason) , value.name = 'kndvi') # value_name = "kndvi" ) 
  names(df_i_deseason)[4] <- var_i
  
  df_var <- df_i_deseason
  
  # save individual dataframes and join to single dataframe
  save(df_var, file=paste0(output_path, 'df_',var_i, '_deseason_full.RData' )    )

}

# finally extract hansen forest cover data - TBC

v_variable_i <- 'treecover'
file_i_baseVar <- paste0(input_hansen, 'hansenAverageTreecover2000MaskAtModisMean0005Scale4326_005_con.nc')            # 
r_i_baseVar    <- rast(file_i_baseVar) #, varname = 'kndvi') ; 
# plot(r_i_baseVar[[1]]) ; summary(r_i_baseVar)
df_var <- terra::as.data.frame(r_i_baseVar, xy = T, na.rm = NA) # set to NA for cells with NA in all layers removed
names(df_var)[3] <- v_variable_i
save(df_var, file=paste0(output_path, 'df_',v_variable_i, '_baseVar_full.RData' )    )


# head(df_kndviclim)
# save(df_kndviclim, file=paste0(output_path, 'df_all_fullKndvi_ljoinClim.RData' )    )


# ###################################################
# ######       TEST PLOT SINGLE SLICE           #####
# ###################################################
# df_i_baseVar_1 <- df_i_baseVar[1:3]
# 
# r_i_baseVar_1 <- terra::rast(  as.matrix(df_i_baseVar_1) , type="xyz")
# plot(r_i_baseVar_1)
# # lost a lot of data - why?
# 
# r2_i_baseVar  <- raster(file_i_baseVar) #, varname = 'kndvi') ; plot(r_i_baseVar[[1]])
# plot(r2_i_baseVar)
# # convert r to dataframe, reset titles, convert to long format
# df2_i_baseVar <- as.data.frame(r2_i_baseVar, xy = T, na.rm = T)
# head(df2_i_baseVar)
