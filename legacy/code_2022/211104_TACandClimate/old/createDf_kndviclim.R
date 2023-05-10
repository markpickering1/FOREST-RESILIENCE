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
# Version       : 1.1
# Authors       : Mark Pickering & Agata Elia
# Maintainer    : Mark Pickering 
# Notes		      : 
# Example use   : 
# ########################################################

###################################################
######     INITIALISE                         #####
###################################################
rm(list = ls())                                   # remove loaded objects
start_time <- Sys.time() ; print(start_time)      # initialise time
# find date (e.g. label output dir):
full_date <- as.character(Sys.time()) ; full_date <- strsplit(full_date, " ")
full_time <- full_date[[1]][2] ; full_date <- full_date[[1]][1]

######     GLOBAL VARS                        #####
script_info <- 'createDF_kndviclim'            # used in output name
output_dir_name <- 'createDF_kndviclim/'       # name of output directory
nc_varname <- 'kndvi'                             # name of ncdf variable

# agg_fact <- 1                                     # aggregation T/F and factor for visualisation purposes (speeds up visualisation but associated problems)
text_size = 10                                    # text size in figures - big maps
legend_size <- 1


######     SET LIBRARIES                      #####
library(chron)   # useful for converting time values
library(dplyr)   # use %>%
library(ncdf4)   # read ncdf
library(reshape) # reshaping dataframes
library(raster)  # package for raster manipulation
library(terra)   # terra library for raster manip
# library(rasterVis)  # enables levelplots
require(ggplot2)      # for plotting
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

# initialise directory
setwd('/eos/jeodpp/data/projects/FOREST-RESILIENCE/')                           # set working directory to link files (rerun if needed)

# initialise input file
input_kndvi <- 'data/vegetation/8day/KNDVI/KNDVI_nc_deseasonalised_005/merged_kndvi_2003_2021_deseasonalised_005_con.nc'
input_t2m   <- 'data/climate/8day/T2M/T2M_3_europe/resolution/t2m_allY_timsel8_europe3nn.nc'
input_VPD   <- 'data/climate/8day/VPD/VPD_3_europe/resolution/VPD_allY_timsel8_europe3nn.nc'
input_SPEI  <- 'data/climate/8day/SPEI/SPEI_3_europe/resolution/SPEI_allY_timsel8_europe3nn.nc'
input_SSR   <- 'data/climate/8day/SSR/SSR_3_europe/resolution/SSR_allY_timsel8_europe3nn.nc'
input_SM    <- 'data/climate/8day/SM/SM_3_europe/resolution/SM_allY_timsel8_europe3nn.nc'

# set/create output directory
output_root <- paste0('figures/', output_dir_name)
output_path <- paste0(output_root, script_info, '_', full_date,  '/')
print(paste0('output_path is : ', output_path ))
if(! dir.exists(output_path)) {dir.create(paste0(output_path),recursive=T)} # create directory if not present

###################################################
######       extract ncdfs                    #####
###################################################
# raster stack method didn't work - use netcdf






# convert ncdf to a raster stack
# r_kndvi  <- rast(input_kndvi) #, lyrs=874) #, varname = 'kndvi')
r_kndvi  <- stack(input_kndvi, varname = 'kndvi')
dim(r_kndvi)
r_kndvi <- r_kndvi[,,1:3]

plot(r_kndvi[,,1])
# plot(r_kndvi$X2003.01.04.00.23.30)
# convert raster stack to a dataframe with xy coord
df_kndvi  <- as.data.frame(r_kndvi, xy = T)
head(df_kndvi)

r_kndvi  <- rast(input_VPD, ) #, varname = 'kndvi')



# r_kndvi  <- rast(input_t2m) #, lyrs=874) #, varname = 'kndvi')
r_t2m  <- stack(input_t2m, varname = 't2m')
# plot(r_kndvi[,,1])
# plot(r_kndvi$X2003.01.04.00.23.30)
# convert raster stack to a dataframe with xy coord
# df_kndvi  <- as.data.frame(r_kndvi, xy = T)
head(df_kndvi)

# r_kndvi  <- rast(input_VPD, ) #, varname = 'kndvi')

r_t2m  <- stack(input_t2m, varname = 't2m')
df_variable <- as.data.frame(r_t2m, xy = T, na.rm = T, long = T)
head(df_variable) ; dim(df_variable)
