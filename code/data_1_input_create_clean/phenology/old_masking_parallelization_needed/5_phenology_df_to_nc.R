# ########################################################
# Title         : 4_phenology_df_to_nc.R
# Description   : generate a 46 columns dataframe/tif with binary masks for growing season for each 8 days timestep (0/1)
# Aims          : growing season mask over 8 days timesteps
# Inputs	      : dataframes of median greenup and dormancy doy
# Outputs	      : 46 fields columns dataframe/tif with masks for growing season
# Options	      : 
# Date          : 27/03/23
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
script_info <- 'phenology_df_to_nc'            # used in output name
script_info_input <- 'phenology_mask'
input_script_date <- '2023-03-27' 

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

input_kndvi <- 'data/vegetation/8day/KNDVI_no_mask/KNDVI_no_mask_nc_merged_rescaled10_deseasonalised_masked_005/merged_kndvi_no_mask_2003_2021_rescaled10_deseasonalised_masked50_005.nc'

# initialise input directory
input_dir <- paste0(root_data_proce, script_info_input, '/', script_info_input, '_', input_script_date,  '/')

# set/create output directory
output_path <- paste0(root_data_proce, script_info, '/', script_info, '_', full_date,  '/')
print(paste0('output_path is : ', output_path ))
if(! dir.exists(output_path)) {dir.create(paste0(output_path),recursive=T)} # create directory if not present

###################################################
######       GLOBAL VARS                      #####
###################################################

# load dataframes of greenup and dormancy
load(paste0(input_dir,'vperc.RData'))

###################################################
######     CREATE NC FILE                     #####
###################################################

# read timesteps from existing nc file
r_i_baseVar  <- rast(input_kndvi)
time_list <- list(time(r_i_baseVar))
#dates_i_baseVar <- sapply(  strsplit( as.character(times_i_baseVar[[1]]), ' ' ) , '[', 1 )

# inspect created lists   
print(time_list)

# define filename of the nc file
ncfname <- paste0(output_path, 'phenology')
print(ncfname)

# define resolution of nc file
array_res <- 0.005 # hardcoded

# create lon/lat array of nc file
lon <- as.array(seq(-10.665 , 44.825 - array_res, array_res)) ;
lat <- as.array(seq( 34.56  , 71.185 - array_res, array_res)) ;

# define nc file dimensions (lon, lat, time)
londim <- ncdim_def("lon", "degrees_east", as.double(lon))
latdim <- ncdim_def("lat", "degrees_north", as.double(lat))
timedim <- ncdim_def("time", "hours since 1900-01-01 00:00:00.0", as.double(time_list)) # removed ' -0:00' from end of def

# define nodata of nc file
fillvalue <- -32767

# define nc file variable
phenology_def <- ncvar_def("phenology_mask", "none", list(londim, latdim, timedim), fillvalue, "phenology_mask", prec="single")

# create nc file
ncout <- nc_create(ncfname, phenology_def, force_v4=TRUE)

# put variables in nc file VERIFY CORRECT ORDER OF COORDINATES
for (i in 1:46) {
  #raster_stack_i <- flip(raster_stack[[i]], direction = 'y') # Flip the original raster and set the crs
  ncvar_put(nc = ncout, 
            varid = phenology_def, 
            vals = values(raster_stack_i),
            start = c(1, 1, i), 
            count = c(-1, -1, 1))
}

# put additional attributes into dimension and data variables
ncatt_put(ncout,"lon","axis","X")
ncatt_put(ncout,"lat","axis","Y")
ncatt_put(ncout,"time","axis","T")

# close the nc files writing data to disk
nc_close(ncout)