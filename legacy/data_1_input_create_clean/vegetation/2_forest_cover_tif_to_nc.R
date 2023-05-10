# ########################################################
# Title         : 0_tif_to_nc.R
# Description   : Convert a given tif file to nc file
#
# Aims          : Convert the Hansen forest cover into nc file
# Inputs	      : Hansen forest cover tiff file
# Outputs	      : Hansen forest cover nc file
# Options	      : 
# Date          : 09/02/2023
# Version       : 2
# Authors       : Agata Elia & Mark Pickering
# Maintainer    : Agata Elia 
# Notes		      : 
# ########################################################


###################################################
######     INITIALISE                         #####
###################################################
rm(list = ls())                                   # remove loaded objects

source('/eos/jeodpp/data/projects/FOREST-RESILIENCE/GIT-FOREST-RESILIENCE/FOREST-RESILIENCE/code/main/initialise_R.R')

######     GLOBAL VARS                        #####
main_dir <- 'ancillary/hansen/'
script_info <- '0_tif_to_nc'              
script_info_input <- 'hansen_forest_cover'
script_info_output <- 'hansen_forest_cover_nc'   
input_script_date <- '2023-02-09' 
file_name <- 'hansenForestCoverNoLoss2000AtModisMean'
variable <- 'forestcover'

######     SET LIBRARIES                      #####
library(chron) 
library(dplyr) 
library(raster) 
library(ncdf4)
library(gtools)

###################################################
######       I/O                              #####
###################################################

# initialise input file
input_dir <- paste0(root_data_input, main_dir, script_info_input, '/')
print(paste0('input_path is : ', input_dir ))

# identify input tif file
tif_file <- paste0(input_dir, file_name, '.tif')
print(paste0('input_file is : ', tif_file ))

# set/create output directory  - use same as before
output_path <- paste0(root_data_input, main_dir, script_info_output, '/')
print(paste0('output_path is : ', output_path ))
if(! dir.exists(output_path)) {dir.create(paste0(output_path),recursive=T)} # create directory if not present

# set output nc file
ncfname <- paste0(output_path, file_name, '.nc')
print(paste0('output_file is : ', ncfname ))

###################################################
######     CREATE NC FILES                    #####
###################################################

# create empty vector
time_list <- vector(mode = "list")

# read the file as a R raster object and print raster metadata
r <-raster(tif_file)
#print(r)

# read number of rows and columns of raster object and verify correctness
nlat <- r@nrows; nlon <- r@ncols
#print(nlat); print(nlon)
if(nlat != 7325 || nlon != 11098){ print(paste("NOTE: the file has ", nlon,' x ', nlat ," variables"))  }

# read resolution of raster object and verify correctness
res <- xres(r)
if(res != 0.005) { print(paste("NOTE: the file has ", res, "resolution"))  }

# print raster object specifications
#print(paste0('resolution: ', array_res, ' grid: ', nlon, ' x ', nlat))

# extract the date from the raster filename to get the number of days/hours since 1900
file_year <- "2000"
file_month <- "1"
file_day <- "1"

# convert the date into number of days since 1900
chron_date <- dates(paste0(file_month,'/',file_day,'/',file_year))
chron_time <- times("00:23:30")
# adjust for leap years
is_leap <- 0
if((file_year == 2004 || file_year == 2008 || file_year == 2012 || file_year == 2016 || file_year == 2020) && chron_date>dates(paste0('02/21/', file_year))) {is_leap <- 1}
chron_date_time <- chron(dates= chron_date, times = chron_time) + is_leap
print(chron_date_time)
chron_date_1900 <- paste0("01/01/1900")
chron_time_1900 <- times("00:00:00")
chron_1900 <- chron(dates= chron_date_1900, times = chron_time_1900)
date_time_hours_since_1900 <- (chron_date_time - chron_1900) * 24

# append date to the created list
time_list <- append(time_list, date_time_hours_since_1900)

# stack together raster list into a brick/stack of rasters
raster_stack <- stack(r)
print(raster_stack)

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
treecover_def <- ncvar_def(variable, "none", list(londim, latdim, timedim), fillvalue, variable, prec="single")

# create nc file
ncout <- nc_create(ncfname, treecover_def, force_v4=TRUE)

# put variables in nc file
for (i in 1:nlayers(raster_stack)) {
  raster_stack_i <- flip(raster_stack[[i]], direction = 'y') # Flip the original raster and set the crs
  #crs(raster_stack_i) <- r_crs
  ncvar_put(nc = ncout, 
            varid = treecover_def, 
            vals = values(raster_stack_i),
            start = c(1, 1, i), 
            count = c(-1, -1, 1))
}

# put additional attributes into dimension and data variables
ncatt_put(ncout,"lon","axis","X")
ncatt_put(ncout,"lat","axis","Y")
ncatt_put(ncout,"time","axis","T")

nc_close(ncout)

