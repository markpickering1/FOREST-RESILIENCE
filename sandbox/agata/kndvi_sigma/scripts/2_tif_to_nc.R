# ########################################################
# Title         : 2_tif_to_nc.R
# Description   : Convert a tiff file into a nc file
# Aims          : Conversion of tiff files into more manageable nc files
# Inputs	      : Tiff file
# Outputs	      : Nc file
# Options	      : 
# Date          : 12/22
# Version       : 1.1
# Authors       : Agata Elia 
# Maintainer    : Agata Elia 
# Notes		      : 
# Example use   : 
# ########################################################

###### SET LIBRARIES #####
library(chron) 
library(dplyr) 
library(raster) 
library(ncdf4)
library(gtools)

# Identify working directories
wd <- '/eos/jeodpp/data/projects/FOREST-RESILIENCE/GIT-FOREST-RESILIENCE/FOREST-RESILIENCE/sandbox/agata/kndvi_sigma/sigma_mean/'
od <- '/eos/jeodpp/data/projects/FOREST-RESILIENCE/GIT-FOREST-RESILIENCE/FOREST-RESILIENCE/sandbox/agata/kndvi_sigma/sigma_mean_nc/'

# Identify tif file
tif_file <- paste0(wd, "modisKNDVISimplifiedBandMean.tif")
print(tif_file)

# Define filename of the nc file
ncfname <- paste(od, "modisKNDVISimplifiedBandMean.nc", sep="")
print(ncfname)

# Define variable name
variable <- "kndvi"

# Create empty time vector
time_list <- vector(mode = "list")

# Read the file as a R raster object
r <-raster(tif_file)

# Read number of rows and columns of raster object and verify correctness
nlat <- r@nrows; nlon <- r@ncols
#print(nlat); print(nlon)
if(nlat != 7325 || nlon != 11098){ print(paste("NOTE: the file has ", nlon,' x ', nlat ," variables"))  }

# Read resolution of raster object and verify correctness
res <- xres(r)
if(res != 0.005) { print(paste("NOTE: the file has ", res, "resolution"))  }

# Extract the date from the raster filename to get the number of days/hours since 1900
file_year <- "2010"
file_month <- "1"
file_day <- "1"

# Convert the date into number of days since 1900
chron_date <- dates(paste0(file_month,'/',file_day,'/',file_year))
chron_time <- times("00:23:30")

# Adjust for leap years
is_leap <- 0
if((file_year == 2004 || file_year == 2008 || file_year == 2012 || file_year == 2016 || file_year == 2020) && chron_date>dates(paste0('02/21/', file_year))) {is_leap <- 1}
chron_date_time <- chron(dates= chron_date, times = chron_time) + is_leap
print(chron_date_time)
chron_date_1900 <- paste0("01/01/1900")
chron_time_1900 <- times("00:00:00")
chron_1900 <- chron(dates= chron_date_1900, times = chron_time_1900)
date_time_hours_since_1900 <- (chron_date_time - chron_1900) * 24

# Append date to the created list
time_list <- append(time_list, date_time_hours_since_1900)

# Stack together raster list into a brick/stack of rasters
raster_stack <- stack(r)
print(raster_stack)

# Define resolution of nc file
array_res <- 0.005 # hardcoded

# Create lon/lat array of nc file
lon <- as.array(seq(-10.665 , 44.825 - array_res, array_res)) ;
lat <- as.array(seq( 34.56  , 71.185 - array_res, array_res)) ;

# Define nc file dimensions (lon, lat, time)
londim <- ncdim_def("lon", "degrees_east", as.double(lon))
latdim <- ncdim_def("lat", "degrees_north", as.double(lat))
timedim <- ncdim_def("time", "hours since 1900-01-01 00:00:00.0", as.double(time_list)) # removed ' -0:00' from end of def

# Define nodata of nc file
fillvalue <- -32767

# Define nc file variable
def <- ncvar_def(variable, "none", list(londim, latdim, timedim), fillvalue, variable, prec="single")

# Create nc file
ncout <- nc_create(ncfname, def, force_v4=TRUE)

# Put variables in nc file
for (i in 1:nlayers(raster_stack)) {
  raster_stack_i <- flip(raster_stack[[i]], direction = 'y') # Flip the original raster and set the crs
  #crs(raster_stack_i) <- r_crs
  ncvar_put(nc = ncout, 
            varid = def, 
            vals = values(raster_stack_i),
            start = c(1, 1, i), 
            count = c(-1, -1, 1))
}

# Put additional attributes into dimension and data variables
ncatt_put(ncout,"lon","axis","X")
ncatt_put(ncout,"lat","axis","Y")
ncatt_put(ncout,"time","axis","T")

nc_close(ncout)

