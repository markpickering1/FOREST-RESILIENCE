# ########################################################
# Title         : 2_tif_to_nc.R
# Description   : Convert a tiff file into a nc file
# Aims          : Conversion of tiff files into more manageable nc files
# Inputs	      : Tiff file
# Outputs	      : Nc file
# Options	      : 
# Date          : 1804/23
# Version       : 1.1
# Authors       : Mark Pickering 
# Maintainer    : Mark Pickering
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
wd <- '/eos/jeodpp/data/projects/FOREST-RESILIENCE/data/biodiversity/biomass/nasa_biomass/dissimilarity_homogeneity/'
od <- wd


# Identify tif file
tif_file_dissim <- paste0(wd, "biomass_dissimilarity_MW3.tif")
tif_file_homog <- paste0(wd, "biomass_homogeneity_MW3.tif")
print(tif_file_dissim)

# Define filename of the nc file
ncfname_biomass <- paste(od, "biomass_diversity_MW3.nc", sep="")
print(ncfname_biomass)

# Define variable name
variable_dissim <- "dissimilarity"
variable_homog <- "homogeneity"

# Create empty time vector
time_list <- vector(mode = "list")

# Read the file as a R raster object
 load(tif_file_dissim)
 load(tif_file_homog)
# r_homog  <-raster(tif_file_homog)
 r_glcm_dissim_count ; r_glcm_homog_count

# Read number of rows and columns of raster object and verify correctness
nlat_dissim <- r_glcm_dissim_count@nrows ; nlon_dissim <- r_glcm_dissim_count@ncols
nlat_homog  <- r_glcm_homog_count@nrows  ; nlon_homog  <- r_glcm_homog_count@ncols

#print(nlat); print(nlon)
if(nlat != 879 || nlon != 1332){ print(paste("NOTE: the file has ", nlon,' x ', nlat ," variables"))  }

# Read resolution of raster object and verify correctness
array_res <- xres(r_glcm_dissim_count)
# if(res != 0.0416666666666667) { print(paste("NOTE: the file has ", res, "resolution"))  }

# Extract the date from the raster filename to get the number of days/hours since 1900
file_year <- "2010"
file_month <- "1"
file_day <- "1"

# Convert the date into number of days since 1900
chron_date <- dates(paste0(file_month,'/',file_day,'/',file_year))
chron_time <- times("00:23:30")

# Adjust for leap years
chron_date_time <- chron(dates= chron_date, times = chron_time) 
print(chron_date_time)
chron_date_1900 <- paste0("01/01/1900")
chron_time_1900 <- times("00:00:00")
chron_1900 <- chron(dates= chron_date_1900, times = chron_time_1900)
date_time_hours_since_1900 <- (chron_date_time - chron_1900) * 24

# Append date to the created list
time_list <- append(time_list, date_time_hours_since_1900)

# Stack together raster list into a brick/stack of rasters
raster_stack <- stack(r_glcm_dissim_count, r_glcm_homog_count)
print(raster_stack)

# Define resolution of nc file
# array_res <- 0.005 # hardcoded

# Create lon/lat array of nc file
# lon <- as.array(seq(-10.6666666669999994 , 44.8333333329999988 - array_res, array_res)) ;
# lat <- as.array(seq( 34.5833333329999988  , 71.2083333329999988 - array_res, array_res)) ;
lon <- as.array(seq(extent(r_glcm_homog_count)[1] , extent(r_glcm_homog_count)[2] - array_res, array_res)) ;
lat <- as.array(seq( extent(r_glcm_homog_count)[3]  , extent(r_glcm_homog_count)[4] - array_res, array_res)) ;

# Define nc file dimensions (lon, lat, time)
londim <- ncdim_def("lon", "degrees_east", as.double(lon))
latdim <- ncdim_def("lat", "degrees_north", as.double(lat))
timedim <- ncdim_def("time", "hours since 1900-01-01 00:00:00.0", as.double(time_list)) # removed ' -0:00' from end of def

# Define nodata of nc file
fillvalue <- -32767

# Define nc file variable
def_1 <- ncvar_def(variable_dissim, "none", list(londim, latdim, timedim), fillvalue, variable_dissim, prec="single")
def_2 <- ncvar_def(variable_homog, "none", list(londim, latdim, timedim), fillvalue, variable_homog, prec="single")

# Create nc file
ncout <- nc_create(ncfname_biomass, list(def_1, def_2), force_v4=TRUE)

# Put variables in nc file
# for (i in 1:nlayers(raster_stack)) {
  raster_stack_i <- flip(raster_stack[[1]], direction = 'y') # Flip the original raster and set the crs
  #crs(raster_stack_i) <- r_crs
  ncvar_put(nc = ncout, 
            varid = def_1, 
            vals = values(raster_stack_i),
            start = c(1, 1, i), 
            count = c(-1, -1, 1))
  
  raster_stack_i <- flip(raster_stack[[2]], direction = 'y') # Flip the original raster and set the crs
  ncvar_put(nc = ncout, 
            varid = def_2, 
            vals = values(raster_stack_i),
            start = c(1, 1, i), 
            count = c(-1, -1, 1))
# }

# Put additional attributes into dimension and data variables
ncatt_put(ncout,"lon","axis","X")
ncatt_put(ncout,"lat","axis","Y")
ncatt_put(ncout,"time","axis","T")

nc_close(ncout)

