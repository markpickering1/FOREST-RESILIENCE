# ########################################################
# Title         : 3_tif_to_yearly_nc.R
# Description   : Generate yearly nc files from input 8-days tiff files
#
# Aims          : Conversion of tiff files into more manageable nc files
# Inputs	      : Tiff files of 8-days average kNDVI
# Outputs	      : Yearly nc files of 8-days average kNDVI
# Options	      : 
# Date          : 06/02/2023
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
start_year <- 2003 ; end_year <- 2021
main_dir <- 'vegetation/8day/KNDVI_no_mask/'
script_info <- '3_tif_to_yearly_nc'              
script_info_input <- 'KNDVI_no_mask'
script_info_output <- 'KNDVI_no_mask_nc'   
input_script_date <- '2023-02-06' 

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

# set/create output directory  - use same as before
output_path <- paste0(root_data_input, main_dir, script_info_output, '/')
print(paste0('output_path is : ', output_path ))
if(! dir.exists(output_path)) {dir.create(paste0(output_path),recursive=T)} # create directory if not present

###################################################
######     CREATE YEARLY NC FILES             #####
###################################################

# loop through years
for(year_i in start_year:end_year){
  
  # print current year
  print(year_i)
  
  # list all files with same year and get length of the list (use mixedsort for right order)
  tif_files <- list.files(path=input_dir, pattern=paste0("^", year_i, "_*"), full.names=TRUE, recursive=FALSE)
  tif_files <- mixedsort(tif_files)
  print(tif_files)
  len <- length(tif_files)
  print(len)
  
  # create empty list that is going to contain the time dimensions
  #time_list <- vector(mode = "list", length = len)
  time_list <- vector(mode = "list")
  
  # create empty list that is going to contain the rasters to be stack
  #raster_list <- vector(mode = "list", length = len)
  raster_list <- vector(mode = "list")
  
  # loop through files
  for (raster_i in tif_files) {
    
      # read each file as a R raster object and print raster metadata
      r <-raster(raster_i)
      #print(r)
      
      # append raster object to the created list
      raster_list <- append(raster_list, r)

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
      file_year <- strsplit(basename(raster_i), "_")[[1]][1]
      file_month <- strsplit(basename(raster_i), "_")[[1]][2]
      file_day <- strsplit(basename(raster_i), "_")[[1]][3]
      
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
      
      # print date in number of days since 1900
      #print(date_time_hours_since_1900)
      
      # append date to the created list
      time_list <- append(time_list, date_time_hours_since_1900)
  }
  
    # inspect created lists   
    
    # print time and raster lists and their length
    print(raster_list)
    print(length(raster_list))
    print(time_list)
    print(length(time_list))
  
    # create nc file for each year

    # define filename of the nc file
    ncfname <- paste(output_path, year_i, "_kndvi.nc", sep="")
    print(ncfname)
    
    if (file.exists(ncfname)){
      print("nc file already exists")
      }
    else {
      # stack together raster list into a brick/stack of rasters
      raster_stack <- stack(raster_list)
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
      kndvi_def <- ncvar_def("kndvi", "none", list(londim, latdim, timedim), fillvalue, "kndvi", prec="single")
      
      # create nc file
      ncout <- nc_create(ncfname, kndvi_def, force_v4=TRUE)
      
      # put variables in nc file
      for (i in 1:nlayers(raster_stack)) {
        raster_stack_i <- flip(raster_stack[[i]], direction = 'y') # Flip the original raster and set the crs
        raster_stack_i[raster_stack_i<0] <- -32767 # Set negative kndvi values to no data
        #crs(raster_stack_i) <- r_crs
        ncvar_put(nc = ncout, 
                  varid = kndvi_def, 
                  vals = values(raster_stack_i),
                  start = c(1, 1, i), 
                  count = c(-1, -1, 1))
      }
      
      # put additional attributes into dimension and data variables
      ncatt_put(ncout,"lon","axis","X")
      ncatt_put(ncout,"lat","axis","Y")
      ncatt_put(ncout,"time","axis","T")
      
      # get a summary of the created nc file
      print(ncout)
  
      # inspect dimensions of the created nc file
      kndvi_nc_array <- ncvar_get(ncout,"kndvi")
      print(paste("Dimensions of nc filse are:", dim(kndvi_nc_array)[1], dim(kndvi_nc_array)[2], dim(kndvi_nc_array)[3]))
      #print(kndvi_nc_array)
  
      # close the nc files writing data to disk
      nc_close(ncout)
    }
}