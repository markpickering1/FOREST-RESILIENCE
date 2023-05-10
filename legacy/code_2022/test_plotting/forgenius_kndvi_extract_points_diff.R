# ########################################################
# Title         : forgenius_kndvi_extract_points.R
# Description   : extract selected values of kndvi across timeseries at 500m
# Aims          : extract kndvi timeseries for selected points
# Inputs	      : netcdf containing KNDVI and set of points to extract
# Outputs	      : two dataframes
# Options	      : 
# Date          : 8/2/23
# Version       : 1
# Authors       : Mark Pickering & Marco Girardello
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
script_info <- 'forgenius_kndvi_extract_points_test'               # used in output name

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
# input_kndvi   <- 'data/vegetation/8day/KNDVI/KNDVI_nc_deseasonalised_005/merged_kndvi_2003_2021_deseasonalised_005_con.nc' # '
# input_kndvi_nodeseason <- 'data/vegetation/8day/KNDVI/KNDVI_nc_merged_005/merged_kndvi_2003_2021_005_con.nc'
input_kndvi <- 'data/vegetation/8day/KNDVI_no_mask/KNDVI_no_mask_nc_merged/merged_kndvi_no_mask_2003_2021.nc'
input_xy    <- 'data/ancillary/forgenius/df_gcu_xy_plots.RData'

# set/create output directory
output_path <- paste0(root_data_proce, script_info, '/', script_info, '_', full_date,  '/')
print(paste0('output_path is : ', output_path ))
if(! dir.exists(output_path)) {dir.create(paste0(output_path),recursive=T)} # create directory if not present

###################################################
######       load data                        #####
###################################################

#  maybe need to cycle over rasters and extract from each specifically
load(input_xy)
head(v_gcu)
# get spatial points
vect_gcu <- vect(v_gcu)

# terra - extract raster
t_i_baseVar  <- terra::rast(input_kndvi) #, varname = 'kndvi') ; plot(t_i_baseVar[[6]])
times_i_baseVar <- list(time(t_i_baseVar)) # ; typeof(input_time[[1]]) # [[c(1,2)]]
dates_i_baseVar <- sapply(  strsplit( as.character(times_i_baseVar[[1]]), ' ' ) , '[', 1 )
head(dates_i_baseVar)
dim(t_i_baseVar)

# create second offset timeseries (only for use when creating the offset dataset)
dates_i_baseVar <- dates_i_baseVar[2:length(dates_i_baseVar)]
dates_i_baseVar[874] <-  "2022-01-04"

crs(vect_gcu)  <-  crs(t_i_baseVar)
df_gcu_rastVals <- data.frame()

# loop over the 
for( i in 1:dim(t_i_baseVar)[3] ){
# for( i in 11:1000 ){
  if(i %% 20 == 0) print(i)
  dates_i_baseVar_i <- dates_i_baseVar[i]
  t_i_baseVar_i <- t_i_baseVar[[i]]
  
  # extract points
  v_gcu_rast_i <- terra::extract(t_i_baseVar_i, vect_gcu  ) #   xy = T extracts location of raster pixel
  names(v_gcu_rast_i)[2] <- dates_i_baseVar_i
  if(i ==1 ){df_gcu_rastVals <- cbind(v_gcu, v_gcu_rast_i) 
  } else{ df_gcu_rastVals <- full_join(df_gcu_rastVals, v_gcu_rast_i) }
  if ( i == 200 ){save(df_gcu_rastVals, file=paste0(output_path, 'df_kndvi_ts_gcuPoints_1-',i ,'.RData' ))    }
  
}

head(df_gcu_rastVals) ; dim(df_gcu_rastVals); summary(df_gcu_rastVals[1:10]) # , maxsum = 10) # getOption("max.print"))
# save 
save(df_gcu_rastVals, file=paste0(output_path, 'df_kndvi_ts_gcuPoints.RData' )    )


ygh

t_i_baseVar_1 <- t_i_baseVar[[1:2]]
head(v_gcu)
vect_gcu <- vect(v_gcu)
crs(vect_gcu)  <-  crs(t_i_baseVar)
# v_gcu_rast_1 <- terra::extract(t_i_baseVar_1, vect_gcu  ) #   xy = T extracts location of raster pixel
v_gcu_rast_1 <- terra::extract(t_i_baseVar, vect_gcu  ) #   xy = T extracts location of raster pixel
# head(v_gcu_rast_1) ; v_gcu_rast_1[72,]
v_gcu_rast_1 <- cbind(v_gcu, v_gcu_rast_1) # head(v_gcu_rast_1_comp)
# v_gcu_rast_df_test <- as.data.frame(t_i_baseVar_1[[2]], xy=T)
# head(v_gcu_rast_df_test %>% filter(x > 17.16, x < 17.18, y>  49.95, y <  49.96))

# next divide by 


r_i_baseVar  <- raster(input_kndvi) #, varname = 'kndvi') ; plot(r_i_baseVar[[2]])
r_i_baseVar[[1]] ; 
r_i_baseVar
times_i_baseVar <- list(time(r_i_baseVar)) # ; typeof(input_time[[1]]) # [[c(1,2)]]
dates_i_baseVar <- sapply(  strsplit( as.character(times_i_baseVar[[1]]), ' ' ) , '[', 1 )

length(dates_i_baseVar) ; head(dates_i_baseVar)


r_i_baseVar  <- raster(input_kndvi) #, varname = 'kndvi') ; plot(r_i_baseVar[[1]])
dim(r_i_baseVar)
r_i_baseVar_1 <- r_i_baseVar[[1]]
sp_gcu <- SpatialPoints(v_gcu)
crs(sp_gcu)  <-  crs(r_i_baseVar)
# v_gcu_rast_1 <- raster::extract(r_i_baseVar_1, sp_gcu,  ) # , sp = T extract(dfr_r, v_gcu)
v_gcu_rast <- raster::extract(r_i_baseVar, sp_gcu,  ) # , sp = T extract(dfr_r, v_gcu)
head(sp_gcu)

length(v_gcu_rast)
# v_gcu_rast <- cbind(v_gcu, v_gcu_rast)
# head(v_gcu_rast) ; head(as.data.frame(r_i_baseVar_1, xy = T) %>% filter(x>20.68, x<20.7, y>52.17, y<52.19))

times_i_baseVar <- list(time(r_i_baseVar)) # ; typeof(input_time[[1]]) # [[c(1,2)]]
dates_i_baseVar <- sapply(  strsplit( as.character(times_i_baseVar[[1]]), ' ' ) , '[', 1 )


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

