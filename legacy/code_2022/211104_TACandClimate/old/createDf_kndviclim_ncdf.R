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
script_info <- 'createDF_kndviclim'               # used in output name
output_dir_name <- 'createDF_kndviclim/'          # name of output directory
nc_varname <- 'kndvi'                             # name of ncdf variable

# agg_fact <- 1                                   # aggregation T/F and factor for visualisation purposes (speeds up visualisation but associated problems)
text_size = 10                                    # text size in figures - big maps
legend_size <- 1

######     SET LIBRARIES                      #####
library(chron)   # useful for converting time values
library(dplyr)   # use %>%
library(ncdf4)   # read ncdf
library(reshape) # reshaping dataframes
library(raster)  # package for raster manipulation
library(terra)   # terra library for raster manip
library(reshape) # melt dfs to wide-long
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

input_t2m_deseason   <- 'data/climate/8day/T2M/T2M_3_europe/resolution/t2m_allY_timsel8_europe3nn_deseason.nc'
input_VPD_deseason   <- 'data/climate/8day/VPD/VPD_3_europe/resolution/VPD_allY_timsel8_europe3nn_deseason.nc'
input_spei_deseason  <- 'data/climate/8day/SPEI/SPEI_3_europe/resolution/SPEI_allY_timsel8_europe3nn_deseason.nc'
input_ssr_deseason   <- 'data/climate/8day/SSR/SSR_3_europe/resolution/SSR_allY_timsel8_europe3nn_deseason.nc'
input_swvl1_deseason <- 'data/climate/8day/SM/SM_3_europe/resolution/SM_allY_timsel8_europe3nn_deseason.nc'

# set/create output directory
output_root <- paste0('data_processing/', output_dir_name)
output_path <- paste0(output_root, script_info, '_', full_date,  '/')
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

v_variables <- c('kndvi', 't2m', 'VPD', 'spei', 'ssr', 'swvl1' )
v_files_deseason     <- c(input_kndvi_deseason,  input_t2m_deseason, input_VPD_deseason, input_spei_deseason, input_ssr_deseason, input_swvl1_deseason)

for (i in 2:length(v_variables)){
  var_i <- v_variables[i] ; print(var_i)
  file_i <- v_files_deseason[i]
  
  # open raster and extract dates of entries
  r_i  <- rast(file_i) #, varname = 'kndvi') ; plot(r_i[[1]])
  times_i <- list(time(r_i)) # ; typeof(input_time[[1]]) # [[c(1,2)]]
  dates_i <- sapply(  strsplit( as.character(times_i[[1]]), ' ' ) , '[', 1 )
  
  # convert r to dataframe, reset titles, convert to long format
  df_i <- as.data.frame(r_i, xy = T, na.rm = T)
  # head(df_i[,1:6]) ; dim(df_i)  # summary(df_i) # hist(df_i[[3]])
  names(df_i)[3:length(names(df_i))] <- dates_i
  df_i <- melt(df_i, id=c("x","y"), variable_name = "date") # head(df_i) , value.name = 'kndvi') # value_name = "kndvi" ) 
  names(df_i)[4] <- var_i
  
  # save individual dataframes and join to single dataframe
  save(df_i, file=paste0(output_path, 'df_',var_i, '_full.RData' )    )
  if(i==1) { df_kndviclim <- df_i
  } else{    df_kndviclim <- left_join(df_kndviclim, df_i)  }
  
  
}

head(df_kndviclim)
save(df_kndviclim, file=paste0(output_path, 'df_all_fullKndvi_ljoinClim.RData' )    )


# ###################################################
# ######       extract ncdfs - raster stack     #####
# ###################################################
# # simple method might take too long
# 
# r_kndvi_part  <- rast(input_kndvi) #, varname = 'kndvi')
# # names(r_kndvi_part) ; plot(r_kndvi_part[[1]]) ; summary(r_kndvi_part[[1]])
# time(r_kndvi_part)
# # time(r_kndvi_part[[c(1,2)]]) # # df_kndvi_1 <- as.data.frame(r_kndvi_part[[c(1,2)]], xy = T, na.rm = T)
# df_kndvi_1 <- as.data.frame(r_kndvi_part, xy = T, na.rm = T)
# head(df_kndvi_1[,1:6]) ; dim(df_kndvi_1)  # summary(df_kndvi) # hist(df_kndvi$kndvi)
# # head(df_kndvi_1$y)
# 
# input <- list(time(r_kndvi_part)) ; typeof(input[[1]]) # [[c(1,2)]]
# dates <- sapply(  strsplit( as.character(input[[1]]), ' ' ) , '[', 1 )
# 
# names(df_kndvi_1)[3:length(names(df_kndvi_1))] <- dates
# df_kndvi_2 <- melt(df_kndvi_1, id=c("x","y"), variable_name = "date") # , value.name = 'kndvi') # value_name = "kndvi" ) 
# names(df_kndvi_2)[4] <- 'kndvi'
# # df_kndvi_2[4] <- as.numeric(df_kndvi_2[4])
# head(df_kndvi_2) ; dim(df_kndvi_2) ; summary(df_kndvi_2)
# 
# 
# 
# 
# ###################################################
# ######       extract ncdfs - raster stack     #####
# ###################################################
# # simple method might take too long
# 
# r_kndvi  <- stack(input_kndvi, varname = 'kndvi')
# # names(r_kndvi) ; plot(r_kndvi[[1]])
# df_kndvi <- as.data.frame(r_kndvi, xy = T, na.rm = T, long = T)
# 
# # convert the time/date to just a date 
# df_kndvi <- df_kndvi %>% mutate( Z = lubridate::date(Z) )
# names(df_kndvi)[1] <- 'x' ; names(df_kndvi)[2] <- 'y'
# names(df_kndvi)[3] <- 'date'
# names(df_kndvi)[4] <- 'kndvi'
# 
# head(df_kndvi) ; dim(df_kndvi)  # summary(df_kndvi) # hist(df_kndvi$kndvi)
# str(df_kndvi)
# save(df_kndvi, file=paste0(output_path, 'df_kndvi.RData' )    )
# 
# ## same for the t2m
# 
# r_t2m  <- stack(input_t2m, varname = 't2m')
# df_t2m <- as.data.frame(r_t2m, xy = T, na.rm = T, long = T)
# df_t2m <- df_t2m %>% mutate( Z = lubridate::date(Z) )
# names(df_t2m)[1] <- 'x' ; names(df_t2m)[2] <- 'y'
# names(df_t2m)[3] <- 'date'
# names(df_t2m)[4] <- 't2m'
# # head(df_t2m) ; dim(df_t2m)  # summary(df_t2m)
# save(df_t2m, file=paste0(output_path, 'df_t2m.RData' )    )
# 
# ## same for the VPD
# 
# r_VPD  <- stack(input_VPD, varname = 'VPD')
# df_VPD <- as.data.frame(r_VPD, xy = T, na.rm = T, long = T)
# df_VPD <- df_VPD %>% mutate( Z = lubridate::date(Z) )
# names(df_VPD)[1] <- 'x' ; names(df_VPD)[2] <- 'y'
# names(df_VPD)[3] <- 'date'
# names(df_VPD)[4] <- 'VPD'
# # head(df_VPD) ; dim(df_VPD)  # summary(df_VPD)
# save(df_VPD, file=paste0(output_path, 'df_VPD.RData' )    )
# 
# ## same for the spei
# 
# r_spei  <- stack(input_SPEI, varname = 'spei')
# df_spei <- as.data.frame(r_spei, xy = T, na.rm = T, long = T)
# df_spei <- df_spei %>% mutate( Z = lubridate::date(Z) )
# names(df_spei)[1] <- 'x' ; names(df_spei)[2] <- 'y'
# names(df_spei)[3] <- 'date'
# names(df_spei)[4] <- 'spei'
# # head(df_spei) ; dim(df_spei)  # summary(df_spei) # hist(df_spei$x)
# save(df_spei, file=paste0(output_path, 'df_spei.RData' )    )
# 
# # ssr
# 
# r_ssr  <- stack(input_ssr, varname = 'ssr')
# df_ssr <- as.data.frame(r_ssr, xy = T, na.rm = T, long = T)
# df_ssr <- df_ssr %>% mutate( Z = lubridate::date(Z) )
# names(df_ssr)[1] <- 'x' ; names(df_ssr)[2] <- 'y'
# names(df_ssr)[3] <- 'date'
# names(df_ssr)[4] <- 'ssr'
# # head(df_ssr) ; dim(df_ssr)  # summary(df_ssr)
# save(df_ssr, file=paste0(output_path, 'df_ssr.RData' )    )
# 
# # swvl1
# 
# r_swvl1  <- stack(input_swvl1, varname = 'swvl1')
# df_swvl1 <- as.data.frame(r_swvl1, xy = T, na.rm = T, long = T)
# df_swvl1 <- df_swvl1 %>% mutate( Z = lubridate::date(Z) )
# names(df_swvl1)[1] <- 'x' ; names(df_swvl1)[2] <- 'y'
# names(df_swvl1)[3] <- 'date'
# names(df_swvl1)[4] <- 'swvl1'
# # head(df_swvl1) ; dim(df_swvl1)  # summary(df_swvl1)
# save(df_swvl1, file=paste0(output_path, 'df_swvl1.RData' )    )
# 
# 
# ###################################################
# ######       JOIN DATAFRAMES                  #####
# ###################################################
# # do the dataframes match up in x,y? 
# df_kndvi$x[1]
# df_kndvi$x[1]
# 
# load('/eos/jeodpp/data/projects/FOREST-RESILIENCE/figures/createDF_kndviclim/createDF_kndviclim_2022-11-07/df_kndvi.RData')
# 
# df_kndvi$x <- round(df_kndvi$x, digits = 3) ; df_kndvi$y <- round(df_kndvi$y, digits = 3)
# df_ssr$x <- round(df_ssr$x, digits = 3) ; df_ssr$y <- round(df_ssr$y, digits = 3)
# 
# df_kndviclim <- left_join(df_kndvi, df_ssr)
# summary(df_kndviclim)
# 
# ###################################################
# ######       extract ncdfs                    #####
# ###################################################
# # raster stack method didn't work - too large? try to use netcdf
# 
# ncin_kndvi <- nc_open(input_kndvi)
# 
# # extract dimensions
# nc_lon <- ncvar_get(ncin_kndvi,"lon")
# nc_lat <- ncvar_get(ncin_kndvi,"lat")
# n_lon <- dim(nc_lon) ; n_lat <- dim(nc_lat)
# nc_time <- ncvar_get(ncin_kndvi,"time")
# tunits <- ncatt_get(ncin_kndvi,"time","units")
# n_time <- dim(nc_time)
# print(paste0('dim: ', n_lon, ', ', n_lat, ', ', n_time))
# 
# # extract values and fill
# arr_kndvi <- ncvar_get(ncin_kndvi, 'kndvi')
# dim(arr_kndvi) # head(arr_kndvi) ; summary(arr_kndvi)
# fillvalue <- ncatt_get(ncin_kndvi, 'kndvi' ,"_FillValue")
# arr_kndvi[arr_kndvi==fillvalue$value] <- NA
# 
# # visualisation check
# # arr_slice <- arr_kndvi[,,2]
# # image(nc_lon,nc_lat,arr_slice, col=rev(brewer.pal(10,"RdBu")))
# 
# # convert time units
# tustr <- strsplit(tunits$value, " ")
# tdstr <- strsplit(unlist(tustr)[3], "-") ; days_or_hours <- tustr[[1]][1]
# tmonth <- as.integer(unlist(tdstr)[2])
# tday <- as.integer(unlist(tdstr)[3])
# tyear <- as.integer(unlist(tdstr)[1])
# chron(nc_time,origin=c(tmonth, tday, tyear))
# 
# # chron calculates days difference (must convert to days if units are hours difference) - if not days or hours, break
# if(days_or_hours == 'hours'){   nc_time <- nc_time/24 }else if(days_or_hours != 'days'){  stop("Incorrect date format", call.=FALSE)}
# nc_time_dates <- chron(nc_time,origin=c(tmonth, tday, tyear))
# 
# # create dataframe
# 
# 
# # convert ncdf to a raster stack
# # r_kndvi  <- rast(input_kndvi) #, lyrs=874) #, varname = 'kndvi')
# r_kndvi  <- stack(input_kndvi, varname = 'kndvi')
# names(r_kndvi)
# plot(r_kndvi[[1]])
# r_kndvi_red <- r_kndvi # [[1:5]]
# # df_kndvi_red  <- as.data.frame(r_kndvi_red, xy = T)
# df_variable_red <- as.data.frame(r_kndvi_red, xy = T, na.rm = T, long = T)
# 
# head(df_variable_red) ; dim(df_variable_red)
# head(df_kndvi_red)    ; dim(df_kndvi_red)
# 
# 
# dim(r_kndvi)
# r_kndvi <- r_kndvi[,,1:3]
# 
# plot(r_kndvi[,,1])
# # plot(r_kndvi$X2003.01.04.00.23.30)
# # convert raster stack to a dataframe with xy coord
# df_kndvi  <- as.data.frame(r_kndvi, xy = T)
# head(df_kndvi)
# 
# r_kndvi  <- rast(input_VPD, ) #, varname = 'kndvi')
# 
# 
# 
# # r_kndvi  <- rast(input_t2m) #, lyrs=874) #, varname = 'kndvi')
# r_t2m  <- stack(input_t2m, varname = 't2m')
# # plot(r_kndvi[,,1])
# # plot(r_kndvi$X2003.01.04.00.23.30)
# # convert raster stack to a dataframe with xy coord
# # df_kndvi  <- as.data.frame(r_kndvi, xy = T)
# head(df_kndvi)
# 
# # r_kndvi  <- rast(input_VPD, ) #, varname = 'kndvi')
# 
# r_t2m  <- stack(input_t2m, varname = 't2m')
# df_variable <- as.data.frame(r_t2m, xy = T, na.rm = T, long = T)
# head(df_variable) ; dim(df_variable)
