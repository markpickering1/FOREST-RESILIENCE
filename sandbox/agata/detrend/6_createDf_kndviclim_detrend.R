# ########################################################
# Title         : 6_createDf_kndviclim_detrend.R
# Description   : detrend timeseries of deseasonalised data (anomalies)
# Aims          : to obtain dataframes of detrended timeseries of deseasonalised data (anomalies)
# Inputs	      : dataframes of deseasonalised data (anomalies)
# Outputs	      : dataframes of detrended deseasonalised data (anomalies) and plots of sample input and output timeseries
# Options	      : 
# Date          : 05/04/23
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

# initialise R and plotting environments
source('/eos/jeodpp/data/projects/FOREST-RESILIENCE/GIT-FOREST-RESILIENCE/FOREST-RESILIENCE/code/main/initialise_R.R')

######     GLOBAL VARS                        #####
script_info <- 'createDF_kndviclim_detrend'               # used in output name
#script_info_input <- 'createDF_kndviclim_fullTS'         # input data dir of full deseas timeseries
#input_script_date <- '2023-03-14'
script_info_input <- 'createDF_kndviclim_phenology'       # input data dir of gs masked deseas timeseries
input_script_date <- '2023-04-15'

######     SET LIBRARIES                      #####
# library(chron)   # useful for converting time values
library(dplyr)   # use %>%
library(stringr)
library(lubridate)
library(pracma)
# library(ncdf4)   # read ncdf
# library(reshape) # reshaping dataframes
# library(raster)  # package for raster manipulation
# library(terra)   # terra library for raster manip
# library(rasterVis)  # enables levelplots
require(ggplot2)      # for plotting
# require(scales)       # for ggplot2 functions eg oob & trans
# library(ggpubr)       # for arranging ggplots together (ggarrange)
# library(ggExtra)      # more complex figures, marginal hists
# library(grid)       # for making multi-gird plots and tables
# library(gridExtra)  # for making multi-gird plots and tables
# library(lattice)  # for making multi-gird plots and tables
# library(ggplotify)  # plots within levelplots
# library(rgdal)        # 
# library(RColorBrewer) # colour palettes
# library(sf)           # utilise shapefiles etc

###################################################
######       I/O                              #####
###################################################

# initialise input directory
input_dir <- paste0(root_data_proce, script_info_input, '/', script_info_input, '_', input_script_date,  '/')

# set/create output directory  - use same as before
output_path <- paste0(root_data_proce, script_info, '/', script_info, '_', full_date,  '/')
print(paste0('output_path is : ', output_path ))
if(! dir.exists(output_path)) {dir.create(paste0(output_path),recursive=T)} # create directory if not present

###################################################
######     SET FUNCTIONS                      #####
###################################################

# get_coefficients <- function(df_i, var_name ){
#   # function takes a df, df_i, with x,y pixels, and values of column var_name for each date
#   # runs a lm grouped by xy and save the coefficients of the model into another dataframe
#   
#   df_coefficients <- df_var %>% dplyr::group_by( x, y ) %>% mutate(coeff = coefficients(lm( !!as.symbol(var_i)~as.Date(date, format = "%Y-%m-%d"), na.action='na.exclude')))
#   df_coefficients <- df_coefficients[!duplicated(df_coefficients[ , c("x", "y")]), ] #https://statisticsglobe.com/unique-rows-of-data-frame-based-on-selected-columns-in-r
#   
#   return(df_coefficients)
# }
# 
# df_coeff <- get_coefficients(df_var, var_i)

####################################################################
##### RUN THROUGH DATAFRAMES AND RUN LM                        #####
####################################################################

# define variables to detrend
v_variables <- c('kndvi', 't2m', 'VPD', 'spei', 'ssr', 'swvl1', 'tp') #select only variables that have an actual time-series
#v_variables <- c('ssr')

# run through the deseasonalised timeseries
for (i in 1:length(v_variables)){
  
  # select variable
  var_i <- v_variables[i] ; print(var_i)
  
  # remove previous df
  rm(df_var)
  rm(df_detrend)
  rm(df_coefficients)
  
  # check if output file already exists
  #if(! file.exists(paste0(output_path, 'df_', var_i, '_deseason' ,'_full_detrended.RData'))){
  if(! file.exists(paste0(output_path, 'df_', var_i, '_deseason' ,'_full_gs_masked_detrended.RData'))){
    
    # open df of deseasonalised data
    #load( paste0(input_dir, 'df_', var_i, '_deseason' ,'_full.RData'  ) )
    load( paste0(input_dir, 'df_', var_i, '_deseason' ,'_full_gs_masked.RData'  ) )
          
    # group by x and y, fit a lm to the timeseries and add fitted values and detrended (as variable - fitted value)
    # df_fitted <- df_var %>% dplyr::group_by( x, y ) %>% 
    #   mutate(fitted = fitted.values(lm( !!as.symbol(var_i)~as.Date(date, format = "%Y-%m-%d"), na.action='na.exclude')))
    # df_detrend <- df_fitted %>% mutate(detrended = !!as.symbol(var_i)-fitted)
    
    # group by x and y, fit a lm to the timeseries and add fitted values and detrended (as variable - fitted value) (case of gs masked data)
    df_detrend <- df_var %>% dplyr::group_by( x, y ) %>%
      filter(any(!is.na(var_gs))) %>%
      mutate(fitted = fitted.values(lm( var_gs~as.Date(date, format = "%Y-%m-%d"), na.action='na.exclude')))
    df_detrend <- df_detrend %>% mutate(detrended = var_gs-fitted)
    
    # save lighter version
    # df_detrend <- df_detrend[,c("x","y","date","detrended")]
    # df_detrend <- na.omit(df_detrend)
    
    # save detrended df
    save(df_detrend, file=paste0(output_path, 'df_', var_i, '_deseason' ,'_full_gs_masked_detrended.RData'  ) )
    
    # group by x and y, fit a lm to the timeseries and return coefficients dataframe
    # df_coefficients <- df_var %>% dplyr::group_by( x, y ) %>% 
    #   mutate(coeff = coefficients(lm( !!as.symbol(var_i)~as.Date(date, format = "%Y-%m-%d"), na.action='na.exclude'))[2])
    
    # group by x and y, fit a lm to the timeseries and return coefficients dataframe (case of gs masked data)
    df_coefficients <- df_var %>% dplyr::group_by( x, y ) %>%
      filter(any(!is.na(var_gs))) %>%
      mutate(coeff = coefficients(lm( var_gs~as.Date(date, format = "%Y-%m-%d"), na.action='na.exclude'))[2])

    # create dataframe with only unique pair of x,y,coefficients
    df_coefficients <- df_coefficients[!duplicated(df_coefficients[ , c("x", "y")]), ] #https://statisticsglobe.com/unique-rows-of-data-frame-based-on-selected-columns-in-r
    df_coefficients <- df_coefficients[,c(1:2, 10)]

    # save coefficients df
    #save(df_coefficients, file=paste0(output_path, 'df_', var_i, '_deseason' ,'_full_coefficients.RData'  ) )
    save(df_coefficients, file=paste0(output_path, 'df_', var_i, '_deseason' ,'_full_gs_masked_coefficients.RData'  ) ) # case of gs masked df
  }
} # end var loop

# # Test on one timeseries only by plotting kndvi versus fitted kndvi 
# load( paste0(input_dir, 'df_', 'kndvi', '_deseason' ,'_full.RData'  ) )
# 
# df_var_test <- df_var[df_var$x=='25.785' & df_var$y=='71.11',]
#  
# g1 <- ggplot(data = df_var_test, aes(x = as.Date(date, format = "%Y-%m-%d"), y = kndvi, group=1)) +
#        geom_line() +
#        labs(x = "Date", y = "kNDVI", title = "kNDVI time series") +
#        scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
#        stat_smooth(method = "lm", formula = y ~ x, geom = "smooth")
# 
# slm <- lm(kndvi~as.Date(date, format = "%Y-%m-%d"), df_var_test, na.action='na.exclude', x=TRUE)
# fitted_values <- fitted.values(slm)
# dates <- unique(df_var_test$date)
# fitted_values_dates <- data.frame(dates, fitted_values)
# colnames(fitted_values_dates) <- c('date', 'fitted')
# joined <- inner_join(df_var_test, fitted_values_dates)
# 
# g2 <- ggplot() + 
#   geom_line(data = joined, aes(x = as.Date(date, format = "%Y-%m-%d"), y = kndvi), color = "blue") +
#   geom_line(data = joined, aes(x = as.Date(date, format = "%Y-%m-%d"), y = fitted), color = "red") +
#   labs(x = "Date", y = "kNDVI", title = "kNDVI time series") +
#   scale_x_date(date_breaks = "1 year", date_labels = "%Y") 
# 
# joined_detrended <- joined %>% mutate(detrended = kndvi - fitted)
# 
# g3 <- ggplot(data = joined_detrended, aes(x = as.Date(date, format = "%Y-%m-%d"), y = detrended, group=1)) +
#   geom_line() +
#   labs(x = "Date", y = "kNDVI", title = "kNDVI time series detrended") +
#   scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
#   stat_smooth(method = "lm", formula = y ~ x, geom = "smooth")