# ########################################################
# Title         : plotDF_kndviclim_detrend.R
# Description   : plot the df of X | Y | coefficients of the detrending lm
#                 for each variable at each point in the ncdf files
#                 plot the time series of sample datapoints of base, deseasonalised and detrended var timeseries
# Aims          : plots/maps of variables
# Inputs	      : df containing detrended time series and coefficients 
# Outputs	      : plots/maps of variables
# Options	      : 
# Date          : 05/04/23
# Version       : 2 (26/01/23)
# Authors       : Mark Pickering & Agata Elia
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
script_info <- 'plotDF_kndviclim_detrend'                         
script_info_base <- 'createDF_kndviclim_fullTS'   
script_info_detrend <- 'createDF_kndviclim_detrend'               
base_script_date <- '2023-03-14'
detrend_script_date <- '2023-04-15'

######     SET LIBRARIES                      #####
library(chron)   # useful for converting time values
library(dplyr)   # use %>%
# library(ncdf4)   # read ncdf
library(reshape) # reshaping dataframes
# library(raster)  # package for raster manipulation
# library(terra)   # terra library for raster manip
# library(rasterVis)  # enables levelplots
require(ggplot2)      # for plotting
require(scales)       # for ggplot2 functions eg oob & trans
library(ggpubr)       # for arranging ggplots together (ggarrange)
library(ggExtra)      # more complex figures, marginal hists
library(grid)       # for making multi-gird plots and tables
library(gridExtra)  # for making multi-gird plots and tables
library(lattice)  # for making multi-gird plots and tables
# library(ggplotify)  # plots within levelplots
library(rgdal)        # 
library(RColorBrewer) # colour palettes
library(sf)           # utilise shapefiles etc
library(cowplot)      # for ggdraw
library(patchwork)

###################################################
######       SET THEME COMMON PLOTTING VARS   #####
###################################################

source(path_figures_init)

###################################################
######       I/O                              #####
###################################################

# initialise input directory
base_input_dir <- paste0(root_data_proce, script_info_base, '/', script_info_base, '_', base_script_date,  '/')
detrend_input_dir <- paste0(root_data_proce, script_info_detrend, '/', script_info_detrend, '_', detrend_script_date,  '/')

# set/create output directory
output_path <- paste0(root_figures, script_info, '/', script_info, '_', full_date,  '/')
print(paste0('output_path is : ', output_path ))
if(! dir.exists(output_path)) {dir.create(paste0(output_path),recursive=T)} # create directory if not present

#######################################
##### FUNCTIONS                   #####
#######################################

# see initialise_figs.R

#######################################
##### INITIALISE VARIABLES        #####
#######################################

# set the hist and map min/max
v_variables <- c('kndvi', 't2m', 'VPD', 'spei', 'ssr', 'swvl1', 'tp')
v_variables_full <- c('kndvi', '2m temperature', 'VPD', 'SPEI', 'net solar radiation')
v_stats <- 'coeff'
v_stats_full <- 'linear model coefficient'

# set limits for each var in hist and map - each column is the limits for the corresponding variable
#                     kndvi           t2m           VPD         SPEI            SSR             SM          TP          
l_coeff_hist  <- list(c(-1.4e-05, 1.3e-05),       c(-1, 1),     c(-1, 1),   c(-1, 1),       c(-1, 1),       c(-1, 1),   c(-1, 1))
l_coeff_map   <- list(c(-1.4e-05, 1.3e-05),       c(-1, 1),     c(-1, 1),   c(-1, 1),       c(-1, 1),       c(-1, 1),   c(-1, 1))         

l_all <- list(l_coeff_hist, l_coeff_map)

# define a sample x,y pair (to be adjusted to sample more automatically)
#x_sample <- '25.785' # this ppint is missing in the gs masked data because phenology and data are not perfectly same in terms of datapoints
#y_sample <- '71.11'
x_sample <- '11.285' # more central europe and it is in phenology
y_sample <- '50.71'

#######################################
##### CREATE FIGURES              #####
#######################################

# this loops over the variables listed above and plots each of the variables as map and histogram
for (i in 1:length(v_variables)){
  var_i <- v_variables[i] ; print(var_i)
  var_i_full <- v_variables_full[i] 
  
  # load coefficients dataframe
  load(paste0(detrend_input_dir, 'df_', var_i, '_deseason' ,'_full_gs_masked_coefficients.RData'))  
  
  # make histogram of coefficients
  lims_h_i <- l_all[[1]][[i]]
  h_dist <- make_hist(df_coefficients, v_stats, v_stats_full, lims_h_i)

  # make map of coefficients
  lims_m_i <- l_all[[2]][[i]]
  g_input <- make_map(df_coefficients, v_stats, v_stats_full, var_i, lims_m_i)
  
  # save map of coefficients
  ggsave(plot = g_input, filename = paste0(output_path, 'g_', var_i ,'_', v_stats, '.png')) #, width = 10, height = 20)
  
  # combine map with hist of coefficients
  g_draw <- ggdraw( clip = 'off') +
    draw_plot(g_input, x = 0.23, y = 0.23, width = 0.77, height = 0.77, hjust = 0.3) +
    draw_plot(h_dist , x = 0, y = 0,    width = 0.77, height = 0.24, hjust = 0 ) 
  
  # save combined map and hist of coefficients
  ggsave(plot = g_draw, filename = paste0(output_path, 'g_comb_', var_i ,'_', v_stats, '.png' )) #, width = 8, height =10 ) # , width = wid, height = hei)

  # load timeseries dataframes
  load(paste0(base_input_dir, 'df_', var_i, '_baseVar' ,'_full.RData'))
  load(paste0(detrend_input_dir, 'df_', var_i, '_deseason' ,'_full_gs_masked_detrended.RData'))
  #load(paste0(detrend_input_dir, 'df_', var_i, '_deseason' ,'_full_detrended.RData'))

  # for a sample pair of x,y plot the baseVar, gs masked, deaeasonalised and detrended timeseries
  df_base_sample <- df_var[df_var$x==x_sample & df_var$y==y_sample,]
  df_detrend_sample <- df_detrend[df_detrend$x==x_sample & df_detrend$y==y_sample,]
  
  # plot base variable
  g_base <- ggplot(data = df_base_sample, aes(x = as.Date(date, format = "%Y-%m-%d"), y = !!as.symbol(var_i), group=1)) +
    geom_line(lwd=0.3) +
    labs(x = "Date", y = var_i, title = "kNDVI full time series") +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    stat_smooth(method = "lm", formula = y ~ x, geom = "smooth", lwd=0.8)
  
  # plot deseasonalised variable
  g_deseas <- ggplot(data = df_detrend_sample, aes(x = as.Date(date, format = "%Y-%m-%d"), y = !!as.symbol(var_i), group=1)) +
    geom_line(lwd=0.3) +
    labs(x = "Date", y = paste0(var_i, ' anomaly'), title = "kNDVI time series deseasonalised") +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    stat_smooth(method = "lm", formula = y ~ x, geom = "smooth", lwd=0.8)  
  
  # g_deseas <- ggplot() +
  #   geom_line(data = df_detrend_sample, aes(x = as.Date(date, format = "%Y-%m-%d"), y = !!as.symbol(var_i)), color = "blue") +
  #   geom_line(data = df_detrend_sample, aes(x = as.Date(date, format = "%Y-%m-%d"), y = fitted), color = "red") +
  #   labs(x = "Date", y = "kndvi", title = "kNDVI time series deseasonalised with fitted values from lm") +
  #   scale_x_date(date_breaks = "1 year", date_labels = "%Y")
  
  # plot gs masked variable and fitted values
  g_gs <- ggplot() +
    geom_line(data = df_detrend_sample, aes(x = as.Date(date, format = "%Y-%m-%d"), y = var_gs), color = "black", lwd=0.3) +
    geom_line(data = df_detrend_sample, aes(x = as.Date(date, format = "%Y-%m-%d"), y = fitted), color = "red", lwd=0.8) +
    labs(x = "Date", y = paste0(var_i, ' anomaly'), title = "kNDVI time series deseasonalised and masked for gs with fitted values from lm") +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y")
    
  # plot detrended variable
  g_detrend <- ggplot(data = df_detrend_sample, aes(x = as.Date(date, format = "%Y-%m-%d"), y = detrended, group=1)) +
    geom_line(lwd=0.3) +
    labs(x = "Date", y = paste0(var_i, ' anomaly detrended'), title = "kNDVI time series detrended") +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    stat_smooth(method = "lm", formula = y ~ x, geom = "smooth", lwd=0.8)
  
  # combine plots of timeseries
  #g_draw <- g_base / g_deseas / g_detrend           # https://patchwork.data-imaginist.com/
  g_draw <- g_base / g_deseas / g_gs / g_detrend     # https://patchwork.data-imaginist.com/
  
  # save combined plots of timeseries
  ggsave(plot = g_draw, filename = paste0(output_path, 'g_comb_time_series_', var_i ,'_.png' )) #, width = 50, height = 50, limitsize = FALSE) # , width = wid, height = hei) 
  
} # end variable loop