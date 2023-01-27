# ########################################################
# Title         : plotDF_kndviclim_muTACcov.R
# Description   : plot the df of X | Y | mu | CoV | TAC_residuals
#                 for each variable at each point in the ncdf files
#                 plot the df of mean, coeff of var, 1-lag TAC for each variable
#                 do some general visualisation
# Aims          : dataframes of all data and RF predictor data for each X/Y
# Inputs	      : df containing KNDVI and CLIMATE data 
# Outputs	      : plots/maps of variables
# Options	      : 
# Date          : 14/11/22
# Version       : 1.2 (1/12/22)
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
script_info <- 'plotDF_kndviclim_muTACcov'               # used in output name
output_dir_name <- 'plotDF_kndviclim_muTACcov/'          # name of output directory

# agg_fact <- 1                                   # aggregation T/F and factor for visualisation purposes (speeds up visualisation but associated problems)
text_size = 10                                    # text size in figures - big maps
legend_size <- 1

######     SET LIBRARIES                      #####
library(chron)   # useful for converting time values
library(dplyr)   # use %>%
# library(ncdf4)   # read ncdf
library(reshape) # reshaping dataframes
# library(raster)  # package for raster manipulation
# library(terra)   # terra library for raster manip
library(reshape) # melt dfs to wide-long
# library(rasterVis)  # enables levelplots
require(ggplot2)      # for plotting
require(scales)       # for ggplot2 functions eg oob & trans
library(ggpubr)       # for arranging ggplots together (ggarrange)
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
# input_dir <- 'data_processing/createDF_kndviclim_muTACcov/createDF_kndviclim_muTACcov_2022-11-11/' # old data
# input_dir <- 'data_processing/createDF_kndviclim_muTACcov/createDF_kndviclim_muTACcov_2022-12-04/' # old data - didn't used na.rm = T cutting out pixels with any missing values
input_dir <- 'data_processing/createDF_kndviclim_muTACcov/createDF_kndviclim_muTACcov_2023-01-12/' #

# set/create output directory
output_root <- paste0('data_processing/', output_dir_name)
output_path <- paste0(output_root, script_info, '_', full_date,  '/')
print(paste0('output_path is : ', output_path ))
if(! dir.exists(output_path)) {dir.create(paste0(output_path),recursive=T)} # create directory if not present



#######################################
##### INITIALISE SHAPEFILES       #####
#######################################
# load and process shapefile
land_shapefile <- 'data/ancillary/world_vectors/50m_coastline/ne_50m_coastline.shp' # set shapefile path (global coastlines@50m)
land_shapefile <- sf::st_read(land_shapefile, quiet = TRUE)                         # read shapefile
# summary(land_shapefile) # plot(land_shapefile)

# load bounding box from shapefile (has coords: -10.66164 34.56369 44.82037 71.18416)
bb_shapefile <- 'data/ancillary/world_vectors/boundingBoxes/Europe_BB.shp'
bboxpolygon <- sf::st_read(bb_shapefile, quiet = TRUE)                         # read shapefile
land_shapefile <- sf::st_crop(land_shapefile, bboxpolygon)                          # crop shapefile to bounding box

# crop land_shapefile
land_shapefile <- sf::st_crop(land_shapefile, bboxpolygon)                          # crop shapefile to bounding box

#######################################
##### INITIALISE FUNCTIONS        #####
#######################################

#for plot making - rounds up to predetermined numbers shown
#roundUpNice <- function(x, nice=c(1,2,4,5,6,8,10)) {
NiceNumbers=c(0,1,1.5,2,3,4,5,6,7,8,9,10)
roundUpNice <- function(x, nice = NiceNumbers) { #https://rdrr.io/cran/subscreen/src/inst/shiny-app/subscreen/app.R
  if (length(x) != 1) stop("'x' must be of length 1")
  if (x >= 0) 10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) *nice)[[1]]]]
  else -1 * (roundDownNice(-x, nice = NiceNumbers))
}

roundDownNice <- function(x, nice = NiceNumbers) {
  if (length(x) != 1) stop("'x' must be of length 1")
  if (x >= 0) 10^floor(log10(x)) * nice[[max(which(x >= 10^floor(log10(x)) * nice))]]
  else -1 * (roundUpNice(-x, nice = NiceNumbers))
}


find_cutpts_from_percentile <- function(df_plotting, col, perc){
  # get the symmetrical cutpoints, (min_scale, max_scale) for a column 'col, within a dataframe, df,
  # using percentiles 'perc' of the values 
  # e.g. perc == 0.01 means max/min scale are at 1% and 99%
  max_val <- quantile(df_plotting[[col]], 1-perc, na.rm = TRUE) ;   min_val <- quantile(df_plotting[[col]], perc,na.rm = TRUE)
  min_scale <-roundDownNice( abs(min_val) ) ;  if( min_val < 0) {min_scale <- 0 - min_scale} ; max_scale <-roundUpNice( max_val )
  if(max_scale*min_scale < 0 ) {
    # if one (only) of max_scale or min_scale is negative, then we want to take the highest absolute value 
    # and set the max or min as that so that the axis is symmetrical 
    if (max_scale >= abs(min_scale)){min_scale <- -1*abs(max_scale)}
    else{max_scale <- abs(min_scale)}
  }
  result <- c(min_scale, max_scale)
  return(result)
}

#######################################
##### INITIALISE THEMES           #####
#######################################

# set theme for figures
theme_fig <- theme( # set the 'theme' various aesthetic features
  legend.position = "bottom",
  legend.background = element_rect(fill = "white"), # grey95 grey box around legend
  # legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid')
  # element_rect = element_blank(),
  text = element_text(size = text_size),
  panel.grid = element_line(color = "white"), # "transparent" set the colour of the grid lines
  panel.background = element_blank(), # set the colour of the panel behind the figure
  plot.title = element_text(size=text_size, hjust = 0.5) # resize centre the plot title
)

hist_theme <- theme(plot.margin = margin(0, 210, 0, 210),    # add left right margin so that the hist arranges better
                    text = element_text(size = text_size)
)           

#######################################
##### INITIALISE VARIABLES        #####
#######################################
# set the hist and map min/max
# probably better to set as universal values rather than dynamically coding them

# load( paste0(input_dir, 'df_kndvi_muTACcov.RData'))
# head(df_stats) ; dim(df_stats) ; summary(df_stats)

v_variables <- c('kndvi', 't2m', 'VPD', 'spei', 'ssr', 'swvl1', 'tp')
v_variables_full <- c('kndvi', '2m temperature', 'VPD', 'SPEI', 'net solar radiation', 'soil moisture', 'total precipitation')
v_stats <- c('mu_var', 'sd_var', 'cv_var', 'tac_resid')
v_stats_full <- c('mean', 'std. dev.', 'CoV', 'residual TAC')
# set limits for each var in hist and map
l_mu_hist  <- list( c(0, 6000 )   , c(250, 300), c(0, 3)   , c(-0.05, 0.05), c(0, 12000000) , c(0, 1)   , c(0,1)      )
l_mu_map   <- list( c(2000, 4000 ), c(270, 290), c(0, 0.8) , c(-0.02, 0.02), c(0, 10000000) , c(0, 0.4) , c(0,0.3)    ) 
l_sd_hist  <- list( c(0, 2500 )   , c(0, 14)   , c(0, 2.5) , c(0, 1)       , c(0, 5500000 ) , c(0, 0.2) , c(0,1)      ) 
l_sd_map   <- list( c(0, 2000 )   , c(0, 10)   , c(0, 0.6) , c(0.9, 1.0)   , c(0, 4000000 ) , c(0, 0.1) , c(0,0.3)    )
l_cv_hist  <- list( c(0, 150 )    , c(0, 5)    , c(0, 120) , c(0, 140)     , c(0, 120)      , c(0, 200) , c(0,300)    )
l_cv_map   <- list( c(0, 60 )     , c(0, 4)    , c(0, 100) , c(115,125)    , c(0, 100)      , c(0, 40)  , c(0, 150)   )
l_tac_hist <- list( c(-0.1, 1 )   , c(0, 0.6)  , c(0, 0.8) , c(0, 1)       , c(0, 0.6)      , c(0, 1)   , c(-0.1,0.3) )
l_tac_map  <- list( c(0, 0.6 )    , c(0, 0.6)  , c(0, 0.6) , c(0.7, 0.9)   , c(0, 0.4)      , c(0, 0.8) , c(0, 0.2)   )

l_all <- list( list(l_mu_hist, l_mu_map), list(l_sd_hist, l_sd_map), list(l_cv_hist, l_cv_map), list(l_tac_hist, l_tac_map) )
# l_all <- list( l_mu_hist, l_mu_map, l_sd_hist, l_sd_map, l_cv_hist, l_cv_map )
# s_tac_hist <- c(0,1) ; s_tac_map <- c(0,0.8)

n_bins <- 100

#######################################
##### CREATE FIGURES              #####
#######################################

for (i in 1:length(v_variables)){
  var_i <- v_variables[i] ; print(var_i)
  var_i_full <- v_variables_full[i] 
  # head(ls()[ls()=='df_i'])
  
  # load( paste0(input_dir, 'df_', file_i_type ,'_full.RData'  ) )
  load( paste0(input_dir, 'df_', var_i, '_muTACcov.RData'  ) )
  
  for(j in 1:length(v_stats)){
    stat_j <- v_stats[j] ; print(stat_j)
    stat_j_full <- v_stats_full[j]
    
    h_dist <- ggplot(df_stats, aes_string(x = stat_j )) + 
      geom_histogram( bins = n_bins, colour = "black", fill='black') + #  # ..density.. breaks = seq(-1, 1, by = 10), aes_string(y =rescaled_hist_density, colour = cat_a)
      labs( y= paste0( 'Frequency'), 
            x= paste0(  stat_j_full    ) ) + 
      theme_classic() +
      scale_x_continuous(limits = l_all[[j]][[1]][[i]] ) + # print the jth statistic of the ith variable
      hist_theme          
    # h_dist
    
    # remove out of bounds values - actually do in ggoplot
    # df_stats <- df_stats %>% mutate( !!as.symbol(stat_j) := ifelse( ( !!as.symbol(stat_j) > max_scale_r) , max_scale_r,  ifelse( ( !!as.symbol(var_1) < min_scale_r), min_scale_r, !!as.symbol(var_1)  ) ) )# recalibrate oob out of bounds values to max val for levelplot
    
    g_input <-  ggplot() +
      geom_tile(data = df_stats, aes_string(x = 'x', y = 'y', fill = stat_j)) + # add the raster data
      geom_sf(data = land_shapefile) + # , color = 'black', size =2) + # add the shapefile (place the base layer first)
      labs( x='', y='', # x= 'latitude', y = 'longitude', 
            fill = paste0( stat_j_full ),  title = paste0( var_i_full) ) + #, '_', gsub('_',' ', stat_j) ) ) + # label axes and title
      coord_sf() + # align the coordinates to those of shapefile?
      # coord_equal() + # different crs
      # discrete_fill_viridis_c() + # alternative colour system
      scale_fill_distiller(limits= l_all[[j]][[2]][[i]] , palette =  'Spectral', na.value = "white", oob=scales::squish) + # set the colour scheme and palette # direction = 1,
      # scale_fill_distiller(limits=c(-1, 1), palette =  'YlGn', direction = 1,    
      #                      trans = "log",  breaks = scales::trans_breaks('log10', function(x) round(10 ^ x, digits=1), n=3)    ) + # , guide = guide_colorbar(frame.colour = "black")
      # theme_void() +
      theme_fig  
    
    # g_input
    
    # save map
    ggsave(plot = g_input, filename = paste0(output_path, 'g_', var_i ,'_', stat_j, '.png' ) ) # , width = wid, height = hei)
    
    # combine with hist
    g_comb <- ggarrange(g_input, h_dist,
                             # labels = c("A", "B"),
                             heights = c(3,1),          # Set g bigger than h
                             widths  = 1, #c(1,0.1),
                             # align = "v",
                             ncol = 1, nrow = 2)
    # g_comb
    
    ggsave(plot = g_comb, filename = paste0(output_path, 'comb_', var_i ,'_', stat_j, '.png' ) ) # , width = wid, height = hei)
    
    } # end stat loop

} # end variable loop

###################################################
######       visualise statistics             #####
###################################################

# load( paste0(input_dir, 'df_all_muTACcov.RData'))
# head(df_kndviclim_muTACcov) ; dim(df_kndviclim_muTACcov) ; summary(df_kndviclim_muTACcov)
# 
# var_i <- 't2m'
# load( paste0(input_dir, 'df_', var_i, '_muTACcov.RData')) ; 
# head(df_stats) ; dim(df_stats) ; summary(df_stats)
