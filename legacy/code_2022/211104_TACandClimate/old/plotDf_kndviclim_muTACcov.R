# ########################################################
# Title         : plotDF_kndviclim_muTACcov.R
# Description   : plot the df of X | Y | mu | TAC | CoV 
#                 for each variable at each point in the ncdf files
#                 plot the df of mean, coeff of var, 1-lag TAC for each variable
#                 do some general visualisation
# Aims          : dataframes of all data and RF predictor data for each X/Y
# Inputs	      : df containing KNDVI and CLIMATE data 
# Outputs	      : plots/maps of variables
# Options	      : 
# Date          : 14/11/22
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
input_dir <- 'data_processing/createDF_kndviclim_muTACcov/createDF_kndviclim_muTACcov_2022-11-11/'

# set/create output directory
output_root <- paste0('data_processing/', output_dir_name)
output_path <- paste0(output_root, script_info, '_', full_date,  '/')
print(paste0('output_path is : ', output_path ))
if(! dir.exists(output_path)) {dir.create(paste0(output_path),recursive=T)} # create directory if not present

###################################################
######       visualise statistics             #####
###################################################

load( paste0(input_dir, 'df_all_muTACcov.RData'))
head(df_kndviclim_muTACcov) ; dim(df_kndviclim_muTACcov) ; summary(df_kndviclim_muTACcov)

var_i <- 't2m'
load( paste0(input_dir, 'df_', var_i, '_muTACcov.RData')) ; 
head(df_stats) ; dim(df_stats) ; summary(df_stats)


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

hist_theme <- theme(plot.margin = margin(0, 160, 0, 160),    # add left right margin so that the hist arranges better
                    text = element_text(size = text_size)
)           

#######################################
##### HISTOGRAMS                  #####
#######################################

if(var_i==  't2m'){
  s_stat <- 'cv_var' ; hist_lims <- c(0,200) # s_stat <- 'cv_calc'
  s_stat <- 'mu_var' ; hist_lims <- c(-20,2)
}
if(var_i==  't2m'){
  s_stat <- 'cv_var' ; hist_lims <- c(0,200) # s_stat <- 'cv_calc'
  s_stat <- 'mu_var' ; hist_lims <- c(-20,2)
  s_stat <- 'TAC' ; hist_lims <- c(0,1) ; z_lims <- c(0,0.8)
  
}


h_dist <- ggplot(df_stats, aes_string(x = s_stat )) + 
  geom_histogram( bins = 100, colour = "black", fill='black') + #  # ..density.. breaks = seq(-1, 1, by = 10), aes_string(y =rescaled_hist_density, colour = cat_a)
  labs( y= paste0( 'Frequency'), 
        x= paste0(  s_stat     ) ) + 
  theme_classic() +
  scale_x_continuous(limits = hist_lims ) +
  hist_theme          

h_dist

# s_stat <- 'mu_var'
# z_lims <- c(-20,2)
# plot the biomass relative to present
g_input <-  ggplot() +
  geom_tile(data = df_stats, aes_string(x = 'x', y = 'y', fill = s_stat)) + # add the raster data
  geom_sf(data = land_shapefile) + # , color = 'black', size =2) + # add the shapefile (place the base layer first)
  labs( x='', y='', # x= 'latitude', y = 'longitude', 
        fill = s_stat,  title = paste0( s_stat, ' ', var_i ) ) + # label axes and title
  coord_sf() + # align the coordinates to those of shapefile?
  # coord_equal() + # different crs
  # discrete_fill_viridis_c() + # alternative colour system
  scale_fill_distiller(limits= z_lims , palette =  'Spectral', na.value = "white") + # set the colour scheme and palette # direction = 1,
  # scale_fill_distiller(limits=c(-1, 1), palette =  'YlGn', direction = 1,    
  #                      trans = "log",  breaks = scales::trans_breaks('log10', function(x) round(10 ^ x, digits=1), n=3)    ) + # , guide = guide_colorbar(frame.colour = "black")
  # theme_void() +
  theme_fig  

g_input
ggsave(plot = g_input, filename = paste0(output_path, script_info, '_', var_i ,'_', s_stat, '.png' ) ) # , width = wid, height = hei)
