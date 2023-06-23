# ########################################################
# Title         : plot_na.R
# Description   : Plot the na values in given dataframes
# Aims          : Inspect where the na values are distributed
# Inputs	      : Dataframe
# Outputs	      : ggplot
# Options	      : 
# Date          : 31/01/2023
# Version       : 1.1
# Authors       : Mark Pickering & Agata Elia
# Maintainer    : Agata Elia 
# Notes		      : 
# Example use   : 
# ########################################################

# Import libraries
library(chron)   # useful for converting time values
library(dplyr)   # use %>%
library(ncdf4)   # read ncdf
library(reshape) # reshaping dataframes
library(raster)  # package for raster manipulation
# library(rasterVis)  # enables levelplots
require(ggplot2)      # for plotting
library(ggpubr)       # for arranging ggplots together
library(ggExtra)      # more complex figures, marginal hists
# library(grid)       # for making multi-gird plots and tables 
# library(gridExtra)  # for making multi-gird plots and tables 
# library(ggplotify)  # plots within levelplots
library(rgdal)        # 
library(RColorBrewer) # colour palettes
library(sf)  # utilise shapefiles etc
library(cowplot)

# Remove existing loaded objects
rm(list = ls())   

# Set working directory
setwd('/eos/jeodpp/data/projects/FOREST-RESILIENCE/GIT-FOREST-RESILIENCE/FOREST-RESILIENCE/sandbox/agata/')                        

# Load RData object
load('df_all_muTACcov.RData')

# Retain only missing values for kndvi and turn them into 1
df_kndvi_missing <- df_kndvi[is.na(df_kndvi$mu_var_kndvi),]
df_kndvi_missing[is.na(df_kndvi_missing$mu_var_kndvi), 4] <- 1

# Retain only missing values for any clim var and turn them into 1
df_spei_missing <- df_kndvi[is.na(df_kndvi$mu_var_spei),]
df_spei_missing[is.na(df_spei_missing$mu_var_spei), 9] <- 1

###################################################
######       PLOTTING PARAMETERS             #####
###################################################

# Load and process shapefile
land_shapefile <- '/eos/jeodpp/data/projects/FOREST-RESILIENCE/data/ancillary/world_vectors/50m_coastline/ne_50m_coastline.shp' 
land_shapefile <- sf::st_read(land_shapefile, quiet = TRUE)                        

# Load bounding box from shapefile (has coords: -10.66164 34.56369 44.82037 71.18416)
bb_shapefile <- '/eos/jeodpp/data/projects/FOREST-RESILIENCE/data/ancillary/world_vectors/boundingBoxes/Europe_BB.shp'
bboxpolygon <- sf::st_read(bb_shapefile, quiet = TRUE)                        

# Crop land_shapefile
land_shapefile <- sf::st_crop(land_shapefile, bboxpolygon) 

text_size = 8                                   
legend_size <- 1
         
# maps
# set plotting parameters
z_lims <- c(0, 1) # z_lims <-  c(0, 0.5)
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

################################################
######       CREATE MAPS                   #####
################################################

# Set figure title
figure_title <- 'Distribution of missing values in kNDVI mu where clim values are present'

# Plot the missing values
g_input <-  ggplot() +
  geom_tile(data = df_kndvi_missing, aes_string(x = 'x', y = 'y', fill = 'mu_var_kndvi')) + # add the raster data
  geom_sf(data = land_shapefile) + # , color = 'black', size =2) + # add the shapefile (place the base layer first)
  labs( x='', y='', # x= 'latitude', y = 'longitude', 
        fill = "kndvi missing",  title = paste0( figure_title) ) + # label axes and title
  coord_sf() + # align the coordinates to those of shapefile?
  # coord_equal() + # different crs
  # discrete_fill_viridis_c() + # alternative colour system
  #scale_fill_distiller(limits= z_lims , palette =  'Spectral', na.value = "white") + # set the colour scheme and palette # direction = 1,
  # scale_fill_distiller(limits=c(-1, 1), palette =  'YlGn', direction = 1,    
  #                      trans = "log",  breaks = scales::trans_breaks('log10', function(x) round(10 ^ x, digits=1), n=3)    ) + # , guide = guide_colorbar(frame.colour = "black")
  # theme_void() +
  theme_fig

# Save figure
ggsave(plot = g_input, filename = paste0('kndvi_missing_values.png'), width = 10, height = 5)

# Set figure title
figure_title <- 'Distribution of missing values in SPEI mu where kNDVI values are present'

# Plot the missing values
g_input <-  ggplot() +
  geom_tile(data = df_spei_missing, aes_string(x = 'x', y = 'y', fill = 'mu_var_spei')) + # add the raster data
  geom_sf(data = land_shapefile) + # , color = 'black', size =2) + # add the shapefile (place the base layer first)
  labs( x='', y='', # x= 'latitude', y = 'longitude', 
        fill = "spei missing",  title = paste0( figure_title) ) + # label axes and title
  coord_sf() + # align the coordinates to those of shapefile?
  # coord_equal() + # different crs
  # discrete_fill_viridis_c() + # alternative colour system
  #scale_fill_distiller(limits= z_lims , palette =  'Spectral', na.value = "white") + # set the colour scheme and palette # direction = 1,
  # scale_fill_distiller(limits=c(-1, 1), palette =  'YlGn', direction = 1,    
  #                      trans = "log",  breaks = scales::trans_breaks('log10', function(x) round(10 ^ x, digits=1), n=3)    ) + # , guide = guide_colorbar(frame.colour = "black")
  # theme_void() +
  theme_fig

# Save figure
ggsave(plot = g_input, filename = paste0('spei_missing_values.png'), width = 10, height = 5)
