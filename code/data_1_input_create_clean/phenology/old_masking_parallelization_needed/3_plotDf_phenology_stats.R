# ########################################################
# Title         : 3_plotDf_phenology_stats.R
# Description   : plot stats of greenup and dormancy
# Aims          : plots
# Inputs	      : one dataframe for summarised greenup and one for summarised dormancy
# Outputs	      : plots
# Options	      : 
# Date          : 20/03/23
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

source('/eos/jeodpp/data/projects/FOREST-RESILIENCE/GIT-FOREST-RESILIENCE/FOREST-RESILIENCE/code/main/initialise_R.R')

######     GLOBAL VARS                        #####
script_info <- 'plotDF_phenology_stats'            # used in output name
script_info_input <- 'createDF_phenology_stats'
input_script_date <- '2023-03-21' 

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

###################################################
######       SET THEME COMMON PLOTTING VARS   #####
###################################################

source(path_figures_init)

###################################################
######       I/O                              #####
###################################################
# this contains the hardcoded extensions of the individual datasets

# initialise input directory
input_dir <- paste0(root_data_proce, script_info_input, '/', script_info_input, '_', input_script_date,  '/')

# set/create output directory
output_path <- paste0(root_figures, script_info, '/', script_info, '_', full_date,  '/')
print(paste0('output_path is : ', output_path ))
if(! dir.exists(output_path)) {dir.create(paste0(output_path),recursive=T)} # create directory if not present

#######################################
##### INITIALISE VARIABLES        #####
#######################################

# set the hist and map min/max
v_variables <- c('greenup', 'dormancy')
v_variables_full <- c('first greenup DOY', 'first dormancy DOY')
v_stats <- c('median', 'mode')
v_stats_full <- c('median', 'mode')

# set limits for each var in hist and map - each column is the limits for the corresponding variable # new limits with rescaled kndvi
#                       greenup   dormancy       
l_median_hist  <- list(c(1, 365), c(1, 365))
l_median_map   <- list(c(1, 365), c(1, 365))
l_mode_hist  <- list(c(1, 365), c(1, 365))
l_mode_map   <- list(c(1, 365), c(1, 365))
l_all <- list(list(l_median_hist, l_median_map), list(l_mode_hist, l_mode_map))

#######################################
##### CREATE FIGURES              #####
#######################################

# this loops over the variables listed above and plots each of the variables as map and histogram
for (i in 1:length(v_variables)){
  var_i <- v_variables[i] ; print(var_i)
  var_i_full <- v_variables_full[i] 

  load(paste0(input_dir, var_i, '_doy_pos_median_mode.RData'))
  
  for(j in 1:length(v_stats)){
    stat_j <- v_stats[j] ; print(stat_j)
    stat_j_full <- v_stats_full[j]

    if(var_i=='greenup'){
    lims_h_i <- l_all[[j]][[1]][[i]]
    h_dist <- make_hist(greenup_df, stat_j, stat_j_full, lims_h_i)

    lims_m_i <- l_all[[j]][[2]][[i]]
    g_input <- make_map(greenup_df, stat_j, stat_j_full, lims_m_i)
    }
      
    else{
      lims_h_i <- l_all[[j]][[1]][[i]]
      h_dist <- make_hist(dormancy_df, stat_j, stat_j_full, lims_h_i)
      
      lims_m_i <- l_all[[j]][[2]][[i]]
      g_input <- make_map(dormancy_df, stat_j, stat_j_full, lims_m_i)
    }  
    
    # save map
    ggsave(plot = g_input, filename = paste0(output_path, 'g_', var_i ,'_', stat_j, '.png' ) ) # , width = wid, height = hei)
    
    # combine with hist
    # adjust the positioning to make sure figures fit
    g_draw <- ggdraw( clip = 'off') +
      # draw_plot(g_input, x = 0, y = 0.4, width = 0.6, height = 0.6) +
      # draw_plot(g_input, x = 0, y = 0.23, width = 0.82, height = 0.82, hjust = 0.3) +
      # draw_plot(h_dist , x = 0, y = 0,    width = 0.345, height = 0.24, hjust = 0 )
      draw_plot(g_input, x = 0.23, y = 0.23, width = 0.77, height = 0.77, hjust = 0.3) +
      draw_plot(h_dist , x = 0, y = 0,    width = 0.77, height = 0.24, hjust = 0 ) 
    # g_draw
    
    ggsave(plot = g_draw, filename = paste0(output_path, 'g_comb_', var_i ,'_', stat_j, '.png' ) , width = 8, height =10 ) # , width = wid, height = hei)
    
  } # end stat loop
  
} # end variable loop
