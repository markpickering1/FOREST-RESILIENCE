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
# Version       : 2 (26/01/23)
# Authors       : Mark Pickering & Agata Elia
# Maintainer    : Mark Pickering 
# Notes		      : 
# Example use   : 
# ########################################################

###################################################
######     INITIALISE                         #####
###################################################
rm(list = ls())                                   # remove loaded objects

# initialise R and plotting environments
source('/eos/jeodpp/data/projects/FOREST-RESILIENCE/GIT-FOREST-RESILIENCE/FOREST-RESILIENCE/code/main/initialise_R.R')

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

# initialise input file 
input_dir <- 'GIT-FOREST-RESILIENCE/FOREST-RESILIENCE/sandbox/agata/kndvi_sigma/sigma_df/'

# set/create output directory
output_path <- 'GIT-FOREST-RESILIENCE/FOREST-RESILIENCE/sandbox/agata/kndvi_sigma/sigma_plots_v2/'
print(paste0('output_path is : ', output_path ))
if(! dir.exists(output_path)) {dir.create(paste0(output_path),recursive=T)} # create directory if not present

#######################################
##### INITIALISE VARIABLES        #####
#######################################
# set the hist and map min/max

v_variables <- c('kndvi_simplified', 'kndvi_fixed')
v_variables_full <- c('kndvi simplified sigma', 'kndvi fixed sigma 0.15')
v_stats <- c('kndvi')
v_stats_full <- c('kndvi')
# set limits for each var in hist and map - each column is the limits for the corresponding variable # new limits with rescaled kndvi
#             kndvi simplified  kndvi fixed         
l_mu_hist  <- list( c(-1, 1)    , c(-1, 1))
l_mu_map   <- list( c(-1, 1)    , c(-1, 1))          

l_all <- list(l_mu_hist, l_mu_map)

#######################################
##### CREATE FIGURES              #####
#######################################
# this loops over the variables listed above and plots each of the variables as map and histogram
print('plotting')

for (i in 1:length(v_variables)){
  var_i <- v_variables[i] ; print(var_i)
  var_i_full <- v_variables_full[i] 

  load( paste0(input_dir, 'df_', var_i, '_baseVar_full.RData'  ) )
  
    # make histogram
    lims_h_i <- c(0, 0.6)
    h_dist <- make_hist(df_var, v_stats, v_stats_full, lims_h_i)
    
    # make map
    lims_m_i <- c(0.3, 0.5)
    g_input <- make_map(df_var, v_stats, v_stats_full, var_i_full, lims_m_i)
    
    # save map
    ggsave(plot = g_input, filename = paste0(output_path, 'g_', var_i ,'_', v_stats, '.png' ) ) # , width = wid, height = hei)
    
    # combine with hist
    # adjust the positioning to make sure figures fit
    g_draw <- ggdraw( clip = 'off') +
      # draw_plot(g_input, x = 0, y = 0.4, width = 0.6, height = 0.6) +
      # draw_plot(g_input, x = 0, y = 0.23, width = 0.82, height = 0.82, hjust = 0.3) +
      # draw_plot(h_dist , x = 0, y = 0,    width = 0.345, height = 0.24, hjust = 0 )
      draw_plot(g_input, x = 0.23, y = 0.23, width = 0.77, height = 0.77, hjust = 0.3) +
      draw_plot(h_dist , x = 0, y = 0,    width = 0.77, height = 0.24, hjust = 0 ) 
    # g_draw
    
    ggsave(plot = g_draw, filename = paste0(output_path, 'g_comb_', var_i ,'_', v_stats, '.png' ) , width = 8, height =10 ) # , width = wid, height = hei)
    
    } # end stat loop

#######################################################
######       CREATE 2D HIST TCI kNDVI std dev     #####
#######################################################

load( paste0(input_dir, 'df_joined.RData'  ) )

n_bins <- 100
f <- as.formula(paste( 'y', paste('x'), sep = " ~ ")) # formula for line of best fit
rf <- colorRampPalette(rev(brewer.pal(11,'Spectral'))) ;  r <- rf(32) #set palette


g_2dhist <- ggplot(df_input, aes_string(x = 'kndvi_simplified', y = 'kndvi_fixed')) +
  stat_bin2d(bins= c(n_bins,n_bins) , aes(fill = ..ndensity..), na.rm = TRUE) +
  geom_smooth(data = df_input, method = "lm", linetype = "dashed", size = 1.5, color="black", show.legend = TRUE, formula = f,mapping = aes_string('kndvi_simplified' , y = 'kndvi_fixed' ) ) + #, mapping = aes(weight = SIFPK_mean) ) + # size = 0.1,
  scale_fill_gradientn(colors = r, name = "Relative distribution  ", limits=c(0, 1), breaks=seq(0,1,0.5)) + #, trans = "log",limits=c(1, 10000), breaks=c(1,3,10,30,100,300,1000,3000,10000) ) + # 
  # xlim(lim_lower[1], lim_upper[1]) + ylim(lim_lower[2],lim_upper[2]) + 
  theme(
    panel.grid = element_line(color = "grey50"), # "transparent" set the colour of the grid lines
    panel.background = element_blank(), # set the colour of the panel behind the figure
    text = element_text(size=text_size), # rel(
    strip.text.x = element_text(size=text_size),
    strip.text.y = element_text(size=text_size),
    legend.position="bottom", #, 
    legend.text=element_text(size=text_size),
    legend.key.size = unit(legend_size, "cm"),
    legend.key.width = unit(legend_size, "cm")) 
#labs(title= paste0( gsub('_',' ', x_var_1), ' Vs ', gsub('_',' ', y_var),' per pixel for first GS of year')) + # add a title on the plot
# labs(#y= paste0( gsub('_',' ', y_var),' [gC m-2 day-1]'),
#   y= paste0('Mean FLUXCOM ', y_var_1_name, ' over growing season ', y_var_1_units),
#   x= paste0('Mean downscaled ', x_var_1_name, ' over growing season ', x_var_1_units) )

ggsave(plot = g_2dhist, filename = paste0(output_path, 'scatter_plot.png'), width = 5, height = 5)

