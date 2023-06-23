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
library(patchwork)

source(path_figures_init)

input_dir <- "/eos/jeodpp/data/projects/FOREST-RESILIENCE/GIT-FOREST-RESILIENCE/FOREST-RESILIENCE/sandbox/agata/foss4g/"

#load data
load(paste0(input_dir, 'df_kndvi_baseVar_TAC.RData'))  

# make map 
lims_m_i <-c(-1, 1)
g_input_1 <- make_map(df_stats_tac, "tac", "TAC", "Map of kNDVI TAC from untreated kNDVI timeseries", lims_m_i)

# save map 
ggsave(plot = g_input_1, filename = paste0(input_dir, 'df_kndvi_baseVar_TAC.png')) #, width = 10, height = 20)

#load data
load(paste0(input_dir, 'df_kndvi_deseas_TAC.RData'))  

# make map 
lims_m_i <-c(0, 0.6)
g_input_2 <- make_map(df_stats, "tac_resid", "TAC", "Map of kNDVI TAC from pre-processed kNDVI timeseries", lims_m_i)

# save map 
ggsave(plot = g_input_2, filename = paste0(input_dir, 'df_kndvi_deseas_TAC.png')) #, width = 10, height = 20)

#load data
load(paste0(input_dir, 'df_RFresiduals_TAC_500trees.RData'))  

# make hist
lims_h_i <- c(-1, 1)
h_dist <- make_hist(df_residuals, "residual", "TAC", lims_h_i)

# make map 
lims_m_i <-c(-0.025, 0.025)
g_input_3 <- make_map(df_residuals, "residual", "TAC", "Map of kNDVI TAC residuals from RF model", lims_m_i)

# save map 
ggsave(plot = g_input_3, filename = paste0(input_dir, 'df_RFresiduals_TAC_500trees.png')) #, width = 10, height = 20)

# make map 
lims_m_i <-c(0, 0.6)
g_input_4 <- make_map(df_residuals, "predicted", "TAC", "Map of kNDVI TAC predicted by RF model", lims_m_i)

# save map 
ggsave(plot = g_input_4, filename = paste0(input_dir, 'df_RFpredictions_TAC_500trees.png')) #, width = 10, height = 20)

# combine
g_comb <- g_input_1 / g_input_2 
g_comb2 <- g_input_4 / g_input_3 