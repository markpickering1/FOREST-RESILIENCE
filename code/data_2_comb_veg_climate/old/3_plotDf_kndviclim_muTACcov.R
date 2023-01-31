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

######     GLOBAL VARS                        #####
script_info <- 'plotDF_kndviclim_muTACcov'               # used in output name
script_info_input <- 'createDF_kndviclim_muTACcov'       # input data dir 
# '2023-01-12' <- 0.05 version before git (theoretically same as one after)
input_script_date <- '2023-01-26'                        # the 0.05 production after creating the GIT (first kndvi version)


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
library(ggExtra)      # more complex figures, marginal hists
library(grid)       # for making multi-gird plots and tables
library(gridExtra)  # for making multi-gird plots and tables
library(lattice)  # for making multi-gird plots and tables
# library(ggplotify)  # plots within levelplots
library(rgdal)        # 
library(RColorBrewer) # colour palettes
library(sf)           # utilise shapefiles etc

###################################################
######       SET THEME COMMON PLOTTING VARS   #####
###################################################

source(path_figures_init)

###################################################
######       I/O                              #####
###################################################


# initialise input file
# input_dir <- 'data_processing/createDF_kndviclim_muTACcov/createDF_kndviclim_muTACcov_2023-01-12/' #
input_dir <- paste0(root_data_proce, script_info_input, '/', script_info_input, '_', input_script_date,  '/')

# set/create output directory
output_path <- paste0(root_figures, script_info, '/', script_info, '_', full_date,  '/')
print(paste0('output_path is : ', output_path ))
if(! dir.exists(output_path)) {dir.create(paste0(output_path),recursive=T)} # create directory if not present


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


#######################################
##### CREATE FIGURES              #####
#######################################
# this loops over the variables listed above and plots each of the variables as map and histogram

for (i in 1:length(v_variables)){
for (i in 1:3){
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
      basic_hist_theme          
    # h_dist
    # ggsave(plot = h_dist, filename = paste0(output_path, 'h_', var_i ,'_', stat_j, '.png' ) ) # , width = wid, height = hei)
    
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
      basic_fig_theme  
    
    # g_input
    
    # save map
    ggsave(plot = g_input, filename = paste0(output_path, 'g_', var_i ,'_', stat_j, '.png' ) ) # , width = wid, height = hei)
    
    # combine with hist
    g_comb <- ggarrange(g_input, h_dist, # labels = c("A", "B"),
                             heights = c(3,1), widths  = 1, #c(1,0.1), # Set g bigger than h # align = "v",
                             ncol = 1, nrow = 2)
    # g_comb
    # ggsave(plot = g_comb, filename = paste0(output_path, 'comb_', var_i ,'_', stat_j, '.png' ) ) # , width = wid, height = hei)
    
    
    # combin hist alt - https://cran.r-project.org/web/packages/egg/vignettes/Ecosystem.html
    g_grid <- grid.arrange( grobs = list(g_input, h_dist) , 
                            ncol = 1, nrow =2, heights = c(2,1) , widths = 1 )
    ggsave(plot = g_grid, filename = paste0(output_path, 'test_comb_', var_i ,'_', stat_j, '.png' ) ) # , width = wid, height = hei)
    
    
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
