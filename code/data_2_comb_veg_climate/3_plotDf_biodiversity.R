# ########################################################
# Title         : plotDF_kndviclim_muTACcov.R
# Description   : plot the df of X | Y | biodiversity variables
#                 for each variable at each point in Marco's tif
#                 
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
script_info_specific <- 'plotDF_biodiversity'               # used in output name
script_info_general <- 'plotDF_kndviclim_muTACcov'               # used in output name
script_info_input <- 'createDF_kndviclim_muTACcov'       # input data dir 
# '2023-01-12' <- 0.05 version before git (theoretically same as one after)
#input_script_date <- '2023-01-26'                        # the 0.05 production after creating the GIT (first kndvi version)
#input_script_date <- '2023-03-05'                        # the 0.05 production with second kndvi version
# input_script_date <- '2023-03-14_diversity1st'                         # the 0.05 production with second kndvi version and heterogeneity data
input_script_date <- '2023-04-17'                         # EGU

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
# input_dir <- 'data_processing/createDF_kndviclim_muTACcov/createDF_kndviclim_muTACcov_2023-01-12/' #
input_dir <- paste0(root_data_proce, script_info_input, '/', script_info_input, '_', input_script_date,  '/')

# set/create output directory
output_path <- paste0(root_figures, script_info_general, '/', script_info_specific, '_', full_date,  '/')
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
# probably better to set as universal values rather than dynamically coding them

# print Marco diversity metrics
load( paste0(input_dir, 'df_diversity_muTACcov.RData'  ) )
# head(df_stats) ; dim(df_stats) ; summary(df_stats) ; hist(df_stats$fhd_avg)
v_variables      <- c('skew_avg'        ,  'kurt_avg'        ,   'rh50_avg'  , 'fhd_avg'                     , 'rh98_avg'    , 'skew_cv'    , 'kurt_cv'    , 'rh50_cv'  , 'fhd_cv')
v_variables_full <- c('average skewness',  'average kurtosis', 'average RH50', 'avg foliage height diversity', 'average RH98', 'CV skewness', 'CV kurtosis', 'CV RH50'  , 'CV foliage height diversity')
# v_stats <- c('mu_var', 'sd_var', 'cv_var', 'tac_resid') # v_stats_full <- c('mean', 'std. dev.', 'CoV', 'residual TAC')
# set limits for each var in hist and map - each column is the limits for the corresponding variable # new limits with rescaled kndvi
#                     skew_avg      kurt_avg     rh50_avg     fhd_avg     rh98_avg     skew_cv        kurt_cv          rh50_cv      fhd_cv  
l_mu_hist  <- list( c(-2.5, 2.5 ) , c(-2, 2)  , c(0, 20)   , c(0, 4)     , c(0, 1)   , c(-5, 5)     , c(-4,4)     , c(0, 2)   ,     c(0, 0.4) )
l_mu_map   <- list( c(-0.8, 0.8 ) , c(-1, 1)  , c(0, 15)   , c(2.2, 3.2) , c(0, 0.5) , c(-2.5, 2.5) , c(-3,1)     , c(0, 1.5) ,     c(0, 0.2) )             
l_all <- list(l_mu_hist, l_mu_map) 

# print dissimilarity metrics
load( paste0(input_dir, 'df_glcmdiversity_muTACcov.RData'  ) )
# head(df_stats) ; dim(df_stats) ; summary(df_stats) ; hist(df_stats$fhd_avg)
v_variables      <- c('EVI_dissimilarity' ,  'EVI_homogeneity' ,   'biomass_dissimilarity'  , 'biomass_homogeneity', 'kndvi_dissimilarity', 'kndvi_homogeneity' )
v_variables_full <- c('EVI dissimilarity' ,  'EVI homogeneity' ,   'biomass dissimilarity'  , 'biomass homogeneity', 'kndvi dissimilarity', 'kndvi homogeneity')
# v_stats <- c('mu_var', 'sd_var', 'cv_var', 'tac_resid') # v_stats_full <- c('mean', 'std. dev.', 'CoV', 'residual TAC')
# set limits for each var in hist and map - each column is the limits for the corresponding variable # new limits with rescaled kndvi
#               EVI_dissimila      EVI_homog     biom_dissimila     biom_hom     kndvi_dissimilarity   kndvi_homog
l_mu_hist  <- list( c(0, 12 )   ,   c(0, 1)    , c(0, 10)   , c(0, 1)        , c(0, 17)   , c(0, 1))
l_mu_map   <- list( c(1, 5 )    ,   c(0, 0.7)  , c(0, 5)    , c(0, 0.7)      , c(0, 8)    , c(0, 1))             
l_all <- list(l_mu_hist, l_mu_map) 

#######################################
##### CREATE FIGURES              #####
#######################################
# this loops over the variables listed above and plots each of the variables as map and histogram

for (i in 5:length(v_variables)){
  var_i <- v_variables[i] ; print(var_i)
  var_i_full <- v_variables_full[i] 
  # head(ls()[ls()=='df_i'])
  
  # load( paste0(input_dir, 'df_', file_i_type ,'_full.RData'  ) )
  # load( paste0(input_dir, 'df_', var_i, '_muTACcov.RData'  ) )
  
    lims_h_i <- l_all[[1]][[i]]
    h_dist <- make_hist(df_stats, var_i, var_i_full, lims_h_i)
    # h_dist
    # ggsave(plot = h_dist, filename = paste0(output_path, 'h_', var_i ,'_', stat_j, '.png' ) ) # , width = wid, height = hei)
    
    lims_m_i <- l_all[[2]][[i]]
    g_input <- make_map(df_stats, var_i, var_i_full, var_i_full, lims_m_i)

    # save map
    ggsave(plot = g_input, filename = paste0(output_path, 'g_diversity_', var_i ,'_', '.png' ) ) # , width = wid, height = hei)
    
    # combine with hist
    # adjust the positioning to make sure figures fit
    g_draw <- ggdraw( clip = 'off') +
      # draw_plot(g_input, x = 0, y = 0.4, width = 0.6, height = 0.6) +
      # draw_plot(g_input, x = 0, y = 0.23, width = 0.82, height = 0.82, hjust = 0.3) +
      # draw_plot(h_dist , x = 0, y = 0,    width = 0.345, height = 0.24, hjust = 0 ) 
      draw_plot(g_input, x = 0.23, y = 0.23, width = 0.77, height = 0.77, hjust = 0.3) +
      draw_plot(h_dist , x = 0, y = 0,    width = 0.77, height = 0.24, hjust = 0 ) 
    # g_draw
    
    ggsave(plot = g_draw, filename = paste0(output_path, 'g_comb_diversity_', var_i , '.png' ) , width = 8, height =10 ) # , width = wid, height = hei)
    

} # end variable loop


############################################################################################################################################################################################################

###################################################
######       visualise statistics             #####
###################################################

# load( paste0(input_dir, 'df_all_muTACcov.RData'))
# head(df_kndviclim_muTACcov) ; dim(df_kndviclim_muTACcov) ; summary(df_kndviclim_muTACcov)
# 
# var_i <- 't2m'
# load( paste0(input_dir, 'df_', var_i, '_muTACcov.RData')) ; 
# head(df_stats) ; dim(df_stats) ; summary(df_stats)
#   
# h_dist <- ggplot(df_stats, aes_string(x = stat_j )) +
#   geom_histogram( bins = n_bins, colour = "black", fill='black') + #  # ..density.. breaks = seq(-1, 1, by = 10), aes_string(y =rescaled_hist_density, colour = cat_a)
#   labs( y= paste0( 'Frequency'), 
#         x= paste0(  stat_j_full    ) ) + 
#   theme_classic() +
#   scale_x_continuous(limits = l_all[[j]][[1]][[i]] ) + # print the jth statistic of the ith variable
#   basic_hist_theme    
# 
# g_input <-  ggplot() +
#   geom_tile(data = df_stats, aes_string(x = 'x', y = 'y', fill = stat_j)) + # add the raster data
#   geom_sf(data = land_shapefile) + # , color = 'black', size =2) + # add the shapefile (place the base layer first)
#   labs( x='', y='', # x= 'latitude', y = 'longitude', 
#         fill = paste0( stat_j_full ),  title = paste0( var_i_full) ) + #, '_', gsub('_',' ', stat_j) ) ) + # label axes and title
#   coord_sf() + # align the coordinates to those of shapefile?
#   # coord_equal() + # different crs
#   # discrete_fill_viridis_c() + # alternative colour system
#   scale_fill_distiller(limits= l_all[[j]][[2]][[i]] , palette =  'Spectral', na.value = "white", oob=scales::squish) + # set the colour scheme and palette # direction = 1,
#   # scale_fill_distiller(limits=c(-1, 1), palette =  'YlGn', direction = 1,    
#   #                      trans = "log",  breaks = scales::trans_breaks('log10', function(x) round(10 ^ x, digits=1), n=3)    ) + # , guide = guide_colorbar(frame.colour = "black")
#   # theme_void() +
#   basic_fig_theme  

  ###################################################
  ######       alt methods combine              #####
  ###################################################

# remove out of bounds values - actually do in ggoplot
# df_stats <- df_stats %>% mutate( !!as.symbol(stat_j) := ifelse( ( !!as.symbol(stat_j) > max_scale_r) , max_scale_r,  ifelse( ( !!as.symbol(var_1) < min_scale_r), min_scale_r, !!as.symbol(var_1)  ) ) )# recalibrate oob out of bounds values to max val for levelplot

  # g_comb <- ggarrange(g_input, h_dist, # labels = c("A", "B"),
  #                     heights = c(3,1), widths  = 1, #c(1,0.1), # Set g bigger than h # align = "v",
  #                     ncol = 1, nrow = 2)
  # # g_comb
  # # ggsave(plot = g_comb, filename = paste0(output_path, 'comb_', var_i ,'_', stat_j, '.png' ) ) # , width = wid, height = hei)
  # 
  # 
  # # combin hist alt - https://cran.r-project.org/web/packages/egg/vignettes/Ecosystem.html
  # g_grid  <- grid.arrange( grobs = list(g_input, h_dist) , 
  #                          ncol = 1, nrow =2, heights = c(2,1) , widths = 1 )
  # g_grid_2 <- grid.arrange(g_input, h_dist, nrow=1, widths = c(1/9, 1/3))
  # 
  # ggsave(plot = g_grid, filename = paste0(output_path, 'test_comb_', var_i ,'_', stat_j, '.png' ) ) # , width = wid, height = hei)
  # 
  # g_cow <- plot_grid(g_input, h_dist,  align = "h", nrow = 2 , ncol = 1, rel_heights = c(1, 1/3)) # 
  # 
  # ggsave(plot = g_cow, filename = paste0(output_path, 'test_comb_cow_wid', var_i ,'_', stat_j, '.png' ) ) # , width = wid, height = hei)
  # 
  # g_cow <- cowplot::align_plots(g_input, h_dist,  align = "h", nrow = 2 , ncol = 1 ) # rel_heights = c(1, 1/2)
  # 
  # setitng equal widths
  # gA <- ggplotGrob(g_input)
  # gA$widths
  # gB <- ggplotGrob(h_dist)
  # gB$widths <- gB$widths/2
  # ggplotGrob(h_dist) <-  gB
  # g_cow <- plot_grid(gA, gB,  align = "h", nrow = 2 , ncol = 1 ) # rel_heights = c(1, 1/2)
  # 