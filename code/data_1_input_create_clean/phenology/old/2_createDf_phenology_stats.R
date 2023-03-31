# ########################################################
# Title         : 2_createDf_phenology_stats.R
# Description   : summarise yearly greenup and dormancy doy with a selected stat
# Aims          : stat of greenup and dormancy over years
# Inputs	      : dataframes of yearly greenup and dormancy
# Outputs	      : one dataframe for summarised greenup and one for summarised dormancy
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
script_info <- 'createDF_phenology_stats'            # used in output name
script_info_input <- 'createDF_phenology'
input_script_date <- '2023-03-20' 

######     SET LIBRARIES                      #####
library(chron)   # useful for converting time values
library(dplyr)   # use %>%
library(ncdf4)   # read ncdf
library(reshape) # reshaping dataframes
library(raster)  # package for raster manipulation
library(terra)   # terra library for raster manip
library(reshape) # melt dfs to wide-long
# library(rasterVis)  # enables levelplots
# require(ggplot2)      # for plotting
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
# this contains the hardcoded extensions of the individual datasets

# initialise input directory
input_dir <- paste0(root_data_proce, script_info_input, '/', script_info_input, '_', input_script_date,  '/')

# set/create output directory
output_path <- paste0(root_data_proce, script_info, '/', script_info, '_', full_date,  '/')
print(paste0('output_path is : ', output_path ))
if(! dir.exists(output_path)) {dir.create(paste0(output_path),recursive=T)} # create directory if not present

###################################################
######       extract statistics               #####
###################################################

# define mode function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# define variable
var_i <- 'greenup' ; print(var_i)

# load dataframe
load(paste0(input_dir, var_i, '_doy.RData'))

# read dataframe rowwise
#df_var <- greenup_df %>% rowwise(id=c("x","y"))
df_var <- greenup_df %>% rowwise(x, y)

# calculate desired statistics by rows
df_stats <- df_var %>% dplyr::summarise(
            median = median(c_across(greenup_2003:greenup_2021), na.rm = T)
            )

# save stats df  
save(df_stats, file=paste0(output_path, var_i, '_doy_median_mode.RData'))

# # melt data into long version and rename field
# df_var <- melt(greenup_df, id=c("x","y"), variable_name = "year")
# colnames(df_var)[4] <- var_i  
# 
# # group by and calculate desired statistic over doy
# df_stats <- df_var %>% dplyr::group_by( x, y ) %>%
#   dplyr::summarise(
#     median = median(!!as.symbol(var_i), na.rm = T),
#     mode = getmode(!!as.symbol(var_i), na.rm = T)
#   )
# 
# # save stats df  
# save(df_stats, file=paste0(output_path, var_i, '_doy_median_mode.RData'))
# 
# # remove loaded objects
# rm(list = ls())  
# 
# # define variable
# var_i <- 'dormancy' ; print(var_i)
# 
# # load dataframe
# load(paste0(input_dir, var_i, '_doy.RData'))
# 
# # melt data into long version and rename field
# df_var <- melt(dormancy_df, id=c("x","y"), variable_name = "year")
# colnames(df_var)[4] <- var_i  
# 
# # group by and calculate desired statistic over doy
# df_stats <- df_var %>% dplyr::group_by( x, y ) %>%
#   dplyr::summarise(
#     median = median(!!as.symbol(var_i), na.rm = T),
#     mode = getmode(!!as.symbol(var_i), na.rm = T)
#   )
# 
# # save stats df  
# save(df_stats, file=paste0(output_path, var_i, '_doy_median_mode.RData'))

# # define variables
# variables <- c('greenup', 'dormancy')
# 
# # loop through variables
# for (i in 1:length(variables)){
#   var_i <- variables[i] ; print(var_i)
#   load(paste0(input_dir, var_i, '_doy.RData'))
#   if(var_i=='greenup'){
#     # melt data into long version and rename field
#     df_var <- melt(greenup_df, id=c("x","y"), variable_name = "year")
#     colnames(df_var)[4] <- var_i  
#     # group by and calculate desired statistic over doy
#     df_stats <- df_var %>% dplyr::group_by( x, y ) %>%
#                 dplyr::summarise(
#                   median = median(!!as.symbol(var_i), na.rm = T),
#                   mode = getmode(!!as.symbol(var_i), na.rm = T)
#                 )}
#   else{
#     # melt data into long version and rename field
#     df_var <- melt(dormancy_df, id=c("x","y"), variable_name = "year")
#     colnames(df_var)[4] <- var_i  
#     # group by and calculate desired statistic over doy
#     df_stats <- df_var %>% dplyr::group_by( x, y ) %>%
#                 dplyr::summarise(
#                 median = median(!!as.symbol(var_i), na.rm = T),
#                 mode = getmode(!!as.symbol(var_i), na.rm = T)
#                )}
#   # save stats df  
#   save(df_stats, file=paste0(output_path, var_i, '_doy_median_mode.RData'))
# }