# ########################################################
# Title         : 4_phenology_mask.R
# Description   : generate a 46 columns dataframe/tif with binary masks for growing season for each 8 days timestep (0/1)
# Aims          : growing season mask over 8 days timesteps
# Inputs	      : dataframes of median greenup and dormancy doy
# Outputs	      : 46 fields columns dataframe/tif with masks for growing season
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
script_info <- 'phenology_mask'            # used in output name
script_info_input <- 'createDF_phenology_stats'
input_script_date <- '2023-03-21' 

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
library(parallel)

#######################################################
######       SET UP FOR PARALLEL PROCESSING       #####
#######################################################

# returns the number of logical processors
maxcores <- parallel::detectCores()
cl <- parallel::makeCluster(maxcores/2)

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
######       GLOBAL VARS                      #####
###################################################

# set global dates and timesteps
timeint <- 8
tstep <- 46
somc <- seq(1, 365, timeint)
eomc <- seq(8, 365, timeint)
eomc <- c(eomc, 365) # adjust for last timestep which is of only 5 days
ths_gs <- 0.75 # threshold for growing season selection

# load dataframes of greenup and dormancy
load(paste0(input_dir,'greenup_doy_pos_median_mode.RData'))
load(paste0(input_dir, 'dormancy_doy_pos_median_mode.RData'))

# create greenup (sos) and dormancy (eos) median dataframes
sos <- greenup_df[c(1, 2, 3)]
eos <- dormancy_df[c(1, 2, 3)]

# create an empty dataframe/matrix with as many rows as sos/eos and 46 columns
# this will hold for each pixels 46 columns with percentages of days between sos and eos for each 8 days timestep at that pixel
vperc <- data.frame(matrix(nrow = nrow(sos), ncol = 46)) 

# loop through each pixel sos and eos only where sos and eos are not NA
for (i in 1:nrow(sos)){
  
  # select sos and eos
  sosi <- sos[i, 3]
  eosi <- eos[i, 3]
  
  if (!is.na(sosi) & !is.na(eosi)){
  
    # create an empty vector of number of days, filled with 0
    varday <- rep(0, 365)
    # create an empty vector of timesteps to host percentages of days between sos and eos, filled with NA
    varday_perc <- rep(NA, tstep)
    
    # check if phenology is cross year or not and set as 1 days in phenology in empty vector of number of days
    if (sosi < eosi) {
      # set intra-annual growing season
      varday[sosi:eosi] <- 1
    } else {
      # set cross-year growing season
      varday[1:eosi] <- 1
      varday[sosi:365] <- 1
    }
    
    # loop through timesteps (46) and calculate for each timesteps the % of days between sos and eos
    for (j in 1:tstep) {
      varday_perc[j] <- sum(varday[somc[j]:eomc[j]]) / (1 + eomc[j] - somc[j])
    }
    
    # fill the previously created empty dataframe/matrix with as many rows as sos/eos and 46 columns 
    vperc[i,] <- varday_perc
  }
  else {vperc[i,] <- NA}
}

# save dataframe of percentages
save(vperc, file=paste0(output_path, 'vperc.RData'))

# create masked dataframe (0-1)
vperc[vperc<ths_gs] <- 0
vperc[vperc>=ths_gs] <- 1

# save dataframe of percentages
save(vperc, file=paste0(output_path, 'vperc_mask.RData'))

# take coordinates
xy <- greenup_df[c(1, 2)]

# create a dataframe with x,y and the 46 columns from the matrix
phenology_df <- cbind(xy, vperc) 
names(phenology_df)[3:length(names(phenology_df))] <- 1:46

# save dataframe of phenology mask
save(phenology_df, file=paste0(output_path, 'phenology_df.RData'))