# ########################################################
# Title         : createDF_biodiveristy_biomass.R
# Description   : create a df of X | Y | n_neighbours | heterogeneity | dissimilarity 
#                 for the biomass dataset at 500m with a 4x4 moving window. Aggregate to 5km
#                 First apply Forest Cover mask
# Aims          : 
# Inputs	      : dfs containing KNDVI and CLIMATE data extracted from raster
# Outputs	      : separate and combined dfs of variables
# Options	      : 
# Date          : 15/04/23
# Version       : 1
# Authors       : Mark Pickering & Agata Elia
# Maintainer    : Mark Pickering 
# Notes		      : 
# Example use   : 
# ########################################################

###################################################
######     INITIALISE                         #####
###################################################
rm(list = ls())                                   # remove loaded objects

source('/eos/jeodpp/data/projects/FOREST-RESILIENCE/GIT-FOREST-RESILIENCE/FOREST-RESILIENCE/code/main/initialise_R.R')

######     GLOBAL VARS                        #####
script_info      <- 'createDF_biodiveristy_biomass'               # used in output name
# set moving window size and min number entries
i_mw_size <- 3          # earthenv uses window of 4 but function requires odd window
i_min_count_MW <- 5     # earthEnv has >50% entries (9/16)

######     SET LIBRARIES                      #####
library(chron)   # useful for converting time values
library(dplyr)   # use %>%
# library(ncdf4)   # read ncdf
# library(reshape) # reshaping dataframes
library(raster)  # package for raster manipulation
# library(terra)   # terra library for raster manip
# library(reshape) # melt dfs to wide-long
# library(rasterVis)  # enables levelplots
# require(ggplot2)      # for plotting
# library(ggpubr)       # for arranging ggplots together
# library(ggExtra)      # more complex figures, marginal hists
# library(grid)       # for making multi-gird plots and tables 
# library(gridExtra)  # for making multi-gird plots and tables 
# library(ggplotify)  # plots within levelplots
# library(rgdal)        # 
# library(RColorBrewer) # colour palettes
# library(sf)           # utilise shapefiles etc
library(glcm)           # calculate image textures (e.g. homogeneity/dissimilarity)


###################################################
######       I/O                              #####
###################################################

# initialise input file
##### input files #####
# biomass file
f_biomass <- 'data/biodiversity/biomass/nasa_biomass/agbRescaledAtModis.tif'
# forest cover at 300m
f_forcov  <- 'data/ancillary/hansen/hansen_forest_cover_nc_mask/hansenForestCoverNoLoss2000AtModisMean_binary50.nc'
# 5km reference file for rescaling
f_input_hansen <- 'data/ancillary/hansen/hansen_forest_cover_nc_005/hansenForestCoverNoLoss2000AtModisMean_005.nc'
# 5km kNDVI data for also creating dissim/homog
f_kndvi_map <- 'data/vegetation/8day/KNDVI_no_mask/KNDVI_no_mask_nc_merged_rescaled10_masked_005/merged/90_percentile_kndvi/merged_kndvi_no_mask_2003_2021_rescaled10_masked50_005_timpctl90.nc'

# initialise output
output_path <- 'data/biodiversity/biomass/nasa_biomass/'
print(paste0('output_path is : ', output_path ))
# if(! dir.exists(output_path)) {dir.create(paste0(output_path),recursive=T)} # create directory if not present

###################################################
######       CREATE AND MASK FC RASTER        #####
###################################################

# load and examine raster
r_biomass <- raster::raster(f_biomass)
r_forcov  <- raster::raster(f_forcov)
r_kndvi_map <- raster::raster(f_kndvi_map)
plot(r_biomass) ; summary(r_biomass) ; res(r_biomass)
plot(r_forcov)  ; summary(r_forcov)  ; res(r_forcov)

# remove NA values
r_biomass[r_biomass==-32767] <- NA
# remove zero values in forest mask
r_forcov[r_forcov==0] <- NA

# crop biomass to raster size -  # probably should check to do this step early in GEE stage
# wkt(r_biomass) == wkt(r_forcov)
# r_biomass_crop <- crop(r_biomass,r_forcov)
r_biomass_resample <- resample(r_biomass,r_forcov, method = 'ngb')

# apply forest mask to raster
r_biomass_masked <- mask(r_biomass_resample, r_forcov)
plot(r_biomass_masked) ; summary(r_biomass_masked) ; res(r_biomass_masked)
# summary(r_biomass_masked - r_biomass_resample) #, zlim=c(0,1))
# plot(r_biomass_masked - r_biomass_resample, zlim=c(-150,0)) # doesn't show the differences as masked are NAs

###################################################
######       CREATE MW COUNT MASK             #####
###################################################
# create a threshold mask that 

# first apply a 4x4 moving window and count entries (in order to apply mask on valid number points)
# create a raster of 1s (filled values) and zeros (NAs)
r_biomass_filled <- r_biomass_masked
values(r_biomass_filled)[values(r_biomass_filled) == 0] <- NA
values(r_biomass_filled)[!is.na(values(r_biomass_filled))] <- 1
values(r_biomass_filled)[is.na(values(r_biomass_filled) ) ] <- 0
# summary(r_biomass_filled) ; plot(r_biomass_filled)

# now sum those filled values in moving window - doesn't work as uneven
m_w <- matrix(1, ncol=i_mw_size, nrow=i_mw_size)
r_biomass_filled_entries <- focal(r_biomass_filled, w=m_w, fun=sum)
plot(r_biomass_filled_entries)
# instead create a new raster from rescaling by factor 1:4 but using a sum function?

# only consider MW count above threshold - create mask
r_entries_mask <- r_biomass_filled_entries
values(r_entries_mask)[values(r_entries_mask) < i_min_count_MW] <- NA
values(r_entries_mask)[!is.na(values(r_entries_mask))] <- 1
plot(r_entries_mask)

###################################################
######       RUN GLCM METRICS                 #####
###################################################

# calculate glcm dissimilarity
r_glcm_dissim <- glcm(r_biomass_masked, window = c(i_mw_size,i_mw_size),
               na_opt = 'ignore',                            # ignore NAs (can later apply MW remove)
               statistics = c("dissimilarity"),              # statistics to calculate
               shift=list(c(0,1), c(1,1), c(1,0), c(1,-1)))  # over all directions with shift=list(c(0,1), c(1,1), c(1,0), c(1,-1))

# plot(r_glcm_dissim)
# save(r_glcm_dissim, file = paste0(output_path, 'biomass_dissimilarity_MW', i_mw_size, '.RDATA' ) )

# calculate glcm homogeneity
r_glcm_homog <- glcm(r_biomass_masked, window = c(i_mw_size,i_mw_size),
                      na_opt = 'ignore',                            # ignore NAs (can later apply MW remove)
                      statistics = c("homogeneity"),              # statistics to calculate
                      shift=list(c(0,1), c(1,1), c(1,0), c(1,-1)))  # over all directions with shift=list(c(0,1), c(1,1), c(1,0), c(1,-1))

# plot(r_glcm_homog)
# save(r_glcm_homog, file = paste0(output_path, 'biomass_homogeneity_MW', i_mw_size, '.RDATA' ) )

# calculate glcm dissimilarity
r_glcm_kndvi <- glcm(r_kndvi_map, window = c(i_mw_size,i_mw_size),
                      na_opt = 'ignore',                            # ignore NAs (can later apply MW remove)
                      statistics = c("dissimilarity", "homogeneity"),              # statistics to calculate
                      shift=list(c(0,1), c(1,1), c(1,0), c(1,-1)))  # over all directions with shift=list(c(0,1), c(1,1), c(1,0), c(1,-1))


###################################################
######       CLEAN BY ENTRIES                 #####
###################################################

# apply minimum MW threshold count  (they used 9/16 - we can use 5/9)
r_glcm_dissim_count <- mask(r_glcm_dissim, r_entries_mask)
r_glcm_dissim_count ; plot(r_glcm_dissim_count) ; summary(r_glcm_dissim_count)  ; res(r_glcm_dissim_count)

r_glcm_homog_count <- mask(r_glcm_homog, r_entries_mask)
r_glcm_homog_count  ; plot(r_glcm_homog_count)  ; summary(r_glcm_homog_count)   ; res(r_glcm_homog_count)

###################################################
######       OUTPUT RASTER AS TIF              #####
###################################################


save(r_glcm_dissim_count, file = paste0(output_path, 'biomass_dissimilarity_MW', i_mw_size, '.tif' ) )
# writeRaster(r_glcm_dissim_count, filename=paste0(output_path, 'biomass_homogeneity_MW', i_mw_size, '.tif' ), overwrite=TRUE)

save(r_glcm_homog_count, file = paste0(output_path, 'biomass_homogeneity_MW', i_mw_size, '.tif' ) )

# save(r_glcm_kndvi_count, file = paste0(output_path, 'biomass_kndvi_MW', i_mw_size, '.tif' ) )

###################################################
######       AGGREGATE                        #####
###################################################
# # aggregate to 5km using a factor
# r_glcm_dissim_agg <- aggregate(r_glcm_dissim_count, 10, fun=mean, expand=TRUE, na.rm=TRUE)
# r_glcm_homog_agg  <- aggregate(r_glcm_homog_count, 10, fun=mean, expand=TRUE, na.rm=TRUE)

# aggregate to 5km using grid used throughout analysis
r_template  <- raster::raster(f_input_hansen) # r_template ; plot(r_template) ; res(r_template)
r_glcm_dissim_agg <- resample(r_glcm_dissim_count, r_template, method = 'bilinear')
r_glcm_homog_agg  <- resample(r_glcm_homog_count , r_template, method = 'bilinear')

# plot(r_glcm_dissim_agg)
# plot(r_glcm_homog_agg)


###################################################
######       CONVERT TO DF                    #####
###################################################

df_dissim <- as.data.frame(r_glcm_dissim_agg, xy = T, na.rm=T)
df_homogeneity <- as.data.frame(r_glcm_homog_agg, xy = T, na.rm=T)
summary(df_homogeneity) ; summary(df_dissim)

df_biomass_diversity <- full_join(df_dissim, df_homogeneity)
summary(df_biomass_diversity)

save(df_biomass_diversity, file = paste0(output_path, 'df_biomass_diversity_5km_MW', i_mw_size, '.RDATA' ) )

df_kndvi_diversity <- as.data.frame(r_glcm_kndvi_count, xy = T, na.rm=T)
summary(df_kndvi_diversity) ; dim(df_kndvi_diversity)
save(df_kndvi_diversity, file = paste0(output_path, 'df_kndvi_diversity_5km_MW', i_mw_size, '.RDATA' ) )

###################################################
######       TESTING CORRELATION              #####
###################################################

print(cor(df_kndvi_diversity$glcm_dissimilarity, df_kndvi_diversity$glcm_homogeneity))

# check it maps onto full variable dataframe:
load('/eos/jeodpp/data/projects/FOREST-RESILIENCE/figures/rf_test_diversity/rf_test_diversity_2023-03-19_diversity1st/df_all.RData')
head(df_comb) # remember many points are missing due to Marco GEDI doiversity df
df_comb_biomass <- left_join(df_comb, df_kndvi_diversity)
summary(df_comb_biomass) ; dim(df_comb_biomass)

############################################################
# TESTING WITH NDVI DATA

# 
# r_biomass_filled
# 
# f_tif <- '/eos/jeodpp/data/projects/FOREST-RESILIENCE/data/vegetation/8day/KNDVI/KNDVI/2016_6_12_kNDVI.tif'
# f_bio <- '/eos/jeodpp/data/projects/FOREST-RESILIENCE/data/ancillary/biomass/nasa_biomass/agbRescaledAtModis.tif'
# 
# r_image <- raster::raster(f_tif)
# plot(r_image)
# r_image
# r_image[r_image==-32767] <- NA
# 
# 
# # calculate glcm
# r_glcm <- glcm(r_image, window = c(3,3), 
#                na_opt = 'ignore',                            # ignore NAs (can later apply MW remove)
#                statistics = c("dissimilarity"),              # statistics to calculate
#              shift=list(c(0,1), c(1,1), c(1,0), c(1,-1)))  # over all directions with shift=list(c(0,1), c(1,1), c(1,0), c(1,-1))
# 
# plot(r_glcm)
# save(r_glcm, file = '/eos/jeodpp/data/projects/FOREST-RESILIENCE/GIT-FOREST-RESILIENCE/FOREST-RESILIENCE/sandbox/mark/test_r_dissimilarity.RDATA')
# 
# # calculate glcm
# r_glcm_h <- glcm(r_image, window = c(3,3), 
#                na_opt = 'ignore',                            # ignore NAs (can later apply MW remove)
#                statistics = c("homogeneity"),                # statistics to calculate
#                shift=list(c(0,1), c(1,1), c(1,0), c(1,-1)))  # over all directions with shift=list(c(0,1), c(1,1), c(1,0), c(1,-1))
# plot(r_glcm_h)
# save(r_glcm_h, file = '/eos/jeodpp/data/projects/FOREST-RESILIENCE/GIT-FOREST-RESILIENCE/FOREST-RESILIENCE/sandbox/mark/test_r_dissimilarity.RDATA')
# 
# 
# 
# r_image <- raster::raster(f_bio)
# plot(r_image)
# r_image
# r_image[r_image==-32767] <- NA
# 
# 
# # calculate glcm
# r_glcm <- glcm(r_image, window = c(3,3), 
#                na_opt = 'ignore',                            # ignore NAs (can later apply MW remove)
#                statistics = c("dissimilarity"),              # statistics to calculate
#                shift=list(c(0,1), c(1,1), c(1,0), c(1,-1)))  # over all directions with shift=list(c(0,1), c(1,1), c(1,0), c(1,-1))
# 
# plot(r_glcm)
# save(r_glcm, file = '/eos/jeodpp/data/projects/FOREST-RESILIENCE/GIT-FOREST-RESILIENCE/FOREST-RESILIENCE/sandbox/mark/test_r_dissimilarity_biomass.RDATA')
# 
# # calculate glcm
# r_glcm_h <- glcm(r_image, window = c(3,3), 
#                  na_opt = 'ignore',                            # ignore NAs (can later apply MW remove)
#                  statistics = c("homogeneity"),                # statistics to calculate
#                  shift=list(c(0,1), c(1,1), c(1,0), c(1,-1)))  # over all directions with shift=list(c(0,1), c(1,1), c(1,0), c(1,-1))
# plot(r_glcm_h)
# save(r_glcm_h, file = '/eos/jeodpp/data/projects/FOREST-RESILIENCE/GIT-FOREST-RESILIENCE/FOREST-RESILIENCE/sandbox/mark/test_r_dissimilarity_biomass.RDATA')
