# ########################################################
# Title         : test_TAC_kNDVIonly_plot_1.R
# Description   : Make a first plot of the time autocorrelation of the deseasonalised kNDVI only (no climate model, 500m res, no detrending)
# Aims          : Produce a summary figure of the TAC
# Inputs	      : netcdf containing temporal autocorrelation of kNDVI calculated in CDO
# Outputs	      : ggplot of TAC with extra information (e.g. corresponding histograms)
# Options	      : 
# Date          : 4/10/22
# Version       : 1.1
# Authors       : Mark Pickering 
# Maintainer    : Mark Pickering 
# Notes		      : 
# Example use   : 
# ########################################################

###################################################
######     INITIALISE                         #####
###################################################
rm(list = ls())                                   # reset 
start_time <- Sys.time() ; print(start_time)      # initialise time
# find date (e.g. label output dir):
full_date <- as.character(Sys.time()) ; full_date <- strsplit(full_date, " ")
full_time <- full_date[[1]][2] ; full_date <- full_date[[1]][1]

######     GLOBAL VARS                        #####
script_info <- 'test_plotting'
nc_varname <- 'kndvi'
real_varname <- 'TAC'
agg_fact <- 4 # aggregation factor for visualisation purposes


######     SET LIBRARIES                      #####
library(chron) # useful for converting time values
library(dplyr) # use %>%
library(ncdf4) # read ncdf
library(reshape) # reshaping dataframes
library(raster)  # package for raster manipulation
# library(rasterVis)  # enables levelplots
require(ggplot2)    # for plotting
# library(grid)      # for making multi-gird plots and tables 
# library(gridExtra) # for making multi-gird plots and tables 
# library(ggplotify)  # plots within levelplots
library(rgdal)      # 
library(RColorBrewer) # colour palettes
library(sf)           # utilise shapefiles etc

###################################################
######       I/O                              #####
###################################################

# initialise directory
setwd('/eos/jeodpp/data/projects/FOREST-RESILIENCE/')

# initialise input file
# input_nc <- '/eos/jeodpp/data/projects/FOREST-RESILIENCE/data/vegetation/8day/KNDVI/KNDVI_tac/merged_kndvi_2003_2021_deseasonalised_tac.nc'
input_nc <- 'data/vegetation/8day/KNDVI/KNDVI_tac/merged_kndvi_2003_2021_deseasonalised_tac.nc'

# set output directory
output_root <- 'figures/test/'
output_path <- paste0(output_root, script_info, '_', full_date,  '/')
print(paste0('output_path is : ', output_path ))
if(! dir.exists(output_path)) {dir.create(paste0(output_path),recursive=T)} # create directory if not present

#######################################
##### INITIALISE SHAPEFILES       #####
#######################################
# load and process shapefile
land_shapefile <- 'data/ancillary/world_vectors/50m_coastline/ne_50m_coastline.shp'
land_shapefile <- sf::st_read(land_shapefile, quiet = TRUE)
summary(land_shapefile) # plot(land_shapefile)

# construct polygon to be clipped 
lon_left <- -12.8 ; lon_right <- 47.2 ; lat_low <- 32 ; lat_high <- 71.3
bboxpolygon = sf::st_polygon(list(rbind(c(lon_left,lat_low),  c(lon_right,lat_low), c(lon_right,lat_high),  c(lon_left,lat_high),     c(lon_left,lat_low))), dim = "XYZ") # removes Turkey/Canaries
# bboxpolygon = sf::st_polygon(list(rbind(c(2.5E6,1E6),  c(6E6,1E6),     c(6E6,6E6),    c(2.5E6,6E6),     c(2.5E6,1E6))), dim = "XYZ") # removes Turkey/Canaries
# land_shapefile <- sf::st_intersection(land_shapefile, bboxpolygon)
land_shapefile <- sf::st_crop(land_shapefile, bboxpolygon)
# plot(land_shapefile)


###################################################
######       CREATE DATAFRAME                 #####
###################################################

# convert ncdf to a raster stack
r_input <- stack(input_nc, varname = nc_varname)
# plot(r_input, zlim=c(-1,1) )

# apply an aggregation to the raster
# obviously we don't want to aggregate for the data analysis, but for visualisation it will be much easier
r_input_agg <- aggregate(r_input, agg_fact, fun=mean, expand=TRUE, na.rm=TRUE)
# plot(r_input_agg, zlim=c(-1,1) )

# convert raster stack to a dataframe with xy coord
df_input <- as.data.frame(r_input_agg, xy = T)
names(df_input)[3] <- real_varname
dim(df_input) ; head(df_input) ; summary(df_input) # #dat.df.all <

###################################################
######       ANALYSE DATAFRAME                #####
###################################################

# create histogram of values
# text_size = 20 # big maps
text_size = 30
h_dist <- ggplot(df_input, aes_string(x = real_varname )) + 
  geom_histogram( bins = 100, colour = "black", fill='black') + #  # ..density.. breaks = seq(-1, 1, by = 10), aes_string(y =rescaled_hist_density, colour = cat_a)
  #stat_function(fun = dnorm, args = list(mean = mean(model.metrics_int$.resid), sd = sd(model.metrics_int$.resid)))
  theme_classic() +
  # scale_y_continuous(limits = c(0, max_y)) +
  scale_x_continuous(limits = c(-1, 1)) +
  theme(text = element_text(size = text_size)) +          
  labs( y= paste0( 'Frequency'), #title= paste0( gsub('_',' ', x_var), ' Vs ', gsub('_',' ', y_var),' per pixel for 1st GS of year'),
        x= paste0(  real_varname     ) )
# ggsave(filename = paste0(output_path,'hist_',plot_name,'.png'), 
#        plot = h_dist , width = wid, height = hei, dpi = dpi_set)
h_dist




###################################################
######       CREATE MAP                       #####
###################################################

z_lims <- c(-1, 1)
z_lims <- c(-0.1, 0.7)

# plot the biomass relative to present
g_input <-  ggplot() +
  geom_tile(data = df_input, aes_string(x = 'x', y = 'y', fill = real_varname)) +
  geom_sf(data = land_shapefile) + # , color = 'black', size =2) + # place the base layer first
  labs( x= 'latitude', y = 'longitude', fill = real_varname
        # title = paste0(str_replace(str_to_title(species_i), '_', ' '), ' distribution loss')
  ) +
  coord_sf() +
  # coord_equal() +
  # discrete_fill_viridis_c() +
  scale_fill_distiller(limits= z_lims , palette =  'Spectral') + # direction = 1,
  # scale_fill_distiller(limits=c(-1, 1), palette =  'YlGn', direction = 1,
  #                      trans = "log",  breaks = scales::trans_breaks('log10', function(x) round(10 ^ x, digits=1), n=3)    ) + # , guide = guide_colorbar(frame.colour = "black")
  # theme_void() +
  theme(
    legend.position = "bottom",
    legend.background = element_rect(fill = "grey95"), # grey box around legend
    plot.title = element_text(hjust = 0.5) # centre the plot title
    # legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid')
  )
g_input

ggsave(plot = g_input, filename = paste0(output_path, 'enfin/', 'g_enfin_', species_i, '_',  s_ENFIN_var , '.png' ) ) # , width = wid, height = hei)



g_caudullo_layer <-  ggplot() + 
  geom_sf(data = land_shapefile) +  # land_shapefile_lonlat
  geom_sf(data = plg_shp, size = 0.8, aes(fill = 'natural'), alpha = 0.8) + # dark green color to match other figures
  scale_fill_manual(name = "continuous", values = c(natural = "#00441b", synanthropic = "#fee391") ) +
  scale_color_manual(name = "fragmented", values = c(natural = "#04cd00", synanthropic = "#fee391") ) +
  # scale_shape_manual(values = c(fragmented_natural = 3, fragmented_synanthropic = 6) ) +
  labs(title = paste0(str_replace(str_to_title(species_i), '_', ' '), ' distribution (G. Caudullo et al.)') , 
       x= 'latitude', y = 'longitude') + 
  coord_sf() + 
  theme(
    legend.position = "bottom",
    legend.background = element_rect(fill = "grey95"), # grey box around legend
    plot.title = element_text(hjust = 0.5) # centre the plot title
    # legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid')
  )

plot(g_basal_area_avg)





###################################################
######       CREATE DATAFRAME                 #####
###################################################


###################################################
######       CREATE HISTOGRAM                 #####
###################################################
