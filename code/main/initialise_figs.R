# ########################################################
# Title         : initialise_figs.R
# Description   : This script initialises certain plotting standards and variables in order to run the analysis R code. Script should be sourced when plotting analysis figures
# Date          : 27/01/23
# Authors       : Mark Pickering & Agata Elia
# Maintainer    : Mark Pickering 
# Notes         : 
# ########################################################

## to setup this environment, run this file in each plotting script via:
# source(path_figures_init)


###################################################
######     PLOTTING VARIABLES                 #####
###################################################
# set global variables for plotting

text_size = 10                # text size in figures - big maps
legend_size <- 1	      # size of legend in figures


########### HISTOGRAMS ONLY #############

n_bins <- 100


###################################################
######     THEMES                             #####
###################################################


# set theme for figures/maps
basic_fig_theme <- theme( # set the 'theme' various aesthetic features
  plot.title = element_text(size=text_size, hjust = 0.5), # resize centre the plot title
  text = element_text(size = text_size),

    legend.position = "bottom",
  legend.background = element_rect(fill = "white"), # grey95 grey box around legend
  # legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid')
  # element_rect = element_blank(),
  panel.grid = element_line(color = "white"), # "transparent" set the colour of the grid lines
  panel.background = element_blank() # set the colour of the panel behind the figure
)

basic_graph_theme <- theme( # set the 'theme' various aesthetic features
  plot.title = element_text(size=text_size*2, hjust = 0.5), # resize centre the plot title
  text = element_text(size = text_size*2),
  # legend.position = "bottom",
  legend.background = element_rect(fill = "white"), # grey95 grey box around legend
  # legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid')
  # element_rect = element_blank(),

  panel.grid = element_line(color = "grey95"), # "transparent" set the colour of the grid lines
  panel.background = element_blank(), # set the colour of the panel behind the figure
  axis.line.x = element_line(color="black", size = 1),
  axis.line.y = element_line(color="black", size = 1)
  
)

########### HISTOGRAMS ONLY #############

basic_hist_theme <- theme( # plot.margin = margin(0, 210, 0, 210),    # add left right margin so that the hist arranges better
                    text = element_text(size = text_size)
)           

###################################################
######     OTHER PLOTTING                     #####
###################################################
# agg_fact <- 1                                   # aggregation T/F and factor for visualisation purposes (speeds up visualisation but associated problems)



###################################################
######     FUNCTIONS                          #####
###################################################


#for plot making - rounds up to predetermined numbers shown
#roundUpNice <- function(x, nice=c(1,2,4,5,6,8,10)) {
NiceNumbers=c(0,1,1.5,2,3,4,5,6,7,8,9,10)
roundUpNice <- function(x, nice = NiceNumbers) { #https://rdrr.io/cran/subscreen/src/inst/shiny-app/subscreen/app.R
  if (length(x) != 1) stop("'x' must be of length 1")
  if (x >= 0) 10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) *nice)[[1]]]]
  else -1 * (roundDownNice(-x, nice = NiceNumbers))
}

roundDownNice <- function(x, nice = NiceNumbers) {
  if (length(x) != 1) stop("'x' must be of length 1")
  if (x >= 0) 10^floor(log10(x)) * nice[[max(which(x >= 10^floor(log10(x)) * nice))]]
  else -1 * (roundUpNice(-x, nice = NiceNumbers))
}


find_cutpts_from_percentile <- function(df_plotting, col, perc){
  # get the symmetrical cutpoints, (min_scale, max_scale) for a column 'col, within a dataframe, df,
  # using percentiles 'perc' of the values 
  # e.g. perc == 0.01 means max/min scale are at 1% and 99%
  max_val <- quantile(df_plotting[[col]], 1-perc, na.rm = TRUE) ;   min_val <- quantile(df_plotting[[col]], perc,na.rm = TRUE)
  min_scale <-roundDownNice( abs(min_val) ) ;  if( min_val < 0) {min_scale <- 0 - min_scale} ; max_scale <-roundUpNice( max_val )
  if(max_scale*min_scale < 0 ) {
    # if one (only) of max_scale or min_scale is negative, then we want to take the highest absolute value 
    # and set the max or min as that so that the axis is symmetrical 
    if (max_scale >= abs(min_scale)){min_scale <- -1*abs(max_scale)}
    else{max_scale <- abs(min_scale)}
  }
  result <- c(min_scale, max_scale)
  return(result)
}

###################################################
######     MAP SHAPEFILES                     #####
###################################################

# Europe only 
# load and process shapefile
land_shapefile_in <- 'data/ancillary/world_vectors/50m_coastline/ne_50m_coastline.shp' # set shapefile path (global coastlines@50m)
land_shapefile_in <- sf::st_read(land_shapefile_in, quiet = TRUE)                         # read shapefile
# summary(land_shapefile) # plot(land_shapefile)

# load bounding box from shapefile (has coords: -10.66164 34.56369 44.82037 71.18416)
# bb_shapefile <- 'data/ancillary/world_vectors/boundingBoxes/Europe_BB.shp'
# bboxpolygon <- sf::st_read(bb_shapefile, quiet = TRUE)                         # read shapefile

# alt - create bounding box manually
# bb_shapefile <- as(raster::extent(-10.66164, 34.56369, 44.82037, 71.18416), "SpatialPolygons")
bb_shapefile <- as(raster::extent(-10.66164, 44.56369, 33.0, 71.18416), "SpatialPolygons")
proj4string(bb_shapefile) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
# bb_shapefile <- rgeos::bbox2SP(n = -10.66164, s = 34.56369, w = 44.82037, e = 71.18416, proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

# crop land_shapefile
land_shapefile <- sf::st_crop(land_shapefile_in, bb_shapefile)                          # crop shapefile to bounding box
plot(land_shapefile)
