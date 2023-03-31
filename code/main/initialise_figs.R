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
#library(terra)
########### HISTOGRAMS ONLY #############
text_size = 10                # text size in figures - big maps
legend_size <- 1	      # size of legend in figures


########### OTHER           #############
# agg_fact <- 1                                   # aggregation T/F and factor for visualisation purposes (speeds up visualisation but associated problems)
# color scheme for variables    purple              red             blue           brown            yellow              green
group.colors_Gio <- c(kNDVI = "#9933ff", T2M = "#e60000",  TP = "#0000ff", SSR ="#734d26",  VPD = "#ffcc00", Other = "#00cc00")


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
######     PLOTTING FUNCTIONS                 #####
###################################################

# create a histogram of stat_in
make_hist <- function(df_in, stat_in, stat_in_full, lims_in){
  h_dist <- ggplot(df_in, aes_string(x = stat_in )) +
    geom_histogram( bins = n_bins, colour = "black", fill='black') + #  # ..density.. breaks = seq(-1, 1, by = 10), aes_string(y =rescaled_hist_density, colour = cat_a)
    labs( y= paste0( 'Frequency'), 
          x= paste0(  stat_in_full    ) ) + 
    theme_classic() +
    scale_x_continuous(limits = lims_in, oob=squish  ) + # print the jth statistic of the ith variable
    basic_hist_theme          
  return(h_dist)
}


# create a map of stat_in with a selected shapefile outline
make_map <- function(df_in, stat_in, stat_in_full, var_i_full, lims_in){
  g_input <-  ggplot() +
    geom_tile(data = df_in, aes_string(x = 'x', y = 'y', fill = stat_in)) + # add the raster data
    geom_sf(data = land_shapefile) + # , color = 'black', size =2) + # add the shapefile (place the base layer first)
    labs( x='', y='', # x= 'latitude', y = 'longitude', 
          fill = paste0( stat_in_full ),  title = paste0( var_i_full) ) + #, '_', gsub('_',' ', stat_j) ) ) + # label axes and title
    coord_sf() + # align the coordinates to those of shapefile?
    # coord_equal() + # different crs
    # discrete_fill_viridis_c() + # alternative colour system
    scale_fill_distiller(limits= lims_in , palette =  'Spectral', na.value = "white", oob=scales::squish) + # set the colour scheme and palette # direction = 1,
    # scale_fill_distiller(limits=c(-1, 1), palette =  'YlGn', direction = 1,    
    #                      trans = "log",  breaks = scales::trans_breaks('log10', function(x) round(10 ^ x, digits=1), n=3)    ) + # , guide = guide_colorbar(frame.colour = "black")
    # theme_void() +
    basic_fig_theme  
  return(g_input)
}

# performance plot of modelled vs observed
# takes in a df with columns 1 = observed, and 2= modelled 
# calculates certain statistics (N, R2, MSE, RMSE, MAE, percentage bias) if given as true
# plots these variables and the Obs vs Mod data points
f_obs_vs_mod_density <- function(df_in, s_title = var_name_i, b_cor = F, b_mse = F, b_rmse = F,  b_mae = F, b_pbias = F  ){

  # calculate metrics
  if(b_cor)  v_cor    <- round( cor(df_in[,1] , df_in[,2]), digits = 3)
  if(b_mse)  v_mse    <- round( mse(df_in[,1] , df_in[,2]), digits = 5)
  if(b_rmse) v_rmse   <- round( rmse(df_in[,1], df_in[,2]), digits = 3)
  if(b_mae)  v_mae    <- round( mae(df_in[,1] , df_in[,2]), digits = 3) # MAE vs RMSE comparison https://medium.com/human-in-a-machine-world/mae-and-rmse-which-metric-is-better-e60ac3bde13d
  if(b_pbias)v_pbias  <- round( percent_bias(df_in[,1], df_in[,2]), digits = 3) # mean(actual - predicted)/abs(actual)  - not really a percentage
  
  # create label
  s_label_stats <- paste0( 
    "N = ", dim(df_in)[1],
    ifelse(b_cor  , paste0("\nR\u00B2 = ", v_cor) , ''), # rho = \u03C1
    ifelse(b_mse  , paste0("\nMSE = "  , v_mse)   , ''),
    ifelse(b_rmse , paste0("\nRMSE = " , v_rmse)  , ''),
    ifelse(b_mae  , paste0("\nMAE = "  , v_mae)   , ''),
    ifelse(b_pbias, paste0("\nPBIAS = ", v_pbias) , '')
  )
  
  n_bins_dens <- n_bins # set to global hist binning value
  
  # create a plot of or density figure
  ggp <- ggplot( df_in , aes(Observed, Modelled)) +    # Draw ggplot2 plot with one axis
    # geom_point(alpha = 0.01, size = 0.0005, color = 'blue') + # to show datapoints
    stat_bin2d( bins= c(n_bins,n_bins) , aes(fill = ..ndensity..), na.rm = TRUE) + # ..ndensity.. ..count..
    scale_fill_distiller(name = 'Relative \ndensity', direction = 1 ) +
    geom_abline(slope=1, intercept=0, linetype = "dashed" ) +
    xlim(-0.1, 1) + ylim(-0.1,1) + 
    labs(title = s_title) +
    basic_graph_theme
  # add text of the selected stats
  ggp <- ggp +  annotate("text" , x=-0.05, y=0.8, size = text_size/2, hjust = 0,
                         label = s_label_stats  ) # geom_text(data = df_cor, size=txt_size*1.4, aes(x = lim_upper[1]*text_pos_x, y = lim_upper[2]*0.96), parse = TRUE)  # add text of data
  return(ggp)
}



# for a df df_in, creates line graph showing the ranking of variables var_in in, ordered  by their values value_in 
# (_full denotes x,y labesl). Option to include a categorisation by color
f_importance_ranking <- function(df_in, var_in, var_in_full, value_in, value_in_full, point_color = 'orange', lims_c){
  
  # set a default color that will be overwritten if vector of colors applied
  ifelse(length(point_color)==1 , point_color_1 <- point_color , point_color_1 <- 'orange' )
  
  g_importance_i <-
    ggplot(df_in %>% arrange( !!sym(value_in) ) %>% mutate( Variable = factor(!!sym(var_in), levels=!!sym(var_in)) ),
           aes_string(x="Variable", y=value_in )) + # , color = "Category"
    geom_segment( aes_string(xend=var_in, yend=0),  size = 1) + # , color = type
    geom_point( size=4  , color = point_color_1  ) + # single color point
    coord_flip() +  #scale_y_log10() + 
    theme(text = element_text(size=18), #axis.text.x = element_text(size = 20) # angle=90, hjust=1
    ) +
    ylim( lims_c ) +
    ylab(value_in_full) + xlab(var_in_full) +
    basic_graph_theme # theme_bw() 
  
  # overwrite color scale with category of colors 
  if(length(point_color) > 1){
    g_importance_i <- g_importance_i +
      geom_point( size=4  , aes_string( color = "Category")   ) + #, color="orange") + #  , aes_string( color = "Category")
      scale_color_manual(name = "Category", values=group.colors) 
    # scale_color_manual(name = "category", values = c(T2M = "red", TP = 'blue', SSR = 'brown', VPD = 'orange', Other = "dark green") )  +
    # scale_color_manual(name = "type",     values = c(mean = "black", TAC = 'grey50', CV = 'grey10') )  +
  }
  return(g_importance_i)  
}


###################################################
######     OTHER FUNCTIONS                    #####
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
sp::proj4string(bb_shapefile) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
# crs(bb_shapefile) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
# bb_shapefile <- rgeos::bbox2SP(n = -10.66164, s = 34.56369, w = 44.82037, e = 71.18416, proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

# crop land_shapefile
land_shapefile <- sf::st_crop(land_shapefile_in, bb_shapefile)                          # crop shapefile to bounding box
plot(land_shapefile)
