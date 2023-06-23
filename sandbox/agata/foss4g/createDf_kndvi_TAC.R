###################################################
######     INITIALISE                         #####
###################################################
rm(list = ls())                                   # remove loaded objects

source('/eos/jeodpp/data/projects/FOREST-RESILIENCE/GIT-FOREST-RESILIENCE/FOREST-RESILIENCE/code/main/initialise_R.R')

######     GLOBAL VARS                        #####
script_info      <- 'createDF_kndviclim_muTACcov'               # used in output name
script_info_input <- '/eos/jeodpp/data/projects/FOREST-RESILIENCE/data_processing/createDF_kndviclim_phenology/createDF_kndviclim_phenology_2023-04-15/'

######     SET LIBRARIES                      #####
library(chron)   # useful for converting time values
library(dplyr)   # use %>%
# library(ncdf4)   # read ncdf
library(reshape) # reshaping dataframes
# library(raster)  # package for raster manipulation
# library(terra)   # terra library for raster manip
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
library(randomForest)

###################################################
######       I/O                              #####
###################################################

# initialise input file
# input_dir <- 'data_processing/createDF_kndviclim/createDF_kndviclim_2023-01-11/'
# input_dir <- paste0(root_data_proce, script_info_input, '/', script_info_input, '_', full_date,  '/')
input_dir   <- script_info_input

# initialise output
output_path <- '/eos/jeodpp/data/projects/FOREST-RESILIENCE/GIT-FOREST-RESILIENCE/FOREST-RESILIENCE/sandbox/agata/foss4g/'
print(paste0('output_path is : ', output_path ))

###################################################
######     SET FUNCTIONS                      #####
###################################################

cacl_tac <- function(df_i, df_dates_t1, var_name ){
  # function takes a df, df_i, with x,y pixels, and values of column var_name for each date
  # creates a copy of the index of these dates shifted by 1 index
  # calculates the autocorrelation of the var_name value shifted by 1 index

  # take index of dates: clone and shift by one index
  names(df_dates_t1) <- c('date', 't_i')
  df_dates_t2 <- data.frame(v_dates, 2:(length(v_dates) + 1 ) )
  names(df_dates_t2) <- c('date', 't_i')
  # head(df_dates_t1) ; tail(df_dates_t1) ; head(df_dates_t2) ; tail(df_dates_t2)

  # create two dfs of var_name with shifted index and values
  df_t1 <-  left_join(df_i, df_dates_t1)
  df_t2 <-  left_join(df_i, df_dates_t2)
  names(df_t1)[4] <- paste0(var_name, '_t1') ; names(df_t2)[4] <- paste0(var_name, '_t2')
  df_t1$date <- NULL ; df_t2$date <- NULL

  # join the shifted
  df_o <- full_join(df_t1, df_t2)
  df_o <- na.omit(df_o)

  # calculate the 1-step lag
  df_o <- df_o %>% dplyr::group_by( x, y ) %>%
    dplyr::summarise( tac_resid = cor( !!as.symbol(paste0(var_name, '_t1')) , !!as.symbol(paste0(var_name, '_t2') )) )

  return(df_o)
}

###################################################
######     EXTRACT DATE INDEX                 #####
###################################################
# take all the dates and label indices for t1 item and t2 (1-lag) item for calculating autocorrel

load(paste0(input_dir, 'df_kndvi_baseVar_full_gs_masked.RData'  ) )

# select all the unique timestamps to be considered in the analysis
v_dates <-  unique(df_var$date)
if(length(v_dates) != 874) {'Warning: check dates maybe something wrong'}
df_dates_index_t1 <- data.frame(v_dates, 1:length(v_dates) )

###################################################
######       extract statistics               #####
###################################################

var_i <- 'kndvi'
  
# clean
df_var <- df_var[,c("x","y","date","kndvi")]

# calc tac
df_stats_tac <- cacl_tac(df_var, df_dates_index_t1, var_i)
names(df_stats_tac)[3] <- "tac"

# save individual dataframes and join to single dataframe
save(df_stats_tac, file=paste0(output_path, 'df_', var_i , '_baseVar_TAC.RData'))

# load deseas kndvi tac
load('/eos/jeodpp/data/projects/FOREST-RESILIENCE/data_processing/createDF_kndviclim_muTACcov/createDF_kndviclim_muTACcov_2023-04-17/df_kndvi_muTACcov.RData')
df_stats <- df_stats[,c("x","y","tac_resid")]
save(df_stats, file=paste0(output_path, 'df_', var_i , '_deseas_TAC.RData'))

################################################################
######       extract statistics from RF model              #####
################################################################

#load('/eos/jeodpp/data/projects/FOREST-RESILIENCE/figures/rf_test_diversity/rf_test_diversity_2023-04-18/v1_first_diversityVars/rf_model_div-no_diversity.RData')
load('/eos/jeodpp/data/projects/FOREST-RESILIENCE/figures/rf_test_diversity/rf_test_diversity_2023-05-03/rf_model_div-no_diversity.RData')
load('/eos/jeodpp/data/projects/FOREST-RESILIENCE/GIT-FOREST-RESILIENCE/FOREST-RESILIENCE/sandbox/agata/foss4g/df_all_all_div-no_diversity.RData')

df_comb_test <- df_comb_i[,4:18]
predicted <- as.data.frame( predict(rf.model, df_comb_test) )
names(predicted) <- "predicted"
df_predicted <- cbind(df_comb_i[,c("x","y", "tac_resid_kndvi")], predicted)
df_residuals <- df_predicted %>% mutate(residual = (tac_resid_kndvi - predicted))
save(df_residuals, file=paste0(output_path, 'df_', 'RFresiduals_TAC_500trees.RData'))