# ########################################################
# Title         : create_rf_v1diversity.R
# Description   : create rf model substituting different diversity metrics as variables each time
#                 
# Aims          : create RF model
# Inputs	      : df containing KNDVI and CLIMATE data
# Outputs	      : figures
# Options	      : 
# Date          : 17/03/23
# Version       : 1
# Authors       : Mark Pickering & Agata Elia
# Maintainer    : Mark Pickering 
# Notes		      : 
# ########################################################



###################################################
######     INITIALISE                         #####
###################################################
rm(list = ls())                                   # remove loaded objects

source('/eos/jeodpp/data/projects/FOREST-RESILIENCE/GIT-FOREST-RESILIENCE/FOREST-RESILIENCE/code/main/initialise_R.R')

######     GLOBAL VARS                        #####
script_info      <- 'rf_test_diversity'               # used in output name
script_info_input <- 'createDF_kndviclim_muTACcov'                 # load input dataframes containing full data and residuals
# input_script_date <- '2023-01-26'                        # the 0.05 production after creating the GIT (first kndvi version)
# input_script_date <- '2023-03-14_diversity1st'           # the 0.05 production with second kndvi version and heterogeneity+diversity data
input_script_date <- '2023-04-17'                          # EGU production new diversity metrics

######     SET LIBRARIES                      #####
# library(chron)   # useful for converting time values
library(dplyr)   # use %>%
# library(ncdf4)   # read ncdf
# library(reshape) # reshaping dataframes
# library(raster)  # package for raster manipulation
# library(terra)   # terra library for raster manip
# library(reshape) # melt dfs to wide-long
# library(rasterVis)  # enables levelplots
require(ggplot2)      # for plotting
# library(ggpubr)       # for arranging ggplots together
# library(ggExtra)      # more complex figures, marginal hists
# library(grid)       # for making multi-gird plots and tables 
# library(gridExtra)  # for making multi-gird plots and tables 
# library(ggplotify)  # plots within levelplots
# library(rgdal)        # 
# library(RColorBrewer) # colour palettes
# library(sf)           # utilise shapefiles etc

# RF model
library(caTools) # for test/training split
library(randomForest) # for random forest regressions and ML




###################################################
######       I/O                              #####
###################################################

# initialise input file
input_dir <- paste0(root_data_proce, script_info_input, '/', script_info_input, '_', input_script_date,  '/')

# set/create output directory  - use same as before
output_path <- paste0(root_figures, script_info, '/', script_info, '_', full_date,  '/')
print(paste0('output_path is : ', output_path ))
if(! dir.exists(output_path)) {dir.create(paste0(output_path),recursive=T)} # create directory if not present

###################################################
######     SET FUNCTIONS                      #####
###################################################
source(path_figures_init)


###################################################
######     LOAD DATAFRAME & SPLIT             #####
###################################################

load( paste0(input_dir, 'df_all.RData') )
summary(df_comb) ; colnames(df_comb)

# only include variables Gio uses for now
v_target <- 'tac_resid_kndvi'
v_identifiers <- c('x', 'y')
v_predictors <- c( 'mu_kndvi', 
                   'mu_socc30cm', # soil carbon content
                   'mu_forestcover', # previously: 'mu_treecover',
                   'mu_ssr', 'cv_ssr', 'tac_resid_ssr',
                   'mu_t2m', 'cv_t2m', 'tac_resid_t2m', 
                   'mu_tp', 'cv_tp', 'tac_resid_tp',
                   'mu_VPD', 'cv_VPD', 'tac_resid_VPD'
                   # 'mu_spei', 'cv_spei', 'tac_resid_spei' # shouldn't really use this as predictor - use VPD instead
              )

# add biodiversity variables
v_optional_predictors <- c( "EVI_dissimilarity_glcmdiversity"  , "EVI_homogeneity_glcmdiversity",
                            "biomass_dissimilarity_glcmdiversity", "biomass_homogeneity_glcmdiversity" ,
                            "skew_avg_diversity" , "kurt_avg_diversity" , "rh50_avg_diversity" ,
                            "fhd_avg_diversity" , "rh98_avg_diversity" , "skew_cv_diversity" , 
                            "kurt_cv_diversity" , "rh50_cv_diversity" ,  "fhd_cv_diversity",
                            "no_diversity",
                            "biomass_dissimilarity_glcmdiversity", "biomass_homogeneity_glcmdiversity" )


# v_optional_predictors <- c( "mu_dissimilarity"  , "skew_avg_diversity" , "kurt_avg_diversity" , "rh50_avg_diversity" ,
#   "fhd_avg_diversity" , "rh98_avg_diversity" , "skew_cv_diversity" , "kurt_cv_diversity" , "rh50_cv_diversity" ,  "fhd_cv_diversity")

###################################################
######     SET RF PARAMETERS                  #####
###################################################
# need to do the test train split by each diversity group and removing NAs

# SIMPLE Train-Test Split - 30% split to start
set.seed(101)
f_train_frac <- 0.7

# parameters to optimise
ntree_1 <- 500                          # to optimise
mtry_1 <- ceiling( (length(v_predictors) + 1 ) /3) # use default and optimise

# sample <- sample.split(df_comb$tac_resid_kndvi , SplitRatio = 0.7)
# df_comb.train <- subset(df_comb, sample == TRUE)
# df_comb.test  <- subset(df_comb, sample == FALSE)
# # summary(df_comb.train) ; summary(df_comb.test) 
# dim(df_comb.train) ; dim(df_comb.test) ; print(dim(df_comb.train)[1] / (dim(df_comb.test)[1] + dim(df_comb.train)[1]))
# 
# # save train and test set for later analysis
# save(df_comb.train, file=paste0(output_path, 'df_comb.train.RData' )    )
# save(df_comb.test , file=paste0(output_path, 'df_comb.test.RData' )    )
# save(df_comb , file=paste0(output_path, 'df_all.RData' )    )


###################################################
######     RUN                                #####
###################################################
rf_start_time <- Sys.time() ; print(rf_start_time)      # initialise time

# loop over 'diversity metrics' adding each one in turn and running the RF
for (i in 15:length(v_optional_predictors)){ 
  var_name_i <- v_optional_predictors[i]
  print(var_name_i)
  
  # select only relevant predictors
  if ( var_name_i == "no_diversity") { v_all_vars <- c( v_target, v_predictors)  }else{
    v_all_vars <- c( v_target, var_name_i, v_predictors) }  # v_identifiers
  
  # select only those columns
  # df_comb_i <- df_comb[, v_all_vars]
  # head(df_comb_i)

  df_comb_i <- df_comb[, v_all_vars]
  df_comb_i <- df_comb_i[complete.cases(df_comb_i), ]
  
  # create test train split
  sample <- sample.split(df_comb_i$tac_resid_kndvi , SplitRatio = f_train_frac)
  df_comb.train_i <- subset(df_comb_i, sample == TRUE)
  df_comb.test_i  <- subset(df_comb_i, sample == FALSE)
  # summary(df_comb.train) ; summary(df_comb.test) 
  # dim(df_comb.train) ; dim(df_comb.test) ; print(dim(df_comb.train)[1] / (dim(df_comb.test)[1] + dim(df_comb.train)[1]))
  
  # save train and test set for later analysis
  save(df_comb.train_i, file=paste0(output_path, 'df_comb.train_div-',var_name_i, '.RData' )    )
  save(df_comb.test_i , file=paste0(output_path, 'df_comb.test_div-',var_name_i, '.RData' )    )
  save(df_comb_i      , file=paste0(output_path, 'df_all_div-',var_name_i, '.RData' )    )
  

  # df_comb.train_i <- df_comb.train[, v_all_vars]
  # df_comb.train_i_complete <- df_comb.train_i[complete.cases(df_comb.train_i), ]

  # rf.model <- randomForest(!!as.symbol(v_target) ~ . , data = df_comb.train,  importance = TRUE)
  rf.model <- randomForest(tac_resid_kndvi ~ . , data = df_comb.train_i, 
                           mtry = mtry_1, ntree = ntree_1, importance = TRUE,
                           na.action=na.omit) # previously cleaned NAs manually
  rf_end_time <- Sys.time() ; print(rf_end_time)      # initialise time
  rf_duration <- rf_end_time - rf_start_time ; print(rf_duration)

  save(rf.model, file=paste0(output_path, 'rf_model_div-',var_name_i, '.RData' )    )

} # end loop over variables

