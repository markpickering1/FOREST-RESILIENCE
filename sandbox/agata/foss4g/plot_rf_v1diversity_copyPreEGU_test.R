# ########################################################
# Title         : plot_rf_v1diversity.R
# Description   : plot RF output to analyse performance and, in particular, variability of the 
#                 model to different diversity metrics 
# Aims          : analyse performance and diversity metrics
# Inputs	      : rf models with different diversity metrics in each 
# Outputs	      : figures for each diversity: 1) performance 2) Importance 3) dice 
#                 summary figures: 1) partial plot of diversity 2) ranked average Importance 3) Importance of diversity metrics
# Options	      : 
# Date          : 18/03/23
# Version       : 1
# Authors       : Mark Pickering & Agata Elia
# Maintainer    : Mark Pickering 
# Notes		      : Could extend with other metrics as well as partial plots of other variables
# ########################################################



###################################################
######     INITIALISE                         #####
###################################################
rm(list = ls())                                   # remove loaded objects

source('/eos/jeodpp/data/projects/FOREST-RESILIENCE/GIT-FOREST-RESILIENCE/FOREST-RESILIENCE/code/main/initialise_R.R')

######     GLOBAL VARS                        #####
script_info       <- 'rf_test_diversity'               # used in output name
script_info_input      <- script_info               # used in output name
# input_script_date <- '2023-03-19_diversity1st'           # the 0.05 production with second kndvi version and heterogeneity+diversity data
input_script_date <- '2023-04-18'                   # EGU GS more diversity dataset

# select which analyses to plot
b_run_performance <- TRUE    # check performance metrics of RF
b_run_importance  <- TRUE    # create importance ranking for RF variables
b_run_partialplot <- TRUE    # create partial plots of RF diversity (and other?) variables
# run and plot dice? honestly the dice takes so long that it is better to run over each variable individuallyand adjust the lims and values as needed
# - I don't recommend running via the function - just step through the function
b_run_dice        <- F    # create (actual) derivative of individual conditional expectation (dice) figs for diversity metrics

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
library(sf)           # utilise shapefiles etc

# RF model
#library(metrics) # perf metrics commonly used in supervised ML e.g. rmse
library(caTools) # for test/training split
library(randomForest) # for random forest regressions and ML
#library(ICEbox)       # Individual conditional expectation https://arxiv.org/pdf/1309.6392.pdf https://cran.r-project.org/web/packages/ICEbox/ICEbox.pdf
                      # https://arxiv.org/pdf/1309.6392.pdf


###################################################
######       I/O                              #####
###################################################

# initialise input file
input_dir <- paste0(root_figures, script_info_input, '/', script_info_input, '_', input_script_date,  '/')
# input_dir_orig <-  "/eos/jeodpp/data/projects/FOREST-RESILIENCE/data_processing/createDF_kndviclim_muTACcov/createDF_kndviclim_muTACcov_2023-03-14_diversity1st/"

# set/create output directory  - use same as before
output_path <- "/eos/jeodpp/data/projects/FOREST-RESILIENCE/GIT-FOREST-RESILIENCE/FOREST-RESILIENCE/sandbox/agata/foss4g/"

###################################################
######     SET FUNCTIONS                      #####
###################################################
source(path_figures_init)

# take a dataframe with a column containing selected keywords and put these keywords as a category
f_add_category <- function(df_in, col_name){
  df_in <- df_in %>% mutate(Category = ifelse( grepl( 't2m', !!sym(col_name), fixed = TRUE), 'T2M',
                                               ifelse( grepl( 'VPD', !!sym(col_name), fixed = TRUE), 'VPD',
                                                       ifelse( grepl( 'tp', !!sym(col_name), fixed = TRUE), 'TP',
                                                               ifelse( grepl( 'ssr', !!sym(col_name), fixed = TRUE), 'SSR',
                                                                       ifelse( grepl( 'kndvi', !!sym(col_name), fixed = TRUE), 'kNDVI',
                                                                               'Other' )
                                                               )      )       )       )       )
  return(df_in)
}

# same for stats
# df_vari_imp_cols <- df_vari_imp_cols %>% mutate(Type = ifelse( grepl( 'tac', Variable, fixed = TRUE), 'TAC',
#                                                                ifelse( grepl( 'cv', Variable, fixed = TRUE), 'CV',
#                                                                        ifelse( grepl( 'mu', Variable, fixed = TRUE), 'mean',
#                                                                                'other' )
#                                                                )             )       )

# function taking dataframe as input, an rf.model to run over and a variable of interest (and plotting limits if required)
# first splits the dataframe into manageable chunks
# honestly, this takes so long it is better to run over each variable individually and not loop over the diversity variables
# unless you are absolutely sure it is working and will plot the correct limits
# also may need to rename input values in functions as function input names are same as acutal names that will be used
# function is untested (but code was separately tested)
f_run_ICE <- function(df_comb_i, rf.model,  var_name_i){ # , lims_h_i = F, lims_h_i = F
  
  # First split the dataframe into manageable chunks over which to run using parameters:
  n_rows <- dim(df_comb_i)[1] ; n_splits <- 10
  split_vals <- ceiling( n_rows/n_splits ) 
  df_comb_ij <- split(df_comb_i, (as.numeric(rownames(df_comb_i))-1) %/% split_vals) # split dataframe
  # dim(df_comb_ij$`0`) ; dim(df_comb_ij$`9`) ; 
  # test the sum of the split rows adds up
  print(dim(df_comb_ij$`0`)[1]*(n_splits - 1) + dim(df_comb_ij[[n_splits ]])[1] == n_rows)
  
  # create empty df of ICE
  df_ice_i <- data.frame()
  
  # create loop over deciles of the df to build the full model
  for (j in 1:n_splits){ # j <- 1
    print(j)
    df_comb_ij_j <- df_comb_ij[[j]]
    
    # create an ice object from the rf model the split df
    rf.ice_j = ice(object = rf.model, X = df_comb_ij_j, y = df_comb_ij_j[[v_target]], predictor = var_name_i, # y = df_comb_i$tac_resid_kndvi
                   indices_to_build = )
    #frac_to_build = 1)
    
    # create a derivative ice object (dice)
    rf.dice_j <- dice(rf.ice_j)
    # test plot ;    plot.dice
    
    # ice sorts the order of the df so need to recombine with the input dataframe 
    # (match the input diversity values with the 'xj' valuse of predictor as a check)
    print('check column match:') ; print(summary(rf.dice_j$Xice[[var_name_i]] == rf.dice_j$xj))
    
    # now bind the dice output of the derivative at the point (local derivative)
    df_rf.dice_j_t <- as.data.frame(rf.dice_j$actual_deriv) #, rf.dice_j$dpdp)
    names(df_rf.dice_j_t) <- 'actual_deriv'
    if(dim(rf.dice_j$Xice)[1] == dim(df_rf.dice_j_t)[1]) {
      df_rf.dice_j <- cbind(rf.dice_j$Xice, df_rf.dice_j_t) }
    # should I add dpdp (overall derivative? It has a different length so can't bind to original df
    # need to confirm what exactly is this from literature but was difficult
    df_rf.dice_j_d <- as.data.frame(rf.dice_j$dpdp) #, rf.dice_j$dpdp) }
    names(df_rf.dice_j_d) <- 'dpdp'
    if( dim(df_rf.dice_j)[1] == dim(df_rf.dice_j_d)[1] ) { print('add dpdp')
      df_rf.dice_j <- cbind(df_rf.dice_j, df_rf.dice_j_d) }
    
    # df_rf.dice_j <- cbind(rf.dice_j$xj,rf.dice_j$actual_deriv) # list of changed points
    # df_rf.dice_j <- as.data.frame(df_rf.dice_j)
    # names(df_rf.dice_j)[2] <- 'ice_deriv'
    # df_rf.dice_j <- cbind(rf.dice_j$Xice, df_rf.dice_j$ice_deriv)
    
    # join the split dice with the other splits
    if (i == 1){df_ice_i <- df_rf.dice_j
    } else{df_ice_i <- rbind(df_ice_i, df_rf.dice_j)}
    
    print('dice split timing complete for split : ', i)
    rf_end_time <- Sys.time() ; print(rf_end_time)      # initialise time
    rf_duration <- rf_end_time - rf_start_time ; print(rf_duration)
    
  } # end loop over deciles
  
  return(df_ice_i)
} # close function


###################################################
######     LOAD DATAFRAME & SET VARIABLES     #####
###################################################

# load original df as well as train test split
load( paste0(input_dir, 'v1_first_diversityVars/df_all.RData') ) # head(df_comb)
load( paste0(input_dir, 'v1_first_diversityVars/df_comb.test.RData') )
load( paste0(input_dir, 'v1_first_diversityVars/df_comb.train.RData') )
summary(df_comb.train) ; colnames(df_comb.train) ; dim(df_comb.train)
summary(df_comb.test)  ; colnames(df_comb.test)  ; dim(df_comb.test)
dim(df_comb.train) ; dim(df_comb.test) ; print(dim(df_comb.train)[1] / (dim(df_comb.test)[1] + dim(df_comb.train)[1]))
dim(df_comb) * c(1,2) == dim(df_comb.train) + dim(df_comb.test)

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
# add biodiversity variables - from first biodiversity look
# v_optional_predictors <- c( "mu_dissimilarity"  , "skew_avg_diversity" , "kurt_avg_diversity" , "rh50_avg_diversity" ,
#                             "fhd_avg_diversity" , "rh98_avg_diversity" , "skew_cv_diversity" , "kurt_cv_diversity" , "rh50_cv_diversity" ,  "fhd_cv_diversity")


# add biodiversity variables - EGU
v_optional_predictors <- c("no_diversity")


###################################################
######     PLOT IMPORTANCE                                #####
###################################################
rf_start_time <- Sys.time() ; print(rf_start_time)      # initialise time


# initialise df for both importance and partial dependence
df_importance  <- data.frame() # data frame containing importance metrics for all variables for each diversity
df_partDep_div <- data.frame() # data frame containing partial dependence points for each diversity

# loop over 'diversity metrics' producing importance figure and adding each to an div importance dataframe
# for (i in 1:length(v_optional_predictors)){ # i <- 1
for (i in 1:length(v_optional_predictors)){ # i <- 1
  var_name_i <- v_optional_predictors[i] # extract individual diversity predictor
  print(var_name_i)
  
  # load rf model
  load(paste0(input_dir, 'v1_first_diversityVars/rf_model_div-',var_name_i, '.RData' ) ) # rf.model load the rf model for each div variable
  # initialise train/test df
  v_all_vars <- c( v_target, var_name_i, v_predictors) # v_identifiers
  df_comb.train_i <- df_comb.train[, v_all_vars]
  df_comb.test_i  <- df_comb.test[, v_all_vars]
  df_comb_i       <- df_comb[, c(v_identifiers ,v_all_vars) ]       # full original df and X, Y 
  
  ###################################################
  ####### IMPORTANCE OF VARIABLES          ##########
  ###################################################
  
  if(b_run_performance){
    # run and plot performance metrics (R2 MSE BIAS g_OBS_vs_PRED) on the test dataset
    print('testing performance')
    
    # get df_test predictions of resilience metric based on rf.model. test df (df_comb.test_i must have target variable removed)
    df_comb.test_i_predictors <- df_comb.test_i[,-which(names(df_comb.test_i) %in% v_target)] # df_comb.test[,-c(1)])
    df_comb.test_i_predict_test <- as.data.frame( predict(rf.model, df_comb.test_i_predictors) )  
    head(df_comb.test_i_predict_test) ; dim(df_comb.test_i_predict_test) ; dim(df_comb.test_i) ;
    
    # combine the observed and the model predicted resilience metrics
    df_comb_predictVobs <- cbind( df_comb.test_i[,which(names(df_comb.test_i) %in% v_target)], df_comb.test_i_predict_test )
    names(df_comb_predictVobs) <- c( 'Observed', 'Modelled' )
    # head(df_comb_predictVobs) ; summary(df_comb_predictVobs)
    df_comb_predictVobs <- df_comb_predictVobs[complete.cases(df_comb_predictVobs), ]
    
    g_perf <- f_obs_vs_mod_density(df_comb_predictVobs, s_title = var_name_i, b_cor = T, b_mse = T, b_rmse = T,  b_mae = T, b_pbias = T  )
    ggsave(plot = g_perf, filename = paste0(output_path, 'g_obsVSmod_div-', var_name_i, '.png' ) ) # , width = wid, height = hei)
    
  }  # end model performance
}