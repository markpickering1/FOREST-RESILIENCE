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
library(Metrics) # perf metrics commonly used in supervised ML e.g. rmse
library(caTools) # for test/training split
library(randomForest) # for random forest regressions and ML
library(ICEbox)       # Individual conditional expectation https://arxiv.org/pdf/1309.6392.pdf https://cran.r-project.org/web/packages/ICEbox/ICEbox.pdf
                      # https://arxiv.org/pdf/1309.6392.pdf


###################################################
######       I/O                              #####
###################################################

# initialise input file
input_dir <- paste0(root_figures, script_info_input, '/', script_info_input, '_', input_script_date,  '/')
# input_dir_orig <-  "/eos/jeodpp/data/projects/FOREST-RESILIENCE/data_processing/createDF_kndviclim_muTACcov/createDF_kndviclim_muTACcov_2023-03-14_diversity1st/"

# set/create output directory  - use same as before
output_path <- paste0(root_figures, script_info, '/', script_info, '_', full_date,'_plot',  '/')
print(paste0('output_path is : ', output_path ))
if(! dir.exists(output_path)) {dir.create(paste0(output_path),recursive=T)} # create directory if not present

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
load( paste0(input_dir, 'df_all.RData') ) # head(df_comb)
load( paste0(input_dir, 'df_comb.test.RData') )
load( paste0(input_dir, 'df_comb.train.RData') )
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
v_optional_predictors <- c( "EVI_dissimilarity_glcmdiversity"  , "EVI_homogeneity_glcmdiversity",
                            "biomass_dissimilarity_glcmdiversity", "biomass_homogeneity_glcmdiversity" ,
                            "skew_avg_diversity" , "kurt_avg_diversity" , "rh50_avg_diversity" ,
                            "fhd_avg_diversity" , "rh98_avg_diversity" , "skew_cv_diversity" , 
                            "kurt_cv_diversity" , "rh50_cv_diversity" ,  "fhd_cv_diversity")


###################################################
######     PLOT IMPORTANCE                                #####
###################################################
rf_start_time <- Sys.time() ; print(rf_start_time)      # initialise time


# initialise df for both importance and partial dependence
df_importance  <- data.frame() # data frame containing importance metrics for all variables for each diversity
df_partDep_div <- data.frame() # data frame containing partial dependence points for each diversity

# loop over 'diversity metrics' producing importance figure and adding each to an div importance dataframe
# for (i in 1:length(v_optional_predictors)){ # i <- 1
for (i in 5:length(v_optional_predictors)){ # i <- 1
  var_name_i <- v_optional_predictors[i] # extract individual diversity predictor
  print(var_name_i)
  
  # load rf model
  load(paste0(input_dir, 'rf_model_div-',var_name_i, '.RData' ) ) # rf.model load the rf model for each div variable
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
  
  ###################################################
  ####### IMPORTANCE OF VARIABLES          ##########
  ###################################################
  
  if(b_run_importance){
    print('importance')
    # extract importance list from model
    df_vari_imp <- as.data.frame(importance(rf.model))
    df_vari_imp <- df_vari_imp[order(df_vari_imp$`%IncMSE`, decreasing = T),] # reorder in terms of importance
    df_vari_imp <- df_vari_imp[1] # extract only INCMSE
    
    # save(varImpPlot(rf.model), file=paste0(output_path, 'rf_importancePlot' , var_name_i , '.RData' )    )
    
    # produce df for importance within this diversity type and names of each variable
    df_vari_imp_cols <- cbind(ID = rownames(df_vari_imp), df_vari_imp) 
    rownames(df_vari_imp_cols) <- NULL
    
    # replace diversity metric with diversity and add diversity name as variable in column
    rownames(df_vari_imp)[rownames(df_vari_imp) == var_name_i] <- 'diversity'
    df_vari_imp <- data.frame(t(df_vari_imp))
    rownames(df_vari_imp) <- NULL
    df_vari_imp$name <- stringr::str_replace(var_name_i, '_diversity', '')
    
    # bind to overall diversity importance df
    if (i == 1){df_importance <- df_vari_imp
    } else{df_importance <- rbind(df_importance, df_vari_imp)}
    
    # print importance figure of individual variables
    # extract meaning of statistical layers
    names(df_vari_imp_cols)[1:2] <- c('Variable', 'Importance')
    
    # categorise each of the RF variables into the different climate types 
    df_vari_imp_cols <- f_add_category(df_vari_imp_cols, 'Variable')
    
    # create plot ranking the importance of different variables in each RF
    g_importance_i <- f_importance_ranking(df_vari_imp_cols, 'Variable', 'Variable', 'Importance', '% increase MSE', point_color = group.colors_Gio, c(0,100) )
    # plot(g_importance_i)
    ggsave(filename = paste0('g_importance_ranking_', var_name_i ,'.png'), plot = g_importance_i, path = output_path, width = 8, height = 8)
  
  } # end importance ranking
  
  
  ###################################################
  ######     PARTIAL DEPENDENCE                 #####
  ###################################################
  # create dataframe showing the biodiv partial dependence
  if(b_run_partialplot){
    print('partial_dependence')
    # select only complete cases
    df_comb.train_i_complete <- df_comb.train_i[complete.cases(df_comb.train_i), ]
    pp_i <- partialPlot(rf.model, df_comb.train_i_complete, names(df_comb.train_i)[2] ) # second name should be the 
    # head(pp_i)
    pp_i <- as.data.frame(pp_i) ;  pp_i[1:2] <- pp_i[1:2] %>% round( digits = 3)
    # take the variable name and the stat name
    # pp_i[3] <- strsplit(names(df_comb.train)[i], '_')[[1]][length(strsplit(names(df_comb.train)[i], '_')[[1]])] # names(df_comb.train)[i] ; # variable = final item in name
    # pp_i[4] <- paste( strsplit(names(df_comb.train)[i], '_')[[1]][1: (length(strsplit(names(df_comb.train)[i], '_')[[1]]) - 1) ] , collapse = "_" ) # stat =  the 1st and last-1 terms and put together
    pp_i[3] <- stringr::str_replace(var_name_i, '_diversity', '')
    if (i == 1){df_partDep_div <- pp_i
    } else{df_partDep_div <- rbind(df_partDep_div, pp_i)}
  }
  
  ###################################################
  ######     ICE                                #####
  ###################################################
  # it takes ~ 10 hours to run per variable so be sure you want to run this
  # also might need to step through - not run through all variables yet
  if(b_run_dice){ 
    
    # create df of dice values
    df_ice_i <- f_run_ICE(df_comb_i, rf.model, var_name_i) 
    
    # save the overall dice object
    save(df_ice_i, file=paste0(output_path, 'df_dice-',var_name_i, '.RData' )    ) # load(/eos/jeodpp/data/projects/FOREST-RESILIENCE/figures/rf_test_diversity/rf_test_diversity_2023-03-21_plot/df_ice_test-fhd_avg_diversity.RData)
    # summary(df_ice_i) ; dim(df_ice_i)
    
    # now can make a plot
    # first select the limits (can probably assume 0.2 and 0.1 for hist and map respectively, tested: fhd, kurtosis)
    lims_h_in <- c(-0.2, 0.2) ; lims_h_in <- c(-0.1, 0.1)
    # if(var_name_i == "fhd_avg_diversity" | var_name_i == "kurt_avg_diversity")  { lims_h_in <- c(-0.2, 0.2) ; lims_h_in <- c(-0.1, 0.1) }
    # if no limits provided then use quantiles 
    # if(lims_m_i == F)  lims_m_i <- quantile( df_ice_i[['actual_deriv']] , probs = c(0.15, 0.85)) ; if(lims_h_i == F)  lims_h_i <- quantile( df_ice_i[['actual_deriv']] , probs = c(0.05, 0.95))
    
    # make hist and map (save)
    h_dist <- make_hist(df_ice_i, 'actual_deriv', 'ICE partial derivative', lims_h_i)
    g_input <- make_map(df_ice_i, 'actual_deriv', 'ICE partial derivative (actual)', var_name_i, lims_m_i)
    ggsave(plot = g_input, filename = paste0(output_path, 'm_', 'ICE' ,'_', var_name_i, '.png' ) ) # , width = wid, height = hei)
    
    # combine map and histogram and save
    g_draw <- ggdraw( clip = 'off') +
      draw_plot(g_input, x = 0.23, y = 0.23, width = 0.77, height = 0.77, hjust = 0.3) +
      draw_plot(h_dist , x = 0, y = 0,    width = 0.77, height = 0.24, hjust = 0 ) 
    ggsave(plot = g_draw, filename = paste0(output_path, 'g_ICE_comb_', var_name_i, '.png' ) , width = 8, height =10 ) # , width = wid, height = hei)
    
  }
  
  print( paste0('diversity variable model plotting complete for variable : ', var_name_i) )
  rf_end_time <- Sys.time() ; print(rf_end_time)      # initialise time
  rf_duration <- rf_end_time - rf_start_time ; print(rf_duration)
  
} # end loop over variables

# df_importance  # the overall importance dataframe for the different rf models 
# df_vari_imp    # the importance df for the last model
# df_partDep_div # the partial dependence df for the different models

##### plot average diversity (and average all variables score) ; Then print the different biodiv importances
if(b_run_importance){
  
  # save overall dataframe
  save(df_importance, file=paste0(output_path, 'df_importance.RData' )    )
  
  # create a plot of importance of just different diversity variables
  g_importance_div <- f_importance_ranking(df_importance, 'name', 'Diversity type', 'diversity', '% increase MSE', point_color = group.colors_Gio['Other'], c(0,60) )
  plot(g_importance_div)
  ggsave(filename = 'g_diversity_var_ranking.png', plot = g_importance_div, path = output_path, width = 8, height = 8)
  
  ###### plot average of all variables Importance score #####
  df_importance_avg <- df_importance
  df_importance_avg$name <- NULL
  df_importance_avg <- colSums(df_importance_avg, na.rm = T)/dim(df_importance)[1]
  df_importance_avg <- as.data.frame(df_importance_avg)
  df_importance_avg <- cbind(ID = rownames(df_importance_avg), df_importance_avg) 
  rownames(df_importance_avg) <- NULL
  names(df_importance_avg) <- c('Variable', 'Importance')
  
  # categorise each of the RF variables into the different climate types 
  df_importance_avg <- f_add_category(df_importance_avg, 'Variable') # add a category 
  # plot the mean importance of each variable across the different models
  g_importance_avg <- f_importance_ranking(df_importance_avg, 'Variable', 'Variable', 'Importance', '% increase MSE', point_color = group.colors_Gio , c(0,60))
  # plot(g_importance_avg)
  ggsave(filename = paste0('g_importance_ranking_mean.png'), plot = g_importance_avg, path = output_path, width = 8, height = 8)
}


##### PLOT PARTIAL DEPENDENCE 
# here we take the separate partial dependence plot info, group the variables into groups covering similar x-lims
# then plot the figures (for each group in turn)
if(b_run_partialplot){
  # organise the df of pdp values for plotting
  head(df_partDep_div) ; summary(df_partDep_div, 16)
  names(df_partDep_div) <- c( "value" , "TAC", "variable")
  df_partDep_div$variable <- as.factor(df_partDep_div$variable)
  save(df_partDep_div, file=paste0(output_path, 'df_rf_model_partDep.RData' )    )
  
  # set harmonised lims on y-axis (TAC)
  lims_pdp <- c(0.28,0.31)
  # exclude some that go outside x limits
  var_group_1 <- c("skew_avg" , "kurt_avg" , "fhd_avg" , "rh98_avg" , "rh50_cv" ,  "fhd_cv") ; group_num <- 1
  var_group_2 <- c('rh50_avg', 'mu_dissimilarity') ; group_num <- 2
  var_group_3 <- c('skew_cv', 'kurt_cv') ; group_num <- 3
  # var_group_3 <- c('kurt_avg', )
  df_partDep_div_plot <- df_partDep_div  %>% filter(  variable %in%  var_group_2 ) #  %>% filter(  !variable %in%  var_group_2 ) # exclude with !
  ggp <- ggplot( df_partDep_div_plot , aes(value, TAC, color = variable)) +    # Draw ggplot2 plot with one axis
    geom_line( size = 1) +
    xlab('Diversity metric') +
    ylim( lims_pdp ) +
    # scale_color_manual(values=group.colors) +
    basic_graph_theme
  ggsave(filename = paste0('g_partDep_diversityMetric_', group_num ,'.png'), plot = ggp, path = output_path, width = 8, height = 8)
}



############################################################################################################################################
############################                   END                  ########################################################################
############################################################################################################################################
