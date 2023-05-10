# ########################################################
# Title         : RF.R
# Description   : test RF output to re-create Giovanni work
#                 
# Aims          : re-create some of Giovanni's figures
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
script_info       <- 'rf_test_diversity'               # used in output name
script_info_input      <- script_info               # used in output name
input_script_date <- '2023-03-19_diversity1st'           # the 0.05 production with second kndvi version and heterogeneity+diversity data

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
library(caTools) # for test/training split
library(randomForest) # for random forest regressions and ML
library(ICEbox)       # Individual conditional expectation https://arxiv.org/pdf/1309.6392.pdf https://cran.r-project.org/web/packages/ICEbox/ICEbox.pdf



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
v_optional_predictors <- c( "mu_dissimilarity"  , "skew_avg_diversity" , "kurt_avg_diversity" , "rh50_avg_diversity" ,
                            "fhd_avg_diversity" , "rh98_avg_diversity" , "skew_cv_diversity" , "kurt_cv_diversity" , "rh50_cv_diversity" ,  "fhd_cv_diversity")


###################################################
######     PLOT IMPORTANCE                                #####
###################################################
rf_start_time <- Sys.time() ; print(rf_start_time)      # initialise time

# run and plot dice?
# honestly the dice takes so long that it is better to run over each variable individuallyand adjust the lims and values as needed
# - I don't recommend running via the function - just step through the function
b_run_dice <- TRUE

# initialise df for both importance and partial dependence
df_importance  <- data.frame() # data frame containing importance metrics for all variables for each diversity
df_partDep_div <- data.frame() # data frame containing partial dependence points for each diversity


# loop over 'diversity metrics' producing importance figure and adding each to an div importance dataframe
for (i in 1:length(v_optional_predictors)){ # i <- 1
  var_name_i <- v_optional_predictors[i] # extract individual diversity predictor
  print(var_name_i)
  
  # load rf model
  load(paste0(input_dir, 'rf_model_div-',var_name_i, '.RData' ) ) # load the rf model for each div variable
  # initialise train/test df
  v_all_vars <- c( v_target, var_name_i, v_predictors) # v_identifiers
  df_comb.train_i <- df_comb.train[, v_all_vars]
  df_comb.test_i  <- df_comb.test[, v_all_vars]
  df_comb_i       <- df_comb[, c(v_identifiers ,v_all_vars) ]       # full original df and X, Y 
  
  ####### IMPORTANCE OF VARIABLES ##########
  # extract importance list from model
  df_vari_imp <- as.data.frame(importance(rf.model))
  df_vari_imp <- df_vari_imp[order(df_vari_imp$`%IncMSE`, decreasing = T),] # reorder in terms of importance
  df_vari_imp <- df_vari_imp[1] # extract only INCMSE
  
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
  
  df_vari_imp_cols <- f_add_category(df_vari_imp_cols, 'Variable')
  
  # create plot ranking the importance of different variables in each RF
  g_importance_i <- f_importance_ranking(df_vari_imp_cols, 'Variable', 'Variable', 'Importance', '% increase MSE', point_color = group.colors_Gio, c(0,65) )
  # plot(g_importance_i)
  ggsave(filename = paste0('g_importance_ranking_', var_name_i ,'.png'), plot = g_importance_i, path = output_path, width = 8, height = 8)
  
  
  ####### PARTIAL DEPENDENCE ##########
  # create figures showing the biodiv partial dependence
  
  pp_i <- partialPlot(rf.model, df_comb.train_i, names(df_comb.train_i)[2] ) # second name should be the 
  # head(pp_i)
  pp_i <- as.data.frame(pp_i) ;  pp_i[1:2] <- pp_i[1:2] %>% round( digits = 3)
  # take the variable name and the stat name
  # pp_i[3] <- strsplit(names(df_comb.train)[i], '_')[[1]][length(strsplit(names(df_comb.train)[i], '_')[[1]])] # names(df_comb.train)[i] ; # variable = final item in name
  # pp_i[4] <- paste( strsplit(names(df_comb.train)[i], '_')[[1]][1: (length(strsplit(names(df_comb.train)[i], '_')[[1]]) - 1) ] , collapse = "_" ) # stat =  the 1st and last-1 terms and put together
  pp_i[3] <- str_replace(var_name_i, '_diversity', '')
  if (i == 1){df_partDep_div <- pp_i
  } else{df_partDep_div <- rbind(df_partDep_div, pp_i)}
  
  ####### ICE ############
    if(b_run_dice){ 
      
      df_ice_i <- f_run_ICE(df_comb_i, rf.model, var_name_i) 
      
      # save the overall dice object
      save(df_ice_i, file=paste0(output_path, 'df_dice-',var_name_i, '.RData' )    ) # load(/eos/jeodpp/data/projects/FOREST-RESILIENCE/figures/rf_test_diversity/rf_test_diversity_2023-03-21_plot/df_ice_test-fhd_avg_diversity.RData)
      
      # now can make a plot
      lims_h_in <- F ; lims_h_in <- F
      if(var_name_i == "fhd_avg_diversity")  { lims_h_in <- c(-0.2, 0.2) ; lims_h_in <- c(-0.1, 0.1) }
      if(var_name_i == "kurt_avg_diversity") { lims_h_in <- c(-0.2, 0.2) ; lims_h_in <- c(-0.1, 0.1) }
      
      # if no limits provided then use quantiles 
      # summary(df_ice_i) ; dim(df_ice_i)
      if(lims_m_i == F)  lims_m_i <- quantile( df_ice_i[['actual_deriv']] , probs = c(0.15, 0.85))
      # lims_m_i <- c(-0.1,0.1) ; # fhd_avg_diversity
      # lims_m_i <- c(-0.075,0.075) ; # kurtosis
      g_input <- make_map(df_ice_i, 'actual_deriv', 'ICE partial derivative (actual)', var_name_i, lims_m_i)
      ggsave(plot = g_input, filename = paste0(output_path, 'm_', 'ICE' ,'_', var_name_i, '.png' ) ) # , width = wid, height = hei)
      # make hist
      # lims_h_i <- c(-0.2, 0.2) # fhd_avg_diversity
      # lims_h_i <- c(-0.2, 0.2) # kurtosis
      if(lims_h_i == F)  lims_h_i <- quantile( df_ice_i[['actual_deriv']] , probs = c(0.05, 0.95))
      h_dist <- make_hist(df_ice_i, 'actual_deriv', 'ICE partial derivative', lims_h_i)
      
      # combine map and histogram and save
      g_draw <- ggdraw( clip = 'off') +
        draw_plot(g_input, x = 0.23, y = 0.23, width = 0.77, height = 0.77, hjust = 0.3) +
        draw_plot(h_dist , x = 0, y = 0,    width = 0.77, height = 0.24, hjust = 0 ) 
      # g_draw
      ggsave(plot = g_draw, filename = paste0(output_path, 'g_ICE_comb_', var_name_i, '.png' ) , width = 8, height =10 ) # , width = wid, height = hei)
      
    }
  

  
} # end loop over variables

# df_importance # df_vari_imp # df_partDep_div

##### plot average diversity (and average all variables score) ; Then print the different biodiv importances
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

# extract keywords
df_importance_avg <- f_add_category(df_importance_avg, 'Variable')
g_importance_avg <- f_importance_ranking(df_importance_avg, 'Variable', 'Variable', 'Importance', '% increase MSE', point_color = group.colors_Gio , c(0,60))
# plot(g_importance_avg)
ggsave(filename = paste0('g_importance_ranking_mean.png'), plot = g_importance_avg, path = output_path, width = 8, height = 8)



##### PLOT PARTIAL DEPENDENCE 
head(df_partDep_div) ; summary(df_partDep_div, 16)
names(df_partDep_div) <- c( "value" , "TAC", "variable")
df_partDep_div$variable <- as.factor(df_partDep_div$variable)
save(df_partDep_div, file=paste0(output_path, 'df_rf_model_partDep.RData' )    )

# exclude some that go outside x limits
var_group_1 <- c('rh50_avg', 'mu_dissimilarity')
var_group_2 <- c('skew_cv', 'kurt_cv')
# var_group_3 <- c('kurt_avg', )
df_partDep_div_plot <- df_partDep_div  %>% filter(!  variable %in%  var_group_2 )   %>% filter(  !variable %in%  var_group_2 ) # exclude with !
ggp <- ggplot( df_partDep_div_plot , aes(value, TAC, color = variable)) +    # Draw ggplot2 plot with one axis
  geom_line( size = 1) +
  xlab('Diversity metric') +
  ylim( c(0.28,0.31) ) +
  # scale_color_manual(values=group.colors) +
  basic_graph_theme
# ggp
ggsave(filename = paste0('g_partDep_diversityMetric_4.png'), plot = ggp, path = output_path, width = 8, height = 8)




  # select only relevant predictors
  v_all_vars <- c( v_target, var_name_i, v_predictors) # v_identifiers
  # select only those columns
  # df_comb_i <- df_comb[, v_all_vars]
  # head(df_comb_i)
  
  df_comb.train_i <- df_comb.train[, v_all_vars]
  
  # rf.model <- randomForest(!!as.symbol(v_target) ~ . , data = df_comb.train,  importance = TRUE)
  rf.model <- randomForest(tac_resid_kndvi ~ . , data = df_comb.train_i, mtry = mtry_1, ntree = ntree_1, importance = TRUE)
  rf_end_time <- Sys.time() ; print(rf_end_time)      # initialise time
  rf_duration <- rf_end_time - rf_start_time ; print(rf_duration)
  
  save(rf.model, file=paste0(output_path, 'rf_model_div-',var_name_i, '.RData' )    )
  

###################################################
######     OBS VS MODEL TAC                   #####
###################################################


###################################################
######     IMPORTANCE                         #####
###################################################

print(rf.model) ; summary(rf.model)
rf.model$importance ; importance(rf.model)
rf.model$importanceSD
rf.model$mse

df_vari_imp <- as.data.frame(importance(rf.model))
df_vari_imp <- df_vari_imp[order(df_vari_imp$`%IncMSE`, decreasing = T),]
varImpPlot(rf.model)[1]
save(varImpPlot(rf.model), file=paste0(output_path, 'rf_importancePlot.RData' )    )

###################################################
######     PARTIAL DEPENDENCE                 #####
###################################################

df_partDep <- data.frame()

# create single dataframe of TAC values and 
for (i in 2:length(names(df_comb.train))) { # var_i <- "mu_treecover" ; var_i <- "mu_kndvi"
  print(i)
  pp_i <- partialPlot(rf.model, df_comb.train, names(df_comb.train)[i] ) # df_comb.train[2]  eval(parse(var_i))
  # head(pp_i)
  pp_i <- as.data.frame(pp_i) ;  pp_i[1:2] <- pp_i[1:2] %>% round( digits = 3)
  # take the variable name and the stat name
  pp_i[3] <- strsplit(names(df_comb.train)[i], '_')[[1]][length(strsplit(names(df_comb.train)[i], '_')[[1]])] # names(df_comb.train)[i] ; # variable = final item in name
  pp_i[4] <- paste( strsplit(names(df_comb.train)[i], '_')[[1]][1: (length(strsplit(names(df_comb.train)[i], '_')[[1]]) - 1) ] , collapse = "_" ) # stat =  the 1st and last-1 terms and put together
  names(pp_i) <- c( "value" , "TAC", "variable", "stat")
  if (i == 1){df_partDep <- pp_i
  } else{df_partDep <- rbind(df_partDep, pp_i)}
  
}

tail(df_partDep)
save(df_partDep, file=paste0(output_path, 'rf_model_gioCp_spei_partDep.RData' )    )

# set colours to match Giovanni's 
#                           purple                 green          brown             red             blue              yellow
group.colors <- c(kndvi = "#9933ff", treecover = "#00cc00", ssr ="#734d26", t2m = "#e60000", tp = "#0000ff", spei = "#ffcc00")

# ForDens figure
stat_i <- 'mu' ; vars_i <- 'kndvi' ; #vars_i <- c('kndvi', 'treecover')
ggp <- ggplot( df_partDep %>% filter(variable %in%  vars_i & stat == stat_i), aes(value, TAC, color = variable)) +    # Draw ggplot2 plot with one axis
  geom_line( size = 1) +
  xlab(stat_i) +
  scale_color_manual(values=group.colors) +
  basic_graph_theme
ggp
ggsave(plot = ggp, filename = paste0(output_path, 'g_partDep_', stat_i, '_', vars_i, '.png' ) ) # , width = wid, height = hei)

# residuals figure
stat_i <- 'tac_resid'
ggp <- ggplot(df_partDep %>% filter(stat == stat_i), aes(value, TAC, color = variable)) +    # Draw ggplot2 plot with one axis
  geom_line( size = 1) +
  xlab(stat_i) +
  scale_color_manual(values=group.colors) +
  basic_graph_theme

ggsave(plot = ggp, filename = paste0(output_path, 'g_partDep_', stat_i, '.png' ) ) # , width = wid, height = hei)

# climate variability figure
stat_i <- 'cv' ; # vars_i <- c('kndvi', 'treecover')
ggp <- ggplot( df_partDep %>% filter( stat == stat_i & variable == 't2m'), aes(value, TAC, color = variable)) +    # Draw ggplot2 plot with one axis
  geom_line( size = 1) +
  xlab(stat_i) +
  scale_color_manual(values=group.colors) +
  basic_graph_theme
ggp
ggsave(plot = ggp, filename = paste0(output_path, 'g_partDep_', stat_i, '_t2m.png' ) ) # , width = wid, height = hei)


# climate background figure
stat_i <- 'mu' ; vars_i <- 'tp' # vars_i <- c('kndvi', 'treecover')
ggp <- ggplot( df_partDep %>% filter( stat == stat_i & variable == vars_i), aes(value, TAC, color = variable)) +    # Draw ggplot2 plot with one axis
  geom_line( size = 1) +
  xlab(stat_i) +
  scale_color_manual(values=group.colors) +
  basic_graph_theme
ggp
ggsave(plot = ggp, filename = paste0(output_path, 'g_partDep_', stat_i, '_', vars_i, '.png' ) ) # , width = wid, height = hei)



###################################################
######     TESTING                            #####
###################################################

rf.model_predict_test <- as.data.frame( predict(rf.model, df_comb.test[,-c(1)]) )
head(rf.model_predict_test) ; dim(rf.model_predict_test) ; dim(df_comb.test) ;
df_comb_predictVobs <- cbind( df_comb.test[,c(1)], rf.model_predict_test )
head(df_comb_predictVobs)
names(df_comb_predictVobs) <- c( 'observed', 'modelled' )

ggp <- ggplot( df_comb_predictVobs , aes(observed, modelled)) +    # Draw ggplot2 plot with one axis
  geom_point(alpha = 0.01, size = 0.0005, color = 'blue') +
  basic_graph_theme
ggsave(plot = ggp, filename = paste0(output_path, 'g_obsVSmod_TACkndvi_test', '.png' ) ) # , width = wid, height = hei)

v_cor <- round( cor(df_comb_predictVobs[,1], df_comb_predictVobs[,2]), digits = 3)
ggp <- ggp +  annotate("text" , x=0, y=0.7, label= paste0( "\u03C1\u00B2 = ", v_cor, "\nN = ", dim(df_comb_predictVobs)[1]) ) # geom_text(data = df_cor, size=txt_size*1.4, aes(x = lim_upper[1]*text_pos_x, y = lim_upper[2]*0.96), parse = TRUE)  # add text of data
ggsave(plot = ggp, filename = paste0(output_path, 'g_obsVSmod_TACkndvi', '_stats.png' ) ) # , width = wid, height = hei)


# Get lm equation and r2 in text for use on a plot - adapted from https://stackoverflow.com/questions/19699858/ggplot-adding-regression-line-equation-and-r2-with-facet
# x and y are the first two columns of df respectively
lm_eqn = function(df){
  #   x <- paste(names(df)[1]) ; y <- paste(names(df)[2])
  names(df)[1] <- "x" ; names(df)[2] <- "y" 
  # f <- as.formula(y, x, sep = " ~ ")
  
  f <- as.formula(paste( names(df)[2], paste(names(df)[1]), sep = " ~ "))
  #m <- lm( data = df, formula = y ~ x, weights = w ) #, weights = w ) ##old
  m <- lm( data = df, formula = f) #, weights = w ) ## lm 0int
  ##old
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)~"="~r2, 
                   #eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)~"="~r2, 
                   list(a = format(coef(m)[[1]], digits = 2), 
                        b = format(coef(m)[[2]], digits = 2), 
                        r2 = format( sqrt(summary(m)$r.squared), digits = 2))) 
  
  #r = format( sqrt(summary(m)$r.squared), digits = 3)))
  as.character(as.expression(eq));                  #old output end
  # output <- paste0(as.character(as.expression(eq)), lm_rmse_val)
  # return(output)
}

output_lmEqn_df <- function(df, weight=FALSE,  int_zero =F){
  # Input is a df where items [1] = x_var and [2] = y_var and [5] = weights (if weight = FALSE this is not required).
  
  x_var <- names(df)[1] ; y_var <- names(df)[2] 
  # get facet names and set up output list
  df_out <- data.frame(matrix(ncol = 4, nrow = 0))
  x <- c('lm','RMSE')
  colnames(df_out) <- x
  
  use_weight <- FALSE
  if(length(weight) > 0){ use_weight <- TRUE}
  
  lm_eq_val <- NA
  lm_eqn_RMSE_val <- NA
  df_cor <- df
  
  lm_eq_val <- lm_eqn(df_cor, w, int_zero = int_zero)
  
  
  if(use_weight){
    w = df_cor[,5]
    #w = df_cor[,5][[1]]
    if(length(w)[1]>0){
      #f <- as.formula(paste( names(df_cor)[2], paste(names(df_cor)[1]), sep = " ~ ")) # use this to obtain formula by parsing names
      #lm_eq <- lm( data = df_cor, formula = f, weights = w ) #, weights = w )
      lm_eq_val <- lm_eqn(df_cor, w, int_zero = int_zero)
      #lm_eqn_RMSE_val <- lm_eqn_RMSE(df_cor, w, int_zero = int_zero)
      lm_eqn_RMSE_val <- lm_eqn_RMSE_n(df_cor, w, int_zero = int_zero)
      #lm_eq <- lm( data = df_cor, formula = !!as.symbol( names(df_cor)[2] ) ~ !!as.symbol( names(df_cor)[1] ) ) #, weights = w )
    }
  }else{
    #f <- as.formula(paste( names(df_cor)[2], paste(names(df_cor)[1]), sep = " ~ ")) # use this to obtain formula by parsing names
    #lm_eq <- lm( data = df_cor, formula = f ) #, weights = w )
    w = df_cor[,5]
    w[] <- 1
    lm_eq <- lm_eqn(df_cor, w, int_zero = int_zero)
    #lm_eqn_RMSE_val <- lm_eqn_RMSE(df_cor, w, int_zero = int_zero)
    lm_eqn_RMSE_val <- lm_eqn_RMSE_n(df_cor, w, int_zero = int_zero)
    #lm_eq <- lm( data = df_cor, formula = GPP_mean ~ SIFPK_mean )
  }
  # key is the name of the ab combination, and value is the lm_eq output
  #my_list[[paste0(a_i,'_',b_j)]] <- lm_eq
  vec_eq <- c(as.character(a_i),as.character(b_j),lm_eq_val, lm_eqn_RMSE_val)
  df_eq <- data.frame(t(vec_eq))
  colnames(df_eq) <- x
  df_out <- rbind(df_out,df_eq)
  
} #b_names
}   # a_names

return(df_out)
}



# now predictions
rf.model_predict <- as.data.frame( predict(rf.model, df_comb.test[,-c(1)]) )
head(rf.model_predict) ; head(df_comb.test[,-c(1)])
# table(df_comb.test$tac_resid_kndvi, rf.model_predict$`predict(rf.model, df_comb.test)`)
mean (( df_comb.test$tac_resid_kndvi - rf.model_predict$`predict(rf.model, df_comb.test)` ) ^2)

# pred1=predict(rf.model, type = "prob")
# library(ROCR)
# perf = prediction(pred1[,2], mydata$Creditability)



######################3
 # old code


g_importance_div <-
  ggplot(df_importance %>% arrange(diversity) %>% mutate(name=factor(name, levels=name)),
         aes(x=name, y=diversity)) +
  geom_segment( aes(xend=name, yend=0)) +
  geom_point( size=4, color="orange") +
  coord_flip() +
  #scale_y_log10() +
  # theme_bw() +
  theme(text = element_text(size=18),
        #axis.text.x = element_text(size = 20) # angle=90, hjust=1
  ) +
  ylab("% increase MSE") + 
  xlab("Diversity type")+
  basic_graph_theme


g_importance_i <-
  ggplot(df_vari_imp_cols %>% arrange(Importance) %>% mutate(Variable=factor(Variable, levels=Variable)),
         aes(x=Variable, y=Importance,  color = category)) + # , color = category
  geom_segment( aes(xend=Variable, yend=0),  size = 1) + # , color = type
  geom_point( size=4) + #, color="orange") + #  , aes( color = category)
  scale_color_manual(name = "category", values = c(T2M = "red", TP = 'blue', SSR = 'brown', VPD = 'orange', Other = "dark green") )  +
  # scale_color_manual(name = "type",     values = c(mean = "black", TAC = 'grey50', CV = 'grey10') )  +
  coord_flip() +
  #scale_y_log10() + # theme_bw() +
  theme(text = element_text(size=18),
        #axis.text.x = element_text(size = 20) # angle=90, hjust=1
  ) +
  ylab("% increase MSE") + 
  xlab("Variable") +
  basic_graph_theme