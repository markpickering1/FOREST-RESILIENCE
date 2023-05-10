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
script_info      <- 'rf_test_diversity'               # used in output name
script_info_input <- 'createDF_kndviclim_muTACcov'                 # load input dataframes containing full data and residuals
# input_script_date <- '2023-01-26'                        # the 0.05 production after creating the GIT (first kndvi version)
input_script_date <- '2023-03-14_diversity1st'           # the 0.05 production with second kndvi version and heterogeneity+diversity data

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
v_optional_predictors <- c( "mu_dissimilarity"  , "skew_avg_diversity" , "kurt_avg_diversity" , "rh50_avg_diversity" ,
  "fhd_avg_diversity" , "rh98_avg_diversity" , "skew_cv_diversity" , "kurt_cv_diversity" , "rh50_cv_diversity" ,  "fhd_cv_diversity")

###################################################
######     SET RF PARAMETERS                  #####
###################################################

# SIMPLE Train-Test Split - 30% split to start
set.seed(101)
sample <- sample.split(df_comb$tac_resid_kndvi , SplitRatio = 0.7)
df_comb.train <- subset(df_comb, sample == TRUE)
df_comb.test  <- subset(df_comb, sample == FALSE)
# summary(df_comb.train) ; summary(df_comb.test) 
dim(df_comb.train) ; dim(df_comb.test) ; print(dim(df_comb.train)[1] / (dim(df_comb.test)[1] + dim(df_comb.train)[1]))

# parameters to optimise
ntree_1 <- 50                           # to optimise
mtry_1 <- ceiling( (length(v_predictors) + 1 ) /3) # use default and optimise

# save train and test set for later analysis
save(df_comb.train, file=paste0(output_path, 'df_comb.train.RData' )    )
save(df_comb.test , file=paste0(output_path, 'df_comb.test.RData' )    )
save(df_comb , file=paste0(output_path, 'df_all.RData' )    )


###################################################
######     RUN                                #####
###################################################
rf_start_time <- Sys.time() ; print(rf_start_time)      # initialise time

# loop over 'diversity metrics' adding each one in turn and running the RF
for (i in 1:length(v_optional_predictors)){ 
  var_name_i <- v_optional_predictors[i]
  print(var_name_i)
  
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

} # end loop over variables

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
