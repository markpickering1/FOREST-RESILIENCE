# ########################################################
# Title         : random_forest.R
# Description   : Read an RData object with response and predictor variables in it, fit a RF regression model,
#                 calculate variable importance metrics and plot variable importance metrics and partial dependance plots
# Aims          : To fit a random forest regression model to data contained in a RData object
# Inputs	      : RData object
# Outputs	      : Tables and plots
# Options	      : 
# Date          : 29/01/2023
# Version       : 1.1
# Authors       : Agata Elia & Mark Pickering
# Maintainer    : Agata Elia & Mark Pickering
# Notes		      : 
# Example use   : 
# ########################################################

# Set directory 
wd = "/eos/jeodpp/data/projects/FOREST-RESILIENCE/GIT-FOREST-RESILIENCE/FOREST-RESILIENCE/sandbox/agata/"

# Remove existing loaded objects
rm(list = ls()) 

# Set source script
source('/eos/jeodpp/data/projects/FOREST-RESILIENCE/GIT-FOREST-RESILIENCE/FOREST-RESILIENCE/code/main/initialise_R.R')

# Global variables
script_info      <- 'rf_test'               # used in output name
script_info_input <- 'createDF_kndviclim_muTACcov'                 # load input dataframes containing full data and residuals
input_script_date <- '2023-01-26'                        # the 0.05 production after creating the GIT (first kndvi version)

# Import relevant libraries
library(dplyr)
library(randomForest)
library(ggplot2)
library(caTools)

# Initialise input file
input_dir <- paste0(root_data_proce, script_info_input, '/', script_info_input, '_', input_script_date,  '/')

# Set/create output directory  - use same as before
output_path <- "/eos/jeodpp/data/projects/FOREST-RESILIENCE/GIT-FOREST-RESILIENCE/FOREST-RESILIENCE/sandbox/agata/"
#print(paste0('output_path is : ', output_path ))
#if(! dir.exists(output_path)) {dir.create(paste0(output_path),recursive=T)} # create directory if not present

# Read the dataframe containing the response and predictor variables
load( paste0(input_dir, 'df_all.RData') )
summary(df_comb) ; colnames(df_comb)

# Only include variables
v_target <- 'tac_resid_kndvi'
v_identifiers <- c('x', 'y')
v_predictors <- c( 'mu_kndvi', 'mu_treecover',
                   'mu_ssr', 'cv_ssr', 'tac_resid_ssr', 
                   'mu_t2m', 'cv_t2m', 'tac_resid_t2m', 
                   'mu_tp', 'cv_tp', 'tac_resid_tp',
                   'mu_spei', 'cv_spei', 'tac_resid_spei'
)

# Merge predictors and response variables
v_all_vars <- c( v_target, v_predictors) # v_identifiers

# Select only those columns from the df
df_comb <- df_comb[, v_all_vars]
head(df_comb)

# Train-Test Split - 30% split to start
set.seed(101)
sample <- sample.split(df_comb$tac_resid_kndvi , SplitRatio = 0.7)
df_comb.train <- subset(df_comb, sample == TRUE)
df_comb.test  <- subset(df_comb, sample == FALSE)
dim(df_comb.train) ; dim(df_comb.test) ; print(dim(df_comb.train)[1] / (dim(df_comb.test)[1] + dim(df_comb.train)[1]))

# Parameters to optimize
ntree_1 <- 50                           
mtry_1 <- ceiling(length(v_predictors)/3) 

# Initialise time
rf_start_time <- Sys.time() ; print(rf_start_time)      

# Train the random forest model using as response variable the kNDVI TAC (https://www.rdocumentation.org/packages/randomForest/versions/4.7-1.1/topics/randomForest)
rf.model <- randomForest(tac_resid_kndvi ~ . , data = df_comb.train, mtry = mtry_1, ntree = ntree_1, importance = TRUE)

# Initialise time
rf_end_time <- Sys.time() ; print(rf_end_time)     

# Calculate duration
rf_duration <- rf_end_time - rf_start_time ; print(rf_duration)

# Save trained model
save(rf.model, "/eos/jeodpp/data/projects/FOREST-RESILIENCE/GIT-FOREST-RESILIENCE/FOREST-RESILIENCE/sandbox/agata/rf_model_gioCp_spei.RData")

# Explore results of the random forest model fitting
print(summary(rf_model))

# Calculate variable importance (https://www.rdocumentation.org/packages/LaplacesDemon/versions/16.1.6/topics/Importance)
var_imp <- importance(rf_model)
print(var_imp)

# Turn the variable importance table into a dataframe
var_imp_df <- data.frame(variable = rownames(var_imp), importance = var_imp[, 1])

# Create list of variables sorted by importance
imp_var_sorted <- rownames(var_imp)[order(var_imp[, 1], decreasing=TRUE)]

# Plot the variables importance sorted by importance
var_imp_plot <- ggplot(var_imp_df, aes(x = reorder(variable, importance), y = importance)) +
                       geom_bar(stat = "identity") + 
                       coord_flip() + 
                       ggtitle("Variable Importances")

# Save the variables importance plot
ggsave(plot = var_imp_plot, filename = paste0(wd, 'variables_importance.png'))#, width = 10, height = 10)

# Calculate and plot partial dependence for one of the variables (https://www.rdocumentation.org/packages/randomForest/versions/4.7-1.1/topics/partialPlot)
partialPlot(rf_model, data.frame(variables), cv_var_spei, xlab="cv_var_spei", 
            main=paste("Partial Dependence on", "cv_var_spei"),
            ylim=c(30, 70))

# Calculate and plot partial dependence for all the variables
op <- par(mfrow=c(2, 3)) #???
for (i in seq_along(imp_var_sorted)) {
  partialPlot(rf_model, data.frame(variables), imp_var_sorted[i], xlab=imp_var_sorted[i],
              main=paste("Partial Dependence on", imp_var_sorted[i]),
              ylim=c(30, 70))
}
par(op) # ????

# Check the prediction accuracy on some test data? (https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/predict)
kndvi_predict <- predict(rf_model, newdata = variables[-train, ])
MSE <- (kndvi_predict - variables[-train, "kNDVITAC"])
print(MSE)
plot(kndvi_predict, variables[-train, "kNDVITAC"])

# # Example as a function
# random_forest_regression <- function(formula, dataset, ntree = 500, mtry = sqrt(ncol(x_train))) {}