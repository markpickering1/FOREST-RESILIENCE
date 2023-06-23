# ########################################################
# Title         : merge_df.R
# Description   : Read dataframes and merge them into one single df
# Aims          : Have a unique df inout for modelling
# Inputs	      : Dataframes
# Outputs	      : Dataframe
# Options	      : 
# Date          : 30/01/2023
# Version       : 1.1
# Authors       : Agata Elia & Mark Pickering
# Maintainer    : Agata Elia
# Notes		      : 
# Example use   : 
# ########################################################

library(stringr)
library(dplyr)

# Remove existing loaded objects
rm(list = ls())   

# Set working directory
setwd("/eos/jeodpp/data/projects/FOREST-RESILIENCE/GIT-FOREST-RESILIENCE/FOREST-RESILIENCE/sandbox/agata/createDF_kndviclim_muTACcov_2023-01-26_copy/")
outd <- "/eos/jeodpp/data/projects/FOREST-RESILIENCE/GIT-FOREST-RESILIENCE/FOREST-RESILIENCE/sandbox/agata/"

# Load all the RData objects
file_list <- list.files(pattern = "*.RData")

# Inspect list
print(file_list)

# Load kndvi df
load("df_kndvi_muTACcov.RData")
df_kndvi <- df_stats

# Add variables prefix to df columns
print(head(df_kndvi))
print(colnames(df_kndvi))
colnames(df_kndvi)[3:8] <- paste(colnames(df_kndvi)[3:8], "kndvi", sep="_")
print(colnames(df_kndvi))

# Round values to join
#df_kndvi[1:2] <- df_kndvi[1:2] %>% round( digits = 3)

# Loop through the RData objects, load them, 
#rename column names adding the variable as a prefix and join with kndvi RData object
for (rdata_file in file_list){
  var_name <- str_sub(basename(rdata_file), 4, -16)
  print(var_name)
  if (var_name != "kndvi"){
    load(rdata_file)
    print(head(df_stats))
    print(colnames(df_stats))
    colnames(df_stats)[3:8] <- paste(colnames(df_stats)[3:8], var_name, sep="_")
    print(colnames(df_stats))
    
    # Join (https://www.rdocumentation.org/packages/dplyr/versions/0.7.8/topics/join)
    
    # Round values to join
    #df_stats[1:2] <- df_stats[1:2] %>% round( digits = 3)
    
    # Consider switching to left join or inner (we are only interested in points that have all variables)
    #df_kndvi <- full_join(df_kndvi, df_stats[c(1, 2, 4:6, 8)])
    df_kndvi <- inner_join(df_kndvi, df_stats[c(1, 2, 4:6, 8)])
  }
}

# Inspect results
print(colnames(df_kndvi))
print(head(df_kndvi))

# Save merged output
save(df_kndvi, file=paste0(outd, "df_all_inner_muTACcov.RData"))