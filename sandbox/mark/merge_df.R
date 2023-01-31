rm(list = ls())                                   # remove loaded objects

library(stringr)
library(dplyr)

# Set working directory
# setwd("/eos/jeodpp/data/projects/FOREST-RESILIENCE/GIT-FOREST-RESILIENCE/FOREST-RESILIENCE/sandbox/mark/")
dir_in <- '/eos/jeodpp/data/projects/FOREST-RESILIENCE/data_processing/createDF_kndviclim_muTACcov/createDF_kndviclim_muTACcov_2023-01-26/'

# Load all the RData objects
file_list <- list.files(path = dir_in, pattern = "*.RData")

# Inspect list
print(file_list)

# Load kndvi df
load(paste0(dir_in, "df_kndvi_muTACcov.RData"))
df_kndvi <- as.data.frame(df_stats)
head(df_kndvi)

# Add variables prefix to df columns
print(head(df_kndvi))
print(colnames(df_kndvi))
colnames(df_kndvi)[3:8] <- paste(colnames(df_kndvi)[3:8], "kndvi", sep="_")
print(colnames(df_kndvi))

# round values to join
df_kndvi[1:2] <- df_kndvi[1:2] %>% round( digits = 3)

# Loop through the RData objects, load them, 
#rename column names adding the variable as a prefix and join with kndvi RData object
for (rdata_file in file_list){ # rdata_file <- 'df_t2m_muTACcov.RData'
  var_name <- str_sub(basename(rdata_file), 4, -16)
  print(var_name)
  if (var_name != "kndvi"){
    load(paste0(dir_in,  rdata_file) )
    df_stats <- as.data.frame(df_stats)
    print(head(df_stats))
    print(colnames(df_stats))
    colnames(df_stats)[3:8] <- paste(colnames(df_stats)[3:8], var_name, sep="_")
    print(colnames(df_stats))
    # Join (https://www.rdocumentation.org/packages/dplyr/versions/0.7.8/topics/join)
    
    # round values to join
    df_stats[1:2] <- df_stats[1:2] %>% round( digits = 3)
    
    # switch to left_join (we are only interested in points which have kndvi values)
    df_kndvi <- left_join(df_kndvi, df_stats)
  }
}

# Inspect results
print(colnames(df_kndvi))
print(head(df_kndvi))

# Save merged output
save(df_kndvi, file=paste0("df_all_muTACcov.RData"))