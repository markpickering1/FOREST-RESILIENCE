#!/bin/bash

####################################################
## mask the rescaled kndvi and the kndvi anomalies time series with forest cover                                                                      
####################################################
# algorithm description
# 1) set working directories
# 2) check if ouput directories exist if not create them
# 3) mask the rescaled kndvi 

# 1) set working directories
in_kndvi_dir="/eos/jeodpp/data/projects/FOREST-RESILIENCE/GIT-FOREST-RESILIENCE/FOREST-RESILIENCE/sandbox/agata/kndvi_sigma/sigma_mean_nc_rescaled10/"
in_mask_dir="/eos/jeodpp/data/projects/FOREST-RESILIENCE/data/static_variables/hansen/hansen_forest_cover_nc_mask/"
out_kndvi_dir="/eos/jeodpp/data/projects/FOREST-RESILIENCE/GIT-FOREST-RESILIENCE/FOREST-RESILIENCE/sandbox/agata/kndvi_sigma/sigma_mean_nc_rescaled10_masked/"
in_kndvi="modisKNDVISimplifiedBandMean_rescaled10.nc"
in_mask="hansenForestCoverNoLoss2000AtModisMean_binary50.nc"

# 2) check if ouput directories exist if not create them
if [ ! -d ${out_kndvi_dir} ]; then
  mkdir ${out_kndvi_dir}
  fi

# 3) mask the rescaled kndvi time series
cdo ifthen ${in_mask_dir}${in_mask} ${in_kndvi_dir}${in_kndvi} ${out_kndvi_dir}${in_kndvi%.nc}_masked50.nc




