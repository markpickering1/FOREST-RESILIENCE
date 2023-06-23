#!/bin/bash

##############################################
## bring back kndvi to native scale dividing by 10000                                                                       
##############################################
# algorithm description
# 1) set working directories
# 2) check if ouput directory exists if not create it
# 3) rescale kndvi values

# 1) set working directories
#in_dir=${ROOT_DATA_INPUT}/vegetation/8day/KNDVI_no_mask/KNDVI_no_mask_nc_merged/
#out_dir=${ROOT_DATA_INPUT}/vegetation/8day/KNDVI_no_mask/KNDVI_no_mask_nc_merged_rescaled10/
in_dir="/eos/jeodpp/data/projects/FOREST-RESILIENCE/GIT-FOREST-RESILIENCE/FOREST-RESILIENCE/sandbox/agata/kndvi_sigma/sigma_mean_nc/"
out_dir="/eos/jeodpp/data/projects/FOREST-RESILIENCE/GIT-FOREST-RESILIENCE/FOREST-RESILIENCE/sandbox/agata/kndvi_sigma/sigma_mean_nc_rescaled10/"
in_file="modisKNDVISimplifiedBandMean.nc"
out_file="modisKNDVISimplifiedBandMean_rescaled10.nc"

# 2) check if ouput directory exists if not create it
if [ ! -d ${out_dir} ]; then
  mkdir ${out_dir}
  fi

# 3) rescale kndvi values
cdo divc,10000  ${in_dir}${in_file} ${out_dir}${out_file}