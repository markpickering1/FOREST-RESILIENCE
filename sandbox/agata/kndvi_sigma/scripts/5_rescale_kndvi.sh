#!/bin/bash

##############################################
## rescale a netCDF file to lower resolution                                                                        
##############################################
# algorithm description
# 1) set working directories
# 2) check if ouput directories exist if not create them
# 3) rescale data

# 1) set working directories
in_dir="/eos/jeodpp/data/projects/FOREST-RESILIENCE/GIT-FOREST-RESILIENCE/FOREST-RESILIENCE/sandbox/agata/kndvi_sigma/sigma_mean_nc_rescaled10_masked/"
in_dir_grid="/eos/jeodpp/data/projects/FOREST-RESILIENCE/GIT-FOREST-RESILIENCE/FOREST-RESILIENCE/code/data_1_input_create_clean/vegetation/targetgrid/"
out_dir="/eos/jeodpp/data/projects/FOREST-RESILIENCE/GIT-FOREST-RESILIENCE/FOREST-RESILIENCE/sandbox/agata/kndvi_sigma/sigma_mean_nc_rescaled10_masked_005/"
in_file="modisKNDVISimplifiedBandMean_rescaled10_masked50.nc"
in_grid="targetgrid_005.txt"
out_file="modisKNDVISimplifiedBandMean_rescaled10_masked50_005.nc"

# 2) check if ouput directory exists if not create it
if [ ! -d ${out_dir} ]; then
  mkdir ${out_dir}
  fi

# 3) rescale data
cdo remapcon,${in_dir_grid}${in_grid} ${in_dir}${in_file} ${out_dir}${out_file}


