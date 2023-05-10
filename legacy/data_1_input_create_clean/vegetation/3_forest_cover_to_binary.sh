#!/bin/bash

##########################################
## reclassify input forest cover to binary map with threshold              
##########################################
# algorithm description
# 1) set working directories
# 2) check if ouput directory exists if not create it
# 3) apply a reclassification expression to input file

# 1) set working directories
#in_dir=${ROOT_DATA_INPUT}/ancillary/hansen/hansen_forest_cover_nc/
#out_dir=${ROOT_DATA_INPUT}/ancillary/hansen/hansen_forest_cover_nc_mask/
in_dir="/eos/jeodpp/data/projects/FOREST-RESILIENCE/data/ancillary/hansen/hansen_forest_cover_nc/"
out_dir="/eos/jeodpp/data/projects/FOREST-RESILIENCE/data/ancillary/hansen/hansen_forest_cover_nc_mask/"
in_file="hansenForestCoverNoLoss2000AtModisMean.nc"
out_file="hansenForestCoverNoLoss2000AtModisMean_binary_50.nc"

# 2) check if ouput directory exists if not create it
if [ ! -d ${out_dir} ]; then
  mkdir ${out_dir}
  fi

# 3) apply a reclassification expression to input file
cdo -expr,'forestcover=(forestcover < 0.5) ? 0 : 1' ${in_dir}${in_file} ${out_dir}${out_file}
