#!/bin/bash

##############################################
## merge a list of nc files                                                                       
##############################################
# algorithm description
# 1) set working directories
# 2) check if ouput directory exists if not create it
# 3) merge all yearly nc files from 2003 to 2021 (to have only complete years)

# 1) set working directories
in_dir="/eos/jeodpp/data/projects/FOREST-RESILIENCE/data/vegetation/8day/KNDVI_no_mask/KNDVI_no_mask_nc/"
out_dir="/eos/jeodpp/data/projects/FOREST-RESILIENCE/data/vegetation/8day/KNDVI_no_mask/KNDVI_no_mask_nc_merged/"
#in_dir=${ROOT_DATA_INPUT}/vegetation/8day/KNDVI_no_mask/KNDVI_no_mask_nc/
#out_dir=${ROOT_DATA_INPUT}/vegetation/8day/KNDVI_no_mask/KNDVI_no_mask_nc_merged/
out_file="merged_kndvi_no_mask_2003_2021.nc"

# 2) check if ouput directory exists if not create it
if [ ! -d ${out_dir} ]; then
  mkdir ${out_dir}
  fi

# 3) merge all yearly nc files from 2003 to 2021 (to have only complete years)
cd ${in_dir}
cdo mergetime *.nc ${out_dir}${out_file}