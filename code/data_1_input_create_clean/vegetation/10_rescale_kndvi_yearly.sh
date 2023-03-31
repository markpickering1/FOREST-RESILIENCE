#!/bin/bash

##############################################
## rescale a netCDF file to lower resolution                                                                        
##############################################
# algorithm description
# 1) set working directories
# 2) check if ouput directories exist if not create them
# 3) rescale data
# 4) merge yearly files

# 1) set working directories
in_dir="/eos/jeodpp/data/projects/FOREST-RESILIENCE/data/vegetation/8day/KNDVI_no_mask/KNDVI_no_mask_nc_merged_rescaled10_masked/"
in_dir_grid="/eos/jeodpp/data/projects/FOREST-RESILIENCE/GIT-FOREST-RESILIENCE/FOREST-RESILIENCE/code/data_1_input_create_clean/vegetation/targetgrid/"
out_dir="/eos/jeodpp/data/projects/FOREST-RESILIENCE/data/vegetation/8day/KNDVI_no_mask/KNDVI_no_mask_nc_merged_rescaled10_masked_005/"
out_dir_merged="/eos/jeodpp/data/projects/FOREST-RESILIENCE/data/vegetation/8day/KNDVI_no_mask/KNDVI_no_mask_nc_merged_rescaled10_masked_005/merged/"
in_file="merged_kndvi_no_mask_2003_2021_rescaled10_masked50.nc"
in_grid="targetgrid_005.txt"
out_file="merged_kndvi_no_mask_2003_2021_rescaled10_masked50_005.nc"

# 2) check if ouput directory exists if not create it
if [ ! -d ${out_dir} ]; then
  mkdir ${out_dir}
  fi

if [ ! -d ${out_dir_merged} ]; then
  mkdir ${out_dir_merged}
  fi

# 3) rescale data
start_y=2010
end_y=2021

for year in $(seq $start_y $end_y) ; do
	echo $year
	cdo -remapcon,${in_dir_grid}${in_grid} -selyear,${year} ${in_dir}${in_file} ${out_dir}${year}_${out_file}.nc
done

# 4) merge yearly files
cd ${out_dir}
cdo mergetime *.nc ${out_dir_merged}${out_file}