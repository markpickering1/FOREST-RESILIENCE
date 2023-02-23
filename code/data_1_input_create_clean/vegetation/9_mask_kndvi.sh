#!/bin/bash

####################################################
## mask the rescaled kndvi and the kndvi anomalies time series with forest cover                                                                      
####################################################
# algorithm description
# 1) set working directories
# 2) check if ouput directories exist if not create them
# 3) mask the rescaled kndvi time series
# 4) mask the kndvi anomalies time series

# 1) set working directories
in_kndvi_dir="/eos/jeodpp/data/projects/FOREST-RESILIENCE/data/vegetation/8day/KNDVI_no_mask/KNDVI_no_mask_nc_merged_rescaled10/"
in_deseas_dir="/eos/jeodpp/data/projects/FOREST-RESILIENCE/data/vegetation/8day/KNDVI_no_mask/KNDVI_no_mask_nc_merged_rescaled10_deseasonalised/"
in_mask_dir="/eos/jeodpp/data/projects/FOREST-RESILIENCE/data/ancillary/hansen/hansen_forest_cover_nc_mask/"
out_kndvi_dir="/eos/jeodpp/data/projects/FOREST-RESILIENCE/data/vegetation/8day/KNDVI_no_mask/KNDVI_no_mask_nc_merged_rescaled10_masked/"
out_deseas_dir="/eos/jeodpp/data/projects/FOREST-RESILIENCE/data/vegetation/8day/KNDVI_no_mask/KNDVI_no_mask_nc_merged_rescaled10_deseasonalised_masked/"
in_kndvi="merged_kndvi_no_mask_2003_2021_rescaled10.nc"
in_deseas="merged_kndvi_no_mask_2003_2021_rescaled10_deseasonalised.nc"
in_mask="hansenForestCoverNoLoss2000AtModisMean_binary50.nc"

# 2) check if ouput directories exist if not create them
if [ ! -d ${out_kndvi_dir} ]; then
  mkdir ${out_kndvi_dir}
  fi

if [ ! -d ${out_deseas_dir} ]; then
  mkdir ${out_deseas_dir}
  fi

# 3) mask the rescaled kndvi time series
cdo ifthen ${in_mask_dir}${in_mask} ${in_kndvi_dir}${in_kndvi} ${out_kndvi_dir}${in_kndvi%.nc}_masked50.nc

# 4) mask the kndvi anomalies time series
cdo ifthen ${in_mask_dir}${in_mask} ${in_deseas_dir}${in_deseas} ${out_deseas_dir}${in_deseas%.nc}_masked50.nc



