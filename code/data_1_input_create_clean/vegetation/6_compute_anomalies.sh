#!/bin/bash

##############################################
## compute anomalies of an input time-series file                                                                       
##############################################
# algorithm description
# 1) set working directories
# 2) check if ouput directories exist if not create them
# 3) calculate climatological average (20 years average for each 8 days timestep)
# 4) subtract climatological average at each time-step to calculate anomalies

# 1) set working directories
in_dir="/eos/jeodpp/data/projects/FOREST-RESILIENCE/data/vegetation/8day/KNDVI_no_mask/KNDVI_no_mask_nc_merged_rescaled10/"
out_ydaymean_dir="/eos/jeodpp/data/projects/FOREST-RESILIENCE/data/vegetation/8day/KNDVI_no_mask/KNDVI_no_mask_nc_merged_rescaled10_ydaymean/"
out_dir="/eos/jeodpp/data/projects/FOREST-RESILIENCE/data/vegetation/8day/KNDVI_no_mask/KNDVI_no_mask_nc_merged_rescaled10_deseasonalised/"
in_file="merged_kndvi_no_mask_2003_2021_rescaled10.nc"

# 2) check if ouput directories exist if not create them
if [ ! -d ${out_ydaymean_dir} ]; then
  mkdir ${out_ydaymean_dir}
  fi

if [ ! -d ${out_dir} ]; then
  mkdir ${out_dir}
  fi

# 3) calculate climatological average (20 years average for each 8 days timestep)
cdo ydaymean ${in_dir}${in_file} ${out_ydaymean_dir}${in_file%.nc}_ydaymean.nc

# 4) subtract climatological average at each time-step to calculate anomalies
cdo ydaysub ${in_dir}${in_file} ${out_ydaymean_dir}${in_file%.nc}_ydaymean.nc ${out_dir}${in_file%.nc}_deseasonalised.nc



