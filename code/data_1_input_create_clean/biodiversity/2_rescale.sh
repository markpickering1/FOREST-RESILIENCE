#!/bin/bash

##############################################
## rescale a netCDF file to lower resolution                                                                        
##############################################
# algorithm description
# 1) set working directories
# 2) check if ouput directories exist if not create them
# 3) rescale data

# 1) set working directories
#in_dir="/eos/jeodpp/data/projects/FOREST-RESILIENCE/data/biodiversity/earthenv/homogeneity_nc/"
in_dir="/eos/jeodpp/data/projects/FOREST-RESILIENCE/data/biodiversity/biomass/nasa_biomass/dissimilarity_homogeneity/"

in_dir_grid="/eos/jeodpp/data/projects/FOREST-RESILIENCE/data/biodiversity/earthenv/scripts/targetgrid/"
in_grid="targetgrid_005.txt"

in_file="biomass_diversity_MW3.nc"
out_file="biomass_diversity_MW3_rescaled_005.nc"

# 2) rescale data
cdo remapbil,${in_dir_grid}${in_grid} ${in_dir}${in_file} ${in_dir}${out_file}


