#########################################
##### 4_europe_deSeason_reMap.sh              #####
#########################################
# algorithm description
# 1) clip file to Europe lon,-10.66164,44.82037 --dmn lat,34.56369,71.18416
# 2) deseasonalise by basic method taking average at timestamp and subtracting
# 3) increase resolution of both deseasonalised and normal
# 4) apply forest mask and/or kndvi filled value mask

VAR="SM"    # this is the name of the variable in dir structure
VAR_nc="swvl1" # this is the name of the variable in the netcdf
IN_DIR="/eos/jeodpp/data/projects/FOREST-RESILIENCE/data/climate/8day/"${VAR}"/"${VAR}"_2_align/"
OUT_DIR="/eos/jeodpp/data/projects/FOREST-RESILIENCE/data/climate/8day/"${VAR}"/"${VAR}"_3_europe/"

#0.05 grid europe
TAR_GRD="/eos/jeodpp/data/projects/FOREST-RESILIENCE/data/climate/8day/other/targetgrid/targetgrid_europe_kNDVIhiResReplica_lonlat_005_1109x732_-10.665tox_34.56toy.txt"

SUB_DIR_1="crop/"
SUB_DIR_2="deseason/"
SUB_DIR_3="resolution/"

## 1) clip ncdf to Europe
#ncks -4 --dmn lon,-10.66164,44.82037 --dmn lat,34.56369,71.18416 ${IN_DIR}${VAR_nc}_allY_timsel8.nc ${OUT_DIR}${SUB_DIR_1}${VAR}_allY_timsel8_europe1.nc
#
## 2) deseasonalise
#cdo ydaymean ${OUT_DIR}${SUB_DIR_1}${VAR}_allY_timsel8_europe1.nc ${OUT_DIR}${SUB_DIR_2}${VAR}_allY_timsel8_europe1_seasonalMean.nc 
#cdo ydaysub ${OUT_DIR}${SUB_DIR_1}${VAR}_allY_timsel8_europe1.nc ${OUT_DIR}${SUB_DIR_2}${VAR}_allY_timsel8_europe1_seasonalMean.nc ${OUT_DIR}${SUB_DIR_2}${VAR}_allY_timsel8_europe2.nc 


# 3) increase resolution to that of kndvi for analysis
# cdo remapbil,${TAR_GRD} ${OUT_DIR}${SUB_DIR_2}${VAR}_allY_timsel8_europe2.nc ${OUT_DIR}${SUB_DIR_3}${VAR}_allY_timsel8_europe3.nc
# cdo remapcon,${TAR_GRD} ${OUT_DIR}${SUB_DIR_2}${VAR}_allY_timsel8_europe2.nc ${OUT_DIR}${SUB_DIR_3}${VAR}_allY_timsel8_europe3.nc
# cdo remapnn,${TAR_GRD} ${OUT_DIR}${SUB_DIR_2}${VAR}_allY_timsel8_europe2.nc ${OUT_DIR}${SUB_DIR_3}${VAR}_allY_timsel8_europe3nn_deseason.nc

cdo remapnn,${TAR_GRD}  ${OUT_DIR}${SUB_DIR_1}${VAR}_allY_timsel8_europe1.nc ${OUT_DIR}${SUB_DIR_3}${VAR}_allY_timsel8_europe3nn_baseVar.nc

# 4) apply mask

