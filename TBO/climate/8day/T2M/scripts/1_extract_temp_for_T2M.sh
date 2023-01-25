#########################################
##### 1_extract_temp_for_T2M.sh     #####
#########################################
# algorithm description
# 1) create a 8-day moving daily average for 2m dewpoint temperature
# 2) create a 8-day T2M max and min for 2m temperature

IN_DIR_T="/eos/jeodpp/data/base/Meteo/GLOBAL/ECMWF/Reanalysis/ERA5-Land/VER1-0/Data/NetCDF/2m_temperature/"
OUT_DIR="/eos/jeodpp/data/projects/FOREST-RESILIENCE/data/climate/8day/T2M/T2M/"

FILE_BASE_T="cds_era5_land_2m_temperature_"

START_Y=2020
END_Y=2020 #2019
# 2000 2003 2008-2012

for YEAR in $(seq $START_Y $END_Y) ; do
    echo $YEAR
    
    IN_PATH_T=${IN_DIR_T}${FILE_BASE_T}${YEAR}.nc

    echo ${IN_PATH_T}
    echo ${OUT_PATH}cds_era5_t2m_${YEAR}_daymean_timsel8.nc
    
    # get minimum temperature
    cdo -timselmean,8 -daymean ${IN_PATH_T} ${OUT_DIR}cds_era5_t2m_${YEAR}_daymean_timsel8.nc

done
