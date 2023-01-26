#########################################
##### moving_average_swvl1.sh       #####
#########################################
# algorithm description
# 1) create a 16-day moving daily average from swvl1 files via timeselmean and daymean
# 2) two separate .nc moving average files initialised on 1st and 9th day (i.e. 8 day separation as in MODIS)
# 3) recombine these .nc files into single, ordered .nc
# 4?)MODIS compositions are labelled from 1st day. cdo timeselmean labelled from middle day - correct for this or bear in mind for later
# 5) Final entry will likely be incorrect as doesn't role into following year - might need to separately run on overlapping years - but realistically it's ~2 missing days in a 16-day moving window

IN_PATH="/eos/jeodpp/data/base/Meteo/GLOBAL/ECMWF/Reanalysis/ERA5-Land/VER1-0/Data/NetCDF/volumetric_soil_water_layer_1/"
OUT_PATH="/eos/jeodpp/data/projects/FOREST-RESILIENCE/data/climate/8day/SM/SM/"

for f in ${IN_PATH}*.nc ; do

    FILE=$(basename -- $f)   # get filename
    #    DIR=$(dirname -- $f)     # get file directory
    echo ${f} # print running file
    #    echo ${FILE}

    FILE_BASE=$(echo $FILE| cut -d'.' -f 1)
    YEAR=$(echo $FILE_BASE| cut -d'_' -f 9) # split file by _ and extract 6th element
    echo ${YEAR}

    # when repeating script - this catches only failed years and re-trys
        if [[ ${YEAR} != '2009' ]]
        then
                echo "Number ${YEAR}!"
                # break # - completely end loop
		continue # skip to next line
        fi

    
    # first create the running mean starting day 1
    cdo -timselmean,8 -daymean ${IN_PATH}${FILE} ${OUT_PATH}cds_era5_swvl1_${YEAR}_temp_daymean_timsel8.nc
    # cds_era5_land_volumetric_soil_water_layer_1_2001.nc ${OUT_PATH}cds_era5_land_volumetric_soil_water_layer_1_2001_daymean_timsel16.nc


    # NO LONGER NECESSARY
    # next create the second running mean starting day 8 
#    cdo -timselmean,16,8 -daymean ${IN_PATH}${FILE} ${OUT_PATH}cds_era5_swvl1_${YEAR}_temp_daymean_timsel16-8_2.nc
    #    recombine
    # next shift the days back by 8
    # this accounts for the MovWindow date being centred in cdo but MW is start date in MODIS
    # do not do yet

    # next 
    

done
