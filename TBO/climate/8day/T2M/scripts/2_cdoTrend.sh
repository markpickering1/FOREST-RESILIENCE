# apply regression and trend stats to T2M multiyear
# note - I tried a number of different methods here thinking that it wasn't working correctly when actually it was, I just need to change the range on the axis. Mostly we see around a degree of warming over around 20 years

ROOT_DIR="/eos/jeodpp/data/projects/FOREST-RESILIENCE/data/climate/8day/T2M/"
INPUT_DIR=${ROOT_DIR}/"T2M/"
OUTPUT_DIR=${ROOT_DIR}"T2M_trend/"

echo ${INPUT_DIR}
echo ${OUTPUT_DIR}

# merge all years to single file (might be useful as chaining operations seems to fail
# cdo mergetime ${INPUT_DIR}cds_era5_t2m_*_daymean_timsel8.nc ${INPUT_DIR}cds_era5_t2m_2002-2021.nc
# then apply the regression (non chained operator, with increased precision and without equal spacing)
cdo -b F64 regres,equal=false ${INPUT_DIR}cds_era5_t2m_2002-2021.nc  ${OUTPUT_DIR}cds_era5_t2m_regresbF64equalF_merged2002-2021.nc


# taking the 8 timestep running mean (old code)
# cdo -timselmean,8 -mergetime ${INPUT_DIR_YEAR}GPP_FluxSat_daily_v2_${YEAR}*.nc4

# run a basic linear regression on the data (outputs the m parameter in mx+c) running on the complete set of years via chained operations
# cdo -regres -mergetime ${INPUT_DIR}cds_era5_t2m_*_daymean_timsel8.nc ${OUTPUT_DIR}cds_era5_t2m_regres.nc

# increase the precision  running on the complete set of years via chained operations
# cdo -b F64 -regres -mergetime ${INPUT_DIR}cds_era5_t2m_*_daymean_timsel8.nc ${OUTPUT_DIR}cds_era5_t2m_regresbF64.nc

# run on complete set of years (with increased precision and without equal spacing)
# cdo -b F64 -regres,equal=false -mergetime ${INPUT_DIR}cds_era5_t2m_*_daymean_timsel8.nc ${OUTPUT_DIR}cds_era5_t2m_regresbF64equalF.nc

# create example year and run regression only on that
# this gives more success
# cdo -b F64 -regres ${INPUT_DIR}cds_era5_t2m_2007_daymean_timsel8.nc ${OUTPUT_DIR}cds_era5_t2m_regresbF64_2007.nc
