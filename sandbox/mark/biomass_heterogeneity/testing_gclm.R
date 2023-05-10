library(raster)
library(glcm) 
f_tif <- '/eos/jeodpp/data/projects/FOREST-RESILIENCE/data/vegetation/8day/KNDVI/KNDVI/2016_6_12_kNDVI.tif'
f_bio <- '/eos/jeodpp/data/projects/FOREST-RESILIENCE/data/ancillary/biomass/nasa_biomass/agbRescaledAtModis.tif'

r_image <- raster::raster(f_tif)
plot(r_image)
r_image
r_image[r_image==-32767] <- NA


# calculate glcm
r_glcm <- glcm(r_image, window = c(3,3), 
               na_opt = 'ignore',                            # ignore NAs (can later apply MW remove)
               statistics = c("dissimilarity"),              # statistics to calculate
             shift=list(c(0,1), c(1,1), c(1,0), c(1,-1)))  # over all directions with shift=list(c(0,1), c(1,1), c(1,0), c(1,-1))

plot(r_glcm)
save(r_glcm, file = '/eos/jeodpp/data/projects/FOREST-RESILIENCE/GIT-FOREST-RESILIENCE/FOREST-RESILIENCE/sandbox/mark/test_r_dissimilarity.RDATA')

# calculate glcm
r_glcm_h <- glcm(r_image, window = c(3,3), 
               na_opt = 'ignore',                            # ignore NAs (can later apply MW remove)
               statistics = c("homogeneity"),                # statistics to calculate
               shift=list(c(0,1), c(1,1), c(1,0), c(1,-1)))  # over all directions with shift=list(c(0,1), c(1,1), c(1,0), c(1,-1))
plot(r_glcm_h)
save(r_glcm_h, file = '/eos/jeodpp/data/projects/FOREST-RESILIENCE/GIT-FOREST-RESILIENCE/FOREST-RESILIENCE/sandbox/mark/test_r_dissimilarity.RDATA')



r_image <- raster::raster(f_bio)
plot(r_image)
r_image
r_image[r_image==-32767] <- NA


# calculate glcm
r_glcm <- glcm(r_image, window = c(3,3), 
               na_opt = 'ignore',                            # ignore NAs (can later apply MW remove)
               statistics = c("dissimilarity"),              # statistics to calculate
               shift=list(c(0,1), c(1,1), c(1,0), c(1,-1)))  # over all directions with shift=list(c(0,1), c(1,1), c(1,0), c(1,-1))

plot(r_glcm)
save(r_glcm, file = '/eos/jeodpp/data/projects/FOREST-RESILIENCE/GIT-FOREST-RESILIENCE/FOREST-RESILIENCE/sandbox/mark/test_r_dissimilarity_biomass.RDATA')

# calculate glcm
r_glcm_h <- glcm(r_image, window = c(3,3), 
                 na_opt = 'ignore',                            # ignore NAs (can later apply MW remove)
                 statistics = c("homogeneity"),                # statistics to calculate
                 shift=list(c(0,1), c(1,1), c(1,0), c(1,-1)))  # over all directions with shift=list(c(0,1), c(1,1), c(1,0), c(1,-1))
plot(r_glcm_h)
save(r_glcm_h, file = '/eos/jeodpp/data/projects/FOREST-RESILIENCE/GIT-FOREST-RESILIENCE/FOREST-RESILIENCE/sandbox/mark/test_r_dissimilarity_biomass.RDATA')
