# apply some checks on the data

library(dplyr)
library(ggplot2)


p_in <- '/eos/jeodpp/data/projects/FOREST-RESILIENCE/data_processing/createDF_kndviclim/createDF_kndviclim_2022-11-10/'

load(paste0(p_in,'df_kndvi_full.RData'))
df_kndvi <- df_i 
load(paste0(p_in,'df_t2m_full.RData'))
df_t2m   <- df_i

head(df_kndvi) ; head(df_t2m)
summary(df_kndvi) ; summary(df_t2m)

df_kndvi_t2m <- left_join(df_kndvi, df_t2m)
head(df_kndvi_t2m) ; dim(df_kndvi_t2m) ; summary(df_kndvi_t2m)

df_t2m_NA <- df_kndvi_t2m %>% filter( is.na(t2m) )
head(df_t2m_NA) ; summary(df_t2m_NA)
hist(as.numeric(df_t2m_NA$date))
hist(df_t2m_NA$x) ; hist(df_t2m_NA$y) 

df_t2m_xy <- df_kndvi_t2m %>% dplyr::group_by(x, y) %>% dplyr::summarise(count_i=n(  ) ) #,
head(df_t2m_xy)

df_t2m_NA_xy <- df_t2m_NA %>% dplyr::group_by(x, y) %>% dplyr::summarise(count_i=n(  ) ) #,

ggplot(df_t2m_xy, aes(x=x, y=y,  colour = count_i) ) +
  # geom_bin2d() +
  geom_point() +
  theme_bw()
# seems as though the missing values
dim(df_t2m_NA_xy)

library(terra)
r_t2m_NA <- terra::rast(  as.matrix(df_t2m_NA_xy) , type="xyz")
plot()



####################################################################3
 # new plot

p_in <- '/eos/jeodpp/data/projects/FOREST-RESILIENCE/data_processing/createDF_kndviclim/createDF_kndviclim_2023-01-11/df_kndvi_baseVar_full.RData'
load(paste0(p_in))
head(df_var) ; dim(df_var)
# summary(df_var) 
df_var_1 <- df_var %>% filter(date == '2014-05-04' ) # 2003-01-04
summary(df_var_1) 
stat_j <- 'kndvi'

g_input <-  ggplot() +
  geom_tile(data = df_var_1, aes_string(x = 'x', y = 'y', fill = stat_j)) + # add the raster data
  # geom_sf(data = land_shapefile) + # , color = 'black', size =2) + # add the shapefile (place the base layer first)
  labs( x='', y='', # x= 'latitude', y = 'longitude', 
        fill = paste0( stat_j ) ) + #, '_', gsub('_',' ', stat_j) ) ) + # label axes and title
  # coord_sf() + # align the coordinates to those of shapefile?
  # coord_equal() + # different crs
  # discrete_fill_viridis_c() + # alternative colour system
  scale_fill_distiller( palette =  'Spectral', na.value = "white", oob=scales::squish) #+ # set the colour scheme and palette # direction = 1,
  # scale_fill_distiller(limits=c(-1, 1), palette =  'YlGn', direction = 1,    
  #                      trans = "log",  breaks = scales::trans_breaks('log10', function(x) round(10 ^ x, digits=1), n=3)    ) + # , guide = guide_colorbar(frame.colour = "black")
  # theme_void() +
  # theme_fig  


ggsave(plot = g_input, filename = paste0(output_path, 'g_', var_i ,'_', stat_j, '.png' ) ) # , width = wid, height = hei)
