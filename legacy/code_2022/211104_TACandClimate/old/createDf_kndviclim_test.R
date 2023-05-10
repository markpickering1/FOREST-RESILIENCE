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

df_t2m_NA_xy <- df_t2m_NA %>% dplyr::group_by(x, y) %>% dplyr::summarise(count_i=n(  ) ) #,

ggplot(df_t2m_NA_xy, aes(x=x, y=y,  colour = count_i) ) +
  # geom_bin2d() +
  geom_point() +
  theme_bw()
# seems as though the missing values
dim(df_t2m_NA_xy)







