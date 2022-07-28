library(sf)
library(tidyverse)
library(mapview)
# library(tmap)
library(ggmap)

## prep preds
preds_all_loc <- "/Users/matthew.harris/Documents/Python_local/traffic-accident-risk-prediction-Utah/data/dataset/2022-04-01-15-29/test/"
preds_all <- read.csv(file.path(preds_all_loc,"SLC_Featurized_all.csv"))
# preds_all_id <- preds_all %>% ## THis will likely mess up time/datre stamps, be careful
#   mutate(id = 1:n())
write.csv(preds_all[1:1000,],file.path(preds_all_loc,"SLC_Featurized_all_1ktest.csv"))
#### end 

osm_dat <- read_sf("~/Documents/Python_local/traffic-accident-risk-prediction-Utah/data/SLC_Featurized.gpkg")

preds <- read_csv("~/Documents/Python_local/traffic-accident-risk-prediction-Utah/data/dataset/2022-04-01-15-29/pred/result-624791ffa6d0842ad091f4cc.csv")

preds_sf <- preds %>% 
  st_as_sf(wkt = "geometry", crs = 4326)

mapview(sample_n(preds_sf,2000))

preds_sf_t <- preds_sf %>% 
  filter(date_time == "2019-03-01 16:00:00")

mapview(preds_sf_t,zcol = "flag_1_PREDICTION")

date_time_units <- unique(preds_sf$date_time)

# api_key <- "YOUR GOOGLE KEY!!!"
# register_google(api_key)
chi_basemap <- get_map(location=c(lon = -111.90830, lat = 40.69313),
                       zoom=12, maptype = 'toner-2011', 
                       source = 'stamen')
# chi_basemap <- get_map(location=c(lon = -111.91516 , lat = 40.64001),
#                        zoom=12, maptype = 'roadmap', 
#                        source = 'google')
bbox <- st_bbox(preds_sf)

for(i in seq_along(date_time_units)){
  cat(i,"\n")
  date_time_i <- date_time_units[i]
  preds_sf_i <- preds_sf %>% 
    filter(date_time == date_time_i)
  p <- ggmap(chi_basemap) +
    geom_sf(data = preds_sf_i, aes(color=flag_1_PREDICTION), inherit.aes = FALSE,
            size = 0.35) +
    scale_color_viridis_c(name = "Accident Risk", option = "A",
                          limits = c(0,1), breaks = seq(0,1,0.2),) +
    theme_minimal() +
    labs(title = unique(preds_sf_i$date_time)) +
    # scale_x_continuous(limits = c(bbox[1],bbox[3])) + 
    # scale_y_continuous(limits = c(bbox[2],bbox[4])) + 
    theme(
      axis.title=element_blank(),
      axis.text=element_blank(),
      axis.ticks=element_blank()
    )
  ggsave(paste0("./plots_featureMode/SLC_featureMode_toner_zoom12NE_frame_",i,".jpg"),
         width = 5, height = 4)
}



p <- ggmap(chi_basemap) +
  geom_sf(data = preds_sf_i, aes(color=flag_1_PREDICTION), inherit.aes = FALSE) +
  scale_color_viridis_c(name = "Accident Risk", option = "A") +
  theme_minimal()
