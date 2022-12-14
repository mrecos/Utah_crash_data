library(sf)
library(tidyverse)
library(mapview)
# library(tmap)
library(ggmap)

osm_dat <- read_sf("~/Documents/Python_local/traffic-accident-risk-prediction-Chicago/data/CHICAGOV2.gpkg")

preds <- read_csv("~/Documents/Python_local/traffic-accident-risk-prediction-Chicago/data/dataset/2022-03-14-12-53/pred/result-622ffdc7f93ffa9be902630e.csv")

preds_sf <- preds %>% 
  st_as_sf(wkt = "geometry", crs = 4326)

mapview(sample_n(preds_sf,2000))

preds_sf_t <- preds_sf %>% 
  filter(date_time == "2021-11-18 16:00:00")

mapview(preds_sf_t,zcol = "flag_1_PREDICTION")

date_time_units <- unique(preds_sf$date_time)

# api_key <- "AIzaSyB1YoJUsPG2--ZpwdxudfN4g8O4CPmu4YI"
# register_google(api_key)
chi_basemap <- get_map(location=c(lon = -87.71697 , lat = 41.89491),
                       zoom=12, maptype = 'roadmap', 
                       source = 'google')
bbox <- st_bbox(preds_sf)

for(i in seq_along(date_time_units)){
  cat(i,"\n")
  date_time_i <- date_time_units[i]
  preds_sf_i <- preds_sf %>% 
    filter(date_time == date_time_i)
  p <- ggmap(chi_basemap) +
    geom_sf(data = preds_sf_i, aes(color=flag_1_PREDICTION), inherit.aes = FALSE) +
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
  ggsave(paste0("./plots_v2/ChicagoV2_frame_",i,".jpg"),
                  width = 5, height = 4)
}



p <- ggmap(chi_basemap) +
  geom_sf(data = preds_sf_i, aes(color=flag_1_PREDICTION), inherit.aes = FALSE) +
  scale_color_viridis_c(name = "Accident Risk", option = "A") +
  theme_minimal()
