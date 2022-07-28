# need to run notebooks 1 to get the OSM data
# conceptually, the difficulty is navigating between multiple
# units of analysis. Issue is that we start with points (accidents)
# but need to end with lines (segments) as we are predicting 
# accidents per segment
# including 
# accidents as points
# roads as lines
# hexes as polygons
# nodes as points
# but in the end it needs to be a common data set that has all features
# in a single geometry and predictable to lines (routes)
# original method brought in data from segment and weather (global),
# but nothing from the accident aside from time and place
# other issues:
# accidents are joined to segments by adjacency
# other features can go along with the accident to segment
# but that only makes some sense b/c we model each accident as IID and 
# then pretend that we are modeling segments. 
# If we want to model segments, we need to (should) aggregate accidents to segment
# in that case, associated crash features would be problematic. 
# thinking this through, accidents are just time/place and 
# separate models for different outcomes (or pooled somehow; multiclass)
# any features get associated with segment centroids after spiting to hexID

library(readr)
library(tidyverse)
library(mapview)
library(sf)
library(lubridate)
library(terra)

#### Function ####
mv <- mapview
g <- glimpse
knn <- function(measureFrom,measureTo,k) {
  measureFrom_Matrix <-
    as.matrix(measureFrom)
  measureTo_Matrix <-
    as.matrix(measureTo)
  nn <-   
    FNN::get.knnx(measureTo, measureFrom, k)$nn.dist
  output <-
    as.data.frame(nn) %>%
    rownames_to_column(var = "thisPoint") %>%
    gather(points, point_distance, V1:ncol(.)) %>%
    arrange(as.numeric(thisPoint)) %>%
    group_by(thisPoint) %>%
    summarize(pointDistance = mean(point_distance)) %>%
    arrange(as.numeric(thisPoint)) %>% 
    dplyr::select(-thisPoint) %>%
    pull()
  
  return(output)  
}
st_c <- function(.x,crs=NULL){
  if(is.null(crs)){
    return(st_coordinates(.x))
  } else {
    return(st_coordinates(st_transform(.x,crs = crs)))
  }
}
st_coid <- st_centroid
'%!in%' <- function(x,y)!('%in%'(x,y))

#### Settings and Data Loading ####

# Settings
UTM12N83 = "EPSG:32612"
## sames coords as config.ini
xmin_LONG = -112.04
xmax_LONG = -111.7818
ymax_LAT  = 40.78366
ymin_LAT  = 40.488

# Crash Data
dat <- read_csv("data/State_of_Utah_Crash_Data_2015-2019.csv")
dat <- dat %>% 
  mutate(CRASH_DATE  = lubridate::mdy_hms(CRASH_DATE)) %>% 
  mutate(CRASH_YEAR  = lubridate::year(CRASH_DATE),
         CRASH_MONTH = month(CRASH_DATE),
         CRASH_HOUR  = hour(CRASH_DATE),
         CRASH_DAY   = day(CRASH_DATE),
         CRASH_WDAY  = wday(CRASH_DATE, label = TRUE)) %>% 
  mutate(Morning_rush = ifelse(CRASH_HOUR %in% c(6,7,8,9),"yes","no"),
         Evening_rush = ifelse(CRASH_HOUR %in% c(15,16,17,18),"yes","no")) %>% 
  mutate(CRASH_DATE_rounded = round_date(CRASH_DATE, "hour"))

dat_sf <- dat %>% 
  filter(!is.na(GCS_Long)) %>% 
  filter(GCS_Long != 0 & GCS_Lat > 1) %>% 
  rename("LONGITUDE" = GCS_Long, "LATITUDE" = GCS_Lat) %>% 
  st_as_sf(coords = c("LONGITUDE","LATITUDE"),crs=4326,
           remove = FALSE)

# OSM - from running python notebook #1
osm_file <- "~/Documents/Python_local/traffic-accident-risk-prediction-Utah/data/SLC.gpkg"
st_layers(osm_file)
osm_edges = st_read(osm_file, query = "SELECT *  FROM \"edges\" ")
osm_nodes = st_read(osm_file, query = "SELECT *  FROM \"nodes\" ")

# load weather from notebook #2
weather_file <- "~/Documents/Python_local/traffic-accident-risk-prediction-Utah/data/SLC_weather.csv"
weather <- read.csv(weather_file) %>% 
  mutate(date_time = ymd_hms(date_time)) %>% 
  mutate(Weather_YEAR  = lubridate::year(date_time),
         Weather_MONTH = month(date_time),
         Weather_HOUR  = hour(date_time),
         Weather_DAY   = day(date_time),
         Weather_WDAY  = wday(date_time, label = TRUE)) %>% 
  mutate(Morning_rush = ifelse(Weather_HOUR %in% c(6,7,8,9),"yes","no"),
         Evening_rush = ifelse(Weather_HOUR %in% c(15,16,17,18),"yes","no"),
         Morning_1csnow = ifelse(totalSnow_cm > 1 & Morning_rush == "yes","yes","no"),
         Morning_3csnow = ifelse(totalSnow_cm > 3 & Morning_rush == "yes","yes","no"),
         Morning_0.5mmrain = ifelse(precipMM  > 0.5 & Morning_rush == "yes","yes","no"),
         Morning_1mmrain = ifelse(precipMM    > 1 & Morning_rush == "yes","yes","no"))
# needs time lags

## testing
weax <- weather %>% 
  left_join(dat, by = c("date_time" = "CRASH_DATE_rounded")) %>% 
  filter(!is.na(Weather_HOUR))
table(datx$Morning_3csnow)

#### end test

#### Fishnet creation ####

## fishnet (bc Uber H3 is a PITA as of now)
net_bbox <- st_bbox(c(xmin = xmin_LONG, 
                      xmax = xmax_LONG, 
                      ymax = ymax_LAT, 
                      ymin = ymin_LAT),
                    crs = "epsg:4326") %>% 
  st_as_sfc()

fishnet <- st_make_grid(st_transform(net_bbox, UTM12N83),
               cellsize = 1000, 
               square = FALSE) %>%
  st_sf() %>%
  mutate(hexID = paste0("HEX_",rownames(.))) %>% 
  st_transform(crs="epsg:4326")

#### KNN Feature Creation ####

## Knn on all data
## WHY BOTHER to do this here. if segment is the unit of analysis
## do segment centroid to accident density
## 
# dat_sf$accident.3nn <- 
#   knn(st_c(st_coid(dat_sf),UTM12N83),
#       st_c(dat_sf,UTM12N83), k = 3)

range(dat$CRASH_DATE)

#### Subset data by date ####

dat_local <- dat_sf %>% 
  filter(LONGITUDE >= xmin_LONG,
         LONGITUDE <= xmax_LONG,
         LATITUDE  <= ymax_LAT,
         LATITUDE  >= ymin_LAT)

dat_2018_19 <- dat_sf %>% 
  filter()
  filter(LONGITUDE >= xmin_LONG,
         LONGITUDE <= xmax_LONG,
         LATITUDE  <= ymax_LAT,
         LATITUDE  >= ymin_LAT)
# mv(sample_n(dat_2018_19,3000), zcol = "accident.3nn") + 
#   mv(fishnet)

#### join fishnet hexID to crash data ####

# no need to do just time slice of data
# dat_2018_19_net <- st_join(st_transform(dat_2018_19,UTM12N83),
#               st_transform(fishnet,UTM12N83), join=st_within) 

dat_net <- st_join(st_transform(dat_local,UTM12N83),
                   st_transform(fishnet,UTM12N83), join=st_within) 

## This is where I would save out the basic crash x,y,time
## but moving forward to make richer data
# dat_sample <- dat_2018_19_net %>%
#   st_drop_geometry() %>%
#   select(dates = CRASH_DATE, LONGITUDE, LATITUDE)

# write_csv(dat_sample, "data/SLC_V1_allFeatures.csv")
# write_sf(fishnet, "data/fishnet.geojson")

#### OSM Data work ####
## intersect osm edges to break into hex's and assign hexID
## this partially normalizes (at least limits) the segment length disparity
## and gives the length and hexID to attribute local spatial features

start.time <- Sys.time()
edges_intersect <- terra::disagg(terra::intersect(vect(osm_edges),
                                                  vect(fishnet)))
edges_net <- st_as_sf(edges_intersect) %>%
  mutate(unique_segment_id = paste0("SEGMENT_",1:n()))
edges_net_reduced <- edges_net %>%
  dplyr::select(unique_segment_id, osmid, hexID, highway, oneway,
                lanes, maxspeed, bridge, access, junction, tunnel, width)
end.time <- Sys.time()
print(end.time - start.time)

#### What to do with this
# focus on local spatial variation via hex bins
# though, hexbin to segment is really generalizing the pattern
# modeling each segment centroid to spatial features would be better

# hex aggregate to segment - use hexID to join to segments

fishnet_nodes <- fishnet %>% 
  st_join(., osm_nodes) %>% 
  st_drop_geometry() %>% 
  group_by(hexID) %>% 
  summarise(node_cnt = n())

fishnet_traffic_signals <- fishnet %>% 
  st_join(., filter(osm_nodes, highway == "traffic_signals")) %>% 
  st_drop_geometry() %>% 
  group_by(hexID) %>% 
  summarise(signal_cnt = n())

# agg avg severity of crash (2015-2020)
# can only be done for Utah data or other that has similar features.
net_accident_severity <- dat_net %>% 
  st_drop_geometry() %>% 
  group_by(hexID) %>% 
  summarise(Med_Crash_Severity = median(CRASH_SEVE)) %>% 
  right_join(.,st_drop_geometry(fishnet), by = "hexID") %>%
  mutate(Med_Crash_Severity = ifelse(is.na(Med_Crash_Severity),
                                     0, Med_Crash_Severity))

net_accident_count <- dat_net %>% 
  st_drop_geometry() %>% 
  group_by(hexID) %>% 
  summarise(accident_count = n()) %>% 
  right_join(.,st_drop_geometry(fishnet), by = "hexID") %>%
  mutate(accident_count = ifelse(is.na(accident_count),
                                 0,accident_count))

### Segment joins - use unique_segment_id to join edge_cnt to edges_net_reduced
## what about the line segment centroid features? try that
# xx <- sample_n(edges_net_reduced, 100) 
# 
edge_cnt <- edges_net_reduced %>% 
  st_centroid() %>% 
  select(unique_segment_id, osmid, hexID)
## knn to nodes
edge_cnt$nodes.3nn <- 
  knn(st_c(edge_cnt,UTM12N83),
      st_c(osm_nodes,UTM12N83), k = 3)
edge_cnt$nodes.1nn <- 
  knn(st_c(edge_cnt,UTM12N83),
      st_c(osm_nodes,UTM12N83), k = 1)
edge_cnt$nodes.10nn <- 
  knn(st_c(edge_cnt,UTM12N83),
      st_c(osm_nodes,UTM12N83), k = 10)
## knn to traffic signals
edge_cnt$signals.3nn <- 
  knn(st_c(filter(edge_cnt),UTM12N83),
      st_c(filter(osm_nodes, highway == "traffic_signals"),UTM12N83),
      k = 3)
edge_cnt$signals.1nn <- 
  knn(st_c(edge_cnt,UTM12N83),
      st_c(filter(osm_nodes, highway == "traffic_signals"),UTM12N83),
      k = 1)
edge_cnt$signals.10nn <- 
  knn(st_c(edge_cnt,UTM12N83),
      st_c(filter(osm_nodes, highway == "traffic_signals"),UTM12N83),
      k = 10)
# knn to traffic circle
edge_cnt$circle.1nn <- 
  knn(st_c(edge_cnt,UTM12N83),
      st_c(filter(osm_nodes, highway %in% c("turning_circle",
                                            "turning_loop",
                                            "mini_roundabout")),UTM12N83),
      k = 1)
# knn to stop sign (only n=145, sounds incomplete)
edge_cnt$signals.1nn <- 
  knn(st_c(edge_cnt,UTM12N83),
      st_c(filter(osm_nodes, highway == "stop"),UTM12N83),
      k = 1)

## Knn to accidents from segment centroids ##
edge_cnt$accident.1nn <- 
  knn(st_c(edge_cnt,UTM12N83),
      st_c(dat_sf,UTM12N83),
      k = 1)
edge_cnt$accident.3nn <- 
  knn(st_c(edge_cnt,UTM12N83),
      st_c(dat_sf,UTM12N83),
      k = 3)
edge_cnt$accident.10nn <- 
  knn(st_c(edge_cnt,UTM12N83),
      st_c(dat_sf,UTM12N83),
      k = 10)



### weather lag?


### The great joining
edges_net_features <- edges_net_reduced %>% 
  left_join(fishnet_nodes, by = "hexID") %>% 
  left_join(fishnet_traffic_signals, by = "hexID") %>% 
  left_join(net_accident_severity, by = "hexID") %>% 
  left_join(net_accident_count, by = "hexID") %>% 
  left_join(st_drop_geometry(edge_cnt), by = "unique_segment_id") %>% 
  select(-osmid.x, -osmid.y, -hexID.x) %>% 
  rename("hexID" = hexID.y)

write_sf(edges_net_features, "./data/SLC_Featurized.gpkg")


#### TO DO ####
# hex spatial features, what is near what?
# hex features
# edge centroids features from nodes
# time lags
# distance from downtown or other built ebv features
# join hex and nodes features to line segments
# do some QA on edge fields (such as lanes)
# export line segments for work in notebook #3

signals <- edges_net_features %>% 
  select(accident_count, starts_with("signals")) %>% 
  rename("nearest 1 signal" = signals.1nn,
         "nearest 3 signals" = signals.3nn,
         "nearest 10 signals" = signals.10nn) %>% 
  st_drop_geometry() %>% 
  mutate(Average = mean(`nearest 1 signal`)) %>% 
  pivot_longer(-accident_count, names_to = "Feature", values_to = "distance") %>% 
  mutate(Feature = factor(Feature, levels = c("Average","nearest 1 signal",
                                              "nearest 3 signals",
                                              "nearest 10 signals")))

# signals <- edges_net_features %>% 
#   select(accident_count, starts_with("nodes")) %>% 
#   rename("nearest 1 signal"   = nodes.1nn,
#          "nearest 3 signals"  = nodes.3nn,
#          "nearest 10 signals" = nodes.10nn) %>% 
#   st_drop_geometry() %>% 
#   mutate(Average = mean(`nearest 1 signal`)) %>% 
#   pivot_longer(-accident_count, names_to = "Feature", values_to = "distance") %>% 
#   mutate(Feature = factor(Feature, levels = c("Average","nearest 1 signal",
#                                               "nearest 3 signals",
#                                               "nearest 10 signals")))

library(DataRobotColors)
# library(silgelib)
library(hrbrthemes)
ggplot(signals, aes(x = accident_count,
                               y = distance)) +
  geom_point(alpha = 0.01, color = "#2d8fe2") +
  geom_smooth(color = "#ff5600", method = "lm",
              formula=y ~ poly(x, 2, raw=TRUE)) +
  # geom_smooth(color = "#ff5600", method = "gam" ) +
  facet_wrap(~Feature, nrow = 1) +
  # scale_color_manual(values = c( "#53718f" ,"#abbdc9","#c3d0d9")) +
  # scale_color_DataRobot(palette = "DR_Blues") +
  scale_x_log10() +
  labs(title = "Spatial Relationship of Accidents to Traffic Signals",
       # subtitle = "Average distance to 1, 3, and 10 traffic signals",
       y = "Average distance (meters)",
       x = "Count of Accidents (log10)",
       caption = "Based on 99,331 vehicle accident in Salt Lake City (2015 to 2019)") +
  theme_ft_rc()
  # theme_roboto() +
  # theme(
  #   plot.title = element_text(face="bold", size = 12),
  #   plot.caption = element_text(size = 6),
  #   axis.title.y = element_text(size = 10),
  #   axis.title.x = element_text(size = 10)
  # )

ggsave("knn_feature.png", dpi = "retina",
       width = 8, height = 3, bg = "white")


