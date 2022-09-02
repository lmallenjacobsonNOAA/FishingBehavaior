# Loading packages
library(data.table)
library(sf)
library(dplyr)
library(ggplot2)

library(ggmap)

library(marmap)
library(maptools)
library(mapdata)

library(ggspatial)
# library(ggsn)
# library(ggOceanMapsData)
# library(ggOceanMaps)

# Annotate pre-gte as fishing/not fishing using GTE data set
# Convert dt to sp
# plot test data

##############################
# Functions

##############################
# Set directories
pwd <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

repository <- "predicting_fishing_behavaior"
path_base <- "C:/Users/lianne.allen-jacobso/Documents/"
check_pwd <- paste0(path_base, "Repositories/",repository)
pwd == check_pwd

dir_output <- paste0(path_base, "Output/", repository)
dir_data <- paste0(path_base, "Data/", repository)

##############################
# Pull in data
#dt_gte <- setDT(readRDS(paste0(dir_output,"/lf_squid_trips_gte_2000_2022.rds")))
dt_polls<- setDT(readRDS(paste0(dir_output,"/lf_squid_trips_poll_data_2000_2022.rds")))
crs_nad83 <- readRDS(paste0(dir_data, "/crs_vtr_buffer.rds")) # Pull in CRS from VTR 
dt_gte_effort <- setDT(readRDS(paste0(dir_output,"/dt_gte_effort.rds")))
##############################
# Prep Data
#dt_gte[, id := paste0(TRIP_ID, "_", LATITUDE, "_", LONGITUDE)]
#dt_polls[, id := paste0(TRIP_ID, "_", LATITUDE, "_", LONGITUDE)]

setkey(x = dt_polls, TRIP_ID)
setkey(x = dt_gte_effort, TRIP_ID)

dt_merge <- dt_gte_effort[
  dt_polls,
  .(TRIP_ID, EFFORT_NUM, DATETIME_GMT, LATITUDE, LONGITUDE,
    SPEED_KNOTS, BOT_DEPTH_M),
  on=.(TRIP_ID = TRIP_ID,
       EFFORT_START_DATE_GMT <= DATETIME_GMT,
       EFFORT_END_DATE_GMT >= DATETIME_GMT)]

#Some points overlap between hauls! so these are included multiple times
# length(dt_merge$TRIP_ID)-length(dt_polls$TRIP_ID)
# 2418
# (length(dt_merge$TRIP_ID)-length(dt_polls$TRIP_ID))/length(dt_polls$TRIP_ID)
# <1% of GPS points 0.017%
#overlap_range_check <- dt_gte_effort[dt_gte_effort, .N, on = .(EFFORT_START_DATE_GMT < EFFORT_END_DATE_GMT, EFFORT_END_DATE_GMT > EFFORT_END_DATE_GMT)]

dt_merge[, fishing := ifelse(is.na(EFFORT_NUM), 'no', 'yes')]
dt_merge[order(DATETIME_GMT), seq_id := seq_len(.N), by = TRIP_ID]
#get.depth(dt_merge, distance = TRUE)

saveRDS(dt_merge, paste0(dir_output,"/dt_polls_annotated.rds"))

dt_polls <- dt_merge
remove(dt_merge)
##############################
# Coerce into SF (simple features)
sf_polls_wgs84 <- st_as_sf(x = dt_polls,                         
               coords = c("LONGITUDE", "LATITUDE"),
               crs = 4326)
saveRDS(sf_polls_wgs84, paste0(dir_output,"/sf_polls_wgs84.rds"))

sf_polls_nad83 <- st_transform(sf_polls_wgs84, crs_nad83)

saveRDS(sf_polls_nad83, paste0(dir_output,"/sf_polls_nad83.rds"))

##############################
# Plot 10 trips
#sf_polls_nad83 <- readRDS(paste0(dir_output,"/sf_polls_nad83.rds"))
sf_polls_wgs84 <- readRDS(paste0(dir_output,"/sf_polls_wgs84.rds"))
dt_polls <- readRDS(paste0(dir_output,"/dt_polls_annotated.rds"))

trips <- unique(dt_polls$TRIP_ID)
trips_100 <- trips[1:20]
this_trip <- trips[1:1]
# blues <- c("lightsteelblue4", "lightsteelblue3",
#            "lightsteelblue2", "lightsteelblue1")
# greys <- c(grey(0.6), grey(0.93), grey(0.99))


#plot using fortify (from Andy's Script)
for(this_trip in trips_100){
  sf <- sf_polls_wgs84 %>% filter(TRIP_ID == this_trip) %>% select(fishing)
  dt_this_trip <- dt_polls %>% filter(TRIP_ID == this_trip)
  
  bbox <- st_bbox(sf)

  bathy <- getNOAA.bathy(lon1 = bbox["xmin"]-.1, lon2 = bbox["xmax"]+.1,
                         lat1 = bbox["ymin"]-.11, lat2 = bbox["ymax"]+.1,
                         resolution = 1)
  
  fortified_bathy = fortify.bathy(bathy)
  
  # get regional polygons
  reg = map_data("world2Hires")
  reg = subset(reg, region %in% c('Canada', 'USA'))
  
  # convert lat longs 
  reg$long = (360 - reg$long)*-1
  
  # set map limits
  lons = c(bbox["xmin"]-.1, bbox["xmax"]+.1)
  lats = c(bbox["ymin"]-.11, bbox["ymax"]+.1)
  
  p <- ggplot() + 
    
    # add 100m contour
    geom_contour(data = fortified_bathy, 
                 aes(x=x, y=y, z=z),
                 breaks=seq(-0,-100,-5),
                 size=c(0.3),
                 colour="grey")+
    
    # add coastline
    geom_polygon(data = reg, aes(x = long, y = lat, group = group), 
                 fill = "darkgrey", color = NA) + 
    
    #Coordinates
    coord_map(xlim = lons, ylim = lats)+
    
    # formatting
    ylab("")+xlab("")+
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_size(trans = 'reverse', range = c(.5, 2))+
    scale_color_viridis_d(option = "rocket",
                          alpha = .5, begin = 0.25, end = .75)+
    
    #adding points for the EXAMPLE DATA records
    geom_point(data=dt_this_trip,aes(x=LONGITUDE,y=LATITUDE,
                                     colour=fishing, size =SPEED_KNOTS),
               ) +
    #add scale bar and north arrow
    # annotation_scale(location = "br", width_hint = 0.5) +
    annotation_north_arrow(location = "br", which_north = "true",
                           height = unit(.3, "in"), width = unit(.3, "in"),
                           pad_x = unit(0.2, "in"), pad_y = unit(0.3, "in"),
                           style = north_arrow_fancy_orienteering)+
    #Adding labels
    labs(x='Longitude',y='Latitude',subtitle='Add subtitle here',
         title='Locations from a single fishing trip',fill='No. records',
         caption='* Add caption')
  
  
  print(p) 
}


#plot tracks labelled with fishing
for (this_trip in trips_100) {
  sf <- sf_polls_wgs84 %>% filter(TRIP_ID == this_trip) %>% select(fishing)
  plot(sf)
}
#plot tracks labelled with speed]

for (this_trip in trips_100) {
  sf <- sf_polls_wgs84 %>% filter(TRIP_ID == this_trip) %>% select(SPEED_KNOTS)
  plot(sf)
}

# plot tracks with context: depth, land, scale, and north arrow
# attempt with ggmaps
for(this_trip in trips){
  sf <- sf_polls_wgs84 %>% filter(TRIP_ID == this_trip) %>% select(fishing)
  bbox <- st_bbox(sf)
  #bounding box lowerleftlon, lowerleftlat, upperrightlon, upperrightlat
  this_location <- c(bbox["xmin"]-.1,
                     bbox["ymin"]-.1,
                     bbox["xmax"]+.1,
                     bbox["ymax"]+.1)
  
  names(this_location)<- c('left', 'bottom', 'right', 'top')
  
  this_map <- get_map(location = this_location,
                    source = "stamen",
                    maptype = "watercolor",
                    crop = FALSE)
  
  (this_plot <- ggmap(this_map) +
      geom_sf(data = sf,inherit.aes = FALSE, aes(color= fishing)) +
      coord_sf(crs = st_crs(4326))+
      scale_color_viridis_d(option = "rocket",
                            alpha = .5, begin = 0.25, end = .75)+
      labs(y = "Latitude", x = "Longitude")+
      annotation_scale(location = "br", width_hint = 0.5) +
      annotation_north_arrow(location = "br", which_north = "true",
                             height = unit(.3, "in"), width = unit(.3, "in"),
                             pad_x = unit(0.2, "in"), pad_y = unit(0.3, "in"),
                             style = north_arrow_fancy_orienteering))
  
  png(paste0(dir_output, "/test/fishing/",this_trip,".png"))
  print(this_plot)
  dev.off()
}

for(this_trip in trips){
  sf <- sf_polls_wgs84 %>% filter(TRIP_ID == this_trip) %>% select(SPEED_KNOTS)
  bbox <- st_bbox(sf)
  #bounding box lowerleftlon, lowerleftlat, upperrightlon, upperrightlat
  this_location <- c(bbox["xmin"]-.1,
                     bbox["ymin"]-.1,
                     bbox["xmax"]+.1,
                     bbox["ymax"]+.1)
  
  names(this_location)<- c('left', 'bottom', 'right', 'top')
  
  this_map <- get_map(location = this_location,
                      source = "stamen",
                      maptype = "watercolor",
                      crop = FALSE)
  
  (this_plot <- ggmap(this_map) +
      geom_sf(data = sf,inherit.aes = FALSE, aes(color= SPEED_KNOTS)) +
      coord_sf(crs = st_crs(4326))+
      scale_color_viridis_c(option = "rocket",
                            alpha = .5, begin = 0.25, end = .75)+
      labs(y = "Latitude", x = "Longitude")+
      annotation_scale(location = "br", width_hint = 0.5) +
      annotation_north_arrow(location = "br", which_north = "true",
                             height = unit(.3, "in"), width = unit(.3, "in"),
                             pad_x = unit(0.2, "in"), pad_y = unit(0.3, "in"),
                             style = north_arrow_fancy_orienteering))
  
  png(paste0(dir_output, "/test/speed/",this_trip,".png"))
  print(this_plot)
  dev.off()
  
  # ggsave(filename = paste0(dir_output, "/test/fishing/",this_trip,".png"),
  #        plot = this_plot, width = width, height = height)
  # 
}
# attempt with marmaps
for(this_trip in trips_100){
  sf <- sf_polls_wgs84 %>% filter(TRIP_ID == this_trip) %>% select(fishing)
  bbox <- st_bbox(sf)
  #bounding box lowerleftlon, lowerleftlat, upperrightlon, upperrightlat
  myLocation <- c(-130, 30, -105, 50)
  
  bathy <- getNOAA.bathy(lon1 = bbox["xmin"]-.1, lon2 = bbox["xmax"]+.1,
                         lat1 = bbox["ymin"]-.11, lat2 = bbox["ymax"]+.1,
                         resolution = 1)
  
  plot(bathy, image = TRUE, land = TRUE, n = 1,
       bpal = list(c(0, max(bathy), greys),
                   c(min(bathy), 0, blues)))
  
  points(dt_this_trip$LONGITUDE, dt_this_trip$LATITUDE, pch = 21, col = "black",
         bg = "yellow", cex = 1.3)
  
  scaleBathy(bathy, deg = 2, x = "bottomleft", inset = 5)
}
for (this_trip in trips_100) {
  sf <- sf_polls_wgs84 %>% filter(TRIP_ID == this_trip) %>% select(fishing)
  bbox <- st_bbox(sf)
  bathy <- getNOAA.bathy(lon1 = bbox["xmin"]-.1, lon2 = bbox["xmax"]+.1,
                         lat1 = bbox["ymin"]-.11, lat2 = bbox["ymax"]+.1,
                         resolution = 1)

  (this_plot <- autoplot.bathy(bathy, geom=c("tile"), coast = FALSE) +
    scale_fill_gradient2(low="dodgerblue4", mid="gainsboro", high="darkgreen") +
    ggsn::scalebar(data = sf, location = "topleft", dist = 4,
                   dist_unit = "km", transform = TRUE,  model = "WGS84")+
    geom_point(data = dt_this_trip,
               aes(x = LONGITUDE, y = LATITUDE, color = SPEED_KNOTS),
               size = 3, alpha = 1, shape = 20) +
    scale_color_viridis_c(option = "rocket", alpha = .5, begin = 0.25, end = .75)+
    labs(y = "Latitude", x = "Longitude", fill = "Depth")+
    # annotation_north_arrow(location = "br", which_north = "true",
    # height = unit(.3, "in"), width = unit(.3, "in"),
    # pad_x = unit(0.2, "in"), pad_y = unit(0.3, "in"),
    # style = north_arrow_fancy_orienteering)+
    coord_cartesian())
  
  width = 11
  height = 8.5
  
  png(filename = ,
      )
  ggsave(filename = paste0(dir_output, "/test/fishing/",this_trip,".png"),
         plot = this_plot,
         width = width, height = height)  
}


##############################
# scrap
trips_1 <- trips[2:2]

#sp <- get.depth(bathy, sf, locator = FALSE)
# plot(bathy, image = TRUE, land = TRUE, axes = TRUE, lwd = NULL,
#      #drawlabels = TRUE,
#      n = 1,
#      bpal = list(c(0, max(bathy), grey(.7), grey(.9), grey(.95)),
#                  c(min(bathy), 0, "darkblue", "lightblue")),
#      asp = .75)
# 

basemap + geom_sf(data = sf, aes(color = fishing))+
  scale_size(trans = 'reverse', range = c(.5, 2))+
  #scale_color_viridis_d(option = "viridis")+
  xlab("Longitude")+
  # ylab("Latitude")+
  # annotation_scale(location = "br", width_hint = 0.5) +
  # annotation_north_arrow(location = "br", which_north = "true",
  #                        height = unit(.3, "in"), width = unit(.3, "in"),
  #                        pad_x = unit(0.2, "in"), pad_y = unit(0.3, "in"),
  #                        style = north_arrow_fancy_orienteering)
  
  plot(sf, add = TRUE)
trip <- trips_1
for (this_trip in trips_1) {
  sf <- sf_polls_nad83 %>% filter(TRIP_ID == this_trip) #%>% select(fishing)
  
  (plot <- ggplot() +
    geom_raster(data = spat_raster)+
    geom_sf(aes(data = sf, color = fishing, size = SPEED_KNOTS))+
    scale_size(trans = 'reverse', range = c(.5, 2))+
    #scale_color_viridis_d(option = "viridis")+
    xlab("Longitude")+
    ylab("Latitude")+
    
}




