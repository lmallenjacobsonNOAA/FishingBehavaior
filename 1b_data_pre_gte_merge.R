# Loading packages
library(data.table)
library(sf)
library(dplyr)
library(marmap)
library(ggplot2)
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
dt <- readRDS(paste0(dir_output,"/dt_polls_annotated.rds"))

trips <- unique(sf_polls_wgs84$TRIP_ID)
trips_100 <- trips[1:100]
trip <- trips[2:2]
blues <- c("lightsteelblue4", "lightsteelblue3",
           "lightsteelblue2", "lightsteelblue1")
greys <- c(grey(0.6), grey(0.93), grey(0.99))


for (trip in trips_100) {
  sf <- sf_polls_wgs84 %>% filter(TRIP_ID == trip) %>% select(fishing)
  plot(sf)
}

for (trip in trips_100) {
  sf <- sf_polls_wgs84 %>% filter(TRIP_ID == trip) %>% select(fishing)
  bbox <- st_bbox(sf)
  bathy <- getNOAA.bathy(lon1 = bbox["xmin"]-.1, lon2 = bbox["xmax"]+.1,
                         lat1 = bbox["ymin"]-.11, lat2 = bbox["ymax"]+.1,
                         resolution = 1)
  sp <- get.depth(bathy, sf, locator = FALSE)
  plot(bathy, image = TRUE, land = TRUE, axes = TRUE, lwd = NULL,
       #drawlabels = TRUE,
       n = 1,
       bpal = list(c(0, max(bathy), grey(.7), grey(.9), grey(.95)),
                   c(min(bathy), 0, "darkblue", "lightblue")),
       asp = .75)
  
  basemap <- autoplot.bathy(bathy, geom=c("tile")) +
    scale_fill_gradient2(low="dodgerblue4", mid="gainsboro", high="darkgreen") +
    # geom_point(data = ctd, aes(x = Longitude, y = Latitude),
    #            colour = 'black', size = 3, alpha = 1, shape = 15) +
    labs(y = "Latitude", x = "Longitude", fill = "Elevation")
    #+coord_cartesian(expand = 0)
  
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
}

trips_1 <- trips[2:2]

trip <- trips_1
for (trip in trips_1) {
  sf <- sf_polls_nad83 %>% filter(TRIP_ID == trip) #%>% select(fishing)
  
  (plot <- ggplot() +
    geom_raster(data = spat_raster)+
    geom_sf(aes(data = sf, color = fishing, size = SPEED_KNOTS))+
    scale_size(trans = 'reverse', range = c(.5, 2))+
    #scale_color_viridis_d(option = "viridis")+
    xlab("Longitude")+
    ylab("Latitude")+
    annotation_scale(location = "br", width_hint = 0.5) +
    annotation_north_arrow(location = "br", which_north = "true",
                           height = unit(.3, "in"), width = unit(.3, "in"), 
                           pad_x = unit(0.2, "in"), pad_y = unit(0.3, "in"),
                           style = north_arrow_fancy_orienteering))
}




