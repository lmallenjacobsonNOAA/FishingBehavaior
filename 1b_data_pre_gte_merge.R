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


#add new variable, "chunk_id" to group all section of trip -
#i.e., similar to haul_id, but including non-fishing
dt_polls<- readRDS(paste0(dir_output,"/dt_polls_annotated.rds"))

dt_polls[order(DATETIME_GMT), chunk := rleidv(EFFORT_NUM), by = TRIP_ID]
saveRDS(dt_polls, paste0(dir_output,"/dt_polls_annotated.rds"))

##############################
# Coerce into SF (simple features)
sf_polls_wgs84 <- st_as_sf(x = dt_polls,                         
               coords = c("LONGITUDE", "LATITUDE"),
               crs = 4326)
saveRDS(sf_polls_wgs84, paste0(dir_output,"/sf_polls_wgs84.rds"))

sf_polls_nad83 <- st_transform(sf_polls_wgs84, crs_nad83)

saveRDS(sf_polls_nad83, paste0(dir_output,"/sf_polls_nad83.rds"))



