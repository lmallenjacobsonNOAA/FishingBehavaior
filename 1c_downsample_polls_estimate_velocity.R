# Loading packages
library(data.table)
library(sf)
library(dplyr)
library(ggplot2)
library(lubridate) # to create hour column

# library(amt)
# #timetk didn't end up working
# library(timetk) # to downsample time series with "summarise_by_time"
# library(trip) # to estimate distance/speeed/max speed by group

library(ggmap) # to pull base maps
library(ggspatial) # to add scale and north arrow to map
library(patchwork) # to plot two maps side by side

# filter dataset, recalculate speed, replot
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
sf_polls_wgs84 <- readRDS(paste0(dir_output,"/sf_polls_wgs84.rds"))
dt_polls <- readRDS(paste0(dir_output,"/dt_polls_annotated.rds"))
crs_nad83 <- readRDS(paste0(dir_data, "/crs_vtr_buffer.rds")) # Pull in CRS from VTR 

#select subset of trips for testing
unique_trips <- unique(dt_polls$TRIP_ID)
these_trips <- unique_trips[1:20]
##############################
# downsample, take first point every hour
dt_polls_hourly <-
  dt_polls %>%
  #filter(TRIP_ID %in% these_trips) %>%
  mutate(hour = hour(DATETIME_GMT)) %>%
  mutate(day = day(DATETIME_GMT)) %>%
  group_by(TRIP_ID, hour, day) %>%
  arrange(DATETIME_GMT) %>%
  filter(row_number()==1) %>%
  ungroup()
  #tally()

##############################
# Coerce into SF (simple features)
sf_polls_hourly_wgs84 <- st_as_sf(x = dt_polls_hourly,                         
                           coords = c("LONGITUDE", "LATITUDE"),
                           crs = 4326)
saveRDS(sf_polls_hourly_wgs84, paste0(dir_output,"/sf_polls_hourly_wgs84.rds"))

sf_polls_hourly_nad83 <- st_transform(sf_polls_hourly_wgs84, crs_nad83)
saveRDS(sf_polls_hourly_nad83, paste0(dir_output,"/sf_polls_hourly_nad83.rds"))

##############################
# side by side plots with original and downsampled data
this_trip <- unique_trips[[1]]
these_trips <- unique_trips[1:50]

for(this_trip in these_trips){
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
      labs(y = "Latitude", x = "Longitude",
           title='Complete: 1/20s',
           subtitle = paste0("Trip:", this_trip))+
      guides(color="none")+
      annotation_scale(location = "br", width_hint = 0.5) +
      annotation_north_arrow(location = "br", which_north = "true",
                             height = unit(.3, "in"), width = unit(.3, "in"),
                             pad_x = unit(0.2, "in"), pad_y = unit(0.3, "in"),
                             style = north_arrow_fancy_orienteering))
  
  sf_hourly <- sf_polls_hourly_wgs84 %>% filter(TRIP_ID == this_trip) %>% select(fishing)
  
  (this_plot_hourly <- ggmap(this_map) +
      geom_sf(data = sf_hourly,inherit.aes = FALSE, aes(color= fishing)) +
      coord_sf(crs = st_crs(4326))+
      scale_color_viridis_d(option = "rocket",
                            alpha = .5, begin = 0.25, end = .75)+
      labs(y = "Latitude", x = "Longitude",
           title='Downsampled: 1/hr',
           subtitle = paste0("Trip:", this_trip))+
      annotation_scale(location = "br", width_hint = 0.5) +
      annotation_north_arrow(location = "br", which_north = "true",
                             height = unit(.3, "in"), width = unit(.3, "in"),
                             pad_x = unit(0.2, "in"), pad_y = unit(0.3, "in"),
                             style = north_arrow_fancy_orienteering))
  
  (this_patchwork <- this_plot+this_plot_hourly)
  
  width = 11
  height = 8.5
  
  ggsave(filename = paste0(dir_output, "/test/downsampled/",this_trip,".png"),
         plot = this_patchwork,
         width = width, height = height)    
  ## re-run and export at a larger size - 11" for width?
  # check old code - re run with ggsave?
  # png(paste0(dir_output, "/test/downsampled/",this_trip,".png"))
  # print(this_patchwork)
  # dev.off()
}


##############################
#Compute distances and velocities between successive points
sf_polls_hourly_wgs84 <- readRDS(paste0(dir_output,"/sf_polls_hourly_wgs84.rds"))

empty <- st_as_sfc("POINT(EMPTY)", crs = 4326)

sf_polls_wgs84 <-
  sf_polls_wgs84 %>%
  group_by(TRIP_ID) %>%
  arrange(DATETIME_GMT) %>%
  mutate(
    elapsed_time = lead(DATETIME_GMT) - DATETIME_GMT,
    distance_to_next = sf::st_distance(
      geometry, 
      lead(geometry, default = empty), 
      by_element = TRUE)
    )

sf_polls_wgs84 <-
  mutate(
    sf_polls_wgs84,
    velocity = units::set_units(
      units::drop_units(distance_to_next)/
        as.numeric(elapsed_time),
      m/s))

saveRDS(sf_polls_wgs84, paste0(dir_output,"/sf_polls_wgs84_speed.rds"))

sf_polls_hourly_wgs84<-
  sf_polls_hourly_wgs84 %>%
  group_by(TRIP_ID) %>%
  arrange(DATETIME_GMT) %>%
  mutate(
    elapsed_time = lead(DATETIME_GMT) - DATETIME_GMT,
    distance_to_next = sf::st_distance(
      geometry, 
      lead(geometry, default = empty), 
      by_element = TRUE)
  )

sf_polls_hourly_wgs84 <-
  mutate(
    sf_polls_hourly_wgs84,
    velocity = units::set_units(
      units::drop_units(distance_to_next)/
        as.numeric(elapsed_time),
      m/s))

saveRDS(sf_polls_hourly_wgs84, paste0(dir_output,"/sf_polls_hourly_wgs84_speed.rds"))

##############################
# create summary tables 
# calculate min, max, and mean velocity by trip, haul, fishing/non-fishing
# maybe ignore haul for now - because there will be fewer hauls in the hourly dataset
#rm(list = ls())
sf_polls_hourly_wgs84 <- readRDS(paste0(dir_output,"/sf_polls_hourly_wgs84_speed.rds"))
sf_polls_wgs84 <- readRDS(paste0(dir_output,"/sf_polls_wgs84_speed.rds"))

dt_polls_hourly <-st_drop_geometry(sf_polls_hourly_wgs84)
dt_polls_hourly <- do.call(data.frame, # Replace Inf in data by NA
                   lapply(dt_polls_hourly,
                          function(x) replace(x, is.infinite(x), NA)))
setDT(dt_polls_hourly)

dt_polls <-st_drop_geometry(sf_polls_wgs84)
setDT(dt_polls)

dt_polls[, velocity_knots := units::drop_units(measurements::conv_unit(velocity, "m_per_sec", "knot"))]
dt_polls_hourly[, velocity_knots := units::drop_units(measurements::conv_unit(velocity, "m_per_sec", "knot"))]


velocity_summary_20s <-
  dt_polls[,.(mean=mean(velocity_knots, na.rm = TRUE),
              st_dev=sd(velocity_knots, na.rm = TRUE),
              max=max(velocity_knots, na.rm = TRUE),
              min=min(velocity_knots, na.rm = TRUE),
              count=.N,
              type="20s"),
           by = .(TRIP_ID, fishing)]


velocity_summary_hourly <-
  dt_polls_hourly[,.(mean=mean(velocity_knots, na.rm=TRUE),
                     st_dev=sd(velocity_knots, na.rm=TRUE),
                     max=max(velocity_knots, na.rm=TRUE),
                     min=min(velocity_knots, na.rm=TRUE),
                     count=.N,
                     type="1hr"),
                  by = .(TRIP_ID, fishing)]

##############################
# create long summary table and plot averages

# cannot calculate stdev with fewer than 2 points - could remove these
# cannot calculate mean, min, max with 1 point - remove these

errors <- velocity_summary_hourly[is.infinite(max)]
error_trip_example <- errors[1:1, .(TRIP_ID, fishing)]
error_polls_example <- dt_polls_hourly[TRIP_ID == error_trip_example$TRIP_ID &
                                         fishing == error_trip_example$fishing]
error_trips <- unique(errors$TRIP_ID)
#rbind two tables
velocity_summary <- rbind(velocity_summary_hourly, velocity_summary_20s)
# remove trips with errors - fewer than 3 (or 2) points per section?
# removes 100 rows
velocity_summary <- velocity_summary[count > 2]

# units create problems when plotting - remove for now
velocity_summary_plot <- units::drop_units(velocity_summary)
#velocity_summary_plot <- velocity_summary_plot[max < 20]

(max <-
  ggplot(velocity_summary_plot, aes(x=type, y=max, fill=fishing)) +
  geom_boxplot() +
  theme_minimal()+
  xlab("Ping Frequency") +
  ylab("Max Velocity (knots)") +
  scale_fill_hue(name="Fishing")+
  ggtitle("Maximum velocity per trip, for fishing and non-fishing steps") +
  ylim(0, 30)+
  theme_bw())

(mean <-
    ggplot(velocity_summary_plot, aes(x=type, y=mean, fill=fishing)) +
    geom_boxplot() +
    theme_minimal()+
    xlab("Ping Frequency") +
    ylab("Max Velocity (knots)") +
    scale_fill_hue(name="Fishing")+
    ggtitle("Average velocity per trip, for fishing and non-fishing steps") +
    ylim(0,7)+
    theme_bw())

(min <-
    ggplot(velocity_summary_plot, aes(x=type, y=min, fill=fishing)) +
    geom_boxplot() +
    theme_minimal()+
    xlab("Ping Frequency") +
    ylab("Max Velocity (knots)") +
    scale_fill_hue(name="Fishing")+
    ggtitle("Minimum velocity per trip, for fishing and non-fishing steps") +
    ylim(0,7)+
    theme_bw())

(count <-
    ggplot(velocity_summary_plot, aes(x=type, y=count, fill=fishing)) +
    geom_boxplot() +
    theme_minimal()+
    xlab("Ping Frequency") +
    ylab("Count") +
    scale_fill_hue(name="Fishing")+
    ggtitle("Number of locations per trip, for fishing and non-fishing steps") +
    #ylim(0,7)+
    scale_y_log10(labels = scales::comma)+
    theme_bw())

(velocity_patchwork <- max/mean/min/count)

height = 11
width = 8.5

ggsave(filename = paste0(dir_output, "/test/velocity.png"),
       plot = velocity_patchwork,
       width = width, height = height)

geom_violin()+
  
##############################
# Join summary tables for pairwise analysis
setkeyv(velocity_summary_20s, c("TRIP_ID", "fishing"))
setkeyv(velocity_summary_hourly, c("TRIP_ID", "fishing"))

velocity_summary_wide <- velocity_summary_hourly[velocity_summary_20s, nomatch = 0]

(max <-
    ggplot(velocity_summary_wide, aes(x=i.max, y=max, color=fishing, group = fishing)) +
    geom_point()+
    geom_smooth(method=lm)+
    geom_abline(slope = 1 , intercept = 0) +
    theme_minimal()+
    xlab("Velcocity from pings ever 20 s") +
    ylab("Velcocity from pings ever 1 hr") +
    scale_fill_hue(name="Fishing")+
    ggtitle("Maximum velocity per trip, for fishing and non-fishing steps") +
    theme_bw())

velocity_summary_wide_filtered <- velocity_summary_wide[i.max < 25]

(max_filtered <-
    ggplot(velocity_summary_wide_filtered, aes(x=i.max, y=max, color=fishing, group = fishing)) +
    geom_point()+
    geom_smooth(method=lm)+
    geom_abline(slope = 1 , intercept = 0) +
    theme_minimal()+
    xlab("Velcocity from pings ever 20 s") +
    ylab("Velcocity from pings ever 1 hr") +
    scale_fill_hue(name="Fishing")+
    ggtitle("Maximum (filtered, <20 knots) velocity per trip, for fishing and non-fishing steps") +
    theme_bw())

(mean <-
  ggplot(velocity_summary_wide, aes(x=i.mean, y=mean, color=fishing, group = fishing)) +
  geom_point()+
  geom_smooth(method=lm)+
  geom_abline(slope = 1 , intercept = 0) +
  theme_minimal()+
  xlab("Velcocity from pings ever 20 s") +
  ylab("Velcocity from pings ever 1 hr") +
  scale_fill_hue(name="Fishing")+
  ggtitle("Average velocity per trip, for fishing and non-fishing steps") +
  theme_bw())

(min <-
    ggplot(velocity_summary_wide, aes(x=i.min, y=min, color=fishing, group = fishing)) +
    geom_point()+
    geom_smooth(method=lm)+
    geom_abline(slope = 1 , intercept = 0) +
    theme_minimal()+
    xlab("Velcocity from pings ever 20 s") +
    ylab("Velcocity from pings ever 1 hr") +
    scale_fill_hue(name="Fishing")+
    ggtitle("Minimum velocity per trip, for fishing and non-fishing steps") +
    theme_bw())


(velocity_patchwork <- (max|max_filtered)/(mean|min))

height = 11
width = 11

ggsave(filename = paste0(dir_output, "/test/velocity_pairwise.png"),
       plot = velocity_patchwork,
       width = width, height = height) 

##############################
# compare reported speed (knots, also calculated, but from mike)
# with the calculated speeds  

dt_polls[, velocity_knots := units::drop_units(measurements::conv_unit(velocity, "m_per_sec", "knot"))]

dt_polls[, speed_diff := SPEED_KNOTS - velocity_knots]

dt_polls[, is_diff := ifelse(speed_diff < 0.5 & speed_diff > -0.5, "N", "Y")]


(diff_plot <- ggplot(data = dt_polls[!(is.na(is_diff))], aes(x = is_diff, by = fishing))+
  geom_bar()+
  scale_y_log10(labels = scales::comma)+
  xlab("Is difference within 0.5 knots?") +
  ylab("Count") +
  ggtitle("Many differences in speed") +
  # theme_minimal()+
  theme_bw())
  

dt_polls[abs(speed_diff) < 5, .N]

min(dt_polls$speed_diff, na.rm = TRUE)

dt_polls_diff <- dt_polls[is_diff == "Y" & speed_diff < 5 & speed_diff > -5]

hist(dt_polls_diff$speed_diff)

(hist <- ggplot(dt_polls_diff, aes(x=abs(speed_diff))) +
  geom_histogram() +
  xlab("Absolute value of speed difference (knots)") +
  ylab("Count") +
  ggtitle("Speed difference, when diff is > 0.5 knots") +
  # theme_minimal()+
  theme_bw())

(diff_patchwork <- diff_plot+hist)

width = 11
height= 5.5

ggsave(filename = paste0(dir_output, "/test/speed_diff.png"),
       plot = diff_patchwork,
       width = width, height = height) 

sf_polls_wgs84 <- sf_polls_wgs84 %>%
  mutate(velocity_knots = units::drop_units(measurements::conv_unit(velocity, "m_per_sec", "knot")))

# velocities do not match - in many cases, I calculate a 0 velocity
# and in these cases the points are the same
# but mike calculates a non-zero velocity - need to find out why
# rounding problem in coordinates?