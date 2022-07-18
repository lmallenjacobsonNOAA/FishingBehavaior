# Loading packages
library(data.table)
library(sf)
library(dplyr)

# Select data with >.39 loligo catch and by year
# remove trips with alphanumeric ids
# select trips with GTE
# remove trips with NA for longitude or latitude
# match tripID to IMGID
# export matched ids
# coerce to SF and change crs to match vtrb - export sf
# add imgid to sf with GTE data

##############################
# Functions
numbers_only <- function(x) !grepl("\\D", x)

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
dt <- setDT(readRDS(file = paste0(dir_output,"/lf_squid_trips_gte_2000_2022.rds"))) # gte data with trips
# to match hauls to subtrips, this came from Mike M. from the rollup
dt_imgids <- as.data.table(readRDS(paste0(dir_data, "/VERSWH.IMAGES_TO_EFFORTS.rds"))) 
crs_nad83 <- readRDS(paste0(dir_data, "/crs_vtr_buffer.rds")) # Pull in CRS from VTR 
dt_revenue <- as.data.table(readRDS(paste0(dir_output,"/dt_revenue.rds")))

# Geret suggested using this table
#dt_revenue <- setDT(readRDS(file = paste0(dir_data, "/apsd.dmis_all_years_squid_2015_on.rds")))
# Ben suggested this table
#dt_revenue <- setDT(readRDS(file = paste0(dir_data, "/apsd.dmis_sfclam_040620.rds")))
# look at CAtch Acountingn and Monitoring System for info about APSD

##############################
# Prep Data
dt_imgids[,tripid_chr :=as.character(TRIP_ID)][
  ,imgid_chr :=as.character(IMG_ID)]

dt_revenue[, imgid_chr := as.character(IMGID)][
  , char_imgid := nchar(imgid_chr)]

dt[,tripid_chr :=as.character(trip_id)][
  , char_tripid := nchar(tripid_chr)]

# Trim data to only include that meet pre-specified criteria
# First, calculate loglio catch
dt_trimmed <- dt[, prop_loligo := TOT_LOLIGO_CATCH/TOT_CATCH][
  , year := substr(dt$sail_date, 1, 4)][
  year>2014][ # Then, filter by year
  prop_loligo>.3999][ # Filter by prop loglio catch
  , isNum := numbers_only(trip_id)][ #add column, T = numbers only
  isNum == TRUE] #Filter alphanumeric trips

# new version does not trim - maybe Andy already filtered data
dt <- dt_trimmed
remove(dt_trimmed)

##############################
# Prep data to coerce into spatial points data frame:
# Clean, subset, and export data:
dt_gte <- dt[has_GTE=="YES"] # Select trips with GPS data (GTE)
dt_gte <- na.omit(dt_gte , c("LONGITUDE", "LATITUDE")) # rows with NAs in lon and lat

# Match cleaned dataset with IMGID dataset
# remove rows that do not have trip_id in other dataset
dt_imgids_matched <- dt_imgids[tripid_chr %in% dt_gte$tripid_chr]
dt_gte_matched <- dt_gte[tripid_chr %in% dt_imgids_matched$tripid_chr]
dt_revenue_matched <- dt_revenue[imgid_chr %in% dt_imgids_matched$imgid_chr]
# remove any subtrips in VTR buffer set that are not 
dt_imgids_matched <- dt_imgids_matched[imgid_chr %in% dt_revenue_matched$imgid_chr]

length(unique(dt_imgids_matched$tripid_chr)) == length(unique(dt_gte_matched$tripid_chr))

length(unique(dt_imgids_matched$imgid_chr)) == length(unique(dt_revenue_matched$IMGID))

##############################
# Create Grouping variable for hauls - using 
#group by stat area, gear size, mesh size
dt_imgids_matched[, group_id := .GRP, by = .(tripid_chr, EFFORT_NUM)]
dt_gte_matched[, group_id := .GRP, by = .(tripid_chr, EFFORT_NUM)]

dt_imgids_matched[, group_id := paste0(tripid_chr, "_",EFFORT_NUM)]
dt_gte_matched[, group_id := paste0(tripid_chr, "_",EFFORT_NUM)]


##############################
# Join tables by trip_area and group_id
# join cleaned imgids to dt_gte_matched
dt_for_join <- dt_imgids_matched[, .(group_id, imgid_chr)]
setkey(dt_for_join, group_id)
setkey(dt_gte_matched, group_id)
dt_gte_joined <- dt_gte_matched[dt_for_join, nomatch = 0] 

dt_revenue_matched <- dt_revenue_matched[imgid_chr %in% dt_gte_joined$imgid_chr]

length(unique(dt_gte_joined$imgid_chr)) == length(unique(dt_revenue_matched$imgid_chr))

setkey(dt_gte_joined, imgid_chr)
setkey(dt_revenue_matched, imgid_chr)
dt_gte_revenue <- dt_gte_joined[dt_revenue_matched, nomatch = 0] 

###############################
# test join - should be one trip_id for each imgid if the join was done correctly
test <- unique(dt_gte_joined[, .(imgid_chr, tripid_chr)])
test_count <- test[, .N, by = imgid_chr]
test_max <- test_count[, lapply(.SD, max)]
# good only 1
test <- unique(dt_imgids_matched[, .(imgid_chr, tripid_chr)])
test_count <- test[, .N, by = imgid_chr]
test_max <- test_count[, lapply(.SD, max)]
# good only 1
test <- unique(dt_gte_revenue[, .(imgid_chr, tripid_chr)])
test_count <- test[, .N, by = imgid_chr]
test_max <- test_count[, lapply(.SD, max)]

###############################
#save cleaned and matched data
dt_gte_final <- dt_gte_revenue
saveRDS(dt_gte_final, paste0(dir_output,"/dt_gte.rds"))
saveRDS(dt_imgids_matched, paste0(dir_output, "/dt_imgids_matched.rds"))
saveRDS(dt_revenue_matched,  paste0(dir_output, "/dt_revenue_matched.rds"))

# dt_gte_final <- readRDS(paste0(dir_output,"/dt_gte.rds"))        
# dt_revenue_matched <- readRDS(paste0(dir_output,"/dt_revenue_matched.rds"))        
# 
# min(dt_gte_final$sail_date)
# max(dt_gte_final$sail_date)
# length(unique(dt_gte_final$permit))
# length(unique(dt_gte_final$imgid_chr))
# length(unique(dt_gte_final$trip_id))
# haul_count<- dt_gte_final[,.(count = length(unique(haul_id))), by = imgid_chr]
# min(haul_count$count)
# max(haul_count$count)
# median(haul_count$count)
# 
# gte_count<- dt_gte_final[,.(count = length(unique(GPS_DATETIME))), by = haul_id]
# min(gte_count$count)
# max(gte_count$count)
# median(gte_count$count)
# 
# min(dt_revenue_matched$value_gdp)
# max(dt_revenue_matched$value_gdp)
# median(dt_revenue_matched$value_gdp)

# to add vessel back in (from andy)
# there's a table in the permits schema on sole called vps_vessels
# that has details I think there's also a tabel in the fvtr schema
# that is just for our boats fvtr_vessels maybe

##############################
# Coerce into SF (simple features)
#crs_proj <- st_crs("+init=epsg:4326") # EPSG code for WGS84, which taks XY or long lat

sf_gte_wgs84 <- st_as_sf(x = dt_gte_final,                         
               coords = c("LONGITUDE", "LATITUDE"),
               crs = 4326)

sf_gte_nad83 <- st_transform(sf_gte_wgs84, crs_nad83)

saveRDS(sf_gte_nad83, paste0(dir_output,"/sf_gte_nad83.rds"))

# can't use this - b/c names are too long for a shp file, must be <10 chr
# st_write(sf_gte_nad83, paste(dir_output, "/sf_gte_nad83.shp"), append = FALSE)