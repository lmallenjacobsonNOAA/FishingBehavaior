library(keyring)
library(ROracle)
library(tidyverse)
library(lubridate)
library(MASS)

##############################
# Original script from A. Jones

##############################
# Set directories
pwd <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

repository <- "MSA_Wind_FootprintBias"
path_base <- "C:/Users/lianne.allen-jacobso/Documents/"
check_pwd <- paste0(path_base, "Repositories/",repository)
pwd == check_pwd

dir_output <- paste0(path_base, "Output/", repository)
dir_data <- paste0(path_base, "Data/", repository)

##############################
# connect using environment with user name, database specifics, and keyring
usr <- c("ljacobson")
drv <- dbDriver("Oracle")
host <- "sole.nefsc.noaa.gov"
port <- 1526
sid <- "sole"
connect.string <- paste(
  "(DESCRIPTION=",
  "(ADDRESS=(PROTOCOL=tcp)(HOST=", host, ")(PORT=", port, "))",
  "(CONNECT_DATA=(SID=", sid, ")))", sep = "")
con <- dbConnect(drv, username = usr,
                 password = keyring::key_get(service = sid, username = usr),
                 dbname = connect.string)
###########################
#Setting the role - andy used this to access the FVTR schema -not sure if its needed
dbGetQuery(con,"SET ROLE ALL")

#test access
conversion_factors_cfdbs_head <- dbGetQuery(con,"SELECT * FROM CFDBS.SPECIES_ITIS_NE WHERE ROWNUM < 5")
conversion_factors_cfdbs_nespp4_head <- dbGetQuery(con,"SELECT * FROM CFDBS.SPECIES_ITIS_VTR_NESPP4  WHERE ROWNUM < 5")
#vtr_gear_codes_head <- dbGetQuery(con,"SELECT * FROM VTR.VLGEAR WHERE ROWNUM < 5") 
conversion_factors_vers_head <- dbGetQuery(con,"SELECT * FROM FVTR.VERS_APPORTION_CONV_TO_CATCH WHERE ROWNUM < 5") 
trip_list_head <- dbGetQuery(con,"SELECT * FROM FVTR.VERS_TRIP_LIST WHERE ROWNUM < 5") 
stflt_catch_head <- dbGetQuery(con,"SELECT * FROM FVTR.STFLT_CATCH WHERE ROWNUM < 5") 
ncrp_catch_head <- dbGetQuery(con,"SELECT * FROM FVTR.NCRP_VERS_CATCH WHERE ROWNUM  < 5") 
stflt_trip_head <- dbGetQuery(con,"SELECT * FROM FVTR.STFLT_TRIP WHERE ROWNUM  < 5") 
ncrp_trip_head <- dbGetQuery(con,"SELECT * FROM FVTR.NCRP_VERS_TRIP WHERE ROWNUM < 5") 
ncrp_catch_head <- dbGetQuery(con,"SELECT * FROM FVTR.NCRP_VERS_CATCH WHERE ROWNUM < 5") 
fvtr_gear_codes_head <- dbGetQuery(con,"SELECT * FROM FVTR.FVTR_GEAR_CODES WHERE ROWNUM < 5")
gte_join_head <- dbGetQuery(con,"SELECT * FROM NERS.GTE_JOIN WHERE ROWNUM < 5")
#vps_vessels_head <- dbGetQuery(con,"SELECT * FROM PERMIT.VPS_VESSEL WHERE ROWNUM < 5")
apsd_head <- dbGetQuery(con,"SELECT * FROM apsd.dmis_sfclam_040620@GARFO_NEFSC.world WHERE ROWNUM < 5")

conversion_factors_cfdbs_names <- names(conversion_factors_cfdbs_head)
conversion_factors_cfdbs_nespp4_names <- names(conversion_factors_cfdbs_nespp4_head)
conversion_factors_vers_names <- names(conversion_factors_vers_head)

remove(conversion_factors_cfdbs_names,
       conversion_factors_cfdbs_nespp4_names,
       conversion_factors_vers_names)

#getting conversion factors (not the best table but what's in FVTR)
conversion_factors_cfdbs <- dbGetQuery(con,"select * from CFDBS.SPECIES_ITIS_NE")
conversion_factors_cfdbs_nespp4 <- dbGetQuery(con,"select * from CFDBS.SPECIES_ITIS_VTR_NESPP4")

#getting gear codes - do not have access here
#vtr_gear_codes <- dbGetQuery(con,"select * from VTR.VLGEAR")

#getting conversion factors (not the best table but what's in FVTR)
conversion_factors_vers <- dbGetQuery(con,"select * from FVTR.VERS_APPORTION_CONV_TO_CATCH")

###########################
###########################

#Pulling the VERS Trip List View
#This excludes trips reported by Carlos R. vessels, trips with errors and those there were part of research/EFPs
trip_list <- dbGetQuery(con,"select * from FVTR.VERS_TRIP_LIST where deleted=0 and moved_status_flag in ('M','L') and trip_category = 1 and CRB_EXCL = 0")

###########################
###########################

#Getting the catch, effort, landing, and trip data
#Catch and discard info
#stflt_catch <- dbGetQuery(con,"select * from FVTR.STFLT_CATCH where species_itis = 082521")
#ncrp_catch <- dbGetQuery(con,"select * from FVTR.NCRP_VERS_CATCH where species_itis = 082521")

stflt_catch <- dbGetQuery(con,"select * from FVTR.STFLT_CATCH")
ncrp_catch <- dbGetQuery(con,"select * from FVTR.NCRP_VERS_CATCH")

crpp_catch <- bind_rows(stflt_catch %>% mutate(SOURCE='STFLT'),
                        ncrp_catch %>% mutate(SOURCE='NCRP'))

#Effort and gear info
stflt_effort <- dbGetQuery(con,"select * from FVTR.STFLT_EFFORT")
ncrp_effort <- dbGetQuery(con,"select * from FVTR.NCRP_VERS_EFFORT")

crpp_effort <- bind_rows(stflt_effort %>% mutate(SOURCE='STFLT'),
                         ncrp_effort %>% mutate(SOURCE='NCRP'))

#Sail date info (in GMT)
stflt_trip <- dbGetQuery(con,"select * from FVTR.STFLT_TRIP")
ncrp_trip <- dbGetQuery(con,"select * from FVTR.NCRP_VERS_TRIP")

crpp_trip <- bind_rows(stflt_trip %>% mutate(SOURCE='STFLT'),
                       ncrp_trip %>% mutate(SOURCE='NCRP'))

#getting gear codes
fvtr_gear_codes <- dbGetQuery(con,"select * from FVTR.FVTR_GEAR_CODES")

#CRB GTE data
gte_join <- dbGetQuery(con,"select * from NERS.GTE_JOIN")

#Pulling vessel data
# vps_vessels <- dbGetQuery(con,"select * from PERMIT.VPS_VESSEL")

#Pulling apsd table
apsd <- dbGetQuery(con,"select * from apsd.dmis_sfclam_040620@GARFO_NEFSC.world")

#Putting this all together
#And then subsetting down to the Haul-by-haul records
pulled_data <- trip_list %>% 
  dplyr::select(TRIP_ID,VESSEL_PERMIT_NUM,VESSEL_HULL_ID,VESSEL_NAME,SAIL_DATE_LCL,
                REPORT_SOURCE,EVTR,SECTOR,STFLT,EM) %>% 
  inner_join(.,crpp_effort %>% 
               mutate(TRIP_ID=as.character(TRIP_ID))) %>% 
  mutate(EFFORT_ID=paste(TRIP_ID,EFFORT_NUM)) %>%
  inner_join(., crpp_catch %>% mutate(TRIP_ID=as.character(TRIP_ID)) %>%
               dplyr::select(TRIP_ID,EFFORT_NUM,SPECIES_ITIS,GRADE_CODE,
                             MARKET_CODE,HAIL_AMOUNT_UOM,
                             HAIL_AMOUNT,DISPOSITION_CODE) %>%
               mutate(EFFORT_ID=paste(TRIP_ID,EFFORT_NUM))) %>% 
  mutate(AREA_CODE = as.numeric(as.character(FA_AREA_CODE))) %>%
  mutate(AREA_FISHED=case_when(AREA_CODE<=599 ~ 'NE',AREA_CODE>=600 ~ 'MA')) %>%
  inner_join(.,fvtr_gear_codes %>%
               dplyr::select(GEAR_CODE,VTR_GEAR_CODE,ACCSP_GEAR_CODE),
             by=c('GC_GEAR_CODE'='GEAR_CODE')) %>%
  mutate(QUARTER=quarter(SAIL_DATE_LCL),YEAR=year(SAIL_DATE_LCL)) %>%
  filter(REPORT_SOURCE=='HBH')

#SOURCE FOR FUNCTIONS
#Making a function to convert dmm to dd for LAT
LAT_convert <- function(x) {
  result <- vector()
  for(i in 1:length(x)){
    if(!is.na(x[i])) {
      a <- as.numeric(str_sub(x[i],start=1,end=2))
      b <- as.numeric(str_sub(x[i],start=3,end=4))/60
      c <- as.numeric(str_sub(x[i],start=5,end=9))/60
      result[i] <- a+b+c} else {result[i] <- 'NA'}}
  return(result)
}

#Making a function to convert dmm to dd for LON
LON_convert <- function(x) {
  result <- vector()
  for(i in 1:length(x)){
    if(!is.na(x[i])) {
      a <- as.numeric(str_sub(x[i],start=2,end=3))
      b <- as.numeric(str_sub(x[i],start=4,end=5))/60
      c <- as.numeric(str_sub(x[i],start=6,end=10))/60
      result[i] <- -1*(a+b+c)} else {result[i] <- 'NA'}}
  return(result)
}

#Applying the function to the columns of interest
pulled_data_edit <- pulled_data %>%
  mutate(START_HAUL_LAT = LAT_convert(START_HAUL_LAT),END_HAUL_LAT = LAT_convert(END_HAUL_LAT),
         START_SET_LAT = LAT_convert(START_SET_LAT),END_SET_LAT = LAT_convert(END_SET_LAT),
         START_HAUL_LON = LON_convert(START_HAUL_LON),END_HAUL_LON = LON_convert(END_HAUL_LON),
         START_SET_LON = LON_convert(START_SET_LON),END_SET_LON = LON_convert(END_SET_LON))


##MODIFYING THE COOPERATIVE RESEARCH DATA
#Converting depth to meters
pulled_data_edit <- pulled_data_edit %>% mutate(DEPTH = as.numeric(DEPTH),
                                                DEPTH_CONV = case_when(DEPTH_UOM == 'FT' ~ 0.166667,
                                                                       DEPTH_UOM == 'M' ~ 0.54680774278,
                                                                       TRUE ~ 1),
                                                DEPTH = DEPTH * DEPTH_CONV)

#Converting the reported weights using conversion factors
pulled_data_edit_cf <- pulled_data_edit %>%
  left_join(.,conversion_factors_cfdbs %>%
              dplyr::select(SPECIES_ITIS,
                            GRADE_CODE,
                            MARKET_CODE,
                            UNIT_OF_MEASURE,
                            COMMON_NAME,
                            GRADE_DESC,
                            MARKET_DESC,
                            NESPP4,
                            CF_LNDLB_LIVLB,
                            CF_RPTQTY_LNDLB),
            by=c('SPECIES_ITIS'='SPECIES_ITIS',
                 'GRADE_CODE'='GRADE_CODE',
                 'MARKET_CODE'='MARKET_CODE',
                 'HAIL_AMOUNT_UOM'='UNIT_OF_MEASURE')) %>%
  mutate(HAIL_AMOUNT_LB = case_when(HAIL_AMOUNT_UOM!='CN' ~ HAIL_AMOUNT * CF_LNDLB_LIVLB,
                                    TRUE ~ HAIL_AMOUNT * CF_RPTQTY_LNDLB))

#Calculating the amount of Atl herring for each catch record
#Will be zero for hauls without Atl herring
#changed hail column because no unit coversion
pulled_data_cf_sum <-pulled_data_edit_cf %>%
  mutate(LOLIGO_KEPTWT = case_when(SPECIES_ITIS==	'082372' & DISPOSITION_CODE == '011' ~ HAIL_AMOUNT_LB,TRUE ~ 0),
         LOLIGO_DISCARDTWT = case_when(SPECIES_ITIS==	'082372' & DISPOSITION_CODE == '100' ~ HAIL_AMOUNT_LB,TRUE ~ 0),
         SUM_LOLIGO_CATCH = LOLIGO_KEPTWT + LOLIGO_DISCARDTWT)

#Adding a trip total  
pulled_data_cf_tot <- pulled_data_cf_sum %>% group_by(TRIP_ID) %>% 
  mutate(TOT_CATCH = sum(HAIL_AMOUNT_LB,na.rm=TRUE),
         TOT_LOLIGO_CATCH = sum(SUM_LOLIGO_CATCH,na.rm=TRUE)) %>% ungroup()

#Summarising the data to the haul level
#Calculates the total catch weight for the haul
pulled_data_cf_sum_sum <- pulled_data_cf_tot %>% 
  mutate(EFFORT_ID=paste(TRIP_ID,EFFORT_NUM,sep=' ')) %>%
  group_by(SOURCE,REPORT_SOURCE,TRIP_ID,EFFORT_ID,VESSEL_PERMIT_NUM,VESSEL_HULL_ID,VESSEL_NAME,
           ACCSP_GEAR_CODE,VTR_GEAR_CODE,AREA_CODE,SAIL_DATE_LCL,
           QUARTER,YEAR,EVTR,SECTOR,STFLT,EM,
           EFFORT_NUM,END_SET_LAT,END_SET_LON,START_HAUL_LAT,
           START_HAUL_LON,END_HAUL_DATE_GMT,END_HAUL_LAT,END_HAUL_LON,
           DEPTH,DEPTH_UOM,GEAR_QUANTITY,
           GEAR_SIZE,MESH_SIZE,#MESH_TYPE,MESH_SIZE_AB,CODCAT,
           NUM_HAULS,SOAK_DURATION_DH,TOT_CATCH,TOT_LOLIGO_CATCH) %>% 
  summarise(LOLIGO_KEPTWT = sum(LOLIGO_KEPTWT,na.rm=TRUE),
            LOLIGO_DISCARDTWT = sum(LOLIGO_DISCARDTWT,na.rm=TRUE),
            SUM_LOLIGO_CATCH = sum(SUM_LOLIGO_CATCH,na.rm=TRUE)
  )

##RENAMING SOME COLUMNS
#Subsetting the study fleet data down to specific columns
pulled_data_cf_sum_sum_sub <- pulled_data_cf_sum_sum %>% ungroup() %>%
  dplyr::select(permit=VESSEL_PERMIT_NUM,area=AREA_CODE,
                trip_id=TRIP_ID,
                haul_num=EFFORT_NUM,
                haul_id=EFFORT_ID,
                sail_date=SAIL_DATE_LCL,
                start_lat=END_SET_LAT,
                start_lon=END_SET_LON,
                end_lat=START_HAUL_LAT,
                end_lon=START_HAUL_LON,
                depth=DEPTH,
                effort_dur=SOAK_DURATION_DH,
                gear_code=VTR_GEAR_CODE,
                #mesh_size=MESH_SIZE,
                LOLIGO_KEPTWT,
                LOLIGO_DISCARDTWT,
                SUM_LOLIGO_CATCH,
                TOT_CATCH,
                TOT_LOLIGO_CATCH) %>% 
  mutate(source='crb') %>%
  mutate(start_lat=as.numeric(as.character(start_lat)),
         start_lon=as.numeric(as.character(start_lon)),
         end_lat=as.numeric(as.character(end_lat)),
         end_lon=as.numeric(as.character(end_lon)))
#adding a shared gear column
# pulled_data_cf_sum_sum_sub <-
#   pulled_data_cf_sum_sum_sub %>%
  # left_join(.,vtr_gear_codes %>% 
  #             dplyr::select(GEARCODE,NEGEAR) %>% 
  #             distinct() %>% drop_na() %>% arrange(GEARCODE) %>% 
  #             group_by(GEARCODE) %>% top_n(1),
  #           by=c('gear_code'='GEARCODE')) %>%
#  dplyr::rename(gear_code_vtr=gear_code,gear_code_obs=NEGEAR)

#Subsetting this down to records where loligo are >0.39 of the catch
#This comesd from the management world where >40% loligo is used to identify trips in the fishery
pulled_lf_squid_trips <- pulled_data_cf_sum_sum_sub %>%
  filter(year(sail_date)<2020,year(sail_date)>2015) %>%
  filter(TOT_LOLIGO_CATCH/TOT_CATCH > 0.39999)


##MAKING SOME DATA PROJECTS

#Making a GTE join product
#Doing this before bringing in the APSD data because it will help determine
#how many trips are not matching that we care about
pulled_lf_squid_trips_gte <-
  pulled_lf_squid_trips %>%
  left_join(.,gte_join %>%
              mutate(haul_id=paste(TRIP_ID,EFFORT_NUM),
                     has_GTE='YES'))

##Adding the APSD data
#Looking at the number of trips in Ben's table
#using a straight match between SF trip_id and DMIS' DOCID
total_number <- pulled_lf_squid_trips_gte  %>% .$trip_id %>% unique() %>% length()

matched_number <- pulled_lf_squid_trips_gte  %>% .$trip_id %>% unique() %>% 
  intersect(.,apsd %>% #mutate(DOCID=str_sub(DOCID,1,14)) %>%
              .$DOCID %>% 
              unique()) %>% length()

#Looking at the proportion of the Study Fleet trips that match TRIP_ID to DOCID
matched_number/total_number

#Joining the data sets to bring over the IMGID that is equivalent to GEARID
SF_MATCHED_TO_APSD <- pulled_lf_squid_trips_gte %>% left_join(.,
                                                              apsd %>% dplyr::select(IMGID,DOCID,DATE_TRIP,PERMIT) %>%
                                                                mutate(PERMIT=as.character(PERMIT)) %>% distinct(),
                                                              #by=c('permit'='PERMIT','sail_date'='DATE_TRIP')) %>% 
                                                              by=c('TRIP_ID'='DOCID')) %>% 
  filter(!is.na(IMGID)) %>% #.$IMGID %>% as.character() %>% unique()
  dplyr::select(PERMIT,permit,trip_id,IMGID,sail_date,DATE_TRIP) %>% distinct()

#writing a file
#write_csv(SF_MATCHED_TO_APSD,'SF_MATCHED_TO_APSD.csv')
saveRDS(SF_MATCHED_TO_APSD, paste0(dir_output,"/SF_MATCHED_TO_APSD.rds"))

#Looking at trips that didn't match                                        
pulled_lf_squid_trips_gte %>% dplyr::select(trip_id,permit,sail_date) %>% 
  filter(trip_id==31047310060109) %>%
  distinct() %>% arrange(desc(sail_date))

########################################
# more years
#Subsetting this down to records where loligo are >0.39 of the catch
#This comesd from the management world where >40% loligo is used to identify trips in the fishery
pulled_lf_squid_trips_allyrs <- pulled_data_cf_sum_sum_sub %>%
  filter(year(sail_date)<2023,year(sail_date)>2006) %>%
  filter(TOT_LOLIGO_CATCH/TOT_CATCH > 0.39999)


##MAKING SOME DATA PROJECTS

#Making a GTE join product
#Doing this before bringing in the APSD data because it will help determine
#how many trips are not matching that we care about
pulled_lf_squid_trips_gte_allyrs <-
  pulled_lf_squid_trips_allyrs %>%
  left_join(.,gte_join %>%
              mutate(haul_id=paste(TRIP_ID,EFFORT_NUM),
                     has_GTE='YES'))

##Adding the APSD data
#Looking at the number of trips in Ben's table
#using a straight match between SF trip_id and DMIS' DOCID
total_number <- pulled_lf_squid_trips_gte_allyrs  %>% .$trip_id %>% unique() %>% length()

matched_number <- pulled_lf_squid_trips_gte_allyrs  %>% .$trip_id %>% unique() %>% 
  intersect(.,apsd %>% #mutate(DOCID=str_sub(DOCID,1,14)) %>%
              .$DOCID %>% 
              unique()) %>% length()

#Looking at the proportion of the Study Fleet trips that match TRIP_ID to DOCID
matched_number/total_number

#Joining the data sets to bring over the IMGID that is equivalent to GEARID
SF_MATCHED_TO_APSD <-
  pulled_lf_squid_trips_gte %>%
  left_join(.,
            apsd %>%
              dplyr::select(IMGID,DOCID,DATE_TRIP,PERMIT) %>%
              mutate(PERMIT=as.character(PERMIT)) %>% distinct(),
            #by=c('permit'='PERMIT','sail_date'='DATE_TRIP')) %>% 
            by=c('TRIP_ID'='DOCID')) %>% 
  filter(!is.na(IMGID)) %>% #.$IMGID %>% as.character() %>% unique()
  dplyr::select(PERMIT,permit,trip_id,IMGID,sail_date,DATE_TRIP) %>% distinct()

#writing a file
#write_csv(SF_MATCHED_TO_APSD,'SF_MATCHED_TO_APSD.csv')
saveRDS(SF_MATCHED_TO_APSD, paste0(dir_output,"/SF_MATCHED_TO_APSD.rds"))



apsd %>% dplyr::select(IMGID,DOCID,DATE_TRIP,PERMIT) %>%
  filter(year(DATE_TRIP)==2010) %>%
  mutate(DOCID=as.character(DOCID)) %>% 
  filter(PERMIT== 310473) %>% arrange(DATE_TRIP)

#look at trips
length(unique(SF_MATCHED_TO_APSD$IMGID))

