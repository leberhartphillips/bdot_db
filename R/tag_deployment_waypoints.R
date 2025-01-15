# remotes::install_github("mpio-be/dbo")
library(dbo)
library(DBI)
library(tidyverse)
library(hms)
library(sf)
library(xml2)

# establish a connection to the entire database (i.e., that I have access to)
con = dbcon()

# connect to the FIELD_2024_BADOatNZ local database
dbExecute(con, "USE FIELD_2024_BADOatNZ")

# wrangle the CAPTURES and GPS_POINTS tables to extract the deployment times and
# locations for all tag deployments
bdot_tag_deploymenets_2024 <- 
  dbq(con, "SELECT * FROM CAPTURES") %>% 
  filter(!is.na(tag_id)) %>% 
  rowwise() %>% 
  mutate(time = as.hms(max(cap_start, caught, released, na.rm = TRUE))) %>% 
  group_by(ring) %>%
  arrange(date, time) %>% 
  slice(1) %>% 
  ungroup() %>% 
  left_join(., dbq(con, "SELECT * FROM GPS_POINTS"), by = c("gps_id", "gps_point")) %>% 
  mutate(across(c(UL, LL, UR, LR), ~ gsub("[,[:space:]]", "", .))) %>%
  mutate(UR = if_else(nchar(UR) == 2, paste0("FW", UR), UR))  %>%
  mutate(across(c(LL, LR), ~ str_replace(., "M", ""))) %>%
  mutate(across(c(UL, LL, UR, LR), ~ if_else(is.na(.), "X", .)))  %>%
  mutate(mark = case_when(
    nchar(LL) == 2 & nchar(LR) == 2 ~ paste0(LL, "-", LR),  # LL and LR have 2 characters
    nchar(UL) == 4 ~ UL,  # UL has 4 characters
    nchar(UR) == 4 ~ UR,  # UR has 4 characters
    TRUE ~ NA_character_  # If none of the conditions are met, assign NA
  )) %>%
  mutate(mark = if_else(
    str_starts(mark, "FW"),  # Check if the string starts with "FW"
    str_replace(mark, "^FW", "Flag-"),  # Replace "FW" with "Flag-" at the start
    mark  # Otherwise, keep the original value
  )) %>% 
  mutate(local_deployment_time = as.POSIXct(with_tz(ymd_hms(paste(date, time, sep = " "), tz = 'Pacific/Auckland'), 'Pacific/Auckland'))) %>% 
  select(site, local_deployment_time, field_sex, ring, mark, tag_id, tag_type, lat, lon)

write.csv(bdot_tag_deploymenets_2024, "data/bdot_tag_deployments_2024.csv", row.names = FALSE)

# Remove rows with missing latitude or longitude
bdot_tag_deploymenets_2024 <- 
  na.omit(bdot_tag_deploymenets_2024[, c("lon", "lat")])

coordinates <- bdot_tag_deploymenets_2024[, c("lon", "lat")]

# Create an sf object from the coordinates
sf_points <- st_as_sf(coordinates, coords = c("lon", "lat"), crs = 4326)

# Create a GPX structure
gpx <- xml_new_root("gpx", version = "1.1", xmlns = "http://www.topografix.com/GPX/1/1")

# Add a track element to the GPX structure
trk <- xml_add_child(gpx, "trk")
trkseg <- xml_add_child(trk, "trkseg")

# Loop through the sf_points and add each point as a 'trkpt' in the GPX file
for (i in 1:nrow(bdot_tag_deploymenets_2024)) {
  xml_add_child(trkseg, "trkpt", lat = bdot_tag_deploymenets_2024$lat[i], lon = bdot_tag_deploymenets_2024$lon[i])
}

# Write the GPX file
write_xml(gpx, "data/bdot_tag_deployments_2024.gpx")
