# Dependent Variable Alterations
# Jan. 26, 2026
setwd("/Users/charleszinn/Desktop/THESIS!!!/Data, Variables")
library(dplyr)
library(stringr)
library(tidygeocoder)
library(lubridate)
library(ggrepel)
library(sf)
library(tidytext)
library(rnaturalearth)
library(rnaturalearthdata)
library(tigris)
library(tidyr)
library(readr)

# Basic Processing
data_incidents <- read.csv("adl_raw_label.csv") # Read in raw ADL data from 1/2016 - 7/2025
data_incidents$date_fixed <- as.Date(data_incidents$date_fixed, format = "%m/%d/%y") # Put dates into date format
data_incidents <- data_incidents %>% # Remove Alaska and Hawaii
  filter(!state %in% c("HI", "AK"))
write.csv(data_incidents, "data_incidents.csv")
data_groups <- read.csv("adl_group_data2.csv") # Same for group data
data_groups <- data_groups %>% # Need to geocode since using expanded data set
  mutate(is_county = str_detect(city, regex("county", ignore_case = TRUE)), 
         location = if_else(is_county, paste(city, state), paste(city, state)))
data_groups <- data_groups %>%
  filter(!state %in% c("HI", "AK"))
data_groups_city <- data_groups[data_groups$is_county == FALSE,]
data_groups_county <- data_groups[data_groups$is_county == TRUE,]
geo <- data_groups_city %>%
  geocode(method = 'osm', city = city, state = state, 
          lat = latitude , long = longitude)
geo1 <- data_groups_county %>%
  geocode(method = 'osm', county = city, state = state, 
          lat = latitude , long = longitude)
geo <- bind_rows(geo, geo1) # Geocoded group data
data_groups <- geo
write.csv(data_groups, "data_groups.csv")
fix <- read.csv("data_incidents_fix.csv")
fix <- fix %>%
  mutate(is_county = str_detect(city, regex("county", ignore_case = TRUE)), 
         location = if_else(is_county, paste(city, state), paste(city, state)))
fix_city <- fix[fix$is_county == FALSE,]
fix_county <- fix[fix$is_county == TRUE,]
fix_city <- fix_city %>%
  geocode(method = 'osm', city = city, state = state, 
          lat = latitude , long = longitude)
fix_county <- fix_county %>%
  geocode(method = 'osm', county = city, state = state, 
          lat = latitude , long = longitude)
write.csv(fix_city, "fix_city.csv")
write.csv(fix_county, "fix_county.csv") # Manually fix NA lat/longs in Excel

data_groups <- read.csv("data_groups.csv") # After fixing NA/incorrect locations
data_incidents <- read.csv("data_incidents.csv")
data_incidents <- data_incidents[!is.na(data_incidents$latitude),]
write.csv(data_incidents, "data_incidents.csv")

# Identifying incidents from new methods
keywords <- c("\\ban anti-israel rally\\b", "\\ban anti-israel protest\\b", "\\ban anti-israel protester\\b",
              "\\bfrom the river to the sea\\b", "\\bthere is only one solution\\b", 
              "\\brespect existence or expect resistance\\b", "\\bresistance is justfied\\b", "\\bend zionism\\b",
              "\\bzionism is terrorism\\b", "\\bglobalize the intifada\\b", "\\bby all means necessary\\b", 
              "\\ban anti-israel demonstrator\\b", "\\ban anti-israel encampment\\b", "\\ban anti-israel day encampment\\b",
              "\\ban anti-israel demonstration\\b")
pattern <- paste(keywords, collapse = "|")
new_incidents <- data_incidents[grepl(pattern, data_incidents$description, ignore.case = TRUE), ]
data_incidents <- data_incidents %>%
  mutate(flag_2 = ifelse(id %in% new_incidents$id, 1, 0))
write.csv(data_incidents, "data_incidents_nlp.csv")

# Fixing false negatives and positives after conducting semantic analysis in Python
data_incidents <- read.csv("data_incidents_nlp.csv")
data_incidents_postOct7 <- data_incidents[data_incidents$date_fixed > "2023-10-07",]
data_incidents_flag <- data_incidents[data_incidents$zionism_score_axis < 0.5 & # "Flag" being rally-based anti-Zionism
                                        data_incidents$flag_2 == 0,] # Incidents that weren't flagged but maybe should be
data_incidents_falsepos <- data_incidents[data_incidents$zionism_score_axis > 0.5 &
                                        data_incidents$flag_2 == 1,] # Incidents that were flagged but maybe shouldn't
write.csv(data_incidents_flag, "data_incidents_flag.csv")
write.csv(data_incidents_falsepos, "data_incidents_falsepos.csv")

data_incidents_flag <- read.csv("data_incidents_flag.csv") # Amending false negatives
data_incidents_flag <- data_incidents_flag[data_incidents_flag$flag_2 == 1,]
idx <- match(data_incidents_flag$id, data_incidents$id)
data_incidents$flag_2[idx[!is.na(idx)]] <- data_incidents_flag$flag_2[!is.na(idx)]

sum(data_incidents$flag_2)

data_incidents_falsepositive <- read.csv("data_incidents_falsepos.csv") # Amending false positives
data_incidents_falsepositive <- data_incidents_falsepositive[data_incidents_falsepositive$flag_2 == 0,]
idx0 <- match(data_incidents_falsepositive$id, data_incidents$id)
data_incidents$flag_2[idx0[!is.na(idx0)]] <- data_incidents_falsepositive$flag_2[!is.na(idx0)]

data_incidents$date_fixed <- as.Date(data_incidents$date_fixed, # Excel keeps unformating the dates...
                                     format = "%m/%d/%y")

test3 <- data_incidents[!grepl("\\bantisemitic\\b", # Removing non-antisemitic incidents
                               data_incidents$type, ignore.case = TRUE), ]
data_incidents <- data_incidents[grepl("\\bantisemitic\\b", data_incidents$type, ignore.case = TRUE), ]

data_incidents_flag <- data_incidents[data_incidents$flag_2 == 1,] # 4424 flagged events

data_incidents_flag <- read.csv("data_incidents_flag.csv")
data_incidents_flag <- data_incidents_flag[data_incidents_flag$flag_2 == 0,] # One last go around, checking again
idx1 <- match(data_incidents_flag$id, data_incidents$id) # Generally unflagging "khaybar" chants and targeted incidents
data_incidents$flag_2[idx1[!is.na(idx1)]] <- data_incidents_flag$flag_2[!is.na(idx1)]
write.csv(data_incidents, "data_incidents.csv")

# Processing groups (fixing lat/long NAs)
data_groups
data_groups <- data_groups[grepl("\\bantisemitic\\b", data_groups$type, ignore.case = TRUE), ] # 3244 incidents
write.csv(data_groups, "data_groups.csv")
data_groups_fix <- read.csv("data_groups_fix.csv")
data_groups_fix <- data_groups_fix[is.na(data_groups_fix$latitude),]
data_groups_fix <- data_groups_fix %>%
  geocode(method = 'osm', city = city, state = state, 
          lat = latitude , long = longitude)
write.csv(data_groups_fix, "groups_fixed.csv") # Then put back into data_groups_fixed
data_groups <- read.csv("data_groups_fix.csv") # Back in
data_groups <- data_groups[data_groups$group != "",] # Remove entries with no group

# Combine
data <- read.csv("data.csv")
data <- data[data$flag_2 == 0,]
data$flag_2 <- NULL
data$X <- NULL # Tried bind_rows but wasn't working, so exported to excel and just combined there

data_combined <- read.csv("data.csv") # Final combined dataset, 29148 incidents

# Count points in polygons
county_polygons2

data_combined_sf <- st_as_sf(data_combined, coords = c("longitude", "latitude"))
data_combined_sf <- st_set_crs(data_combined_sf, 4326)
data_combined_sf <- st_transform(data_combined_sf, 4269)
data_combined_sf_TEST <- st_join(data_combined_sf, county_polygons2, join = st_within) # Each point has county data
data_grouped_geoid <- data_combined_sf_TEST %>%
  st_drop_geometry() %>% # Make just a normal dataframe
  group_by(GEOID) %>% # Group by GEOID (becomes new key)
  summarise(incidents = n()) # Create new column with the number of incidents, condense rows
data_county <- county_polygons2 %>%
  left_join(data_grouped_geoid, by = "GEOID") %>%
  mutate(incidents = replace_na(incidents, 0))

# Side issue: Wichita Falls area doesn't have Jewish pop estimates
wichita <- data_county[, c("GEOID", "jewish_pop_county")]
wichita <- st_drop_geometry(wichita)
write.csv(wichita, "wichita.csv")
wichita <- read.csv("wichita.csv")
wichita$GEOID <- str_pad(wichita$GEOID, width = 5, pad = "0")
wichita$GEOID <- as.character(wichita$GEOID)
wichita <- wichita %>%
  rename(jewish_pop = jewish_pop_county)
data_county <- data_county %>%
  left_join(wichita, by = "GEOID")

# Fixed that
data_county <- data_county %>%
  mutate(incident_rate = if_else(jewish_pop == 0, NA_real_, incidents / jewish_pop))

nrow(data_county[data_county$incident_rate == 0,])
dim(data_county)
data_county$incident_rate <- data_county$incident_rate * 1000
data_county$pop_density <- (data_county$POP / (data_county$ALAND / 1000000))
st_write(data_county, "data_county.shp")


# Remove NA for counties with zero incidents and zero Jews
data_county <- data_county %>%
  mutate(incdnt_ = ifelse(is.na(incdnt_), 0, incdnt_))




