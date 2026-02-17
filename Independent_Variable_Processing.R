# Other script got too long
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

###########
# Independent variables
# county_polygons_test will contain values
suburbanization <- read.csv("Data/other/suburbanization.csv")
suburbanization$Rural_Urban_Continuum_Code_2023 <- factor(suburbanization$Rural_Urban_Continuum_Code_2023, levels = 1:9,
                                                          ordered = TRUE) # Set as factor
suburbanization$FIPS_Code <- str_pad(suburbanization$FIPS_Code, width = 5, pad = "0")
suburbanization <- suburbanization %>%
  rename(GEOID = FIPS_Code) # Rename FIPS
county_polygons_test <- county_polygons %>%
  left_join(suburbanization, by = "GEOID") # Join with county_polygons
county_polygons_test$Rural_Urban_Continuum_Code_2023[551] <- 3 # Fix Kalawao County
county_polygons_test <- county_polygons_test %>%
  rename(rural_urban = Rural_Urban_Continuum_Code_2023) # rename variable
county_polygons_test$rural_urban2 <- factor(county_polygons_test$rural_urban, ordered = FALSE)


economic <- read.csv("Data/other/economic_filtered.csv")
unemployment <- economic[, c("FIPS_Code", "Unemployment_rate_2023")] # retain only these two
unemployment$FIPS_Code <- str_pad(unemployment$FIPS_Code, width = 5, pad = "0") # leading zeroes
unemployment <- unemployment %>%
  rename(GEOID = FIPS_Code) # rename fips
county_polygons_test <- county_polygons_test %>%
  left_join(unemployment, by = "GEOID") # combine with county_polygons
write.csv(unemployment, "Data/other/unemployment.csv") # missing Connecticut, add by hand
unemployment <- read.csv("Data/other/unemployment.csv") # re-upload updated
unemployment$GEOID <- str_pad(unemployment$GEOID, width = 5, pad = "0") # leading zeroes
county_polygons_test <- county_polygons_test %>%
  left_join(unemployment, by = "GEOID") # recombine
county_polygons_test$Unemployment_rate_2023.y[551] <- 0 # add Kalawao
county_polygons_test <- county_polygons_test %>%
  rename(unemployment_rate = Unemployment_rate_2023.y) # rename variable

hate_groups <- read.csv("Data/other/splc_hate_data_2024.csv")
hate_groups <- st_as_sf(hate_groups, coords = c("longitude", "latitude"))
hate_groups <- st_set_crs(hate_groups, 4326)
hate_groups <- st_join(hate_groups, county_polygons, join = st_within)
hate_groups <- hate_groups[hate_groups$statewide == FALSE,]
hate_groups_geoid <- hate_groups %>%
  st_drop_geometry() %>%
  group_by(GEOID) %>%
  summarise(groups = n())
county_polygons_test <- county_polygons_test %>%
  left_join(hate_groups_geoid, by = "GEOID") %>%
  mutate(groups = replace_na(groups, 0))
county_polygons_test$groups_rate <- (county_polygons_test$groups / county_polygons_test$POP) * 100000

# poverty
poverty <- read.csv("Data/other/poverty.csv")
poverty$GEOID <- as.character(poverty$GEOID)
poverty$GEOID <- str_pad(poverty$GEOID, width = 5, pad = "0")
poverty <- poverty[poverty$County.FIPS.Code != 0,]
poverty <- poverty[, c("GEOID", "Poverty.Percent..All.Ages")]
poverty <- poverty %>%
  rename(poverty = Poverty.Percent..All.Ages)
county_polygons_test <- county_polygons_test %>%
  left_join(poverty, by = "GEOID")
county_polygons_test$poverty2 <- as.numeric(county_polygons_test$poverty)

# education
education <- read.csv("Data/other/education_filtered.csv")
education <- education[, c("GEOID", "Percent.of.adults.with.a.bachelor.s.degree.or.higher..2019.23")]
education <- education %>%
  rename(college = Percent.of.adults.with.a.bachelor.s.degree.or.higher..2019.23)
education$GEOID <- as.character(education$GEOID)
education$GEOID <- str_pad(education$GEOID, width = 5, pad = "0")
county_polygons_test <- county_polygons_test %>%
  left_join(education, by = "GEOID")

# ethinicity
ethnicity <- read.csv("Data/other/ethnicity_filter.csv")
dim(ethnicity)
dim(county_polygons_test)
county_polygons_test$white <- ethnicity$white

# political
political <- read_tsv("Data/other/countypres_2000-2024.tab")
political <- political[political$year == 2024 & political$party == "REPUBLICAN",]
political_test <- political %>%
  group_by(county_fips) %>%
  summarise(gop_votes = sum(candidatevotes, na.rm = TRUE), .groups = "drop")
fips_total <- political[, c("county_fips", "totalvotes")]
fips_total <- fips_total %>%
  distinct(county_fips, .keep_all = TRUE)
political_test <- political_test %>%
  left_join(fips_total, by = "county_fips")
political_test <- political_test %>%
  rename(GEOID = county_fips)
political_test$GEOID <- str_pad(political_test$GEOID, width = 5, pad = "0")
anti_join_political <- anti_join(political_test, county_polygons_test, by = "GEOID")
political_test$gop_share <- political_test$gop_votes / political_test$totalvotes
political_test$gop_share <- round(political_test$gop_share, 2)
write.csv(political_test, "partisan.csv")

partisan <- read.csv("Data/other/2024_elections.csv")
partisan <- partisan %>%
  rename(GEOID = county_fips)
partisan$GEOID <- str_pad(partisan$GEOID, width = 5, pad = "0")
anti_join_partisan <- anti_join(partisan, county_polygons_test, by = "GEOID")
write.csv(partisan, "partisan.csv")
partisan <- read.csv("Data/other/partisan.csv")
partisan$GEOID <- str_pad(partisan$GEOID, width = 5, pad = "0")
county_polygons_test <- county_polygons_test %>%
  left_join(partisan, by = "GEOID")
county_polygons_test$X <- NULL
county_polygons_test$state_name <- NULL
county_polygons_test$county_name <- NULL

# religion
religion <- read.csv("Data/other/religion.csv")
religion_test <- religion[, c("FIPS", "TOTRATE_2020")]
religion_test <- religion_test %>%
  rename(GEOID = FIPS)
religion_test$GEOID <- str_pad(religion_test$GEOID, width = 5, pad = "0")
county_polygons_test <- county_polygons_test %>%
  left_join(religion_test, by = "GEOID")
county_polygons_test <- county_polygons_test %>%
  rename(religious_rate = TOTRATE_2020)


# data without coding for FP and FN
adl_groups <- read.csv("adl_groups.csv")
adl <- adl[, c("date_fixed", "season", "city", "state", "type", "ideology","subideology", "group", "description",
               "location_type", "type_of_attack", "israel_zionism_related", "latitude", "longitude")]
adl_groups <- adl_groups[, c("date_fixed", "season", "city", "state", "type", "ideology","subideology", "group", 
                             "description", "location_type", "type_of_attack", "israel_zionism_related", "latitude", "longitude")]
adl_test <- bind_rows(adl, adl_groups)
adl_test <- adl_test[!is.na(adl_test$latitude),] # some are missing, will fix
adl_test$id <- seq_len(nrow(adl_test))
adl_test <- st_as_sf(adl_test, coords = c("longitude", "latitude"))
adl_test <- st_set_crs(adl_test, 4326)
st_write(adl_test, "adl_test.shp")
adl_test <- st_join(adl_test, county_polygons, join = st_within)
adl_test_geoid <- adl_test %>%
  st_drop_geometry() %>%
  group_by(GEOID) %>%
  summarise(incidents = n())
county_polygons_test <- county_polygons_test %>%
  left_join(adl_test_geoid, by = "GEOID") %>%
  mutate(incidents = replace_na(incidents, 0))
county_polygons_test$incident_rate <- (county_polygons_test$incidents / county_polygons_test$POP) * 1000000
st_write(county_polygons_test, "county_polygons_w_variables_01-13-26.shp")

# take out AK and HI
county_polygons2 <- county_polygons_test[county_polygons_test$STATEFP != "02" & county_polygons_test$STATEFP != "15",]



