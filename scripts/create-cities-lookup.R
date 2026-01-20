# prior versions of this script are named assign-faz-lg-area-cities.R. This version is developed
# for cities 2023 with 116 records in Jan 2026
# the cities lookup tables are created from the current citiesXX_dashboard layer (ask Peter)
# need to manually edit the following large areas: UU Snohomish (Everett lg area), Bremerton UGA, Pierce County Enumclaw

library(sf)
library(psrcelmer)
library(dplyr)

cities_name <- 'cities18_dashboard' # will change to cities23_dashboard
faz_lrg_area_name <- 'faz_lrg_areas2010'

cities <- st_read_elmergeo(cities_name)
fla <- st_read_elmergeo(faz_lrg_area_name)

# convert to point
cities_pt<- st_make_valid(cities) |> 
  st_centroid(cities2)

# What are the new records?
old_cities_01 <- read.csv("data/cities18_ref.csv") # contains large area, large area group names
old_cities_02 <- read.csv("data/cities18.csv") # main city information
old_cities_03 <- read.csv("data/cities18_b_can_delete.csv") # 01 and 03 are the same

all_duplicates <- cities |> 
  select(county_id, city_name, rg_name) |> 
  group_by(city_name, county_id) |> 
  filter(n() > 1) |> 
  ungroup()

# overlay with faz large area 2010
cfla <- st_join(cities_pt, fla)

# divide into large area groups using old information in cities23.csv
oc <- old_cities_02 |> 
  select(county_id, city_name, old_large_area = large_area, old_lgarea_group = lgarea_group) |> 
  mutate(city_name = ifelse(city_name == "Sea Tac", "SeaTac", city_name),
         city_name = ifelse(city_name =="Uninc Urban Snoh", "Uninc Urban Snohomish", city_name))

cfla_for_edit <- cfla |> 
  left_join(oc, by = c("county_id", "city_name")) |> 
  st_drop_geometry()

openxlsx::write.xlsx(cfla_for_edit, "data/cities23.xlsx")

# Open xlsx and manually edit to set the following:
# Bremerton UGA = Kitsap, Central, North, and South Kitsap
# Uninc Urban Snohomish (Everett) = Everett, Everett
# Enumclaw (53) = Pierce Other, Pierce, Pierce Other, Pierce Other (1)
# Delete unnecessary columns

new_cities_01 <- openxlsx::read.xlsx("data/cities23.xlsx")
write.csv(new_cities_01, "data/cities23.csv", row.names = FALSE)
