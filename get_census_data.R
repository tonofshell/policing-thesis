library(tidyverse)
library(ggplot2)
library(here)
library(ggmap)
library(maps)
library(mapdata)
library(tidycensus)
library(sf)
source("api_keys.R")
census_api_key(census_key, install = TRUE)

if (FALSE) {
  dec_vars_10 = load_variables(year = 2010, dataset = "sf1")
  saveRDS(dec_vars_10, file = "Data/dec_vars_10.rds")
}

vars = c(total_housing_units="H001001",
         total_hu_occupied="H003002",
         total_hu_vacant="H003003",
         total_hu_owned_loan="H004002",
         total_hu_owned="H004003",
         total_hu_rented="H004004",
         total_pop_in_hus="H011001",
         avg_household_size="H012001",
         total_pop_one_pers_hh="H013002",
         total_pop="P001001",
         total_pop_urban="P002002",
         total_pop_urban_areas="P002003",
         total_pop_urban_clusters="P002004",
         total_pop_rural="P002005",
         total_pop_white="P003002",
         total_pop_black="P003003",
         total_pop_native="P003004",
         total_pop_asian="P003005",
         total_pop_islander="P003006",
         total_pop_other_race="P003007",
         total_pop_two_plus_races="P003008",
         total_pop_latino="P005010",
         total_pop_latino_white="P005011",
         total_pop_latino_black="P005012",
         total_pop_latino_native="P005013",
         total_pop_latino_asian="P005014",
         total_pop_latino_islander="P005015",
         total_pop_latino_other_race="P005016",
         total_pop_latino_two_plus_races="P005017",
         total_pop_male="P012002",
         total_pop_female="P012026",
         median_age="P013001",
         total_pop_under_18="P016002",
         total_pop_family_hh="P018002",
         avg_family_size="P037001",
         total_pop_husb_wife_fam="P038002")

new_names = c("GEOID",
              "NAME",
              "avg_family_size",
              "avg_household_size",
              "median_age",
              "total_housing_units",
              "hu_occupied",
              "hu_owned",
              "hu_owned_loan",
              "hu_rented",
              "hu_vacant",
              "total_pop",
              "pop_asian",
              "pop_black",
              "pop_family_hh",
              "pop_female",
              "pop_husb_wife_fam",
              "pop_in_hus",
              "pop_islander",
              "pop_latino",
              "pop_latino_asian",
              "pop_latino_black",
              "pop_latino_islander",
              "pop_latino_native",
              "pop_latino_other_race",
              "pop_latino_two_plus_races",
              "pop_latino_white",
              "pop_male",
              "pop_native",
              "pop_one_pers_hh",
              "pop_other_race",
              "pop_rural",
              "pop_two_plus_races",
              "pop_under_18",
              "pop_urban",
              "pop_urban_areas",
              "pop_urban_clusters",
              "pop_white",
              "geometry")

# State 17 is Illinois, county 31 is Cook County

dec_tract_2010 =  get_decennial(geography = "tract", year = 2010, geometry = TRUE, state = "17", county = "31",
                                variables = vars)
dec_block_2010 =  get_decennial(geography = "block", year = 2010, geometry = TRUE, state = "17", county = "31",
                                variables = vars, show_call = T)

dec_tract_2010 = dec_tract_2010 %>% spread("variable", "value")
dec_block_2010 = dec_block_2010 %>% spread("variable", "value")

saveRDS(dec_tract_2010, file = "Data/dec_tracts_2010.rds")
saveRDS(dec_block_2010, file = "Data/dec_blocks_2010.rds")

tract_no_geom = dec_tract_2010 %>% st_drop_geometry()

dec_tract_2010_formatted = tract_no_geom[,1:6] %>% bind_cols(tract_no_geom[,7:11] / tract_no_geom$total_housing_units) %>% bind_cols(tract_no_geom[,12]) %>% bind_cols(tract_no_geom[,13:38] / tract_no_geom$total_pop) %>% bind_cols(dec_tract_2010[,39]) %>% st_as_sf()
names(dec_tract_2010_formatted) = new_names

block_no_geom = dec_block_2010 %>% st_drop_geometry()
dec_block_2010_formatted = block_no_geom[,1:6] %>% bind_cols(block_no_geom[,7:11] / block_no_geom$total_housing_units) %>% bind_cols(block_no_geom[,12]) %>% bind_cols(block_no_geom[,13:38] / block_no_geom$total_pop) %>% bind_cols(dec_block_2010[,39]) %>% st_as_sf()
names(dec_block_2010_formatted) = new_names

saveRDS(dec_tract_2010_formatted, file = "Data/dec_tracts_2010_prop.rds")
saveRDS(dec_block_2010_formatted, file = "Data/dec_blocks_2010_prop.rds")

