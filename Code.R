# Road Density

library(tidyverse)
library(dplyr)
library(sf)
library(usmap)
library(tigris)

# counties

counties_geoms <- us_map(regions = "counties", data_year = 2024)
counties <- as.data.frame(counties_geoms) %>% mutate(state = substr(fips, 1, 2), county = substr(fips, 3, 5))%>% select(state, county)

# roads()
# LINEARID is a unique ID for a road, but a road can be in more than 1 county
# Luckily, when a road is in a different county, the length in that county is
# recorded with a duplicated LINEARID rather than being added in the same entry.

for (i in 1:nrow(counties)) {
  roads_i <- roads(state = counties[i,1], county = counties[i,2], year = 2024) %>% 
    mutate(road_length = as.numeric(st_length(.))) %>% 
    as.data.frame() %>% select(-geometry)
  if(i == 1) {
    counties_roads <- data.frame(
      fips = paste0(counties[i,1], counties[i,2]),
      linearid = roads_i$LINEARID,
      mtfcc = roads_i$MTFCC,
      road_length = roads_i$road_length
    )
  } else {
    counties_roads <- rbind(
      counties_roads,
      data.frame(
        fips = paste0(counties[i,1], counties[i,2]),
        linearid = roads_i$LINEARID,
        mtfcc = roads_i$MTFCC,
        road_length = roads_i$road_length
      )
    )
  }
}

# road stats
# use https://www2.census.gov/geo/pdfs/reference/mtfccs2022.pdf
# for reference to the types of roads
# counties_roads %>% select(mtfcc) %>% unique()

counties_roads_stats <- counties_roads %>% group_by(fips, mtfcc) %>%
  summarize(
    road_count = n(),
    road_length = sum(road_length, na.rm = FALSE)
  ) %>%
  left_join(counties_geoms %>% mutate(county_area = as.numeric(st_area(.))) %>% as.data.frame() %>% select(-geom), by = "fips") %>% 
  left_join(countypop %>% select(fips, pop_2022), by = "fips") %>% 
  mutate(
    road_count_to_county_area = road_count/county_area,
    road_length_to_county_area = road_length/county_area,
    road_count_to_county_pop = road_count/pop_2022,
    road_length_to_county_pop = road_length/pop_2022
  ) %>% ungroup() %>% rename(state_abbr = abbr, state_full = full) %>% 
  select(state_abbr, state_full, county, fips, county_area, pop_2022, mtfcc, 
         road_count, road_length, road_count_to_county_area, road_length_to_county_area,
         road_count_to_county_pop, road_length_to_county_pop)
# write_csv(counties_roads_stats, "road-density/counties_roads_stats.csv")