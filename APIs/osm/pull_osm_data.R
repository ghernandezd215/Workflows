library(tidyverse)
library(sf)
library(osmdata)
library(tigris)

# Grab OSM data for Philadelphia, PA

# Code by Michael Fichman - github.com/mafichman

# Check out the wiki for the key/value pairs for
# Different Amenities - that lets you figure out what you
# Can call from OSM https://wiki.openstreetmap.org/wiki/Category:Tag_descriptions_by_value

# set bounding box - the maximum x-y extent you are interested in
# Do some right-clicking in google maps to figure this out

q0 <- opq(bbox = c(-75.3,39.85,-74.9,40.15)) 

# Parks

park <- add_osm_feature(opq = q0, key = 'leisure', value = "park") %>%
  osmdata_sf(.)

park.sf <- st_geometry(park$osm_polygons) %>%
  st_transform(4326) %>%
  st_sf() %>%
  cbind(., park$osm_polygons$name) %>%
  rename(NAME = park.osm_polygons.name)

# Transportation - Subway

subway <- add_osm_feature(opq = q0, key = 'railway', value = "subway") %>%
  osmdata_sf(.)

subway.sf <- st_geometry(subway$osm_lines) %>%
  st_transform(4326) %>%
  st_sf() %>%
  cbind(., subway$osm_lines$name) %>%
  rename(NAME = subway.osm_lines.name) %>%
  filter(NAME %in% c("Market-Frankford Line", "Broad Street Line"))

# Amenities - Restaurant

# Check in the attributes of `restaurant` and see what the different
# variables and values are.

# These data seem really shaky by the way

restaurant <- add_osm_feature(opq = q0, key = 'amenity', value = "restaurant") %>%
  osmdata_sf(.)

restaurant.sf <- st_geometry(restaurant$osm_points) %>%
  st_transform(4326) %>%
  st_sf() %>%
  cbind(., restaurant$osm_points$name) %>%
  cbind(., restaurant$osm_points$osm_id) %>%
  cbind(., restaurant$osm_points$addr.street) %>%
  cbind(., restaurant$osm_points$addr.postcode) %>%
  cbind(., restaurant$osm_points$addr.city) %>%
  cbind(., restaurant$osm_points$addr.state) %>%
  cbind(., restaurant$osm_points$cuisine)

# Remove "restaurant.osm_points." string from column names
colnames(restaurant.sf) <- gsub("restaurant.osm_points.","",colnames(restaurant.sf))

# Add the features for Philadelphia County
# You can use this to clip things if it's necessary

paCounties <- counties(state = 'PA', cb = FALSE)

phila <- paCounties %>%
  filter(NAME == "Philadelphia") %>%
  st_as_sf() %>%
  st_transform(4326)

# Map it

ggplot()+
  geom_sf(data = phila, color = "black")+
  scale_color_manual(values = c("orange", "blue"))+
  geom_sf(data = park.sf, fill = "light green", color = "transparent")+
  geom_sf(data = subway.sf, aes(color = NAME), size = 1)+
  geom_sf(data = restaurant.sf, color = "red", size = 0.2, alpha = 0.2)
