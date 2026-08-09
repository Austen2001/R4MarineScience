# Workshop 4 Spatial data in R

# install packages if needed
# install.packages("sf")
# install.packages("terra")
# install.packages("tmap")

# load packages
library(tidyverse)
library(sf)
library(terra)
library(tmap)

# load copepod data
dat <- read_csv("data/data-for-course/copepods_raw.csv")
dat

# plot sample locations
ggplot(dat) +
  aes(x = longitude, y = latitude, color = richness_raw) +
  geom_point()

# look at richness by latitude
ggplot(dat, aes(x = latitude, y = richness_raw)) +
  stat_smooth() +
  geom_point()

# turn the data into spatial points
sdat <- st_as_sf(
  dat,
  coords = c("longitude", "latitude"),
  crs = 4326
)

# check spatial data
sdat

# check CRS
crs4326 <- st_crs(4326)

crs4326

# CRS name
crs4326$Name

# CRS details
crs4326$wkt

# look at sf data
sdat

# plot richness
plot(sdat["richness_raw"])

# plot all variables
plot(sdat)

# make a richness map
tm_shape(sdat) +
  tm_dots(col = "richness_raw")

# load Australia shapefile
aus <- st_read(
  "data/data-for-course/spatial-data/Aussie/Aussie.shp"
)

# load continental shelf shapefile
shelf <- st_read(
  "data/data-for-course/spatial-data/aus_shelf/aus_shelf.shp"
)

# check Australia data
aus

# plot continental shelf
tm_shape(shelf) +
  tm_polygons()

# add all map layers
tm_shape(shelf, bbox = sdat) +
  tm_polygons() +
  tm_shape(aus) +
  tm_polygons() +
  tm_shape(sdat) +
  tm_dots()

# change map style
tmap_style("beaver")

# view map again
tm_shape(shelf, bbox = sdat) +
  tm_polygons() +
  tm_shape(aus) +
  tm_polygons() +
  tm_shape(sdat) +
  tm_dots()

# tmap vignette
# vignette("tmap-getstarted")

# save final map
tm1 <- tm_shape(shelf, bbox = sdat) +
  tm_polygons() +
  tm_shape(aus) +
  tm_polygons() +
  tm_shape(sdat) +
  tm_dots()

# export map
tmap_save(
  tm1,
  filename = "Richness-map.png",
  width = 600,
  height = 600
)