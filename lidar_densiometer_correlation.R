
library(tidyverse)
library(janitor)
library(raster)
library(rgdal)

# Load Data
chm <- raster("D:/SERDP/Mission_Creek/LiDAR/chm/mosaic.tif")
creek_points <- readOGR("D:/SERDP/Mission_Creek/field_data/hydro_points.gpkg")
# Reproject Points
creek_points <- spTransform(creek_points, crs(chm))
# Subset CHM to target region
point_buffer <- 100 # m
creek_points_extent_buffered <- extent(creek_points)
creek_points_extent_buffered@xmin <- creek_points_extent_buffered@xmin - point_buffer
creek_points_extent_buffered@xmax <- creek_points_extent_buffered@xmax + point_buffer
creek_points_extent_buffered@ymax <- creek_points_extent_buffered@ymax - point_buffer
creek_points_extent_buffered@ymax <- creek_points_extent_buffered@ymax + point_buffer
chm <- crop(chm, creek_points_extent_buffered)
# Visualize Data
plot(chm, zlim=c(0,20))
plot(creek_points, add=TRUE)

getCanopySample <- function(point_ind)
{
  height_values <- (raster::extract(x=chm, y=creek_points[point_ind,], buffer=5))[[1]]
  length(height_values[height_values < 4])
}

# Mean Cover
creek_points$point_id <- 1:nrow(creek_points)
creek_points_cover <- as.data.frame(creek_points) %>% 
  pivot_longer(1:4, names_to="cover_direction", values_to="cover") %>%
  group_by(point_id) %>% 
  summarize(cover=mean(cover, na.rm=TRUE)/37)
coordinates(creek_points_cover) <- coordinates(creek_points) # add back in spatial data (stripped for Tidyverse compatibility)
crs(creek_points_cover) <- crs(creek_points)
writeOGR(obj=creek_points_cover, layer="creek_points_cover", dsn="D:/SERDP/Mission_Creek/field_data/hydro_point_cover.gpkg", driver="GPKG")

ggplot(as.data.frame(creek_points_cover)) +
  geom_histogram(aes(x=cover), bins=10, breaks=seq(0,10)/10) +
  xlab("Percent Cover") + 
  ylab("Frequency") + 
  ggtitle("Distribution of Cover Measurements")

# Get Neighboring LiDAR CHM Heights
getCanopyMean <- function(point_ind, buffer_size)
{
  height_values <- (raster::extract(x=chm, y=creek_points[point_ind,], buffer=buffer_size))[[1]]
  mean(height_values, na.rm=TRUE)
}
height_means <- as.numeric(lapply(1:43, getCanopyMean, buffer_size=10))
creek_points_cover$height_mean <- height_means
# Get the number of neighboring LiDAR heights below a given value
getCanopyBelowX <- function(point_ind, buffer_size, target_height)
{
  height_values <- (raster::extract(x=chm, y=creek_points[point_ind,], buffer=buffer_size))[[1]]
  length(height_values[height_values>target_height])
}
heights_above_ten <- as.numeric(lapply(1:43, getCanopyBelowX, buffer_size=10, target_height=10))
creek_points_cover$heights_above_ten <- heights_above_ten
# Visualize Relationships
ggplot(as.data.frame(creek_points_cover)) + 
  geom_point(aes(x=height_mean, y=cover))
ggplot(as.data.frame(creek_points_cover)) + 
  geom_point(aes(x=heights_above_ten, y=cover))
