# Load packages
library(sf)
library(tmap)
library(GISTools)
library(spatstat)

# Import shapefiles
# shapefile of US State boundaires
states.sf <- st_read("####.shp")
# shapefile of tornado touchdown points
tornus.sf <- st_read("####.shp")
# shapefile of a frame to clip other data to
frame.sf <- st_read("####.shp")

# convert shapefiles for sf to sp format
tornus.sp <- as(tornus.sf, "Spatial")
states.sp <- as(states.sf, "Spatial")
frame.sp <- as(frame.sf, "Spatial")

#Create Kernel density plot of tornadoes in the US
#plot tornado points
plot(tornus.sp, pch=16, cex=.05, col = "red", main = "United States Tornado Touchdowns")
# run the kernel density estimation
torn_kde <- kde.points(tornus.sp, 300000, n=300, lims=NULL)
#plot the kernel density estimation as a raster
level.plot(torn_kde, add = TRUE)
#Add the state boundaries on top
plot(states.sp, border = 1, add = TRUE)

# Analysis of Tornado clustering in the Midwest
#Clip original tornado and state shapefiles to midwest frame
tornus_clip <- tornus.sf[frame.sf,]
states_clip <- st_intersection(frame.sf, states.sf)

#map resulting clipped data
tm_shape(frame.sf)+
  tm_borders(col="black") +
  tm_layout(frame=F) +
  tm_shape(tornus_clip) +
  tm_dots(col = NA, size = 0.02, shape = 16, title = NA,
          legend.show = TRUE, legend.is.portrait = TRUE) +
  tm_shape(states_clip) +
  tm_borders(col="grey70",lw=2) +
  tm_layout (frame=F)
# covert clipped tornado touchdown points to sp and then planar point pattern formats
tornclip.sp <- as(tornus_clip, "Spatial")
tornus.ppp <- as(tornus.sp, "ppp")

#Run K, L, and G functions to identify distances at which clustering occurs
torn_k <- envelope(tornus.ppp, Kest, verbose = FALSE, correction="border")
plot(torn_k, main="Midwest Tornado Clustering")
torn_l <- envelope(tornus.ppp, Lest, verbose = FALSE, correction="border")
plot(torn_l, main="Midwest Tornado Clustering")
torn_g <- envelope(tornus.ppp, Gest, verbose = FALSE, correction="border")
plot(torn_g, main="Midwest Tornado Clustering")

#Test K function results for statistical significance
# maximum absolute deviation test
mad.test(tornus.ppp,Kest,verbose=FALSE)
# Diggle Cressie Loosmore Ford test
dclf.test(tornus.ppp,Kest, verbose=FALSE)