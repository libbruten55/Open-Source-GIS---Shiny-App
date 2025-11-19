#################################################################
##           Practical 6 for GEOM184 - Open Source GIS         ##
##                      14/11/2025                             ##
##                  Creating a ShinyApp                        ##
##                        Global.R                             ##
##        code by Diego Panici (d.panici@exeter.ac.uk)         ##
#################################################################


# G1 Load large wood, river, and bridge data ----
lw_points <- st_read("LW.shp")
river <- st_read("RiverTorridge.shp", crs=27700)
bridges <- st_read("TorridgeBridges.shp")
bridges <- bridges[!st_is_empty(bridges$geometry), ]
catchers <- st_read("Catchers.shp")

#Convert vectors to CRS 4326
lw_points <- st_transform(lw_points, crs = 4326)
river <- st_transform(river, crs = 4326)
bridges <- st_transform(bridges, crs = 4326)
bridges_snapped <-st_transform(bridges_snapped, crs=4326)
catchers <- st_transform(catchers, crs=4326)

clusters <- st_read("ClustersD.shp")
clusters <- st_transform(clusters, crs = 4326)


# Dynamically generate colours based on number of unique clusters
num_clusters <- length(unique(clusters$CLUSTER_ID))
pal_clusters <- colorFactor(palette = colorRampPalette(brewer.pal(12, "Paired"))(num_clusters), domain = clusters$CLUSTER_ID)

heatmap <- rast("RiverBuffer.tif")
heatmap <- project(heatmap, crs(river))

pal_heatmap <- colorNumeric(palette = "inferno", domain = na.omit(values(heatmap)), na.color = "transparent")
heatmap <- aggregate(heatmap, fact=3)

aspect <- rast("aspect.tif")
aspect <- project(aspect, crs(river))

pal_aspect <- colorNumeric(palette = "inferno", domain = na.omit(values(aspect)), na.color = "transparent")
aspect<- aggregate(aspect, fact=3)

slope <- rast("slope.tif")
slope <- project(slope, crs(river))

pal_slope <- colorNumeric(palette = "inferno", domain = na.omit(values(slope)), na.color = "transparent")
slope<- aggregate(slope, fact=3)

heatmap <- raster(heatmap)