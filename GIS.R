#################################################################
## Practical 5 for GEOM184 - Open Source GIS ##
## 07/11/2025 ##
## Using R for geospatial analysis ##
## code by Libby Bruten (eb987@exeter.ac.uk) ##
#################################################################

# 1.0 Part A - Load packages and libraries ----
install.packages("maps", "sf", "mapproj", "terra", "gridextra", "ggplot2", "dplyr", "rosm")

#loading libraries
library(ggplot2)
library(maps)
library(sf)
library(terra)
library(gridExtra)
library(dplyr)
library(rosm)

# 2.0 Part B - Work with large wood datasets ----
## 2.1 Add input files ----
lw_points <-st_read("LW.shp")
river <-st_read("RiverTorridge.shp", crs = 27700)
river <-st_transform(river, crs=4326)
bridges <-st_read("TorridgeBridges.shp")

plot(lw_points)

## 2.2 Plot points within a map ----
#Get bounding box from river data to define basemap extent
buff <- st_buffer(river, dist=2000) # Create a buffer of 2km around river limits
bbox <- st_bbox(buff) # Extract min/max coordinates
#Download the basemap using ROSM
basemap <- osm.image(x = bbox, type = "osm") 

#Plot with ggplot
plot1 <- ggplot() +
  # Add the basemap
  annotation_raster(basemap, xmin=bbox["xmin"], xmax=bbox["xmax"],
                    ymin=bbox["ymin"], ymax=bbox["ymax"]) +
  # Add river (line)
  geom_sf(data = river, aes(color = "RiverTorridge"), size = 1.2) +
  # Add bridges (points)
  geom_sf(data = bridges, aes(color = "TorridgeBridges"), size = 3, shape = 21, fill = "white") +
  # Add large wood (points)
  geom_sf(data = lw_points, aes(color = "Large_wood"), size = 2, shape = 24, fill = "white") +
  #add a scalebar
  ggspatial::annotation_scale(
    location = "tr", #location on the top right
    bar_cols = c("black", "white")
  ) +
  #add a North arrow
  ggspatial::annotation_north_arrow( 
    location ="br", which_north = "true", #bottom-right and showing true North
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("black", "white"),
      line_col = "grey20"
    )
  ) +
  # Define a manual color scale for legend
  scale_color_manual(name = "Legend", values = c("RiverTorridge" = "blue",
                                                  "TorridgeBridges" = "red",
                                                  "Large_wood" = "black")) +
  # Titles and theme
  labs(title = "Large_wood_on_the_River_Torridge",
       subtitle = "Using_OpenStreetMap_Basemap",
       x = "Longitude", y = "Latitude")+
  theme_minimal()
#Visualise your plot
plot1

##2.3 Create a heatmap ----
plot1+
  stat_density2d(data=lw_points, aes(x=st_coordinates(lw_points)[,1],
                                     y=st_coordinates(lw_points)[,2],
                                     fill=..level..),
                 geom="polygon", alpha=0.6, h= c(1, 1))+
  scale_fill_viridis_c(name="LW_Density")#Using Viridis colour scale 

install.packages("dbscan")
library(dbscan)
## 2.4 Create clusters ----
# Convert sf points to a matrix of coordinates
lw_coords <- st_coordinates(lw_points)
# Apply DBSCAN clustering
clusters <- dbscan(lw_coords, eps = 2000, minPts = 3) # Adjust parameters as needed
# Add cluster IDs back to the sf object
lw_points$cluster <- as.factor(clusters$cluster)
plot1 +
  geom_sf(data = lw_points, aes(color = cluster), size = 2) +
  scale_color_viridis_d(name = "LW_Clusters", option="turbo")

bridges<- st_transform(bridges, crs=3857)
## 2.5 Nearest distance ----
# Find nearest bridge for each LW point
nearest_bridge <- st_nearest_feature(lw_points, bridges)
# Create lines between LW points and nearest bridges
lines_list <- mapply(function(p, b) {
  st_linestring(rbind(st_coordinates(p), st_coordinates(b)))
}, st_geometry(lw_points), st_geometry(bridges[nearest_bridge, ]), SIMPLIFY = FALSE)
# Convert to sf object
lw_to_bridge_lines <- st_sfc(lines_list, crs = st_crs(lw_points)) |> st_sf()
plot1 +
  geom_sf(data = lw_to_bridge_lines, color = "black", linetype = "solid", size = 0.8)

# 3.0 Part C - Topographic analysis ----
## 3.1 Add the DTM ----
DTM <- rast("DTM_Torridge.tif") #load the raster
# Ensure CRS matches the river network
DTM <- project(DTM, crs(river)) #reproject the raster on the same CRS
DTM <- ifel(DTM == 0, NA, DTM) #remove any 0 value
DTM_clip <- crop(DTM, bbox)

aspect <- terrain(DTM_clip, "aspect", unit="degrees")
slope <- terrain(DTM_clip, "slope", unit="degrees")
aspect_df <- as.data.frame(aspect, xy = TRUE)
slope_df <- as.data.frame(slope, xy = TRUE)
plot1 + 
  geom_raster(data = aspect_df, aes(x = x, y = y, fill = aspect)) +
    scale_fill_viridis_c(option="plasma")

plot2 <- ggplot() +
  annotation_raster(basemap, xmin=bbox["xmin"], xmax=bbox["xmax"],
                    ymin=bbox["ymin"], ymax=bbox["ymax"]) +
  geom_raster(data = aspect_df, aes(x = x, y = y, fill = aspect)) +
  scale_fill_viridis_c(option="plasma", name = "Aspect (degrees)") +
  geom_sf(data = river, aes(color = "River Torridge"), size = 1.2) +
  geom_sf(data = bridges, aes(color = "Bridges"), size = 3, shape = 21, fill = "white") +
  geom_sf(data = lw_points, aes(color = "Large wood"), size = 2, shape = 24, fill = "white") +
  scale_color_manual(name = "Legend", values = c("River Torridge" = "blue",
                                                 "Bridges" = "red",
                                                 "Large wood" = "black")) +
  labs(title = "Aspect Map - River Torridge",
       x = "Longitude", y = "Latitude") +
  theme_minimal()

plot2

plot1 + 
  geom_raster(data = slope_df, aes(x = x, y = y, fill = slope)) +
  scale_fill_viridis_c(option="plasma")

plot2 <- ggplot() +
  annotation_raster(basemap, xmin=bbox["xmin"], xmax=bbox["xmax"],
                    ymin=bbox["ymin"], ymax=bbox["ymax"]) +
  geom_raster(data = slope_df, aes(x = x, y = y, fill = slope)) +
  scale_fill_viridis_c(option="plasma", name = "Slope (degrees)") +
  geom_sf(data = river, aes(color = "River Torridge"), size = 1.2) +
  geom_sf(data = bridges, aes(color = "Bridges"), size = 3, shape = 21, fill = "white") +
  geom_sf(data = lw_points, aes(color = "Large wood"), size = 2, shape = 24, fill = "white") +
  scale_color_manual(name = "Legend", values = c("River Torridge" = "blue",
                                                 "Bridges" = "red",
                                                 "Large wood" = "black")) +
  labs(title = "Slope Map - River Torridge",
       x = "Longitude", y = "Latitude") +
  theme_minimal()

plot2

st_crs(bridges)
st_crs(river)

bridges<- st_transform(bridges, crs=3857)
river<- st_transform(river, crs=3857)

bridges <- bridges[!st_is_empty(bridges$geometry), ]
river <- st_union(river)

river<- st_sf(geometry = st_sfc(river), crs =3857)

# Define function to snap bridges to rivers
snap_to_river <- function(points, river_line) {
  nearest <- st_nearest_points(points, river_line)
  pts <- st_cast(nearest, "POINT")
  # Keep only the snapped points (every second point in the returned sequence)
  pts <- pts[seq(2, length(pts), 2)]
  st_sf(st_drop_geometry(points), geometry = pts, crs = st_crs(points))
}
#Then apply to your bridges
bridges_snapped <- snap_to_river(bridges, river)
bridges_snapped$id <- seq_len(nrow(bridges_snapped))


get_upstream_DEMzone <- function(bridge, river, lw_points, slope_rast, aspect_rast,
                                 buffer_length = 2000, buffer_width = 300) {
  # bridge: single-row sf POINT
  # river: river sf (may contain multiple features)
  # lw_points: sf POINT layer of large wood
  # slope_rast, aspect_rast: terra SpatRaster layers
  # buffer_length: distance upstream along the river to include (m)
  # buffer_width: lateral buffer around the upstream reach (m)
  
  # 1. Bridge geometry
  b_geom <- st_geometry(bridge)[[1]]
  
  # 2. Choose the nearest river feature to the bridge
  idx <- which.min(st_distance(river, bridge))
  r_geom <- st_geometry(river[idx, ])[[1]]  # sfg LINESTRING
  
  # 3. Fractional position of the bridge along the river (0 = start, 1 = end)
  pos_frac <- tryCatch({
    st_line_locate_point(r_geom, b_geom) %>% as.numeric()   # lwgeom method
  }, error = function(e) {
    # Fallback: approximate by nearest vertex fraction
    coords <- st_coordinates(r_geom)
    cumdist <- cumsum(c(0, sqrt(diff(coords[,1])^2 + diff(coords[,2])^2)))
    b_coords <- st_coordinates(b_geom)
    nearest_vertex <- which.min(sqrt((coords[,1]-b_coords[1])^2 + (coords[,2]-b_coords[2])^2))
    cumdist[nearest_vertex] / max(cumdist)
  })
  
  # 4. Compute start fraction for upstream segment
  total_len <- as.numeric(st_length(r_geom))
  frac_start <- max(0, pos_frac - buffer_length / total_len)
  
  # 5. Extract upstream segment (try lwgeom's substring, fallback to vertex subset)
  upstream_segment <- tryCatch({
    st_line_substring(r_geom, from = frac_start, to = pos_frac)
  }, error = function(e) {
    coords <- st_coordinates(r_geom)
    cumdist <- cumsum(c(0, sqrt(diff(coords[,1])^2 + diff(coords[,2])^2)))
    keep <- which(cumdist >= frac_start * max(cumdist) &
                    cumdist <= pos_frac * max(cumdist))
    if (length(keep) < 2) keep <- c(max(1, min(keep)-1), min(nrow(coords), max(keep)+1))
    st_sfc(st_linestring(coords[keep, c("X","Y")]), crs = st_crs(river))
  })
  upstream_segment <- st_sf(geometry = st_sfc(upstream_segment, crs = st_crs(river)))
  
  # 6. Buffer to create an analysis zone and count LW inside
  zone <- st_buffer(upstream_segment, dist = buffer_width)
  lw_count <- sum(st_intersects(zone, lw_points, sparse = FALSE))
  
  # 7. Extract slope & aspect values along the upstream segment
  seg_pts <- st_cast(upstream_segment, "POINT")
  if (length(seg_pts) >= 1) {
    coords <- st_coordinates(seg_pts)
    s_vals <- terra::extract(slope_rast, coords[, c("X", "Y")])
    s_vals <- s_vals[, !names(s_vals) %in% "ID", drop = TRUE]
    a_vals <- terra::extract(aspect_rast, coords[, c("X", "Y")])
    a_vals <- a_vals[, !names(a_vals) %in% "ID", drop = TRUE]
    mean_s <- mean(s_vals, na.rm = TRUE)
    mean_a <- mean(a_vals, na.rm = TRUE)
  } else {
    mean_s <- NA_real_; mean_a <- NA_real_
  }
  
  list(zone = zone, lw_count = lw_count, mean_slope = mean_s, mean_aspect = mean_a)
}

#create empty lists that will need filling later in the loop
bridge_zones <- list()
bridges_snapped$LW_upstream <- NA_integer_
bridges_snapped$mean_slope  <- NA_real_
bridges_snapped$mean_aspect <- NA_real_

#now we run the loop
for (i in seq_len(nrow(bridges_snapped))) {
  res <- get_upstream_DEMzone(bridges_snapped[i, ], river, lw_points, slope, aspect,
                              buffer_length = 2000, buffer_width = 300)
  bridges_snapped$LW_upstream[i] <- res$lw_count
  bridges_snapped$mean_slope[i]  <- res$mean_slope
  bridges_snapped$mean_aspect[i] <- res$mean_aspect
  bridge_zones[[i]] <- res$zone
  if (i %% 5 == 0) cat("Processed", i, "of", nrow(bridges_snapped), "bridges\n")
}


upstream_zones <- do.call(rbind, bridge_zones)
upstream_zones <- st_sf(
  bridge_id = bridges_snapped$id,
  lw_count  = bridges_snapped$LW_upstream,
  geometry  = st_geometry(upstream_zones),
  crs       = st_crs(river)
)
  
ggplot() +
  geom_sf(data = river, color = "steelblue") +
  geom_sf(data = upstream_zones, aes(fill = lw_count), alpha = 0.5) +
  geom_sf(data = lw_points, color = "sienna4", size = 1) +
  geom_sf(data = bridges_snapped, color = "red3", size = 3, shape = 17) +
  geom_sf_text(data = bridges_snapped, aes(label = LW_upstream), nudge_y = 100, size = 3) +
  scale_fill_viridis_c(name = "LW Count\nin Zone") +
  theme_minimal()

plot1

# Scatterplot: LW count vs mean aspect
ggplot(bridges_snapped) +
  geom_point(aes(x = mean_aspect, y = LW_upstream), color = "darkgreen", size = 2) +
  geom_smooth(aes(x = mean_aspect, y = LW_upstream),
              method = "lm", color = "red", linetype = "dashed") +
  labs(x = "Mean_Upstream_Aspect", y = "LW Count",
       title = "Impact_of_aspect_upstream_of_a_bridge_on_LW_count",
       caption = paste("Correlation:",
                        round(cor(bridges_snapped$mean_aspect,
                                  bridges_snapped$LW_upstream, use = "complete.obs"), 3))) +
  theme_minimal()


# Scatterplot: LW count vs mean slope
ggplot(bridges_snapped) +
  geom_point(aes(x = mean_slope, y = LW_upstream), color = "darkgreen", size = 2) +
  geom_smooth(aes(x = mean_slope, y = LW_upstream),
              method = "lm", color = "red", linetype = "dashed") +
  labs(x = "Mean_Upstream_Slope", y = "LW Count",
       title = "Impact_of_Slope_upstream_of_a_bridge_on_LW_count",
       caption = paste("Correlation:",
                       round(cor(bridges_snapped$mean_slope,
                                 bridges_snapped$LW_upstream, use = "complete.obs"), 3))) +
  theme_minimal()

#landuse analysis 
install.packages("geodata") #in case it is not installed already
library(geodata)
lc <- landcover(var = "trees")
lc <- project(lc, st_crs(river)$wkt)
aoi <- st_buffer(st_union(river), 5000)
aoi_vect <- vect(aoi)
lc_crop <- crop(lc, aoi_vect)
lc_mask <- mask(lc_crop, aoi_vect)


zones_vect <- vect(upstream_zones) #turn it into a vector
tree_vals <- extract(lc_crop, zones_vect)
tree_summary <- tree_vals %>%
  group_by(ID) %>%
  summarise(
    total_cells = length(ID),
    tree_cells = sum(trees, na.rm=TRUE),
    tree_ratio = tree_cells / total_cells
  )

writeRaster(aspect, "aspect.tif", overwrite = TRUE)
writeRaster(slope, "slope.tif", overwrite = TRUE)
