# December 30, 2024
# Sophie Ruehr

# DWR shapefile data simplification.
  # For each year,
    # 1. Oepn file
    # 2. Select variables & filter to crops of interest (annuals) and single-use
    # 3. Simplify geometries (tolerance = 70m)
    # 4. Add info on centroids & year
    # 5. Crop to study area (Central Valley)
    # 6. Save

library(pacman)
p_load(here, data.table, sf, raster, terra, exactextractr, dplyr)

folders <- list.files(here("data", "raw_data", "DWR"), full.names= T)

for (i in 1:length(folders)) {
  
  # Open data
  folder <- folders[i]
  shapefiles <- list.files(folder, pattern = "\\.shp$", full.names = TRUE)
  shapefile <- st_read(shapefiles)
  
  # Remove unwanted classes: perennials (vineyards, orchards), rice, urban, etc.
  wanted_classes <- c('P', 'G', 'F', 'T')
  shapefile_filt <- shapefile %>% 
    filter(CLASS2 %in% wanted_classes)
  
  # Filter to single-use fields 
  shapefile_filt <- shapefile_filt %>% 
    filter(MULTIUSE == 'S')
  
  # Select variables of interest
  vars <- c('UniqueID', 'CLASS2', 'SUBCLASS2', 'ACRES', 'COUNTY')
  shapefile_filt <- shapefile_filt %>% select(all_of(vars))
  
  # Simplify geometries
  fixed_shapefile <- st_make_valid(shapefile_filt)
  simplified_shapefile <- st_simplify(fixed_shapefile, dTolerance = 70,  preserveTopology = TRUE)  # Adjust tolerance as needed for simplification
  
  # Calculate centroids, add x, y coordinates, and year
  simplified_shapefile <- simplified_shapefile[st_is_empty(simplified_shapefile$geometry) == FALSE, ]
  simplified_shapefile$centroid <- st_centroid(simplified_shapefile$geometry)
  simplified_shapefile$x <- as.numeric(st_coordinates(simplified_shapefile$centroid)[, 1])
  simplified_shapefile$y <- as.numeric(st_coordinates(simplified_shapefile$centroid)[, 2])
  year <- substr(folder, nchar(folder) - 3, nchar(folder))
  simplified_shapefile$year <- year
  simplified_shapefile <- st_make_valid(simplified_shapefile)
  
  # Crop to Central Valley
  study_area <- here("data", "raw_data", "study_area", "study_area.shp")
  study_area <- st_read(study_area)
  study_area <- st_transform(study_area, crs = st_crs(simplified_shapefile))
  cropped_shapefile <- st_intersection(simplified_shapefile, study_area)
  
  # Save
  output_path <- here("data", "processed_data", "DWR", "simplified_shapefiles",  paste0("DWR_", year, ".shp"))
  st_write(cropped_shapefile, output_path, append=F)
  
  rm(list = setdiff(ls(), "folders"))
}

beepr::beep()
