# Calculate mixed pixels at 500m resolution
# Sophie Ruehr
# December 30, 2024


library(pacman)
p_load(here, data.table, sf, raster, terra, exactextractr, dplyr, doParallel, foreach, sf)

# Open data
# 1. DWR shapefiles
dwr_files <- list.files(here('data', "processed_data", 'DWR', "simplified_shapefiles"), pattern = "\\.shp$", full.names = TRUE)

# 2. One SIF file (all same grid)
sif_files <- list.files(here('data', 'processed_data', 'SIF'), pattern = '.tif', full.names = T)
sif_file <- sif_files[10]
sif <- rast(sif_file)

# Crop SIF to study area
study_area <- here::here("data", "raw_data", "study_area", "study_area.shp")
study_area <- st_read(study_area)
study_area <- st_transform(study_area, crs(sif))
study_area_vect <- vect(study_area)  
sif <- crop(sif, study_area_vect)
sif <- mask(sif, study_area_vect)

for (i in 1:length(dwr_files)) {
# foreach(i = 1:length(dwr_files), .packages = c("sf", "dplyr", "stars", "raster", "here", "terra"), .export = c("sif")) %dopar% {
  dwr_file <- dwr_files[i]
  dwr <- st_read(dwr_file)
  
  # Add crop factor variable
  dwr$crop <- paste0(dwr$CLASS2, dwr$SUBCLASS2)
  dwr$crop_factor <- as.numeric(factor(dwr$crop))
  dwr <-  st_transform(dwr, crs = st_crs(sif))
  
  # Now take mode including NA at SIF resolution and mixed pixel threshold = 0.75 (75% unmixed)
  mode_function <- function(x, na.rm = FALSE, threshold = 0.75) {
    if (na.rm) {
      x <- x[!is.na(x)]
    }
    if (length(x) == 0) return(NA) # Handle case where all values are NA
    uniq_vals <- unique(x)
    mode = uniq_vals[which.max(tabulate(match(x, uniq_vals)))]
    perc =  length(which(x == mode)) / length(x)
    if (perc >= threshold) {
      return(mode)
    } else {return(NA)}
  }
  
  # Rasterize DWR
  fact = 10
  dwr_rasterized <- stars::st_rasterize(dwr %>% dplyr::select(crop_factor, geometry),
                                        nx = ncol(sif) * fact, ny = nrow(sif) * fact, fun = mode)
  dwr_rast <- terra::rast(dwr_rasterized)
  dwr_mode <- aggregate(dwr_rast, fact, fun=mode_function)
  
  # Resample to SIF (very close, just off by ~1 m)
  dwr_resamp <- resample(dwr_mode, sif, method = 'near')
  
  # Save output
  year <- substr(dwr_file, nchar(dwr_file)-7, nchar(dwr_file)-4)
  mode_path <- here("data", "processed_data", "DWR", "rasters",  paste0("mode_new", year, ".tif"))
  writeRaster(dwr_resamp, mode_path, overwrite=T)
  
  # Save crop variables
  crop_factors <- as.data.frame(table(dwr$crop, dwr$crop_factor)) %>% filter(Freq > 0) %>% 
    select(-Freq)
  colnames(crop_factors) <- c('crop', 'value')
  crop_factors$year <- year
  csv_path <- here("data", "processed_data", "DWR", "rasters",  paste0("cropfactor_", year, ".csv"))
  write.csv(crop_factors, csv_path)
  
}

