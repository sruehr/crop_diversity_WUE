# Calculate CWD from OpenET and GRIDMET precipitation for each water year
# December 30 2024
# Sophie Ruehr


library(pacman)
p_load(here, data.table, sf, raster, terra, exactextractr, dplyr, plyr, tidyr, parallel, doParallel)
rm(list = ls())

# Open data
et_files <- list.files(here::here("data", "processed_data", "OpenET"), pattern = 'tif', full.names = T)
et <- stack(et_files)
et_dates <- substr(et_files, nchar(et_files)-10, nchar(et_files)-4)
names(et) <- et_dates

pr_files <- list.files(here::here('data', 'raw_data', 'GRIDMET'), pattern = 'pr', full.names = T)
pr <- stack(pr_files)
pr_dates <- substr(pr_files, nchar(pr_files)-10, nchar(pr_files)-4)
names(pr) <- pr_dates

resamp_grid <- et[[1]]

# Define the main function to resample each layer of a raster brick
resample_brick <- function(brick) {
  
  # Get the number of cores for parallel processing
  n_cores <- detectCores() - 1
  cl <- makeCluster(n_cores)
  registerDoParallel(cl)
  
  # Export necessary items to cluster
  clusterExport(cl, list("resamp_grid"))
  
  # Resample each layer of the raster brick in parallel
  resampled_layers <- foreach(i = 1:nlayers(brick), .packages = 'raster') %dopar% {
    layer <- brick[[i]]
    resampled_layer <- resample(x = layer, y = resamp_grid, method = "bilinear")
    return(resampled_layer)
  }
  
  # Stop the cluster
  stopCluster(cl)
  
  resampled_brick <- brick(resampled_layers)
  return(resampled_brick)
}

pr <- resample_brick(pr)
pr <- crop(pr, et)

# Only matching dates
et <- et[[which(names(et) %in% names(pr))]]

# Get water years
date_df <- data.frame(et_names = names(et),
                   pr_names = names(pr)) %>% 
  
  mutate(
    date = as.Date(paste0(substr(et_names, 2, 9), '.01'), format = '%Y.%m.%d')) %>% 
  mutate(
    year = as.numeric(format(date, '%Y')),
    month = as.numeric(format(date, '%m')),
    water_year = year + ifelse(as.numeric(month) >= 10, 1, 0)
  )

water_years <- unique(date_df$water_year)

# Calculate CWD for each month
outdat <- list()
for (j in 1:length(water_years)) {
  date_df_j <- date_df %>% filter(water_year == water_years[j])
  pr_j <- pr[[which(names(pr) %in% date_df_j$pr_names)]]
  et_j <- et[[which(names(et) %in% date_df_j$et_names)]]
  
  # Take difference for monthly CWD
  diff <- pr_j - et_j
  
  # Now take cumsum for each month
  cwd <- calc(diff, fun = function(x) {
    # Calculate cumulative sum for each cell
    cumsum(x)
  })

  names(cwd) <- names(pr_j)
  
  # Save CWD info
  outdat[[j]] <- cwd
  print(water_years[j])
}

# Save
savedat <- brick(unlist(outdat))

output_path <- here::here("data", "processed_data", "for_analysis",  paste0('CWD.grd'))
writeRaster(savedat, output_path, overwrite = T)

# 
# # Convert to DT
# make_df <- function(input, varname, format = '%Y.%m.%d', remove = T) {
#   dt <- as.data.table(as.data.frame(input, xy = TRUE))
#   # Convert to long format
#   dt_long <- melt(dt, id.vars = c('x', 'y'), variable.name = 'date', value.name = varname)
#   # Remove NA values
#   dt_long <- dt_long[!is.na(get(varname))]
#   # Update date format
#   if (format == '%Y.%m.%d') {
#     dt_long$date <- paste0(substr(dt_long$date, 2, 9), '.01')
#   }
#   # Remove input if specified
#   if (remove) {
#     rm(input)
#   }
#   return(dt_long)
# }
# 
# # Merge
# et_df <- make_df(et, 'et')
# pr_df <- make_df(pr, 'pr')
# 
# # Fix floating point errors (off by 0.1 meters)
# et_df = et_df[, `:=`(x = round(x, 6), y = round(y, 6))]
# pr_df = pr_df[, `:=`(x = round(x, 6), y = round(y, 6))]
# both <- merge(et_df, pr_df, by = c('x', 'y', 'date'), all = F)

# Get date
# both$date <- as.Date(both$date, format = '%Y.%m.%d')
# both <- both %>% 
#     mutate(
#       year = as.numeric(format(date, '%Y')),
#       month = as.numeric(format(date, '%m')),
#       water_year = year + ifelse(as.numeric(month) >= 10, 1, 0)
#     )
# 
# # Calculate CWD
# wateryears <- unique(both$water_year)
# outdat <- c()
# for (j in 1:length(wateryears)) {
#   dat <- both %>% filter(water_year == wateryears[j])
#   months <- unique(dat$month)
#   if (length(months) == 12) {
#     dat <- dat %>% mutate(water_deficit = pr - et) %>% # convert PR to mm per day
#       mutate(new_month = (month - 10) %% 12 + 1) %>% 
#       dplyr::arrange(x, y, water_year, new_month) %>%  # Order the data by x, y, sub_crop, year, month
#       dplyr::group_by(x, y, water_year) %>%  # Group by x, y, sub_crop, and year
#       dplyr::mutate(CWD = cumsum(water_deficit)) %>%  # Calculate the cumulative sum of water_deficit
#       ungroup()
#     outdat <- rbind(outdat, dat)
#   }
#   print(wateryears[j])
# }
# 
# dim(outdat)
# outdat_save <- outdat %>% 
#   mutate(month = month.x,
#          year = year.x) %>% 
#   select(c(x, y, month, year, CWD)) %>% 
#   filter(year > 2017)

# Save or use the data.table
# output_path <- here::here("data", "processed_data", "for_analysis",  'CWD.csv')
# fwrite(outdat_save, output_path, append = F)
