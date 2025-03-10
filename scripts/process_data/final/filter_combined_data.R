# Filter combined data before analysis
# Sophie Ruehr
# December 30, 2024


library(pacman)
p_load(here, data.table, plyr,dplyr, tidyr, lubridate, sf)
rm(list = ls())

# Load data
file_path <- here::here("data", "processed_data", "for_analysis",  'merged_data.csv')
data <- fread(file_path)

# Remove any NA values for month, GPP, ET, meteorology
data <- data %>% 
  filter(!is.na(gpp), !is.na(et), !is.na(vpd))
 
# Calculate daily average ET from monthly value (to match SIF-derived average daily GPP)
data <- data %>% 
  mutate(date = make_date(year, month)) %>%
  mutate(days_in_month = days_in_month(date)) %>% 
  mutate(et = et / days_in_month) %>% 
  select(-days_in_month)

# Filter to growing season (top 2 GPP months not in winter) for each pixel year
monthly_gpp <- data[month %in% 3:10,  # Removing winter (Nov-Feb)
                    .SD, 
                    by = .(year, month, x, y)][
                      order(-gpp), head(.SD, 2), by = .(x, y, year)] %>% 
  mutate(gpp_mean = gpp) %>% select(gpp_mean, month, year, x, y)

# Merge back to the original data
data_gs <- merge(data, monthly_gpp, by = c('year', 'month', 'x', 'y')) %>% 
  select(-gpp_mean)

# Filter to periods when cumulative water deficit < 0
data_gs <- data_gs %>% 
  filter(cwd < 0)

# Calculate cwe 
data_gs$cwe <- data_gs$gpp / data_gs$et

# Add crop info
crop_files <- list.files(here::here('data', 'processed_data',"DWR", 'rasters'), pattern = '.csv', full.names = T)
crop_data <- do.call(rbind, lapply(crop_files, read.csv)) %>% select(-X) %>%
  mutate(dwr = value,
         code = crop,
         year = year) %>% select(-value, -crop)
data_gs <- data_gs %>%
  merge(crop_data, by = c('year','dwr'), all = T) %>%
  select(-dwr) 

crop_file <- list.files(here::here('data', 'processed_data', 'DWR'), pattern = 'code_key', full.names = T)
crop_key <- readxl::read_excel(crop_file) %>% filter(variable == 'SUBCLASS') %>% 
  mutate(crop = short_name) %>% dplyr::select(code, crop)

data_gs <- data_gs %>% 
  merge(crop_key, by = 'code', all.x = T) %>% 
  mutate(CLASS2 = substr(code, 1, 1)) %>% 
  select(-code) 

# Remove some bad data from crop files 
data_gs <- data_gs %>% filter(!is.na(month))

# Rename some crops for consistency and brevity
data_gs <- data_gs %>% 
  mutate(crop = ifelse(crop == 'Potatoes', 'Potato', crop)) %>% 
  mutate(crop = ifelse(crop == 'Tomatoes (processing)', 'Tomatoes', crop),
         crop = ifelse(crop == 'Corn/sorghum', 'Corn', crop))

# Input county info
counties <- st_read('/Users/sophieruehr/Documents/Academic/Berkeley/Research/final_chapter/data.nosync/raw/ca_counties/CA_Counties.shp')
ca_counties_wgs84 <- st_transform(counties, crs = 4326)
points_dt <- data_gs %>% dplyr::select(x,y) %>%  unique()
points_sf <- st_as_sf(points_dt, coords = c("x", "y"), crs = st_crs(ca_counties_wgs84))
points_sf <- st_join(points_sf, ca_counties_wgs84[, c("NAME")])
points_dt[, county := points_sf$NAME]
data_gs <- merge(data_gs, points_dt, by = c('x', 'y'), all.x = T)

# Remove outliers based on crop and county
calculate_extremes <- function(data = data_gs, variable, group_var = 'crop') {
  extremes <- data %>%
    group_by(across(all_of(group_var))) %>%
    summarise_at(
      .vars = variable,
      .funs = c("mean", "sd"),
      na.rm = TRUE
    )
  new_names <- c(group_var, paste0(variable, "_mean"), paste0(variable, "_sd"))
  names(extremes) <- new_names
  return(extremes)
}

grouping_vars=c('crop', 'county')
gpp_extreme <- calculate_extremes(variable = 'gpp', group_var = grouping_vars)
et_extreme <- calculate_extremes(variable = 'et', group_var = grouping_vars)
cwe_extreme <- calculate_extremes(variable = 'cwe', group_var = grouping_vars)
vpd_extreme <- calculate_extremes(variable = 'vpd', group_var =grouping_vars)
annual_pr_extreme <- calculate_extremes(variable = 'annual_pr', group_var = grouping_vars)
wtd_extreme <- calculate_extremes(variable = 'wtd', group_var = grouping_vars)
spi_extreme <- calculate_extremes(variable = 'spi', group_var = grouping_vars)
srad_extreme <- calculate_extremes(variable = 'srad', group_var = grouping_vars)

# Merge these values with dataset
data_extremes <- data_gs %>% 
  merge(gpp_extreme, by = grouping_vars) %>% 
  merge(et_extreme, by = grouping_vars) %>% 
  merge(cwe_extreme, by = grouping_vars) %>% 
  merge(vpd_extreme, by = grouping_vars) %>% 
  merge(annual_pr_extreme, by = grouping_vars) %>% 
  merge(wtd_extreme, by = grouping_vars) %>% 
  merge(spi_extreme, by = grouping_vars) %>% 
  merge(srad_extreme, by = grouping_vars) 
  
# Remove observations that are too far from mean value by crop and county
remove_extreme <- function(data, variable, threshold = 3) {
  data %>%
    mutate(
      diff = abs(data[[variable]] - data[[paste0(variable, "_mean")]]),
      limit = threshold * data[[paste0(variable, "_sd")]]
    ) %>%
    filter(diff <= limit | is.na(diff)) %>%
    select(-matches(paste0(variable, "_mean|", variable, "_sd")), -diff, -limit)
}

variables <- c("gpp", "et", "cwe", "vpd", "annual_pr", "wtd", "spi", "srad")

data_for_scaling <- data_extremes %>%
  remove_extreme('gpp', threshold = 3) %>%
  remove_extreme('et', threshold = 3) %>%
  remove_extreme('cwe', threshold = 3) %>%
  remove_extreme('vpd', threshold = 3) %>%
  remove_extreme('annual_pr', threshold = 3) %>%
  remove_extreme('wtd', threshold = 3) %>%
  remove_extreme('spi', threshold = 3) %>%
  remove_extreme('srad', threshold = 3)

# Take z-score normalization for each year, month, and crop 
years = unique(data_for_scaling$year)
months = unique(data_for_scaling$month)
crops = unique(data_for_scaling$crop)
counties = unique(data_for_scaling$county)

outdat <- c()
for (i in 1:length(years)) {
  for (j in 1:length(months)) {
    for (k in 1:length(crops)) {
      for (l in 1:length(counties)) {
        
        dati <- data_for_scaling %>% 
          filter(year == years[i],
                 month == months[j],
                 crop == crops[k],
                 county == counties[l])
        
        if (dim(dati)[1] > 5) { # >5 observations per month, year, crop (exclude n<5 observations)
            # I CHANGED THIS TO 10 in the FINAL!
          scaled_vars <- dati %>% 
            summarise_at(.vars = c('gpp', 'cwe', 'et', 'vpd','srad'),
                         .funs = list('sd' = sd, 'mean' = mean),
                         na.rm = T)
          dati <- cbind(dati,scaled_vars)
          
          dati <- dati %>% 
            mutate(cwe_scaled = (cwe - cwe_mean) / cwe_sd, 
                   gpp_scaled = (gpp - gpp_mean) / gpp_sd,
                   et_scaled = (et - et_mean) / et_sd,
                   vpd_scaled = (vpd - vpd_mean) / vpd_sd, 
                   srad_scaled =  (srad - srad_mean) / srad_sd, 
                   year = as.factor(year),
                   month = as.factor(month))
          outdat <- rbind(outdat, dati) 
        }
      }
    }
  }
}

# Save output
save_path <- here::here("data", "processed_data", "for_analysis",  'filtered_data.csv')
fwrite(outdat, save_path)

