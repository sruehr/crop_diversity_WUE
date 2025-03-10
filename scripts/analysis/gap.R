# Sophie Ruehr
# Dec 30 2024
# WUE gap analysis

gc()
rm(list = ls())
library(pacman)
p_load(scales, ggplot2, plyr, dplyr, tidyr, ggpubr,ggpattern, RColorBrewer,
       data.table, here, parallel)

# Select variables of interest on which to perform analysis
input_vars <- c('wue_scaled', 'diversity_scaled',
                'vpd_scaled', 'annual_pr', 'wtd',
                'srad_scaled', 'spi', 'clay')

# Open data ---------------------------------------------------------------
data_file <-  here::here("data", "processed_data", "for_analysis",  'filtered_data.csv')
data <- fread(data_file)

# Filter data  ---------------------------------------
# Only crops of interest (as determined in RF model)
crops_interest <- c('Corn', 'Cotton', 'Tomatoes', 'Wheat')
data <- data %>%
  filter(crop %in% crops_interest)

# Take top GPP month only 
data_filt <- data %>% 
  group_by(x,y,year) %>% 
  arrange(desc(gpp)) %>%
  slice(1) 

# Normalize data by crop & county ----------------------------------------------
data_filt <- data_filt %>%
  group_by(crop, county) %>%
  # Normalize diversity such that absolute diff between crops and counties are managed
  mutate(diversity_scaled = scale(diversity)) %>%
  ungroup() 

# Functions to calculate distributions by crop, county, year windows --------------------------
# Inputs
years <- as.numeric(unique(data_filt$year)) # Get windows of average yield over pixel
window_no <- seq(1:5) # number of unique windows

# function over window of size `win`
get_av_null <- function(df = data_filt, win, years, vars = c('wue_scaled')) {

  # Get the number of group combinations based on window size
  yr_groups <- combn(years, win, simplify = TRUE)
  grp_no <- dim(yr_groups)[2]
  observed_gap <- list()
  null_gap <- list()
  
 for (l in 1:grp_no) {
   years_of_interest <- yr_groups[,l]
   
   df_filtered <- df %>% ungroup() %>% 
     filter(year %in% years_of_interest) %>%
     add_count(x, y) %>% filter(n == win) %>% 
     dplyr::select(-n)
   
   # Get mean for each XY pixel over that temporal window by county
   mean_values_window <- df_filtered %>% 
     group_by(county, x, y) %>%
     summarise(across(all_of(vars), mean, .names = "{.col}"), .groups = "drop") %>%
     ungroup() %>% 
     add_count(county)
   
   # Calculate WUE gap (95th percentile - median)
   observed_gap_l <- mean_values_window %>% 
     mutate(n_obs = n) %>% 
     group_by(county, n_obs) %>% 
     summarise(across(all_of(vars),
                      list(q95 = ~quantile(., 0.95, na.rm = TRUE),
                           q50 = ~quantile(., 0.5, na.rm = TRUE)),
                      .names = "{.col}_{fn}"),
               .groups = "drop") %>% 
     mutate(across(ends_with("q95"), 
                   .fns = ~ . - get(sub("_q95$", "_q50", cur_column())), 
                   .names = "{sub('_q95$', '_spread', .col)}"))
   
   # Get mean for each XY pixel over that temporal window by county after shuffling XY info
   null_spread <- df_filtered %>%
     add_count(county, year) %>% 
     filter(n > 1) %>% dplyr::select(-n) %>% 
     group_by(county, year) %>%
     mutate(across(all_of(vars), ~ sample(.))) %>%
     ungroup() %>%
     group_by(county, x, y) %>%
     summarise(across(all_of(vars), mean, .names = "{.col}"), .groups = "drop") %>%
     add_count(county)
   
   null_dist_df_l <- null_spread %>%
     mutate(n_obs = n) %>%
     group_by(county, n_obs) %>%
     summarise(across(all_of(vars), 
                      list(q95 = ~quantile(., 0.95, na.rm = TRUE), 
                           q50 = ~quantile(., 0.5, na.rm = TRUE)), 
                      .names = "{.col}_{fn}"),
               .groups = "drop") %>%
     mutate(across(ends_with("q95"), 
                   .fns = ~ . - get(sub("_q95$", "_q50", cur_column())), 
                   .names = "{sub('_q95$', '_spread', .col)}"))
   
   # Save outputs
   observed_gap_l$win <- win
   observed_gap_l$yrs <- paste(yr_groups[,l], collapse = "_")   
   null_dist_df_l$win <- win
   null_dist_df_l$yrs <- paste(yr_groups[,l], collapse = "_")

   observed_gap <- rbind(observed_gap, observed_gap_l)
   null_gap <- rbind(null_gap, null_dist_df_l)
   
   output <- list(observed_gap, null_gap)
   names(output) <- c('obs', 'null')
 }
  
  return(output)
}

# Loop thru all temporal windows ------------------------------------------
set.seed(123)
observed <- list()
null <- list()

for (i in window_no) {
  average_null_dist <- get_av_null(df = data_filt, win = window_no[i],
                               years = years, vars = input_vars)
  observed[[i]] <- average_null_dist$obs
  null[[i]] <- average_null_dist$null
  print(i)
}


null_df <- bind_rows(null)
obs_df <- bind_rows(observed)

names(obs_df)[3:(length(input_vars)*3+2)] <- paste0(names(obs_df)[3:(length(input_vars)*3+2)], '_o')
names(null_df)[3:(length(input_vars)*3 + 2)] <- paste0(names(null_df)[3:(length(input_vars)*3+2)], '_n')

joined_df <- null_df %>% 
  merge(obs_df, by = c('county', 'win', 'yrs', 'n_obs'))

# Save output -------------------------------------------------------------
data_wd <- here::here("outputs", "gapyield_output")
write.csv(joined_df, file= paste0(data_wd, '/temporal_averaging.csv'))
