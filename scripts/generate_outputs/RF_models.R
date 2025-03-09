# Sophie Ruehr
# Dec 30 2024
# Generate figures and tables for random forest output

rm(list = ls())
gc()
library(pacman)
p_load(ggplot2, data.table, here, plyr, dplyr, corrplot,ModelMetrics, tidyr,
       foreach, boot, parallel, doParallel, tibble, pdp, ranger, cowplot,
       sf,  rnaturalearth, rnaturalearthdata,raster, patchwork)

# To generate all required outputs, this code should be ***run 3 times***
# x3: predicted_variable = 'gpp', 'et', and 'cwe' as target variable
  # Or you could write a loop but I was lazy ;)

# Main choices --------------------------------------------------------------
# Set variable of interest
predicted_variable <- 'cwe' # choose from 'cwe', 'et', or 'gpp'

# Set additional parameters -----------------------------------------------------------

# For figures
fig_wd <- here::here("outputs", "figures")
colors <- c("#7f3b08", "#e08214", "#fdb863", "#948c7c", "#7c7e99", "#b2abd2", "#8073ac", "#542788", "#2d004b")

# Set general text size
textsize = 8.5

# Select quantiles for PDP plots
quantiles <- c(0.25, 0.75)

# Set filename & figure labels bassed on variable
if (predicted_variable == 'cwe') {
  filename_appendix <- '_CWE'
  axis_lab <- 'WUE'
} else if (predicted_variable == 'gpp') {
  filename_appendix <- '_GPP'
  axis_lab <- 'GPP'
} else if (predicted_variable == 'et') {
  filename_appendix <- '_ET'
  axis_lab <- 'ET'
}

# Read in data outputs from RF model --------------------------------------
model_filename <- here::here("outputs", "model_output", paste0('RF_model', filename_appendix, '.rds'))
training_filename <- here::here("outputs", "model_output", paste0('RF_model_training', filename_appendix, '.csv'))
testing_filename <- here::here("outputs", "model_output", paste0('RF_model_testing', filename_appendix, '.csv'))

testing <- fread(testing_filename)
training <- fread(training_filename)
model <- readRDS(model_filename)

crops <- unique(testing$crop) 

# Regression plots --------------------------------------------------------
lims <- c(-3.5, 3.5)
ymark <- 2
xmark <- -2
regression_plots <- list()
text_size <- 2.7

# All crop regression
regression_summary_full <- testing %>%
  do(data.frame(
    rmse = round(rmse(testing$gap, testing$pred), 2),
    r_squared = summary(lm(gap ~ pred, data = .))$r.squared,
    equation = paste0("y=", round(coef(lm(gap ~ pred, data = .))[2], 1), 
                      "x+", round(coef(lm(gap ~ pred, data = .))[1], 2)),
    n = paste0('n=',dim(training)[1]))) %>% 
  mutate(crop = 'full')

alpha <- 0.4
size = 0.4
regression_plot <- testing %>% 
  filter(gap < 7.5, pred < 7.5) %>%
  ggplot(aes(x = pred, y = gap)) +
  geom_point(size = size, alpha = alpha,aes(col = crop)) +
  geom_abline(linetype = 'dashed', linewidth = 0.3, col = 'black') +
  geom_smooth(method = 'lm', col = 'black',# fill = colors[2], 
              formula = y ~ x, linewidth = 0.6) +
  labs(x = paste0("Predicted ", axis_lab, "'"), y =  paste0("Observed ", axis_lab, "'")) +
  geom_text(data = regression_summary_full,
            aes(label = sprintf("R²=%.2f\nRMSE=%.2f\n%s\n%s",  r_squared, rmse, equation, n)),
            x = xmark, y = ymark, size = text_size, inherit.aes = FALSE) +
  theme_bw() +
  xlim(lims) + ylim(lims)  +
  theme(legend.position = "inside",
        legend.position.inside = c(0.83, 0.15),
        legend.spacing = unit(0.01, 'cm'),
        legend.title = element_blank(),
        legend.key.size =  unit(0.01, "cm"),
        legend.text = element_text(size = 6),
        plot.margin = unit(c(0.5, 0.01, 0.01, 0.01), "cm"),
        axis.text = element_text(size = textsize, color = 'black'), 
        legend.background = element_rect(colour = "black", linewidth = 0.2)) +
  scale_color_manual(values = c(colors[1], colors[3], colors[5], colors[7], colors[9]),
                     guide = guide_legend(override.aes = list(size = 3))) +
  guides( color = guide_legend(override.aes = list(size = 1, alpha = 1)))
  
# For individual crops
reg_plots <- list()
regression_summaries <- list()
alpha <- 0.4
size = 0.7
  
for (i in 1:length(crops)) {
  color_crops <-  c(colors[1], colors[3], colors[5], colors[7], colors[9])
  
  testing_i <- testing %>% filter(crop == crops[i])
  training_i <- training %>% filter(crop == crops[i])
  
  regression_summary_i <- testing_i %>%
    do(data.frame(
      rmse = round(rmse(testing_i$gap, testing_i$pred), 2),
      r_squared = summary(lm(gap ~ pred, data = .))$r.squared,
      equation = paste0("y=", round(coef(lm(gap ~ pred, data = .))[2], 1), 
                        "x+", round(coef(lm(gap ~ pred, data = .))[1], 2)),
      n = paste0('n=',dim(training_i)[1])))
  
  regression_plots[[i]] <- testing_i %>% 
    filter(gap < 7.5, pred < 7.5) %>%
    ggplot(aes(x = pred, y = gap)) +
    geom_point(size = size, alpha = alpha, col = color_crops[i]) +
    geom_abline(linetype = 'dashed', linewidth = 0.3, col = 'black') +
    geom_smooth(method = 'lm', col = 'black', formula = y ~ x, linewidth = 0.6) +
    labs(x =  paste0("Predicted ", axis_lab, "'"), y =  paste0("Observed ", axis_lab, "'")) +
    geom_text(data = regression_summary_i,
              aes(label = sprintf("R²=%.2f\nRMSE=%.2f\n%s\n%s",  r_squared, rmse, equation, n)),
              x = xmark, y = ymark, size = text_size, inherit.aes = FALSE) +
    theme_bw() +
    xlim(lims) + ylim(lims)   +
    ggtitle(crops[i]) +
    theme(plot.title = element_text(size = 14, hjust = 0.5)) +
    theme(  plot.margin = unit(c(0.01, 0.01, 0.01, 0.01), "cm"),
            axis.text = element_text(size = textsize, color = 'black'))
  
  names(regression_plots)[i] <- crops[i]
  regression_summaries[[i]] <- regression_summary_i %>% mutate(crop = crops[i])
}


# PDP plots ---------------------------------------------------------------
model_formula <- model$variable.importance # should be same for all models
factor_vars <- c('legume1', 'month', 'year', 'crop')
continuous_vars <- names(model_formula)[!names(model_formula) %in% factor_vars]

# Define sample size, grid res, and n_boot for PDP plots
sample_size <- 2 # should be 10
n_boot <- 2 # should be 20
grid_res = 10 # should be 20

# Set up parallel backend
set.seed(123)
numCores <- detectCores() - 1
cl <- makeCluster(numCores)
registerDoParallel(cl)

# To save info
pdp_info <- list()
pdp_full <- list()
pdp_div <- list()
pdp_data <- c()

# Run over full model and by individual crop
pdp_runs <- c('full', crops)

for (i in 1:length(pdp_runs)) {
  if (pdp_runs[i] == 'full') {
    trainingdf <- training
  } else {
    trainingdf <- training %>% filter(crop == pdp_runs[i])
  }
  
    # Compute PDPs for each continuous variable
    pdp_list <- foreach(indicator = continuous_vars, 
                        .export = c('continuous_vars'),
                        .packages = c("dplyr", "pdp", "ranger", 'tidyr')) %dopar% {
                          
                          # Filter data for the current indicator
                          indicator_column <- trainingdf[[indicator]]
                          bottom <- quantile(indicator_column, 0.01)
                          top <- quantile(indicator_column, 0.99)
                          
                          filtered_training_i <- trainingdf[indicator_column >= bottom & indicator_column <= top, ]
                          
                          # Bootstrapping PDPs
                          pdp_bootstrap <- replicate(sample_size, {
                            # Sample data and convert to wide format
                            sample_data <- filtered_training_i %>%
                              sample_frac(0.7, replace = FALSE)
                            
                            # Generate partial dependence plot data
                            partial(model, pred.var = indicator, plot = FALSE,
                                    train = sample_data, grid.resolution = grid_res) %>%
                              mutate(var = indicator)
                          }, simplify = FALSE)
                          
                          # Combine bootstrap results
                          bind_rows(pdp_bootstrap)
                        }
    pdp_data[[i]] <- pdp_list
    names(pdp_data)[i] <- pdp_runs[i]
    print(pdp_runs[i])
}

for (i in 1:length(pdp_runs)) {
  pdp_list <- pdp_data[[i]]
  
  # Combine PDP data into a single dataframe
  pdp_sub_df <- bind_rows(pdp_list) %>% 
    dplyr::select(-var) %>% 
    pivot_longer(!c(yhat)) %>% na.omit() %>% 
    mutate(name = ifelse(name == 'annual_pr', 'Annual precip', name)) %>% 
    mutate(name = ifelse(name == 'vpd_scaled', 'VPD', name)) %>% 
    mutate(name = ifelse(name == 'srad_scaled', 'SRAD', name)) %>% 
    mutate(name = ifelse(name == 'clay', 'Clay', name)) %>% 
    mutate(name = ifelse(name == 'diversity', 'Diversity', name)) %>% 
    mutate(name = ifelse(name == 'spi', 'SPI', name)) %>% 
    mutate(name = ifelse(name == 'wtd', 'WTD', name)) %>% 
    mutate(name = ifelse(name == 'year', 'Year', name)) %>% 
    mutate(name = ifelse(name == 'legume1', 'Legume', name)) %>% 
    mutate(name = ifelse(name == 'legume5', 'Legumes', name)) %>% 
    mutate(name = ifelse(name == 'month', 'Month', name)) 
  
  pdp_sub_df$crop <- pdp_runs[i]
  
  # # Summarize the bootstrapped PDPs
  pdp_summary <- pdp_sub_df %>%
    mutate(value = round(value,1)) %>% 
    group_by(name, value) %>% 
    summarise_at(.vars= 'yhat',
                 .funs =list(
                   'low' = min,
                   'high' =max,
                   'med' = median)) %>% 
    mutate(low = ifelse(low > med, NA, low),
           high = ifelse(high < med, NA, high))
  
  # Get bounds on effect high to low
  vars <- bind_rows(pdp_list) %>% dplyr::select(var) %>% unique()
  quant <- pdp_sub_df %>% 
    group_by(name) %>% 
    summarise_at(.vars = 'value',
                 .funs = list('low' = function(x) {quantile(x, quantiles[1])},
                              'high' = function(x) {quantile(x,quantiles[2])}))
  
  trainingdf <- if (pdp_runs[i] == 'full') {training} else {training %>% filter(crop == pdp_runs[i])}
  
  ticks <- trainingdf[, names(trainingdf) %in% vars$var, with = FALSE] %>%
    filter(srad_scaled > -3 & srad_scaled < 3,
           vpd_scaled < 3 & vpd_scaled > -3,
           clay < 50,
           wtd > -35,
           annual_pr < 550) 
  ticks <- ticks %>%   
    melt(measure.vars = names(ticks),  # Columns to pivot
         variable.name = "variable",   # New column for variable names
         value.name = "value")  %>%
    mutate(name=as.character(variable)) %>% 
    mutate(name = ifelse(name == 'annual_pr', 'Annual precip', name)) %>% 
    mutate(name = ifelse(name == 'vpd_scaled', 'VPD', name)) %>% 
    mutate(name = ifelse(name == 'srad_scaled', 'SRAD', name)) %>% 
    mutate(name = ifelse(name == 'clay', 'Clay', name)) %>% 
    mutate(name = ifelse(name == 'diversity', 'Diversity', name)) %>% 
    mutate(name = ifelse(name == 'spi', 'SPI', name)) %>% 
    mutate(name = ifelse(name == 'wtd', 'WTD', name)) %>% 
    mutate(name = ifelse(name == 'year', 'Year', name)) %>% 
    mutate(name = ifelse(name == 'legume1', 'Legume', name)) %>% 
    mutate(name = ifelse(name == 'legume5', 'Legumes', name)) %>% 
    mutate(name = ifelse(name == 'month', 'Month', name)) 
  
  long_ticks <- ticks %>%
    left_join(quant, by = "name") %>%
    # filter(value >= low & value <= high) %>%
    dplyr::select(-low, -high) 
  
  # Plot
  alph = 0.2
  alpha2 = 0.007
  pdp_full[[i]] <- pdp_sub_df %>% 
    filter(value > -0.5) %>%
    ggplot(aes(x = value, y = value)) +
    facet_wrap(~name, scales = 'free_x', nrow = 2) +
    geom_hline(yintercept = 0, col = 'black', linewidth = 0.4) +
    geom_smooth(data = pdp_summary, aes(x = value, y = low), col = 'black', linewidth = .8) +
    geom_vline(data = quant, aes(xintercept = low), col = colors[2], linetype = 'dashed', linewidth = 0.3) +
    geom_vline(data = quant, aes(xintercept = high), col = colors[2], linetype = 'dashed', linewidth = 0.3) +
    ylab( paste0(axis_lab, "'")) +
    xlab('Feature') +
    geom_rug(data = long_ticks %>% sample_frac(0.3), aes(x = value, y = -Inf), sides = "b",
             linewidth = 0.05, alpha = 0.03) +
    theme_bw() +
    theme(
      strip.background = element_blank(), 
      strip.text = element_blank(),
      axis.text=element_text(size = 11),
      axis.title = element_text(size = 12)
    )  +
    geom_text(
      data = pdp_summary %>% group_by(name) %>% summarise_at(.vars = 'value', .funs = mean),
      aes(x = (value), y = .4, label = name),  # Replace with your annotation text
      size = 4,  # Adjust size as needed
      fontface = "bold"  # Change font style if needed
    ) +
    theme(plot.title = element_text(hjust = 0.5))
  
  pdp_div[[i]] <-  pdp_sub_df %>% 
    filter(name == 'Diversity') %>% 
    filter(value > -0.5) %>% 
    ggplot(aes(x = value, y = value)) +
    facet_wrap(~name, scales = 'free_x', nrow = 2) +
    geom_hline(yintercept = 0, col = 'black', linewidth = 0.4) +
    geom_smooth(data = pdp_summary %>% filter(name == 'Diversity'), aes(x = value, y = low), col = 'black', linewidth = .8) +
    geom_vline(data = quant %>% filter(name == 'Diversity'), aes(xintercept = low), col = colors[2], linetype = 'dashed', linewidth = 0.3) +
    geom_vline(data = quant %>% filter(name == 'Diversity'), aes(xintercept = high), col = colors[2], linetype = 'dashed', linewidth = 0.3) +
    ylab( paste0(axis_lab, "'")) +
    xlab('Diversity') +
    geom_rug(data = long_ticks %>% sample_frac(0.3)  %>% filter(name == 'Diversity'), aes(x = value, y = -Inf), sides = "b", 
             linewidth = 0.05, alpha = 0.03, length = unit(0.05, "npc")) +
    theme_bw() +
    theme(
      strip.background = element_blank(), 
      strip.text = element_blank(),
      axis.title = element_text(size = 12),
      plot.margin = unit(c(0.5, 0.01, 0.01, 0.01), "cm"),
      axis.text = element_text(size = textsize, color = 'black')
    ) 
  
  pdp_info[[i]] <- pdp_sub_df
  names(pdp_div)[i] <- pdp_runs[i]
  names(pdp_full)[i] <- pdp_runs[i]
  print(pdp_runs[i])
 
}


# Absolute change plots ---------------------------------------------------
calculate_absolute_change_ci_vector <- function(old_values = bottom_cwe, new_values = top_cwe, alpha = 0.05) {
  
  # Calculate mean of old and new values
  mean_old <- mean(old_values, na.rm = T)
  mean_new <- mean(new_values, na.rm = T)
  
  # Calculate overall absolute change
  absolute_change <- mean_new - mean_old
  
  # Calculate standard deviations
  sd_old <- sd(old_values, na.rm = T)
  sd_new <- sd(new_values, na.rm = T)
  
  # Calculate standard error of the mean difference
  se <- sqrt((sd_old^2 / length(old_values)) + (sd_new^2 / length(new_values)))
  
  # Calculate degrees of freedom
  n <- length(old_values)  # Number of observations
  df <- n - 1
  
  # Calculate critical t-value
  t_critical <- qt(1 - alpha / 2, df)
  
  # Calculate confidence interval for the absolute change
  lower_ci <- absolute_change - (t_critical * se)
  upper_ci <- absolute_change + (t_critical * se)
  
  # Return results as a data frame
  return(data.frame(
    change = absolute_change,
    lower_ci = lower_ci,
    upper_ci = upper_ci
  ))
}

# For saving
change_plts <-list()
change_data <- c()

for (i in 1:length(pdp_runs)) {
  # Get high and low quantiles for each variable 
  pdp_sub_df_crop <- pdp_info[[i]] %>% filter(crop == pdp_runs[i])
  quant <- pdp_sub_df_crop %>%
    group_by(name) %>%
    summarise(low = quantile(value, quantiles[1]),
              high = quantile(value, quantiles[2]))
  quants <- pdp_sub_df_crop %>%
    merge(quant, by = 'name', all = T) %>%
    filter(value <= low | value >= high) %>%
    mutate(bin = ifelse(value <= low, 'low', 'high')) %>%
    group_by(name, bin)
  abs_change <- c()
  for (j in 1:length(unique(quants$name))) {
    change <- quants %>% 
      filter(name == unique(quants$name)[j])
    out <- calculate_absolute_change_ci_vector(old_values = change$yhat[change$bin == 'low'],
                                               new_values = change$yhat[change$bin == 'high'])
    out$name = unique(quants$name)[j]
    out <- as.data.frame(out)
    abs_change <- rbind(abs_change, out)
  }
  
  # Plot
  abs_change$color <- 'black'
  abs_change$color[abs_change$name == 'Diversity'] <- colors[2]
  abs_change <- abs_change %>% mutate(shape = ifelse(name == ('Diversity'), 'x', 'o'))
  
  abs_changeplot <- abs_change %>% 
    ggplot(aes(y=reorder(name, change))) +
    geom_vline(xintercept = 0, col = 'black', linewidth = 0.3, linetype = 'dashed') +
    geom_point(aes(x = change * 100, shape = shape), size = 1.4, color = abs_change$color) +
    geom_errorbarh(aes(xmin = lower_ci * 100, xmax = upper_ci * 100),
                   linewidth = 0.4, height = 0, col= abs_change$color)+
    labs(x = bquote(Delta * .(axis_lab) * "'%" ), y = '') +  theme_bw() +
    theme(plot.margin = unit(c(0.5, 0.01, 0.01, 0.01), "cm"),
          axis.text = element_text(size = textsize, color = 'black'),
          axis.title = element_text(size = 12),
          legend.position = 'none')  
  
  # Save output
  change_plts[[i]] <- abs_changeplot
  names(change_plts)[i] <- pdp_runs[i]
  abs_change$crop <- pdp_runs[i]
  change_data <- rbind(change_data, abs_change)
}

change_plts

# Importance plots --------------------------------------------------------
# Open data
imp_filename <- here::here("outputs", "model_output", paste0('importance', filename_appendix, '.csv'))

imp_summaries <- fread(imp_filename)

# Plot the bootstrapped importance scores with error bars
ind <- 'diversity'
imp_summary <- imp_summaries %>%
  mutate(color = ifelse(variable == (ind), colors[2], 'black'),  # Assign color based on condition
         shape = ifelse(variable == (ind), 'x', 'o'))  # Assign shape based on condition

# Make plot
importance_plot <- imp_summary %>% 
  mutate(variable = ifelse(variable == 'annual_pr', 'Annual precip', variable)) %>% 
  mutate(variable = ifelse(variable == 'vpd_scaled', 'VPD', variable)) %>% 
  mutate(variable = ifelse(variable == 'srad_scaled', 'SRAD', variable)) %>% 
  mutate(variable = ifelse(variable == 'clay', 'Clay', variable)) %>% 
  mutate(variable = ifelse(variable == 'diversity', 'Diversity', variable)) %>% 
  mutate(variable = ifelse(variable == 'spi', 'SPI', variable)) %>% 
  mutate(variable = ifelse(variable == 'wtd', 'WTD', variable)) %>% 
  mutate(variable = ifelse(variable == 'year', 'Year', variable)) %>% 
  mutate(variable = ifelse(variable == 'legume1', 'Legume', variable)) %>% 
  mutate(variable = ifelse(variable == 'legume5', 'Legumes', variable)) %>% 
  mutate(variable = ifelse(variable == 'month', 'Month', variable)) %>% 
  ggplot(aes(x = mean * 100, y = reorder(variable, mean))) +
  geom_errorbarh(aes(xmin = min * 100, xmax = max* 100),  
                 color = imp_summary$color, height = 0, linewidth = 0.4) +  # Error bars
  geom_point(aes(shape = shape), color = imp_summary$color, size = 1.4) +  # Use fixed color and shape from dataframe
  labs(x = 'Relative MDG (%)', y = '') +  # Labels
  theme_bw() +  # Theme
  theme(legend.position = 'none',
        plot.margin = unit(c(0.5, 0.01, 0.01, 0.01), "cm"),
        axis.text = element_text(size = textsize, color = 'black')) 


# Correlation plot  --------------------------------------------------------
cor_matrix <- training %>%
  mutate(CWE = gap, 
         Diversity = diversity,
         Legumes = legume5,
         VPD = vpd_scaled,
         SRAD = srad_scaled,
         SPI = spi,
         'Annual precip' = annual_pr,
         Clay = clay,
         WTD = wtd,) %>% 
  dplyr::select(c(CWE, Diversity, 
                  Legumes,
                  VPD, SRAD, SPI, 
                  'Annual precip',
                  Clay, WTD)) %>%
  dplyr::select(where(is.numeric)) %>%
  sample_frac(0.1) %>% cor(use = 'complete.obs') 

# Print tables ------------------------------------------------------------
table_save_wd <- fig_wd <- here::here("outputs", "tables")

# 1. Regression / model performance summaries
regression_table <- bind_rows(regression_summary_full, bind_rows(regression_summaries)) %>% 
  mutate(r_squared = round(r_squared, 3))
write.csv(regression_table, paste0(table_save_wd, '/model_performance', filename_appendix, '.csv'))

# 2. RF feature importance
importance_table <- imp_summaries %>% dplyr::select(variable, mean, lower_ci, upper_ci) 
write.csv(importance_table, paste0(table_save_wd, '/variable_importance', filename_appendix, '.csv'))

# 3. Percentage change in CWE from low to high quantiles
change_table <- change_data %>% 
  mutate(sd = change - lower_ci) %>% 
  dplyr::select(-shape, -color) %>% 
  mutate(change = round(change * 100, 2),
         lower_ci = round(lower_ci * 100, 2),
         upper_ci = round(upper_ci * 100, 2),
         sd = round(sd * 100, 2))
write.csv(change_table, paste0(table_save_wd, '/percent_change', filename_appendix, '.csv'))


# Save main text figures --------------------------------------------------------------
fig_wd <- here::here("outputs", "figures")

# Save RF output and performance
textsize = 8
reg <- regression_plot + theme(plot.margin = unit(c(0.01, 0.01, 0.01, 0.01), "cm"), plot.title = element_blank(),
                                     axis.text = element_text(size = textsize, color = 'black'))+
  theme(legend.text = element_text(size = 7),
        legend.position =  'inside', 
        legend.position.inside = c(0.8, 0.15),
        legend.background = element_rect(fill = NA, color = NA))
change <- change_plts$full + theme(plot.margin = unit(c(0.01, 0.01, 0.01, 0.01), "cm"),
                                   plot.title = element_blank(),
                                   axis.title.x = element_text(size = textsize + 3),
                                   axis.text = element_text(size = textsize, color = 'black'))
imp <- importance_plot + theme(plot.margin = unit(c(0.01, 0.01, 0.01, 0.01), "cm"),
                                     plot.title = element_blank(),
                                     axis.text = element_text(size = textsize, color = 'black'))
pdp <- pdp_div$full +  theme(plot.margin = unit(c(0.01, 0.01, 0.01, 0.01), "cm"), plot.title = element_blank(),
                             axis.text = element_text(size = textsize, color = 'black'))
full_plot <- cowplot::plot_grid(reg, imp, pdp, change, axis = 'lr', align = 'vh',
                                labels = 'auto', label_x = 0.32,
                                label_y = 0.99, label_size = 12)
setwd(fig_wd)
ggsave(paste0(fig_wd, '/RF', filename_appendix, '.pdf'), full_plot,
       height = 5, width = 6)

# Save full PDP plot
ggsave(paste0(fig_wd, '/PDP', filename_appendix, '.pdf'), pdp_full$full,
       height = 5, width = 6)

# Save supplemental figures -----------------------------------------------
# align all crop RF plots
fig_wd <- here::here("outputs", "figures", "supplement")

sz = 5
allplots <- plot_grid(plotlist = c(regression_plots, 
                                   change_plts[names(change_plts) != "full"], 
                                   pdp_div[names(pdp_div) != "full"]),
                      ncol = 4,
                      align = 'v',
                      labels = 'auto',
                      label_size = 12,
                      label_x = c(0.92),
                      label_y = c(rep(0.3, 6), c(rep(0.35, 6))),
                      axis = 'lr',
                      rel_heights = c(0.9,0.9,0.8, 0.8))

# Save RF outputs
ggsave(paste0(fig_wd, '/RF_by_crop', filename_appendix, '.pdf'), allplots,  height = 7, width = 11)

# Save PDP plots
for (i in pdp_runs[pdp_runs != "full"]) {
  ggsave(paste0(fig_wd, '/PDP_', i, filename_appendix, '.pdf'), pdp_full[[i]],
         height = 5, width = 6)
}

# Save correlation plot
corr_plot_filename = paste0(fig_wd, '/corrplot', filename_appendix, '.pdf')
pdf(corr_plot_filename, width = 7, height = 7)
corrplot(cor_matrix, method = "circle", diag = FALSE,
         outline = TRUE, type = 'lower', is.corr = TRUE,
         tl.col = "black",    
         cl.cex = 1,         
         cl.length = 5)        
dev.off()
