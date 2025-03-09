# Sophie Ruehr
# Dec 30 2024
# Running random forest to predict CWE; save outputs

gc()
rm(list = ls())
library(pacman)
p_load(plyr, dplyr, stringr, data.table, ggplot2, foreach, boot, 
       tibble, ModelMetrics, here, corrplot)

# To generate all required outputs, this code is looped *** 3 times***
# x3: predicted_variable = 'gpp', 'et', and 'cwe' as target variable

targets <- c('cwe', 'gpp', 'et')

for (predicted_variable in targets) {
  
  # Model set up ------------------------------------------------------------
  # How many observations needed per crop to keep in dataset?
  obs_threshold <- 6000
  # Choose main variable of interest: temporal crop diversity
  ind = 'diversity'
  # Set number of bootstrapped models for importance estimation
  n_boot = 20 # should be 20
  # Set save directory
  savewd <- here::here('outputs', 'model_output')
  
  # Set formula to predict target
  features <- c('month','year',
                'spi',
                'diversity',
                'crop',
                'legume5',
                'vpd_scaled',
                'srad_scaled',
                'wtd',
                'clay',
                'annual_pr'
  )
  
  # Read data
  data_file <-  here::here("data", "processed_data", "for_analysis",  'filtered_data.csv')
  data <- fread(data_file)
  
  # Select predicted variable & set save file name
  if (predicted_variable == 'cwe') {
    data <- data %>%
      mutate(gap = cwe_scaled)
    filename_appendix <- '_CWE'
  } else if (predicted_variable == 'gpp') {
    data <- data %>%
      mutate(gap = gpp_scaled)
    filename_appendix <- '_GPP'
  } else if (predicted_variable == 'et') {
    data <- data %>%
      mutate(gap = et_scaled)
    filename_appendix <- '_ET'
  }
  
  # Create factor variables ------------------- ------------------------------
  data <- data %>% 
    mutate(year = as.factor(year), 
           month = as.factor(month), 
           date = as.Date(date))
  
  # Prep model data ---------------------------------------------------------------
  p_load(scales, ggplot2, dplyr, plyr, tidyr, ggpubr, ggpmisc, ranger, doParallel,
         RColorBrewer, cowplot, data.table, pdp, caret, randomForest, cowplot)
  
  # Prepare data
  set.seed(123)
  
  # Set RF variables
  input_vars <- c('month', 
                  'year',
                  'crop', 
                  'spi',
                  'diversity',
                  'legume5',
                  'vpd_scaled',
                  'srad_scaled',
                  'wtd',
                  'clay',
                  'annual_pr',
                  'x', 'y',
                  'county',
                  'gpp_sd', 'gpp_mean', 'et_sd', 'et_mean', 'cwe_sd', 'cwe_mean', 
                  'gpp', 'et', 'cwe'
  )
  
  factor_vars <-  c('month', 'year', 'CLASS2', 'county')
  
  # Prep input 
  data_model <- data %>% ungroup() %>%  dplyr::select(c('gap', input_vars, 'CLASS2', 'county')) %>% 
    na.omit()
  
  # Remove few observations for each crop
  small_obs_df <- as.data.frame(table(data_model$crop)) %>% filter(Freq >= obs_threshold)
  
  # Remove unwated classes and crops (perennials and highly-irrigated crops)
  data_model <- data_model %>% 
    filter(crop %in% small_obs_df$Var1) %>% 
    filter(!crop == 'Rice') %>% 
    filter(!crop %in% c('Alfalfa')) %>%
    filter(!crop %in% c('Misq. grain/hay', 'Unknown truck', 'Mixed pasture'))
  
  # Display crops
  plot(table(data_model$crop))
  
  # Partition data
  trainIndex <- createDataPartition(data_model$crop, p = 0.7, list = FALSE)
  training <- data_model[trainIndex, ] 
  testing <- data_model[-trainIndex, ] 
  
  # Extra variables to include in output but not in modeling
  extra_vars <- c('x', 'y', 'county', 'gpp_sd', 'gpp_mean', 'et_sd', 'et_mean', 'cwe_sd', 'cwe_mean',
                  'gpp_s', 'et', 'cwe_s', 'cwe')
  
  # Set RF parameters -------------------------------------------------------
  
  # RF parameters
  num.trees <- 200
  mtry <- 6
  min.node.size <- 5
  
  # RF model: FULL model (including all crops) or individual crop (one model per crop)?
  formula <- as.formula(paste("gap ~", paste(features, collapse = " + ")))
  
  # Save RF outputs to a list
  rfmodel <- list()
  testing_out <- c()
  training_out <- c()
  
  # Save bootstrapped Gini distance values to list
  imp_summaries <- c()
  
  # Run RF model & bootstrap feature importance ---------------------------------------------------------------
  # Set up parallel backend
  set.seed(123)
  numCores <- detectCores() - 1
  cl <- makeCluster(numCores)
  registerDoParallel(cl)
  
  # Saving training and testing data
  training_i <- training
  testing_i <- testing 
  
  # Create a class weights vector
  class_freq <- table(training_i$crop)
  class_weights <- 1 / class_freq
  class_weights <- class_weights / sum(class_weights)  # Normalize
  class_weights_vector <- sapply(training_i$crop, function(x) class_weights[as.character(x)])
  
  # Train Random Forest model with class weights
  rf_model <- ranger(
    formula, 
    data = training_i, 
    num.trees = num.trees, 
    mtry = mtry, 
    min.node.size = min.node.size, 
    importance = 'impurity', 
    num.threads = numCores,
    case.weights = class_weights_vector  
  )
  
  # Predict on testing set
  testing_i$pred <- predict(rf_model, data = testing_i)$predictions
  training_i$pred <- predict(rf_model, data = training_i)$predictions
  
  # Save outputs
  testing_out <- rbind(testing_out, testing_i)
  training_out <- rbind(training_out, training_i)
  
  
  # Bootstrap feature importance scores
  bootstrap_importance <- c()
  for (b in 1:n_boot) {
    training_boot <- training_i[sample(1:nrow(training_i), nrow(training_i)*.8, replace = F), ]
    class_freq <- table(training_boot$crop)
    class_weights <- 1 / class_freq
    class_weights <- class_weights / sum(class_weights)  # Normalize
    class_weights_vector <- sapply(training_boot$crop, function(x) class_weights[as.character(x)])
    
    rf_model_boot <- ranger(
      formula, 
      data = training_boot, 
      num.trees = num.trees, 
      mtry = mtry, 
      min.node.size = min.node.size, 
      importance = 'impurity', 
      num.threads = numCores,
      case.weights = class_weights_vector  # Add weights
    )
    
    bootstrap_importance[[b]] <- rf_model_boot$variable.importance
    print(b)
  }
  
  bootstrap_imp_df <- do.call(rbind, lapply(bootstrap_importance, function(x) {
    df <- as.data.frame(x)
    rownames(df) <- NULL
    return(df)
  }))
  bootstrap_imp_df$variable <- names(bootstrap_importance[[1]])  
  
  # Get relative Gini scores
  total_mean_gini <- bootstrap_imp_df %>%
    group_by(variable) %>%
    summarise_at(.vars = 'x',.funs= 'mean') %>% 
    dplyr::select(-variable) %>% 
    summarise_all(.funs = 'sum')
  total_mean_gini <- total_mean_gini$x
  
  # Calculate summary statistics (mean, standard deviation, confidence intervals, etc.)
  imp_summary <- bootstrap_imp_df %>%
    mutate(x = x / total_mean_gini) %>%
    group_by(variable) %>%
    summarise_at(.vars = 'x',.funs = list(
      'mean' = mean,
      'sd' = sd,
      lower_ci = function(x) {quantile(x, 0.025)},
      upper_ci = function(x) {quantile(x, 0.975)},
      'min' = min,
      'max' = max)) %>% 
    filter(!variable == 'crop')
  
  # Stop parallel backend
  stopCluster(cl)
  beepr::beep()
  
  # Write model data --------------------------------------------------------------
  rf_filename <- paste0('RF_model', filename_appendix, '.rds')
  training_filename <- paste0('RF_model_training', filename_appendix, '.csv')
  testing_filename <- paste0('RF_model_testing', filename_appendix, '.csv')
  
  saveRDS(rf_model, file = here::here(savewd, rf_filename))
  fwrite(training_out, file = here::here(savewd, training_filename))
  fwrite(testing_out, file = here::here(savewd, testing_filename))
  
  # Write bootstrapped importance values ------------------------------------
  imp_filename <- paste0('importance', filename_appendix, '.csv')
  fwrite(imp_summary, file = here::here(savewd, imp_filename))
  beepr::beep()

}
