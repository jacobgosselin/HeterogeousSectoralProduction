
################
# gen_delta
################

gen_delta <- function(list, year, horizon, log_change = FALSE) {
  
  # Convert year to numeric in case it's provided as a string
  year <- as.numeric(year)
  
  # Identify the names of the current year and horizon years ago
  current_year_name <- as.character(year)
  h_years_ago_name <- as.character(year - horizon)
  
  # Extract the dataframes for the current year and horizon years ago
  current_year_df <- list[[current_year_name]]
  h_years_ago_df <- list[[h_years_ago_name]]
  
  # Check if both dataframes exist
  if (is.null(current_year_df) || is.null(h_years_ago_df)) {
    stop("One or both of the required dataframes are missing.")
  }
  
  # Subset both dataframes to include only columns from the 3rd column onwards
  current_year_df_subset <- current_year_df[, 3:(ncol(current_year_df)-1)] # last column is total industry output/spending
  h_years_ago_df_subset <- h_years_ago_df[, 3:(ncol(h_years_ago_df)-1)] # last column is total industry output/spending
  
  # Calculate the change
  if (log_change) {
    change_df <- log(current_year_df_subset) - log(h_years_ago_df_subset)
    # Note that this will produce NAs, given that some values are 0; that's ok, we exclude them in analysis.
  } 
  else {
    change_df <- current_year_df_subset - h_years_ago_df_subset
  }
  
  # Add the first two columns (industry codes and descriptions) back to the dataframe, along with last column (total output)
  change_df <- cbind(current_year_df[, 1:2], change_df, current_year_df[, ncol(current_year_df)])
  colnames(change_df) <- colnames(current_year_df)
  
  # Return the resulting dataframe
  return(change_df)
}


#################################
# iXj --> ij, for A matrices
#################################
make_mat_long <- function(A){
  # reshape A matrices to long format
  A_long <- reshape2::melt(A, id.vars = c("Code", "Industry Description"))
  colnames(A_long) <- c("Code", "Industry Description", "j", "val")
  # subset out T008 (total industry output)
  A_long <- subset(A_long, j != "T008")
  return(A_long)
}

make_mat_list_long <- function(A_list){
  og_names <- names(A_list)
  A_long_list <- lapply(A_list, make_mat_long)
  names(A_long_list) <- og_names
  A_long_list <- lapply(names(A_long_list), function(year) {
    df <- A_long_list[[year]]
    df$year <- year  # Add a new column 'year' with the name of the list item
    return(df)
  })
  A_long <- do.call(rbind, A_long_list)
  return(A_long)
}

###################
# For each delta dataframe, sum the absolute changes
###################

sum_abs_changes <- function(df) {
  
  value_mat = df[, 3:(ncol(df)-1)]
  # set all inf and NaN values in dataframe to NA
  value_mat <- lapply(value_mat, function(x) {
    # Replace Inf and NaN with NA in each column
    x[is.nan(x) | is.infinite(x)] <- NA
    return(x)
  })
  # Convert list back to data frame
  value_mat <- as.data.frame(value_mat)
  
  # Calculate the sum of the absolute changes for each row
  sum_abs <- apply(value_mat, 1, function(row) {
    sum(abs(row), na.rm = TRUE)
  })
  
  result = cbind(df[,1:2], data.frame(sum_abs))
  colnames(result) <- c("Code", "Industry Description", "sum_abs")
  return(result)
}

#####################
# For each delta dataframe, extract max and min row by row
#####################

calculate_extremes <- function(df) {
  
  value_mat = df[, 3:(ncol(df)-1)]
  # set all inf and NaN values in dataframe to NA
  value_mat <- lapply(value_mat, function(x) {
    # Replace Inf and NaN with NA in each column
    x[is.nan(x) | is.infinite(x)] <- NA
    return(x)
  })
  # Convert list back to data frame
  value_mat <- as.data.frame(value_mat)
  
  # Calculate the min and max for each row
  extremes <- apply(value_mat, 1, function(row) {
    min_value <- min(row, na.rm = TRUE)
    max_value <- max(row, na.rm = TRUE)
    min_index <- which.min(row)
    max_index <- which.max(row)
    
    # Calculate absolute values
    abs_min <- abs(min_value)
    abs_max <- abs(max_value)
    
    # Determine the largest absolute change
    if (abs_min > abs_max) {
      largest_change <- min_value
      largest_change_index <- min_index
      largest_change_desc <- df$`Industry Description`[min_index]
    } else {
      largest_change <- max_value
      largest_change_index <- max_index
      largest_change_desc <- df$`Industry Description`[max_index]
    }
    
    c(delta_abs = abs(largest_change),
      j_abs = colnames(value_mat)[largest_change_index],
      j_abs_desc = largest_change_desc,
      delta_min = min_value, 
      j_min = colnames(value_mat)[min_index],
      j_min_desc = df$`Industry Description`[min_index], 
      delta_max = max_value, 
      j_max = colnames(value_mat)[max_index],
      j_max_desc = df$`Industry Description`[max_index])
  })
  
  result = cbind(df[,1:2], data.frame(t(extremes)))
  # make columns delta_abs, delta_min, delta_max numeric
  result$delta_abs <- as.numeric(result$delta_abs)
  result$delta_min <- as.numeric(result$delta_min)
  result$delta_max <- as.numeric(result$delta_max)
  
  return(result)
}

########################################
# overlapping histogram by horizon
#######################################
gen_hist <- function(df, cutoff = 0.01, x_title, binwidth = 0.01){
  df$delta[abs(df$delta) <= cutoff] <- NA
  # Define a consistent color palette for the horizons
  horizon_colors <- c("1yr" = "#440154FF", "5yrs" = "#21908CFF", "10yrs" = "#FDE725FF")
  
  hist <- ggplot(df, aes(x = delta, fill = horizon)) +
    geom_histogram(binwidth = .01, alpha = 0.5, position = "identity", aes(y = ..count..)) + # Adjusted to plot frequencies
    scale_fill_manual(values = horizon_colors) + # Use the defined color palette
    labs(x = x_title, y = "Frequency", fill = "Horizon") + # Adjusted y-axis label to "Frequency"
    theme_minimal() +
    theme(legend.position = "bottom", text = element_text(family = "serif", size = 32))
  
  return(hist)
}

################################################
# define function that takes 4 A matrices and plots weights of one on x-axis, rest on y-axis 
################################################
XY_change <- function(A1, A2, A3, A4, lab1, lab2, lab3){
  # Initialize A_long with melted A1
  A_long <- reshape2::melt(A1, id.vars = c("Code", "Industry Description"))
  A_long$w1 <- A_long$value
  A_long <- A_long[, c("Code", "Industry Description", "variable", "w1")]
  colnames(A_long) <- c("Code", "Industry Description", "j", "w1")
  
  # List of additional datasets to merge
  datasets <- list(A2, A3, A4)
  
  # Loop through each dataset for merging
  for (i in seq_along(datasets)) {
    melted_data <- reshape2::melt(datasets[[i]], id.vars = c("Code", "Industry Description"))
    colnames(melted_data) <- c("Code", "Industry Description", "j", paste0("w", i + 1))
    A_long <- merge(A_long, melted_data, by = c("Code", "Industry Description", "j"))
  }
  # take out total output
  A_long <- subset(A_long, j != "T008")
  A_long <- subset(A_long, j != "total_industry_output")
  # log w1-w4
  A_long <- A_long %>%
    mutate(w1 = ifelse(w1>0, log(w1), NA),
           w2 = ifelse(w2>0, log(w2), NA),
           w3 = ifelse(w3>0, log(w3), NA),
           w4 = ifelse(w4>0, log(w4), NA))
  
  colors <- setNames(hue_pal()(3), c(lab1, lab2, lab3))
  
  # Plot 1: w1 vs w2
  p1 <- ggplot(A_long, aes(x = w1, y = w2)) +
    geom_point(aes(color = lab1), alpha = .5) +
    labs(x = expression(log(a[ij])), y = expression(log(a[ij]))) +
    guides(color = guide_legend(title = "Year")) +
    scale_color_manual(values = colors, breaks = c(lab1, lab2, lab3)) +
    theme_minimal() +
    theme(text = element_text(family = "serif", size = 32))
  
  # Plot 2: w1 vs w2 and w1 vs w3
  p2 <- ggplot(A_long) +
    geom_point(aes(x = w1, y = w2, color = lab1), alpha = .5) +
    geom_point(aes(x = w1, y = w3, color = lab2), alpha = .5) +
    labs(x = expression(log(a[ij])), y = expression(log(a[ij]))) +
    guides(color = guide_legend(title = "Year")) +
    scale_color_manual(values = colors, breaks = c(lab1, lab2, lab3)) +
    theme_minimal() +
    theme(text = element_text(family = "serif", size = 32))
  
  # Plot 3: w1 vs w2, w1 vs w3, and w1 vs w4
  
  p3 <- ggplot(A_long) +
    geom_point(aes(x = w1, y = w2, color = lab1), alpha = .5) +
    geom_point(aes(x = w1, y = w3, color = lab2), alpha = .5) +
    geom_point(aes(x = w1, y = w4, color = lab3), alpha = .5) +
    labs(x = expression(log(a[ij])), y = expression(log(a[ij]))) +
    guides(color = guide_legend(title = "Year")) +
    scale_color_manual(values = colors, breaks = c(lab1, lab2, lab3)) +
    theme_minimal() +
    theme(text = element_text(family = "serif", size = 32))
  
  return(list(p1, p2, p3))
}

#############################
# regression wrapper
#############################
reg_wrapper <- function(data, formula) {
  results <- lm(formula, data = data)
  results_SE <- coeftest(results, vcov = vcovHC(results, type = "HC1"))[, "Std. Error"]
  results_p <- coeftest(results, vcov = vcovHC(results, type = "HC1"))[, "Pr(>|t|)"]
  return(list(results, results_SE, results_p))
}

reg_wrapper_feols <- function(data, formula) {
  dat <- data
  reg <- feols(formula, data = dat)
  reg_sum <- reg_sum <- summary(reg, vcov_cluster(cluster = "Code")) # cluster SEs by industry
  table <- reg_sum$coeftable
  results_SE <- table[, "Std. Error"]
  results_p <- table[, "Pr(>|t|)"]
  return(list(reg, results_SE, results_p))
}

#############################
# local projections wrapper
#############################

extract_lp_data <- function(lp_result, label) {
  data.frame(
    irf = as.vector(lp_result$irf_panel_mean),
    se = as.vector(lp_result$irf_panel_mean) - as.vector(lp_result$irf_panel_low),
    type = label,
    horizon = 0:(ncol(lp_result$irf_panel_mean)-1)
  )
}

graph_lp_both <- function(lp_og, lp_resid){
  
  patents_lp_og_data <- extract_lp_data(lp_og, "og")
  patents_lp_resid_data <- extract_lp_data(lp_resid, "resid")
  
  # Combine the data into a single data frame
  patents_lp_plot_data <- bind_rows(patents_lp_og_data, patents_lp_resid_data)
  
  # Plot the overlapping results with confidence intervals
  ggplot(patents_lp_plot_data, aes(x = horizon, y = irf, color = type, fill = type)) +
    geom_line(size = 1) +
    geom_ribbon(aes(ymin = irf - se, ymax = irf + se), alpha = 0.4) +
    geom_ribbon(aes(ymin = irf - 1.65*se, ymax = irf + 1.65*se), alpha = 0.2) +
    scale_color_manual(values = c("og" = "blue", "resid" = "red")) +
    scale_fill_manual(values = c("og" = "blue", "resid" = "red")) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
    labs(title = "",
         x = "Horizon",
         y = "",
         color = "Measure",
         fill = "Measure") + 
    theme_minimal() + theme(legend.position = "bottom", text = element_text(family = "serif", size = 32))
}

graph_lp_one <- function(lp, label){
  patents_lp_data <- extract_lp_data(lp, label)
  
  ggplot(patents_lp_data, aes(x = horizon, y = irf)) +
    geom_line(size = 1) +
    geom_ribbon(aes(ymin = irf-se, ymax = irf+se), alpha = 0.4) +
    geom_ribbon(aes(ymin = irf-1.65*se, ymax = irf+1.65*se), alpha = 0.2) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
    labs(title = "",
         x = "Horizon",
         y = "") + 
    theme_minimal() + theme(legend.position = "bottom", text = element_text(family = "serif", size = 32))
}

##############################
# rolling_regression
# impose constant theta on +/- margin yrs of t
# recover theta 
# recover residual
##################################

rolling_regression <- function(data, yr, margin=2){
  

  # run empirical specification
  dat = subset(data, year >= yr-margin & year <= yr+margin)
  dat$industry_year <- factor(paste(dat$Code, dat$year))
  dat$input_year <- factor(paste(dat$j, dat$year))
  mod = feols(delta_1 ~ delta_logPratio_1_II:Code | industry_year, data = dat)
  mod_sum = summary(mod, vcov = "hetero") # cluster SEs by input-year
  
  # recover thetas
  coef_names = names(mod_sum$coefficients)
  coef = mod_sum$coefficients[grep("delta_logPratio_1_II", coef_names)]
  std_err = mod_sum$se[grep("delta_logPratio_1_II", coef_names)]
  theta = 1 - coef
  theta = data.frame(Code = levels(dat$Code), year = yr, theta = theta, std_err = std_err)
  
  # gen sum of preceding price changes
  # margin+1 - delta_price_ratio; want to sum over preceding margin periods, exclude contemperanous change
  dat <- dat %>%
    group_by(Code, j) %>%
    arrange(year) %>%
    mutate(delta_logPratio_1_CumSum = rollapply(delta_logPratio_1_II, margin+1, sum, fill = NA, align = "right") - delta_logPratio_1_II)
  
  # recover residuals
  dat$residual = mod$residuals
  year_dat = subset(dat, year == yr) # restrict to year of interest
  year_dat = merge(year_dat, theta, by = c("Code","year"))
  year_dat <- year_dat %>% select(Code, j, year, delta_1, delta_logPratio_1_II, residual, theta, delta_logPratio_1_CumSum)
  
  # return theta and data
  return(list(theta = theta, data = year_dat))
}

##############################
# plot_elast_coef
# plot theta_i for a given t
# i across x-axis
# +/- 1.645*std_err as error bars
##################################

plot_elast_coef <- function(data) {
  year = unique(data$year_group)
  plot <- ggplot(data, aes(x = Code, y = 0)) +
    geom_errorbar(aes(ymin = lower_theta, ymax = upper_theta), width = 0.2) +
    geom_point(aes(y = theta)) +
    labs(x = expression(~theta[i]),
         y = "Confidence Interval",
         color = "Includes Zero", title = year) + theme_minimal() +
    theme(axis.text.x = element_blank(), text = element_text(family = "serif", size = 24), legend.position = "none") +
    geom_hline(yintercept = 0, linetype = "dashed") + geom_hline(yintercept = 1, linetype = "dashed") + coord_cartesian(ylim = c(-2, 2))
  return(plot)
}

