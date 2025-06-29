################################################################################
# Load Libraries
################################################################################
library(car)
library(ggpubr)
library(effects)
library(jtools)

################################################################################
# Import functions
################################################################################
source(file.path("scripts", "functions", "calc_plot_dims.R"))
source(file.path("scripts", "functions", "clean_R2_metrics.R"))

################################################################################
# Define metrics that need to be squared to align with R2 values
################################################################################
metrics_to_square <- c(
  "Pearson correlation coefficeint r",
  "r",
  "|r|",
  "R"
)

################################################################################
# Drop these specific comparisons because results for the individual fires were
# note reported - only results at the "vegetation type" level within each fire
################################################################################
labels_to_drop <- c(
  "dNBR woodlands: 0.70; RdNBR sagebrush",
  "dNBR woodlands: 0.69; dNBR sagebrush",
  "dNBR woodlands: 0.53; RdNBR sagebrush",
  "dNBRwet: 0.64; dNBRdamp: 0.65; dNBRforby",
  "RadarBurnRatiowet: 0.30; RadarBurnRatiodamp: 0.39; RadarBurnRatioforby",
  "RadarBurnRatioRFDI wet: 0.44; RadarBurnRatioHH damp: 0.47; RadarBurnRatioHH forby: 0.44 (wet conditions)",
  "RadarBurnRatioHV wet: 0.62; RadarBurnRatioHV damp: 0.42; RadarBurnRatioHV forby: 0.48 (dry conditions)"
)

################################################################################
# Apply function to clean up spatial and temporal comparisons of R2 values
# to circular and square spatial alignment, RS/plot and plot/fire temporal
# synchrony
################################################################################
sync_size_circular_performance <- clean_R2_metrics(
  sync_df = sync_size_circular,
  labels_to_drop = labels_to_drop,
  metrics_to_square = metrics_to_square,
  synchrony_type = "spatial"
)

sync_size_square_performance <- clean_R2_metrics(
  sync_df = sync_size_square,
  labels_to_drop = labels_to_drop,
  metrics_to_square = metrics_to_square,
  synchrony_type = "spatial"
)

sync_temp_rs_plot_performance <- clean_R2_metrics(
  sync_df = sync_plot_imagery,
  labels_to_drop = labels_to_drop,
  metrics_to_square = metrics_to_square,
  synchrony_type = "temporal_rs_plot"
)

sync_temp_plot_fire_performance <- clean_R2_metrics(
  sync_df = sync_fire_plot,
  labels_to_drop = labels_to_drop,
  metrics_to_square = metrics_to_square,
  synchrony_type = "temporal_plot_fire"
)

################################################################################
# Subset prior outputs so that across each "comparison" (using Comparison_idx),
# there is the same ASI, same spatial alignment, same temporal synchrony
################################################################################

# Circular set
df1_subset <- sync_size_circular_performance$comparisons_same
df1_subset$source <- 1

df2_subset <- sync_temp_plot_fire_performance$comparisons_same
df2_subset$source <- 2

df3_subset <- sync_temp_rs_plot_performance$comparisons_same
df3_subset$source <- 3

# Merge df1 and df2 on the first 10 columns (i.e., comparison columns)
merged_12 <- merge(df1_subset, df2_subset, by = "Comparison_idx")

# Merge the result with df3 on the same columns
merged_all_circular <- merge(merged_12, df3_subset, by = "Comparison_idx")

# Square set
df1_subset <- sync_size_square_performance$comparisons_same
df1_subset$source <- 1

df2_subset <- sync_temp_plot_fire_performance$comparisons_same
df2_subset$source <- 2

df3_subset <- sync_temp_rs_plot_performance$comparisons_same
df3_subset$source <- 3

# Merge df1 and df2 on the first 10 columns (i.e., comparison columns)
merged_12 <- merge(df1_subset, df2_subset, by = "Comparison_idx")

# Merge the result with df3 on the same columns
merged_all_square <- merge(merged_12, df3_subset, by = "Comparison_idx")

# Merge the circular and square comparison sets together
# Identify missing columns
missing_cols_df1 <- setdiff(names(merged_all_square), names(merged_all_circular))
missing_cols_df2 <- setdiff(names(merged_all_circular), names(merged_all_square))

# Add missing columns with NA to each dataframe
for (col in missing_cols_df1) {
  merged_all_circular[[col]] <- NA
}

for (col in missing_cols_df2) {
  merged_all_square[[col]] <- NA
}

# Ensure the columns are in the same order
merged_all_circular <- merged_all_circular[, names(merged_all_square)] # Reorder df1 columns to match df2

# rbind them
combined_df <- rbind(merged_all_circular, merged_all_square)

# Assign single ASI value depending on whether comparison was circular or square plots
combined_df$asi_abs <- ifelse(!is.na(combined_df$circ_asi_abs), combined_df$circ_asi_abs, combined_df$square_asi_abs)

# Drop any rows that do not have an R2 value
combined_df <- combined_df[complete.cases(combined_df$metric_value), ]

# Add columns for temporal synchrony
combined_df$delay_rs_plots <- combined_df$Postfire_month - combined_df$Plots_month.x
combined_df$delay_plots_fire <- combined_df$Plots_month.x - combined_df$Fire_start_month

# Write out number of models contained in combined_df
number_aligned_comarisons <- nrow(combined_df)
write.csv(
  number_aligned_comarisons,
  file = "tables/number_aligned_comarisons.csv",
  row.names = FALSE
)

################################################################################
# Stepwise regression modeling
################################################################################

# Factorize smoothing variable
combined_df$Smoothing <- as.factor(combined_df$Smoothing)

# Null model (no predictors)
null_model <- lm(
  metric_value ~ 1, 
  data = combined_df, 
  weights = n_fires
)

# Full model (all predictors)
full_model <- lm(
  metric_value ~ asi_abs * Smoothing * delay_plots_fire * delay_rs_plots,
  data = combined_df, 
  weights = n_fires
)

# Stepwise mdel selection
model_step <- step(
  null_model, 
  scope = list(lower = null_model, upper = full_model),
)

# Summary of the selected model
model_step_summary <- summary(model_step)

################################################################################
# Create effects plots
################################################################################

# Calculate mean R2 value
metric_value_mean <- weighted.mean(combined_df$metric_value, w = combined_df$n_fires)
delay_rs_plots_mean <- weighted.mean(combined_df$delay_rs_plots, w = combined_df$n_fires)
delay_plots_fire_mean <- weighted.mean(combined_df$delay_plots_fire, w = combined_df$n_fires)

aligned_model_set_means <- data.frame(
  r2 = metric_value_mean,
  delay_rs_plots = delay_rs_plots_mean,
  delay_plots_fire = delay_plots_fire_mean
)

# Timing between RS data and plots effects plot
effect_rs_plots <- effect_plot(
  model = model_step, 
  pred = delay_rs_plots, 
  interval = TRUE, 
  plot.points = TRUE, 
  x.label = "Remotely sensed data month - Field plot month",
  y.label = expression(R^2),
  ) + 
  scale_y_continuous(limits = c(0, 1)) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 12),
    axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 14)),
    axis.title.y = element_text(size = 14, face = "bold", margin = margin(t = 14)),
    plot.margin = margin(t = 25, r = 5, b = 5, l = 5)
  ) + 
  geom_hline(
    yintercept = metric_value_mean,
    color = "red",
    linetype = "dashed",
    linewidth = 0.5
  )

# Timing between fire start and plots effects plot
effect_plots_fire <- effect_plot(
  model = model_step, 
  pred = delay_plots_fire, 
  interval = TRUE, 
  plot.points = TRUE, 
  x.label = "Field plot month - Fire start month",
  y.label = expression(R^2),
) + 
  scale_y_continuous(limits = c(0, 1)) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 12),
    axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 14)),
    axis.title.y = element_text(size = 14, face = "bold", margin = margin(t = 14)),
    plot.margin = margin(t = 25, r = 5, b = 5, l = 5)
  ) + 
  geom_hline(
    yintercept = metric_value_mean,
    color = "red",
    linetype = "dashed",
    linewidth = 0.5
  )






################################################################################
# Extract relevant information from lists of lumped comparisons with unsynchronous or unaligned data
################################################################################

sync_size_circular_inconsistent_indices <- unique(
  sync_size_circular_performance$lumped_comparisons_diff$Comparison_idx
  )
sync_size_circular_results <- vector(
  mode = "list", length = length(sync_size_circular_inconsistent_indices)
  )

for (i in seq_along(sync_size_circular_inconsistent_indices)) {
  inconsistent_subset <- sync_size_circular_performance$lumped_comparisons_diff[
    sync_size_circular_performance$lumped_comparisons_diff$Comparison_idx == sync_size_circular_inconsistent_indices[i], 
    ]
  citation <- unique(inconsistent_subset$Citation)
  plot_sizes <- inconsistent_subset$Plot_sqm
  pixel_sizes <- inconsistent_subset$Post_sqm
  ifelse(length(unique(plot_sizes)) > 1, diff_plot_sizes <- TRUE, diff_plot_sizes <- FALSE)
  ifelse(length(unique(pixel_sizes)) > 1, diff_pixel_sizes <- TRUE, diff_pixel_sizes <- FALSE)
  
  sync_size_circular_results[[i]] <- list(
    citation = citation,
    comparison_idx = sync_size_circular_inconsistent_indices[i],
    plot_sizes = plot_sizes,
    pixel_sizes = pixel_sizes,
    diff_plot_sizes = diff_plot_sizes,
    diff_pixel_sizes = diff_pixel_sizes
  )
}

sync_size_square_inconsistent_indices <- unique(
  sync_size_square_performance$lumped_comparisons_diff$Comparison_idx
  )
sync_size_square_results <- vector(
  mode = "list", length = length(sync_size_square_inconsistent_indices)
  )
for (i in seq_along(sync_size_square_inconsistent_indices)) {
  inconsistent_subset <- sync_size_square_performance$lumped_comparisons_diff[
    sync_size_square_performance$lumped_comparisons_diff$Comparison_idx == sync_size_square_inconsistent_indices[i], 
    ]
  citation <- unique(inconsistent_subset$Citation)
  plot_sizes <- inconsistent_subset$Plot_sqm
  pixel_sizes <- inconsistent_subset$Post_sqm
  ifelse(length(unique(plot_sizes)) > 1, diff_plot_sizes <- TRUE, diff_plot_sizes <- FALSE)
  ifelse(length(unique(pixel_sizes)) > 1, diff_pixel_sizes <- TRUE, diff_pixel_sizes <- FALSE)
  
  sync_size_square_results[[i]] <- list(
    citation = citation,
    comparison_idx = sync_size_square_inconsistent_indices[i],
    plot_sizes = plot_sizes,
    pixel_sizes = pixel_sizes,
    diff_plot_sizes = diff_plot_sizes,
    diff_pixel_sizes = diff_pixel_sizes
  )
}

sync_temp_plot_fire_inconsistent_indices <- unique(
  sync_temp_plot_fire_performance$lumped_comparisons_diff$Comparison_idx
  )
sync_temp_plot_fire_results <- vector(
  mode = "list", length = length(sync_temp_plot_fire_inconsistent_indices)
  )
for (i in seq_along(sync_temp_plot_fire_inconsistent_indices)) {
  inconsistent_subset <- sync_temp_plot_fire_performance$lumped_comparisons_diff[
    sync_temp_plot_fire_performance$lumped_comparisons_diff$Comparison_idx == sync_temp_plot_fire_inconsistent_indices[i], 
    ]
  citation <- unique(inconsistent_subset$Citation)
  plot_months <- inconsistent_subset$Plots_month
  fire_months <- inconsistent_subset$Fire_start_month
  ifelse(length(unique(plot_months)) > 1, diff_plot_months <- TRUE, diff_plot_months <- FALSE)
  ifelse(length(unique(fire_months)) > 1, diff_fire_months <- TRUE, diff_fire_months <- FALSE)
  delay <- plot_months - fire_months
  
  sync_temp_plot_fire_results[[i]] <- list(
    citation = citation,
    comparison_idx = sync_temp_plot_fire_inconsistent_indices[i],
    plot_months = plot_months,
    fire_months = fire_months,
    diff_plot_months = diff_plot_months,
    diff_fire_months = diff_fire_months,
    delay = delay
  )
}

sync_temp_rs_plot_inconsistent_indices <- unique(
  sync_temp_rs_plot_performance$lumped_comparisons_diff$Comparison_idx
  )
sync_temp_rs_plot_results <- vector(
  mode = "list", length = length(sync_temp_rs_plot_inconsistent_indices)
  )
for (i in seq_along(sync_temp_rs_plot_inconsistent_indices)) {
  inconsistent_subset <- sync_temp_rs_plot_performance$lumped_comparisons_diff[
    sync_temp_rs_plot_performance$lumped_comparisons_diff$Comparison_idx == sync_temp_rs_plot_inconsistent_indices[i], 
    ]
  citation <- unique(inconsistent_subset$Citation)
  imagery_months <- inconsistent_subset$Postfire_month
  plot_months <- inconsistent_subset$Plots_month
  ifelse(length(unique(imagery_months)) > 1, diff_imagery_months <- TRUE, diff_imagery_months <- FALSE)
  ifelse(length(unique(plot_months)) > 1, diff_plot_months <- TRUE, diff_plot_months <- FALSE)
  delay <- imagery_months - plot_months
  
  sync_temp_rs_plot_results[[i]] <- list(
    citation = citation,
    comparison_idx = sync_temp_rs_plot_inconsistent_indices[i],
    imagery_months = imagery_months,
    plot_months = plot_months,
    diff_imagery_months = diff_imagery_months,
    diff_plot_months = diff_plot_months,
    delay = delay
  )
}




# Create summary tables from relevant information

if (length(sync_size_circular_results) > 0) {
  sync_size_circular_summary_table <-
    do.call(rbind, lapply(sync_size_circular_results, function(x) {
      data.frame(
        citation = x$citation,
        diff_plot_sizes = x$diff_plot_sizes,
        diff_pixel_sizes = x$diff_pixel_sizes
      )
    }))
  
  sync_size_circular_summary_table <- cbind(
    sync_size_circular_summary_table,
    n_obs = sapply(sync_size_circular_results, function(x) length(x$plot_sizes)),
    min_plot_size = sapply(sync_size_circular_results, function(x) min(x$plot_sizes)),
    max_plot_size = sapply(sync_size_circular_results, function(x) max(x$plot_sizes)),
    mean_plot_size = round(sapply(sync_size_circular_results, function(x) mean(x$plot_sizes)), 1),
    sd_plot_size = round(sapply(sync_size_circular_results, function(x) sd(x$plot_sizes)), 1)
  )
  sync_size_circular_summary_table$cv_plot_size <- round(
    sync_size_circular_summary_table$sd_plot_size / sync_size_circular_summary_table$mean_plot_size * 100, 1
    )
  
  sync_size_circular_summary_table <- cbind(
    sync_size_circular_summary_table,
    min_pixel_size = sapply(sync_size_circular_results, function(x) min(x$pixel_sizes)),
    max_pixel_size = sapply(sync_size_circular_results, function(x) max(x$pixel_sizes)),
    mean_pixel_size = round(sapply(sync_size_circular_results, function(x) mean(x$pixel_sizes)), 1),
    sd_pixel_size = round(sapply(sync_size_circular_results, function(x) sd(x$pixel_sizes)), 1)
  )
  sync_size_circular_summary_table$cv_pixel_size <- round(
    sync_size_circular_summary_table$sd_pixel_size / sync_size_circular_summary_table$mean_pixel_size * 100, 1
    )
  sync_size_circular_summary_table$comparison_idx <- sapply(sync_size_circular_results, function(x) x$comparison_idx)
}

if (length(sync_size_square_results) > 0) {
  sync_size_square_summary_table <-
    do.call(rbind, lapply(sync_size_square_results, function(x) {
      data.frame(
        citation = x$citation,
        diff_plot_sizes = x$diff_plot_sizes,
        diff_pixel_sizes = x$diff_pixel_sizes
      )
    }))
  
  sync_size_square_summary_table <- cbind(
    sync_size_square_summary_table,
    n_obs = sapply(sync_size_square_results, function(x) length(x$plot_sizes)),
    min_plot_size = sapply(sync_size_square_results, function(x) min(x$plot_sizes)),
    max_plot_size = sapply(sync_size_square_results, function(x) max(x$plot_sizes)),
    mean_plot_size = round(sapply(sync_size_square_results, function(x) mean(x$plot_sizes)), 1),
    sd_plot_size = round(sapply(sync_size_square_results, function(x) sd(x$plot_sizes)), 1)
  )
  sync_size_square_summary_table$cv_plot_size <- round(
    sync_size_square_summary_table$sd_plot_size / sync_size_square_summary_table$mean_plot_size * 100, 1
    )
  
  
  sync_size_square_summary_table <- cbind(
    sync_size_square_summary_table,
    min_pixel_size = sapply(sync_size_square_results, function(x) min(x$pixel_sizes)),
    max_pixel_size = sapply(sync_size_square_results, function(x) max(x$pixel_sizes)),
    mean_pixel_size = round(sapply(sync_size_square_results, function(x) mean(x$pixel_sizes)), 1),
    sd_pixel_size = round(sapply(sync_size_square_results, function(x) sd(x$pixel_sizes)), 1)
  )
  sync_size_square_summary_table$cv_pixel_size <- round(
    sync_size_square_summary_table$sd_pixel_size / sync_size_square_summary_table$mean_pixel_size * 100, 1
    )
  sync_size_square_summary_table$comparison_idx <- sapply(sync_size_square_results, function(x) x$comparison_idx)
}


if (length(sync_temp_plot_fire_results) > 0) {
  sync_temp_plot_fire_summary_table <-
    do.call(rbind, lapply(sync_temp_plot_fire_results, function(x) {
      data.frame(
        citation = x$citation,
        diff_plot_months = x$diff_plot_months,
        diff_fire_months = x$diff_fire_months
      )
    }))
  
  sync_temp_plot_fire_summary_table <- cbind(
    sync_temp_plot_fire_summary_table,
    n_obs = sapply(sync_temp_plot_fire_results, function(x) length(x$plot_months)),
    min_plot_months = sapply(sync_temp_plot_fire_results, function(x) min(x$plot_months)),
    max_plot_months = sapply(sync_temp_plot_fire_results, function(x) max(x$plot_months)),
    mean_plot_months = round(sapply(sync_temp_plot_fire_results, function(x) mean(x$plot_months)), 1),
    sd_plot_months = round(sapply(sync_temp_plot_fire_results, function(x) sd(x$plot_months)), 1)
  )
  sync_temp_plot_fire_summary_table$cv_plot_months <- round(
    sync_temp_plot_fire_summary_table$sd_plot_months / sync_temp_plot_fire_summary_table$mean_plot_months * 100, 1
    )
  
  sync_temp_plot_fire_summary_table <- cbind(
    sync_temp_plot_fire_summary_table,
    min_fire_months = sapply(sync_temp_plot_fire_results, function(x) min(x$fire_months)),
    max_fire_months = sapply(sync_temp_plot_fire_results, function(x) max(x$fire_months)),
    mean_fire_months = round(sapply(sync_temp_plot_fire_results, function(x) mean(x$fire_months)), 1),
    sd_fire_months = round(sapply(sync_temp_plot_fire_results, function(x) sd(x$fire_months)), 1)
  )
  sync_temp_plot_fire_summary_table$cv_fire_months <- round(
    sync_temp_plot_fire_summary_table$sd_fire_months / sync_temp_plot_fire_summary_table$mean_fire_months * 100, 1
    )
  
  sync_temp_plot_fire_summary_table <- cbind(
    sync_temp_plot_fire_summary_table,
    min_delay = sapply(sync_temp_plot_fire_results, function(x) min(x$delay)),
    max_delay = sapply(sync_temp_plot_fire_results, function(x) max(x$delay)),
    mean_delay = round(sapply(sync_temp_plot_fire_results, function(x) mean(x$delay)), 1),
    sd_delay = round(sapply(sync_temp_plot_fire_results, function(x) sd(x$delay)), 1)
  )
  sync_temp_plot_fire_summary_table$range_delay <- sync_temp_plot_fire_summary_table$max_delay - sync_temp_plot_fire_summary_table$min_delay
  sync_temp_plot_fire_summary_table$cv_delay <- round(
    sync_temp_plot_fire_summary_table$sd_delay / sync_temp_plot_fire_summary_table$mean_delay * 100, 1
    )
  sync_temp_plot_fire_summary_table$comparison_idx <- sapply(sync_temp_plot_fire_results, function(x) x$comparison_idx)
}


if (length(sync_temp_rs_plot_results) > 0) {
  sync_temp_rs_plot_summary_table <-
    do.call(rbind, lapply(sync_temp_rs_plot_results, function(x) {
      data.frame(
        citation = x$citation,
        diff_imagery_months = x$diff_imagery_months,
        diff_plot_months = x$diff_plot_months
      )
    }))
  
  sync_temp_rs_plot_summary_table <- cbind(
    sync_temp_rs_plot_summary_table,
    n_obs = sapply(sync_temp_rs_plot_results, function(x) length(x$plot_months)),
    min_imagery_month = sapply(sync_temp_rs_plot_results, function(x) min(x$imagery_months)),
    max_imagery_month = sapply(sync_temp_rs_plot_results, function(x) max(x$imagery_months)),
    mean_imagery_month = round(sapply(sync_temp_rs_plot_results, function(x) mean(x$imagery_months)), 1),
    sd_imagery_month = round(sapply(sync_temp_rs_plot_results, function(x) sd(x$imagery_months)), 1)
  )
  sync_temp_rs_plot_summary_table$cv_imagery_month <- round(
    sync_temp_rs_plot_summary_table$sd_imagery_month / sync_temp_rs_plot_summary_table$mean_imagery_month * 100, 1
    )
  
  sync_temp_rs_plot_summary_table <- cbind(
    sync_temp_rs_plot_summary_table,
    min_plot_months = sapply(sync_temp_rs_plot_results, function(x) min(x$plot_months)),
    max_plot_months = sapply(sync_temp_rs_plot_results, function(x) max(x$plot_months)),
    mean_plot_months = round(sapply(sync_temp_rs_plot_results, function(x) mean(x$plot_months)), 1),
    sd_plot_months = round(sapply(sync_temp_rs_plot_results, function(x) sd(x$plot_months)), 1)
  )
  sync_temp_rs_plot_summary_table$cv_plot_months <- round(
    sync_temp_rs_plot_summary_table$sd_plot_months / sync_temp_rs_plot_summary_table$mean_plot_months * 100, 1
    )
  
  sync_temp_rs_plot_summary_table <- cbind(
    sync_temp_rs_plot_summary_table,
    min_delay = sapply(sync_temp_rs_plot_results, function(x) min(x$delay)),
    max_delay = sapply(sync_temp_rs_plot_results, function(x) max(x$delay)),
    mean_delay = round(sapply(sync_temp_rs_plot_results, function(x) mean(x$delay)), 1),
    sd_delay = round(sapply(sync_temp_rs_plot_results, function(x) sd(x$delay)), 1)
  )
  sync_temp_rs_plot_summary_table$range_delay <- sync_temp_rs_plot_summary_table$max_delay - sync_temp_rs_plot_summary_table$min_delay
  sync_temp_rs_plot_summary_table$cv_delay <- round(
    sync_temp_rs_plot_summary_table$sd_delay / sync_temp_rs_plot_summary_table$mean_delay * 100, 1
    )
  sync_temp_rs_plot_summary_table$comparison_idx <- sapply(sync_temp_rs_plot_results, function(x) x$comparison_idx)
}



# Write out number of misaligned comparisons
number_misaligned_comparisons <- length(sync_size_circular_results) +
  length(sync_size_square_results) +
  length(sync_temp_plot_fire_results) +
  length(sync_temp_rs_plot_results)



# Calculate mean and sd of ASI for models where there is variable ASI between the plots/imagery within the data set (circular plots)
sync_size_circular_summary_table_mean <- aggregate(
  x = sync_size_circular[sync_size_circular$Comparison_idx %in% sync_size_circular_summary_table$comparison_idx, ]$circ_asi_abs, 
  by = list(
    comparison_idx = sync_size_circular[
      sync_size_circular$Comparison_idx %in% sync_size_circular_summary_table$comparison_idx, 
      ]$Comparison_idx
    ),
  FUN = mean
)
colnames(sync_size_circular_summary_table_mean)[2] <- "asi_mean"

sync_size_circular_summary_table_sd <- aggregate(
  x = sync_size_circular[sync_size_circular$Comparison_idx %in% sync_size_circular_summary_table$comparison_idx, ]$circ_asi_abs, 
  by = list(
    comparison_idx = sync_size_circular[
      sync_size_circular$Comparison_idx %in% sync_size_circular_summary_table$comparison_idx, 
      ]$Comparison_idx
    ),
  FUN = sd
)
colnames(sync_size_circular_summary_table_sd)[2] <- "asi_sd"

# Join back to summary table
sync_size_circular_summary_table <- merge(
  x = sync_size_circular_summary_table,
  y = sync_size_circular_summary_table_mean,
  by = "comparison_idx"
)

sync_size_circular_summary_table <- merge(
  x = sync_size_circular_summary_table,
  y = sync_size_circular_summary_table_sd,
  by = "comparison_idx"
)

# Calculate ASI CV
sync_size_circular_summary_table$asi_cv <- round(
  sync_size_circular_summary_table$asi_sd / sync_size_circular_summary_table$asi_mean * 100, 1
  )

# Merge back R2 values
sync_size_circular_summary_table <- merge(
  x = sync_size_circular_summary_table,
  y = sync_size_circular_performance$lumped_comparisons_diff[
    !duplicated(sync_size_circular_performance$lumped_comparisons_diff$Comparison_idx), c("Comparison_idx", "metric_value")
    ],
  by.x = "comparison_idx",
  by.y = "Comparison_idx"
)
sync_size_circular_summary_table <- merge(
  x = sync_size_circular_summary_table,
  y = sync_size_circular_performance$lumped_comparisons_diff[
    !duplicated(sync_size_circular_performance$lumped_comparisons_diff$Comparison_idx), c("Comparison_idx", "Model_metric")
    ],
  by.x = "comparison_idx",
  by.y = "Comparison_idx"
)

sync_size_circular_summary_table$asi_cv <- round(sync_size_circular_summary_table$asi_cv, 1)
sync_size_circular_summary_table$asi_mean <- round(sync_size_circular_summary_table$asi_mean, 1)
sync_size_circular_summary_table$asi_sd <- round(sync_size_circular_summary_table$asi_sd, 1)


# Calculate mean and sd of ASI for models where there is variable ASI between the plots/imagery within the data set (square plots)
if (exists("sync_size_square_summary_table")) {
  sync_size_square_summary_table_mean <- aggregate(
    x = sync_size_square[
      sync_size_square$Comparison_idx %in% sync_size_square_summary_table$comparison_idx, 
      ]$circ_asi_abs, 
    by = list(
      comparison_idx = sync_size_square[
        sync_size_square$Comparison_idx %in% sync_size_square_summary_table$comparison_idx, ]$Comparison_idx
      ),
    FUN = mean
  )
  colnames(sync_size_square_summary_table_mean)[2] <- "asi_mean"
  
  sync_size_square_summary_table_sd <- aggregate(
    x = sync_size_square[
      sync_size_square$Comparison_idx %in% sync_size_square_summary_table$comparison_idx, 
      ]$circ_asi_abs, 
    by = list(
      comparison_idx = sync_size_square[
        sync_size_square$Comparison_idx %in% sync_size_square_summary_table$comparison_idx, ]$Comparison_idx
      ),
    FUN = sd
  )
  colnames(sync_size_square_summary_table_sd)[2] <- "asi_sd"
  
  # Join back to summary table
  sync_size_square_summary_table <- merge(
    x = sync_size_square_summary_table,
    y = sync_size_square_summary_table_mean,
    by = "comparison_idx"
  )
  
  sync_size_square_summary_table <- merge(
    x = sync_size_square_summary_table,
    y = sync_size_square_summary_table_sd,
    by = "comparison_idx"
  )
  
  # Calculate ASI CV
  sync_size_square_summary_table$asi_cv <- round(
    sync_size_square_summary_table$asi_sd / sync_size_square_summary_table$asi_mean * 100, 1
    )
  
  # Merge back R2 values
  sync_size_square_summary_table <- merge(
    x = sync_size_square_summary_table,
    y = sync_size_square_performance$lumped_comparisons_diff[
      !duplicated(sync_size_square_performance$lumped_comparisons_diff$Comparison_idx), c("Comparison_idx", "metric_value")
      ],
    by.x = "comparison_idx",
    by.y = "Comparison_idx"
  )
  sync_size_square_summary_table <- merge(
    x = sync_size_square_summary_table,
    y = sync_size_square_performance$lumped_comparisons_diff[
      !duplicated(sync_size_square_performance$lumped_comparisons_diff$Comparison_idx), c("Comparison_idx", "Model_metric")
      ],
    by.x = "comparison_idx",
    by.y = "Comparison_idx"
  )
  
  sync_size_square_summary_table$asi_cv <- round(sync_size_square_summary_table$asi_cv, 1)
  sync_size_square_summary_table$asi_mean <- round(sync_size_square_summary_table$asi_mean, 1)
  sync_size_square_summary_table$asi_sd <- round(sync_size_square_summary_table$asi_sd, 1)
}


# Subset temporal sync tables

# delay: should always be positive, i.e., plots sampled after ire start
sync_temp_plot_fire_summary_table <- sync_temp_plot_fire_summary_table[, c(
  "citation", "comparison_idx", "diff_plot_months", "diff_fire_months", 
  "n_obs", "min_delay", "max_delay", "range_delay", "mean_delay", "sd_delay", "cv_delay"
  )]

# delay: positive if imagery taken after plots sampled, negative if imagery taken before plots sampled
sync_temp_rs_plot_summary_table <- sync_temp_rs_plot_summary_table[, c(
  "citation", "comparison_idx", "diff_imagery_months", "diff_plot_months", 
  "n_obs", "min_delay", "max_delay", "range_delay", "mean_delay", "sd_delay", "cv_delay"
  )]

# Merge back R2 values
sync_temp_plot_fire_summary_table <- merge(
  x = sync_temp_plot_fire_summary_table,
  y = sync_temp_plot_fire_performance$lumped_comparisons_diff[
    !duplicated(sync_temp_plot_fire_performance$lumped_comparisons_diff$Comparison_idx), 
    c("Comparison_idx", "metric_value")
    ],
  by.x = "comparison_idx",
  by.y = "Comparison_idx"
)
sync_temp_plot_fire_summary_table <- merge(
  x = sync_temp_plot_fire_summary_table,
  y = sync_temp_plot_fire_performance$lumped_comparisons_diff[
    !duplicated(sync_temp_plot_fire_performance$lumped_comparisons_diff$Comparison_idx), 
    c("Comparison_idx", "Model_metric")
    ],
  by.x = "comparison_idx",
  by.y = "Comparison_idx"
)

# Merge back R2 values
sync_temp_rs_plot_summary_table <- merge(
  x = sync_temp_rs_plot_summary_table,
  y = sync_temp_rs_plot_performance$lumped_comparisons_diff[
    !duplicated(sync_temp_rs_plot_performance$lumped_comparisons_diff$Comparison_idx), 
    c("Comparison_idx", "metric_value")
    ],
  by.x = "comparison_idx",
  by.y = "Comparison_idx"
)
sync_temp_rs_plot_summary_table <- merge(
  x = sync_temp_rs_plot_summary_table,
  y = sync_temp_rs_plot_performance$lumped_comparisons_diff[
    !duplicated(sync_temp_rs_plot_performance$lumped_comparisons_diff$Comparison_idx), 
    c("Comparison_idx", "Model_metric")
    ],
  by.x = "comparison_idx",
  by.y = "Comparison_idx"
)

# Summarize number of fires included in each modeling set
sync_size_circular_summary_table_fires <- table(
  comparisons[comparisons$Comparison_idx %in% sync_size_circular_summary_table$comparison_idx, ]$Fire_name, 
  comparisons[comparisons$Comparison_idx %in% sync_size_circular_summary_table$comparison_idx, ]$Comparison_idx
)
sync_size_circular_summary_table_fires <- colSums(sync_size_circular_summary_table_fires)
sync_size_circular_summary_table_fires <- as.data.frame(sync_size_circular_summary_table_fires)
sync_size_circular_summary_table_fires$comparison_idx <- rownames(sync_size_circular_summary_table_fires)
colnames(sync_size_circular_summary_table_fires)[1] <- "n_fires"

sync_size_circular_summary_table <- merge(
  sync_size_circular_summary_table, 
  sync_size_circular_summary_table_fires, 
  by = "comparison_idx"
  )

if (exists("sync_size_square_summary_table")) {
  sync_size_square_summary_table_fires <- table(
    comparisons[comparisons$Comparison_idx %in% sync_size_square_summary_table$comparison_idx, ]$Fire_name, 
    comparisons[comparisons$Comparison_idx %in% sync_size_square_summary_table$comparison_idx, ]$Comparison_idx
  )
  sync_size_square_summary_table_fires <- colSums(sync_size_square_summary_table_fires)
  sync_size_square_summary_table_fires <- as.data.frame(sync_size_square_summary_table_fires)
  sync_size_square_summary_table_fires$comparison_idx <- rownames(sync_size_square_summary_table_fires)
  colnames(sync_size_square_summary_table_fires)[1] <- "n_fires"
  
  sync_size_square_summary_table <- merge(
    sync_size_square_summary_table, 
    sync_size_square_summary_table_fires, 
    by = "comparison_idx"
  )
}

sync_temp_plot_fire_summary_table_fires <- table(
  comparisons[comparisons$Comparison_idx %in% sync_temp_plot_fire_summary_table$comparison_idx, ]$Fire_name, 
  comparisons[comparisons$Comparison_idx %in% sync_temp_plot_fire_summary_table$comparison_idx, ]$Comparison_idx
)
sync_temp_plot_fire_summary_table_fires <- colSums(sync_temp_plot_fire_summary_table_fires)
sync_temp_plot_fire_summary_table_fires <- as.data.frame(sync_temp_plot_fire_summary_table_fires)
sync_temp_plot_fire_summary_table_fires$comparison_idx <- rownames(sync_temp_plot_fire_summary_table_fires)
colnames(sync_temp_plot_fire_summary_table_fires)[1] <- "n_fires"

sync_temp_plot_fire_summary_table <- merge(
  sync_temp_plot_fire_summary_table, 
  sync_temp_plot_fire_summary_table_fires, 
  by = "comparison_idx"
)

sync_temp_rs_plot_summary_table_fires <- table(
  comparisons[comparisons$Comparison_idx %in% sync_temp_rs_plot_summary_table$comparison_idx, ]$Fire_name, 
  comparisons[comparisons$Comparison_idx %in% sync_temp_rs_plot_summary_table$comparison_idx, ]$Comparison_idx
)
sync_temp_rs_plot_summary_table_fires <- colSums(sync_temp_rs_plot_summary_table_fires)
sync_temp_rs_plot_summary_table_fires <- as.data.frame(sync_temp_rs_plot_summary_table_fires)
sync_temp_rs_plot_summary_table_fires$comparison_idx <- rownames(sync_temp_rs_plot_summary_table_fires)
colnames(sync_temp_rs_plot_summary_table_fires)[1] <- "n_fires"

sync_temp_rs_plot_summary_table <- merge(
  sync_temp_rs_plot_summary_table, 
  sync_temp_rs_plot_summary_table_fires, 
  by = "comparison_idx"
)


# Studies that appear across all types of alignment and synchrony
circular_all_three <- Reduce(
  intersect, 
  list(
    sync_size_circular_summary_table$comparison_idx,
    sync_temp_plot_fire_summary_table$comparison_idx,
    sync_temp_rs_plot_summary_table$comparison_idx
    )
  )

circular_all_three <- comparisons[comparisons$Comparison_idx %in% circular_all_three, c("Citation", "Comparison_idx")]
circular_all_three <- unique(circular_all_three)
circular_all_three <- merge(
  circular_all_three, 
  sync_size_circular_summary_table[, c("comparison_idx", "n_fires")], 
  by.x = "Comparison_idx",
  by.y = "comparison_idx"
  )

if (exists("sync_size_square_summary_table")) {
  square_all_three <- Reduce(
    intersect, 
    list(
      sync_size_square_summary_table$citation,
      sync_size_square_summary_table$citation,
      sync_size_square_summary_table$citation
    )
  )
  square_all_three <- comparisons[comparisons$Comparison_idx %in% square_all_three, c("Citation", "Comparison_idx")]
  square_all_three <- unique(square_all_three)
  square_all_three <- merge(
    square_all_three, 
    sync_size_square_summary_table[, c("comparison_idx", "n_fires")], 
    by.x = "Comparison_idx",
    by.y = "comparison_idx"
  )
}



