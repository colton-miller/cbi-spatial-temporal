


source(file.path(repo_dir, "scripts", "clean_R2_metrics.R"))

metrics_to_square <- c(
  "Pearson correlation coefficeint r",
  "r",
  "|r|",
  "R"
)

labels_to_drop <- c(
  "dNBR woodlands: 0.70; RdNBR sagebrush",
  "dNBR woodlands: 0.69; dNBR sagebrush",
  "dNBR woodlands: 0.53; RdNBR sagebrush",
  "dNBRwet: 0.64; dNBRdamp: 0.65; dNBRforby",
  "RadarBurnRatiowet: 0.30; RadarBurnRatiodamp: 0.39; RadarBurnRatioforby",
  "RadarBurnRatioRFDI wet: 0.44; RadarBurnRatioHH damp: 0.47; RadarBurnRatioHH forby: 0.44 (wet conditions)",
  "RadarBurnRatioHV wet: 0.62; RadarBurnRatioHV damp: 0.42; RadarBurnRatioHV forby: 0.48 (dry conditions)"
)

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


# Define the layout matrix and relative widths
layout_matrix <- matrix(c(1, 2, 5, 
                          3, 4, 5), nrow = 2, byrow = TRUE)

# Set the layout with specified relative widths and heights
layout(layout_matrix, widths = c(4, 4, 1.5), heights = c(1, 1))

# Add margins to the plots for clarity
par(mar = c(4, 5, 5, 0), xpd = TRUE)


plot(
  x = sync_size_circular_performance$comparisons_same$circ_asi_abs, 
  y = sync_size_circular_performance$comparisons_same$metric_value,
  pch = 16,
  cex = sync_size_circular_performance$comparisons_same$n_fires,
  col = rgb(0, 0, 0, 0.5),
  las = 1,
  xlab = "|Assumption Strength Index (ASI)|",
  ylab = expression(bold(R^2)),
  ylim = c(0, 1),
  xlim = c(0, 100),
  font.lab = 2,
  bty = "n",
  cex.lab = 1.5,
  cex.axis = 1.5
  )
mtext(
  text = paste0("a) Spatial alignment: circular plots \n N = ", nrow(sync_size_circular_performance$comparisons_same)),
  side = 3,
  line = -1,
  adj = 0,
  font = 2
)


plot(
  x = sync_size_square_performance$comparisons_same$square_asi_abs, 
  y = sync_size_square_performance$comparisons_same$metric_value,
  pch = 16,
  cex = sync_size_square_performance$comparisons_same$n_fires,
  col = rgb(0, 0, 0, 0.5),
  las = 1,
  xlab = "|Assumption Strength Index (ASI)|",
  ylab = expression(bold(R^2)),
  ylim = c(0, 1),
  xlim = c(0, 100),
  font.lab = 2,
  bty = "n",
  cex.lab = 1.5,
  cex.axis = 1.5
  )
mtext(
  text = paste0("b) Spatial alignment: square plots \n N = ", nrow(sync_size_square_performance$comparisons_same)),
  side = 3,
  line = -1,
  adj = 0,
  font = 2
)



delay_min <- min(sync_temp_plot_fire_performance$comparisons_same$Plots_month - sync_temp_plot_fire_performance$comparisons_same$Fire_start_month)
delay_max <- max(sync_temp_plot_fire_performance$comparisons_same$Plots_month - sync_temp_plot_fire_performance$comparisons_same$Fire_start_month)

plot(
  x = sync_temp_plot_fire_performance$comparisons_same$Plots_month - sync_temp_plot_fire_performance$comparisons_same$Fire_start_month, 
  y = sync_temp_plot_fire_performance$comparisons_same$metric_value,
  pch = 16,
  cex = sync_temp_plot_fire_performance$comparisons_same$n_fires,
  col = rgb(0, 0, 0, 0.5),
  las = 1,
  xlab = "Field plots month - fire start month",
  ylab = expression(bold(R^2)),
  ylim = c(0, 1),
  xlim = c(delay_min - 1, delay_max + 1),
  font.lab = 2,
  bty = "n",
  cex.lab = 1.5,
  cex.axis = 1.5
)
mtext(
  text = paste0("c) Temporal synchrony: feild plots and fire igntion \n N = ", nrow(sync_temp_plot_fire_performance$comparisons_same)),
  side = 3,
  line = -1,
  adj = 0,
  font = 2
)


delay_min <- min(sync_temp_rs_plot_performance$comparisons_same$Postfire_month - sync_temp_rs_plot_performance$comparisons_same$Plots_month)
delay_max <- max(sync_temp_rs_plot_performance$comparisons_same$Postfire_month - sync_temp_rs_plot_performance$comparisons_same$Plots_month)


plot(
  x = sync_temp_rs_plot_performance$comparisons_same$Postfire_month - sync_temp_rs_plot_performance$comparisons_same$Plots_month, 
  y = sync_temp_rs_plot_performance$comparisons_same$metric_value,
  pch = 16,
  cex = sync_temp_rs_plot_performance$comparisons_same$n_fires,
  col = rgb(0, 0, 0, 0.5),
  las = 1,
  xlab = "Remotely sensed data month - field plots month",
  ylab = expression(bold(R^2)),
  ylim = c(0, 1),
  xlim = c(delay_min - 1, delay_max + 1),
  font.lab = 2,
  bty = "n",
  cex.lab = 1.5,
  cex.axis = 1.5
  )
mtext(
  text = paste0("d) Temporal synchrony: remotely sensed data and field plots \n N = ", nrow(sync_temp_rs_plot_performance$comparisons_same)),
  side = 3,
  line = -1,
  adj = 0,
  font = 2
)

par(mar = c(0, 2, 6, 0))
plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "", ann = FALSE)
legend(
  "topleft",
  legend = c("1", "3", "5", "10", "16"),
  col = rgb(0, 0, 0, 0.5),
  pch = 16,
  pt.cex = c(1, 3, 5, 10, 16),
  title = "Number of fires\nin comparison",
  y.intersp = c(0.75, 1, 1.5, 2, 4),
  x.intersp = c(0.75, 1.25, 1.5, 2, 3),
  bty = "n",
  cex = 1.75
)



# Extract relevant information from lists of lumped comparisons with unsynchronous or unaligned data

sync_size_circular_inconsistent_indices <- unique(sync_size_circular_performance$lumped_comparisons_diff$lumped_inconsistent)
sync_size_circular_results <- vector(mode = "list", length = length(sync_size_circular_inconsistent_indices))
for (i in seq_along(sync_size_circular_inconsistent_indices)) {
  
  inconsistent_subset <- sync_size_circular_performance$lumped_comparisons_diff[sync_size_circular_performance$lumped_comparisons_diff$lumped_inconsistent == sync_size_circular_inconsistent_indices[i], ]
  citation <- unique(inconsistent_subset$Citation)
  plot_sizes <- inconsistent_subset$Plot_sqm
  pixel_sizes <- inconsistent_subset$Post_sqm
  ifelse(length(unique(plot_sizes)) > 1, diff_plot_sizes <- TRUE, diff_plot_sizes <- FALSE)
  ifelse(length(unique(pixel_sizes)) > 1, diff_pixel_sizes <- TRUE, diff_pixel_sizes <- FALSE)
  
  sync_size_circular_results[[i]] <- list(
    citation = citation, 
    plot_sizes = plot_sizes, 
    pixel_sizes = pixel_sizes, 
    diff_plot_sizes = diff_plot_sizes, 
    diff_pixel_sizes = diff_pixel_sizes
    )
}

sync_size_square_results <- vector(mode = "list", length = length(sync_size_square_inconsistent_indices))
for (i in seq_along(sync_size_square_inconsistent_indices)) {
  
  inconsistent_subset <- sync_size_square_performance$lumped_comparisons_diff[sync_size_square_performance$lumped_comparisons_diff$lumped_inconsistent == sync_size_square_inconsistent_indices[i], ]
  citation <- unique(inconsistent_subset$Citation)
  plot_sizes <- inconsistent_subset$Plot_sqm
  pixel_sizes <- inconsistent_subset$Post_sqm
  ifelse(length(unique(plot_sizes)) > 1, diff_plot_sizes <- TRUE, diff_plot_sizes <- FALSE)
  ifelse(length(unique(pixel_sizes)) > 1, diff_pixel_sizes <- TRUE, diff_pixel_sizes <- FALSE)
  
  sync_size_square_results[[i]] <- list(
    citation = citation, 
    plot_sizes = plot_sizes, 
    pixel_sizes = pixel_sizes, 
    diff_plot_sizes = diff_plot_sizes, 
    diff_pixel_sizes = diff_pixel_sizes
  )
}

sync_temp_plot_fire_inconsistent_indices <- unique(sync_temp_plot_fire_performance$lumped_comparisons_diff$lumped_inconsistent)
sync_temp_plot_fire_results <- vector(mode = "list", length = length(sync_temp_plot_fire_inconsistent_indices))
for (i in seq_along(sync_temp_plot_fire_inconsistent_indices)) {
  
  inconsistent_subset <- sync_temp_plot_fire_performance$lumped_comparisons_diff[sync_temp_plot_fire_performance$lumped_comparisons_diff$lumped_inconsistent == sync_temp_plot_fire_inconsistent_indices[i], ]
  citation <- unique(inconsistent_subset$Citation)
  plot_months <- inconsistent_subset$Plots_month
  fire_months <- inconsistent_subset$Fire_start_month
  ifelse(length(unique(plot_months)) > 1, diff_plot_months <- TRUE, diff_plot_months <- FALSE)
  ifelse(length(unique(fire_months)) > 1, diff_fire_months <- TRUE, diff_fire_months <- FALSE)
  delay <- plot_months - fire_months
  
  sync_temp_plot_fire_results[[i]] <- list(
    citation = citation, 
    plot_months = plot_months, 
    fire_months = fire_months, 
    diff_plot_months = diff_plot_months, 
    diff_fire_months = diff_fire_months,
    delay = delay
  )
}

sync_temp_rs_plot_inconsistent_indices <- unique(sync_temp_rs_plot_performance$lumped_comparisons_diff$lumped_inconsistent)
sync_temp_rs_plot_results <- vector(mode = "list", length = length(sync_temp_rs_plot_inconsistent_indices))
for (i in seq_along(sync_temp_rs_plot_inconsistent_indices)) {
  
  inconsistent_subset <- sync_temp_rs_plot_performance$lumped_comparisons_diff[sync_temp_rs_plot_performance$lumped_comparisons_diff$lumped_inconsistent == sync_temp_rs_plot_inconsistent_indices[i], ]
  citation <- unique(inconsistent_subset$Citation)
  imagery_months <- inconsistent_subset$Postfire_month
  plot_months <- inconsistent_subset$Plots_month
  ifelse(length(unique(imagery_months)) > 1, diff_imagery_months <- TRUE, diff_imagery_months <- FALSE)
  ifelse(length(unique(plot_months)) > 1, diff_plot_months <- TRUE, diff_plot_months <- FALSE)
  delay <- imagery_months - plot_months
  
  sync_temp_rs_plot_results[[i]] <- list(
    citation = citation, 
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
  sync_size_circular_summary_table$cv_plot_size <- round(sync_size_circular_summary_table$sd_plot_size / sync_size_circular_summary_table$mean_plot_size * 100, 1)
  
  sync_size_circular_summary_table <- cbind(
    sync_size_circular_summary_table,
    min_pixel_size = sapply(sync_size_circular_results, function(x) min(x$pixel_sizes)),
    max_pixel_size = sapply(sync_size_circular_results, function(x) max(x$pixel_sizes)),
    mean_pixel_size = round(sapply(sync_size_circular_results, function(x) mean(x$pixel_sizes)), 1),
    sd_pixel_size = round(sapply(sync_size_circular_results, function(x) sd(x$pixel_sizes)), 1)
    )
  sync_size_circular_summary_table$cv_pixel_size <- round(sync_size_circular_summary_table$sd_pixel_size / sync_size_circular_summary_table$mean_pixel_size * 100, 1)
  
  
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
  sync_size_square_summary_table$cv_plot_size <- round(sync_size_square_summary_table$sd_plot_size / sync_size_square_summary_table$mean_plot_size * 100, 1)
  
  
  sync_size_square_summary_table <- cbind(
    sync_size_square_summary_table,
    min_pixel_size = sapply(sync_size_square_results, function(x) min(x$pixel_sizes)),
    max_pixel_size = sapply(sync_size_square_results, function(x) max(x$pixel_sizes)),
    mean_pixel_size = round(sapply(sync_size_square_results, function(x) mean(x$pixel_sizes)), 1),
    sd_pixel_size = round(sapply(sync_size_square_results, function(x) sd(x$pixel_sizes)), 1)
  )
  sync_size_square_summary_table$cv_pixel_size <- round(sync_size_square_summary_table$sd_pixel_size / sync_size_square_summary_table$mean_pixel_size * 100, 1)
  
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
  sync_temp_plot_fire_summary_table$cv_plot_months <- round(sync_temp_plot_fire_summary_table$sd_plot_months / sync_temp_plot_fire_summary_table$mean_plot_months * 100, 1)
  
  sync_temp_plot_fire_summary_table <- cbind(
    sync_temp_plot_fire_summary_table,
    min_fire_months = sapply(sync_temp_plot_fire_results, function(x) min(x$fire_months)),
    max_fire_months = sapply(sync_temp_plot_fire_results, function(x) max(x$fire_months)),
    mean_fire_months = round(sapply(sync_temp_plot_fire_results, function(x) mean(x$fire_months)), 1),
    sd_fire_months = round(sapply(sync_temp_plot_fire_results, function(x) sd(x$fire_months)), 1)
  )
  sync_temp_plot_fire_summary_table$cv_fire_months <- round(sync_temp_plot_fire_summary_table$sd_fire_months / sync_temp_plot_fire_summary_table$mean_fire_months * 100, 1)
  
  sync_temp_plot_fire_summary_table <- cbind(
    sync_temp_plot_fire_summary_table,
    min_delay = sapply(sync_temp_plot_fire_results, function(x) min(x$delay)),
    max_delay = sapply(sync_temp_plot_fire_results, function(x) max(x$delay)),
    mean_delay = round(sapply(sync_temp_plot_fire_results, function(x) mean(x$delay)), 1),
    sd_delay = round(sapply(sync_temp_plot_fire_results, function(x) sd(x$delay)), 1)
  )
  sync_temp_plot_fire_summary_table$cv_delay <- round(sync_temp_plot_fire_summary_table$sd_delay / sync_temp_plot_fire_summary_table$mean_delay * 100, 1)
  
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
  sync_temp_rs_plot_summary_table$cv_imagery_month <- round(sync_temp_rs_plot_summary_table$sd_imagery_month / sync_temp_rs_plot_summary_table$mean_imagery_month * 100, 1)
  
  sync_temp_rs_plot_summary_table <- cbind(
    sync_temp_rs_plot_summary_table,
    min_plot_months = sapply(sync_temp_rs_plot_results, function(x) min(x$plot_months)),
    max_plot_months = sapply(sync_temp_rs_plot_results, function(x) max(x$plot_months)),
    mean_plot_months = round(sapply(sync_temp_rs_plot_results, function(x) mean(x$plot_months)), 1),
    sd_plot_months = round(sapply(sync_temp_rs_plot_results, function(x) sd(x$plot_months)), 1)
  )
  sync_temp_rs_plot_summary_table$cv_plot_months <- round(sync_temp_rs_plot_summary_table$sd_plot_months / sync_temp_rs_plot_summary_table$mean_plot_months * 100, 1)
  
  sync_temp_rs_plot_summary_table <- cbind(
    sync_temp_rs_plot_summary_table,
    min_delay = sapply(sync_temp_rs_plot_results, function(x) min(x$delay)),
    max_delay = sapply(sync_temp_rs_plot_results, function(x) max(x$delay)),
    mean_delay = round(sapply(sync_temp_rs_plot_results, function(x) mean(x$delay)), 1),
    sd_delay = round(sapply(sync_temp_rs_plot_results, function(x) sd(x$delay)), 1)
  )
  sync_temp_rs_plot_summary_table$cv_delay <- round(sync_temp_rs_plot_summary_table$sd_delay / sync_temp_rs_plot_summary_table$mean_delay * 100, 1)
  
}




