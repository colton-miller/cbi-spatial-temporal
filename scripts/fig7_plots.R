library(fields)
library(scatterplot3d)
library(png)
library(grid)

# Import function for cleaning up spatial and temporal comparisons of R2 values
source(file.path(repo_dir, "scripts", "clean_R2_metrics.R"))

calc_plot_dims <- function(w, h, orig_res, target_res) {
  scaling_factor <- target_res / orig_res
  new_width <- w * scaling_factor
  new_height <- h * scaling_factor
  return(list(w = new_width, h = new_height, r = target_res))
}

# Define metrics that need to be squared to align with R2 values
metrics_to_square <- c(
  "Pearson correlation coefficeint r",
  "r",
  "|r|",
  "R"
)

# Drop these specific comparisons because results for the individual fires were
# note reported - only results at the "vegetation type" level within each fire
labels_to_drop <- c(
  "dNBR woodlands: 0.70; RdNBR sagebrush",
  "dNBR woodlands: 0.69; dNBR sagebrush",
  "dNBR woodlands: 0.53; RdNBR sagebrush",
  "dNBRwet: 0.64; dNBRdamp: 0.65; dNBRforby",
  "RadarBurnRatiowet: 0.30; RadarBurnRatiodamp: 0.39; RadarBurnRatioforby",
  "RadarBurnRatioRFDI wet: 0.44; RadarBurnRatioHH damp: 0.47; RadarBurnRatioHH forby: 0.44 (wet conditions)",
  "RadarBurnRatioHV wet: 0.62; RadarBurnRatioHV damp: 0.42; RadarBurnRatioHV forby: 0.48 (dry conditions)"
)

# Apply function to clean up spatial and temporal comparisons of R2 values
# to circular and square spatial alignment, RS/plot and plot/fire temporal
# synchrony
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



# Subset prior outputs so that across each "comparison" (using Comparison_idx),
# there is the same ASI, same spatial alignment, same temporal synchrony

# Circular set
df1_subset <- sync_size_circular_performance$comparisons_same
df2_subset <- sync_temp_plot_fire_performance$comparisons_same
df3_subset <- sync_temp_rs_plot_performance$comparisons_same

df1_subset$source <- 1
df2_subset$source <- 2
df3_subset$source <- 3

# Merge df1 and df2 on the first 10 columns (i.e., comparison columns)
merged_12 <- merge(df1_subset, df2_subset, by = "Comparison_idx")

# Merge the result with df3 on the same columns
merged_all_circular <- merge(merged_12, df3_subset, by = "Comparison_idx")


# Square set
df1_subset <- sync_size_square_performance$comparisons_same
df2_subset <- sync_temp_plot_fire_performance$comparisons_same
df3_subset <- sync_temp_rs_plot_performance$comparisons_same

df1_subset$source <- 1
df2_subset$source <- 2
df3_subset$source <- 3

# Merge df1 and df2 on the first 10 columns (i.e., comparison columns)
merged_12 <- merge(df1_subset, df2_subset, by = "Comparison_idx")

# Merge the result with df3 on the same columns
merged_all_square <- merge(merged_12, df3_subset, by = "Comparison_idx")

# Merge the circular and square comparison sets together
# 1. Identify missing columns
missing_cols_df1 <- setdiff(names(merged_all_square), names(merged_all_circular))
missing_cols_df2 <- setdiff(names(merged_all_circular), names(merged_all_square))

# 2. Add missing columns with NA to each dataframe
for (col in missing_cols_df1) {
  merged_all_circular[[col]] <- NA
}

for (col in missing_cols_df2) {
  merged_all_square[[col]] <- NA
}

# 3. Ensure the columns are in the same order
merged_all_circular <- merged_all_circular[, names(merged_all_square)]  # Reorder df1 columns to match df2

# 4.rbind them
combined_df <- rbind(merged_all_circular, merged_all_square)

# 5. Assign single ASI value depending on whether comparison was circular or square plots 
combined_df$asi_abs <- ifelse(!is.na(combined_df$circ_asi_abs), combined_df$circ_asi_abs, combined_df$square_asi_abs)

# 6. Drop any rows that do not have an R2 value
combined_df <- combined_df[complete.cases(combined_df$metric_value), ]



## 3d plot

# Define a fixed color palette from light to dark blue
color_palette <- colorRampPalette(c("lightblue", "darkblue"))

# Manually map the metric values to the color scale
combined_df$color <- color_palette(100)[
  as.integer(pmin(pmax(combined_df$metric_value, 0), 1) * 99) + 1
]

# Define color of point outline based on whether metric is R2 or R2adj
combined_df$outline <- "black"
combined_df[combined_df$Model_metric == "R2adj", "outline"] <- "red"

# Write out 3d plot
plot_dims <- calc_plot_dims(w = 800, h = 800, orig_res = 72, target_res = 300)
png(
  filename = "/Users/cwm/Desktop/scatterplot3d.png", 
  width = plot_dims$w, 
  height = plot_dims$h, 
  res = plot_dims$r
)

scatterplot3d(
  x = combined_df$asi_abs, 
  y = combined_df$Postfire_month - combined_df$Plots_month.x, 
  z = combined_df$Plots_month.x - combined_df$Fire_start_month, 
  color = combined_df$outline, 
  bg = combined_df$color,
  pch = 21,
  xlab = "Spatial alignment: Assumption Strength Index (ASI)|",
  ylab = "Temporal synchrony: \n Remotely sensed data month - field plots month",
  zlab = "Temporal synchrony: \n Field plots month - fire start month",
  type = "h",
  cex.symbols = combined_df$n_fires,
  font.lab = 2,
  box = FALSE,
  angle = 45,
  mar = c(4, 4, 0, 2),
  cex.axis = 1.25,
  cex.lab = 1.25,
  )

# Add the size legend for the point sizes (based on n_fires)
legend(x = 6, y = 7, 
       legend = c("1", "5", "10"), 
       pch = 16, 
       col = "#3E4EAC",
       pt.cex = c(1, 3, 5), 
       y.intersp = c(1, 1, 2.25),
       x.intersp = c(0.75, 1, 1.5),
       title = "Number of fires", 
       bty = "n", 
       border = "white"
)

# Add the size legend for the metric type (based on R2)
legend(
  x = 7.25, y = 7,
  legend = c(expression(R^2), expression(R[adj]^2)),
  pch = 21,
  col = c("black", "red"), 
  pt.bg = "#3E4EAC",
  pt.cex = 3,
  lwd = 1.5,
  y.intersp = 1.5,
  x.intersp = 1,
  title = "Metric",
  bty = "n",
  border = "white"
)

# Add title to the plot
mtext(paste("Spatial alignment and temporal synchrony for comparisons that
  have consistent values among all fires\nN =", nrow(combined_df)), side = 3, line = -5, font = 2, cex = 1.25)

dev.off()

# Write out new plot for the color bar legend
plot_dims <- calc_plot_dims(w = 800, h = 800, orig_res = 72, target_res = 300)
png(
  filename = "/Users/cwm/Desktop/scatterplot3d_R2_legend.png", 
  width = plot_dims$w, 
  height = plot_dims$h, 
  res = plot_dims$r
)

# Create empty plot
plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "", ann = FALSE)

# Add a color bar with a bold "R²" title
image.plot(
  zlim = c(0, 1), 
  col = color_palette(100), 
  legend.only = TRUE, 
  horizontal = FALSE, 
  add = TRUE, 
  legend.width = 2, 
  legend.shrink = 0.15, 
  legend.mar = 4,
  legend.args = list(
    text = expression(R[(adj)]^"2"),
    side = 3, 
    line = 1, 
    cex = 2
    ),
  axis.args = list(
    at = seq(0, 1, by = 0.5),
    cex.axis = 1.5
  )
)

dev.off()



# Read in 3d plot and color bar legend and combine into single plot
plot_path <- "/Users/cwm/Desktop/scatterplot3d.png"
legend_path <- "/Users/cwm/Desktop/scatterplot3d_R2_legend.png"
plot3d <- readPNG(plot_path)
legend <- readPNG(legend_path)

# Convert image to grayscale (for simplicity, assuming white is [1,1,1] in RGB)
# You may need to adjust the threshold depending on your image
gray_img <- apply(legend, c(1, 2), function(x) sum(x) / 3)  # Convert to grayscale

# Identify non-white pixels (assumed white is [1, 1, 1] in RGB or 1 in grayscale)
non_white_pixels <- gray_img < 1  # Identifies pixels that are not white (0 < pixel value < 1)

# Find the bounding box of non-white pixels
non_white_rows <- which(apply(non_white_pixels, 1, any))  # Find rows with non-white pixels
non_white_cols <- which(apply(non_white_pixels, 2, any))  # Find columns with non-white pixels

# Get the bounding box coordinates
bounding_box <- c(min(non_white_rows), max(non_white_rows), min(non_white_cols), max(non_white_cols))

# Define the buffer value
buffer <- 5

# Increase the bounding box size by the buffer value
expanded_bounding_box <- c(
  max(1, bounding_box[1] - buffer),  # Subtract buffer from the top, ensure it doesn't go below 1
  min(nrow(legend), bounding_box[2] + buffer),  # Add buffer to the bottom, ensure it doesn't exceed image height
  max(1, bounding_box[3] - buffer),  # Subtract buffer from the left, ensure it doesn't go below 1
  min(ncol(legend), bounding_box[4] + buffer)  # Add buffer to the right, ensure it doesn't exceed image width
)

# Crop image by bounding box
cropped_img <- legend[expanded_bounding_box[1]:expanded_bounding_box[2], expanded_bounding_box[3]:expanded_bounding_box[4], ]

# Calculate the aspect ratio of the cropped image
cropped_width <- ncol(cropped_img)
cropped_height <- nrow(cropped_img)
aspect_ratio <- cropped_width / cropped_height

# Define a desired width for the overlay
desired_width <- 0.05  # For example, 20% of the plot's width

# Calculate the height based on the aspect ratio
desired_height <- desired_width / aspect_ratio

# Write out combined plot w/ color bar added
plot_dims <- calc_plot_dims(w = 800, h = 800, orig_res = 72, target_res = 300)
png(
  filename = "/Users/cwm/Desktop/scatterplot3d_R2_legend_combined.png", 
  width = plot_dims$w, 
  height = plot_dims$h, 
  res = plot_dims$r
)
grid.raster(plot3d)
grid.raster(cropped_img, x = 0.925, y = 0.8025, width = desired_width, height = desired_height)
dev.off()





# Extract relevant information from lists of lumped comparisons with unsynchronous or unaligned data
sync_size_circular_inconsistent_indices <- unique(sync_size_circular_performance$lumped_comparisons_diff$Comparison_idx)
sync_size_circular_results <- vector(mode = "list", length = length(sync_size_circular_inconsistent_indices))
for (i in seq_along(sync_size_circular_inconsistent_indices)) {
  
  inconsistent_subset <- sync_size_circular_performance$lumped_comparisons_diff[sync_size_circular_performance$lumped_comparisons_diff$Comparison_idx == sync_size_circular_inconsistent_indices[i], ]
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

sync_size_square_inconsistent_indices <- unique(sync_size_square_performance$lumped_comparisons_diff$Comparison_idx)
sync_size_square_results <- vector(mode = "list", length = length(sync_size_square_inconsistent_indices))
for (i in seq_along(sync_size_square_inconsistent_indices)) {
  
  inconsistent_subset <- sync_size_square_performance$lumped_comparisons_diff[sync_size_square_performance$lumped_comparisons_diff$Comparison_idx == sync_size_square_inconsistent_indices[i], ]
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

sync_temp_plot_fire_inconsistent_indices <- unique(sync_temp_plot_fire_performance$lumped_comparisons_diff$Comparison_idx)
sync_temp_plot_fire_results <- vector(mode = "list", length = length(sync_temp_plot_fire_inconsistent_indices))
for (i in seq_along(sync_temp_plot_fire_inconsistent_indices)) {
  
  inconsistent_subset <- sync_temp_plot_fire_performance$lumped_comparisons_diff[sync_temp_plot_fire_performance$lumped_comparisons_diff$Comparison_idx == sync_temp_plot_fire_inconsistent_indices[i], ]
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

sync_temp_rs_plot_inconsistent_indices <- unique(sync_temp_rs_plot_performance$lumped_comparisons_diff$Comparison_idx)
sync_temp_rs_plot_results <- vector(mode = "list", length = length(sync_temp_rs_plot_inconsistent_indices))
for (i in seq_along(sync_temp_rs_plot_inconsistent_indices)) {
  
  inconsistent_subset <- sync_temp_rs_plot_performance$lumped_comparisons_diff[sync_temp_rs_plot_performance$lumped_comparisons_diff$Comparison_idx == sync_temp_rs_plot_inconsistent_indices[i], ]
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
  sync_size_circular_summary_table$cv_plot_size <- round(sync_size_circular_summary_table$sd_plot_size / sync_size_circular_summary_table$mean_plot_size * 100, 1)
  
  sync_size_circular_summary_table <- cbind(
    sync_size_circular_summary_table,
    min_pixel_size = sapply(sync_size_circular_results, function(x) min(x$pixel_sizes)),
    max_pixel_size = sapply(sync_size_circular_results, function(x) max(x$pixel_sizes)),
    mean_pixel_size = round(sapply(sync_size_circular_results, function(x) mean(x$pixel_sizes)), 1),
    sd_pixel_size = round(sapply(sync_size_circular_results, function(x) sd(x$pixel_sizes)), 1)
    )
  sync_size_circular_summary_table$cv_pixel_size <- round(sync_size_circular_summary_table$sd_pixel_size / sync_size_circular_summary_table$mean_pixel_size * 100, 1)
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
  sync_size_square_summary_table$cv_plot_size <- round(sync_size_square_summary_table$sd_plot_size / sync_size_square_summary_table$mean_plot_size * 100, 1)
  
  
  sync_size_square_summary_table <- cbind(
    sync_size_square_summary_table,
    min_pixel_size = sapply(sync_size_square_results, function(x) min(x$pixel_sizes)),
    max_pixel_size = sapply(sync_size_square_results, function(x) max(x$pixel_sizes)),
    mean_pixel_size = round(sapply(sync_size_square_results, function(x) mean(x$pixel_sizes)), 1),
    sd_pixel_size = round(sapply(sync_size_square_results, function(x) sd(x$pixel_sizes)), 1)
  )
  sync_size_square_summary_table$cv_pixel_size <- round(sync_size_square_summary_table$sd_pixel_size / sync_size_square_summary_table$mean_pixel_size * 100, 1)
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
  sync_temp_rs_plot_summary_table$comparison_idx <- sapply(sync_temp_rs_plot_results, function(x) x$comparison_idx)
}




