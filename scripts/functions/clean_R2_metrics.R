#' Clean and Process R2 Metrics Data
#'
#' This function processes a data frame of R2 metrics based on specified synchrony types, 
#' dropping certain labels, squaring specified metrics, and organizing comparisons into 
#' consistent ("same") or inconsistent ("different") synchrony groups.
#'
#' @param sync_df A data frame containing the R2 metrics data.
#' @param labels_to_drop A character vector of labels to exclude from the data.
#' @param metrics_to_square A character vector of metric names for which values should be squared.
#' @param synchrony_type A character string specifying the type of synchrony check to apply. 
#'   Options are:
#'   - "spatial": Ensures consistency in plot size and pixel size within a comparison set.
#'   - "temporal_rs_plot": Ensures consistent delay between post-fire imagery and plot timing.
#'   - "temporal_plot_fire": Ensures consistent delay between plot timing and fire start.
#'
#' @return A list with two elements:
#'   - `comparisons_same`: A data frame of comparisons with consistent synchrony.
#'   - `lumped_comparisons_diff`: A data frame of comparisons with inconsistent synchrony.
#'
#' @examples
#' clean_R2_metrics(
#'   sync_df = sync_plot_imagery,
#'   labels_to_drop = c("label1", "label2"),
#'   metrics_to_square = c("MetricA", "MetricB"),
#'   synchrony_type = "temporal_fire_plot"
#' )
#'
#' @export
clean_R2_metrics <- function(sync_df, labels_to_drop, metrics_to_square, synchrony_type) {
  
  # Step 1: Split the "highest model" column into 'metric_label' and 'metric_value'
  sync_df$metric_label <- sub(":\\s?-?\\d*\\.?\\d*$", "", sync_df$Highest_model)
  sync_df$metric_value <- as.numeric(sub(".*:\\s?", "", sync_df$Highest_model))
  
  # Step 2: Exclude rows with labels to drop
  sync_df_clean <- sync_df[!(sync_df$metric_label %in% labels_to_drop), ]
  
  # Step 3: Square values for specified metrics
  sync_df_clean[sync_df_clean$Model_metric %in% metrics_to_square, ]$metric_value <- 
    sync_df_clean[sync_df_clean$Model_metric %in% metrics_to_square, ]$metric_value^2
  
  # Step 4: Identify "lumped" comparisons with more than one row per index
  comparison_freq <- as.data.frame(table(sync_df_clean$Comparison_idx))
  lumped_idx <- as.numeric(as.character(comparison_freq[comparison_freq$Freq > 1, ]$Var1))
  
  # Separate lumped and unlumped comparisons
  lumped_fires <- sync_df_clean[sync_df_clean$Comparison_idx %in% lumped_idx, ]
  unlumped_fires <- sync_df_clean[!(sync_df_clean$Comparison_idx %in% lumped_idx), ]
  
  # Step 5: Initialize result objects for comparisons
  lumped_comparisons_same <- data.frame()  # Consistent synchrony
  lumped_comparisons_diff <- data.frame()  # Inconsistent synchrony
  
  # Step 6: Process each unique lumped study
  unique_lumped_studies <- unique(lumped_fires$Citation)
  for (study in unique_lumped_studies) {
    obs <- lumped_fires[lumped_fires$Citation == study, ]
    unique_idx <- unique(obs$Comparison_idx)
    
    for (idx in unique_idx) {
      obs_lumped <- obs[obs$Comparison_idx == idx, ]
      
      # Check consistency based on synchrony type
      if (synchrony_type == "spatial") {
        plot_size <- length(unique(obs_lumped$Plot_sqm))
        image_size <- length(unique(obs_lumped$Post_sqm))
        consistent <- plot_size == image_size
      } else if (synchrony_type == "temporal_rs_plot") {
        delay <- obs_lumped$Postfire_month - obs_lumped$Plots_month
        consistent <- max(delay) - min(delay) <= 2
      } else if (synchrony_type == "temporal_plot_fire") {
        delay <- obs_lumped$Plots_month - obs_lumped$Fire_start_month
        consistent <- max(delay) - min(delay) <= 2
      } else {
        stop("Invalid synchrony_type specified.")
      }
      
      # Store comparisons based on synchrony consistency
      if (consistent) {
        comparison <- cbind(obs_lumped[1, ], n_fires = nrow(obs_lumped))
        lumped_comparisons_same <- rbind(lumped_comparisons_same, comparison)
      } else {
        lumped_comparisons_diff <- rbind(lumped_comparisons_diff, obs_lumped)
      }
    }
  }
  
  # Step 7: Combine unlumped comparisons with consistent synchrony
  unlumped_fires <- cbind(unlumped_fires, n_fires = 1)
  comparisons_same <- rbind(lumped_comparisons_same, unlumped_fires)
  
  # Step 8: Return results as a list
  return(
    list(
      comparisons_same = comparisons_same,
      lumped_comparisons_diff = lumped_comparisons_diff
    )
  )
}