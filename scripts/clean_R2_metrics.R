# Function to clean and process R2 metrics data based on synchrony type
# synchrony_type options: "spatial", "temporal_rs_plot", "temporal_fire_plot"
# - "spatial" checks if there is only 1 plot size and 1 pixel size within each comparison set
# - "temporal_rs_plot" checks if all comparisons in a set have the same delay between post-fire imagery and plot timing
# - "temporal_plot_fire" checks if all comparisons in a set have the same delay between plot timing and fire start

clean_R2_metrics <- function(sync_df, labels_to_drop, metrics_to_square, synchrony_type) {
  
  # Step 1: Split the "highest model" column into two parts
  # Create 'metric_label' by removing numeric values from 'Highest_model' column
  sync_df$metric_label <- sub(":\\s?-?\\d*\\.?\\d*$", "", sync_df$Highest_model)
  
  # Create 'metric_value' by extracting the numeric part of 'Highest_model'
  sync_df$metric_value <- as.numeric(sub(".*:\\s?", "", sync_df$Highest_model))
  
  # Step 2: Subset data to exclude rows with labels in 'labels_to_drop'
  sync_df_clean <- sync_df[!(sync_df$metric_label %in% labels_to_drop), ]
  
  # Step 3: Square the values of specified metrics
  sync_df_clean[sync_df_clean$Model_metric %in% metrics_to_square, ]$metric_value <- 
    sync_df_clean[sync_df_clean$Model_metric %in% metrics_to_square, ]$metric_value ^ 2
  
  # Step 4: Get indices of rows with "lumped" comparisons
  comparison_freq <- as.data.frame(table(sync_df_clean$Comparison_idx))
  lumped_idx <- comparison_freq[comparison_freq$Freq > 1, ]$Var1
  lumped_idx <- as.numeric(as.character(lumped_idx))
  
  # Step 5: Define objects for unlumped and lumped comparisons
  lumped_fires <- sync_df_clean[sync_df_clean$Comparison_idx %in% lumped_idx, ]
  unlumped_fires <- sync_df_clean[!(sync_df_clean$Comparison_idx %in% lumped_idx), ]
  
  # Step 6: Get names of unique citations in lumped comparisons
  unique_lumped_studies <- unique(lumped_fires$Citation)
  
  # Step 7: Initialize objects to store comparisons
  lumped_comparisons_same <- data.frame()  # For "same" synchrony comparisons
  lumped_comparisons_diff <- data.frame()  # For "different" synchrony comparisons
  
  # Step 8: Loop over each unique lumped study and check synchrony type
  for (i in seq_along(unique_lumped_studies)) {
    
    # Get the subset of observations for this study
    study <- unique_lumped_studies[i]
    obs <- lumped_fires[lumped_fires$Citation == study, ]
    unique_idx <- unique(obs$Comparison_idx)
    
    # Loop over each unique idx within the study
    for (j in seq_along(unique_idx)) {
      
      idx <- unique_idx[j]
      obs_lumped <- obs[obs$Comparison_idx == idx, ]
      
      # Check for consistency based on synchrony type
      if (synchrony_type == "spatial") {
        plot_size <- length(unique(obs_lumped$Plot_sqm))
        image_size <- length(unique(obs_lumped$Post_sqm))
        consistent <- plot_size == image_size
      }
      
      if (synchrony_type == "temporal_rs_plot") {
        delay <- length(unique(obs_lumped$Postfire_month - obs_lumped$Plots_month))
        consistent <- delay == 1
      }
      if (synchrony_type == "temporal_plot_fire") {
        delay <- length(unique(obs_lumped$Plots_month - obs_lumped$Fire_start_month))
        consistent <- delay == 1
      }
      
      # Step 9: Store comparisons based on synchrony consistency
      if (consistent) {
        # If synchrony is consistent, keep only one row and add 'n_fires' column
        comparison <- cbind(
          obs_lumped[1, ], 
          n_fires = nrow(obs_lumped)
          )
        lumped_comparisons_same <- rbind(lumped_comparisons_same, comparison)
      } else {
        # If synchrony is inconsistent, add all rows to the "diff" object
        # Add index to track unique instances of asynchronous or unaligned 
        # comparisons
        lumped_comparisons_diff <- rbind(lumped_comparisons_diff, obs_lumped)
        
      }
    }
  }
  
  # Step 10: Add 'n_fires' column to the unlumped comparisons and combine with "same" comparisons
  unlumped_fires <- cbind(
    unlumped_fires, 
    n_fires = 1
    )
  comparisons_same <- rbind(lumped_comparisons_same, unlumped_fires)
  
  # Step 11: Return the processed data as a list
  return(
    list(
      comparisons_same = comparisons_same,  # Comparisons with consistent synchrony
      lumped_comparisons_diff = lumped_comparisons_diff  # Comparisons with inconsistent synchrony
    )
  )
}

# clean_R2_metrics(
#   sync_df = sync_plot_imagery,
#   labels_to_drop = labels_to_drop,
#   metrics_to_square = metrics_to_square,
#   synchrony_type = "temporal_fire_plot"
#   )

