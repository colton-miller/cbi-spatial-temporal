################################################################################
# Load Libraries
################################################################################

library(ggplot2)
library(ggpubr)
library(grid)
library(jsonlite)
library(this.path)

################################################################################
# Setup and Directories
################################################################################

# Define paths and set working directory
script_path <- this.path()
repo_dir <- dirname(script_path) |> dirname()
setwd(repo_dir) # Set directory to save outputs in "plots" and "tables" subfolders

################################################################################
# Define Functions
################################################################################

# Load custom functions for calculations and CSV loading
source(file.path("scripts", "functions", "calc_plot_dims.R"))
source(file.path("scripts", "functions", "load_csvs.R"))

################################################################################
# Load Data
################################################################################

# Load intermediate datasets
load_csvs("intermediate_data")

# Load additional comparisons data
comparisons <- read.csv(
  file.path(repo_dir, "data", "comparisons_results.csv")
)

################################################################################
# Generate Plots
################################################################################

### Figure 2: Assumptions Definition Plots ###
plot_dims <- calc_plot_dims(w = 1000, h = 500, orig_res = 72, target_res = 300)
png(
  filename = "plots/MillerFig2.png", 
  width = plot_dims$w, 
  height = plot_dims$h, 
  res = plot_dims$r
)
source(file.path(repo_dir, "scripts", "figures", "fig2_plots.R"))
dev.off()

### Figure 3: Circular Field Plots Spatial Synchrony ###
plot_dims <- calc_plot_dims(w = 800, h = 800, orig_res = 72, target_res = 300)
png(
  filename = "plots/MillerFig3.png", 
  width = plot_dims$w, 
  height = plot_dims$h, 
  res = plot_dims$r
)
source(file.path(repo_dir, "scripts", "figures", "fig3_plots.R"))
dev.off()

### Figure 4: Square Field Plots Spatial Synchrony ###
plot_dims <- calc_plot_dims(w = 800, h = 533, orig_res = 72, target_res = 300)
png(
  filename = "plots/MillerFig4.png", 
  width = plot_dims$w, 
  height = plot_dims$h, 
  res = plot_dims$r
)
source(file.path(repo_dir, "scripts", "figures", "fig4_plots.R"))
dev.off()

### Figure 5: ASI Boxplots ###
plot_dims <- calc_plot_dims(w = 1400, h = 1200, orig_res = 72, target_res = 300)
png(
  filename = "plots/MillerFig5.png", 
  width = plot_dims$w, 
  height = plot_dims$h, 
  res = plot_dims$r
)
source(file.path(repo_dir, "scripts", "figures", "fig5_plots.R"))
ggarrange(
  annotate_figure(
    plot_a, 
    fig.lab = "a) Circular plots", 
    fig.lab.size = 20, 
    fig.lab.pos = c("top.left"), 
    fig.lab.face = "bold"
  ),
  NULL,
  annotate_figure(
    plot_b, 
    fig.lab = "b) Square plots", 
    fig.lab.size = 20, 
    fig.lab.pos = c("top.left"), 
    fig.lab.face = "bold"
  ),
  ncol = 1, 
  heights = c(1, 0.05, 1)
)
dev.off()

### Figure 6: Temporal Synchrony Plots ###
plot_dims <- calc_plot_dims(w = 1100, h = 1100, orig_res = 72, target_res = 300)
png(
  filename = "plots/MillerFig6.png", 
  width = plot_dims$w, 
  height = plot_dims$h, 
  res = plot_dims$r
)
source(file.path(repo_dir, "scripts", "figures", "fig6_plots.R"))
dev.off()


### Figure 7: Synchrony and Alignment vs. R² ###
plot_dims <- calc_plot_dims(w = 1000, h = 500, orig_res = 72, target_res = 300)
png(
  filename = "plots/MillerFig7.png", 
  width = plot_dims$w, 
  height = plot_dims$h, 
  res = plot_dims$r
)
source(file.path(repo_dir, "scripts", "figures", "fig7_plots.R"))

ggarrange(effect_plots_fire,
          effect_rs_plots, 
          ncol = 2, align = "h",
          labels = c("a) Timing between field plots and fire ignition", "b) Timing between remotely sensed data and \n      field plots"), # Add panel labels
          label.x = 0.08,          # Adjust horizontal position of labels
          label.y = 0.98, 
          hjust = -0.1,
          vjust = 1
)

dev.off()

plot_dims <- calc_plot_dims(w = 600, h = 600, orig_res = 72, target_res = 300)
png(
  filename = "plots/MillerS2.png", 
  width = plot_dims$w, 
  height = plot_dims$h, 
  res = plot_dims$r
)
par(mfrow = c(2, 2))
plot(model_step, pch = 16, col = rgb(0, 0, 0, 0.5), bty = "n")
dev.off()

################################################################################
# Save Tables
################################################################################
# write.csv(plot_delay_after_fire, file = "tables/imagery_delay_after_plots.csv", row.names = FALSE)
# write.csv(imagery_delay_after_plots, file = "tables/imagery_delay_after_plots.csv", row.names = FALSE)

# Write summaries and tables to CSV (produced in fig 7 script)

write.csv(
  number_misaligned_comparisons,
  file = file.path("tables", "results_model_perf_dissim_number_misaligned_n_comparisons.csv"),
  row.names = FALSE
)

write.csv(
  aligned_model_set_means,
  file = file.path("tables", "results_model_perf_sim_model_set_means.csv"),
  row.names = FALSE
)

write.csv(
  sync_size_circular_summary_table, 
  file = file.path("tables", "tab_dissim_sync_size_circular_summary_table.csv"), 
  row.names = FALSE
)

if (exists("sync_size_square_summary_table")) {
  write.csv(
    sync_size_square_summary_table, 
    file = file.path("tables", "tab_dissim_sync_size_square_summary_table.csv"), 
    row.names = FALSE
  )
}

write.csv(
  sync_temp_plot_fire_summary_table, 
  file = file.path("tables", "tab_dissim_sync_temp_plot_fire_summary_table.csv"), 
  row.names = FALSE
)
write.csv(
  sync_temp_rs_plot_summary_table, 
  file = file.path("tables", "tab_dssim_sync_temp_rs_plot_summary_table.csv"), 
  row.names = FALSE
)

write.csv(
  circular_all_three, 
  file = file.path("tables", "res_dissim_circular_all_three.csv"), 
  row.names = FALSE
)

if (exists("square_all_three")) {
  write.csv(
    square_all_three, 
    file = file.path("tables", "res_dissim_square_all_three.csv"), 
    row.names = FALSE
  )
}

write.csv(
  model_step_coefs, 
  file = file.path("tables", "res_model_step_coefs_per12mo.csv"), 
  row.names = FALSE
)

# Extract key summary components, including p-values
summary_list <- list(
  coefficients = as.data.frame(summary(model_step)$coefficients), # Includes p-values
  r_squared = summary(model_step)$r.squared,
  adj_r_squared = summary(model_step)$adj.r.squared,
  f_statistic = summary(model_step)$fstatistic
)

# Save to JSON
write_json(
  summary_list, 
  file.path("tables", "tab_model_results_alignment_synchrony.json"), 
  pretty = TRUE
  )


################################################################################
# Run table script and save outputs
################################################################################

source(file.path(repo_dir, "scripts", "tables", "tables.R"))

write.csv(
  unique_studies, 
  file = file.path("tables", "sup_tab_study_selection_unique_studies.csv"), 
  row.names = FALSE
)

write.csv(
  n_unique_studies, 
  file = file.path("tables", "methods_study_selection_n_unique_studies.csv"), 
  row.names = FALSE
)
write.csv(
  circ_square_studies, 
  file = file.path("tables", "methods_spatial_circ_square_plots_studies.csv"), 
  row.names = FALSE
)
write.csv(
  smoothing_type_by_shape_both, 
  file = file.path("tables", "tab_extract_methods_by_field_plots.csv"), 
  row.names = FALSE
)
write.csv(
  image_type_by_shape, 
  file = file.path("tables", "tab_image_type_by_shape.csv"), 
  row.names = TRUE
)
write.csv(
  n_sensors_used, 
  file = file.path("tables", "results_overview_n_sensors_used.csv"), 
  row.names = TRUE
)
write.csv(
  number_comparisons_by_type, 
  file = file.path("tables", "tab_number_comparisons_by_type.csv")
)
write.csv(
  entry_summary, 
  file = file.path("tables", "tab_plot_data_collection_time_periods.csv"), 
  row.names = FALSE
)
write.csv(
  plot_range, 
  file = file.path("tables", "methods_temporal_multi_month_field_data_range_average.csv"), 
  row.names = FALSE
)
write.csv(
  mean_plot_size, 
  file = file.path("tables", "results_spatial_avg_field_plot_size_ha.csv"), 
  row.names = FALSE
)
write.csv(
  plot_size_by_shape, 
  file = file.path("tables", "results_spatial_most_common_plot_size_by_shape.csv"), 
  row.names = FALSE
)
write.csv(
  circular_assumption_freq, 
  file = file.path("tables", "results_spatial_circular_assumption_type_freq.csv"), 
  row.names = FALSE
)
write.csv(
  square_assumption_freq, 
  file = file.path("tables", "results_spatial_square_assumption_type_freq.csv"), 
  row.names = FALSE
)
write.csv(
  circular_aligned_extraction_methods, 
  file = file.path("tables", "results_spatial_circular_aligned_extraction_methods.csv"), 
  row.names = FALSE
)
write.csv(
  square_aligned_extraction_methods, 
  file = file.path("tables", "results_spatial_square_aligned_extraction_methods.csv"), 
  row.names = FALSE
)
write.csv(
  extract_methods_by_shape, 
  file = file.path("tables", "tab_extract_methods_by_shape.csv"), 
  row.names = FALSE
)
write.csv(
  field_plot_after_fire_timing_df, 
  file = file.path("tables", "results_field_plot_after_fire_timing_range_average.csv"), 
  row.names = FALSE
)
write.csv(
  field_plot_after_imagery_timing_df, 
  file = file.path("tables", "results_field_plot_after_imagery_timing_range_average.csv"), 
  row.names = FALSE
)
write.csv(
  within_sync_ttest,
  file = file.path("tables", "tab_within_sync_ttest.csv"),
  row.names = FALSE
  )
write.csv(
  within_sync_wilcox,
  file = file.path("tables", "tab_within_sync_wilcox.csv"),
  row.names = FALSE
)
write.csv(
  within_sync_tab,
  file = file.path("tables", "tab_within_sync_results.csv"),
  row.names = FALSE
)


