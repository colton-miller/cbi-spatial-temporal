################################################################################
# Load Libraries
################################################################################
library(this.path)

################################################################################
# Data Loading
################################################################################

# Define paths
script_path <- this.path()
repo_dir <- dirname(script_path) |> dirname()

# Read in data
comparisons <- read.csv(
  file.path(repo_dir, "data", "comparisons_results.csv")
)

# Set working directory
setwd(repo_dir)

################################################################################
# Load Functions
################################################################################

# Define paths to custom functions and source them
source(file.path("scripts", "functions", "scale_months.R"))
source(file.path("scripts", "functions", "calc_asi.R"))

################################################################################
# Data Preprocessing
################################################################################

### Create dataframes for synchrony analysis between field plots and RS data ###

# Subset to relevant fields
fields_to_keep <- c(
  "Citation", "Fire_name", "Fire_start_month", "Fire_start_year",
  "Plots_month_avg", "Plots_year", "Postfire_month_avg", "Postfire_year",
  "Highest_model", "Model_metric", "Model_notes", "Comparison_idx"
)
sync <- comparisons[, fields_to_keep]

# Convert character fields to integer (may introduce NAs)
cols_to_convert <- c(
  "Fire_start_month", "Fire_start_year", "Plots_month_avg",
  "Plots_year", "Postfire_month_avg", "Postfire_year"
)
sync[cols_to_convert] <- lapply(sync[cols_to_convert], as.integer)

# Rename columns for clarity
sync$Plots_month <- sync$Plots_month_avg
sync$Postfire_month <- sync$Postfire_month_avg

# Reorder dataframe by index and month
sync <- sync[
  order(sync$Citation, sync$Fire_name, sync$Postfire_year, sync$Postfire_month),
]

# Create subsets for different synchrony comparisons
sync_fire_plot <- sync[, c(
  "Citation", "Fire_name", "Fire_start_month", "Fire_start_year",
  "Plots_month", "Plots_year", "Highest_model", "Model_metric",
  "Model_notes", "Comparison_idx"
)]
sync_plot_imagery <- sync[, c(
  "Citation", "Fire_name", "Plots_month", "Plots_year",
  "Postfire_month", "Postfire_year", "Highest_model", "Model_metric",
  "Model_notes", "Comparison_idx"
)]
sync_fire_imagery <- sync[, c(
  "Citation", "Fire_name", "Fire_start_month", "Fire_start_year",
  "Postfire_month", "Postfire_year", "Highest_model", "Model_metric",
  "Model_notes", "Comparison_idx"
)]

# Subset to complete cases
sync_fire_plot <- sync_fire_plot[complete.cases(sync_fire_plot), ]
sync_plot_imagery <- sync_plot_imagery[complete.cases(sync_plot_imagery), ]
sync_fire_imagery <- sync_fire_imagery[complete.cases(sync_fire_imagery), ]

# Determine the first year of data input
min_yr <- min(
  sync_fire_plot$Fire_start_year, sync_fire_plot$Plots_year,
  sync_plot_imagery$Plots_year, sync_plot_imagery$Postfire_year,
  sync_fire_imagery$Fire_start_year, sync_fire_imagery$Postfire_year
)
cat("First year of data input:", min_yr)
write.csv(
  min_yr,
  file.path("intermediate_data", "min_yr.csv"),
  row.names = FALSE
)

# Normalize months relative to the first year of data input
sync_fire_plot$Fire_start_month <- scale_months(
  data = sync_fire_plot, month_col = "Fire_start_month",
  year_col = "Fire_start_year", min_yr = min_yr
)
sync_fire_plot$Plots_month <- scale_months(
  data = sync_fire_plot, month_col = "Plots_month",
  year_col = "Plots_year", min_yr = min_yr
)
sync_plot_imagery$Plots_month <- scale_months(
  data = sync_plot_imagery, month_col = "Plots_month",
  year_col = "Plots_year", min_yr = min_yr
)
sync_plot_imagery$Postfire_month <- scale_months(
  data = sync_plot_imagery, month_col = "Postfire_month",
  year_col = "Postfire_year", min_yr = min_yr
)
sync_fire_imagery$Fire_start_month <- scale_months(
  data = sync_fire_imagery, month_col = "Fire_start_month",
  year_col = "Fire_start_year", min_yr = min_yr
)
sync_fire_imagery$Postfire_month <- scale_months(
  data = sync_fire_imagery, month_col = "Postfire_month",
  year_col = "Postfire_year", min_yr = min_yr
)

################################################################################
# Synchrony Analysis Between Plot Size and RS Data Resolution
################################################################################

# Subset to relevant fields
sync_size <- comparisons[, c(
  "Citation", "Fire_name", "Plot_sqm", "Post_sqm", "Smoothing",
  "Plot_shape", "Postfire_image", "Highest_model", "Model_metric",
  "Model_notes", "Comparison_idx"
)]

# Convert plot and pixel size to numeric
sync_size$Plot_sqm <- as.numeric(sync_size$Plot_sqm)
sync_size$Post_sqm <- as.numeric(sync_size$Post_sqm)

# Subset to complete cases
sync_size <- sync_size[complete.cases(sync_size), ]

# Rename smoothing types for better plot titles
# Define the mapping of old to new values
smoothing_map <- c(
  "Mean values of 175 sample points within each plot" = "mean of sample points within \n      plot",
  "Mean 900 sample points within each plot" = "mean of sample points within \n      plot",
  "sampled imagery at 5 points systematically located within each CBI plot and averaged them" = "mean of sample points within \n      plot",
  "Mean 5 points within plot" = "mean of sample points within \n      plot",
  "mean of all pixels wihtin 15 m of plot centroid weighting center double" = "mean of values of pixels within \n      distance of plot",
  "3x3" = "focal mean"
)

# Apply the recoding
sync_size$Smoothing <- dplyr::recode(sync_size$Smoothing, !!!smoothing_map)

# Calculate ASI for each comparison
sync_size <- calc_asi(
  data = sync_size, post_sqm_col = "Post_sqm", 
  plot_sqm_col = "Plot_sqm", plot_shape_col = "Plot_shape"
)

# Subset to circular and square plots for easier analysis
sync_size_circular <- sync_size[sync_size$Plot_shape == "Circular", ]
sync_size_square <- sync_size[sync_size$Plot_shape == "Square", ]

################################################################################
# Save Processed Datasets
################################################################################

write.csv(
  sync_fire_plot, 
  file.path("intermediate_data", "sync_fire_plot.csv"), 
  row.names = FALSE
  )
write.csv(
  sync_plot_imagery, 
  file.path("intermediate_data", "sync_plot_imagery.csv"), 
  row.names = FALSE
  )
write.csv(
  sync_size, 
  file.path("intermediate_data", "sync_size.csv"), 
  row.names = FALSE
  )
write.csv(
  sync_size_circular, 
  file.path("intermediate_data", "sync_size_circular.csv"), 
  row.names = FALSE
  )
write.csv(
  sync_size_square, 
  file.path("intermediate_data", "sync_size_square.csv"), 
  row.names = FALSE
  )
