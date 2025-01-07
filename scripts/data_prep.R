library(this.path)


##### Data Loading #####

# read in data
script_path <- this.path()
repo_dir <- dirname(script_path) |> dirname()
comparisons <- read.csv(
  file.path(
    repo_dir,
    "data", 
    "comparisons_results.csv"
  )
)

setwd(repo_dir) # folder to save outputs named "plots" in this directory

##### Define functions #####
source(file.path("functions", "calc_plot_dims.R"))
source(file.path("functions", "scale_months.R"))
source(file.path("functions", "calc_asi.R"))

##### Data Preprocessing #####

### create dataframes for analysis of:
# synchrony between field plot timing and post-fire RS data

# subset to relevant fields
fields_to_keep <- c("Citation",
                    "Fire_name", 
                    "Fire_start_month", 
                    "Fire_start_year",
                    "Plots_month_avg", 
                    "Plots_year",
                    "Postfire_month_avg", 
                    "Postfire_year",
                    "Highest_model",
                    "Model_metric",
                    "Model_notes",
                    "Comparison_idx"
)

sync <- comparisons[, fields_to_keep]

# convert chr fields to integer 
# note: introduces NA's for some obs that were not specified

# List of columns to convert to integers
cols_to_convert <- c(
  "Fire_start_month", 
  "Fire_start_year", 
  "Plots_month_avg", 
  "Plots_year", 
  "Postfire_month_avg", 
  "Postfire_year"
)

# Apply as.integer to each column in the list and 
# assign the result back to the corresponding column
sync[cols_to_convert] <- lapply(sync[cols_to_convert], as.integer)

# Rename columns where necessary
sync$Plots_month <- sync$Plots_month_avg
sync$Postfire_month <- sync$Postfire_month_avg



# reorder data frame by index and month
sync <- sync[
  order(
    sync$Citation, 
    sync$Fire_name, 
    sync$Postfire_year, 
    sync$Postfire_month
  ), 
  
]


# subset to complete cases for fire-plot and plot-imagery 
# note: not using same cases across all comparisons - 
# some studies missing only one date and still allowing other comparisons, 
# but one missing date varies across studies
# also, they can be duplicates of the same citation/fire 
# due to different post-fire imagery acquisitions

sync_fire_plot <- sync[, c(
  "Citation",
  "Fire_name",
  "Fire_start_month",
  "Fire_start_year",
  "Plots_month",
  "Plots_year",
  "Highest_model",
  "Model_metric",
  "Model_notes",
  "Comparison_idx"
)
]

sync_plot_imagery <- sync[, c(
  "Citation",
  "Fire_name",
  "Plots_month",
  "Plots_year",
  "Postfire_month",
  "Postfire_year",
  "Highest_model",
  "Model_metric",
  "Model_notes",
  "Comparison_idx"
)
]

sync_fire_imagery <- sync[, c(
  "Citation",
  "Fire_name",
  "Fire_start_month",
  "Fire_start_year",
  "Postfire_month",
  "Postfire_year",
  "Highest_model",
  "Model_metric",
  "Model_notes",
  "Comparison_idx"
)
]

sync_fire_plot <- sync_fire_plot[complete.cases(sync_fire_plot), ]
sync_plot_imagery <- sync_plot_imagery[complete.cases(sync_plot_imagery), ]
sync_fire_imagery <- sync_fire_imagery[complete.cases(sync_fire_imagery), ]


# first year of any data input (fire start, plot collection, image acquisition)
min_yr <- min(
  sync_fire_plot$Fire_start_year, 
  sync_fire_plot$Plots_year,
  sync_plot_imagery$Plots_year, 
  sync_plot_imagery$Postfire_year,
  sync_fire_imagery$Fire_start_year,
  sync_fire_imagery$Postfire_year
)

# normalize months (relative to first year of data input)
cat("first year of data input:", min_yr)

sync_fire_plot$Fire_start_month <- scale_months(
  data = sync_fire_plot, 
  month_col = "Fire_start_month", 
  year_col = "Fire_start_year", 
  min_yr = min_yr
)

sync_fire_plot$Plots_month <- scale_months(
  data = sync_fire_plot, 
  month_col = "Plots_month", 
  year_col = "Plots_year", 
  min_yr = min_yr
)


sync_plot_imagery$Plots_month <- scale_months(
  data = sync_plot_imagery, 
  month_col = "Plots_month", 
  year_col = "Plots_year", 
  min_yr = min_yr
)

sync_plot_imagery$Postfire_month <- scale_months(
  data = sync_plot_imagery, 
  month_col = "Postfire_month", 
  year_col = "Postfire_year", 
  min_yr = min_yr
)

sync_fire_imagery$Fire_start_month <- scale_months(
  data = sync_fire_imagery, 
  month_col = "Fire_start_month", 
  year_col = "Fire_start_year", 
  min_yr = min_yr
)

sync_fire_imagery$Postfire_month <- scale_months(
  data = sync_fire_imagery, 
  month_col = "Postfire_month", 
  year_col = "Postfire_year", 
  min_yr = min_yr
)

### create dataframes for analysis of: 
# syncrhony between field plot size and RS data resolution

# subset to relevant fields
sync_size <- comparisons[, c(
  "Citation",
  "Fire_name",
  "Plot_sqm",
  "Post_sqm",
  "Smoothing",
  "Plot_shape",
  "Postfire_image",
  "Highest_model",
  "Model_metric",
  "Model_notes",
  "Comparison_idx"
)
]

# convert plot and pixel size to numeric 
# note: NA's introduced by not specified fields
sync_size$Plot_sqm <- as.numeric(sync_size$Plot_sqm)
sync_size$Post_sqm <- as.numeric(sync_size$Post_sqm)

# subset to complete cases
sync_size <- sync_size[complete.cases(sync_size), ]

# rename smoothing types (for plot titles)
sync_size$Smoothing[
  sync_size$Smoothing == "Mean values of 175 sample points within each plot"
] = 
  "mean of sample points within \n      plot"

sync_size$Smoothing[
  sync_size$Smoothing == "Mean 900 sample points within each plot"
] = 
  "mean of sample points within \n      plot"

sync_size$Smoothing[
  sync_size$Smoothing == "sampled imagery at 5 points systematically located within each CBI plot and averaged them"
] = 
  "mean of sample points within \n      plot"

sync_size$Smoothing[
  sync_size$Smoothing == "Mean 5 points within plot"
] = 
  "mean of sample points within \n      plot"

sync_size$Smoothing[
  sync_size$Smoothing == "mean of all pixels wihtin 15 m of plot centroid weighting center double"
] = 
  "mean of values of pixels within \n      distance of plot"

sync_size$Smoothing[
  sync_size$Smoothing == "3x3"
] = 
  "focal mean"


### calculate ASI for each comparison
sync_size <- calc_asi(sync_size, post_sqm_col = "Post_sqm", plot_sqm_col = "Plot_sqm", plot_shape_col = "Plot_shape")

# subset to circular and square plots for easier analysis
sync_size_circular <- sync_size_circular[sync_size_circular$Plot_shape == "Circular", ]
sync_size_square <- sync_size_circular[sync_size_circular$Plot_shape == "Square", ]


# write out prepped datasets
write.csv(
  sync_fire_plot,
  file = file.path("intermediate_data", "sync_fire_plot.csv"),
  row.names = FALSE
  )

write.csv(
  sync_plot_imagery,
  file = file.path("intermediate_data", "sync_plot_imagery.csv"),
  row.names = FALSE
)

write.csv(
  sync_size,
  file = file.path("intermediate_data", "sync_size.csv"),
  row.names = FALSE
)

write.csv(
  sync_size_circular,
  file = file.path("intermediate_data", "sync_size_circular.csv"),
  row.names = FALSE
)

write.csv(
  sync_size_square,
  file = file.path("intermediate_data", "sync_size_square.csv"),
  row.names = FALSE
)

