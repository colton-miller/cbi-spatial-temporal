################################################################################
# Number of Studies Used in Analysis (of 62 possible)
################################################################################

# These are all the data.frames used for analysis from the original csv 
# (subset based on having information needed for plotting or tables)
unique_studies <- c(
  unique(sync_size_circular$Citation), 
  unique(sync_size_square$Citation), 
  unique(sync_fire_plot$Citation), 
  unique(sync_plot_imagery$Citation)
)

unique_studies <- unique(unique_studies)
n_unique_studies <- length(unique_studies)


################################################################################
# Number of Citations by Plot Shape
################################################################################

# Remove duplicates of the same citation & same shape
# wanting to count number of studies that used each shape 
# (where both plot size and post-fire pixel size were available)
sync_size_shape_study <- sync_size[
  !duplicated(paste0(sync_size$Citation, sync_size$Plot_shape)), 
  ]

circ_square_studies <- table(sync_size_shape_study$Plot_shape)
circ_square_studies <- as.data.frame(circ_square_studies)
circ_square_studies <- circ_square_studies[
  circ_square_studies$Var1 %in% c("Circular", "Square"), 
  ]

################################################################################
# Smoothing Type by Shape
################################################################################

# Remove duplicates of the same citation/fire/shape/smoothing
sync_size_smoothing <- sync_size[
  !duplicated(
    paste0(
      sync_size$Citation, 
      sync_size$Fire_name, 
      sync_size$Plot_shape, 
      sync_size$Smoothing
      )
    ),
  ]

# Create smoothing type by shape table
smoothing_type_by_shape <- table(
  sync_size_smoothing$Smoothing, 
  sync_size_smoothing$Plot_shape
  )
smoothing_type_by_shape <- as.data.frame(smoothing_type_by_shape)

# Split by shape and merge
smoothing_type_by_shape_circ <- smoothing_type_by_shape[
  smoothing_type_by_shape$Var2 == "Circular", 
  ]
colnames(smoothing_type_by_shape_circ) <- c("method", "shape", "circular")
smoothing_type_by_shape_circ <- smoothing_type_by_shape_circ[, c("method", "circular")]

smoothing_type_by_shape_square <- smoothing_type_by_shape[
  smoothing_type_by_shape$Var2 == "Square", 
  ]
colnames(smoothing_type_by_shape_square) <- c("method", "shape", "square")
smoothing_type_by_shape_square <- smoothing_type_by_shape_square[, c("method", "square")]

smoothing_type_by_shape_both <- merge(smoothing_type_by_shape_circ,
                                      smoothing_type_by_shape_square,
                                      by = "method")

################################################################################
# Image Type by Shape
################################################################################

# Remove duplicates of the same citation/fire/shape/image type
sync_size_imagery <- sync_size[
  !duplicated(
    paste0(
      sync_size$Citation, 
      sync_size$Fire_name, 
      sync_size$Plot_shape, 
      sync_size$Postfire_image
      )
    ), 
  ]

# Create table of image type by shape
image_type_by_shape <- table(
  sync_size_imagery$Postfire_image, 
  sync_size_imagery$Plot_shape
  )

# Summing all Landsat imagery types into one row
image_type_by_shape <- rbind(
  image_type_by_shape, 
  colSums(
    image_type_by_shape[
      rownames(image_type_by_shape) %in% c(
        "ETM", "ETM+", "Landsat", "OLI", "TM", "TM or ETM+", "TM/ETM+"
        ), 
      ]
    )
  )

rownames(image_type_by_shape)[nrow(image_type_by_shape)] <- "All Landsat"

# Remove original Landsat rows
ind_to_remove <- which(
  rownames(image_type_by_shape) 
  %in% 
    c("ETM", "ETM+", "Landsat", "OLI", "TM", "TM or ETM+", "TM/ETM+")
  )

image_type_by_shape <- image_type_by_shape[-ind_to_remove, ]

# Summing all Sentinel (2, MSI) imagery types into one row
image_type_by_shape <- rbind(
  image_type_by_shape, 
  colSums(
    image_type_by_shape[
      rownames(image_type_by_shape) %in% c(
        "Sentinel 2A", "Sentinel 2 MSI"
      ), 
    ]
  )
)

rownames(image_type_by_shape)[nrow(image_type_by_shape)] <- "Sentinel MSI"

# Remove original Landsat rows
ind_to_remove <- which(
  rownames(image_type_by_shape) 
  %in% 
    c("Sentinel 2A", "Sentinel 2 MSI")
)

image_type_by_shape <- image_type_by_shape[-ind_to_remove, ]

# Keep only circular and square columns and sort by sensor name
image_type_by_shape <- image_type_by_shape[, which(
  colnames(image_type_by_shape) %in% c("Circular", "Square")
  )
  ]

# Summary of sensors
image_type_by_shape <- image_type_by_shape[order(rownames(image_type_by_shape)), ]
total_sensors <- nrow(image_type_by_shape)
nonzero_counts <- colSums(image_type_by_shape != 0)
sensor_summary <- c(total_sensors, as.vector(nonzero_counts))
n_sensors_used <- as.data.frame(t(sensor_summary))
colnames(n_sensors_used) <- c("Total_Sensors", colnames(image_type_by_shape))


################################################################################
# Number of Comparisons by Assessment Type
################################################################################

spatial_circ_summary <- data.frame(
  table(sync_size_circular[!is.na(sync_size_circular$circ_asi_abs), ]$Citation)
  )
colnames(spatial_circ_summary) <- c("Citation", "Frequency")

spatial_square_summary <- data.frame(
  table(sync_size_square[!is.na(sync_size_square$square_asi_abs), ]$Citation)
  )
colnames(spatial_square_summary) <- c("Citation", "Frequency")

temporal_fire_plot_summary <- data.frame(
  table(
    sync_fire_plot[!is.na(sync_fire_plot$Fire_start_month) & !is.na(sync_fire_plot$Plots_month), ]$Citation
    )
  )
colnames(temporal_fire_plot_summary) <- c("Citation", "Frequency")

temporal_plots_rs_summary <- data.frame(
  table(
    sync_plot_imagery[!is.na(sync_plot_imagery$Plots_month) & !is.na(sync_plot_imagery$Postfire_month), ]$Citation
    )
  )
colnames(temporal_plots_rs_summary) <- c("Citation", "Frequency")


aligned_comparisons <- read.csv("tables/number_aligned_comarisons.csv")
misaligned_comparisons <- read.csv("tables/results_model_perf_dissim_number_misaligned_n_comparisons.csv")

number_comparisons_by_type <- data.frame(
  spatial_circular = sum(spatial_circ_summary$Frequency),
  spatial_square = sum(spatial_square_summary$Frequency),
  temporal_fire_plot = sum(temporal_fire_plot_summary$Frequency),
  temporal_plot_rs = sum(temporal_plots_rs_summary$Frequency),
  performance_aligned = aligned_comparisons$x,
  performance_misaligned = misaligned_comparisons$x
)

number_comparisons_by_type <- t(number_comparisons_by_type)
colnames(number_comparisons_by_type) <- "Frequency"


################################################################################
# Field Data Timing For Multi-Month Studies
################################################################################

# Number of unique study-fire combinations reporting field data collection over specified time periods
# also calculate the range in timing for multi-month field campaigns and average


# Check that all entries are accounted for in the spreadsheet
if (
  !length(comparisons$Plots_month) == sum(comparisons$Plots_month == "Not specified") + 
  sum(comparisons$Plots_month == "growing season") + 
  sum(comparisons$Plots_month == "Summer") + 
  sum(grepl("^\\d+$", comparisons$Plots_month)) +
  sum(grepl(",", comparisons$Plots_month))) {
  stop("Unknown data entry in 'Plots_month' column")
}

# Summarize entries by type
entry_summary <- data.frame(
  Not_specified = sum(comparisons$Plots_month == "Not specified"),
  Growing_season = sum(comparisons$Plots_month == "growing season"),
  Summer = sum(comparisons$Plots_month == "Summer"),
  Single_month = sum(grepl("^\\d+$", comparisons$Plots_month)),
  Multiple_months = sum(grepl(",", comparisons$Plots_month))
)


# Extract multiple month entries
plot_month_list <- lapply(
  comparisons[grepl(",", comparisons$Plots_month), "Plots_month"], 
  function(x) as.integer(strsplit(x, ",")[[1]])
)


# Summarize the range over multiple-month entries
plot_range <- sapply(plot_month_list, function(x) max(x)) - sapply(plot_month_list, function(x) min(x))
plot_range <- as.data.frame(t(summary(plot_range)))

plot_range <- plot_range[, -1]
colnames(plot_range) <- c("Statistic", "Value")
plot_range$Value <- round(plot_range$Value, 1)


################################################################################
# Field Data Sizes (Total, Circular, Square)
################################################################################

mean_plot_size_total <- mean(
  sync_size[sync_size$Citation %in% unique_studies & sync_size$Plot_shape != "Not specified", ]$Plot_sqm
  )
mean_plot_size_circular <- mean(
  sync_size[sync_size$Citation %in% unique_studies & sync_size$Plot_shape != "Circular", ]$Plot_sqm
  )
mean_plot_size_square <- mean(
  sync_size[sync_size$Citation %in% unique_studies & sync_size$Plot_shape != "Square", ]$Plot_sqm
  )

mean_plot_size <- c(mean_plot_size_total, mean_plot_size_circular, mean_plot_size_square)
mean_plot_size <- round(mean_plot_size, 1)
mean_plot_size <- as.data.frame(mean_plot_size)
mean_plot_size$Shape <- c("Total", "Circular", "Square")
mean_plot_size$mean_plot_size <-  mean_plot_size$mean_plot_size / 10000

plot_size_by_shape <- table(
  sync_size[sync_size$Citation %in% unique_studies & sync_size$Plot_shape != "Not specified", ]$Plot_sqm, 
  sync_size[sync_size$Citation %in% unique_studies & sync_size$Plot_shape != "Not specified", ]$Plot_shape
  )
plot_size_by_shape <- as.data.frame(plot_size_by_shape)
colnames(plot_size_by_shape) <- c("Plot_sqm", "Plot_shape", "Freq")
plot_size_by_shape <- plot_size_by_shape[
  order(plot_size_by_shape$Plot_shape, plot_size_by_shape$Freq, decreasing = TRUE), 
  ]


################################################################################
# Smoothing Methods by Assumption Type for Each Shape
################################################################################

# Comparisons using different assumption types by shape
circular_assumption_freq <- as.data.frame(table(sync_size_circular$sign))
square_assumption_freq <- as.data.frame(table(sync_size_square$sign))

# Spatially aligned extraction methods (circular)
circular_aligned_extraction_methods <- as.data.frame(
  table(
    sync_size_circular[sync_size_circular$sign == "no assumption", "Smoothing"]
  )
)

circular_aligned_extraction_methods <- circular_aligned_extraction_methods[
  order(circular_aligned_extraction_methods$Freq, decreasing = TRUE), 
  ]

# Spatially aligned extraction methods (square)
square_aligned_extraction_methods <- as.data.frame(
  table(
    sync_size_square[sync_size_square$sign == "no assumption", "Smoothing"]
  )
)

square_aligned_extraction_methods <- square_aligned_extraction_methods[
  order(square_aligned_extraction_methods$Freq, decreasing = TRUE), 
  ]

################################################################################
# Pixel extraction methods by plot shape
################################################################################

extract_methods_by_shape <- table(
  sync_size[sync_size$Citation %in% unique_studies, "Smoothing"], 
  sync_size[sync_size$Citation %in% unique_studies, "Plot_shape"]
  )
extract_methods_by_shape <- as.data.frame(extract_methods_by_shape)
colnames(extract_methods_by_shape) <- c("Method", "Shape", "Freq")
extract_methods_by_shape <- extract_methods_by_shape[
  extract_methods_by_shape$Shape != "Not specified", 
  ]
extract_methods_by_shape <- extract_methods_by_shape[
  order(extract_methods_by_shape$Shape, extract_methods_by_shape$Freq, decreasing = TRUE), 
  ]


################################################################################
# Field Data Timing: Post-Fire
################################################################################

field_plot_after_fire_timing <- sync_fire_plot$Plots_month - sync_fire_plot$Fire_start_month
field_plot_after_fire_timing_summary_stats <- summary(field_plot_after_fire_timing)

# Convert the summary to a data frame
field_plot_after_fire_timing_df <- data.frame(
  Statistic = names(field_plot_after_fire_timing_summary_stats),  
  Value = as.numeric(field_plot_after_fire_timing_summary_stats)
)
field_plot_after_fire_timing_df$Value <- round(field_plot_after_fire_timing_df$Value, 1)


################################################################################
# Field Data Timing: Post-RS Acquisition
################################################################################

field_plot_after_imagery_timing <- sync_plot_imagery$Postfire_month - sync_plot_imagery$Plots_month
field_plot_after_imagery_timing_summary_stats <- summary(field_plot_after_imagery_timing)

# Convert the summary to a data frame
field_plot_after_imagery_timing_df <- data.frame(
  Statistic = names(field_plot_after_imagery_timing_summary_stats),
  Value = as.numeric(field_plot_after_imagery_timing_summary_stats)
)
field_plot_after_imagery_timing_df$Value <- round(field_plot_after_imagery_timing_df$Value, 1)


################################################################################
# Within-study effect of temporal synchrony on model R2 (base R; readxl only to read the xlsx)
################################################################################

# Studies from the comparison database with multi-timing comparisons
studies <- "Cocke|Allen|Veraverbeke|Chen|Strand|Stambaugh|Boucher|Quintano"
within_sync <- comparisons[comparisons$Duplicate_fire_notes %in% "timing comparisons" & grepl(studies, comparisons$Authors), ]

# Convert months to numeric
for (v in c("Plots_month_avg", "Postfire_month_avg", "Plots_year", "Postfire_year")) {
  within_sync[[v]] <- suppressWarnings(as.numeric(within_sync[[v]]))
}

# Drop double metrics (in this case just sagebrush in Strand)
metrics_list <- strsplit(within_sync$Highest_model, ";")
metrics_list <- lapply(metrics_list, function(x) x[!grepl("sagebrush", x)])
stopifnot(all(lengths(metrics_list) == 1))

# Extract metric value
within_sync$r2 <- as.numeric(sub(".*:\\s?", "", unlist(metrics_list)))

# Square metrics if needed
metrics_to_square <- c(
  "Pearson correlation coefficeint r",
  "r",
  "|r|",
  "R"
)
within_sync[within_sync$Model_metric %in% metrics_to_square, ]$r2 <- 
  within_sync[within_sync$Model_metric %in% metrics_to_square, ]$r2^2


within_sync$metric_label <- sub(":\\s?-?\\d*\\.?\\d*$", "", unlist(metrics_list))

# Months from field sampling to image acquisition (negative = image before field)
within_sync$delay <- (within_sync$Postfire_year - within_sync$Plots_year) * 12 + 
  (within_sync$Postfire_month_avg - within_sync$Plots_month_avg)

# Boucher: same pooled model listed once per fire
within_sync <- within_sync[!duplicated(within_sync[, c("Citation", "Smoothing", "delay", "r2")]), ]

# Earliest image vs. image nearest the field sampling date, within fire x smoothing
within_sync$group <- paste(within_sync$Citation, within_sync$Fire_name, within_sync$Smoothing, sep = " | ")
groups    <- split(within_sync, within_sync$group)   # one data frame per study x fire x smoothing

pair_list <- list()
for (name in names(groups)) {
  g <- groups[[name]]
  
  # need at least two image dates to make a comparison
  if (nrow(g) < 2) next
  
  # sort image dates from earliest to latest
  g <- g[order(g$delay), ]
  
  # earliest image = first row after sorting
  early <- g[1, ]
  
  # nearest image = among the remaining rows, the one closest to the field date
  # (smallest absolute delay); +1 converts back to a row number in g
  remaining   <- g[-1, ]
  nearest_row <- which.min(abs(remaining$delay)) + 1
  near        <- g[nearest_row, ]
  
  pair_list[[name]] <- data.frame(
    study       = paste(early$Authors, early$Year, early$Jounral),
    index       = early$metric_label,
    metric      = early$Model_metric,
    fire        = early$Fire_name,
    smoothing   = early$Smoothing,
    delay_early = early$delay,
    r2_early    = early$r2,
    delay_near  = near$delay,
    r2_near     = near$r2
  )
}

pairs <- do.call(rbind, pair_list)   # stack the one-row data frames
pairs$d <- pairs$r2_near - pairs$r2_early
rownames(pairs) <- NULL

# Add loboda et al., Appendix D
# Delta (pre - post) regression R2 for RdNBR; post-fire images with equal n,
# one row per pre-fire baseline
loboda <- data.frame(
  prefire     = c("4-Jun-09", "20-Jun-09", "16-Aug-09", "30-May-10", "23-Jun-10"),
  n           = c(42, 52, 64, 51, 59),
  RdNBR_sep10 = c(0.76, 0.68, 0.68, 0.73, 0.69),
  RdNBR_jun11 = c(0.77, 0.80, 0.77, 0.81, 0.77)
)
loboda$d <- loboda$RdNBR_jun11 - loboda$RdNBR_sep10
loboda_d <- mean(loboda$d)   # average across pre-fire baselines

# Supplementary table: one row per study
# average numeric columns within study (e.g., Stambaugh's two extraction methods)
num_cols <- c("delay_early", "r2_early", "delay_near", "r2_near", "d")
supp <- aggregate(pairs[, num_cols], by = list(study = pairs$study), FUN = mean)

# add text columns (one value per study)
info <- pairs[!duplicated(pairs$study), c("study", "index", "metric")]
supp <- merge(info, supp, by = "study")

# Loboda row: mean R2 across the five pre-fire baselines.
# Delays averaged across fires (field sampling June 2011 for four fires, August 2011
# for Kaluktavik River): Sep 2010 = -9.4 months, Jun 2011 = -0.4 months.
loboda_row <- data.frame(
  study       = "Loboda et al. (2013)",
  index       = "RdNBR",
  metric      = "R2",
  delay_early = -9.4,
  r2_early    = mean(loboda$RdNBR_sep10),
  delay_near  = -0.4,
  r2_near     = mean(loboda$RdNBR_jun11),
  d           = loboda_d
)
supp <- rbind(supp, loboda_row)
supp <- supp[order(supp$study), ]

# round for reporting and drop the helper column
within_sync_tab <- supp[, c("study", "index", "metric", num_cols)]
within_sync_tab[, num_cols] <- round(within_sync_tab[, num_cols], 2)
names(within_sync_tab) <- c("Study", "Index", "Metric reported",
                     "Earliest image delay (months)", "R2 earliest",
                     "Nearest image delay (months)", "R2 nearest", "Delta R2")
rownames(within_sync_tab) <- NULL
# ---- 3. One value per study and summary -------------------------------------
by_study <- supp[, c("study", "d")]   # one value per study, including Loboda


# One value per study and summary
by_study <- aggregate(d ~ study, data = pairs, FUN = mean)
by_study <- rbind(by_study, data.frame(study = "Loboda", d = loboda_d))

# Create t-test
x  <- by_study$d
tt <- t.test(x)
within_sync_ttest <- data.frame(n_studies = length(x), mean_dR2 = mean(x), median_dR2 = median(x),
                      ci_low = tt$conf.int[1], ci_high = tt$conf.int[2], p = tt$p.value,
                      n_negative = sum(x < 0), n_zero = sum(x == 0), n_positive = sum(x > 0))

within_sync_ttest <- round(within_sync_ttest, 3)

# Sensitivity: Wilcoxon signed-rank test (no normality assumption)
# Zero differences carry no sign, so they are dropped; with no zeros or ties
# left, R can compute the exact p-value.
x_nonzero <- x[x != 0]
within_sync_wilcox <- wilcox.test(x_nonzero, mu = 0, exact = TRUE)
within_sync_wilcox <- data.frame(
  stat = within_sync_wilcox$statistic,
  p = round(within_sync_wilcox$p.value, 3)
)
