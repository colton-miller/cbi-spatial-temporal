# load libraries
library(ggplot2)
library(ggpubr)
library(grid)
library(readxl)
library(this.path)


##### Define functions #####

calc_plot_dims <- function(w, h, orig_res, target_res) {
  scaling_factor <- target_res / orig_res
  new_width <- w * scaling_factor
  new_height <- h * scaling_factor
  return(list(w = new_width, h = new_height, r = target_res))
}

# Original resolution (72 PPI)
original_width <- 900
original_height <- 900
original_res <- 72

# Target resolution (300 PPI)
target_res <- 300
scaling_factor <- target_res / original_res

# Adjust width and height
new_width <- original_width * scaling_factor
new_height <- original_height * scaling_factor

##### Data Loading #####

# read in data
script_path <- this.path()
repo_dir <- dirname(script_path) |> dirname()
comparisons <- read_xlsx(
  file.path(
    repo_dir,
    "data", 
    "comparisons_results_20240825.xlsx"
    )
  )

setwd(repo_dir) # folder to save outputs named "plots" in this directory


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
                    "Postfire_year"
                    )

sync <- comparisons[, fields_to_keep]

# convert chr fields to integer 
# note: introduces NA's for some obs that were not specified
sync$Fire_start_month <- as.integer(sync$Fire_start_month)
sync$Fire_start_year <- as.integer(sync$Fire_start_year)
sync$Plots_month <- as.integer(sync$Plots_month_avg)
sync$Plots_year <- as.integer(sync$Plots_year)
sync$Postfire_month <- as.integer(sync$Postfire_month_avg)
sync$Postfire_year <- as.integer(sync$Postfire_year)

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
  "Plots_year"
  )
  ]

sync_plot_imagery <- sync[, c(
  "Citation",
  "Fire_name",
  "Plots_month",
  "Plots_year",
  "Postfire_month",
  "Postfire_year"
  )
  ]

sync_fire_imagery <- sync[, c(
  "Citation",
  "Fire_name",
  "Fire_start_month",
  "Fire_start_year",
  "Postfire_month",
  "Postfire_year"
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

sync_fire_plot$Fire_start_month <- 
  sync_fire_plot$Fire_start_month + 
  12 * (sync_fire_plot$Fire_start_year - min_yr)

sync_fire_plot$Plots_month <- 
  sync_fire_plot$Plots_month + 
  12 * (sync_fire_plot$Plots_year - min_yr)

sync_plot_imagery$Plots_month <- 
  sync_plot_imagery$Plots_month + 
  12 * (sync_plot_imagery$Plots_year - min_yr)

sync_plot_imagery$Postfire_month <- 
  sync_plot_imagery$Postfire_month + 
  12 * (sync_plot_imagery$Postfire_year - min_yr)

sync_fire_imagery$Fire_start_month <- 
  sync_fire_imagery$Fire_start_month + 
  12 * (sync_fire_imagery$Fire_start_year - min_yr)

sync_fire_imagery$Postfire_month <- 
  sync_fire_imagery$Postfire_month + 
  12 * (sync_fire_imagery$Postfire_year - min_yr)

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
  "Postfire_image"
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

# get min and max plot radii
x <- sqrt(sync_size$Post_sqm)
rmin <- (1/2) * x
rmax <- (sqrt(2 * x ^ 2) / 2)

# calculate min and max syncrhonous size for circular plots
sync_size$cir_amin <- pi * rmin ^ 2
sync_size$circ_amax <- pi * rmax ^ 2

# set up min and max plot size if data were syncrounous
sync_size$circ_asi_min <- sign(
  sync_size$cir_amin - sync_size$Plot_sqm
  ) * sqrt(
    abs(sync_size$cir_amin - sync_size$Plot_sqm)
    )

sync_size$circ_asi_max <- sign(
  sync_size$circ_amax - sync_size$Plot_sqm
  ) * sqrt(
    abs(sync_size$circ_amax - sync_size$Plot_sqm)
    )

# calculate ASI values for each circular plot 
# note: separating calcs based on whether plot is 
# bigger or smaller than what is synchronous
sync_size$circ_asi <- NA

circ_asi_neg <- sync_size$circ_asi_min < 0 & sync_size$circ_asi_max < 0
circ_asi_pos <- sync_size$circ_asi_min > 0 & sync_size$circ_asi_max > 0
circ_asi_opp <- sign(sync_size$circ_asi_min) != sign(sync_size$circ_asi_max)

circular <- sync_size$Plot_shape == "Circular"

# if both min and max asi neg, calculate the maximum value between 
# circ_asi_min and circ_asi_max for each row in the subsetted data frame

sync_size[circ_asi_neg & circular,]$circ_asi <- round(
  with(
      sync_size[circ_asi_neg & circular, ], 
      pmax(circ_asi_min, circ_asi_max)
      ),
    3
    )

# if both min and max asi pos, calculate the minimum value between 
# circ_asi_min and circ_asi_max for each row in the subsetted data frame
sync_size[circ_asi_pos & circular, ]$circ_asi <- round(
  with(
    sync_size[circ_asi_pos & circular, ], 
    pmin(circ_asi_min, circ_asi_max)
    ),
  3
  )

# if min and max asi have opposite signs, then asi = 0
sync_size[circ_asi_opp & circular, ]$circ_asi = 0

# calculate ASI values for each square plot
sync_size$square_asi <- 
  sign(sync_size$Post_sqm - sync_size$Plot_sqm) * 
  sqrt(abs(sync_size$Post_sqm - sync_size$Plot_sqm))

sync_size[sync_size$Plot_shape == "Circular", ]$square_asi = NA

# create subset dataframes for each plot shape
sync_size_circular = sync_size[sync_size$Plot_shape == "Circular",  ]
sync_size_square = sync_size[sync_size$Plot_shape == "Square",  ]

# calculate sign of asi (tells what assumption is made)
sync_size_circular$sign = sign(sync_size_circular$circ_asi)
sync_size_circular$sign = as.factor(sync_size_circular$sign)
sync_size_circular$sign = factor(
  sync_size_circular$sign, 
  levels = c("1", "0", "-1")
  )
levels(sync_size_circular$sign) = c(
  "Pixel homogeneity", 
  "no assumption", 
  "Plot homogeneity"
  )

sync_size_square$sign = sign(sync_size_square$square_asi)
sync_size_square$sign = as.factor(sync_size_square$sign)
sync_size_square$sign = factor(
  sync_size_square$sign, 
  levels = c("1", "0", "-1")
  )
levels(sync_size_square$sign) = c(
  "Pixel homogeneity", 
  "no assumption", 
  "Plot homogeneity"
  )

# calculate absolute value so can plot side by side
sync_size_circular$circ_asi_abs = abs(sync_size_circular$circ_asi)
sync_size_square$square_asi_abs = abs(sync_size_square$square_asi)



##### Analysis #####

# table outputs
source(file.path(repo_dir, "scripts", "tables.R"))
write.csv(
  circ_square_studies, 
  file = "plots/circ_square_plots_citations.csv", 
  row.names = FALSE
  )
write.csv(
  circ_square_comparisons, 
  file = "plots/circ_square_plots_comparisons.csv", 
  row.names = FALSE
  )
write.csv(
  smoothing_type_by_shape_both, 
  file = "plots/smoothing_type_by_shape.csv", 
  row.names = FALSE
  )
write.csv(
  image_type_by_shape, 
  file = "plots/image_type_by_shape.csv", 
  row.names = TRUE
  )

# assumptions definition plots
plot_dims <- calc_plot_dims(w = 1000, h = 500, orig_res = 72, target_res = 300)
png(
  filename = "plots/MillerFig2.png", 
  width = plot_dims$w, 
  height = plot_dims$h, 
  res = plot_dims$r
  )
source(file.path(repo_dir, "scripts", "fig2_plots.R"))
dev.off()

# circular field plots spatial synchrony
plot_dims <- calc_plot_dims(w = 800, h = 800, orig_res = 72, target_res = 300)
png(
  filename = "plots/MillerFig3.png", 
  width = plot_dims$w, 
  height = plot_dims$h, 
  res = plot_dims$r
  )
source(file.path(repo_dir, "scripts", "fig3_plots.R"))
dev.off()

# square field plots spatial synchrony
plot_dims <- calc_plot_dims(w = 800, h = 533, orig_res = 72, target_res = 300)
png(
  filename = "plots/MillerFig4.png", 
  width = plot_dims$w, 
  height = plot_dims$h, 
  res = plot_dims$r
  )
source(file.path(repo_dir, "scripts", "fig4_plots.R"))
dev.off()

# ASI boxplots
# save circular and square plot ASI into one figure
plot_dims <- calc_plot_dims(w = 1400, h = 1200, orig_res = 72, target_res = 300)
png(
  filename = "plots/MillerFig5.png", 
  width = plot_dims$w, 
  height = plot_dims$h, 
  res = plot_dims$r
  )
source(file.path(repo_dir, "scripts", "fig5_plots.R"))
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

# temporal synchrony plots
plot_dims <- calc_plot_dims(w = 900, h = 900, orig_res = 72, target_res = 300)
png(
  filename = "plots/MillerFig6.png", 
  width = plot_dims$w, 
  height = plot_dims$h, 
  res = plot_dims$r
  )
source(file.path(repo_dir, "scripts", "fig6_plots.R"))
dev.off()


