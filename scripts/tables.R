# data for tables 3, 4, 5

# number of studies used in analysis (of 62 possible)

# these are all the data.frames used for analysis from the original csv 
# (subset based on having information needed for plotting or tables)
unique_studies <- c(
  unique(sync_size_circular$Citation), 
  unique(sync_size_square$Citation), 
  unique(sync_fire_plot$Citation), 
  unique(sync_plot_imagery$Citation)
)

unique_studies <- unique(unique_studies)
write.csv(
  length(unique_studies),
  file = "tables/n_studies.csv",
  row.names = FALSE
)


# broad brush number of "comparisons" (i.e, number of rows per study)
comparison_summary <- table(comparisons[comparisons$Citation %in% unique_studies, ]$Citation)
write.csv(
  comparison_summary,
  file = "tables/comparison_summary.csv",
  row.names = FALSE
)



# number of citations that used circular or square plots (remove duplicates of the same citation)

# remove duplicate studies using same shape
# wanting to count number of studies that used each shape (where both plot size
# and post-fire pixel size were available)
sync_size_shape_study <- sync_size[
  !duplicated(paste0(sync_size$Citation, sync_size$Plot_shape)), 
  ]

circ_square_studies <- table(sync_size_shape_study$Plot_shape)
circ_square_studies <- as.data.frame(circ_square_studies)
circ_square_studies <- circ_square_studies[
  circ_square_studies$Var1 %in% c("Circular", "Square"), 
  ]


# number of comparisons that used circular or square plots 

# remove duplicates of the same citation/fire combo/shape
# wanting to count number of comparisons that used each shape
# these numbers add up to the N's on fig3 and fig4
circ_square_comparisons <- table(sync_size$Plot_shape)
circ_square_comparisons <- as.data.frame(circ_square_comparisons)
circ_square_comparisons <- circ_square_comparisons[
  circ_square_studies$Var1 %in% c("Circular", "Square"), 
  ]


# smoothing type by shape table

# remove duplicates of the same citation/fire/shape/smoothing
sync_size_smoothing <- sync_size[
  !duplicated(paste0(sync_size$Citation, sync_size$Fire_name, sync_size$Plot_shape, sync_size$Smoothing)),
]

smoothing_type_by_shape <- table(sync_size_smoothing$Smoothing, sync_size_smoothing$Plot_shape)
smoothing_type_by_shape <- as.data.frame(smoothing_type_by_shape)

smoothing_type_by_shape_circ <- smoothing_type_by_shape[smoothing_type_by_shape$Var2 == "Circular", ]
colnames(smoothing_type_by_shape_circ) <- c("method", "shape", "circular")
smoothing_type_by_shape_circ <- smoothing_type_by_shape_circ[, c("method", "circular")]
smoothing_type_by_shape_square <- smoothing_type_by_shape[smoothing_type_by_shape$Var2 == "Square", ]
colnames(smoothing_type_by_shape_square) <- c("method", "shape", "square")
smoothing_type_by_shape_square <- smoothing_type_by_shape_square[, c("method", "square")]

smoothing_type_by_shape_both <- merge(smoothing_type_by_shape_circ,
                                      smoothing_type_by_shape_square,
                                      by = "method")

# image type by shape

# remove duplicates of the same citation/fire/shape/image type
sync_size_imagery <- sync_size[
  !duplicated(paste0(sync_size$Citation, sync_size$Fire_name, sync_size$Plot_shape, sync_size$Postfire_image)), 
  ]

image_type_by_shape <- table(sync_size_imagery$Postfire_image, sync_size_imagery$Plot_shape)

# add row summing up all landsat imagery
image_type_by_shape <- rbind(
  image_type_by_shape, 
  colSums(
    image_type_by_shape[
      rownames(image_type_by_shape) %in% c("ETM", "ETM+", "Landsat", "OLI", "TM", "TM or ETM+", "TM/ETM+"), 
      ]
    )
  )

# name added landsat row
rownames(image_type_by_shape)[nrow(image_type_by_shape)] <- "All Landsat"

# remove original landsat rows
ind_to_remove <- which(
  rownames(image_type_by_shape) 
  %in% 
    c("ETM", "ETM+", "Landsat", "OLI", "TM", "TM or ETM+", "TM/ETM+")
  )

image_type_by_shape <- image_type_by_shape[-ind_to_remove, ]




# number of comparisons for each type of assessment

spatial_circ_summary <- data.frame(table(sync_size_circular[!is.na(sync_size_circular$circ_asi_abs), ]$Citation))
colnames(spatial_circ_summary) <- c("Citation", "Frequency")

spatial_square_summary <- data.frame(table(sync_size_square[!is.na(sync_size_square$square_asi_abs), ]$Citation))
colnames(spatial_square_summary) <- c("Citation", "Frequency")

temporal_fire_plot_summary <- data.frame(table(sync_fire_plot[!is.na(sync_fire_plot$Fire_start_month) & !is.na(sync_fire_plot$Plots_month), ]$Citation))
colnames(temporal_fire_plot_summary) <- c("Citation", "Frequency")

temporal_plots_rs_summary <- data.frame(table(sync_plot_imagery[!is.na(sync_plot_imagery$Plots_month) & !is.na(sync_plot_imagery$Postfire_month), ]$Citation))
colnames(temporal_plots_rs_summary) <- c("Citation", "Frequency")


aligned_comparisons <- read.csv("tables/number_aligned_comarisons.csv")
misaligned_comparisons <- read.csv("tables/number_misaligned_comparisons.csv")

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

write.csv(
  number_comparisons_by_type,
  file = "tables/number_comparisons_by_type.csv",
)
