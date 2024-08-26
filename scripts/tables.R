# data for tables 3, 4, 5

# number of citations that used circular or square plots (remove duplicates of the same citation)

# remove duplicate studies using same shape
# wanting to count number of studies that used each shape
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
sync_size_shape_comparison <- sync_size[
  !duplicated(paste0(sync_size$Citation, sync_size$Fire_name, sync_size$Plot_shape)),
]

circ_square_comparisons <- table(sync_size_shape_comparison$Plot_shape)
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

