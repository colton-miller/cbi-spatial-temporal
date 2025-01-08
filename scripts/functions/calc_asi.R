#' Calculate the Assumption Strength Index (ASI) for Circular and Square Plots
#' 
#' This function calculates the Assumption Strength Index (ASI) for circular and square plots
#' based on their plot sizes compared to a given post-fire image resolution. The ASI is computed using 
#' different formulas for circular and square plots, and the results are returned in the 
#' original dataframe.
#' 
#' @param data A dataframe containing columns for plot size (both observed and post-fire sizes) 
#'   and plot shape (either "Circular" or "Square").
#' @param post_sqm_col A string indicating the column name for the post-fire plot area in square meters.
#' @param plot_sqm_col A string indicating the column name for the observed plot area in square meters.
#' @param plot_shape_col A string indicating the column name for the plot shape ("Circular" or "Square").
#' 
#' @return A dataframe with new columns added: 
#'   - `circ_amin`, `circ_amax`, `circ_asi_min`, `circ_asi_max`, `circ_asi`
#'   - `square_asi`, `sign`, `circ_asi_abs`, `square_asi_abs`
#' 
#' @examples
#' data <- calculate_asi(data, "Post_sqm", "Plot_sqm", "Plot_shape")
#' head(data)
#' 
#' @export
calc_asi <- function(data, post_sqm_col, plot_sqm_col, plot_shape_col) {
  
  # Ensure the specified columns exist in the dataframe
  if (!(post_sqm_col %in% names(data)) || !(plot_sqm_col %in% names(data)) || !(plot_shape_col %in% names(data))) {
    stop("The specified columns do not exist in the dataframe.")
  }
  
  # Calculate ASI for circular plots
  x <- sqrt(data[[post_sqm_col]])
  rmin <- (1/2) * x
  rmax <- (sqrt(2 * x ^ 2) / 2)
  
  # Compute the minimum and maximum circular plot areas
  data$cir_amin <- pi * rmin ^ 2
  data$circ_amax <- pi * rmax ^ 2
  
  # Calculate the minimum and maximum ASI for circular plots
  data$circ_asi_min <- sign(data$cir_amin - data[[plot_sqm_col]]) * 
    sqrt(abs(data$cir_amin - data[[plot_sqm_col]]))
  data$circ_asi_max <- sign(data$circ_amax - data[[plot_sqm_col]]) * 
    sqrt(abs(data$circ_amax - data[[plot_sqm_col]]))
  
  # Initialize ASI for circular plots and identify conditions for negative, positive, or opposite sign ASI
  data$circ_asi <- NA
  circ_asi_neg <- which(data$circ_asi_min < 0 & data$circ_asi_max < 0 & data[[plot_shape_col]] == "Circular")
  circ_asi_pos <- which(data$circ_asi_min > 0 & data$circ_asi_max > 0 & data[[plot_shape_col]] == "Circular")
  circ_asi_opp <- which(sign(data$circ_asi_min) != sign(data$circ_asi_max) & data[[plot_shape_col]] == "Circular")
  
  # Apply calculations to circular plots based on conditions
  data$circ_asi[circ_asi_neg] <- round(pmax(data$circ_asi_min[circ_asi_neg], data$circ_asi_max[circ_asi_neg]), 3)
  data$circ_asi[circ_asi_pos] <- round(pmin(data$circ_asi_min[circ_asi_pos], data$circ_asi_max[circ_asi_pos]), 3)
  data$circ_asi[circ_asi_opp] <- 0
  
  # Calculate ASI for square plots
  data$square_asi <- sign(data[[post_sqm_col]] - data[[plot_sqm_col]]) * 
    sqrt(abs(data[[post_sqm_col]] - data[[plot_sqm_col]]))
  
  # For circular plots, set square ASI to NA
  data[data[[plot_shape_col]] == "Circular", ]$square_asi <- NA
  
  # Determine the assumption type based on sign
  data[data[[plot_shape_col]] == "Circular", "sign"] <- factor(
    sign(data[data[[plot_shape_col]] == "Circular", ]$circ_asi),
    levels = c(1, 0, -1),
    labels = c("Pixel homogeneity", "no assumption", "Plot homogeneity")
  )
  
  data[data[[plot_shape_col]] == "Square", "sign"] <- factor(
    sign(data[data[[plot_shape_col]] == "Square", ]$square_asi),
    levels = c(1, 0, -1),
    labels = c("Pixel homogeneity", "no assumption", "Plot homogeneity")
  )
  
  # Calculate the absolute value of ASI for easier plotting
  data$circ_asi_abs <- abs(data$circ_asi)
  data$square_asi_abs <- abs(data$square_asi)
  
  # Return the modified dataframe with ASI values for both circular and square plots
  return(data)
}