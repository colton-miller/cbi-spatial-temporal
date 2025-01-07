#' Sync Month Based on Year Difference
#'
#' This function normalizes a month variable based on the difference between 
#' the year and a minimum reference year, adjusting the month by adding the 
#' appropriate number of months.
#'
#' @param data A data frame containing columns for month and year.
#' @param month_col A string specifying the column name for the month (e.g., "Fire_start_month").
#' @param year_col A string specifying the column name for the year (e.g., "Fire_start_year").
#' @param min_yr A numeric value representing the minimum year to reference for the calculation.
#'
#' @return A vector of the adjusted month values.
#'
#' @examples
#' # Example of how to use the function
#' adjusted_months <- scale_months(sync_fire_plot, "Fire_start_month", "Fire_start_year", 2000)
#' head(adjusted_months)
#'
#' @export
scale_months <- function(data, month_col, year_col, min_yr) {
  # Ensure the specified columns exist in the data
  if (!(month_col %in% names(data)) || !(year_col %in% names(data))) {
    stop("The specified columns do not exist in the dataframe.")
  }
  
  # Calculate the scaled months based on the year difference
  scaled_months <- data[[month_col]] + 12 * (data[[year_col]] - min_yr)
  
  # Return the vector of scaled months
  return(scaled_months)
}