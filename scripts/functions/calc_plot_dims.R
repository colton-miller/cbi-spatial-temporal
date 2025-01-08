#' Calculate New Plot Dimensions for Target Resolution
#'
#' This function calculates the new width and height of a plot based on the original
#' resolution and the target resolution, scaling the dimensions accordingly. It can
#' be used to adjust plot sizes when changing the resolution of images or graphics.
#'
#' @param w numeric. The original width of the plot (in any unit, e.g., inches or pixels).
#' @param h numeric. The original height of the plot (in the same unit as `w`).
#' @param orig_res numeric. The original resolution of the plot (e.g., pixels per inch).
#' @param target_res numeric. The target resolution to scale the plot dimensions to.
#'
#' @return A list containing:
#'   - `w`: The new width of the plot scaled to the target resolution.
#'   - `h`: The new height of the plot scaled to the target resolution.
#'   - `r`: The target resolution used in the calculation.
#'
#' @examples
#' # Example of how to use the function
#' new_dims <- calc_plot_dims(6, 4, 300, 600)
#' print(new_dims)
#'
#' @export
calc_plot_dims <- function(w, h, orig_res, target_res) {
  # Calculate the scaling factor based on the ratio of target resolution to original resolution
  scaling_factor <- target_res / orig_res
  
  # Apply the scaling factor to the original width and height to get the new dimensions
  new_width <- w * scaling_factor
  new_height <- h * scaling_factor
  
  # Return a list containing the new width, height, and target resolution
  return(list(w = new_width, h = new_height, r = target_res))
}