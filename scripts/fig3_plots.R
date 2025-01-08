################################################################################
# Plots of alignment between field plot size and RS data resolution (Circular)
################################################################################

# Define panel labels and smoothing method details
panel <- c("a", "b", "c", "d", "e", "f", "g", "h", "i")
smoothing_order <- c(
  "Not specified", "focal mean", "none", 
  "mean of sample points within \n      plot", 
  "bilinear", "area weighted", 
  "mean of values of pixels within \n      distance of plot", 
  "object-based"
)
smoothing_order_title <- c(
  "Not specified", "Focal mean", "None", 
  "Mean of sample points within \n      plot", 
  "Bilinear", "Area-weighted", 
  "Mean of values of pixels within \n      distance of plot", 
  "Object-based"
)

################################################################################
# Spatial synchrony equations and other constants
################################################################################
x <- seq(0, sqrt(13000), 0.1)  # Range for synchrony lines
y1 <- (x / 2)^2 * pi           # Synchrony line 1
y2 <- (x * sqrt(2) / 2)^2 * pi # Synchrony line 2

n_circular <- length(
  unique(sync_size[sync_size$Plot_shape == "Circular", ]$Smoothing)
)
max_val <- 1  # Max x/y value (in hectares)

################################################################################
#  Set Up Plot Layout
################################################################################
par(mfrow = c(3, 3), mar = c(6.5, 2.5, 1.5, 3), oma = c(0, 4, 1, 0))

min_obs <- rep(NA, n_circular)
max_obs <- rep(NA, n_circular)

################################################################################
# Loop through smoothing methods and create plots
################################################################################
for (s in 1:n_circular) {
  
  #  Subset Data for Circular Plots
  data_subset <- sync_size[sync_size$Smoothing == smoothing_order[s] & 
                             sync_size$Plot_shape == "Circular", c("Plot_sqm", "Post_sqm")]
  
  # Convert to hectares
  data_subset = data_subset / 10000
  
  # Generate empty plot
  plot(
    NA, xlim = c(0, max_val), ylim = c(0, max_val),
    xlab = "", ylab = "", main = "",
    las = 2, xaxt = "n", yaxt = "n", bty = "n"
  )
  
  # Count observations and add panel title
  smooth_n <- nrow(data_subset)
  mtext(
    text = paste("  ", panel[s], ") ", smoothing_order_title[s], 
                 " (N = ", smooth_n, ")", sep = ""), 
    adj = 0, line = 1, font = 2, cex = 1.25
  )
  
  # Add diagonal labels
  mtext(text = "     Pixel", side = 3, line = -4, adj = 0)
  mtext(text = "     Plot", side = 1, line = -4, adj = 1)
  mtext(text = "     Aligned", side = 3, line = -5, adj = 1)
  
  # Add axis labels for specific rows/columns
  if (s %in% c(1, 4, 7)) {
    mtext(
      text = expression(bold(paste("Resolution (ha)", sep = ""))),
      side = 2, font = 2, line = 4.5, cex = 1.25
    )
  }
  if (s %in% 5:7) {
    mtext(
      text = expression(bold(paste("Plot size (ha)", sep = ""))),
      side = 1, font = 2, line = 4.5, cex = 1.25
    )
  }
  
  # Add axes ticks and labels
  axis(
    side = 1, at = seq(0, max_val, length.out = 6), 
    labels = formatC(seq(0, max_val, length.out = 6), format = "f", digits = 2), 
    cex.axis = 1.5
  )
  axis(
    side = 2, at = seq(0, max_val, length.out = 6), 
    labels = formatC(seq(0, max_val, length.out = 6), format = "f", digits = 2), 
    cex.axis = 1.5, las = 2
  )
  
  # Add spatial alignment lines
  lines(x = y1, y = I(x^2), lwd = 1, col = "grey")
  lines(x = y2, y = I(x^2), lwd = 1, col = "grey")
  
  # Count frequency of unique points
  freq <- as.numeric(table(apply(data_subset, 1, paste, collapse = ",")))
  xy_unique <- unique(data_subset)
  
  # Add data points scaled by observation frequency
  points(
    x = xy_unique$Plot_sqm, y = xy_unique$Post_sqm,
    pch = 1, cex = sqrt(freq)
  )
  
  #  Store Min and Max Frequency
  min_obs[s] <- min(freq)
  max_obs[s] <- max(freq)
}

################################################################################
# Add legend in the final panel
################################################################################
plot(
  NA, xlim = c(0, max_val), ylim = c(0, max_val),
  xlab = "", ylab = "", main = "",
  las = 2, xaxt = "n", yaxt = "n", bty = "n"
)

n_obs_summary <- summary((unique(c(min_obs, max_obs))))
n_obs_summary <- data.frame(
  Statistic = names(n_obs_summary),
  Value = as.numeric(n_obs_summary) 
)

obs_breaks <- n_obs_summary[
  n_obs_summary$Statistic %in% c("Min.", "1st Qu.", "Median", "3rd Qu.", "Max."), 
  "Value"
  ]

legend(
  "top",
  legend = obs_breaks,
  pch = 1, pt.cex = sqrt(obs_breaks),
  x.intersp = 2, y.intersp = sqrt(obs_breaks / 4) + c(0.5, rep(0, length(obs_breaks) - 1)),
  bty = "n",
  title = paste0(
    "Number of observations per point\nMin: ", 
    min(min_obs), "\nMax: ", max(max_obs)
  ),
  title.font = 2, title.cex = 1.25
)