################################################################################
# Plots of alignment between field plot size and RS data resolution (Square)
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
#  Number of Smoothing Types
################################################################################
n_square = length( unique( sync_size[sync_size$Plot_shape == "Square", ]$Smoothing ) )
max_val = 1

################################################################################
#  Set Up Plot Layout
################################################################################
par(mfrow = c(2, 3), mar = c(6.5, 2.5, 1.5, 3), oma = c(0, 4, 1, 0))

################################################################################
#  Initialize Observation Vectors
################################################################################
min_obs <- rep(NA, length(n_square))
max_obs <- rep(NA, length(n_square))

################################################################################
# Loop through smoothing methods and create plots
################################################################################
for (s in 1:n_square) {
  
  #  Subset Data for Square Plots
  data_subset = sync_size[ sync_size$Smoothing == smoothing_order[s] & 
                             sync_size$Plot_shape == "Square", c("Plot_sqm", "Post_sqm") ]
  
  # Convert to hectares
  data_subset = data_subset / 10000
  
  #  Generate Empty Plot
  plot(NA, 
       xlim = c(0, max_val), 
       ylim = c(0, max_val), 
       xlab = "", 
       ylab = "", 
       main = "", 
       las = 2, 
       xaxt = "n", 
       yaxt = "n", 
       bty = "n")
  
  #  Add Title with Number of Observations
  smooth.n = nrow(data_subset)
  
  mtext( text = paste("  ", panel[s], ") ", smoothing_order_title[s], 
                      " (N = ", smooth.n, ")", sep = ""), 
         adj = 0, line = 1, font = 2, cex = 1.25 )
  
  #  Add Axis Labels
  mtext(text = "     Pixel", side = 3, line = -4, adj = 0)
  mtext(text = "     Plot", side = 1, line = -4, adj = 1)
  
  # Add axis labels for specific rows/columns
  if(s == 1 | s == 4) {
    mtext(text = expression(bold(paste("Resolution (ha)", sep = ""))), 
          side = 2, font = 2, line = 4.5, cex = 1.25)
  }
  
  if(s %in% 4:6) {
    mtext(text = expression(bold(paste("Plot size (ha)", sep = ""))), 
          side = 1, font = 2, line = 4.5, cex = 1.25)
  }
  
  # Add axes ticks and labels
  axis(side = 1, 
       at = seq(0, max_val, length.out = 6), 
       labels = formatC(seq(0, max_val, length.out = 6), 
                        format = "f", digits = 2), 
       cex.axis = 1.5)
  
  axis(side = 2, 
       at = seq(0, max_val, length.out = 6), 
       labels = formatC(seq(0, max_val, length.out = 6), 
                        format = "f", digits = 2), 
       cex.axis = 1.5, las = 2)
  
  #  Add spatial alignment line
  abline(0, 1, col = "grey")
  
  #  Add Alignment Text
  text(x = 0.8, y = 0.8, labels = "Aligned", srt = 45, cex = 1.5, 
       font = 2, col = "white")
  
  text(x = 0.8, y = 0.8, labels = "Aligned", srt = 45, cex = 1.5)
  
  # Count frequency of unique points
  freq <- as.numeric(table(apply(data_subset[, c("Plot_sqm", "Post_sqm")], 1, 
                                 paste, collapse = ",")))
  xy_unique <- unique(data_subset[, c("Plot_sqm", "Post_sqm")])
  
  # Add data points scaled by observation frequency
  points(x = xy_unique$Plot_sqm, 
         y = xy_unique$Post_sqm, 
         pch = 1, 
         cex = sqrt(freq))
  
  #  Store Min and Max Frequency
  min_obs[s] <- min(freq)
  max_obs[s] <- max(freq)
}

################################################################################
#  Add Legend to Plot
################################################################################
n_obs_summary <- summary((unique(c(min_obs, max_obs))))
n_obs_summary <- data.frame(
  Statistic = names(n_obs_summary), 
  Value = as.numeric(n_obs_summary) 
)

obs_breaks <- n_obs_summary[
  n_obs_summary$Statistic %in% c("Min.", "1st Qu.", "Median", "3rd Qu.", "Max."), 
  "Value"
]
legend("topright", 
       legend = obs_breaks, 
       pch = 1, 
       pt.cex = sqrt(obs_breaks), 
       x.intersp = 2, 
       y.intersp = sqrt(obs_breaks / 4) + c(0.5, rep(0, length(obs_breaks) - 1)), 
       bty = "o", 
       box.lwd = 0, 
       title = paste0("Number of\n observations\n per point\nMin: ", 
                      min(min_obs), 
                      "\nMax: ", 
                      max(max_obs)), 
       title.font = 2, 
       title.cex = 1.25)