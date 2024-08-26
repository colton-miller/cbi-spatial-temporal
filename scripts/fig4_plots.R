# plots of alignment between field plot size and spatial resolution of RS data
panel = c("a", "b", "c", "d", "e", "f", "g", "h", "i")

# rename smoothing types for plot titles
# capitalizing, adding hypthens
smoothing_order = c("Not specified","focal mean", "none", 
                    "mean of sample points within \n      plot", "bilinear","area weighted",
                    "mean of values of pixels within \n      distance of plot", 
                    "object-based")
smoothing_order_title = c("Not specified","Focal mean", "None", 
                          "Mean of sample points within \n      plot", "Bilinear","Area-weighted",
                          "Mean of values of pixels within \n      distance of plot", 
                          "Object-based")



# number of smoothing types in circular comparisons
n_square = length(
  unique(
    sync_size[sync_size$Plot_shape == "Square", ]$Smoothing
    )
  )

max_val = 1

par(mfrow = c(2, 3), mar = c(6.5, 2.5, 1.5, 3), oma = c(0, 4, 1, 0))

min_obs <- rep(NA, length(n_square))
max_obs <- rep(NA, length(n_square))
for (s in 1:n_square) {
  
  # subset to square plots, smoothing type, scale to ha from sq m
  data_subset = sync_size[
    sync_size$Smoothing == smoothing_order[s] & sync_size$Plot_shape == "Square", 
    c("Plot_sqm", "Post_sqm")
    ]
  
  data_subset = data_subset / 10000
  
  # generate empty plot
  plot(NA, 
       xlim = c(0, max_val), 
       ylim = c(0, max_val),
       xlab = "", 
       ylab = "", 
       main = "", 
       las = 2, 
       xaxt = "n", 
       yaxt = "n", 
       bty = "n"
       )
  
  # count number of obs
  smooth.n =  nrow(data_subset)
  
  # add text
  mtext(
    text = paste("  ", panel[s], ") ",   smoothing_order_title[s], " (N = ", smooth.n, ")", sep = ""), 
    adj = 0, 
    line = 1, 
    font = 2, 
    cex = 1.25
    )
  
  mtext(
    text = "     Pixel", 
    side = 3, 
    line = -4, 
    adj = 0
    )
  
  mtext(
    text = "     Plot", 
    side = 1, 
    line = -4, 
    adj = 1
    )
  
  # add axes labels for first plots in column (for y label),
  # all plots on last row (for x label)
  if(s == 1 | s == 4) {
    mtext(
      text = expression(bold(paste("Resolution (ha)", sep = ""))), 
      side = 2, 
      font = 2, 
      line = 4.5, 
      cex = 1.25)
  }
  
  if(s %in% 4:6) {
    mtext(
      text = expression(bold(paste("Plot size (ha)", sep = ""))), 
      side = 1,
      font = 2, 
      line = 4.5, 
      cex = 1.25)
    }
  
  # add exes
  axis(
    side = 1, 
    at = seq(0, max_val, length.out = 6), 
    labels = formatC(seq(0, max_val, length.out = 6), format = "f", digits = 2), 
    cex.axis = 1.5
    )
  
  axis(
    side = 2, 
    at = seq(0, max_val, length.out = 6), 
    labels = formatC(seq(0, max_val, length.out = 6), format = "f", digits = 2), 
    cex.axis = 1.5, 
    las = 2
    )
  
  # line of synchrony
  abline(0, 1, col = "grey")
  
  # add text for aligned (w/ background white)
  text(
    x = 0.8, 
    y = 0.8, 
    labels = "Aligned", 
    srt = 45, 
    cex = 1.5, 
    font = 2, 
    col = "white"
    )
  
  text(
    x = 0.8, 
    y = 0.8, 
    labels = "Aligned", 
    srt = 45, 
    cex = 1.5
    )
  
  
  # count the frequency of each unique point
  freq <- as.numeric(table(apply(data_subset[, c("Plot_sqm", "Post_sqm")], 1, paste, collapse = ",")))
  xy_unique <- unique(data_subset[, c("Plot_sqm", "Post_sqm")])
  
  # add points (scaled by number of obs at each location)
  points(
    x = xy_unique$Plot_sqm,  
    y = xy_unique$Post_sqm,
    pch = 1, 
    cex = sqrt(freq)
  )
  
  min_obs[s] <- min(freq)
  max_obs[s] <- max(freq)
  
}

# plot(NA,
#      xlim = c(0, max_val),
#      ylim = c(0, max_val),
#      xlab = "",
#      ylab = "",
#      main = "",
#      las = 2,
#      xaxt = "n",
#      yaxt = "n",
#      bty = "n"
# )
legend(
  "topright",
  legend = c(1, 5, 10, 15, 20),
  pch = 1,
  pt.cex = sqrt(c(1, 5, 10, 15, 20)),
  x.intersp = 2, 
  y.intersp = sqrt(c(5, 5, 10, 15, 20)/4),
  bty = "o",
  box.lwd = 0,
  title = paste0(
    "Number of\n observations\n per point\nMin: ", 
    min(min_obs), 
    "\nMax: ", 
    max(max_obs)
  ),
  title.font = 2,
  title.cex = 1.25
)



