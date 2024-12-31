# figure 6 plots: 
# a. field plot month vs fire start month, 
# b. delay between field plots and fire ignition
# c. RS month vs field plot month
# d. delay between RS data and field plots

# function to round range of values out to nearest factor of c
round_to_nearest_c <- function(x, c) {
  min_val <- floor(min(x) / c) * c
  max_val <- ceiling(max(x) / c) * c
  return(c(min_val, max_val))
}

round_down_even <- function(x) {
  floor(x / 2) * 2
}

round_up_even <- function(x) {
  ceiling(x / 2) * 2
}


# get earliest and latest months observed in dataset
valmin <- min(
  sync_fire_plot$Fire_start_month, 
  sync_fire_plot$Plots_month,
  sync_plot_imagery$Postfire_month, 
  sync_plot_imagery$Plots_month
  )

valmax <- max(
  sync_fire_plot$Fire_start_month, 
  sync_fire_plot$Plots_month,
  sync_plot_imagery$Postfire_month, 
  sync_plot_imagery$Plots_month
  )

# round months to years encompassing data
val_min_max <- round_to_nearest_c(c(valmin, valmax), 12)

# calculate years to use in plotting
start_yr <- min_yr + val_min_max[1] / 12
start_yr <- round_down_even(start_yr)

end_yr <- min_yr + val_min_max[2] / 12
end_yr <- round_up_even(end_yr)

month_min <- (start_yr - min_yr) * 12
month_max <- (end_yr - min_yr) * 12

# plot setup
par(mfrow = c(2, 2), oma = c(0,0.25,0.25,0.25), mar = c(6, 5, 2, 1))
size <- 1.25
hist_y_max <- 150

# a. plot of field plot month by fire ignition month

plot(NA, 
     xlim = c(month_min, month_max), 
     ylim = c(month_min, month_max), 
     bty = "n",
     xaxt = "n", 
     yaxt = "n", 
     xlab = "", 
     ylab = "", 
)

axis(
  side = 1,
  at = seq(month_min + 12, month_max - 12, 24),
  labels = rep("", length(seq(month_min + 12, month_max - 12, 24))),
  las = 2,
  tck = -0.01
)

axis(
  side = 1, 
  at = seq(month_min, month_max, 24), 
  labels = seq(start_yr, end_yr, 2), 
  las = 2, 
  lwd.ticks = 1.5, 
  cex.axis = 1.25
)

axis(
  side = 2,
  at = seq(month_min + 12, month_max - 12, 24),
  labels = rep("", length(seq(month_min + 12, month_max - 12, 24))),
  las = 2,
  tck = -0.01
)

axis(
  side = 2, 
  at = seq(month_min, month_max, 24), 
  labels = seq(start_yr, end_yr, 2), 
  las = 2, 
  lwd.ticks = 1.5, 
  cex.axis = 1.25
)

mtext(
  text = "Fire start timing (year)", 
  side = 1, 
  line = 4, 
  font = 2, 
  cex = size
)

mtext(
  text = "Field plot timing (year)", 
  side = 2, 
  line = 4, 
  font = 2, 
  cex = size
)

mtext(
  "  a) Field plot month by fire ignition month", 
  side = 3, 
  font = 2, 
  adj = 0, 
  cex = size, 
  line = 0.5
)

lines(
  x = c(0, valmax), 
  y = c(0, valmax), 
  lwd = 1, 
  col = "grey"
)

points(
  x = sync_fire_plot$Fire_start_month,
  y = sync_fire_plot$Plots_month, 
  pch = 16, 
  cex = 1, 
  col = rgb(0, 0, 0, 0.25)
)

mtext(
  text = paste("      N = ", nrow(sync_fire_plot)), 
  side = 3, 
  font = 2, 
  adj = 0, 
  line = -1, 
  cex = size
)

# b. histogram of field plot month minus fire ignition month
plot_delay_range <- range(
  sync_fire_plot$Plots_month - sync_fire_plot$Fire_start_month
  )
plot_delay_range <- round_to_nearest_c(plot_delay_range, 4)

hist(
  sync_fire_plot$Plots_month - sync_fire_plot$Fire_start_month, 
  breaks = seq(plot_delay_range[1], plot_delay_range[2], 2), 
  ylim = c(0, hist_y_max),
  col = "grey", 
  main = "", 
  xlab = "", 
  ylab = "", 
  las = 2, 
  xaxt = "n", 
  xlim = plot_delay_range, 
  cex = 1.25, 
  cex.axis = 1.25
)

axis(
  side = 1, 
  at = seq(0, 36, 4), 
  labels = 
    seq(0, 36, 4), 
  cex.axis = 0.9, 
  cex.axis = 1.25
)

mtext(
  text = "Field plots month - fire start month", #"Delay between field plots and fire ignition \n(months)", 
  side = 1, 
  font = 2, 
  line = 4, 
  cex = size, 
  adj = 0.5
)

mtext(
  text = "Number of comparisons", 
  side = 2, 
  font = 2, 
  line = 3, 
  cex = size
)

mtext(
  text = "  b) Delay between field plots and fire ignition", 
  side = 3, 
  font = 2, 
  adj = 0, 
  cex = size, 
  line = 0.5
)

mtext(
  text = paste("      N = ", nrow(sync_fire_plot)), 
  side = 3, 
  font = 2, 
  adj = 0, 
  line = -1, 
  cex = size
)

# c. plot of field plot month by fire ignition month

plot(NA,
     xlim = c(month_min, month_max), 
     ylim = c(month_min, month_max),
     bty = "n", 
     xaxt = "n", 
     yaxt = "n",
     xlab = "", 
     ylab = ""
)

axis(
  side = 1,
  at = seq(month_min + 12, month_max - 12, 24),
  labels = rep("", length(seq(month_min + 12, month_max - 12, 24))),
  las = 2,
  tck = -0.01
)

axis(
  side = 1, 
  at = seq(month_min, month_max, 24), 
  labels = seq(start_yr, end_yr, 2), 
  las = 2, 
  lwd.ticks = 1.5, 
  cex.axis = 1.25
)


axis(
  side = 2,
  at = seq(month_min + 12, month_max - 12, 24),
  labels = rep("", length(seq(month_min + 12, month_max - 12, 24))),
  las = 2,
  tck = -0.01
)

axis(
  side = 2, 
  at = seq(month_min, month_max, 24), 
  labels = seq(start_yr, end_yr, 2), 
  las = 2, 
  lwd.ticks = 1.5, 
  cex.axis = 1.25
)


mtext(
  text = "Field plot timing (year)", 
  side = 1, 
  line = 4, 
  font = 2, 
  cex = size
)

mtext(
  text = "Remotely sensed data timing (year)", 
  side = 2, 
  line = 4, 
  font = 2, 
  cex = size
)

lines(
  x = c(0, valmax), 
  y = c(0, valmax), 
  lwd = 1, 
  col = "grey"
)

points(
  x = sync_plot_imagery$Plots_month, 
  y = sync_plot_imagery$Postfire_month, 
  pch = 16, 
  cex = 1, 
  col = rgb(0, 0, 0, 0.25)
)

mtext(
  text = "  c) Remotely sensed month by field plot month", 
  side = 3, 
  font = 2, 
  adj = 0, 
  cex = size, 
  line = 0.5
)

mtext(
  text = paste("      N = ", nrow(sync_plot_imagery)), 
  side = 3, 
  font = 2, 
  adj = 0, 
  line = -1, 
  cex = size
)

# d. histogram of rs data month minus field plot month

rs_delay_range <- range(
  sync_plot_imagery$Postfire_month - sync_plot_imagery$Plots_month
  )
rs_delay_range <- round_to_nearest_c(rs_delay_range, 4)

hist(
  sync_plot_imagery$Postfire_month - sync_plot_imagery$Plots_month, 
  breaks = seq(rs_delay_range[1], rs_delay_range[2], 2), 
  ylim = c(0, hist_y_max),
  col = "grey", 
  main = "", 
  xlab = "", 
  ylab = "", 
  las = 2, 
  xaxt = "n", 
  xlim = c(rs_delay_range[1] - 4, rs_delay_range[2]), 
  cex = 1.25, 
  cex.axis = 1.25
)


# Generate the sequence
seq_values <- seq(rs_delay_range[1] - 4, rs_delay_range[2], 8)

# Interleave with empty scalar ""
interleaved <- c(rbind(seq_values, rep("", length(seq_values) - 1)))

# Convert the interleaved result to a character vector
interleaved <- as.character(interleaved)


axis(
  side = 1, 
  at = seq(rs_delay_range[1] - 4,rs_delay_range[2], 4), 
  labels = interleaved[-length(interleaved)], 
  cex.axis = 0.9, 
  cex.axis = 1.25,
  las = 1
)

mtext(
  text = "Remotely sensed data month - field plots month", #"Delay between remotely sensed data and \nfield plots (months)", 
  side = 1, 
  font = 2, 
  line = 4, 
  cex = size, 
  adj = 0.5
)

mtext(
  text = "Number of comparisons", 
  side = 2, 
  font = 2, 
  line = 3, 
  cex = size
)

mtext(
  text = "  d) Delay between remotely sensed data and \n      field plots", 
  side = 3, 
  font = 2, 
  adj = 0, 
  cex = size, 
  line = -1
)

mtext(
  text = paste("      N = ", nrow(sync_plot_imagery)), 
  side = 3, 
  font = 2, 
  adj = 0, 
  line = -2.5, 
  cex = size
)


plot_delay_after_fire <- data.frame(
  mean = round(mean(sync_fire_plot$Plots_month - sync_fire_plot$Fire_start_month), 1),
  sd = round(sd(sync_fire_plot$Plots_month - sync_fire_plot$Fire_start_month), 1)
)

imagery_delay_after_plots <- data.frame(
  mean = round(mean(sync_plot_imagery$Postfire_month - sync_plot_imagery$Plots_month), 1),
  sd = round(sd(sync_plot_imagery$Postfire_month - sync_plot_imagery$Plots_month), 1)
)
