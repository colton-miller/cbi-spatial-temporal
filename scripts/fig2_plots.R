# plots showing definitions of spatial synchrony
par(mfrow = c(1, 2), mar = c(4, 6, 2.5, 0.5), oma = c(0.5, 0.5, 0.5, 0.5))


# circular plot synchrony definition

# true synchrony (circular)
x = seq(0, sqrt(13000), 0.1)
y1 = (x / 2) ^ 2 * pi
y2 = (x * sqrt(2) / 2) ^ 2 * pi


## description of assumptions panel
plot(
  NA, 
  xlim = c(0, 10000), 
  ylim = c(0, 10000), 
  xaxt = "n", 
  yaxt = "n", 
  xlab = "", 
  ylab = "", 
  bty = "n"
  )

mtext(
  side = 1, 
  text = "     Increasing ->", 
  line = -0.5, 
  cex = 2, 
  adj = 0
  )

mtext(
  side = 2,
  text = "     Increasing ->", 
  line = -1, 
  cex = 2, 
  adj = 0
  )

mtext(
  side = 1, 
  text = expression(bold(paste("Plot size (ha)", sep = ""))), 
  font = 2, 
  line = 2, 
  cex = 2
  )

mtext(
  side = 2, 
  text = expression(bold(paste("Resolution (ha)", sep = ""))), 
  font = 2, 
  line = 1, 
  cex = 2
  )

polygon(
  x = c(0, 0, max(y1)), 
  y = c(0, 12000, max(I(x^2))), 
  col = "lightgrey"
  )

polygon(
  x = c(0, 12000, max(y2)),
  y = c(0, 0, max(I(x^2))), 
  col = "lightgrey"
  )

polygon(
  x = c(0, max(y2), max(y1)), 
  y = c(0, max(I(x^2)), max(I(x^2))), 
  col = "white"
  )

text(
  x = 3000, 
  y = 8000, 
  labels = "Pixel homogeneity", 
  font = 2, 
  cex = 1.5
  )

text(
  x = 8000, 
  y = 2500, 
  labels = "Plot homogeneity", 
  font = 2, 
  cex = 1.5
  )

text(
  x = 8100, 
  y = 7600, 
  labels = "Spatial alignment", 
  font = 2, 
  cex = 1.5
  )

mtext(
  text = paste("     a", ") ",   "Circular plots", sep = ""), 
  adj = 0, 
  line = 1.25, 
  font = 2, 
  cex = 2
  )


# square plot synchrony definition

## description of assumptions panel
plot(
  NA, 
  xlim = c(0, 1500), 
  ylim = c(0, 1500), 
  xaxt = "n", 
  yaxt = "n", 
  xlab = "", 
  ylab = "", 
  bty = "n"
  )

mtext(
  side = 1, 
  text = "     Increasing ->", 
  line = -0.5, 
  cex = 2, 
  adj = 0
  )

mtext(
  side = 2, 
  text = "     Increasing ->", 
  line = -1, 
  cex = 2, 
  adj = 0
  )

mtext(
  side = 1, 
  text = expression(bold(paste("Plot size (ha)", sep = ""))), 
  font = 2, 
  line = 2, 
  cex = 2
  )

mtext(
  side = 2, 
  text = expression(bold(paste("Resolution (ha)", sep = ""))), 
  font = 2, 
  line = 1, 
  cex = 2
  )

polygon(
  x = c(0, 0, 10000), 
  y = c(0, 10000, 10000), 
  col = "lightgrey"
  )

polygon(
  x = c(0, 10000, 10000), 
  y = c(0, 0, 10000), 
  col = "lightgrey"
  )

text(
  x = 750, 
  y = 800, 
  labels = "Spatial alignment", 
  font = 2,
  cex = 1.5, 
  srt = 45
  )

text(
  x = 500, 
  y = 1100, 
  labels = "Pixel homogeneity", 
  font = 2, 
  cex = 1.5
  )

text(
  x = 1000, 
  y = 400, 
  labels = "Plot homogeneity", 
  font = 2, 
  cex = 1.5
  )

mtext(
  text = paste("     b", ") ",   "Square plots", sep = ""), 
  adj = 0, 
  line = 1.25,
  font = 2, 
  cex = 2
  )


