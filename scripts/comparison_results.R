# load libraries
library(ggplot2)
library(ggpubr)
library(grid)



##### Analysis #####

# data sets used in each analysis:
# tables: 
# fig3_plots: sync_size
# fig4_plots: sync_size
# fig5_plots: sync_size_circular, sync_size_square
# fig6_plots: sync_fire_plot, sync_plot_imagery
# fig7_plots: sync_size_circular, sync_size_square, sync_fire_plot, sync_plot_imagery

# table outputs
source(file.path(repo_dir, "scripts", "tables.R"))
write.csv(
  circ_square_studies, 
  file = "tables/circ_square_plots_citations.csv", 
  row.names = FALSE
  )
write.csv(
  circ_square_comparisons, 
  file = "tables/circ_square_plots_comparisons.csv", 
  row.names = FALSE
  )
write.csv(
  smoothing_type_by_shape_both, 
  file = "tables/smoothing_type_by_shape.csv", 
  row.names = FALSE
  )
write.csv(
  image_type_by_shape, 
  file = "tables/image_type_by_shape.csv", 
  row.names = TRUE
  )

# assumptions definition plots
plot_dims <- calc_plot_dims(w = 1000, h = 500, orig_res = 72, target_res = 300)
png(
  filename = "plots/MillerFig2.png", 
  width = plot_dims$w, 
  height = plot_dims$h, 
  res = plot_dims$r
  )
source(file.path(repo_dir, "scripts", "fig2_plots.R"))
dev.off()

# circular field plots spatial synchrony
plot_dims <- calc_plot_dims(w = 800, h = 800, orig_res = 72, target_res = 300)
png(
  filename = "plots/MillerFig3.png", 
  width = plot_dims$w, 
  height = plot_dims$h, 
  res = plot_dims$r
  )
source(file.path(repo_dir, "scripts", "fig3_plots.R"))
dev.off()

# square field plots spatial synchrony
plot_dims <- calc_plot_dims(w = 800, h = 533, orig_res = 72, target_res = 300)
png(
  filename = "plots/MillerFig4.png", 
  width = plot_dims$w, 
  height = plot_dims$h, 
  res = plot_dims$r
  )
source(file.path(repo_dir, "scripts", "fig4_plots.R"))
dev.off()

# ASI boxplots
# save circular and square plot ASI into one figure
plot_dims <- calc_plot_dims(w = 1400, h = 1200, orig_res = 72, target_res = 300)
png(
  filename = "plots/MillerFig5.png", 
  width = plot_dims$w, 
  height = plot_dims$h, 
  res = plot_dims$r
  )
source(file.path(repo_dir, "scripts", "fig5_plots.R"))
ggarrange(
  annotate_figure(
    plot_a, 
    fig.lab = "a) Circular plots", 
    fig.lab.size = 20, 
    fig.lab.pos = c("top.left"), 
    fig.lab.face = "bold"
    ),
  NULL,
  annotate_figure(
    plot_b, 
    fig.lab = "b) Square plots", 
    fig.lab.size = 20, 
    fig.lab.pos = c("top.left"), 
    fig.lab.face = "bold"
    ),
  ncol = 1, 
  heights = c(1, 0.05, 1)
  )
dev.off()

# temporal synchrony plots
plot_dims <- calc_plot_dims(w = 1100, h = 1100, orig_res = 72, target_res = 300)
png(
  filename = "plots/MillerFig6.png", 
  width = plot_dims$w, 
  height = plot_dims$h, 
  res = plot_dims$r
  )
source(file.path(repo_dir, "scripts", "fig6_plots.R"))
dev.off()

write.csv(
  plot_delay_after_fire,
  file = "tables/imagery_delay_after_plots.csv",
  row.names = FALSE
  )

write.csv(
  imagery_delay_after_plots,
  file = "tables/imagery_delay_after_plots.csv",
  row.names = FALSE
)

# plots of synchrony and alingment vs. R2
plot_dims <- calc_plot_dims(w = 800, h = 800, orig_res = 72, target_res = 300)
png(
  filename = "plots/MillerFig7.png", 
  width = plot_dims$w, 
  height = plot_dims$h, 
  res = plot_dims$r
)
source(file.path(repo_dir, "scripts", "fig7_plots.R"))
dev.off()

write.csv(
  sync_size_circular_summary_table,
  file = "tables/sync_size_circular_summary_table.csv",
  row.names = FALSE
)
if (exists("sync_size_square_summary_table")) {
  write.csv(
  sync_size_square_summary_table,
  file = "tables/sync_size_square_summary_table.csv",
  row.names = FALSE
  )
}

write.csv(
  sync_temp_plot_fire_summary_table,
  file = "tables/sync_temp_plot_fire_summary_table.csv",
  row.names = FALSE
)
write.csv(
  sync_temp_rs_plot_summary_table,
  file = "tables/sync_temp_rs_plot_summary_table.csv",
  row.names = FALSE
)




