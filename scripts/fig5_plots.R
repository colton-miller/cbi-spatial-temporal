library(dplyr)
library(forcats)

# ASI boxplots by plot shape

summary(sync_size$circ_asi)
summary(sync_size$square_asi)


smoothing_order = c("Not specified","focal mean", "none",
                    "mean of sample points within \n      plot", "bilinear","area weighted",
                    "mean of values of pixels within \n      distance of plot",
                    "object-based")
smoothing_names = c("Not specified", "Focal mean", "None", 
                    "Mean of sample \npoints within plot","Bilinear", "Area weighted", 
                    "Mean of values \nof pixels within \ndistance of plot", 
                    "Object-based")
text_size = 1.55
asi_n_line = 0.5
asi_neg_line = 0.5


# 
table(sync_size_circular$Smoothing, sync_size_circular$sign)
table(sync_size_square$Smoothing, sync_size_square$sign)


# circular plots

# check max so can define appropriate limits on plots
max(sync_size_circular$circ_asi_abs)

plot_list = vector(mode = "list", length = 7)
for (i in 1:7) {
  
  # define smoothing method so can subset to that type
  smooth_method = smoothing_order[i]
  smooth_name = smoothing_names[i]
  
  # create a subsetted dataframe to specific smoothing type
  data_subset = sync_size_circular[sync_size_circular$Smoothing == smooth_method, ]
  
  # calculate number of rows that were spatially synchronous (no assumption)
  n_no_assumptoin = nrow(data_subset[data_subset$sign == "no assumption", ])
  
  # calculate number of rows with some assumption
  pos_assumption = data_subset[data_subset$sign != "no assumption", ]
  pos_assumption$sign = droplevels(pos_assumption$sign)
  
  # reassign levels to shorthand naming
  both_levels <- length(unique(pos_assumption$sign)) == 2
  plot_homogeneity <- unique(pos_assumption$sign) == "Plot homogeneity"
  pixel_homogeneity <- unique(pos_assumption$sign) == "Pixel homogeneity"
  plot_homogeneity <- if (length(plot_homogeneity) == 0) FALSE else plot_homogeneity
  pixel_homogeneity <- if (length(pixel_homogeneity) == 0) FALSE else pixel_homogeneity
  
  if (both_levels) {
    pos_assumption$sign <- fct_recode(
      pos_assumption$sign, 
      "Pixel" = "Pixel homogeneity", 
      "Plot" = "Plot homogeneity"
    )
  } else if (plot_homogeneity) {
    pos_assumption$sign <- fct_recode(
      pos_assumption$sign, 
      "Plot" = "Plot homogeneity"
    )
  } else if (pixel_homogeneity) {
    pos_assumption$sign <- fct_recode(
      pos_assumption$sign, 
      "Pixel" = "Pixel homogeneity", 
    )
  } else {
    invisible()
  }


  
  # if more than one row with assumption, count number of comparisons
  # for each type (and proceed to plotting)
  if (nrow(pos_assumption) > 0) {
    n_pos_assumption = aggregate(
      x = paste0(pos_assumption$Citation, pos_assumption$Fire_name), 
      by = list(Smoothing = pos_assumption$Smoothing, Assumption = pos_assumption$sign), 
      FUN = length
      )
    
    # if there is only one type of assumption (pixel or plot), 
    # then discover which and create 0 for the other
    if(nrow(n_pos_assumption) == 1) {
      if (n_pos_assumption$Assumption == "Pixel") {
        
        # cast Assumption as character, add row, refactor
        n_pos_assumption$Assumption <- as.character(n_pos_assumption$Assumption)
        n_pos_assumption[2, ] = c(smooth_method, "Plot", 0)
        n_pos_assumption$Assumption <- as.factor(n_pos_assumption$Assumption)
        
        pos_assumption$sign <- factor(pos_assumption$sign, levels = c("Pixel", "Plot"))
        
        
      } else{
        # cast Assumption as character, add row, refactor
        n_pos_assumption$Assumption <- as.character(n_pos_assumption$Assumption)
        n_pos_assumption[2, ] = c(smooth_method, "Pixel", 0)
        n_pos_assumption$Assumption <- as.factor(n_pos_assumption$Assumption)
        
        pos_assumption$sign <- factor(pos_assumption$sign, levels = c("Pixel", "Plot"))
        
      }
    }
    
    # create text objects to add to plots number of comparison for each 
    # assumption type
    text_no_assumption = textGrob(
      paste0("(No assumption = ", n_no_assumptoin,")"), 
      gp = gpar(fontsize = 14)
    )
    
    text_pixel_assumption = textGrob(
      paste("N =", n_pos_assumption[n_pos_assumption$Assumption == "Pixel", ]$x), 
      gp = gpar(fontsize = 14)
    )
    
    text_plot_assumption = textGrob(
      paste("N =", n_pos_assumption[n_pos_assumption$Assumption == "Plot", ]$x), 
      gp = gpar(fontsize = 14)
    )
    
    # calculate group size for knowing whether to plot violin or scatter plot
    pos_assumption <- pos_assumption %>%
      group_by(sign) %>%
      mutate(group_size = n()) %>%
      ungroup()
    
    # create plot for first smoothing type (assuming n > 0)
    if( i == 1) {
      
      p = ggplot(data = pos_assumption, aes(x = sign, y = circ_asi_abs)) +
        geom_violin(data = filter(pos_assumption, group_size > 1), trim = TRUE, fill = grey(0.8), drop = FALSE) +
        geom_jitter(shape=16, position=position_jitter(0.05)) +
        scale_color_grey() + theme_classic() +
        scale_x_discrete(drop=F)+
        theme(text = element_text(size = 20),
              plot.margin = unit(c(2,1,3,1), "lines")) +
        xlab(smooth_name) +
        ylab("|Assumption Strength Index (ASI)|") +
        ylim(c(-1, 100)) +
        annotation_custom(text_no_assumption, ymin = -60, ymax = -1) +
        annotation_custom(text_pixel_assumption, ymin = -2, ymax = -2, xmin = 1, xmax = 1) +
        annotation_custom(text_plot_assumption, ymin = -2, ymax = -2, xmin = 2, xmax = 2) +
        coord_cartesian(clip = "off")
      
      # create plots for all other smoothing types (assuming n > 0)
    } else {
      p = ggplot(data = pos_assumption, aes(x = sign, y = circ_asi_abs)) +
        geom_violin(data = filter(pos_assumption, group_size > 1), trim = TRUE, fill = grey(0.8), drop = FALSE) +
        geom_jitter(shape=16, position=position_jitter(0.05)) +
        scale_color_grey() + theme_classic() +
        scale_x_discrete(drop=F)+
        theme(text = element_text(size = 20),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              axis.line.y = element_blank(),
              axis.title.y = element_blank()) +
        xlab(smooth_name) +
        ylab("|Assumption Strength Index (ASI)|") +
        ylim(c(-1, 100)) +
        annotation_custom(text_no_assumption, ymin = -60, ymax = -1) +
        annotation_custom(text_pixel_assumption, ymin = -2, ymax = -2, xmin = 1, xmax = 1) +
        annotation_custom(text_plot_assumption, ymin = -2, ymax = -2, xmin = 2, xmax = 2) +
        coord_cartesian(clip = "off")
      
    }
    
    # create empty plots for when n = 0
  } else {
    
    text_pixel_assumption = textGrob(
      paste("N = 0"), 
      gp = gpar(fontsize = 14)
      )
    
    text_plot_assumption = textGrob(
      paste("N = 0"), 
      gp = gpar(fontsize = 14)
      )
    
    
    text_no_assumption = textGrob(
      paste0("(No assumption = ", n_no_assumptoin,")"), 
      gp = gpar(fontsize = 14)
    )
    
    data_empty <- data.frame(
      sign = factor(levels = c("Pixel", "Plot")),
      circ_asi_abs = numeric()
    )
    p =  ggplot() + # data_empty,  aes(x = sign, y = circ_asi_abs)) +
      geom_violin() +
      ylim(-1, 100) +
      xlab(smooth_name) +
      scale_color_grey() +
      scale_x_discrete(limits = c("Pixel", "Plot")) +  # Set the same scale as in real plot
      theme_classic() +
      theme(text = element_text(size = 20),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.line.y = element_blank(),
            axis.title.y = element_blank())+
      annotation_custom(text_no_assumption, ymin = -60, ymax = -1) +
      annotation_custom(text_pixel_assumption, ymin = -2, ymax = -2, xmin = 1, xmax = 1) +
      annotation_custom(text_plot_assumption, ymin = -2, ymax = -2, xmin = 2, xmax = 2) +
      coord_cartesian(clip = "off")

    
  }
  
  # add plot to list
  plot_list[[i]] = p
  
  # Vector of object names to remove
  objects_to_remove <- c("smooth_method", "smooth_name", "data_subset", "n_no_assumptoin", 
                         "pos_assumption", "n_pos_assumption", "text_no_assumption", 
                         "text_pixel_assumption", "text_plot_assumption", "p")
  
  # Use lapply to remove objects only if they exist
  lapply(objects_to_remove, function(obj) {
    if (exists(obj, envir = .GlobalEnv)) {
      rm(list = obj, envir = .GlobalEnv)
    }
  })
}


plot_a = ggarrange(plot_list[[1]], 
                   plot_list[[2]], 
                   plot_list[[3]], 
                   plot_list[[4]], 
                   plot_list[[5]], 
                   plot_list[[6]], 
                   plot_list[[7]], 
                   ncol = 7, align = "h")


# square plots


# check max so can define appropriate limits on plots
max(sync_size_square$square_asi_abs)

plot_list = vector(mode = "list", length = 7)
for (i in 1:7) {

  # define smoothing method so can subset to that type
  smooth_method = smoothing_order[i]
  smooth_name = smoothing_names[i]
  
  # create a subsetted dataframe to specific smoothing type
  data_subset = sync_size_square[sync_size_square$Smoothing == smooth_method, ]
  
  # calculate number of rows that were spatially synchronous (no assumption)
  n_no_assumptoin = nrow(data_subset[data_subset$sign == "no assumption", ])
  
  # calculate number of rows with some assumption
  pos_assumption = data_subset[data_subset$sign != "no assumption", ]
  pos_assumption$sign = droplevels(pos_assumption$sign)
  
  # reassign levels to shorthand naming
  both_levels <- length(unique(pos_assumption$sign)) == 2
  plot_homogeneity <- unique(pos_assumption$sign) == "Plot homogeneity"
  pixel_homogeneity <- unique(pos_assumption$sign) == "Pixel homogeneity"
  plot_homogeneity <- if (length(plot_homogeneity) == 0) FALSE else plot_homogeneity
  pixel_homogeneity <- if (length(pixel_homogeneity) == 0) FALSE else pixel_homogeneity
  
  if (both_levels) {
    pos_assumption$sign <- fct_recode(
      pos_assumption$sign, 
      "Pixel" = "Pixel homogeneity", 
      "Plot" = "Plot homogeneity"
    )
  } else if (plot_homogeneity) {
    pos_assumption$sign <- fct_recode(
      pos_assumption$sign, 
      "Plot" = "Plot homogeneity"
    )
  } else if (pixel_homogeneity) {
    pos_assumption$sign <- fct_recode(
      pos_assumption$sign, 
      "Pixel" = "Pixel homogeneity", 
    )
  } else {
    invisible()
  }
  
  # if more than one row with assumption, count number of comparisons
  # for each type (and proceed to plotting)
  if (nrow(pos_assumption) > 0) {
    n_pos_assumption = aggregate(
      x = paste0(pos_assumption$Citation, pos_assumption$Fire_name), 
      by = list(Smoothing = pos_assumption$Smoothing, Assumption = pos_assumption$sign), 
      FUN = length
    )
    
    # if there is only one type of assumption (pixel or plot), 
    # then discover which and create 0 for the other
    if(nrow(n_pos_assumption) == 1) {
      if (n_pos_assumption$Assumption == "Pixel") {
        
        # cast Assumption as character, add row, refactor
        n_pos_assumption$Assumption <- as.character(n_pos_assumption$Assumption)
        n_pos_assumption[2, ] = c(smooth_method, "Plot", 0)
        n_pos_assumption$Assumption <- as.factor(n_pos_assumption$Assumption)
        
        pos_assumption$sign <- factor(pos_assumption$sign, levels = c("Pixel", "Plot"))
        
        
      } else{
        # cast Assumption as character, add row, refactor
        n_pos_assumption$Assumption <- as.character(n_pos_assumption$Assumption)
        n_pos_assumption[2, ] = c(smooth_method, "Pixel", 0)
        n_pos_assumption$Assumption <- as.factor(n_pos_assumption$Assumption)
        
        pos_assumption$sign <- factor(pos_assumption$sign, levels = c("Pixel", "Plot"))
        
      }
    }
    
    
    
    # create text objects to add to plots number of comparison for each 
    # assumption type
    text_no_assumption = textGrob(
      paste0("(No assumption = ", n_no_assumptoin,")"), 
      gp = gpar(fontsize = 14)
    )
    
    text_pixel_assumption = textGrob(
      paste("N =", n_pos_assumption[n_pos_assumption$Assumption == "Pixel", ]$x), 
      gp = gpar(fontsize = 14)
    )
    
    text_plot_assumption = textGrob(
      paste("N =", n_pos_assumption[n_pos_assumption$Assumption == "Plot", ]$x), 
      gp = gpar(fontsize = 14)
    )
    
    # create plot for first smoothing type (assuming n > 0)
    if( i == 1) {
      
      p = ggplot(data = pos_assumption, aes(x = sign, y = square_asi_abs)) +
        geom_violin(trim = TRUE, fill = grey(0.8), drop = FALSE) +
        geom_jitter(shape=16, position=position_jitter(0.05)) +
        scale_color_grey() + theme_classic() +
        scale_x_discrete(drop=F)+
        theme(text = element_text(size = 20),
              plot.margin = unit(c(2,1,3,1), "lines")) +
        xlab(smooth_name) +
        ylab("|Assumption Strength Index (ASI)|") +
        ylim(c(-1, 100)) +
        annotation_custom(text_no_assumption, ymin = -60, ymax = -1) +
        annotation_custom(text_pixel_assumption, ymin = -2, ymax = -2, xmin = 1, xmax = 1) +
        annotation_custom(text_plot_assumption, ymin = -2, ymax = -2, xmin = 2, xmax = 2) +
        coord_cartesian(clip = "off")
      
      # create plots for all other smoothing types (assuming n > 0)
    } else {
      p = ggplot(data = pos_assumption, aes(x = sign, y = square_asi_abs)) +
        geom_violin(trim = TRUE, fill = grey(0.8), drop = FALSE) +
        geom_jitter(shape=16, position=position_jitter(0.05)) +
        scale_color_grey() + theme_classic() +
        scale_x_discrete(drop=F)+
        theme(text = element_text(size = 20),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              axis.line.y = element_blank(),
              axis.title.y = element_blank()) +
        xlab(smooth_name) +
        ylab("|Assumption Strength Index (ASI)|") +
        ylim(c(-1, 100)) +
        annotation_custom(text_no_assumption, ymin = -60, ymax = -1) +
        annotation_custom(text_pixel_assumption, ymin = -2, ymax = -2, xmin = 1, xmax = 1) +
        annotation_custom(text_plot_assumption, ymin = -2, ymax = -2, xmin = 2, xmax = 2) +
        coord_cartesian(clip = "off")
    }
    
    # create empty plots for when n = 0
  } else {
    
    text_pixel_assumption = textGrob(
      paste("N = 0"), 
      gp = gpar(fontsize = 14)
    )
    
    text_plot_assumption = textGrob(
      paste("N = 0"), 
      gp = gpar(fontsize = 14)
    )
    
    text_no_assumption = textGrob(
      paste0("(No assumption = ", n_no_assumptoin,")"), 
      gp = gpar(fontsize = 14)
    )
    
    data_empty <- data.frame(
      sign = factor(levels = c("Pixel", "Plot")),
      circ_asi_abs = numeric()
    )
    p =  ggplot() + # data_empty,  aes(x = sign, y = circ_asi_abs)) +
      geom_violin() +
      ylim(-1, 100) +
      xlab(smooth_name) +
      scale_color_grey() +
      scale_x_discrete(limits = c("Pixel", "Plot")) +  # Set the same scale as in real plot
      theme_classic() +
      theme(text = element_text(size = 20),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.line.y = element_blank(),
            axis.title.y = element_blank())+
      annotation_custom(text_no_assumption, ymin = -60, ymax = -1) +
      annotation_custom(text_pixel_assumption, ymin = -3, ymax = -3, xmin = 1, xmax = 1) +
      annotation_custom(text_plot_assumption, ymin = -3, ymax = -3, xmin = 2, xmax = 2) +
      coord_cartesian(clip = "off")
    
  }
  
  # add plot to list
  plot_list[[i]] = p
  
  # Vector of object names to remove
  objects_to_remove <- c("smooth_method", "smooth_name", "data_subset", "n_no_assumptoin", 
                         "pos_assumption", "n_pos_assumption", "text_no_assumption", 
                         "text_pixel_assumption", "text_plot_assumption", "p")
  
  # Use lapply to remove objects only if they exist
  lapply(objects_to_remove, function(obj) {
    if (exists(obj, envir = .GlobalEnv)) {
      rm(list = obj, envir = .GlobalEnv)
    }
  })
  
}


plot_b = ggarrange(plot_list[[1]], 
                   plot_list[[2]], 
                   plot_list[[3]], 
                   plot_list[[4]], 
                   plot_list[[5]], 
                   plot_list[[6]], 
                   plot_list[[7]], 
                   ncol = 7, align = "h")

