########################################################
# Prediction of valve for detected Brassica napus seeds#
# Also calculate position and space between seeds for  #
# each valve - Single File Version                     #
# Version: 18/07/23 Author: Evangeline Corcoran        #
########################################################


seedpod_2D_single <- function(seed_coord, plot_name) {
  # load required packages
  library(tidyverse)
  library(gplots)
  library(dplyr)

  # remove - from dataframe column names
  colnames(seed_coord) <- gsub("-", ".", colnames(seed_coord))

  #sort seeds in ascending order of Z-axis centroids
  seed_sort_XZ <- seed_coord %>% arrange(centroid.1)

  #save number of seeds in data
  seed_num <- nrow(seed_sort_XZ)

  if (seed_num >= 3){
    #remove outliers on y-axis and x-axis
    seed_sort_XZ$y_axis_z_score <- ((seed_sort_XZ$centroid.1-mean(seed_sort_XZ$centroid.1))/sd(seed_sort_XZ$centroid.1))
    seed_sort_XZ$x_axis_z_score <- ((seed_sort_XZ$centroid.0-mean(seed_sort_XZ$centroid.0))/sd(seed_sort_XZ$centroid.0))
    seed_sort_XZ <- subset(seed_sort_XZ, y_axis_z_score <= 1.5)
    seed_sort_XZ <- subset(seed_sort_XZ, x_axis_z_score <= 1.5)

    #save number of seeds in data
    seed_num <- nrow(seed_sort_XZ)
  } else {
    seed_sort_XZ <- seed_sort_XZ
  }

  #fit lowess line to XZ seed centroids
  if(seed_num <= 5){
    lowess_xy <- lowess(seed_sort_XZ$centroid.1, seed_sort_XZ$centroid.0, f = 1)
    lowess_xy
    #plot(lowess(seed_sort_XZ$centroid.1, seed_sort_XZ$centroid.0, f = 1))
  } else {
    lowess_xy <- lowess(seed_sort_XZ$centroid.1, seed_sort_XZ$centroid.0)
    lowess_xy
    #plot(lowess(seed_sort_XZ$centroid.1, seed_sort_XZ$centroid.0))
  }

  #add lowess y values to sorted seed location data
  seed_sort_XZ$lowess_XZ_y <- lowess_xy$y

  #calculate distance between seed and lowess line
  seed_sort_XZ$lowess_dist <- ((seed_sort_XZ$centroid.0) - (seed_sort_XZ$lowess_XZ_y))

  #categorize seeds into valves base on distance to lowess line
  # and create plot of seeds categorised into valves
  if (seed_num <= 5) {
    seed_sort_XZ$valve <- cut(seed_sort_XZ$lowess_dist,
                              breaks = c(-Inf, -1, Inf),
                              labels = c("Valve 2", "Valve 1"))
    png(plot_name)
    plot(seed_sort_XZ$centroid.1, seed_sort_XZ$centroid.0,
        main ='Predicted Valve for Detected Seeds',
        xlab = 'Predicted Z-axis centroid for seed',
        ylab = 'Predicted X-axis centroid for seed',
        col = c("red","lightblue")[seed_sort_XZ$valve],
        pch = 16,
        cex = 2)
    text(seed_sort_XZ$centroid.0~seed_sort_XZ$centroid.1,
        labels = seed_sort_XZ$label,
        cex = 0.9, 
        font = 2)
    lines(lowess(seed_coord$centroid.1, seed_coord$centroid.0, f = 1),
        col = "darkgreen") # lowess line (x,y)
    legend("bottomright", 
        inset = .002,
        title = 'Seed Pod Valve',
        c('1', '2'), 
        fill = c("lightblue", "red"),
        horiz = TRUE,
        cex = 0.8)
    dev.off()
    } else {
      seed_sort_XZ$valve <- cut(seed_sort_XZ$lowess_dist, 
                                breaks = c(-Inf, 0, Inf),
                                labels = c("Valve 2", "Valve 1"))
      png(plot_name)
      plot(seed_sort_XZ$centroid.1, seed_sort_XZ$centroid.0, 
          main='Predicted Valve for Detected Seeds', 
          xlab = 'Predicted Z-axis centroid for seed',
          ylab = 'Predicted X-axis centroid for seed',
          col = c("red","lightblue")[seed_sort_XZ$valve],
          pch = 16,
          cex = 2)
      text(seed_sort_XZ$centroid.0~seed_sort_XZ$centroid.1, 
          labels=seed_sort_XZ$label, 
          cex=0.9, 
          font=2)
      lines(lowess(seed_coord$centroid.1, seed_coord$centroid.0), 
          col="darkgreen") # lowess line (x,y)
      legend("bottomright", 
          inset = .002,
          title = 'Seed Pod Valve',
          c('1', '2'), 
          fill = c("lightblue", "red"),
          horiz = TRUE,
          cex = 0.8)
      dev.off()
    }

  #valve 1
  #sequence position of seed along valve from beak to pedicel
  seed_pos_v1 <- subset(seed_sort_XZ, valve == 'Valve 1')
  if (nrow(seed_pos_v1) >= 1){
    seed_pos_v1$valve_pos <- seq.int(nrow(seed_pos_v1))
    #calculate space between sequential seeds
    # minor-axis
    valve_1_dist <- diff(seed_pos_v1$bbox.3)
    first_valve_dist <- 'NA' #value for first seed closest to beak is NA
    valve_1_dist <- c(first_valve_dist, valve_1_dist)
    seed_pos_v1$valve_dist <- valve_1_dist
    seed_pos_v1$valve_dist <- as.numeric(seed_pos_v1$valve_dist)
    seed_pos_v1$minor_axis_length <- as.numeric(seed_pos_v1$minor_axis_length)
    seed_pos_v1$valve_dist <- seed_pos_v1$valve_dist - seed_pos_v1$minor_axis_length
    #major-axis
    valve_1_dist2 <- diff(seed_pos_v1$bbox.2)
    first_valve_dist2 <- 'NA' #value for first seed closest to beak is NA
    valve_1_dist2 <- c(first_valve_dist2, valve_1_dist2)
    seed_pos_v1$valve_dist2 <- valve_1_dist2
    seed_pos_v1$valve_dist2 <- as.numeric(seed_pos_v1$valve_dist2)
    seed_pos_v1$major_axis_length <- as.numeric(seed_pos_v1$major_axis_length)
    seed_pos_v1$valve_dist2 <- seed_pos_v1$valve_dist2 - seed_pos_v1$major_axis_length
    #space between seeds
    seed_pos_v1$valve_space_btw <- sqrt((seed_pos_v1$valve_dist^2)+(seed_pos_v1$valve_dist2^2))
  } else {
    seed_pos_v1 <- seed_pos_v1
  }

  #valve 2
  #sequence position of seed along valve from beak to pedicel
  seed_pos_v2 <- subset(seed_sort_XZ, valve == 'Valve 2')
  if (nrow(seed_pos_v2) >= 1){
    seed_pos_v2$valve_pos <- seq.int(nrow(seed_pos_v2))
    #calculate space between sequential seeds
    #minor-axis
    valve_2_dist <- diff(seed_pos_v2$bbox.3)
    valve_2_dist <- c(first_valve_dist, valve_2_dist)
    seed_pos_v2$valve_dist <- valve_2_dist
    seed_pos_v2$valve_dist <- as.numeric(seed_pos_v2$valve_dist)
    seed_pos_v2$minor_axis_length <- as.numeric(seed_pos_v2$minor_axis_length)
    seed_pos_v2$valve_dist <- seed_pos_v2$valve_dist - seed_pos_v2$minor_axis_length
    #major-axis
    valve_2_dist2 <- diff(seed_pos_v2$bbox.2)
    first_valve_dist2 <- 'NA' #value for first seed closest to beak is NA
    valve_2_dist2 <- c(first_valve_dist2, valve_2_dist2)
    seed_pos_v2$valve_dist2 <- valve_2_dist2
    seed_pos_v2$valve_dist2 <- as.numeric(seed_pos_v2$valve_dist2)
    seed_pos_v2$major_axis_length <- as.numeric(seed_pos_v2$major_axis_length)
    seed_pos_v2$valve_dist2 <- seed_pos_v2$valve_dist2 - seed_pos_v2$major_axis_length
    #space between seeds
    seed_pos_v2$valve_space_btw <- sqrt((seed_pos_v2$valve_dist^2)+(seed_pos_v2$valve_dist2^2))
  } else {
    seed_pos_v2 <- seed_pos_v2
  }

  #merge data into final dataframe with all valve info
  seed_valve <- rbind(seed_pos_v1, seed_pos_v2)

  # re-add - into dataframe column names
  colnames(seed_valve) <- gsub("\\.", "-", colnames(seed_valve))
  
  #export data with valve info
  return(seed_valve)
}