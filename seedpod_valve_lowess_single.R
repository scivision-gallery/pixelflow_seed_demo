########################################################
# Prediction of valve for detected Brassica napus seeds#
# Also calculate position and space between seeds for  #
# each valve - Single File Version                     #
# Version: 04/10/21 Author: Evangeline Corcoran        #
########################################################

#Install and load required packages
install.packages('tidyverse')
install.packages('gplots')

library(tidyverse)
library(gplots)


#load detected seed data
setwd('~/coord-20220614T135111Z-001/coord')
seed_coord <- read.csv('C0007865_coord.csv')
        
#sort seeds in ascending order of Z-axis centroids
seed_sort_XZ <- seed_coord %>% arrange(centroid.0)

#save number of seeds in data
seed_num <- nrow(seed_sort_XZ)
seed_num

#fit lowess line to XZ seed centroids
if(seed_num <= 10){
        lowess_xy <- lowess(seed_coord$centroid.0, seed_coord$centroid.2, f = 1)
        lowess_xy
        plot(lowess(seed_coord$centroid.0, seed_coord$centroid.2, f = 1))
        } else {
        lowess_xy <- lowess(seed_coord$centroid.0, seed_coord$centroid.2)
        lowess_xy
        plot(lowess(seed_coord$centroid.0, seed_coord$centroid.2))
                }

#add lowess y values to sorted seed location data
seed_sort_XZ$lowess_XZ_y <- lowess_xy$y
        
#calculate distance between seed and lowess line
seed_sort_XZ$lowess_dist <- ((seed_sort_XZ$centroid.2) - (seed_sort_XZ$lowess_XZ_y))
        
#categorize seeds into valves base on distance to lowess line
# and create plot of seeds categorised into valves
if (seed_num <= 10) {
        seed_sort_XZ$valve <- cut(seed_sort_XZ$lowess_dist, 
                                  breaks = c(-Inf, -1, Inf),
                                  labels = c("Valve 2", "Valve 1"))
        plot(seed_sort_XZ$centroid.0, seed_sort_XZ$centroid.2, 
             main ='Predicted Valve for Detected Seeds', 
             xlab = 'Predicted Z-axis centroid for seed', 
             ylab = 'Predicted X-axis centroid for seed', 
             col = c("red","lightblue")[seed_sort_XZ$valve], 
             pch = 16, 
             cex = 2)
        text(seed_sort_XZ$centroid.2~seed_sort_XZ$centroid.0, 
             labels = seed_sort_XZ$label, 
             cex = 0.9, 
             font = 2)
        lines(lowess(seed_coord$centroid.0, seed_coord$centroid.2, 
                     f = 1), 
              col = "darkgreen") # lowess line (x,y)
        legend("bottomright", 
               inset = .002,
               title = 'Seed Pod Valve',
               c('1', '2'), 
               fill = c("lightblue", "red"),
               horiz = TRUE,
               cex = 0.8)
        } else {
                seed_sort_XZ$valve <- cut(seed_sort_XZ$lowess_dist, 
                                          breaks = c(-Inf, 0, Inf),
                                          labels = c("Valve 2", "Valve 1"))
                plot(seed_sort_XZ$centroid.0, seed_sort_XZ$centroid.2, 
                     main='Predicted Valve for Detected Seeds', 
                     xlab = 'Predicted Z-axis centroid for seed',
                     ylab = 'Predicted X-axis centroid for seed',
                     col = c("red","lightblue")[seed_sort_XZ$valve],
                     pch = 16,
                     cex = 2)
                text(seed_sort_XZ$centroid.2~seed_sort_XZ$centroid.0, 
                     labels=seed_sort_XZ$label, 
                     cex=0.9, 
                     font=2)
                lines(lowess(seed_coord$centroid.0, seed_coord$centroid.2), 
                      col="darkgreen") # lowess line (x,y)
                legend("bottomright", 
                       inset = .002,
                       title = 'Seed Pod Valve',
                       c('1', '2'), 
                       fill = c("lightblue", "red"),
                       horiz = TRUE,
                       cex = 0.8)
        }
        
        
#valve 1
#sequence position of seed along valve from beak to pedicel
seed_pos_v1 <- subset(seed_sort_XZ, valve == 'Valve 1')
seed_pos_v1$valve_pos <- seq.int(nrow(seed_pos_v1))
#calculate space between sequential seeds
valve_1_dist <- diff(seed_pos_v1$centroid.0)
first_valve_dist <- 'NA' #value for first seed closest to beak is NA
valve_1_dist <- c(first_valve_dist, valve_1_dist)
seed_pos_v1$valve_dist <- valve_1_dist
        
#valve 2
#sequence position of seed along valve from beak to pedicel
seed_pos_v2 <- subset(seed_sort_XZ, valve == 'Valve 2')
seed_pos_v2$valve_pos <- seq.int(nrow(seed_pos_v2))
#calculate space between sequential seeds
valve_2_dist <- diff(seed_pos_v2$centroid.0)
valve_2_dist <- c(first_valve_dist, valve_2_dist)
seed_pos_v2$valve_dist <- valve_2_dist
        
        
#merge data into final dataframe with all valve info
seed_valve <- rbind(seed_pos_v1, seed_pos_v2)
        
#export data with valve info
write.csv(seed_valve, 'C0007865_coord_with_valve.csv')
