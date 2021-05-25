
# Credits ---------------------------

# Created by
# Luara Tourinho (https://github.com/luaratourinho)

# Last update: 21 May 2021


# Required packages

library(tidyverse)
library(dplyr)
library(raster)
library(rgdal)
library(rgeos)

# Read your species list
sp.names <- read.csv("./my_species.csv", head=T)
sp.names <- sp.names$species
n <-length(sp.names)

# OR set a vector
sp.names <- c("Bradypus_torquatus", "Bradypus_tridactylus", "Bradypus_variegatus")
n <-length(sp.names)


# Building table by a loop ---------------------------------------------------

Results_diff_area <- matrix(nrow = n, ncol = 25)
colnames(Results_diff_area) <- c("Species", "unchanged_percent_bin","loss_percent_bin", 
                                 "gain_percent_bin", "gain_loss_perc_pixel", 
                                 "unchanged", "loss", "gain", "total_cells",
                                 "mean_diff_fut_cur_percent", "t", "p",
                                 "ocorrencia_area_cu", "ocorrencia_area_fu",
                                 "loss_area3", "gain_area3", "unchaged_area3",
                                 "perc_loss_rel_pres", "perc_gain_rel_pres",
                                 "net_value_area", "net_value_perc",
                                 "percentage_change", "ocorrencia_area_nogain",
                                 "percentage_change_nogain", "net_value_area_nogain")


for(i in 1:n){
  
  # Read your current and future rasters
  # Binary and continuous projections
  cu <- raster(paste0("./", sp.names[i], "/CUR.bin_", sp.names[i], ".asc"))
  fu <- raster(paste0("./", sp.names[i], "/MPI8570.bin_", sp.names[i], ".asc"))
  cu_cont <- raster(paste0("./", sp.names[i], "/CUR.cont_", sp.names[i], ".asc"))
  fu_cont <- raster(paste0("./", sp.names[i], "/MPI8570.cont_", sp.names[i], ".asc"))
  
  
  #BINARY
  
  cu2 = cu
  cu2[cu2 == 1] <- 2
  # fut-pres
  # 1-2 = -1 -> Here should be 1-1 = 0, i.e., the two have not changed, STABLE
  # 1-0 = 1 -> GAIN
  # 0-2 = -2 ->  Here should be 0-1 = -1, i.e., LOSS
  # 0-0 = 0 -> inadequate area in both scenarios, so the next step we turn into NA
  
  diff_bin <- fu - cu2
  diff_bin[diff_bin == 0] <- NA
 
   
  
  # Values and percentage related to the number of pixels
  
  unchanged <- ncell(which(diff_bin[] == -1))
  loss <- ncell(which(diff_bin[] == -2))
  gain <- ncell(which(diff_bin[] == 1))
  total_cells <- unchanged + loss + gain
  
  unchanged_percent_bin <- (unchanged/total_cells)*100
  loss_percent_bin <- (loss/total_cells)*100
  gain_percent_bin <- (gain/total_cells)*100
  gain_loss_perc_pixel <- gain_percent_bin - loss_percent_bin
  
  
  # Reclassing to -1, zero and 1 to save the raster
  # This is a raster that you could present in your final work
  
  diff_bin2 = diff_bin
  diff_bin2[diff_bin2 == -1] <- 0
  diff_bin2[diff_bin2 == -2] <- -1
  
  writeRaster(diff_bin2, filename = paste0("./Diff_raster/", sp.names[i],
                                          "_diff_bin.tif"),
              format="GTiff", overwrite=T)
  
  
  # CONTINUOUS
  
  # Evaluating the difference between future and current continuous projections
  # Mean of difference - Negative value means loss, positive value means gain
  
  testt <- t.test(fu_cont[], cu_cont[], paired=T)
  chars <- capture.output(print(testt))
  mean_diff_estimate <- as.data.frame(testt$estimate)
  mean_diff_fut_cur <- mean_diff_estimate[1,]
  mean_diff_fut_cur_percent <- mean_diff_fut_cur*100
  mean_diff_conf.int <- as.data.frame(testt$conf.int)
  t <- testt$statistic
  p <- testt$p.value
  
  diff_cont <- fu_cont - cu_cont
  writeRaster(diff_cont, filename = paste0("./Diff_raster/", sp.names[i],
                                           "_diff_cont.tif"),
              format="GTiff", overwrite=T)
  
  
  
  # AREA --------------------------------------------------------------------
  
  # To calculate the areas, we normally use binary results
  
  # Values and percentage related to the area in km2
  
  #current
  cu_area = cu
  cu_area[cu_area == 0] <- NA
  r = raster(nrow = cu_area@nrows, ncol = cu_area@ncols, xmn = cu_area@extent@xmin, 
             xmx = cu_area@extent@xmax, ymn = cu_area@extent@ymin, ymx = cu_area@extent@ymax) 
  # calculate area of each cell (the area changes along the latitudes / longitudes)
  x = raster::area(r) 
  #plot(x)
  ocorrencia_area = x * cu_area
  ocorrencia_area_cu <- cellStats(ocorrencia_area, stat='sum', na.rm=TRUE, asSample=TRUE)
  
  #future
  fu_area = fu 
  fu_area[fu_area == 0] <- NA
  ocorrencia_area2 = x * fu_area
  ocorrencia_area_fu <- cellStats(ocorrencia_area2, stat='sum', na.rm=TRUE, asSample=TRUE)
  
  # Expected area to contract (loss)
  
  loss_area = diff_bin 
  loss_area[loss_area == -1] <- NA
  loss_area[loss_area == 1] <- NA
  loss_area[loss_area == -2] <- 1
  loss_area2 = x * loss_area
  loss_area3 <- cellStats(loss_area2, stat='sum', na.rm=TRUE, asSample=TRUE)
  # Percentage of loss related to the current area
  perc_loss_rel_pres <- (loss_area3*100)/ocorrencia_area_cu
  
  # Expected area to expand (gain)
  
  gain_area = diff_bin 
  gain_area[gain_area == -1] <- NA
  gain_area[gain_area == -2] <- NA
  gain_area2 = x * gain_area
  gain_area3 <- cellStats(gain_area2, stat='sum', na.rm=TRUE, asSample=TRUE)
  # Percentage of gain related to the current area
  perc_gain_rel_pres <- (gain_area3*100)/ocorrencia_area_cu
  
  
  # Expected area not to change (unchanged)
  
  unchaged_area = diff_bin 
  unchaged_area[unchaged_area == 1] <- NA
  unchaged_area[unchaged_area == -2] <- NA
  unchaged_area[unchaged_area == -1] <- 1
  unchaged_area2 = x * unchaged_area
  unchaged_area3 <- cellStats(unchaged_area2, stat='sum', na.rm=TRUE, asSample=TRUE)
  
  
  # How much did change?
  
  # Net value in area and in percentage
  net_value_area <- gain_area3 - loss_area3
  net_value_perc <- perc_gain_rel_pres - perc_loss_rel_pres
  
  # Percentage of Change = (FV/IV - 1) * 100
  # Where FV is the final value of the transaction, whereas IV refers to the initial value.
  # The increase was ((700/500) - 1) * 100 = 0,4 × 100 = 40%.
  # The loss was ((500/700) - 1)* 100 = -29%
  percentage_change <- ((ocorrencia_area_fu/ocorrencia_area_cu) - 1)*100
  
  
# Non-dispersal scenario --------------------------------------------------
  
  diff_bin_perda <- diff_bin
  diff_bin_perda[diff_bin_perda == 1] <- NA
  diff_bin_perda[diff_bin_perda == -1] <- 1
  diff_bin_perda[diff_bin_perda == -2] <- NA
  ocorrencia_area = x * diff_bin_perda
  ocorrencia_area_nogain <- cellStats(ocorrencia_area, stat='sum', na.rm=TRUE, asSample=TRUE)
  
  percentage_change_nogain <- ((ocorrencia_area_nogain/ocorrencia_area_cu) - 1)*100
  net_value_area_nogain <- 0 - loss_area3

  
  Results_diff_area[i, ] <- c(sp.names[i], unchanged_percent_bin, loss_percent_bin,
                              gain_percent_bin, gain_loss_perc_pixel, 
                              unchanged, loss, gain, total_cells,
                              mean_diff_fut_cur_percent, t, p,
                              ocorrencia_area_cu, ocorrencia_area_fu,
                              loss_area3, gain_area3, unchaged_area3,
                              perc_loss_rel_pres, perc_gain_rel_pres,
                              net_value_area, net_value_perc,
                              percentage_change, ocorrencia_area_nogain,
                              percentage_change_nogain, net_value_area_nogain)
  
}

write.csv(Results_diff_area, "./9_calculating_areas.csv", row.names = F)

