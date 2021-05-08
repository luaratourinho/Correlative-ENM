
# Credits ---------------------------

# Created by
# Luara Tourinho (https://github.com/luaratourinho)

# Date: 08 may 2021


# Cropping variables from Worldclim ---------------------------------------


# Required packages 

library(raster)
library(dplyr)
library(rgeos)
library(reshape)
library(rgdal)


# Opening GCMs ------------------------------------------------------------

# Now, for other GCMs
# And selected variables
# By Spearman test (0.6): 
# "X_wc2.1_2.5m_bio_15" "X_wc2.1_2.5m_bio_18" 
# "X_wc2.1_2.5m_bio_3"   "X_wc2.1_2.5m_bio_8"

## Future variables ------------------------------------------------------

to_crop <- raster('./data/env/present/wc2.1_2.5m_bio_1.tif')
#to_crop <- envi.mask2[[1]]

# If future biovariables come separately, build a list like you did before


# BCC ---------------------------------------------------------------------

# spp126 ------------------------------------------------------------------

# Ps. 2021-2040 was done in the step 5_crop_environmental_variables.R

# 2041-2060

future_var_stk <- stack("./data/env/future/wc2.1_2.5m_bioc_BCC-CSM2-MR_ssp126_2041-2060.tif")
envi_fut.cut<- crop(future_var_stk, to_crop)

# Save all variables
envi_fut.mask<- mask(envi_fut.cut, to_crop)
dir.create(paste0("./data/env_cropped/future/bc25_ssp126_41-60"))
writeRaster(envi_fut.mask, filename='./data/env_cropped/future/bc25_ssp126_41-60/', format="GTiff", 
            bylayer=TRUE, suffix="names", overwrite=TRUE)


# Save only that selected variables 
envi_fut_sel <- envi_fut.mask[[c(3,8,15,18)]]
names(envi_fut_sel) <- c("bio3", "bio8", "bio15","bio18")
dir.create(paste0("./data/env_sel/future/bc25_ssp126_41-60"))
writeRaster(envi_fut_sel, filename='./data/env_sel/future/bc25_ssp126_41-60/', format="GTiff", 
            bylayer=TRUE, suffix="names", overwrite=TRUE)


# 2061-2080

future_var_stk <- stack("./data/env/future/wc2.1_2.5m_bioc_BCC-CSM2-MR_ssp126_2061-2080.tif")
envi_fut.cut<- crop(future_var_stk, to_crop)
envi_fut.mask<- mask(envi_fut.cut, to_crop)

# Save all variables
dir.create(paste0("./data/env_cropped/future/bc25_ssp126_61-80"))
writeRaster(envi_fut.mask, filename='./data/env_cropped/future/bc25_ssp126_61-80/', format="GTiff", 
            bylayer=TRUE, suffix="names", overwrite=TRUE)


# Save only that selected variables 
envi_fut_sel <- envi_fut.mask[[c(3,8,15,18)]]
names(envi_fut_sel) <- c("bio3", "bio8", "bio15","bio18")
#dir.create(paste0("./data/env_sel/future/bc25_ssp126_61-80"))
writeRaster(envi_fut_sel, filename='./data/env_sel/future/bc25_ssp126_61-80/', format="GTiff", 
            bylayer=TRUE, suffix="names", overwrite=TRUE)



# 2081-2100

future_var_stk <- stack("./data/env/future/wc2.1_2.5m_bioc_BCC-CSM2-MR_ssp126_2081-2100.tif")
envi_fut.cut<- crop(future_var_stk, to_crop)
envi_fut.mask<- mask(envi_fut.cut, to_crop)

# Save all variables
dir.create(paste0("./data/env_cropped/future/bc25_ssp126_81-00"))
writeRaster(envi_fut.mask, filename='./data/env_cropped/future/bc25_ssp126_81-00/', format="GTiff", 
            bylayer=TRUE, suffix="names", overwrite=TRUE)


# Save only that selected variables 
envi_fut_sel <- envi_fut.mask[[c(3,8,15,18)]]
names(envi_fut_sel) <- c("bio3", "bio8", "bio15","bio18")
#dir.create(paste0("./data/env_sel/future/bc25_ssp126_81-00"))
writeRaster(envi_fut_sel, filename='./data/env_sel/future/bc25_ssp126_81-00/', format="GTiff", 
            bylayer=TRUE, suffix="names", overwrite=TRUE)



# BCC spp585 --------------------------------------------------------------


# 2021-2040

future_var_stk <- stack("./data/env/future/wc2.1_2.5m_bioc_BCC-CSM2-MR_ssp585_2021-2040.tif")
envi_fut.cut<- crop(future_var_stk, to_crop)
envi_fut.mask<- mask(envi_fut.cut, to_crop)

# Save all variables
dir.create(paste0("./data/env_cropped/future/bc25_ssp585_21-40"))
writeRaster(envi_fut.mask, filename='./data/env_cropped/future/bc25_ssp585_21-40/', format="GTiff", 
            bylayer=TRUE, suffix="names", overwrite=TRUE)


# Save only that selected variables 
envi_fut_sel <- envi_fut.mask[[c(3,8,15,18)]]
names(envi_fut_sel) <- c("bio3", "bio8", "bio15","bio18")
#dir.create(paste0("./data/env_sel/future/bc25_ssp585_21-40"))
writeRaster(envi_fut_sel, filename='./data/env_sel/future/bc25_ssp585_21-40/', format="GTiff", 
            bylayer=TRUE, suffix="names", overwrite=TRUE)


# 2041-2060

future_var_stk <- stack("./data/env/future/wc2.1_2.5m_bioc_BCC-CSM2-MR_ssp585_2041-2060.tif")
envi_fut.cut<- crop(future_var_stk, to_crop)
envi_fut.mask<- mask(envi_fut.cut, to_crop)

# Save all variables
dir.create(paste0("./data/env_cropped/future/bc25_ssp585_41-60"))
writeRaster(envi_fut.mask, filename='./data/env_cropped/future/bc25_ssp585_41-60/', format="GTiff", 
            bylayer=TRUE, suffix="names", overwrite=TRUE)


# Save only that selected variables 
envi_fut_sel <- envi_fut.mask[[c(3,8,15,18)]]
names(envi_fut_sel) <- c("bio3", "bio8", "bio15","bio18")
#dir.create(paste0("./data/env_sel/future/bc25_ssp585_41-60"))
writeRaster(envi_fut_sel, filename='./data/env_sel/future/bc25_ssp585_41-60/', format="GTiff", 
            bylayer=TRUE, suffix="names", overwrite=TRUE)


# 2061-2080

future_var_stk <- stack("./data/env/future/wc2.1_2.5m_bioc_BCC-CSM2-MR_ssp585_2061-2080.tif")
envi_fut.cut<- crop(future_var_stk, to_crop)
envi_fut.mask<- mask(envi_fut.cut, to_crop)

# Save all variables
dir.create(paste0("./data/env_cropped/future/bc25_ssp585_61-80"))
writeRaster(envi_fut.mask, filename='./data/env_cropped/future/bc25_ssp585_61-80/', format="GTiff", 
            bylayer=TRUE, suffix="names", overwrite=TRUE)


# Save only that selected variables 
envi_fut_sel <- envi_fut.mask[[c(3,8,15,18)]]
names(envi_fut_sel) <- c("bio3", "bio8", "bio15","bio18")
#dir.create(paste0("./data/env_sel/future/bc25_ssp585_61-80"))
writeRaster(envi_fut_sel, filename='./data/env_sel/future/bc25_ssp585_61-80/', format="GTiff", 
            bylayer=TRUE, suffix="names", overwrite=TRUE)



# 2081-2100

future_var_stk <- stack("./data/env/future/wc2.1_2.5m_bioc_BCC-CSM2-MR_ssp585_2081-2100.tif")
envi_fut.cut<- crop(future_var_stk, to_crop)
envi_fut.mask<- mask(envi_fut.cut, to_crop)

# Save all variables
dir.create(paste0("./data/env_cropped/future/bc25_ssp585_81-00"))
writeRaster(envi_fut.mask, filename='./data/env_cropped/future/bc25_ssp585_81-00/', format="GTiff", 
            bylayer=TRUE, suffix="names", overwrite=TRUE)


# Save only that selected variables 
envi_fut_sel <- envi_fut.mask[[c(3,8,15,18)]]
names(envi_fut_sel) <- c("bio3", "bio8", "bio15","bio18")
#dir.create(paste0("./data/env_sel/future/bc25_ssp585_81-00"))
writeRaster(envi_fut_sel, filename='./data/env_sel/future/bc25_ssp585_81-00/', format="GTiff", 
            bylayer=TRUE, suffix="names", overwrite=TRUE)


