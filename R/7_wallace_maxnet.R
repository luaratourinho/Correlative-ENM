
# Credits -----------------------------------------------------------------

# This script was made using wallace package
# See these links:
# https://wallaceecomod.github.io/
# https://wallaceecomod.github.io/vignettes/wallace_vignette.html
# Kass et al. 2018 (https://doi-org.ezproxy.gc.cuny.edu/10.1111/2041-210X.12945)


# Edited by Luara Tourinho (https://github.com/luaratourinho)


# Last update: 18 May 2021


# If do you want to work with shiny version, use these following commands:
# install.packages(wallace)
# library(wallace)
# run_wallace()


# loading packages

library(spocc)
library(dismo)
library(rgeos)
library(ENMeval)
library(dplyr)

source("./Scripts/functions.R")


# Reading species data, only names

target_species <- read.csv("./outputs/02_n_records.csv",
                           stringsAsFactors = FALSE) %>%
  pull(species)

# Reading occurrence table of all species
# in case that your occurrence table have more species than you want to project
clean_df <- read.csv("./outputs/03_clean_df_thin_20.csv",
                     stringsAsFactors = FALSE) %>%
  filter(species %in% target_species) # retaining only records from target species

clean_df_sp1 <- filter(clean_df, species == "Leopardus guttulus")

colnames(clean_df_sp1) <- c("name", "longitude", "latitude")


occs = clean_df_sp1
occs$occID <- row.names(occs)

# Reading environmental variables
envs <- list.files("./data/env_sel/present/",
                 pattern = "tif$", full.names = TRUE) %>%
  stack()


# Building a mpc buffer

occs.xy <- occs[c('longitude', 'latitude')]
sp::coordinates(occs.xy) <- ~ longitude + latitude
bgExt <- mcp(occs.xy)
bgExt <- rgeos::gBuffer(bgExt, width = 2) # 2 degrees

# crop the environmental rasters by the background extent shape
envsBgCrop <- raster::crop(envs, bgExt)
# mask the background extent shape from the cropped raster
envsBgMsk <- raster::mask(envsBgCrop, bgExt)


# Creating a background (pseudoabsence)

# sample random background points
bg.xy <- dismo::randomPoints(envsBgMsk, 10000)
# convert matrix output to data frame
bg.xy <- as.data.frame(bg.xy)  


# Partition Occurrence Data

occs.xy <- occs[c('longitude', 'latitude')]
# Using Jackknife
group.data <- ENMeval::get.jackknife(occ=occs.xy, bg.coords=bg.xy)
# Using spatial blocks
#group.data <- ENMeval::get.block(occ=occs.xy, bg.coords=bg.xy)

# pull out the occurrence and background partition group numbers from the list
occs.grp <- group.data[[1]]
bg.grp <- group.data[[2]]


# Build and Evaluate Niche Model

# define the vector of regularization multipliers to test
rms <- seq(1, 3, 1)
# iterate model building over all chosen parameter settings
e <- ENMeval::ENMevaluate(occs.xy, envsBgMsk, bg.coords = bg.xy, RMvalues = rms, 
                          fc = c('L', 'LQ', 'H', 'LQH', 'LQHP'), 
                          method = 'user', occs.grp, bg.grp, clamp = TRUE, 
                          algorithm = "maxnet")

# unpack the results data frame, the list of models, and the RasterStack of raw predictions
evalTbl <- e@results
evalMods <- e@models
names(evalMods) <- e@results$settings
evalPreds <- e@predictions


# view response curves for environmental variables with non-zero coefficients
#'type' can be one of “link”, “exponential”, “cloglog”, “logistic”
plot(evalMods[["LQ_1"]], vars = c('bio_15', 'bio_18', 'bio_3', 'bio_8'), type = "logistic")

# view ENMeval results
ENMeval::eval.plot(evalTbl, value = "avg.test.AUC")
ENMeval::eval.plot(evalTbl, value = "avg.test.or10pct")
ENMeval::eval.plot(evalTbl, value = "AICc")

# Select your model from the models list
mod <- evalMods[["LQ_1"]]

#'type' can be one of “link”, “exponential”, “cloglog”, “logistic”
# generate logistic prediction
pred <- ENMeval::maxnet.predictRaster(mod, envsBgMsk, type = 'logistic', clamp = TRUE)

# plot the model prediction
plot(pred)

# Binary projection

# get predicted values for occurrence grid cells
occPredVals <- raster::extract(pred, occs.xy)
# define minimum training presence threshold
# "p10" Define 10% training presence threshold
# "mtp" minimum training presence threshold
thr <- thresh(occPredVals, "p10")
# threshold model prediction
pred_bin <- pred > thr

# Project Niche Model to New Time

envsFuture <- list.files("./data/env_sel/future/mi25_ssp585_41-60/",
                              pattern = "tif$", full.names = TRUE) %>%
  stack()

predsProj <- raster::crop(envsFuture, bgExt)
predsProj <- raster::mask(predsProj, bgExt)

# predict model
proj <- ENMeval::maxnet.predictRaster(mod, predsProj, type = 'logistic', clamp = TRUE)
plot(proj)


# Binary

# get predicted values for occurrence grid cells
occPredVals <- raster::extract(pred, occs.xy)
# define minimum training presence threshold
thr <- thresh(occPredVals, "p10")
# threshold model prediction
proj_bin <- proj > thr
