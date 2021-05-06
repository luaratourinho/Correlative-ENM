
# Credits ---------------------------

# Script created by
# Daniele Moreira (https://github.com/daniomoreira)

# Edited by
# Luara Tourinho

# Date: 01 abr 2021



# Required packages

library(raster)
library(sp)
library(RStoolbox)



# Variables selection by PCA ---------------------------------------------



# to make PCA from the worldclim's environmental variables
#nSamples means that we use a 70% sampling of pixels from variables in the envi.shp file

present_list <- list.files("data/env_croped/", pattern = "tif$", full.names = T)

# object with present variables
present_ras <- stack(present_list)

envi.pca <- rasterPCA(present_ras, 
                      nSamples = 0.6*ncell(present_ras), 
                      nComp = nlayers(present_ras), 
                      spca = FALSE, maskCheck = TRUE)

?rasterPCA

# To inform components importance 
summary(envi.pca$model)

# in this example, the first two components explain more than 96% of the variation (proportion of variance)

# to inform the importance of the components for each variable
loadings(envi.pca$model)

#Plot o PCA
plot(envi.pca$map[[1:2]])

# Another way to plot the map
ggRGB(envi.pca$map,1,2, stretch="lin", q=0)
if(require(gridExtra)){
  plots <- lapply(1:2, function(x) ggR(envi.pca$map, x, geom_raster = TRUE))
  grid.arrange(plots[[1]],plots[[2]], ncol=2)
}

#######################  SAVE PCA RASTERS  #############################

# Create a file inside a directory
dir.create("./data/raster/wc5_amsul/pca")

# create pca file to storage raster pca results
writeRaster(envi.pca$map[[1:2]],"./data/raster/wc5_amsul/pca/env.pca.tif", format="GTiff", overwrite=TRUE, bylayer=TRUE) # write pcaraster

#generate a list of input rasters ("grids")
#pattern = "*.tif$" - filters for main raster files only and skips any associated files (e.g. world files)
variaveis <- list.files(path="./data/raster/wc5_amsul/pca", ".*.tif$",
                        full.names = TRUE)

variaveis

#create a raster stack from the input raster files
variaveis.stack <- stack(variaveis)

#Plot rasters in a map
plot(variaveis.stack)

variaveis.stack[2]
