
# Function from Vilela et al. 2018 ----------------------------------------

# Authors: Bruno Vilela, Filipe Augusto Nascimento & Marcos Vinícius Carneiro Vital
# Tittle: Impacts of climate change on small-ranged amphibians of the northern Atlantic Forest
# Oecologia Australis 22(2) 2018
# doi: 10.4257/oeco.2018.2202.03

# Edited by Luara Tourinho (https://github.com/luaratourinho)

# Date: 04 May 2021


# Euclidean distance is a good option for rare species modelling ----------



# Required packages

library(dismo)
library(raster)
library(vegan)


# Before running the script below, run the function "Euclidean_distance_function.R"

# We can apply the function using the example data from the package dismo.
# So,first we load the data.

# Occurrence points
occurence <- paste(system.file(package="dismo"), '/ex/bradypus.csv', sep='')
occ <- read.table(occurence, header=TRUE, sep=',')[,-1]

# Predictors
fnames <- list.files(path=paste(system.file(package="dismo"), '/ex', sep=''),
                     pattern='grd', full.names=TRUE )
predictors <- stack(fnames)

# Now we run the function using the method = “mean”, and returning actual 
# Euclidean distance (suitability = FALSE). 
# We also standardize the data using the “standardize” method from vegan::decostand.

result <- dist_euc(occ, predictors, method = "mean", suitability = FALSE,
                   decostand.method = "standardize")

#Plot the distance map:
plot(result, col = gray.colors(100))
points(occ, col = rgb(1, 0, 0, .4), pch = 20, cex = .5)

