
# Function from Vilela et al. 2018 ----------------------------------------

# Authors: Bruno Vilela, Filipe Augusto Nascimento & Marcos Vinícius Carneiro Vital
# Tittle: Impacts of climate change on small-ranged amphibians of the northern Atlantic Forest
# Oecologia Australis 22(2) 2018
# doi: 10.4257/oeco.2018.2202.03

# The code bellow defines a function to calculate the Euclidean distance of 
# occurrence records to each cell in a grid given an environmental layer(raster).
# Note that the function requires the packages vegan and raster.

#' Function to calculate environmental distance
#'
#' @param occ a two column matrix or a data.frame with the first column being
#'the longitude and the second the latitude
#' @param env a raster with the environmental variables, cropped to the
#'projection area
#' @param method if mean, the "mean" distance to all points is calculated, if
#'"min" the minimum distance to any point is calculated.
#' @param decostand.method What method should be applied to standardize the
#'environmental data. See the function decostand from the package vegan, argument
#'method.
#' @param suitability Whether the function should return a suitability (TRUE)
#' or the actual distance calculated (FALSE).
#'
#' @Value
#' The function returns a raster with the distance (for suitability = FALSE)
#' or the suitability values (for suitability = TRUE).

dist_euc <- function (occ, env, method = "mean",
                      decostand.method = "standardize",
                      suitability = FALSE) {
  require(raster)
  require(vegan)
  if (class(env) != "raster" & class(env) != "RasterStack"
      & class(env) != "RasterBrick") {
    stop("env has to be a raster")
  }
  if (class(occ) != "matrix" & class(occ) != "data.frame") {
    stop("occ has to be a matrix or data.frame")
  }
  if (ncol(occ) != 2) {
    stop("occ has to be a matrix or data.frame of 2 columns(x and y)")
  }
  values <- values(env)
  values <- apply(values, 2, decostand, method = decostand.method,
                  na.rm = TRUE)
  values(env) <- values
  values_occ <- extract(env, occ)
  pos <- is.na(values[, 1])
  values2 <- values[!pos, ]
  n <- nrow(values2)
  eu <- numeric(n)
  values_occ <- na.omit(values_occ)
  for (i in 1:n) {
    for (j in 1:ncol(values_occ)) {
      temp <- eu[i] + ((values2[i, j] - values_occ[, j]) ^ 2)
      if (method == "mean") {
        eu[i] <- mean(temp, na.rm = TRUE)
      }
      if (method == "min") {
        eu[i] <- min(temp, na.rm = TRUE)
      }
    }
    eu[i] <- sqrt(eu[i])
  }
  env <- raster(env, 1)
  if (!suitability) {
    values(env)[!pos] <- eu
    return(env)
  }
  if (suitability) {
    values(env)[!pos] <- decostand(-eu, method = "range", na.rm = TRUE)
    return(env)
  }
}
