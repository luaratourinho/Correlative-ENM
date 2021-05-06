
# Credits ---------------------------

# Original routine by 
# Vitor Cavalcante

# Edited by
# Luara Tourinho (https://github.com/luaratourinho)

# Date: 03 Aug 2020

# This script is an example for generating pseudoabsence using biomod2 ----


# Required packages

library(biomod2)
library(raster)
library(dplyr)

setwd("./mydirectory")


# Opening occurences ------------------------------------------------------

sp <- read.table("./spdata/3_clean_df_thin_10.csv",header=TRUE, sep=",")
My_target_species <- sp[,2:3]



# Reading environmental variables -----------------------------------------

tmax_avg <- raster("./variables/current/tmax_avg.tif")
tmin_avg <- raster("./variables/current/tmin_avg.tif")
r_sum10to04.crop <- raster("./variables/current/r_sum10to04.crop.tif")
r_sum05to09.crop <- raster("./variables/current/r_sum05to09.crop.tif")

environment <- stack(tmax_avg, tmin_avg, r_sum10to04.crop, r_sum05to09.crop)
names(environment) <- c("Tmax","Tmin","W_Prec","S_Prec")

occurrence.resp <- rep(1, length(My_target_species$lon))

Mymodel <- BIOMOD_FormatingData(
  resp.var = occurrence.resp,
  expl.var = environment,
  resp.xy = My_target_species,
  resp.name = "Occurrence",
  PA.nb.rep = 1,
  PA.nb.absences = 100,
  PA.strategy = "disk",
  PA.dist.min = 50000,
  PA.dist.max = 20000000,
  na.rm = TRUE)
Mymodel

## function to get PA dataset
get_PAtab <- function(bfd){
  dplyr::bind_cols(
    x = bfd@coord[, 1],
    y = bfd@coord[, 2],
    status = bfd@data.species,
    bfd@PA
  )
}

## function to get background mask
get_mask <- function(bfd){
  bfd@data.mask
}

(pres.xy <- get_PAtab(Mymodel) %>% 
    filter(status == 1) %>%
    select(x, y))

## get the coordinates of pseudoabsences
## all repetition of pseudoabsences sampling merged 
(pa.all.xy <- get_PAtab(Mymodel) %>% 
    filter(is.na(status)) %>%
    select(x, y)) %>%
  distinct()


plot(environment[[1]])
points(pa.all.xy, pch = 18)
points(pres.xy, pch = 20, col= "red")

write.csv(pa.all.xy,"./spdata/pseudoabs_tmaxtmin.csv", row.names = F)
pseudoabs <- read.csv("./spdata/pseudoabs_tmaxtmin.csv")
head(pseudoabs)
dim(pseudoabs)

pres = sp
# Replace using your species name in "Genus_epithet"
pres$`species` <- sub(pattern = "Genus_epithet", replacement = "1", x = pres$`species`)
head(pres)
pseudo_0 <- rep(0,length(pseudoabs))
pseudoabs$species <- pseudo_0
pseudoabs <- pseudoabs[,c(3,1,2)]
names(pseudoabs) <-c("species","lon","lat")
pres_pseudo_table <- rbind(pres,pseudoabs)
head(pres_pseudo_table)
tail(pres_pseudo_table)
dim(pres_pseudo_table)
names(pres_pseudo_table) <-c("pa","lon","lat")
write.csv(pres_pseudo_table,"./spdata/pres_pseudoabs_biomod_tmaxtmin.csv", row.names = F)
pres_pseudo_table <- read.csv("./spdata/pres_pseudoabs_biomod_tmaxtmin.csv")
head(pres_pseudo_table)
