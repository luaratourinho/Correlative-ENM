################################################################################
###                                                                          ###
###                     LIST OF TREE SPECIES PER BIOMES                      ###
###                                                                          ###
################################################################################

# Credits

# Created by Andrea Sanchez-Tapia
# https://github.com/AndreaSanchezTapia/flora

# Edited by Luara Tourinho



# Package flora ------------------------------------------------------------


#install.packages("devtools")
#devtools::install_github("liibre/Rocc")
library(Rocc)
library(flora)


# Read a table with your species list
species <- read.csv("./Lista de sp/Bacia_gualaxo.csv", sep = ';')
species <- as.vector(species$species)

sp_search_flora <- get.taxa(species, domain = TRUE)

# The argument establishment says if the species are native, etc, e.g.:
# get.taxa(gualaxo$species, life.form = TRUE, habitat = TRUE, 
# vegetation.type = TRUE, establishment = TRUE, states = TRUE, domain = TRUE, 
# endemism = TRUE)

write.csv(sp_search_flora, "./results/0_search_flora.csv")


# get.taxa can automatically fix misspelled names when possible 
# (when suggest.names = TRUE), but you can also use suggest.names for that:
#   
#   suggest.names("Cofea arabyca")
# [1] "Coffea arabica"

# You may also search for a species using vernacular names:
# vernacular("Pimenta", exact = T)



# List of species and biomes ----------------------------------------------


#??search_flora()
# based on the List of Species of the Brazilian Flora (FB2020)
# search_flora(
#   domain = NULL,  # e.g. "Caatinga", "Mata Atlântica", "Cerrado", "Pantanal", "Pampa", "Amazônia"
#   stateProvince = NULL,  # e.g. "PE"
#   life_form = NULL,  
#   habitat = NULL,  # character, search species in the habitat options according to the FB2020
#   vegetation_type = NULL,  # character, filter by vegetation type according to the FB2020
#   endemism = NULL,  # e.g. TRUE
#   force_update = FALSE
# )



# Choosing the domain ------------------------------------------------------

# writing the table with selected species from some domain
# Later you can compare with your species list, if you want to


sp_search_flora <- search_flora(domain = "Mata Atlântica",
                    force_update = FALSE)
write.csv(sp_search_flora, "./results/0_search_flora_mataatlantica.csv")

sp_search_flora <- search_flora(domain = "Cerrado",
                                force_update = FALSE)
write.csv(sp_search_flora, "./results/0_search_flora_cerrado.csv")

sp_search_flora <- search_flora(domain = "Mata Atlântica",
                                endemism = TRUE,
                                force_update = FALSE)
write.csv(sp_search_flora, "./results/0_search_flora_mataatlantica_endemicas.csv")

sp_search_flora <- search_flora(domain = "Cerrado",
                                endemism = TRUE,
                                force_update = FALSE)
write.csv(sp_search_flora, "./results/0_search_flora_cerrado_endemicas.csv")




