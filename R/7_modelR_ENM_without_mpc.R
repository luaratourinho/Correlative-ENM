
# Credits ---------------------------


# https://github.com/Model-R/modleR

# Elaborated by
# Diogo S. B. Rocha (https://github.com/diogosbr)
# Bruno M. Carvalho (https://github.com/###########)


# Edited by
# Luara Tourinho

# Date: 13 abr 2021



# Install packages

# remotes::install_github("Model-R/modleR", build = TRUE)
# remotes::install_github("marlonecobos/kuenm")
# remotes::install_github("mrmaxent/maxnet")


# Required packages

library(dplyr)
library(raster)
library(progress)
library(modleR)
library(foreach)
library(rgeos)
library(doParallel)



# ENM using ModelR --------------------------------------------------------



# Reading data ------------------------------------------------------------


# reading species data, only names
target_species <- read.csv("./outputs/2_n_records.csv",
                           stringsAsFactors = FALSE) %>%
  pull(species)

# reading occurrence table of all species
# in case that your occurrence table have more species than you want to project
clean_df <- read.csv("./outputs/3_clean_df_thin_10.csv",
                     stringsAsFactors = FALSE) %>%
  filter(species %in% target_species) # retaining only records from target species

# converting species names for modleR, adding a "_" between genus and epithet
clean_df$species <-
  gsub(x = clean_df$species,
       pattern = " ",
       replacement = "_")
target_species <-
  gsub(x = target_species,
       pattern = " ",
       replacement = "_")


# if you want to run for only one species, use this routine, 
# otherwise, go to line 77
#target_species <- "Leopardus_tigrinus"
#clean_df$species <- gsub(x = clean_df$species, pattern = " ", replacement = "_")
#target_species <- gsub(x = target_species, pattern = " ", replacement = "_")
#clean_df <- clean_df %>% filter(species %in% target_species)


# reading climatic data from prensent conditions
wc <- list.files("./data/env_sel/present/",
                 pattern = "tif$",
                 full.names = TRUE) %>%
  stack()



# Parallel ----------------------------------------------------------------

# loops for ENM
# number of species if you have enough nodes
registerDoParallel(cores = 8) 



# setup_sdmdata step ---------------------------------------------------------


foreach(
  sp = target_species,
  .inorder = FALSE,
  .packages = c("modleR", "sp", "raster", "rgeos", "dplyr")
) %dopar% {
  species_df <-
    clean_df[clean_df$species == sp, ] # getting occurrences
  
  # choosing the type of partition depending on the number of records
  partition_type <- ifelse(nrow(species_df) > 50, "crossvalidation", "bootstrap")
  
  setup_sdmdata(
    species_name = sp,
    occurrences = species_df,
    predictors = wc,
    models_dir = "./outputs/models/",
    # folder to save partitions
    seed = 123,
    clean_dupl = TRUE,
    clean_nas = TRUE,
    clean_uni = TRUE,
    # remove records falling at the same pixel
    png_sdmdata = TRUE,
    n_back = nrow(species_df) * 10,
    # number of pseudoabsences
    partition_type = partition_type,
    cv_partitions = 10,
    cv_n = 1,
    boot_n = 10, 
    boot_proportion = 0.8)
}



# do_many step ------------------------------------------------------------


# partitions
foreach(sp = target_species,
        .inorder = FALSE,
        .packages = "modleR") %dopar% {
          do_many(
            species_name = sp,
            predictors = wc,
            models_dir = "./outputs/models",
            project_model = TRUE,
            proj_data_folder = "./data/env_sel/future/",
            png_partitions = FALSE,
            write_bin_cut = TRUE,
            dismo_threshold = "spec_sens",
            equalize = TRUE,
            # equalize presence and pseudoabsences for RF and BRT
            bioclim = TRUE,
            glm = TRUE,
            maxent = TRUE,
            rf = TRUE,
            svmk = TRUE
          )
        }




# final_model step --------------------------------------------------------


#combine partitions into one final model per algorithm
# projections path names

# final_models
foreach(sp = target_species,
        .inorder = FALSE,
        .packages = "modleR") %dopar% {
          final_model(
            species_name = sp,
            models_dir = "./outputs/models/",
            which_models = c("raw_mean", "bin_consensus"),
            consensus_level = 0.5,
            # proportion of models in the binary consensus
            proj_dir = "present",
            uncertainty = FALSE,
            png_final = FALSE,
            overwrite = TRUE
          )
          
          final_model(
            species_name = sp,
            models_dir = "./outputs/models/",
            which_models = c("raw_mean", "bin_consensus"),
            consensus_level = 0.5,
            proj_dir = "bc50",
            uncertainty = FALSE,
            png_final = FALSE,
            overwrite = TRUE
          )
        }
gc()




# ensemble_model step -----------------------------------------------------


# generate ensemble models, combining final models from all algorithms
# ensemble models

foreach(
  sp = target_species,
  .inorder = TRUE,
  .packages = c("modleR", "raster", "dplyr")
) %dopar% {
  species_df <- clean_df[clean_df$species == sp,]
  ensemble_model(
    species_name = sp,
    algorithms = c("bioclim", "glm", "maxent", "rf", "svmk"),
    models_dir = "./outputs/models/",
    performance_metric = "TSSmax",
    proj_dir = "present",
    which_ensemble = c("weighted_average", "consensus"),
    which_final = c("raw_mean", "bin_consensus"),
    ensemble_dir = "ensemble_all_algo",
    consensus_level = 0.5,
    png_ensemble = FALSE,
    uncertainty = TRUE,
    overwrite = TRUE
  )
  
  ensemble_model(
    species_name = sp,
    algorithms = c("bioclim", "glm", "maxent", "rf", "svmk"),
    models_dir = "./outputs/models/",
    performance_metric = "TSSmax",
    proj_dir = "bc50",
    which_ensemble = c("weighted_average", "consensus"),
    which_final = c("raw_mean", "bin_consensus"),
    ensemble_dir = "ensemble_all_algo",
    consensus_level = 0.5,
    png_ensemble = FALSE,
    uncertainty = TRUE,
    overwrite = TRUE
  )
  
}

gc()
