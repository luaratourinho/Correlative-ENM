

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

# loading packages

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
clean_df$species <- gsub(x = clean_df$species, pattern = " ", replacement = "_")
target_species <- gsub(x = target_species, pattern = " ", replacement = "_")


# if you want to run for only one species
#target_species <- "Leopardus_tigrinus"
#clean_df$species <- gsub(x = clean_df$species, pattern = " ", replacement = "_")
#target_species <- gsub(x = target_species, pattern = " ", replacement = "_")
#clean_df <- clean_df %>% filter(species %in% target_species)


# reading climatic data from prensent conditions
wc <- list.files("./data/env_sel/present/",
                 pattern = "tif$", full.names = TRUE) %>%
  stack()

# projections
# needed to perform the minimum convex polygon (mcp) below
crs.wgs84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")  # geographical, datum WGS84
crs.albers <- CRS("+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs") # projected, South America Albers Equal Area Conic

# loops for ENM 
# parallelism
registerDoParallel(cores=8)



# setup_sdmdata step ---------------------------------------------------------


foreach(sp = target_species, .inorder = FALSE,
        .packages = c("modleR", "sp", "raster", "rgeos", "dplyr")) %dopar% {
          
          # getting only occurrences for this species
          species_df <- clean_df[clean_df$species == sp, ] 
          # creating model calibration area for this species
          coords <- species_df[ ,2:3]
          coordinates(coords) <- c("lon", "lat")
          # define original projection, e.g. wgs84
          proj4string(coords) <- crs.wgs84  
          # project to Albers Equal Area
          coords <- spTransform(coords, crs.albers)
          # create minimum convex polygon
          mcp <- gConvexHull(coords) 
          mcp_buf <- mcp %>%
            # draw a buffer of 20% in km around of the minimum convex polygon (Barve et al. 2011)
            gBuffer(width = gArea(mcp)*2e-07) %>% 
            #convert back into wgs84
            spTransform(crs.wgs84) %>% 
            SpatialPolygonsDataFrame(data = data.frame("val" = 1, row.names = "buffer"))
          
          # choosing the type of partition depending on the number of records
          partition_type <- ifelse(nrow(species_df) > 50, "crossvalidation", "bootstrap")
          
          setup_sdmdata(species_name = sp,
                        occurrences = species_df,
                        predictors = wc, # set of predictors for running the models
                        models_dir = "./outputs/models/", # folder to save partitions
                        seed = 123, # set seed for random generation of pseudoabsences
                        buffer_type = "user", # buffer type for sampling pseudoabsences
                        buffer_shape = mcp_buf,
                        clean_dupl = TRUE, # remove duplicate records
                        clean_nas = TRUE, # remove records with na values from variables
                        clean_uni = TRUE, # remove records falling at the same pixel
                        png_sdmdata = TRUE, # save minimaps in png
                        n_back = nrow(species_df) * 10, # number of pseudoabsences
                        partition_type = partition_type,
                        cv_partitions = 10, # number of folds for crossvalidation
                        cv_n = 1,# number of crossvalidation runs
                        boot_n = 1, # number of crossvalidation runs
                        boot_proportion = 0.1) # number of partitions in the crossvalidation
                        #0.8 80%, 0.5 50% 50%
        }



# do_many step ------------------------------------------------------------


# partitions
foreach(sp = target_species, .inorder = FALSE,
        .packages = "modleR") %dopar% {
          # for(sp in target_species){
          #   pb$tick()
          
          # run selected algorithms for each partition
          do_many(species_name = sp,
                  predictors = wc,
                  models_dir = "./outputs/models",
                  project_model = TRUE, # project models into other sets of variables
                  proj_data_folder = "./data/env_sel/future/", # folder with projection variables
                  png_partitions = FALSE, # save minimaps in png?
                  write_bin_cut = TRUE, # save binary and cut outputs?
                  dismo_threshold = "spec_sens", # threshold rule for binary outputs
                  equalize = TRUE, # equalize numbers of presence and pseudoabsences for random forest and brt
                  bioclim = TRUE,
                  glm = TRUE,
                  maxent = TRUE,
                  rf = TRUE,
                  svmk = TRUE
          )
        }



# final_model step --------------------------------------------------------


# projections path names
# paths <- c("present", "bc50", "ip50", "mi50")

# final_models
  foreach(sp = target_species, .inorder = FALSE,
          .packages = "modleR") %dopar% {
            
            #combine partitions into one final model per algorithm
            final_model(species_name = sp,
                        models_dir = "./outputs/models/",
                        which_models = c("raw_mean", "bin_consensus"),
                        consensus_level = 0.5, # proportion of models in the binary consensus
                        proj_dir = "present", #path, #"present" "bc50" "ip50" "mi50"
                        uncertainty = FALSE,
                        png_final = FALSE,
                        overwrite = TRUE)
            
            final_model(species_name = sp,
                        models_dir = "./outputs/models/",
                        which_models = c("raw_mean", "bin_consensus"),
                        consensus_level = 0.5, # proportion of models in the binary consensus
                        proj_dir = "bc50", 
                        uncertainty = FALSE,
                        png_final = FALSE,
                        overwrite = TRUE)
          }
  gc()




# ensemble_model step -----------------------------------------------------


# ensemble models
  foreach(sp = target_species, .inorder = TRUE, .packages = c("modleR", "raster", "dplyr")) %dopar% {
    
    # getting only occurrences for this species
    species_df <- clean_df[clean_df$species == sp, ]
    
    #generate ensemble models, combining final models from all algorithms
    ensemble_model(species_name = sp,
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
                    overwrite = TRUE)
    
    ensemble_model(species_name = sp,
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
                   overwrite = TRUE)
    
  }

gc()


