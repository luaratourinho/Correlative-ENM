
# Credits ---------------------------


# https://github.com/Model-R/modleR

# Script was created by

# Diogo S. B. Rocha (https://github.com/diogosbr)
# Bruno M. Carvalho (https://github.com/###########)


# Script was edited by
# Luara Tourinho

# Date: 01 abr 2021



# ENM using ModelR --------------------------------------------------------


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

# reading species data #species <- unique(table3$sp)
target_species <- read.csv("./outputs/2_n_records.csv",
                           stringsAsFactors = FALSE) %>%
   pull(species)

clean_df <- read.csv("./outputs/3_clean_df_thin_10.csv",
                     stringsAsFactors = FALSE) %>%
  filter(species %in% target_species) # retaining only records from target species

# converting species names for modleR
clean_df$species <- gsub(x = clean_df$species, pattern = " ", replacement = "_")
target_species <- gsub(x = target_species, pattern = " ", replacement = "_")

# reading climatic data
wc <- list.files("./data/env_sel/present/",
                 pattern = "tif$", full.names = TRUE) %>%
  stack()

# projections
crs.wgs84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")  # geographical, datum WGS84
crs.albers <- CRS("+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs") # projected, South America Albers Equal Area Conic

# loops for ENM ----------------------------------------------------------------

# parallelism
# cl = snow::makeCluster(60, outfile = '')
# doSNOW::registerDoSNOW(cl)
registerDoParallel(cores=4)

# setup data
system.time( #Monitorando o tempo de processamento do loop
foreach(sp = target_species, .inorder = FALSE,
        .packages = c("modleR", "sp", "raster", "rgeos", "dplyr")) %dopar% {
          
          species_df <- clean_df[clean_df$species == sp, ] # getting only occurrences for this species
          # creating model calibration area for this species
          coords <- species_df[ ,2:3]
          coordinates(coords) <- c("lon", "lat")
          proj4string(coords) <- crs.wgs84  # define original projection - wgs84
          coords <- spTransform(coords, crs.albers)  # project to Albers Equal Area
          mcp <- gConvexHull(coords) # create minimum convex polygon
          mcp_buf <- mcp %>%
            gBuffer(width = gArea(mcp)*2e-07) %>% # draw a buffer of 20% in km around of the minimum convex polygon (Barve et al. 2011)
            spTransform(crs.wgs84) %>% #convert back into wgs84
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
                        
                        # env_filter = TRUE,
                        # env_distance = "centroid",
                        # min_env_dist = 0.1,
                        # select_variables = FALSE,
                        # cut_off = 0.6
                        # sample_proportion = 0.5
                        
                        clean_dupl = TRUE, # remove duplicate records
                        clean_nas = TRUE, # remove records with na values from variables
                        clean_uni = TRUE, # remove records falling at the same pixel
                        png_sdmdata = TRUE, # save minimaps in png
                        n_back = nrow(species_df) * 10, # number of pseudoabsences
                        partition_type = partition_type,
                        cv_partitions = 10, # number of folds for crossvalidation
                        cv_n = 1,# number of crossvalidation runs
                        boot_n = 10, # number of crossvalidation runs
                        boot_proportion = 0.1) # number of partitions in the crossvalidation
                        #0.8 80%, 0.5 50% 50%
        }

)

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
                  mask = mcp_buf, # mask for projecting the models
                  png_partitions = FALSE, # save minimaps in png?
                  write_bin_cut = FALSE, # save binary and cut outputs?
                  dismo_threshold = "spec_sens", # threshold rule for binary outputs
                  equalize = TRUE, # equalize numbers of presence and pseudoabsences for random forest and brt
                  bioclim = TRUE,
                  glm = TRUE,
                  maxent = TRUE,
                  rf = TRUE,
                  svmk = TRUE
          )
        }

# projections path names
paths <- c("present", "bc50", "ip50", "mi50")

# final_models
for (path in paths) {
  foreach(sp = target_species, .inorder = FALSE,
          .packages = "modleR") %dopar% {
            
            #combine partitions into one final model per algorithm
            final_model(species_name = sp,
                        models_dir = "./outputs/models/",
                        which_models = c("raw_mean", "bin_consensus"),
                        consensus_level = 0.5, # proportion of models in the binary consensus
                        proj_dir = path, #"present" "bc50" "ip50" "mi50"
                        uncertainty = TRUE,
                        png_final = FALSE,
                        overwrite = TRUE)
          }
  gc()
}


# some species did not run the brt algorithm because they did not have enough points
spp_without_brt <- c('Amazona_brasiliensis','Automolus_lammi','Conopophaga_cearae','Cotinga_maculata','Glaucis_dohrnii','Leptodon_forbesi','Onychorhynchus_swainsoni','Phylloscartes_beckeri','Pyriglena_atra')

# select algorithms to do the ensemble models
algo_four <- c("glm", "maxent", "rf", "svme")
algo_five <- c("glm", "maxent", "rf", "svme", "brt")

#projections path names
paths <- c("present", "ac85bi50", "he85bi50", "mp85bi50")
source('R/ensemble_2.R')

# ensemble models
for (path in paths) {
  foreach(sp = target_species, .inorder = TRUE, .packages = c("modleR", "raster", "dplyr")) %dopar% {
    
    # selecting algorithms to do the ensemble models
    ifelse(sp %in% spp_without_brt, algo <- algo_four, algo <- algo_five)
    
    # getting only occurrences for this species
    species_df <- clean_df[clean_df$species == sp, ]
    
    #generate ensemble models, combining final models from all algorithms
    ensemble_model2(species_name = sp,
                    #occurrences = species_df,
                    algorithms = algo,
                    models_dir = "./outputs/models/",
                    performance_metric = "TSSmax",
                    proj_dir = path,
                    which_ensemble = c("best","weighted_average", "consensus"),
                    which_final = c("raw_mean", "bin_consensus"),
                    #ensemble_dir = "ensemble_five_algo",
                    ensemble_dir = "ensemble_all_algo",
                    consensus_level = 0.5,
                    png_ensemble = FALSE,
                    uncertainty = TRUE,
                    overwrite = TRUE)
  }
  gc()
}, gcFirst = TRUE)

#snow::stopCluster(cl)

