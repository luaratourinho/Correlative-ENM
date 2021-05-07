
# Credits ---------------------------

# Script created by
# Bruno M. Carvalho (https://github.com/brunomc-eco)

# Edited by
# Luara Tourinho (https://github.com/luaratourinho)

# Date: 04 May 2021


# Getting species records -------------------------------------------------



# Required packages

library(tidyverse)
library(rgbif)
library(taxize) # for get_gbifid_
library(data.table)
library(dplyr)
library(purrr)
library(readr)
library(magrittr) # for %T>% pipe
library(rgbif) # for occ_download
library(Rocc) # fpr spescieslink


# An example for American Felidae ---------------------------

# Creating/reading our species list

# Small ones
splist <- c("Leopardus geoffroyi", "Leopardus guttulus", "Leopardus jacobita",
            "Leopardus tigrinus", "Leopardus guigna")

# Big ones
#splist <- c("Leopardus pardalis", "Leopardus wiedii", "Panthera onca", 
#                "Puma concolor", "Leopardus colocolo", "Lynx rufus",
#                "Puma yagouaroundi", "Herpailurus yagouaroundi")



# GBIF --------------------------------------------------------------------


# getting records from gbif
# got this code from https://data-blog.gbif.org/post/downloading-long-species-lists-on-gbif/
# Manual of rgbif: https://cran.r-project.org/web/packages/rgbif/rgbif.pdf

gbif_taxon_keys <- splist %>%
  get_gbifid_(method="backbone") %>% # get taxonkeys for each species name
  imap(~ .x %>% mutate(original_sciname = .y)) %>% # add original name back to data.frame
  bind_rows() # combine all results in a single data.frame

only_keys <- gbif_taxon_keys %>%
  filter(matchtype == "EXACT" & status == "ACCEPTED") %>% # get only accepted names
  pull(usagekey) #retain only the taxonkeys

# download data directly at GBIF
# (file needs to be manually fetched at the user's downloads page at gbif.org)

# enter GBIF credentials
user <- "xxx" # your gbif.org username
pwd <- "xxx" # your gbif.org password
email <- "xxx" # your email

occ_download(
  pred_in("taxonKey", only_keys),
  pred("hasCoordinate", TRUE),
  pred("hasGeospatialIssue", FALSE),
  format = "SIMPLE_CSV",
  user = user, pwd = pwd, email = email
)

# DOI 10.15468/dl.q3p6je
gbif_df <- fread("./0267718-200613084148143.csv", na.strings = c("", NA))

gbif_df2 <- gbif_df[,c("family","species","decimalLongitude","decimalLatitude",
                       "year","countryCode")]
head(gbif_df2)
colnames(gbif_df2) <- c("family", "species", "lon", "lat", "year", "country")



# speciesLink -------------------------------------------------------------


# https://rdrr.io/github/saramortara/rspeciesLink/man/rspeciesLink.html

splist_specieslink <- rspeciesLink(dir = "./",
                                    filename = "splist",
                                    save = TRUE,
                                    basisOfRecord = NULL,
                                    species = splist,
                                    collectionCode = NULL,
                                    country = NULL,
                                    stateProvince = NULL,
                                    county = NULL,
                                    Coordinates = "Yes", #		Yes | No | Original | Automatic | Blocked
                                    CoordinatesQuality = "Good",	#Good | Bad
                                    Typus = FALSE,
                                    Images = NULL,
                                    RedList = FALSE,
                                    MaxRecords = NULL)

splist_specieslink2 <- splist_specieslink[,c("family","genus","specificEpithet",
                                             "scientificName","decimalLongitude",
                                             "decimalLatitude","year","country")]

colnames(splist_specieslink2)

splist_specieslink2$species <- with(splist_specieslink2, 
                                    paste(splist_specieslink2$genus, splist_specieslink2$specificEpithet))

splist_specieslink3 <- splist_specieslink2[, c("family","species","decimalLongitude",
                                               "decimalLatitude","year","country")]
head(splist_specieslink3)
colnames(splist_specieslink3) <- c("family", "species", "lon", "lat", "year", "country")



# Table with search results -----------------------------------------------

splist_specieslink_table <-
  tibble(species = splist,
         date_of_search = rep(Sys.Date(), length(splist))) %>%
  left_join(splist_specieslink3, by = c("species" = "species"))
  
gbif_table <-
  tibble(species = splist,
         date_of_search = rep(Sys.Date(), length(splist))) %>%  
  left_join(gbif_df2, by = "species")

searches <- rbind(splist_specieslink_table, gbif_table)

only_keys <- tibble(taxonKey = only_keys)



# Saving outputs ----------------------------------------------------------

write_csv(searches, "./outputs/01_search_refined_results.csv")
write_csv(splist_specieslink3, "./outputs/01_specieslink_refined.csv")
write_csv(gbif_df2, "./outputs/01_gbif_refined.csv")
write_csv(splist_specieslink, "./outputs/01_unclean_records_specieslink.csv")
write_csv(gbif_df, "./outputs/01_unclean_records_gbif.csv")
write_csv(only_keys, "./outputs/01_gbif_taxonkeys.csv")
