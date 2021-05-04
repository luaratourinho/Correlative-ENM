
# Check the name of the species -------------------------

# devtools::install_github("liibre/Rocc")
library(Rocc)
library(stringr)
library(flora)

source("./Scripts/check_string.R")

splist <- c("Leopardus geoffroyi", "Leopardus guttulus", "Leopardus pardalis",
            "Leopardus tigrinus", "Leopardus wiedii", "Panthera onca", 
            "Puma concolor", "Puma yagouaroundi")

check_string(splist)
check_string("Leopardus geoffroyi")
check_string("Leopardus geoffro")
