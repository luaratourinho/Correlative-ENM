# Copying future files selected -------------------------------------------

#BCC
target_dir <- "./data/env_sel/future/bc50/"
current_folder <- "./data/env_cropped/future/bc50/"

file <- list.files(current_folder, "_wc2.1_2.5m_bioc_BCC.CSM2.MR_ssp585_2041.2060.2.tif")
file.copy(file.path(current_folder,file), target_dir)
file <- list.files(current_folder, "_wc2.1_2.5m_bioc_BCC.CSM2.MR_ssp585_2041.2060.8.tif")
file.copy(file.path(current_folder,file), target_dir)
file <- list.files(current_folder, "_wc2.1_2.5m_bioc_BCC.CSM2.MR_ssp585_2041.2060.15.tif")
file.copy(file.path(current_folder,file), target_dir)
file <- list.files(current_folder, "_wc2.1_2.5m_bioc_BCC.CSM2.MR_ssp585_2041.2060.18.tif")
file.copy(file.path(current_folder,file), target_dir)

#IPSL
target_dir <- "./data/env_sel/future/ip50/"
current_folder <- "./data/env_cropped/future/ip50/"

file <- list.files(current_folder, "_wc2.1_2.5m_bioc_IPSL.CM6A.LR_ssp585_2041.2060.2.tif")
file.copy(file.path(current_folder,file), target_dir)
file <- list.files(current_folder, "_wc2.1_2.5m_bioc_IPSL.CM6A.LR_ssp585_2041.2060.8.tif")
file.copy(file.path(current_folder,file), target_dir)
file <- list.files(current_folder, "_wc2.1_2.5m_bioc_IPSL.CM6A.LR_ssp585_2041.2060.15.tif")
file.copy(file.path(current_folder,file), target_dir)
file <- list.files(current_folder, "_wc2.1_2.5m_bioc_IPSL.CM6A.LR_ssp585_2041.2060.18.tif")
file.copy(file.path(current_folder,file), target_dir)

#MIROC6
target_dir <- "./data/env_sel/future/mi50/"
current_folder <- "./data/env_cropped/future/mi50/"

file <- list.files(current_folder, "_wc2.1_2.5m_bioc_MIROC6_ssp585_2041.2060.2.tif")
file.copy(file.path(current_folder,file), target_dir)
file <- list.files(current_folder, "_wc2.1_2.5m_bioc_MIROC6_ssp585_2041.2060.8.tif")
file.copy(file.path(current_folder,file), target_dir)
file <- list.files(current_folder, "_wc2.1_2.5m_bioc_MIROC6_ssp585_2041.2060.15.tif")
file.copy(file.path(current_folder,file), target_dir)
file <- list.files(current_folder, "_wc2.1_2.5m_bioc_MIROC6_ssp585_2041.2060.18.tif")
file.copy(file.path(current_folder,file), target_dir)


# Changing names ----------------------------------------------------------

# Just as 'dismo', to run ENM in the 'modelR' the variables names from present
#and future have to have the same names

current_folder <- "./data/env_sel/present/"
file <- list.files(current_folder, "tif$")
file
#[1] "_wc2.1_2.5m_bio_15.tif" "_wc2.1_2.5m_bio_18.tif" "_wc2.1_2.5m_bio_2.tif"  "_wc2.1_2.5m_bio_8.tif" 




