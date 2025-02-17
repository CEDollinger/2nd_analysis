# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Set up ###########################################################################################################################################
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

library(tidyverse)
library(ggpubr)
library(RSQLite); library(dbplyr)
library(terra)
library(plotly)
library(stringr)
library(sf)
library(reshape2)
library(concaveman)
# install.packages('reticulate')                                            # only needs to be run the first time
# reticulate::install_miniconda()                                           # -"-
# reticulate::conda_install('r-reticulate', 'python-kaleido==0.1.*')        # -"-
# reticulate::conda_install('r-reticulate', 'plotly', channel = 'plotly')   # -"-
library(reticulate); reticulate::use_miniconda('r-reticulate') # to save plotly graphs as png
reticulate::py_run_string("import sys")
library(scales)

"%ni%" <- Negate("%in%")
landscapes <- c("bgd", "grte", "stoko")
vars <- c("size", "freq", "fecundity", "browsing")
areas <- data.frame(area = c(8645, 42586, 35676), landscape=landscapes) # forested landscape area in ha; "bgd", "grte", "stoko"
response.colors <- c("#1b9e77", "#7570b3", "#d95f02"); names(response.colors) <- c('1. Structure\nBasal area decreased by >50 % from reference',  '2. Composition\nDominant species changed from reference', '3. Remaining forest\nStem density dropping below 50 trees/ha')
colors.landscape <- c("#009988", "#999933", "#882255")
names(colors.landscape) <- c("Berchtesgaden", "Grand Teton", "Shiretoko")



# Most common species per landscape ###
# (common.bgd <- dbConnect(RSQLite::SQLite(), "raw_data/breakingTheSystem_BGD_Results/data1/output.sqlite") %>%
#    tbl(., "landscape") %>% collect() %>%
#    group_by(species) %>% summarise(a=mean(basal_area_m2)) %>% arrange(desc(a)) %>% slice(1:8) %>% pull(species))
common.bgd <- c("piab", "lade", "fasy", "pice", "abal", "acps", "frex", "soau")

# (common.grte <- dbConnect(RSQLite::SQLite(), "raw_data/breakingTheSystem_GRTE_Results/data1/output.sqlite") %>%
#     tbl(., "landscape") %>% collect() %>%
#     group_by(species) %>% summarise(a=mean(basal_area_m2)) %>% arrange(desc(a)) %>% slice(1:8) %>% pull(species))
common.grte <- c("Pico", "Psme", "PicS", "Pien", "Potr", "Abla", "Pial")

# (common.stoko <- dbConnect(RSQLite::SQLite(), "raw_data/breakingTheSystem_STOKO_Results/data1/output.sqlite") %>%
#     tbl(., "landscape") %>% collect() %>%
#     group_by(species) %>% summarise(a=mean(basal_area_m2)) %>% arrange(desc(a)) %>% slice(1:8) %>% 
#     pull(species))
common.stoko <- c("beer", "absa", "acmo", "kase", "qumo", "alhi", "tacu", "soco")

common.species <- list(common.bgd, common.grte, common.stoko); names(common.species) <- landscapes; rm(common.bgd, common.grte, common.stoko)

helpers <- list(list(), list(), list(), tibble(), tibble()); names(helpers) <- c("ru", "stand", "elev", "species", "env")
helper.files <- list(helpers, helpers, helpers); names(helper.files) <- landscapes; rm(helpers)

helper.files[["bgd"]][["ru"]] <- rast("processed_data/helper_files/bgd_objectid.asc")
helper.files[["bgd"]][["stand"]] <- rast("processed_data/helper_files/bgd_standgrid.asc")
helper.files[["bgd"]][["elev"]] <- rast("processed_data/helper_files/bgd_dem100_rid.asc") %>% 
  resample(., helper.files[["bgd"]][["ru"]])
helper.files[["bgd"]][["species"]] <- dbConnect(RSQLite::SQLite(),  dbname = "processed_data/helper_files/bgd_species.sqlite") %>%
  tbl(., "species") %>% collect() %>% dplyr::select(shortName, lightResponseClass, isConiferous, sapHDSapling, psiMin, displayColor) %>%
  rename(species = shortName) %>% mutate(lightResponseClass = as.numeric(lightResponseClass)) %>% 
  mutate(species.code = as.numeric(as.factor(species))) %>% 
  mutate(species.code = ifelse(species %in% common.species[["bgd"]], species.code, 0))
helper.files[["bgd"]][["env"]] <- read_delim("processed_data/helper_files/bgd_environment.txt")

helper.files[["grte"]][["ru"]] <- rast("processed_data/helper_files/grte_landscape_env_grid.txt")
helper.files[["grte"]][["stand"]] <- rast("processed_data/helper_files/grte_landscape_stand_grid.txt")
helper.files[["grte"]][["elev"]] <- rast("processed_data/helper_files/grte_landscape_DEM.txt") %>%
  resample(., helper.files[["grte"]][["ru"]])
helper.files[["grte"]][["species"]] <- dbConnect(RSQLite::SQLite(),  dbname = "processed_data/helper_files/grte_species.sqlite") %>%
  tbl(., "species") %>% collect() %>% dplyr::select(shortName, lightResponseClass, isConiferous, sapHDSapling, psiMin, displayColor) %>%
  rename(species = shortName) %>% mutate(lightResponseClass = as.numeric(lightResponseClass)) %>%
  mutate(species.code = as.numeric(as.factor(species))) %>%
  filter(species %ni% c("Potr_backup", "Pial_backup_org", "Pico_backup")) %>%
  mutate(species.code = ifelse(species %in% common.species$grte, species.code, 0))
helper.files[["grte"]][["env"]] <- read_delim("processed_data/helper_files/grte_environment_file_cluster_kbdi_downscaledclimate.txt")

helper.files[["stoko"]][["ru"]] <- rast("processed_data/helper_files/stoko_resourceUnit_snp_220908.asc")
helper.files[["stoko"]][["stand"]] <- rast("processed_data/helper_files/stoko_standgrid_speed.asc")
helper.files[["stoko"]][["elev"]] <- rast("processed_data/helper_files/stoko_jpn_elv_clipClim_JGD2000Zone13_res100m_cropped.asc") %>%
  resample(., helper.files[["stoko"]][["ru"]])
helper.files[["stoko"]][["species"]] <- dbConnect(RSQLite::SQLite(),  dbname = "processed_data/helper_files/stoko_species.sqlite") %>%
  tbl(., "speciesParameters") %>% collect() %>% dplyr::select(shortName, lightResponseClass, isConiferous, sapHDSapling, psiMin, displayColor) %>%
  rename(species = shortName) %>% mutate(lightResponseClass = as.numeric(lightResponseClass)) %>%
  mutate(species.code = as.numeric(as.factor(species))) %>%
  mutate(species.code = ifelse(species %in% common.species$stoko, species.code, 0))
helper.files[["stoko"]][["env"]] <- read_delim("processed_data/helper_files/stoko_environment_final.txt")

rid.df <- read_csv("processed_data/helper_files/rid.df.csv")