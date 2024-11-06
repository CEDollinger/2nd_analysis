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
# install.packages('reticulate')                                            # only needs to be run once
# reticulate::install_miniconda()                                           # only needs to be run once
# reticulate::conda_install('r-reticulate', 'python-kaleido==0.1.*')        # only needs to be run once
# reticulate::conda_install('r-reticulate', 'plotly', channel = 'plotly')   # only needs to be run once
library(reticulate); reticulate::use_miniconda('r-reticulate') # to save plotly graphs as png
reticulate::py_run_string("import sys")
library(scales)

"%ni%" <- Negate("%in%")
options(dplyr.summarise.inform = FALSE, readr.show_col_types = FALSE)
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); getwd() # set wd to file location
landscapes <- c("bgd", "grte", "stoko")
vars <- c("size", "freq", "fecundity", "browsing")
areas <- data.frame(area = c(8645, 42586, 35676), landscape=landscapes) # forested landscape area in ha; "bgd", "grte", "stoko"
response.colors <- c("#1b9e77", "#7570b3", "#d95f02"); names(response.colors) <- c('1. Structure\nBasal area decreased by >50 % from reference',  '2. Composition\nDominant species changed from reference', '3. Remaining forest\nStem density dropping below 50 trees/ha')
colors.landscape <- c("#009988", "#999933", "#882255")
names(colors.landscape) <- c("Berchtesgaden", "Grand Teton", "Shiretoko")
# areas <- c(8645, 42586, 35676) # same order as landscapes
#


# Most common species ###
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

helper.files[["bgd"]][["ru"]] <- rast("../project2ndStudy/projectTest_bgd/gis/objectid.asc")
helper.files[["bgd"]][["stand"]] <- rast("../project2ndStudy/projectTest_bgd/gis/standgrid.asc")
helper.files[["bgd"]][["elev"]] <- rast("../project2ndStudy/projectTest_bgd/gis/dem100_rid.asc") %>% 
  resample(., helper.files[["bgd"]][["ru"]])
helper.files[["bgd"]][["species"]] <- dbConnect(RSQLite::SQLite(),  dbname = "../project2ndStudy/projectTest_bgd/database/species_bgd.sqlite") %>%
  tbl(., "species") %>% collect() %>% dplyr::select(shortName, lightResponseClass, isConiferous, sapHDSapling, psiMin, displayColor) %>%
  rename(species = shortName) %>% mutate(lightResponseClass = as.numeric(lightResponseClass)) %>% 
  mutate(species.code = as.numeric(as.factor(species))) %>% 
  mutate(species.code = ifelse(species %in% common.species[["bgd"]], species.code, 0))
helper.files[["bgd"]][["env"]] <- read_delim("../project2ndStudy/projectTest_bgd/gis/environment.txt")

helper.files[["grte"]][["ru"]] <- rast("../project2ndStudy/projectTest_grte/gis/landscape_env_grid.txt")
helper.files[["grte"]][["stand"]] <- rast("../project2ndStudy/projectTest_grte/gis/landscape_stand_grid.txt")
helper.files[["grte"]][["elev"]] <- rast("../project2ndStudy/projectTest_grte/gis/landscape_DEM.txt") %>%
  resample(., helper.files[["grte"]][["ru"]])
helper.files[["grte"]][["species"]] <- dbConnect(RSQLite::SQLite(),  dbname = "../project2ndStudy/projectTest_grte/database/species_grte.sqlite") %>%
  tbl(., "species") %>% collect() %>% dplyr::select(shortName, lightResponseClass, isConiferous, sapHDSapling, psiMin, displayColor) %>%
  rename(species = shortName) %>% mutate(lightResponseClass = as.numeric(lightResponseClass)) %>%
  mutate(species.code = as.numeric(as.factor(species))) %>%
  filter(species %ni% c("Potr_backup", "Pial_backup_org", "Pico_backup")) %>%
  mutate(species.code = ifelse(species %in% common.species$grte, species.code, 0))
helper.files[["grte"]][["env"]] <- read_delim("../project2ndStudy/projectTest_grte/gis/environment_file_cluster_kbdi_downscaledclimate.txt")

helper.files[["stoko"]][["ru"]] <- rast("../project2ndStudy/projectTest_stoko/gis/resourceUnit_snp_220908.asc")
helper.files[["stoko"]][["stand"]] <- rast("../project2ndStudy/projectTest_stoko/gis/standgrid_speed.asc")
helper.files[["stoko"]][["elev"]] <- rast("../project2ndStudy/projectTest_stoko/gis/jpn_elv_clipClim_JGD2000Zone13_res100m_cropped.asc") %>%
  resample(., helper.files[["stoko"]][["ru"]])
helper.files[["stoko"]][["species"]] <- dbConnect(RSQLite::SQLite(),  dbname = "../project2ndStudy/projectTest_stoko/database/species_stoko.sqlite") %>%
  tbl(., "speciesParameters") %>% collect() %>% dplyr::select(shortName, lightResponseClass, isConiferous, sapHDSapling, psiMin, displayColor) %>%
  rename(species = shortName) %>% mutate(lightResponseClass = as.numeric(lightResponseClass)) %>%
  mutate(species.code = as.numeric(as.factor(species))) %>%
  mutate(species.code = ifelse(species %in% common.species$stoko, species.code, 0))
helper.files[["stoko"]][["env"]] <- read_delim("../project2ndStudy/projectTest_stoko/gis/environment_final.txt")

rid.prelim <- data.frame(); keep.stoko <- read_csv("raw_data/helper_files/keep.csv") %>% pull(keep) 
for (landscape in 1:3) {
  rid_i <- helper.files[[landscapes[landscape]]][["ru"]] %>% 
    terra::as.data.frame(., xy=T) %>% rename(rid=3) %>% 
    drop_na() %>% mutate(landscape = landscapes[landscape])
  if (landscape == 3) rid_i <- rid_i %>% filter(rid %in% keep.stoko)
  rid.prelim <- bind_rows(rid.prelim, rid_i); rm(rid_i)
}; rm(keep.stoko)

rid.df <- bind_rows(
  
  helper.files[["bgd"]][["elev"]]  %>% 
    resample(., helper.files[["bgd"]][["ru"]] ) %>% 
    as.data.frame(., xy=T) %>% 
    rename(elevation = 3) %>% 
    full_join(rid.prelim[rid.prelim$landscape=="bgd",]),
  
  helper.files[["stoko"]][["elev"]]  %>% 
    resample(., helper.files[["stoko"]][["ru"]]) %>% 
    as.data.frame(., xy=T) %>% 
    rename(elevation = 3) %>% 
    full_join(rid.prelim[rid.prelim$landscape=="stoko",]) %>%
    filter(!is.na(rid)) %>% 
    mutate(elevation = ifelse(is.na(elevation), 0, elevation)),
  
  helper.files[["grte"]][["elev"]]  %>% 
    resample(., helper.files[["grte"]][["ru"]] ) %>% 
    as.data.frame(., xy=T) %>% 
    rename(elevation = 3) %>% 
    full_join(rid.prelim[rid.prelim$landscape=="grte",]) %>% 
    filter(!is.na(rid)) 
) %>% 
  group_by(landscape) %>% 
  mutate(dist.treeline = max(elevation)-elevation) %>% 
  ungroup(); summary(rid.df); nrow(rid.prelim); rm(rid.prelim)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# discarded

# reorg.ls<-readRDS("raw_data/helper_files/reorg.ls.RDATA")
# keep <- reorg.ls[["stoko"]] %>% summarise(unique=unique(rid)) %>% pull(unique)
# write_csv(as.data.frame(keep), "raw_data/helper_files/keep.csv")
