library(tidyverse)
library(ggpubr)
library(RSQLite); library(dbplyr)
library(terra)
library(beepr)
"%ni%" <- Negate("%in%")
options(dplyr.summarise.inform = FALSE, readr.show_col_types = FALSE)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); getwd() # set wd to file location
landscapes <- c("bgd", "grte", "stoko")
vars <- c("size", "freq", "fecundity", "browsing")
#

# set up ####
# Most common species ###
(common.bgd <- dbConnect(RSQLite::SQLite(), paste0("../project2ndStudy/projectTest_bgd/output/output_bgd_baseline_rep1_size1_freq1_browsing1_fecundity100.sqlite")) %>%
   tbl(., "landscape") %>% collect() %>%
   group_by(species) %>% summarise(a=mean(basal_area_m2)) %>% arrange(desc(a)) %>% slice(1:8) %>% pull(species))

(common.grte <- dbConnect(RSQLite::SQLite(), paste0("../project2ndStudy/projectTest_grte/output/output_grte_baseline_rep1_size1_freq1_browsing1_fecundity100.sqlite")) %>%
    tbl(., "landscape") %>% collect() %>%
    group_by(species) %>% summarise(a=mean(basal_area_m2)) %>% arrange(desc(a)) %>% slice(1:8) %>% pull(species))

(common.stoko <- dbConnect(RSQLite::SQLite(), paste0("../project2ndStudy/projectTest_stoko/output/output_stoko_baseline_rep1_size1_freq1_browsing1_fecundity100.sqlite")) %>%
    tbl(., "landscape") %>% collect() %>%
    group_by(species) %>% summarise(a=mean(basal_area_m2)) %>% arrange(desc(a)) %>% slice(1:8) %>% 
    pull(species))
common.species <- list(common.bgd, common.grte, common.stoko); names(common.species) <- landscapes; rm(common.bgd, common.grte, common.stoko)

helpers <- list(list(), list(), list(), tibble(), tibble()); names(helpers) <- c("ru", "stand", "elev", "species", "env")
helper.files <- list(helpers, helpers, helpers); names(helper.files) <- landscapes; rm(helpers)

helper.files[["bgd"]][["ru"]] <- rast("../project2ndStudy/projectTest_bgd/gis/objectid.asc")
helper.files[["bgd"]][["stand"]] <- rast("../project2ndStudy/projectTest_bgd/gis/standgrid.asc")
helper.files[["bgd"]][["elev"]] <- rast("../project2ndStudy/projectTest_bgd/gis/dem100_rid.asc") %>% 
  resample(., helper.files[["bgd"]][["ru"]])
helper.files[["bgd"]][["species"]] <- dbConnect(RSQLite::SQLite(),  dbname = "../project2ndStudy/projectTest_bgd/database/species_bgd.sqlite") %>%
  tbl(., "species") %>% collect() %>% dplyr::select(shortName, lightResponseClass, isConiferous, sapHDSapling, psiMin) %>%
  rename(species = shortName) %>% mutate(lightResponseClass = as.numeric(lightResponseClass)) %>% 
  mutate(species.code = as.numeric(as.factor(species))) %>% 
  mutate(species.code = ifelse(species %in% common.species[["bgd"]], species.code, 0))
helper.files[["bgd"]][["env"]] <- read_delim("../project2ndStudy/projectTest_bgd/gis/environment.txt")

helper.files[["grte"]][["ru"]] <- rast("../project2ndStudy/projectTest_grte/gis/landscape_env_grid.txt")
helper.files[["grte"]][["stand"]] <- rast("../project2ndStudy/projectTest_grte/gis/landscape_stand_grid.txt")
helper.files[["grte"]][["elev"]] <- rast("../project2ndStudy/projectTest_grte/gis/landscape_DEM.txt") %>%
  resample(., helper.files[["grte"]][["ru"]])
helper.files[["grte"]][["species"]] <- dbConnect(RSQLite::SQLite(),  dbname = "../project2ndStudy/projectTest_grte/database/species_grte.sqlite") %>%
  tbl(., "species") %>% collect() %>% dplyr::select(shortName, lightResponseClass, isConiferous, sapHDSapling, psiMin) %>%
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
  tbl(., "speciesParameters") %>% collect() %>% dplyr::select(shortName, lightResponseClass, isConiferous, sapHDSapling, psiMin) %>%
  rename(species = shortName) %>% mutate(lightResponseClass = as.numeric(lightResponseClass)) %>%
  mutate(species.code = as.numeric(as.factor(species))) %>%
  mutate(species.code = ifelse(species %in% common.species$stoko, species.code, 0))
helper.files[["stoko"]][["env"]] <- read_delim("../project2ndStudy/projectTest_stoko/gis/environment_final.txt")

rid.df <- data.frame() 
for (landscape in 1:3) {
  rid_i <-helper.files[[landscapes[landscape]]][["ru"]] %>% 
    terra::as.data.frame(., xy=T) %>% rename(rid=3) %>% 
    drop_na() %>% mutate(landscape = landscapes[landscape])
  rid.df <- bind_rows(rid.df, rid_i);rm(rid_i)
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Analysis ###########################################################################################################################################
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
ls <- readRDS("results/datasets/ls.ls.RDATA")
ds <- readRDS("results/datasets/ds.ls.RDATA")
rem <- readRDS("results/datasets/rem.ls.RDATA")

missing.ls <- list(c(), c(), c()); names(missing.ls) <- landscapes; ls.ls <- missing.ls; rem.ls <- missing.ls; patch.ls <- missing.ls
ds.ls.i <- list(tibble(), tibble(), tibble()); names(ds.ls.i) <- c("basal", "dom", "forest")
ds.ls <- list(ds.ls.i, ds.ls.i, ds.ls.i); names(ds.ls) <- landscapes; rm(ds.ls.i)
landscape_i <- 2; i<-1
for (landscape_i in 1:3) {
  landscapename <- landscapes[landscape_i]
  master <- read_delim(paste0("../project2ndStudy/dss/breakingTheSystem_", toupper(landscapename), "/cluster_master_", landscapename, ".csv"))
  
  # reference conditions for calculating differences in basal area / dominant species / regime shift
  db.conn <- dbConnect(RSQLite::SQLite(), paste0("raw_data/breakingTheSystem_", toupper(landscapename) ,"_Results/data1/output.sqlite"))
  ds.ref <- tbl(db.conn, "dynamicstand") %>% 
    dplyr::select(-ru) %>% 
    collect() %>% 
    rename(basal_area_m2 = basalarea_sum, count_ha = if_height_0_1_0_sum) %>% 
    mutate(climate = master$climate[1], rep = master$rep[1], size = master$sizeMod[1], freq = master$freqMod[1], 
           browsing = master$browsingMod[1], fecundity = master$fecundityMod[1],
           identifier = paste0(master$climate[1], "_rep", master$rep[1], "_size", master$sizeMod[1], "_freq", master$freqMod[1], 
                               "_browsing", master$browsingMod[1], "_fecundity", master$fecundityMod[1])) %>% 
    mutate(size = factor(size, levels = rev(c("10", "5", "2", "1"))),
           freq = factor(freq, levels = rev(c("10", "5", "2", "1"))),
           fecundity = factor(fecundity, levels = c("100", "50", "20", "10")),
           browsing = factor(browsing, levels = rev(c("10", "5", "2", "1"))),
           landscape = landscapename)
  dbDisconnect(db.conn); rm(db.conn)
  
  rem <- data.frame(); ls <- data.frame(); ds.basal <- data.frame(); ds.dom <- data.frame(); ds.forest <- data.frame(); missing <- c(NA); patch <- data.frame()
  for (i in c(1, sample(2:2560, 50))) { # nrow(master)) {
    dbname <- paste0("raw_data/breakingTheSystem_", toupper(landscapename) ,"_Results/data", master$run_id[i], "/output.sqlite")
    patch_file <- paste0("raw_data/breakingTheSystem_", toupper(landscapename) ,"_Results/data", master$run_id[i], "/patches.csv")
    if (file.exists(dbname) & file.exists(patch_file)) {
      db.conn <- dbConnect(RSQLite::SQLite(), dbname = dbname) # output_baseline_rep1_size1_freq1_browsing1_fecundity75

      # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
      ## landscape output ####
      # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
      ls.i <- tbl(db.conn, "landscape") %>%
        dplyr::select(year, species, count_ha, basal_area_m2, gwl_m3, volume_m3, height_avg_m, cohort_count_ha) %>%
        collect() %>%
        mutate(climate = master$climate[i], rep = master$rep[i], size = master$sizeMod[i], freq = master$freqMod[i],
               browsing = master$browsingMod[i], fecundity = master$fecundityMod[i],
               identifier = paste0(master$climate[i], "_rep", master$rep[i], "_size", master$sizeMod[i], "_freq", master$freqMod[i],
                                   "_browsing", master$browsingMod[i], "_fecundity", master$fecundityMod[i])) %>%
        mutate(size = factor(size, levels = rev(c("10", "5", "2", "1"))),
               freq = factor(freq, levels = rev(c("10", "5", "2", "1"))),
               fecundity = factor(fecundity, levels = c("100", "50", "20", "10")),
               browsing = factor(browsing, levels = rev(c("10", "5", "2", "1"))),
               landscape = landscapename)

      # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
      # landscape_remove output ####
      # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
      rem.i <- tbl(db.conn, "landscape_removed") %>%
        dplyr::select(year, species, count, basal_area_m2, volume_m3, dbh_class, reason) %>%
        collect() %>%
        mutate(climate = master$climate[i], rep = master$rep[i], size = master$sizeMod[i], freq = master$freqMod[i],
               browsing = master$browsingMod[i], fecundity = master$fecundityMod[i],
               identifier = paste0(master$climate[i], "_rep", master$rep[i], "_size", master$sizeMod[i], "_freq", master$freqMod[i],
                                   "_browsing", master$browsingMod[i], "_fecundity", master$fecundityMod[i])) %>%
        mutate(size = factor(size, levels = rev(c("10", "5", "2", "1"))),
               freq = factor(freq, levels = rev(c("10", "5", "2", "1"))),
               fecundity = factor(fecundity, levels = c("100", "50", "20", "10")),
               browsing = factor(browsing, levels = rev(c("10", "5", "2", "1"))),
               landscape = landscapename)

      # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
      # dynamicstand output ####
      # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
      ds.i <- tbl(db.conn, "dynamicstand") %>%
        dplyr::select(-ru) %>%
        collect() %>%
        rename(basal_area_m2 = basalarea_sum, count_ha = if_height_0_1_0_sum) %>%
        mutate(climate = master$climate[i], rep = master$rep[i], size = master$sizeMod[i], freq = master$freqMod[i],
               browsing = master$browsingMod[i], fecundity = master$fecundityMod[i],
               identifier = paste0(master$climate[i], "_rep", master$rep[i], "_size", master$sizeMod[i], "_freq", master$freqMod[i],
                                   "_browsing", master$browsingMod[i], "_fecundity", master$fecundityMod[i])) %>%
        mutate(size = factor(size, levels = rev(c("10", "5", "2", "1"))),
               freq = factor(freq, levels = rev(c("10", "5", "2", "1"))),
               fecundity = factor(fecundity, levels = c("100", "50", "20", "10")),
               browsing = factor(browsing, levels = rev(c("10", "5", "2", "1"))),
               landscape = landscapename)

      # reference always baseline_rep1_size1_freq1_browsing1_fecundity100 (also for hot-dry runs)
      # prop of basal area decreased by more than 50 %
      ds.basal.i <- ds.i %>%
        group_by(rid, climate, rep, size, freq, browsing, fecundity, identifier, landscape, year) %>%
        summarise(basal_c = sum(basal_area_m2)) %>% ungroup() %>% # basal area sum in 2020, 2050, 2100
        inner_join(ds.ref %>%
                    filter(identifier == "baseline_rep1_size1_freq1_browsing1_fecundity100") %>%
                    group_by(rid, climate, rep, size, freq, browsing, fecundity, identifier, landscape, year) %>%
                    summarise(basal_ref = sum(basal_area_m2)) %>% ungroup() %>% # basal area sum in 2020, 2050, 2100 under reference conditions
                    dplyr::select(rid, basal_ref, year), by = join_by(rid, year),
                  multiple = "all") %>%
        filter(!is.na(basal_ref)) %>% # filter out RUs for which no reference conditions exist
        group_by(climate, rep, size, freq, browsing, fecundity, identifier, landscape, year) %>%
        mutate(basal_diff = (basal_c-basal_ref)/basal_ref, # relative change in basal area sum
               threshold = ifelse(basal_diff < -0.5, "below", "above"), # count RUs where basal area dropped by more than 50% from reference; unidirectional: only if DECREASE > 50%
               n_rid = length(unique(rid))) %>% ungroup()

      # prop of dominant species changed
      ds.dom.i <- ds.i %>%
        group_by(rid, climate, rep, size, freq, browsing, fecundity, identifier, landscape, year) %>%
        filter(basal_area_m2 == max(basal_area_m2)) %>% ungroup() %>% # species with max basal area in 2020, 2050, 2100
        rename(dom_c = species) %>%
        dplyr::select(rid, climate, rep, size, freq, browsing, fecundity, identifier, landscape, dom_c, year) %>%
        inner_join(ds.ref %>%
                    filter(identifier == "baseline_rep1_size1_freq1_browsing1_fecundity100") %>%
                    group_by(rid, climate, rep, size, freq, browsing, fecundity, identifier, landscape, year) %>%
                    filter(basal_area_m2 == max(basal_area_m2)) %>% ungroup() %>% # species with max basal area in 2020, 2050, 2100 under reference conditions
                    rename(dom_ref = species) %>%
                    dplyr::select(rid, dom_ref, year), by = join_by(rid, year),
                  multiple = "all") %>% #summary()
        filter(!is.na(dom_ref)) %>% # filter out RUs for which no reference conditions exist
        group_by(climate, rep, size, freq, browsing, fecundity, identifier, landscape, year) %>%
        mutate(threshold = ifelse(dom_c == dom_ref, "same", "different"), # count RUs where dominant species changed
               n_rid = length(unique(rid))) %>% ungroup()

      # prop of trees count > 50
      ds.forest.i <- ds.i %>%
        group_by(rid, climate, rep, size, freq, browsing, fecundity, identifier, landscape, year) %>%
        summarise(count_sum_c = sum(count_ha)) %>% ungroup() %>% # tree count sum in 2020, 2050, 2100
        inner_join(ds.ref %>%
                    filter(identifier == "baseline_rep1_size1_freq1_browsing1_fecundity100") %>%
                    group_by(rid, climate, rep, size, freq, browsing, fecundity, identifier, landscape, year) %>%
                    summarise(count_sum_ref = sum(count_ha)) %>% ungroup() %>% # tree count sum in 2020, 2050, 2100 under reference conditions
                    dplyr::select(rid, count_sum_ref, year), by = join_by(rid, year),
                  multiple = "all") %>%
        filter(!is.na(count_sum_ref)) %>% # filter out RUs for which no reference conditions exist
        group_by(climate, rep, size, freq, browsing, fecundity, identifier, landscape, year) %>%
        mutate(threshold = as.factor(ifelse(count_sum_c < 50 & count_sum_ref >= 50, "non-forest", "forest")), # count RUs where tree count dropped below 50 (and reference value is also higher than 50)
               n_rid = length(unique(rid))) %>% ungroup()

      dbDisconnect(db.conn)

      # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
      # patch output ####
      # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
      # requirements for the management intervention to count as a disturbance patch:
      ## needs to be forested (before_ba > 0, n_cells > 0)
      ## trees need to have been killed (killed_ba > 0)

      patch.i <- read_delim(patch_file) %>%
        mutate(landscape=landscapename,
               climate=master$climate[i], rep=master$rep[i], size=master$sizeMod[i], freq=master$freqMod[i],
               browsing=master$browsingMod[i], fecundity=master$fecundityMod[i],
               n_cells = ifelse(killed_ba == 0, 0, n_cells)) %>%
        filter(n_cells > 0)

      # combine all scenarios for the different outputs
      rem <- bind_rows(rem, rem.i); ls <- bind_rows(ls, ls.i); patch <- bind_rows(patch, patch.i)
      ds.basal <- bind_rows(ds.basal, ds.basal.i); ds.dom <- bind_rows(ds.dom, ds.dom.i); ds.forest <- bind_rows(ds.forest, ds.forest.i)
      rm(db.conn, rem.i, ls.i, patch.i, ds.i, ds.basal.i, ds.dom.i, ds.forest.i)
    } else { 
      missing <- c(missing, i); print(paste0("Oh no! Seems like I (", i, ") am missing")) 
    }
    if (i%%64==0) print(paste0(round((i/nrow(master)*100),1), " %: rep ", master$rep[i], ", ", i))
    print(i)
  }
  ds.ls[[landscapename]][["basal"]] <- ds.basal; ds.ls[[landscapename]][["dom"]] <- ds.dom; ds.ls[[landscapename]][["forest"]] <- ds.forest
  missing.ls[[landscapename]] <- missing
  ls.ls[[landscapename]] <- ls; rem.ls[[landscapename]] <- rem; patch.ls[[landscapename]] <- patch
  rm(ls, rem, patch, ds.ref, ds.basal, ds.dom, ds.forest, master, dbname, missing); gc()
  
}; saveRDS(ds.ls, "results/datasets/ds.ls.RDATA"); saveRDS(ls.ls, "results/datasets/ls.ls.RDATA"); saveRDS(patch.ls, "results/datasets/patch.ls.RDATA"); saveRDS(rem.ls, "results/datasets/rem.ls.RDATA"); saveRDS(missing.ls, "results/datasets/missing.ls.RDATA")

# note
## date:
## server:
## landscape(s) finished and saved:

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

### line plots ####

# over time
overTime <- ds.ls[[landscapename]][["basal"]] %>% 
  group_by(threshold, climate, rep, size, freq, browsing, fecundity, identifier, landscape, year) %>% 
  summarise(prop = mean(sum(n())/n_rid)) %>% # convert from count to proportion
  filter(threshold == "above") %>% # only keep proportion of unchanged landscape
  ungroup() %>% rename('1. Structure: basal area decreased by >50 % from reference' = prop) %>% dplyr::select(-threshold) %>% 
  full_join(ds.ls[[landscapename]][["dom"]] %>% 
              group_by(threshold, climate, rep, size, freq, browsing, fecundity, identifier, landscape, year) %>% 
              summarise(prop = mean(sum(n())/n_rid)) %>% # convert from count to proportion
              filter(threshold == "same") %>% # only keep proportion of unchanged landscape
              ungroup() %>% rename('2. Composition: dominant species changed from reference' = prop) %>% dplyr::select(-threshold),
            by = join_by(climate, rep, size, freq, browsing, fecundity, identifier, landscape, year)) %>% 
  full_join(ds.ls[[landscapename]][["forest"]] %>% # summary()
              group_by(threshold, climate, rep, size, freq, browsing, fecundity, identifier, landscape, year) %>% 
              summarise(prop = mean(sum(n())/n_rid)) %>% # convert from count to proportion
              filter(threshold == "forest") %>% # only keep proportion of unchanged landscape
              ungroup() %>% rename('3. Remaining forest: stem density dropping below 50 trees/ha' = prop) %>% dplyr::select(-threshold),
            by = join_by(climate, rep, size, freq, browsing, fecundity, identifier, landscape, year)) 

overTime %>% #summary()
  filter(identifier %in% c("baseline_rep1_size1_freq1_browsing1_fecundity100",
                           "hotdry_rep1_size1_freq1_browsing1_fecundity100")) # test: props should always be 1 for baseline

png(paste0("results/figures/lineplot_overtime_", landscapename ,".png"), res=200,
    height=1500, width=1500)
overTime %>% 
  pivot_longer(cols=10:12) %>% 
  ggplot(aes(x=year+2020, y=value*100, group=identifier, col=climate)) +
  geom_line(linewidth=0.5) +
  facet_wrap(~name, ncol=1) +
  scale_color_manual(values=c("baseline" ="blue", "hotdry" = "red")) +
  labs(x="Year", y="Proportion of unchanged landscape [%]", col="Climate scenario") +
  theme_bw()
dev.off()

# structure: basal area
# var <- "fecundity"
plot.basal.fc <- function(var) {
  others <- vars[vars %ni% var]
  copy <- overTime %>% filter(year==80)
  var_use <- copy %>% pull(var)
  copy$groups <- paste(copy %>% pull(all_of(others[1])),
                       copy %>% pull(all_of(others[2])),
                       copy %>% pull(all_of(others[3])),
                       copy %>% pull(rep),
                       copy %>% pull(climate))
  copy %>% 
    rename(prop = 10) %>% 
    ggplot(aes(x=var_use, y=prop*100, group=groups, col=climate)) +
    geom_line(linewidth=0.8) +
    scale_color_manual(values=c("baseline" ="blue", "hotdry" = "red")) +
    labs(x=var, y="", col="Climate scenario") +
    theme_bw()
  
}; basal.ls <- lapply(vars, plot.basal.fc)
png(paste0("results/figures/lineplot_basal80_", landscapename ,".png"), res=200,
    height=1000, width=2000)
ggpubr::ggarrange(basal.ls[[1]], basal.ls[[2]], basal.ls[[3]], basal.ls[[4]], common.legend = T) %>% 
  annotate_figure(top = "Structure: basal area decreased by >50 % from reference",
                  left = "Proportion of unchanged landscape [%]"
  )
dev.off()

# composition: dominant species
plot.dom.fc <- function(var) {
  others <- vars[vars %ni% var]
  copy <- overTime %>% filter(year==80)
  var_use <- copy %>% pull(var)
  copy$groups <- paste(copy %>% pull(all_of(others[1])),
                       copy %>% pull(all_of(others[2])),
                       copy %>% pull(all_of(others[3])),
                       copy %>% pull(rep),
                       copy %>% pull(climate))
  copy %>% 
    rename(prop = 11) %>% 
    ggplot(aes(x=var_use, y=prop*100, group=groups, col=climate)) +
    geom_line(linewidth=0.8) +
    scale_color_manual(values=c("baseline" ="blue", "hotdry" = "red")) +
    labs(x=var, y="", col="Climate scenario") +
    theme_bw()
  
}; dom.ls <- lapply(vars, plot.dom.fc)

png(paste0("results/figures/lineplot_dom80_", landscapename ,".png"), res=200,
    height=1000, width=2000)
ggpubr::ggarrange(dom.ls[[1]], dom.ls[[2]], dom.ls[[3]], dom.ls[[4]], common.legend = T) %>% 
  annotate_figure(top = "Composition: dominant species changed from reference",
                  left = "Proportion of unchanged landscape [%]"
  )
dev.off()

# forest/non-forest
plot.forest.fc <- function(var) {
  others <- vars[vars %ni% var]
  copy <- overTime %>% filter(year==80)
  var_use <- copy %>% pull(var)
  copy$groups <- paste(copy %>% pull(all_of(others[1])),
                       copy %>% pull(all_of(others[2])),
                       copy %>% pull(all_of(others[3])),
                       copy %>% pull(rep),
                       copy %>% pull(climate))
  copy %>% 
    rename(prop = 12) %>% 
    ggplot(aes(x=var_use, y=prop*100, group=groups, col=climate)) +
    geom_line(linewidth=0.8) +
    scale_color_manual(values=c("baseline" ="blue", "hotdry" = "red")) +
    labs(x=var, y="", col="Climate scenario") +
    theme_bw()
  
}; forest.ls <- lapply(vars, plot.forest.fc)
png(paste0("results/figures/lineplot_forest80_", landscapename ,".png"), res=200,
    height=1000, width=2000)
ggpubr::ggarrange(forest.ls[[1]], forest.ls[[2]], forest.ls[[3]], forest.ls[[4]], common.legend = T) %>% 
  annotate_figure(top = "Remaining forest: stem density dropping below 50 trees/ha",
                  left = "Proportion of unchanged landscape [%]"
  )
dev.off()


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
### maps ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# structure
basal.map <- ds.ls[[landscapename]][["basal"]] %>% 
  filter(year==80,
         threshold=="above") %>%
  group_by(climate) %>% 
  mutate(chances = length(unique(identifier))) %>% ungroup() %>% 
  group_by(rid, climate) %>% 
  summarise(unchanged = mean(sum(n())/chances)*100) %>% ungroup() %>% 
  full_join(rid.df[rid.df$landscape==landscapename,]) %>% 
  dplyr::select(climate, x,y,unchanged) %>% 
  mutate(unchanged = ifelse(is.na(unchanged), 0, unchanged))
png(paste0("results/figures/map_prop_basal80_baseline_", landscapename ,".png"), res=200,
    height=1000, width=1000)
basal.map %>% filter(climate=="baseline") %>% dplyr::select(-climate) %>% filter(!is.na(x)) %>% 
  rast() %>% plot(range=c(0,100), cex.main=0.5, type="continuous",
                  main="1. Structure: basal area decreased by >50 % from reference\nBaseline climate\nPercentage of runs where cell's structure remained unchanged [%]")
dev.off()  
png(paste0("results/figures/map_prop_basal80_hotdry_", landscapename ,".png"), res=200,
    height=1000, width=1000)
basal.map %>% filter(climate=="hotdry") %>% dplyr::select(-climate) %>% filter(!is.na(x)) %>% 
  rast() %>% plot(range=c(0,100), cex.main=0.5, type="continuous",
                  main="1. Structure: basal area decreased by >50 % from reference\nHot-dry climate\nPercentage of runs where cell's structure remained unchanged [%]")
dev.off()  

# composition
dom.map <- ds.ls[[landscapename]][["dom"]] %>% 
  filter(year==80,
         threshold=="same") %>%
  group_by(climate) %>% 
  mutate(chances = length(unique(identifier))) %>% ungroup() %>% 
  group_by(rid, climate) %>% 
  summarise(unchanged = mean(sum(n())/chances)*100) %>% ungroup() %>% 
  full_join(rid.df[rid.df$landscape==landscapename,]) %>% 
  dplyr::select(x,y,unchanged, climate) %>% 
  mutate(unchanged = ifelse(is.na(unchanged), 0, unchanged))
png(paste0("results/figures/map_prop_dom80_baseline_", landscapename ,".png"), res=200,
    height=1000, width=1000)
dom.map %>% filter(climate=="baseline") %>% dplyr::select(-climate) %>% filter(!is.na(x)) %>% 
  rast() %>% plot(range=c(0,100), cex.main=0.5, type="continuous",
                  main="2. Composition: dominant species changed from reference\nBaseline climate\nPercentage of runs where cell's dominant species remained unchanged [%]")
dev.off()
png(paste0("results/figures/map_prop_dom80_hotdry_", landscapename ,".png"), res=200,
    height=1000, width=1000)
dom.map %>% filter(climate=="hotdry") %>% dplyr::select(-climate) %>% filter(!is.na(x)) %>% 
  rast() %>% plot(range=c(0,100), cex.main=0.5, type="continuous",
                  main="2. Composition: dominant species changed from reference\nHot-dry climate\nPercentage of runs where cell's dominant species remained unchanged [%]")
dev.off()

# forest
forest.map <- ds.ls[[landscapename]][["forest"]] %>% 
  filter(year==80,
         threshold=="forest") %>%
  group_by(climate) %>% 
  mutate(chances = length(unique(identifier))) %>% ungroup() %>% 
  group_by(rid, climate) %>% 
  summarise(unchanged = mean(sum(n())/chances)*100) %>% ungroup() %>% 
  full_join(rid.df[rid.df$landscape==landscapename,], by = join_by(rid)) %>% 
  dplyr::select(x,y,unchanged, climate)
png(paste0("results/figures/map_prop_forest80_baseline_", landscapename ,".png"), res=200,
    height=1000, width=1000)
forest.map %>% filter(climate=="baseline") %>% dplyr::select(-climate) %>% filter(!is.na(x)) %>%  
  rast() %>% plot(range=c(0,100), cex.main=0.5, type="continuous",
                  main="3. Remaining forest: stem density dropping below 50 trees/ha\nBaseline climate\nPercentage of runs where cell remained forested [%]")
dev.off()
png(paste0("results/figures/map_prop_forest80_hotdry_", landscapename ,".png"), res=200,
    height=1000, width=1000)
forest.map %>% filter(climate=="hotdry") %>% dplyr::select(-climate) %>% filter(!is.na(x)) %>%  
  rast() %>% plot(range=c(0,100), cex.main=0.5, type="continuous",
                  main="3. Remaining forest: stem density dropping below 50 trees/ha\nHot-dry climate\nPercentage of runs where cell remained forested [%]")
dev.off()


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
## landscape output ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# mean basal area over time
(mean_line <- ls.ls[[landscapename]] %>% 
    group_by(year, climate, size, freq, fecundity, browsing, rep) %>% 
    summarise(n = sum(basal_area_m2)) %>% ungroup() %>% 
    group_by(size, freq, climate, year) %>% 
    summarise(mean = mean(n)) %>% ungroup() %>% 
    mutate(dist=paste(size, freq), distRate = as.numeric(as.character(size)) * as.numeric(as.character(freq))))

png(paste0("results/figures/landscapeWide_basalArea_overTime_", landscapename, ".png"), res=200,
    height=2000, width=4000)
ls.ls[[landscapename]] %>% 
  mutate(groups = paste("size:", size, ", freq:", freq, ", fecundity:", fecundity, ", browsing:", browsing, ", rep:", rep)) %>% 
  group_by(groups, year, climate, rep, size, freq) %>% 
  summarise(n = sum(basal_area_m2)) %>% ungroup() %>% 
  ggplot(aes(x=year, y=n)) +
  geom_line(aes(group=groups), show.legend = F, alpha=0.6) +
  geom_line(data=mean_line, aes(y=mean, col=reorder(dist, distRate)), linewidth=2) +
  facet_grid(~climate) +
  labs(x="Year", y="Mean basal area [m²/ha]", col="Size and frequency modification", 
       title=paste0("How does process modfication impact basal area development in ", toupper(landscapename), "?\nColored lines: mean trajectory for the disturbance regime")) +
  theme_bw()
dev.off()

# final basal area
## stratified by disturbance
png(paste0("results/figures/landscapeWide_finalBasalArea_disturbances_", landscapename, ".png"), res=200,
    height=1600, width=3400)
ls.ls[[landscapename]] %>% 
  mutate(groups = paste("size:", size, ", freq:", freq, ", fecundity:", fecundity, ", browsing:", browsing)) %>% 
  filter(year == 80) %>% 
  group_by(groups, climate, rep, size, freq, fecundity, browsing) %>% 
  summarise(n = sum(basal_area_m2)) %>% 
  mutate(disturbanceRate = as.numeric(as.character(size)) * as.numeric(as.character(freq)),
         level=paste0("x", disturbanceRate, "\nsize x ", size, "\nfreq x ", freq),
         regen = paste(fecundity, browsing)) %>% 
  mutate(regen=factor(regen, levels=c("100 1", "100 2", "50 1", "50 2", "100 5", "20 1", "100 10",
                                      "50 5", "20 2", "10 1", "50 10", "10 2", "20 5", "20 10", "10 5", "10 10"))) %>% 
  ggplot(aes(x=reorder(level, disturbanceRate), y=n, col=regen)) +
  geom_point(size=4, col="black") +
  geom_point(size=3) +
  geom_hline(aes(yintercept = ls.ls[["bgd"]] %>% filter(year==0) %>% pull(basal_area_m2) %>% unique() %>% sum()),
             col="black") +
  facet_grid(~climate) +
  labs(x="Disturbance rate modification", y="Mean basal area in 2100 [m²/ha]",
       col="Fecundity and browsing modification level",
       title="Basal area in 2100: all 256 possible process modification combinations\nSolid line: value in 2020") +
  theme_bw() +
  theme(legend.position="bottom")
dev.off()

## stratified by regeneration
png(paste0("results/figures/landscapeWide_finalBasalArea_regeneration_", landscapename, ".png"), res=200,
    height=1600, width=3400)
ls.ls[[landscapename]] %>% 
  mutate(groups = paste("size:", size, ", freq:", freq, ", fecundity:", fecundity, ", browsing:", browsing)) %>% 
  filter(year == 80) %>% 
  group_by(groups, climate, rep, size, freq, fecundity, browsing) %>% 
  summarise(n = sum(basal_area_m2)) %>% 
  mutate(level=paste0("fecundity x ", as.numeric(as.character(fecundity))/100, "\nbrowsing x ", browsing),
         regen=1/as.numeric(as.character(fecundity))*as.numeric(as.character(browsing)),
         dist = paste(size, freq)) %>% 
  mutate(dist=factor(dist, levels=c("1 1", "1 2", "2 1", "2 2", "1 5", "5 1", "1 10", "10 1",
                                    "2 5", "5 2", "10 2", "2 10", "5 5", "10 5", "5 10", "10 10"))) %>% 
  ggplot(aes(x=reorder(level, regen), y=n, col=dist)) +
  geom_point(size=4, col="black") +
  geom_point(size=3) +
  geom_hline(aes(yintercept = ls.ls[["bgd"]] %>% filter(year==0) %>% pull(basal_area_m2) %>% unique() %>% sum()),
             col="black") +
  facet_grid(~climate) +
  labs(x="Disturbance rate modification", y="Mean basal area in 2100 [m²/ha]",
       col="Fecundity and browsing modification level",
       title="Basal area in 2100: all 256 possible process modification combinations\nSolid line: value in 2020") +
  theme_bw() +
  theme(legend.position="bottom")
dev.off()

# regeneration processes: final basal area
png(paste0("results/figures/landscapeWide_basalArea_regeneration_", landscapename, ".png"), res=200,
    height=2000, width=3000)
ls.ls[[landscapename]] %>% 
  filter(year > 70) %>% # last simulation decade
  mutate(distRate = as.numeric(size)*as.numeric(freq), 
         groups = paste("fecundity:", fecundity, "\nbrowsing:", browsing)) %>% 
  group_by(fecundity, browsing, distRate, climate, rep, size, freq) %>% 
  summarise(n = sum(basal_area_m2)/10) %>% 
  filter(distRate==1) %>% 
  ggplot() +
  geom_line(aes(x=as.numeric(as.character(browsing)), y=n, col=as.factor(as.numeric(as.character(fecundity)))),
            linewidth=2) +
  facet_grid(~climate) +
  labs(x="browsing modification", y="Mean basal area in last sim. decade [m²/ha]", col="Fecundity modification [%]", 
       title=paste0("Modified regeneration: impact on basal area in ", toupper(landscapename), "\nDisturbance rate kept constant at original value\nBlack star: disturbances simulated via modules instead of management")) +
  theme_bw()
dev.off()


# dominant species
mean_line.species <- ls.ls[[landscapename]] %>% 
  group_by(year, climate, rep, size, freq, fecundity, browsing) %>% 
  mutate(total = sum(basal_area_m2)) %>% 
  filter(species == common.species[[landscapename]][1]) %>% 
  summarise(n = basal_area_m2/total) %>% 
  group_by(size, freq, climate, year) %>% 
  summarise(mean = mean(n)) %>% ungroup() %>% 
  mutate(dist=paste(size, freq), distRate = as.numeric(size) * as.numeric(freq)) 
png(paste0("results/figures/landscapeWide_mostCommonSpecies_overTime_", landscapename, ".png"), res=200,
    height=2000, width=4000)
ls.ls[[landscapename]] %>% 
  mutate(groups = paste("size:", size, ", freq:", freq, ", fecundity:", fecundity, ", browsing:", browsing, ", rep:", rep)) %>% 
  group_by(groups, year, climate, rep) %>% 
  mutate(total = sum(basal_area_m2)) %>% 
  filter(species == common.species[[landscapename]][1]) %>% 
  summarise(n = basal_area_m2/total) %>% 
  ggplot(aes(x=year, y=n*100)) +
  geom_line(aes(group=groups), show.legend = F, alpha=0.6) +
  geom_line(data=mean_line.species, aes(y=mean*100, col=reorder(dist, distRate)), linewidth=2) +
  facet_grid(~climate) +
  labs(x="Year", y=paste0("Proportion of ", common.species[[landscapename]][1], " [%, basal area]"), col="", 
       title=paste0("How does the most common species fare in ", toupper(landscapename), "?\nDotted red line: simulation with disturbances simulated via modules instead of management\nColored lines: mean trajectory for the disturbance regime")) +
  theme_bw()
dev.off()

# # map: spruce dominance
# dist.ds %>% filter(climate=="hotdry", year == 80) %>% group_by(rid) %>% 
#   filter(basalarea_sum==max(basalarea_sum)) %>% ungroup() %>% full_join(rid.df[rid.df$landscape=="bgd",]) %>%
#   dplyr::select(x,y,species) %>% mutate(species = ifelse(species == "piab", 1, 0)) %>% 
#   rast() %>% plot()

# species composition: size x 10, freq x 10
label.df <- data.frame(year=15, basal_area_m2=0.25, label=c("Reference conditions", "Most extreme scenario"),
                       groups=c("size: 1 , freq: 1 , fecundity: 100 , browsing: 1",
                                "size: 10 , freq: 10 , fecundity: 10 , browsing: 10"),
                       climate="baseline")
png(paste0("results/figures/landscapeWide_speciesComposition_relative_extremes_", landscapename, ".png"), res=200,
    height=2000, width=3000)
ls.ls[[landscapename]] %>% 
  filter(identifier %in% c("baseline_rep1_size1_freq1_browsing1_fecundity100", "baseline_rep1_size10_freq10_browsing10_fecundity10",
                           "hotdry_rep1_size1_freq1_browsing1_fecundity100", "hotdry_rep1_size10_freq10_browsing10_fecundity10")) %>% 
  mutate(species = ifelse(species %in% common.species[[landscapename]], species, "other"),
         groups = paste("size:", size, ", freq:", freq, ", fecundity:", fecundity, ", browsing:", browsing)) %>% 
  ggplot(aes(x=year, y=basal_area_m2)) +
  geom_bar(aes(fill=species), stat="identity", position=position_fill(), width=1.1) +
  geom_label(data=label.df, aes(label=label)) +
  facet_grid(groups~climate) +
  labs(x="Year", fill="Species", y="Proportion of basal area") +
  theme_bw()
dev.off()

# absolute
png(paste0("results/figures/landscapeWide_speciesComposition_absolute_extremes_", landscapename, ".png"), res=200,
    height=2000, width=3000)
ls.ls[[landscapename]] %>% 
  filter(identifier %in% c("baseline_rep1_size1_freq1_browsing1_fecundity100", "baseline_rep1_size10_freq10_browsing10_fecundity10",
                           "hotdry_rep1_size1_freq1_browsing1_fecundity100", "hotdry_rep1_size10_freq10_browsing10_fecundity10")) %>% 
  mutate(species = ifelse(species %in% common.species[[landscapename]], species, "other"),
         groups = paste("size:", size, ", freq:", freq, ", fecundity:", fecundity, ", browsing:", browsing)) %>% 
  ggplot(aes(x=year, y=basal_area_m2)) +
  geom_bar(aes(fill=species), stat="identity", width=1.1) +
  facet_grid(groups~climate) +
  labs(x="Year", fill="Species", y="Mean basal area [m²/ha]") +
  theme_bw()
dev.off()

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
## trees killed output ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

mean_line.rem <- rem.ls[[landscapename]] %>% 
  group_by(year, climate, rep, size, freq, fecundity, browsing) %>% 
  summarise(n = sum(basal_area_m2)) %>% 
  group_by(size, freq, climate, year) %>% 
  summarise(mean = mean(n)) %>% ungroup() %>% 
  mutate(dist=paste(size, freq), distRate = as.numeric(size) * as.numeric(freq)) 

png(paste0("results/figures/landscapeWide_treesKilled_", landscapename, ".png"), res=200,
    height=2000, width=3000)
rem.ls[[landscapename]] %>% 
  mutate(groups = paste("size:", size, ", freq:", freq, ", fecundity:", fecundity, ", browsing:", browsing)) %>% 
  group_by(groups, year, climate, rep) %>% 
  summarise(n = sum(basal_area_m2)) %>% 
  ggplot(aes(x=year, y=n)) +
  geom_line(aes(group=groups), show.legend = F, alpha=0.6) +
  geom_line(data=mean_line.rem, aes(y=mean, col=reorder(dist, distRate)), linewidth=2) +
  scale_y_log10() +
  facet_grid(~climate) +
  labs(x="Year", y="Basal area killed by disturbance [m²]", col="", 
       title=paste0("How does process modfication impact tree mortality in ", toupper(landscapename), "?\nBlack line: simulation with disturbances simulated via modules instead of management\nY-axis log10-transformed")) +
  theme_bw()
dev.off()

rem.ls[[landscapename]] %>% filter(climate=="baseline") %>% pull(basal_area_m2) %>% summary()
rem.ls[[landscapename]] %>% filter(climate=="hotdry") %>% pull(basal_area_m2) %>% summary()
rem.ls[[landscapename]] %>% group_by(climate) %>% summarise(sum_basal = sum(basal_area_m2))

## patch output ####
ref.df <- read_csv("raw_data/helper_files/ref.df.csv")
patch.ls[[landscapename]] %>% 
  group_by(landscape, climate, rep, size, freq, browsing, fecundity, agent) %>% 
  summarise(size_actual = mean(n_cells), 
            events_actual = n()) %>% 
  full_join(ref.df[ref.df$landscape==landscapename,]) %>% 
  mutate(size_expected = size_expected * mean(as.numeric(size)),
         events_expected = events_expected * mean(as.numeric(freq)),
         area_expected = size_expected * events_expected/100,
         area_actual = size_actual * events_actual/100,
         area_diff = (area_actual-area_expected)/area_expected) %>% #summary()
  ggplot() + geom_histogram(aes(x=area_diff)) + theme_bw()


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# DISCARDED ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

