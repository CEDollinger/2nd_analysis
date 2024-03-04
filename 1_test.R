library(tidyverse)
library(ggpubr)
library(RSQLite); library(dbplyr)
library(terra)
library(beepr)
"%ni%" <- Negate("%in%")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); getwd() # set wd to file location
landscapes <- c("bgd", "grte", "stoko")
vars <- c("size", "freq", "fecundity", "browsing")

# set up ####
# Most common species ###
(common.bgd <- dbConnect(RSQLite::SQLite(), paste0("../project2ndStudy/projectTest_bgd/output/output_bgd_baseline_rep1_size1_freq1_browsing1_fecundity100.sqlite")) %>%
   tbl(., "landscape") %>% collect() %>%
   group_by(species) %>% summarise(a=mean(basal_area_m2)) %>% arrange(desc(a)) %>% slice(1:8) %>% pull(species))
# always the same 8 most common species!

(common.grte <- dbConnect(RSQLite::SQLite(), paste0("../project2ndStudy/projectTest_grte/output/output_grte_baseline_rep1_size1_freq1_browsing1_fecundity100.sqlite")) %>%
    tbl(., "landscape") %>% collect() %>%
    group_by(species) %>% summarise(a=mean(basal_area_m2)) %>% arrange(desc(a)) %>% slice(1:8) %>% pull(species))
# always the same 8 most common species!

(common.stoko <- dbConnect(RSQLite::SQLite(), paste0("../project2ndStudy/projectTest_stoko/output/output_stoko_baseline_rep1_size1_freq1_browsing1_fecundity100.sqlite")) %>%
    tbl(., "landscape") %>% collect() %>%
    group_by(species) %>% summarise(a=mean(basal_area_m2)) %>% arrange(desc(a)) %>% slice(1:8) %>% 
    pull(species))
# always the same 5 (and 8) most common species!
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
# A. --- ####################################################################################################################
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

landscapename <- "grte"
# all possible simulation runs (for now)
sims <- expand_grid(climate=c("baseline", "hotdry"), rep=c("1"), 
                    sizeMods=c("1", "2", "5", "10"), freqMods=c("1", "2", "5", "10"), 
                    browsingMods=c("1", "2", "5", "10"), fecundityMods= c("100", "50", "20", "10")); sims$dbname <- 
  paste0("output_", landscapename, "_", sims$climate, "_rep", sims$rep, "_size", sims$sizeMods, 
         "_freq", sims$freqMods, "_browsing", sims$browsingMods, "_fecundity", sims$fecundityMods, ".sqlite"); sims
ist <- data.frame(dbname=dir(paste0("../project2ndStudy/projectTest_", landscapename, "/output/"), pattern = "browsing"))  

# identify which simulations are missing
missing <- anti_join(sims, ist)

# read in landscape and landscape_removed output
rem <- data.frame(); ls <- data.frame(); ds <- data.frame(); i<-1
for (i in 1:nrow(sims)) {
  dbname <- paste0("../project2ndStudy/projectTest_", landscapename, "/output/", sims$dbname[i])
  if (file.exists(dbname)) {
    db.conn <- dbConnect(RSQLite::SQLite(),
                         dbname = dbname) # output_baseline_rep1_size1_freq1_browsing1_fecundity75
    ls.i <- tbl(db.conn, "landscape") %>% 
      dplyr::select(year, species, count_ha, basal_area_m2, gwl_m3, volume_m3, height_avg_m) %>% 
      collect() %>% 
      mutate(climate = sims$climate[i], rep = sims$rep[i], size = sims$sizeMods[i], freq = sims$freqMods[i], 
             browsing = sims$browsingMods[i], fecundity = sims$fecundityMods[i],
             identifier = paste0(sims$climate[i], "_rep", sims$rep[i], "_size", sims$sizeMods[i], "_freq", sims$freqMods[i], 
                                 "_browsing", sims$browsingMods[i], "_fecundity", sims$fecundityMods[i])) %>% 
      mutate(size = factor(size, levels = rev(c("10", "5", "2", "1"))),
             freq = factor(freq, levels = rev(c("10", "5", "2", "1"))),
             fecundity = factor(fecundity, levels = c("100", "50", "20", "10")),
             browsing = factor(browsing, levels = rev(c("10", "5", "2", "1"))),
             landscape = landscapename)
    rem.i <- tbl(db.conn, "landscape_removed") %>% 
      dplyr::select(year, species, count, basal_area_m2, volume_m3, dbh_class, reason) %>% 
      collect() %>% 
      mutate(climate = sims$climate[i], rep = sims$rep[i], size = sims$sizeMods[i], freq = sims$freqMods[i], 
             browsing = sims$browsingMods[i], fecundity = sims$fecundityMods[i],
             identifier = paste0(sims$climate[i], "_rep", sims$rep[i], "_size", sims$sizeMods[i], "_freq", sims$freqMods[i], 
                                 "_browsing", sims$browsingMods[i], "_fecundity", sims$fecundityMods[i])) %>% 
      mutate(size = factor(size, levels = rev(c("10", "5", "2", "1"))),
             freq = factor(freq, levels = rev(c("10", "5", "2", "1"))),
             fecundity = factor(fecundity, levels = c("100", "50", "20", "10")),
             browsing = factor(browsing, levels = rev(c("10", "5", "2", "1"))),
             landscape = landscapename)
    ds.i <- tbl(db.conn, "dynamicstand") %>% 
      dplyr::select(-ru) %>% 
      collect() %>% 
      rename(basal_area_m2 = basalarea_sum, count_ha = if_height_0_1_0_sum) %>% 
      mutate(climate = sims$climate[i], rep = sims$rep[i], size = sims$sizeMods[i], freq = sims$freqMods[i], 
             browsing = sims$browsingMods[i], fecundity = sims$fecundityMods[i],
             identifier = paste0(sims$climate[i], "_rep", sims$rep[i], "_size", sims$sizeMods[i], "_freq", sims$freqMods[i], 
                                 "_browsing", sims$browsingMods[i], "_fecundity", sims$fecundityMods[i])) %>% 
      mutate(size = factor(size, levels = rev(c("10", "5", "2", "1"))),
             freq = factor(freq, levels = rev(c("10", "5", "2", "1"))),
             fecundity = factor(fecundity, levels = c("100", "50", "20", "10")),
             browsing = factor(browsing, levels = rev(c("10", "5", "2", "1"))),
             landscape = landscapename)
    dbDisconnect(db.conn)
    rem <- bind_rows(rem, rem.i); ls <- bind_rows(ls, ls.i); ds <- bind_rows(ds, ds.i)
    rm(db.conn, rem.i, ls.i, ds.i)
  }
  if (i%%16==0) print(paste0(round((i/nrow(sims)*100),1), " %: ", dbname))
}; beep(0)

saveRDS(ls, paste0("results/datasets/ls_", landscapename, "_raw.RDATA"))
saveRDS(ds, paste0("results/datasets/ds_", landscapename, "_raw.RDATA"))
saveRDS(rem, paste0("results/datasets/rem_", landscapename, "_raw.RDATA")); beep(0)

ls <- readRDS(paste0("results/datasets/ls_", landscapename, "_raw.RDATA"))
ds <- readRDS(paste0("results/datasets/ds_", landscapename, "_raw.RDATA"))
rem <- readRDS(paste0("results/datasets/rem_", landscapename, "_raw.RDATA"))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
## dynamic stand ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

### datasets ####

ds.ls <- readRDS(paste0("results/datasets/ds.ls_", landscapename, ".RDATA"))

ds.ls <- list(tibble(), tibble(), tibble()); names(ds.ls) <- c("basal", "dom", "forest")

# reference always baseline_rep1_size1_freq1_browsing1_fecundity100 (also for hot-dry runs)
# prop of basal area decreased by more than 50 %
ds.ls[["basal"]] <- ds %>% 
  group_by(rid, climate, rep, size, freq, browsing, fecundity, identifier, landscape, year) %>% 
  summarise(basal_c = sum(basal_area_m2)) %>% ungroup() %>% # basal area sum in 2020, 2050, 2100 
  left_join(ds %>% 
              filter(identifier == "baseline_rep1_size1_freq1_browsing1_fecundity100") %>% 
              group_by(rid, climate, rep, size, freq, browsing, fecundity, identifier, landscape, year) %>% 
              summarise(basal_ref = sum(basal_area_m2)) %>% ungroup() %>% # basal area sum in 2020, 2050, 2100 under reference conditions
              dplyr::select(rid, basal_ref, year),
            multiple = "all") %>% 
  filter(!is.na(basal_ref)) %>% # filter out RUs for which no reference conditions exist
  group_by(climate, rep, size, freq, browsing, fecundity, identifier, landscape, year) %>% 
  mutate(basal_diff = (basal_c-basal_ref)/basal_ref, # relative change in basal area sum
         threshold = ifelse(basal_diff < -0.5, "below", "above"), # count RUs where basal area dropped by more than 50% from reference; unidirectional: only if DECREASE > 50%
         n_rid = length(unique(rid))) %>% ungroup()

# prop of dominant species changed 
ds.ls[["dom"]] <- ds %>% 
  group_by(rid, climate, rep, size, freq, browsing, fecundity, identifier, landscape, year) %>% 
  filter(basal_area_m2 == max(basal_area_m2)) %>% ungroup() %>% # species with max basal area in 2020, 2050, 2100 
  rename(dom_c = species) %>% 
  dplyr::select(rid, climate, rep, size, freq, browsing, fecundity, identifier, landscape, dom_c, year) %>% 
  full_join(ds %>% 
              filter(identifier == "baseline_rep1_size1_freq1_browsing1_fecundity100") %>% 
              group_by(rid, climate, rep, size, freq, browsing, fecundity, identifier, landscape, year) %>% 
              filter(basal_area_m2 == max(basal_area_m2)) %>% ungroup() %>% # species with max basal area in 2020, 2050, 2100 under reference conditions
              rename(dom_ref = species) %>% 
              dplyr::select(rid, dom_ref, year),
            multiple = "all") %>% #summary()
  filter(!is.na(dom_ref)) %>% # filter out RUs for which no reference conditions exist
  group_by(climate, rep, size, freq, browsing, fecundity, identifier, landscape, year) %>% 
  mutate(threshold = ifelse(dom_c == dom_ref, "same", "different"), # count RUs where dominant species changed
         n_rid = length(unique(rid))) %>% ungroup() 


# prop of trees count > 50 
ds.ls[["forest"]] <- ds %>% 
  group_by(rid, climate, rep, size, freq, browsing, fecundity, identifier, landscape, year) %>% 
  summarise(count_sum_c = sum(count_ha)) %>% ungroup() %>% # tree count sum in 2020, 2050, 2100 
  full_join(ds %>% 
              filter(identifier == "baseline_rep1_size1_freq1_browsing1_fecundity100") %>% 
              group_by(rid, climate, rep, size, freq, browsing, fecundity, identifier, landscape, year) %>% 
              summarise(count_sum_ref = sum(count_ha)) %>% ungroup() %>% # tree count sum in 2020, 2050, 2100 under reference conditions
              dplyr::select(rid, count_sum_ref, year),
            multiple = "all") %>% 
  filter(!is.na(count_sum_ref)) %>% # filter out RUs for which no reference conditions exist
  group_by(climate, rep, size, freq, browsing, fecundity, identifier, landscape, year) %>% 
  mutate(threshold = as.factor(ifelse(count_sum_c < 50 & count_sum_ref >= 50, "non-forest", "forest")), # count RUs where tree count dropped below 50 (and reference value is also higher than 50)
         n_rid = length(unique(rid))) %>% ungroup() 

saveRDS(ds.ls, paste0("results/datasets/ds.ls_", landscapename, ".RDATA")); beep(0)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

### line plots ####

# over time
overTime <- ds.ls[["basal"]] %>% 
  group_by(threshold, climate, rep, size, freq, browsing, fecundity, identifier, landscape, year) %>% 
  summarise(prop = mean(sum(n())/n_rid)) %>% # convert from count to proportion
  filter(threshold == "above") %>% # only keep proportion of unchanged landscape
  ungroup() %>% rename('1. Structure: basal area decreased by >50 % from reference' = prop) %>% dplyr::select(-threshold) %>% 
  full_join(ds.ls[["dom"]] %>% 
              group_by(threshold, climate, rep, size, freq, browsing, fecundity, identifier, landscape, year) %>% 
              summarise(prop = mean(sum(n())/n_rid)) %>% # convert from count to proportion
              filter(threshold == "same") %>% # only keep proportion of unchanged landscape
              ungroup() %>% rename('2. Composition: dominant species changed from reference' = prop) %>% dplyr::select(-threshold)) %>% 
  full_join(ds.ls[["forest"]] %>% # summary()
              group_by(threshold, climate, rep, size, freq, browsing, fecundity, identifier, landscape, year) %>% 
              summarise(prop = mean(sum(n())/n_rid)) %>% # convert from count to proportion
              filter(threshold == "forest") %>% # only keep proportion of unchanged landscape
              ungroup() %>% rename('3. Remaining forest: stem density dropping below 50 trees/ha' = prop) %>% dplyr::select(-threshold)) 

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
basal.map <- ds.ls[["basal"]] %>% 
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
dom.map <- ds.ls[["dom"]] %>% 
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
forest.map <- ds.ls[["forest"]] %>% 
  filter(year==80,
         threshold=="forest") %>%
  group_by(climate) %>% 
  mutate(chances = length(unique(identifier))) %>% ungroup() %>% 
  group_by(rid, climate) %>% 
  summarise(unchanged = mean(sum(n())/chances)*100) %>% ungroup() %>% 
  full_join(rid.df[rid.df$landscape==landscapename,]) %>% 
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


# maps for different runs
basal.rast.df <- ds.ls[["basal"]] %>% 
  filter(year==80,
         identifier %in% c("baseline_rep1_size10_freq10_browsing10_fecundity10",
                           "baseline_rep1_size1_freq1_browsing1_fecundity10",
                           "baseline_rep1_size1_freq1_browsing10_fecundity100",
                           "baseline_rep1_size1_freq10_browsing1_fecundity100",
                           "baseline_rep1_size10_freq1_browsing1_fecundity100")) %>% 
         mutate(year = paste("year", year, sep=""))
basal.rast.ls <- base::split(basal.rast.df, list(basal.rast.df$identifier, basal.rast.df$year), sep="; ")

toRast.fc <- function(x) {
  x %>% full_join(rid.df[rid.df$landscape=="bgd",]) %>% 
    dplyr::select(x,y,basal_diff) %>% 
    rast() 
}

interim<-lapply(basal.rast.ls, toRast.fc); names(interim) <- names(basal.rast.ls)
basal.rast <- rast()
for (i in 1:length(interim)) basal.rast <- c(basal.rast, unlist(interim[[i]]))
names(basal.rast) <- names(basal.rast.ls); rm(interim)

png(paste0("results/figures/map_basal80diff_baseline_", landscapename ,".png"), res=200,
    height=1000, width=2000)
basal.rast %>% plot(range=c(-1,0))
dev.off()

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
## landscape output ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# runs with disturbance modules
dist <- readRDS(paste0("results/datasets/dist_", landscapename, ".RDATA"))
dist.ls <- readRDS(paste0("results/datasets/dist.ls_", landscapename, ".RDATA"))
dist.ds <- readRDS(paste0("results/datasets/dist.ds_", landscapename, ".RDATA"))

# baseline climate
db.conn <- dbConnect(RSQLite::SQLite(), paste0("../project2ndStudy/projectTest_", landscapename,"/output/",landscapename, "_baseline_disturbanceModulesEnabled.sqlite"))
dist1 <- tbl(db.conn, "landscape_removed") %>% 
  collect() %>% 
  mutate(climate = "baseline")
dist.ls1 <- tbl(db.conn, "landscape") %>% 
  collect() %>% 
  mutate(climate = "baseline")
dist.ds1 <- tbl(db.conn, "dynamicstand") %>% 
  collect() %>% 
  mutate(climate = "baseline")
dbDisconnect(db.conn); rm(db.conn)
# hotdry climate
db.conn <- dbConnect(RSQLite::SQLite(), paste0("../project2ndStudy/projectTest_", landscapename,"/output/",landscapename, "_hotdry_disturbanceModulesEnabled.sqlite"))
dist2 <- tbl(db.conn, "landscape_removed") %>% 
  collect() %>% 
  mutate(climate = "hotdry")
dist.ls2 <- tbl(db.conn, "landscape") %>% 
  collect() %>% 
  mutate(climate = "hotdry")
dist.ds2 <- tbl(db.conn, "dynamicstand") %>% 
  collect() %>% 
  mutate(climate = "hotdry")
dbDisconnect(db.conn); rm(db.conn)
dist <- bind_rows(dist1, dist2); rm(dist1, dist2)
dist.ls <- bind_rows(dist.ls1, dist.ls2); rm(dist.ls1, dist.ls2)
dist.ds <- bind_rows(dist.ds1, dist.ds2); rm(dist.ds1, dist.ds2)
saveRDS(dist, paste0("results/datasets/dist_", landscapename, ".RDATA"))
saveRDS(dist.ls, paste0("results/datasets/dist.ls_", landscapename, ".RDATA"))
saveRDS(dist.ds, paste0("results/datasets/dist.ds_", landscapename, ".RDATA"))

# mean basal area over time
(mean_line <- ls %>% 
    group_by(year, climate, rep, size, freq, fecundity, browsing) %>% 
    summarise(n = sum(basal_area_m2)) %>% ungroup() %>% 
    group_by(size, freq, climate, rep, year) %>% 
    summarise(mean = mean(n)) %>% ungroup() %>% 
    mutate(dist=paste(size, freq), distRate = as.numeric(as.character(size)) * as.numeric(as.character(freq))))

png(paste0("results/figures/landscapeWide_basalArea_overTime_", landscapename, ".png"), res=200,
    height=2000, width=4000)
ls %>% 
  mutate(groups = paste("size:", size, ", freq:", freq, ", fecundity:", fecundity, ", browsing:", browsing)) %>% 
  group_by(groups, year, climate, rep, size, freq) %>% 
  summarise(n = sum(basal_area_m2)) %>% ungroup() %>% 
  ggplot(aes(x=year, y=n)) +
  geom_line(aes(group=groups), show.legend = F, alpha=0.6) +
  geom_line(data=mean_line, aes(y=mean, col=reorder(dist, distRate)), linewidth=2) +
  geom_line(data =  dist.ls %>% group_by(year, climate) %>% summarise(n = sum(basal_area_m2)) %>% mutate(groups = "Disturbance modules"),
            col="red", linetype = 2, linewidth=2) +
  facet_grid(~climate) +
  labs(x="Year", y="Mean basal area [m²/ha]", col="Size and frequency modification", 
       title=paste0("How does process modfication impact basal area development in ", toupper(landscapename), "?\nDotted red line: simulation with disturbances simulated via modules instead of management\nColored lines: mean trajectory for the disturbance regime")) +
  theme_bw()
dev.off()

# final basal area
## stratified by disturbance
png(paste0("results/figures/landscapeWide_finalBasalArea_disturbances_", landscapename, ".png"), res=200,
    height=1600, width=3400)
ls %>% 
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
  # geom_hline(aes(yintercept=dist.ls %>% filter(year==80, climate=="baseline") %>% pull(basal_area_m2) %>% unique() %>% sum()),
  #            linetype=3, linewidth=2) +
  geom_hline(aes(yintercept = ls %>% filter(year==0) %>% pull(basal_area_m2) %>% unique() %>% sum()),
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
ls %>% 
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
  geom_hline(aes(yintercept=dist.ls %>% filter(year==80, climate=="baseline") %>% pull(basal_area_m2) %>% unique() %>% sum()),
             linetype=3, linewidth=2) +
  geom_hline(aes(yintercept = ls %>% filter(year==0) %>% pull(basal_area_m2) %>% unique() %>% sum()),
             col="black") +
  facet_grid(~climate) +
  labs(x="Disturbance rate modification", y="Mean basal area in 2100 [m²/ha]",
       col="Fecundity and browsing modification level",
       title="Basal area in 2100: all 256 possible process modification combinations\nDotted line: value in 2100 with disturbance simulated via modules, baseline climate\nSolid line: value in 2020") +
  theme_bw() +
  theme(legend.position="bottom")
dev.off()

# regeneration processes: final basal area
png(paste0("results/figures/landscapeWide_basalArea_regeneration_", landscapename, ".png"), res=200,
    height=2000, width=3000)
ls %>% 
  filter(year > 70) %>% # last simulation decade
  mutate(distRate = as.numeric(size)*as.numeric(freq), 
         groups = paste("fecundity:", fecundity, "\nbrowsing:", browsing)) %>% 
  group_by(fecundity, browsing, distRate, climate, rep, size, freq) %>% 
  summarise(n = sum(basal_area_m2)/10) %>% 
  filter(distRate==1) %>% 
  ggplot() +
  geom_line(aes(x=as.numeric(as.character(browsing)), y=n, col=as.factor(as.numeric(as.character(fecundity)))),
            linewidth=2) +
  geom_point(data =  dist.ls %>% filter(year > 70, climate=="baseline") %>% group_by(climate) %>% summarise(n = sum(basal_area_m2)/10),
             aes(x=1, y=n), col="black", size=4, shape=8) +
  facet_grid(~climate) +
  labs(x="browsing modification", y="Mean basal area in last sim. decade [m²/ha]", col="Fecundity modification [%]", 
       title=paste0("Modified regeneration: impact on basal area in ", toupper(landscapename), "\nDisturbance rate kept constant at original value\nBlack star: disturbances simulated via modules instead of management")) +
  theme_bw()
dev.off()


# dominant species
mean_line.species <- ls %>% 
  group_by(year, climate, rep, size, freq, fecundity, browsing) %>% 
  mutate(total = sum(basal_area_m2)) %>% 
  filter(species == common.species[[landscapename]][1]) %>% 
  summarise(n = basal_area_m2/total) %>% 
  group_by(size, freq, climate, rep, year) %>% 
  summarise(mean = mean(n)) %>% ungroup() %>% 
  mutate(dist=paste(size, freq), distRate = as.numeric(size) * as.numeric(freq)) 
png(paste0("results/figures/landscapeWide_mostCommonSpecies_overTime_", landscapename, ".png"), res=200,
    height=2000, width=4000)
ls %>% 
  mutate(groups = paste("size:", size, ", freq:", freq, ", fecundity:", fecundity, ", browsing:", browsing)) %>% 
  group_by(groups, year, climate, rep) %>% 
  mutate(total = sum(basal_area_m2)) %>% 
  filter(species == common.species[[landscapename]][1]) %>% 
  summarise(n = basal_area_m2/total) %>% 
  ggplot(aes(x=year, y=n*100)) +
  geom_line(aes(group=groups), show.legend = F, alpha=0.6) +
  geom_line(data=mean_line.species, aes(y=mean*100, col=reorder(dist, distRate)), linewidth=2) +
  geom_line(data =  dist.ls %>% group_by(year, climate) %>% 
              mutate(total = sum(basal_area_m2)) %>% 
              filter(species == common.species[[landscapename]][1]) %>% 
              mutate(n = basal_area_m2/total),
            col="red", linetype = 2, linewidth=2) +
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
ls %>% 
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
ls %>% 
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
# trees killed
mean_line.rem <- rem %>% 
  group_by(year, climate, rep, size, freq, fecundity, browsing) %>% 
  summarise(n = sum(basal_area_m2)) %>% 
  group_by(size, freq, climate, rep, year) %>% 
  summarise(mean = mean(n)) %>% ungroup() %>% 
  mutate(dist=paste(size, freq), distRate = as.numeric(size) * as.numeric(freq)) 

png(paste0("results/figures/landscapeWide_treesKilled_", landscapename, ".png"), res=200,
    height=2000, width=3000)
rem %>% 
  mutate(groups = paste("size:", size, ", freq:", freq, ", fecundity:", fecundity, ", browsing:", browsing)) %>% 
  group_by(groups, year, climate, rep) %>% 
  summarise(n = sum(basal_area_m2)) %>% 
  ggplot(aes(x=year, y=n)) +
  # geom_line(aes(group=groups), show.legend = F, alpha=0.6) +
  geom_line(data=mean_line.rem, aes(y=mean, col=reorder(dist, distRate)), linewidth=2) +
  geom_line(data =  dist %>% filter(climate=="baseline") %>% group_by(year, climate) %>% summarise(n = sum(basal_area_m2)) %>% mutate(groups = "Disturbance modules"),
            col="black", linewidth=2) +
  scale_y_log10() +
  facet_grid(~climate) +
  labs(x="Year", y="Basal area killed by disturbance [m²]", col="", 
       title=paste0("How does process modfication impact tree mortality in ", toupper(landscapename), "?\nBlack line: simulation with disturbances simulated via modules instead of management\nY-axis log10-transformed")) +
  theme_bw()
dev.off()

rem %>% filter(climate=="baseline") %>% pull(basal_area_m2) %>% summary()
rem %>% filter(climate=="hotdry") %>% pull(basal_area_m2) %>% summary()
# different, but barely
