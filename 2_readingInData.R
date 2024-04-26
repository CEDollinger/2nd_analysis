# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Reading in data ###########################################################################################################################################
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# missing.ls <- list(c(), c(), c()); names(missing.ls) <- landscapes; ls.ls <- missing.ls; rem.ls <- missing.ls; patch.ls <- missing.ls
# ds.ls.i <- list(tibble(), tibble(), tibble()); names(ds.ls.i) <- c("basal", "dom", "forest")
# ds.ls <- list(ds.ls.i, ds.ls.i, ds.ls.i); names(ds.ls) <- landscapes; rm(ds.ls.i)
landscape_i <- 1; i<-1; id<-1
for (landscape_i in 1:3) {
  landscapename <- landscapes[landscape_i]
  master <- read_delim(paste0("../project2ndStudy/dss/breakingTheSystem_", toupper(landscapename), "/cluster_master_", landscapename, ".csv"))
  #master <- read_delim(paste0("../project2ndStudy/cluster_master_", landscapename, ".csv"))
  
  # reference conditions for calculating differences in basal area / dominant species / regime shift, rep1-5
  ref.ids <- c(0:4)*512+1 # run ids of the baseline runs
  ds.ref <- data.frame()
  for (id in ref.ids) {
    db.conn <- dbConnect(RSQLite::SQLite(), paste0("raw_data/breakingTheSystem_", toupper(landscapename) ,"_Results/data",id,"/output.sqlite"))
    ds.ref_i <- tbl(db.conn, "dynamicstand") %>% 
      dplyr::select(-ru) %>% 
      collect() %>% 
      rename(basal_area_m2 = basalarea_sum, count_ha = if_height_0_1_0_sum) %>% 
      mutate(climate = master$climate[id], rep = master$rep[id], size = master$sizeMod[id], freq = master$freqMod[id], 
             browsing = master$browsingMod[id], fecundity = master$fecundityMod[1],
             identifier = paste0(master$climate[id], "_rep", master$rep[id], "_size", master$sizeMod[id], "_freq", master$freqMod[id], 
                                 "_browsing", master$browsingMod[id], "_fecundity", master$fecundityMod[id])) %>% 
      mutate(size = factor(size, levels = rev(c("10", "5", "2", "1"))),
             freq = factor(freq, levels = rev(c("10", "5", "2", "1"))),
             fecundity = factor(fecundity, levels = c("100", "50", "20", "10")),
             browsing = factor(browsing, levels = rev(c("10", "5", "2", "1"))),
             landscape = landscapename)
    dbDisconnect(db.conn); rm(db.conn)
    ds.ref <- bind_rows(ds.ref_i, ds.ref); rm(ds.ref_i)
  }
  
  rem <- data.frame(); ls <- data.frame(); ds.basal <- data.frame(); ds.dom <- data.frame(); ds.forest <- data.frame(); missing <- c(NA); patch <- data.frame()
  for (i in c(1:2560)) { # nrow(master)) {
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
        summarise(basal_c = sum(basal_area_m2)) %>% ungroup() %>% # basal area sum per RU in 2020, 2050, 2100
        inner_join(ds.ref %>%
                     group_by(rid, climate, size, freq, browsing, fecundity, landscape, year) %>%
                     summarise(basal_ref = sum(basal_area_m2)/5) %>% ungroup() %>% # basal area sum in 2020, 2050, 2100 under reference conditions
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
                     group_by(rid, rep, climate, size, freq, browsing, fecundity, identifier, landscape, year) %>%
                     filter(basal_area_m2 == max(basal_area_m2)) %>% # species with max basal area in 2020, 2050, 2100 under reference conditions
                     ungroup() %>% rename(dom_ref = species) %>%
                     dplyr::select(rid, dom_ref, year) %>% 
                     # chose most common dominant species by majority vote
                     group_by(rid, year, dom_ref) %>% 
                     mutate(n_species = n()) %>% ungroup() %>% 
                     group_by(rid, year) %>% 
                     filter(n_species == max(n_species)) %>%
                     dplyr::select(-n_species) %>% distinct() %>% 
                     # if rids have multiple equally popular dominant species: sample one randomly
                     mutate(n_rows=n(),
                            dom_ref = ifelse(n_rows > 1, sample(unique(dom_ref), 1), dom_ref)) %>% ungroup() %>%
                     dplyr::select(-n_rows) %>% distinct(), by = join_by(rid, year),
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
                     group_by(rid, climate, size, freq, browsing, fecundity, landscape, year) %>%
                     summarise(count_sum_ref = sum(count_ha)/5) %>% ungroup() %>% # tree count sum in 2020, 2050, 2100 under reference conditions
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
    #print(i)
  }
  # ds.ls[[landscapename]][["basal"]] <- ds.basal; ds.ls[[landscapename]][["dom"]] <- ds.dom; ds.ls[[landscapename]][["forest"]] <- ds.forest
  # missing.ls[[landscapename]] <- missing
  # ls.ls[[landscapename]] <- ls; rem.ls[[landscapename]] <- rem; patch.ls[[landscapename]] <- patch
  saveRDS(ls, paste0("results/datasets/ls_", landscapename,"_backup.RDATA")); saveRDS(rem, paste0("results/datasets/rem_", landscapename,"_backup.RDATA")); saveRDS(missing, paste0("results/datasets/missing_", landscapename,"_backup.RDATA"))
  saveRDS(patch, paste0("results/datasets/patch_", landscapename,"_backup.RDATA")); saveRDS(list(ds.basal, ds.dom, ds.forest), paste0("results/datasets/ds.ls_", landscapename,"_backup.RDATA"))
  rm(ls, rem, patch, ds.ref, ds.basal, ds.dom, ds.forest, master, dbname, missing)
  gc()
  
} #; saveRDS(ds.ls, "results/datasets/ds.ls.RDATA"); saveRDS(ls.ls, "results/datasets/ls.ls.RDATA"); saveRDS(patch.ls, "results/datasets/patch.ls.RDATA"); saveRDS(rem.ls, "results/datasets/rem.ls.RDATA"); saveRDS(missing.ls, "results/datasets/missing.ls.RDATA")

# note
## date: 
## server: anduin
## saving data to: Z:\ ge93ruw NAS
## landscape(s) running:
## landscape(s) finished and saved: bgd, grte, stoko (17.04, 9:00)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Sub-data frames of ds.ls ###########################################################################################################################################
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# empty lists
overtime.ls <- list(c(), c(), c()); names(overtime.ls) <- landscapes

landscape_i <- 3
for (landscape_i in 3:3) {
  landscapename <- landscapes[landscape_i]
  
  ds.ls <- readRDS(paste0("results/datasets/ds.ls_", landscapename, "_backup.RDATA")) # bgd: 26 GB
  names(ds.ls) <- c("basal", "dom", "forest")
  
  ## over time ####
  overtime.ls[[landscapename]] <- ds.ls[["basal"]] %>%
    group_by(threshold, climate, rep, size, freq, browsing, fecundity, identifier, landscape, year) %>%
    summarise(prop = mean(sum(n())/n_rid)) %>% # convert from count to proportion
    filter(threshold == "above") %>% # only keep proportion of unchanged landscape
    ungroup() %>% rename('1. Structure\nBasal area decreased by >50 % from reference' = prop) %>% dplyr::select(-threshold) %>%
    full_join(ds.ls[["dom"]] %>%
                group_by(threshold, climate, rep, size, freq, browsing, fecundity, identifier, landscape, year) %>%
                summarise(prop = mean(sum(n())/n_rid)) %>% # convert from count to proportion
                filter(threshold == "same") %>% # only keep proportion of unchanged landscape
                ungroup() %>% rename('2. Composition\nDominant species changed from reference' = prop) %>% dplyr::select(-threshold),
              by = join_by(climate, rep, size, freq, browsing, fecundity, identifier, landscape, year)) %>%
    full_join(ds.ls[["forest"]] %>% # summary()
                group_by(threshold, climate, rep, size, freq, browsing, fecundity, identifier, landscape, year) %>%
                summarise(prop = mean(sum(n())/n_rid)) %>% # convert from count to proportion
                filter(threshold == "forest") %>% # only keep proportion of unchanged landscape
                ungroup() %>% rename('3. Remaining forest\nStem density dropping below 50 trees/ha' = prop) %>% dplyr::select(-threshold),
              by = join_by(climate, rep, size, freq, browsing, fecundity, identifier, landscape, year))
  
  print(overtime.ls[[landscapename]] %>% #summary()
          filter(identifier %in% c("baseline_rep1_size1_freq1_browsing1_fecundity100",
                                   "hotdry_rep1_size1_freq1_browsing1_fecundity100")) )
  # test: props should always be very close to 1 for baseline and for year 0 hotdry, i.e., rows 1-4 cols 10-12
  
  if (landscapename == "stoko") overtime.ls[["stoko"]]<-overtime.ls[["stoko"]][-709,] # fix malformed factor
  if (landscapename == "grte") overtime.ls[["grte"]]<-overtime.ls[["grte"]][-7681,]
  
  # map.df ####
  
  # fix malformed factor in shiretoko
  if (landscapename == "stoko") ds.ls[["basal"]][ds.ls[["basal"]]$rid==35001 & ds.ls[["basal"]]$climate=="baseline" & ds.ls[["basal"]]$rep==1 & ds.ls[["basal"]]$size==10 & ds.ls[["basal"]]$freq==5 & ds.ls[["basal"]]$browsing==5 & ds.ls[["basal"]]$year==80 & ds.ls[["basal"]]$threshold=="above",][2,"fecundity"] <- factor("50", levels=c(100,50,20,10))
  
  a<-ds.ls[["basal"]] %>% 
    #sample_frac(0.001) %>% 
    filter(year==80, threshold=="above", rid != 0) %>% 
    dplyr::select(-basal_c, -basal_ref, -basal_diff, -year) %>% 
    full_join(rid.df[rid.df$landscape==landscapename,] %>%
                expand_grid(size = c("10", "5", "2", "1"), freq = c("10", "5", "2", "1"),
                            fecundity = c("100", "50", "20", "10"), browsing = c("10", "5", "2", "1"),
                            rep = 1:5, climate = c("baseline", "hotdry")),
              by=c("rid", "climate", "rep", "size", "freq", "browsing", "fecundity", "landscape")) %>%
    mutate(structure_change = ifelse(is.na(threshold), "yes", "no"),
           structure_change = as.factor(structure_change),
           identifier = paste0(climate, "_rep", rep, "_size", size, "_freq", freq, 
                               "_browsing", browsing, "_fecundity", fecundity)) %>% 
    dplyr::select(-threshold) %>% 
    group_by(size, freq, browsing, fecundity, rep, climate, identifier, landscape) %>% 
    mutate(n_rid = n()) %>% ungroup(); gc()
  print("finished a")
  
  b<-ds.ls[["dom"]] %>% 
    filter(year==80, threshold=="same", rid != 0) %>% 
    dplyr::select(-dom_ref, -dom_c, -year) %>% 
    full_join(rid.df[rid.df$landscape==landscapename,] %>% 
                expand_grid(size = c("10", "5", "2", "1"), freq = c("10", "5", "2", "1"),
                            fecundity = c("100", "50", "20", "10"), browsing = c("10", "5", "2", "1"),
                            rep = 1:5, climate = c("baseline", "hotdry")),
              by=c("rid", "climate", "rep", "size", "freq", "browsing", "fecundity", "landscape")) %>%
    mutate(composition_change = ifelse(is.na(threshold), "yes", "no"),
           composition_change = as.factor(composition_change),
           identifier = paste0(climate, "_rep", rep, "_size", size, "_freq", freq, 
                               "_browsing", browsing, "_fecundity", fecundity)) %>% 
    dplyr::select(-threshold) %>% 
    group_by(size, freq, browsing, fecundity, rep, climate, identifier, landscape) %>% 
    mutate(n_rid = n()) %>% ungroup(); gc()
  print("finished b")
  
  if (landscapename == "stoko") a$n_rid <- 39999; b$n_rid <- 39999
  interim <- full_join(a, b, by=c("rid", "climate", "rep", "size", "freq", "browsing", "fecundity", "landscape", "x", "y", "n_rid", "identifier")) 
  rm(a,b); gc()
  
  c<-ds.ls[["forest"]] %>% 
    filter(year==80, threshold=="forest", rid != 0) %>% 
    dplyr::select(-count_sum_ref, -count_sum_c, -year) %>% 
    full_join(rid.df[rid.df$landscape==landscapename,] %>% 
                expand_grid(size = c("10", "5", "2", "1"), freq = c("10", "5", "2", "1"),
                            fecundity = c("100", "50", "20", "10"), browsing = c("10", "5", "2", "1"),
                            rep = 1:5, climate = c("baseline", "hotdry")),
              by=c("rid", "climate", "rep", "size", "freq", "browsing", "fecundity", "landscape")) %>%
    mutate(regime_shift = ifelse(is.na(threshold), "yes", "no"),
           regime_shift = as.factor(regime_shift),
           identifier = paste0(climate, "_rep", rep, "_size", size, "_freq", freq, 
                               "_browsing", browsing, "_fecundity", fecundity)) %>% 
    dplyr::select(-threshold) %>% 
    group_by(size, freq, browsing, fecundity, rep, climate, identifier, landscape) %>% 
    mutate(n_rid = n()) %>% ungroup(); rm(ds.ls); gc()
  if (landscapename == "stoko") c$n_rid <- 39999
  print("finished c")
  
  map.df <- interim %>% 
    full_join(c, by=c("rid", "climate", "rep", "size", "freq", "browsing", "fecundity", "landscape", "x", "y", "n_rid", "identifier"))
  rm(interim, c); gc()
  print("finished map.df, now only saveRDS unfinished")
  
  saveRDS(map.df, paste0("results/datasets/map.df_", landscapename, ".RDATA"))
  rm(map.df); gc()
  
  
}; saveRDS(overtime.ls, "results/datasets/overTime.ls.RDATA"); rm(overtime.ls)

