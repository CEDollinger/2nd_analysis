# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Reading in data ###########################################################################################################################################
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

landscape_i <- 1; i<-1; id<-1
for (landscape_i in c(1:3)) {
  landscapename <- landscapes[landscape_i]
  master <- read_delim(paste0("../project2ndStudy/dss/breakingTheSystem_", toupper(landscapename), "/cluster_master_", landscapename, ".csv"))
  
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
             browsing = master$browsingMod[id], fecundity = master$fecundityMod[id],
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
  
  rem <- data.frame(); ls <- data.frame(); missing <- c(NA); 
  patch <- data.frame(); regen <- data.frame()
  ds.basal <- data.frame(); ds.dom <- data.frame(); ds.forest <- data.frame()
  for (i in 1:2560) {
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
      # removed output ####
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
      
      # each run compared to "baseline_rep1_size1_freq1_browsing1_fecundity100" for its own rep (also for hot-dry runs)
      # prop of basal area decreased by more than 50 %
      ds.basal.i <- ds.i %>%
        group_by(rid, climate, rep, size, freq, browsing, fecundity, identifier, landscape, year) %>%
        summarise(basal_c = sum(basal_area_m2)) %>% ungroup() %>% # basal area sum per RU in 2020, 2050, 2100
        inner_join(ds.ref %>%  
                     filter(rep == master$rep[i]) %>% 
                     group_by(rid, year) %>% 
                     summarise(basal_ref = sum(basal_area_m2)) %>% # basal area sum in 2020, 2050, 2100 under reference conditions
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
        mutate(rel_count=count_ha/sum(count_ha), rel_basal=basal_area_m2/sum(basal_area_m2),
               iv = rel_count+rel_basal) %>%
        filter(iv == max(iv)) %>% ungroup() %>% # species with max IV in 2020, 2050, 2100
        rename(dom_c = species) %>%
        dplyr::select(rid, climate, rep, size, freq, browsing, fecundity, identifier, landscape, dom_c, year) %>%
        inner_join(ds.ref %>%
                     filter(rep == master$rep[i]) %>% 
                     group_by(rid, climate, rep, size, freq, browsing, fecundity, identifier, landscape, year) %>%
                     mutate(rel_count=count_ha/sum(count_ha), rel_basal=basal_area_m2/sum(basal_area_m2),
                            iv = rel_count+rel_basal) %>%
                     filter(iv == max(iv)) %>% ungroup() %>% # species with max IV in 2020, 2050, 2100
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
                     filter(rep == master$rep[i]) %>% 
                     group_by(rid, year) %>% 
                     summarise(count_sum_ref = sum(count_ha)) %>% # tree count in 2020, 2050, 2100 under reference conditions
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
               browsing=master$browsingMod[i], fecundity=master$fecundityMod[i]) 
      
      # regeneration rate ####
      regen.i <- ls.i %>% 
        group_by(landscape, year, identifier, size, freq, climate, rep, browsing, fecundity) %>% 
        summarise(count.sum = sum(count_ha)) %>% ungroup() %>% 
        mutate(count.previous = lag(count.sum),
               zuwachs = count.sum - count.previous) %>% ungroup() %>% 
        filter(year > 0) %>% 
        full_join(rem.i %>% 
                    filter(year > 0) %>% 
                    mutate(count_ha = count/areas[landscape_i, "area"]) %>% 
                    group_by(landscape, year) %>% # sum up over species + causes of death
                    summarise(died = sum(count_ha)) %>% ungroup(), by = c("year", "landscape")) %>% 
        mutate(recruited_mean = zuwachs + died) 
      
      # combine all scenarios for the different outputs
      rem <- bind_rows(rem, rem.i); ls <- bind_rows(ls, ls.i); patch <- bind_rows(patch, patch.i); regen <- bind_rows(regen, regen.i)
      ds.basal <- bind_rows(ds.basal, ds.basal.i); ds.dom <- bind_rows(ds.dom, ds.dom.i); ds.forest <- bind_rows(ds.forest, ds.forest.i)
      rm(db.conn, rem.i, ls.i, patch.i, regen.i, ds.i, ds.basal.i, ds.dom.i, ds.forest.i)
    } else { 
      missing <- c(missing, i); print(paste0("Oh no! Seems like I (", i, ") am missing")) 
    }
    if (i%%64==0) print(paste0(round((i/nrow(master)*100),1), " %: rep ", master$rep[i], ", ", i, "; ", format(Sys.time(), "%a %b %d %X")))
    #print(i)
  }
  
  saveRDS(ls, paste0("results/datasets/ls_", landscapename,"_backup.RDATA")); saveRDS(rem, paste0("results/datasets/rem_", landscapename,"_backup.RDATA"))
  saveRDS(missing, paste0("results/datasets/missing_", landscapename,"_backup.RDATA"))
  saveRDS(list(ds.basal, ds.dom, ds.forest), paste0("results/datasets/ds.ls_", landscapename,"_backup.RDATA"))
  saveRDS(patch, paste0("results/datasets/patch_", landscapename,"_backup.RDATA")); saveRDS(regen, paste0("results/datasets/regen_", landscapename,"_backup.RDATA"))
  rm(ls, rem, patch, regen, ds.ref, ds.basal, ds.dom, ds.forest, master, dbname, missing)
  gc()
  
}


# note
## date: 21.09.2024, 19:00
## server: Lorien (bgd + grte), Anduin (stoko) 
## saving data to: E:\ 
## landscape(s) running: on Lorien bgd + grte, on Anduin stoko
## landscape(s) finished and saved: bgd (22.09, 16:00), stoko (23.09, 07:00)
## misc: use only own rep as reference for comparison instead of mean over all 5
## note: i backed up the previous datasets on my private laptop (19.09.2024, 16:00)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Sub-data frames of ds.ls ###########################################################################################################################################
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# empty lists
overtime.ls <- list(c(), c(), c()); names(overtime.ls) <- landscapes

# overtime.ls <- readRDS("results/datasets/overTime.ls.RDATA")

landscape_i <- 1
for (landscape_i in 1:3) {
  landscapename <- landscapes[landscape_i]
  
  ds.ls <- readRDS(paste0("results/datasets/ds.ls_", landscapename, "_backup.RDATA")) # bgd: 26 GB
  names(ds.ls) <- c("basal", "dom", "forest")
  
  ## over time ####
  overtime.ls[[landscapename]] <- ds.ls[["basal"]] %>%
    group_by(threshold, climate, rep, size, freq, browsing, fecundity, identifier, landscape, year) %>%
    summarise(
      mean_basal_c = mean(basal_c), median_basal_c = median(basal_c), sd_basal_c = sd(basal_c), # landscape-wide mean basal area per ha
      mean_basal_ref = mean(basal_ref), median_basal_ref = median(basal_ref), sd_basal_ref = sd(basal_ref),
      prop = mean(sum(n())/n_rid)) %>% # convert from count to proportion
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
  # test: props should always be 1 for year 0 (baseline + hotdry)
  
  
  if (landscapename == "stoko") overtime.ls[["stoko"]]<-overtime.ls[["stoko"]][-709,] # fix malformed factor
  if (landscapename == "grte") overtime.ls[["grte"]]<-overtime.ls[["grte"]][c(-7681, -7682),]
  
  rm(ds.ls)
  
  # map.df ####
  
  # # fix malformed factor in shiretoko
  # if (landscapename == "stoko") ds.ls[["basal"]][ds.ls[["basal"]]$rid==35001 & ds.ls[["basal"]]$climate=="baseline" & ds.ls[["basal"]]$rep==1 & ds.ls[["basal"]]$size==10 & ds.ls[["basal"]]$freq==5 & ds.ls[["basal"]]$browsing==5 & ds.ls[["basal"]]$year==80 & ds.ls[["basal"]]$threshold=="above",][2,"fecundity"] <- factor("50", levels=c(100,50,20,10))
  # 
  # # # endpoint maps with actual basal area, species etc. for a few scenarios (n=8)
  # # ds.ls[["basal"]] %>% 
  # #   filter(year == 80,
  # #          identifier %in% c(paste0("baseline_rep",c(1:5),"_size1_freq1_browsing1_fecundity100"),
  # #                            paste0("hotdry_rep",c(1:5),"_size1_freq1_browsing1_fecundity100"),
  # #                            
  # #                            paste0("baseline_rep",c(1:5),"_size10_freq10_browsing1_fecundity100"),
  # #                            paste0("hotdry_rep",c(1:5),"_size10_freq10_browsing1_fecundity100"),
  # #                            
  # #                            paste0("baseline_rep",c(1:5),"_size1_freq1_browsing10_fecundity10"),
  # #                            paste0("hotdry_rep",c(1:5),"_size1_freq1_browsing10_fecundity10"),
  # #                            
  # #                            paste0("baseline_rep",c(1:5),"_size10_freq10_browsing10_fecundity10"),
  # #                            paste0("hotdry_rep",c(1:5),"_size10_freq10_browsing10_fecundity10"))) %>% 
  # #   dplyr::select(-threshold, -n_rid) %>% 
  # #   full_join(ds.ls[["dom"]] %>% 
  # #               filter(year == 80,
  # #                      identifier %in% c(paste0("baseline_rep",c(1:5),"_size1_freq1_browsing1_fecundity100"),
  # #                                        paste0("hotdry_rep",c(1:5),"_size1_freq1_browsing1_fecundity100"),
  # #                                        
  # #                                        paste0("baseline_rep",c(1:5),"_size10_freq10_browsing1_fecundity100"),
  # #                                        paste0("hotdry_rep",c(1:5),"_size10_freq10_browsing1_fecundity100"),
  # #                                        
  # #                                        paste0("baseline_rep",c(1:5),"_size1_freq1_browsing10_fecundity10"),
  # #                                        paste0("hotdry_rep",c(1:5),"_size1_freq1_browsing10_fecundity10"),
  # #                                        
  # #                                        paste0("baseline_rep",c(1:5),"_size10_freq10_browsing10_fecundity10"),
  # #                                        paste0("hotdry_rep",c(1:5),"_size10_freq10_browsing10_fecundity10"))) %>% 
  # #               dplyr::select(-threshold, -n_rid)) %>% 
  # #   full_join(ds.ls[["forest"]] %>% 
  # #               filter(year == 80,
  # #                      identifier %in% c(paste0("baseline_rep",c(1:5),"_size1_freq1_browsing1_fecundity100"),
  # #                                        paste0("hotdry_rep",c(1:5),"_size1_freq1_browsing1_fecundity100"),
  # #                                        
  # #                                        paste0("baseline_rep",c(1:5),"_size10_freq10_browsing1_fecundity100"),
  # #                                        paste0("hotdry_rep",c(1:5),"_size10_freq10_browsing1_fecundity100"),
  # #                                        
  # #                                        paste0("baseline_rep",c(1:5),"_size1_freq1_browsing10_fecundity10"),
  # #                                        paste0("hotdry_rep",c(1:5),"_size1_freq1_browsing10_fecundity10"),
  # #                                        
  # #                                        paste0("baseline_rep",c(1:5),"_size10_freq10_browsing10_fecundity10"),
  # #                                        paste0("hotdry_rep",c(1:5),"_size10_freq10_browsing10_fecundity10"))) %>% 
  # #               dplyr::select(-threshold, -n_rid)) %>% 
  # #   saveRDS(paste0("results/datasets/map_endpoints_",landscapename,".RDATA"))
  # 
  # # maps with % landscape unchanged
  # a<-ds.ls[["basal"]] %>% 
  #   # sample_frac(0.001) %>% 
  #   filter(year==80, rid != 0) %>% 
  #   dplyr::select(-year) %>% #-basal_c, -basal_ref, -basal_diff, 
  #   full_join(rid.df[rid.df$landscape==landscapename,]) %>% 
  #   mutate(threshold = ifelse(threshold == "above", "no", "yes")) %>% 
  #   rename('1. Structure\nBasal area decreased by >50 % from reference' = threshold) %>% 
  #   group_by(size, freq, browsing, fecundity, rep, climate, identifier, landscape) %>% 
  #   filter(!is.na(rep)) %>% 
  #   mutate(n_rid = n()) %>% ungroup(); gc()
  # print("finished a")
  # 
  # b<-ds.ls[["dom"]] %>% 
  #   # sample_frac(0.001) %>% 
  #   filter(year==80, rid != 0) %>% 
  #   dplyr::select(-year) %>% #-dom_ref, -dom_c, 
  #   full_join(rid.df[rid.df$landscape==landscapename,]) %>% 
  #   mutate(threshold = ifelse(threshold == "same", "no", "yes")) %>% 
  #   rename('2. Composition\nDominant species changed from reference' = threshold) %>% 
  #   group_by(size, freq, browsing, fecundity, rep, climate, identifier, landscape) %>% 
  #   filter(!is.na(rep)) %>% 
  #   mutate(n_rid = n()) %>% ungroup(); gc()
  # print("finished b")
  # 
  # if (landscapename == "stoko") {
  #   a$n_rid <- 39999; b$n_rid <- 39999
  # }
  # interim <- full_join(a, b, by=c("rid", "climate", "rep", "size", "freq", "browsing", "fecundity", "landscape", "x", "y", "n_rid", "identifier")) 
  # rm(a,b); gc()
  # 
  # c<-ds.ls[["forest"]] %>% 
  #   filter(year==80, rid != 0) %>% 
  #   dplyr::select(-year) %>% #-count_sum_ref, -count_sum_c, 
  #   full_join(rid.df[rid.df$landscape==landscapename,]) %>% 
  #   mutate(threshold = ifelse(threshold == "forest", "no", "yes")) %>% 
  #   rename('3. Remaining forest\nStem density dropping below 50 trees/ha' = threshold) %>% 
  #   group_by(size, freq, browsing, fecundity, rep, climate, identifier, landscape) %>% 
  #   filter(!is.na(rep)) %>% 
  #   mutate(n_rid = n()) %>% ungroup(); rm(ds.ls); gc()
  # 
  # if (landscapename == "stoko") c$n_rid <- 39999
  # print("finished c")
  # 
  # map.df <- interim %>% 
  #   full_join(c, by=c("rid", "climate", "rep", "size", "freq", "browsing", "fecundity", "landscape", "x", "y", "n_rid", "identifier"))
  # rm(interim, c); gc()
  # print("finished map.df, now only saveRDS unfinished")
  # 
  # saveRDS(map.df, paste0("results/datasets/map.df_", landscapename, "_extended.RDATA"))
  # rm(map.df)
  # 
  print("finished a landscape"); gc()
  
  
}; saveRDS(overtime.ls, "results/datasets/overTime.ls.RDATA"); rm(overtime.ls)

# Patch sizes ####
### for stoko + bgd ####

rep.i <- 1
patchlist <- data.frame()
for (rep.i in 1:5) {
  mgmt.full.prelim <- read_csv(paste0("../prep/data/bgd_rep", rep.i, "_level16.csv")) %>% 
    group_by(unique, mod.size, mod.freq, mgmt.rep, agent) %>% mutate(sum=sum(n())) %>% ungroup() %>% filter(sum>4) %>% # filter out patches smaller than 0.5 ha
    dplyr::select(unique, sum, year, mod.size, mod.freq, landscape) %>% 
    distinct() %>% 
    mutate(rep = rep.i) %>% 
    rename(n_cells_soll = sum, size = mod.size, freq=mod.freq, patchID=unique) 
  patchlist <- bind_rows(patchlist, mgmt.full.prelim); rm(mgmt.full.prelim)
  print(rep.i)
}
patchlist %>% summary(); nrow(patchlist); nrow(patchlist %>% ungroup() %>% distinct())
saveRDS(patchlist %>% distinct(), "results/datasets/patchlist_bgd.RDATA")

### for grte ####
landscapename <- "grte"
i<-1; rep<-1
patchlist <- data.frame()
for (rep in c(1:5)) { 
  rep_i <- rep
  level <- data.frame()
  for (i in c(1:16)) { 
    mgmt.full <- bind_rows(read_csv(paste0("../prep/data/grte_rep", rep_i, "_level", i,"_fire.csv")), 
                           read_csv(paste0("../prep/data/grte_rep", rep_i, "_level", i,"_bite.csv"))) %>%
      group_by(unique, mod.size, mod.freq, mgmt.rep, agent) %>% mutate(sum=sum(n())) %>% ungroup() %>% filter(sum>4) %>% # filter out patches smaller than 0.5 ha
      dplyr::select(unique, sum, year, mod.size, mod.freq, landscape) %>% 
      mutate(rep=rep_i) %>% 
      distinct() %>% 
      rename(n_cells_soll = sum, size = mod.size, freq=mod.freq, patchID=unique)
    level<-bind_rows(level, mgmt.full); rm(mgmt.full)
    print(i)
  }
  saveRDS(level, paste0("results/datasets/grte_patchlist_rep", rep_i, ".RDATA"))
  patchlist <- bind_rows(patchlist, level); rm(level)
} 
patchlist %>% summary(); nrow(patchlist); nrow(patchlist %>% distinct())
saveRDS(patchlist %>% distinct(), "results/datasets/patchlist_grte.RDATA")



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# DISCARDED ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# # fix run 49 in GRTE: baseline_rep1_size1_freq10_browsing1_fecundity100
# ls <- readRDS(paste0("results/datasets/ls_", "grte","_backup.RDATA")) %>% 
#   filter(identifier != "baseline_rep1_size1_freq10_browsing1_fecundity100")
# rem <- readRDS(paste0("results/datasets/rem_", "grte","_backup.RDATA")) %>% 
#   filter(identifier != "baseline_rep1_size1_freq10_browsing1_fecundity100")
# ds.ls <- readRDS(paste0("results/datasets/ds.ls_grte_backup.RDATA"))
# ds.basal <- ds.ls[[1]]%>% 
#   filter(identifier != "baseline_rep1_size1_freq10_browsing1_fecundity100")
# ds.dom <- ds.ls[[2]]%>% 
#   filter(identifier != "baseline_rep1_size1_freq10_browsing1_fecundity100")
# ds.forest <- ds.ls[[3]]%>% 
#   filter(identifier != "baseline_rep1_size1_freq10_browsing1_fecundity100")
# rm(ds.ls); gc()

# # reference always mean over all 5 reps of "baseline_rep1_size1_freq1_browsing1_fecundity100" (also for hot-dry runs)
# # prop of basal area decreased by more than 50 %
# ds.basal.i <- ds.i %>%
#   group_by(rid, climate, rep, size, freq, browsing, fecundity, identifier, landscape, year) %>%
#   summarise(basal_c = sum(basal_area_m2)) %>% ungroup() %>% # basal area sum per RU in 2020, 2050, 2100
#   inner_join(ds.ref %>%
#                group_by(rid, climate, size, freq, browsing, fecundity, landscape, year) %>%
#                summarise(basal_ref = sum(basal_area_m2)/5) %>% ungroup() %>% # basal area sum in 2020, 2050, 2100 under reference conditions
#                dplyr::select(rid, basal_ref, year), by = join_by(rid, year),
#              multiple = "all") %>%
#   filter(!is.na(basal_ref)) %>% # filter out RUs for which no reference conditions exist
#   group_by(climate, rep, size, freq, browsing, fecundity, identifier, landscape, year) %>%
#   mutate(basal_diff = (basal_c-basal_ref)/basal_ref, # relative change in basal area sum
#          threshold = ifelse(basal_diff < -0.5, "below", "above"), # count RUs where basal area dropped by more than 50% from reference; unidirectional: only if DECREASE > 50%
#          n_rid = length(unique(rid))) %>% ungroup()
# 
# # prop of dominant species changed
# ds.dom.i <- ds.i %>%
#   group_by(rid, climate, rep, size, freq, browsing, fecundity, identifier, landscape, year) %>%
#   mutate(rel_count=count_ha/sum(count_ha), rel_basal=basal_area_m2/sum(basal_area_m2),
#          iv = rel_count+rel_basal) %>%
#   filter(iv == max(iv)) %>% ungroup() %>% # species with max IV in 2020, 2050, 2100
#   rename(dom_c = species) %>%
#   dplyr::select(rid, climate, rep, size, freq, browsing, fecundity, identifier, landscape, dom_c, year) %>%
#   inner_join(ds.ref %>%
#                group_by(rid, rep, climate, size, freq, browsing, fecundity, identifier, landscape, year) %>%
#                mutate(rel_count=count_ha/sum(count_ha), rel_basal=basal_area_m2/sum(basal_area_m2),
#                       iv = rel_count+rel_basal) %>%
#                filter(iv == max(iv)) %>% # species with max IV in 2020, 2050, 2100 under reference conditions
#                ungroup() %>% rename(dom_ref = species) %>%
#                dplyr::select(rid, dom_ref, year) %>%
#                # chose most common dominant species by majority vote
#                group_by(rid, year, dom_ref) %>%
#                mutate(n_species = n()) %>% ungroup() %>%
#                group_by(rid, year) %>%
#                filter(n_species == max(n_species)) %>%
#                dplyr::select(-n_species) %>% distinct() %>%
#                # if rids have multiple equally popular dominant species: sample one randomly
#                mutate(n_rows=n(),
#                       dom_ref = ifelse(n_rows > 1, sample(unique(dom_ref), 1), dom_ref)) %>% ungroup() %>%
#                dplyr::select(-n_rows) %>% distinct(), by = join_by(rid, year),
#              multiple = "all") %>% #summary()
#   filter(!is.na(dom_ref)) %>% # filter out RUs for which no reference conditions exist
#   group_by(climate, rep, size, freq, browsing, fecundity, identifier, landscape, year) %>%
#   mutate(threshold = ifelse(dom_c == dom_ref, "same", "different"), # count RUs where dominant species changed
#          n_rid = length(unique(rid))) %>% ungroup()
# 
# # prop of trees count > 50
# ds.forest.i <- ds.i %>%
#   group_by(rid, climate, rep, size, freq, browsing, fecundity, identifier, landscape, year) %>%
#   summarise(count_sum_c = sum(count_ha)) %>% ungroup() %>% # tree count sum in 2020, 2050, 2100
#   inner_join(ds.ref %>%
#                group_by(rid, climate, size, freq, browsing, fecundity, landscape, year) %>%
#                summarise(count_sum_ref = sum(count_ha)/5) %>% ungroup() %>% # tree count sum in 2020, 2050, 2100 under reference conditions
#                dplyr::select(rid, count_sum_ref, year), by = join_by(rid, year),
#              multiple = "all") %>%
#   filter(!is.na(count_sum_ref)) %>% # filter out RUs for which no reference conditions exist
#   group_by(climate, rep, size, freq, browsing, fecundity, identifier, landscape, year) %>%
#   mutate(threshold = as.factor(ifelse(count_sum_c < 50 & count_sum_ref >= 50, "non-forest", "forest")), # count RUs where tree count dropped below 50 (and reference value is also higher than 50)
#          n_rid = length(unique(rid))) %>% ungroup()

