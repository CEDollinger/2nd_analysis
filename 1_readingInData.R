# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Reading in data ###########################################################################################################################################
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

## 1. Simulation in- and outputs to intermediate data sets ####

landscape_i <- 1
i <- 1
id <- 1
for (landscape_i in c(1:3)) {
  landscapename <- landscapes[landscape_i]
  master <- read_delim(paste0("../project2ndStudy/dss/breakingTheSystem_", toupper(landscapename), "/cluster_master_", landscapename, ".csv"))

  # reference conditions for calculating differences in basal area / dominant species / regime shift, rep1-5
  ref.ids <- c(0:4) * 512 + 1 # run ids of the baseline runs
  ds.ref <- data.frame()
  for (id in ref.ids) {
    db.conn <- dbConnect(RSQLite::SQLite(), paste0("raw_data/breakingTheSystem_", toupper(landscapename), "_Results/data", id, "/output.sqlite"))
    ds.ref_i <- tbl(db.conn, "dynamicstand") %>%
      dplyr::select(-ru) %>%
      collect() %>%
      rename(basal_area_m2 = basalarea_sum, count_ha = if_height_0_1_0_sum) %>%
      mutate(
        climate = master$climate[id], rep = master$rep[id], size = master$sizeMod[id], freq = master$freqMod[id],
        browsing = master$browsingMod[id], fecundity = master$fecundityMod[id],
        identifier = paste0(
          master$climate[id], "_rep", master$rep[id], "_size", master$sizeMod[id], "_freq", master$freqMod[id],
          "_browsing", master$browsingMod[id], "_fecundity", master$fecundityMod[id]
        )
      ) %>%
      mutate(
        size = factor(size, levels = rev(c("10", "5", "2", "1"))),
        freq = factor(freq, levels = rev(c("10", "5", "2", "1"))),
        fecundity = factor(fecundity, levels = c("100", "50", "20", "10")),
        browsing = factor(browsing, levels = rev(c("10", "5", "2", "1"))),
        landscape = landscapename
      )
    dbDisconnect(db.conn)
    rm(db.conn)
    ds.ref <- bind_rows(ds.ref_i, ds.ref)
    rm(ds.ref_i)
  }

  rem <- data.frame()
  ls <- data.frame()
  missing <- c(NA)
  patch <- data.frame()
  regen <- data.frame()
  ds.basal <- data.frame()
  ds.dom <- data.frame()
  ds.forest <- data.frame()
  for (i in 1:2560) {
    dbname <- paste0("raw_data/breakingTheSystem_", toupper(landscapename), "_Results/data", master$run_id[i], "/output.sqlite")
    patch_file <- paste0("raw_data/breakingTheSystem_", toupper(landscapename), "_Results/data", master$run_id[i], "/patches.csv")
    if (file.exists(dbname) & file.exists(patch_file)) {
      db.conn <- dbConnect(RSQLite::SQLite(), dbname = dbname) # output_baseline_rep1_size1_freq1_browsing1_fecundity75

      # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
      ### landscape output ####
      # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
      ls.i <- tbl(db.conn, "landscape") %>%
        dplyr::select(year, species, count_ha, basal_area_m2, gwl_m3, volume_m3, height_avg_m, cohort_count_ha) %>%
        collect() %>%
        mutate(
          climate = master$climate[i], rep = master$rep[i], size = master$sizeMod[i], freq = master$freqMod[i],
          browsing = master$browsingMod[i], fecundity = master$fecundityMod[i],
          identifier = paste0(
            master$climate[i], "_rep", master$rep[i], "_size", master$sizeMod[i], "_freq", master$freqMod[i],
            "_browsing", master$browsingMod[i], "_fecundity", master$fecundityMod[i]
          )
        ) %>%
        mutate(
          size = factor(size, levels = rev(c("10", "5", "2", "1"))),
          freq = factor(freq, levels = rev(c("10", "5", "2", "1"))),
          fecundity = factor(fecundity, levels = c("100", "50", "20", "10")),
          browsing = factor(browsing, levels = rev(c("10", "5", "2", "1"))),
          landscape = landscapename
        )

      # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
      ## removed output ####
      # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
      rem.i <- tbl(db.conn, "landscape_removed") %>%
        dplyr::select(year, species, count, basal_area_m2, volume_m3, dbh_class, reason) %>%
        collect() %>%
        mutate(
          climate = master$climate[i], rep = master$rep[i], size = master$sizeMod[i], freq = master$freqMod[i],
          browsing = master$browsingMod[i], fecundity = master$fecundityMod[i],
          identifier = paste0(
            master$climate[i], "_rep", master$rep[i], "_size", master$sizeMod[i], "_freq", master$freqMod[i],
            "_browsing", master$browsingMod[i], "_fecundity", master$fecundityMod[i]
          )
        ) %>%
        mutate(
          size = factor(size, levels = rev(c("10", "5", "2", "1"))),
          freq = factor(freq, levels = rev(c("10", "5", "2", "1"))),
          fecundity = factor(fecundity, levels = c("100", "50", "20", "10")),
          browsing = factor(browsing, levels = rev(c("10", "5", "2", "1"))),
          landscape = landscapename
        )

      # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
      ## dynamicstand output ####
      # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
      ds.i <- tbl(db.conn, "dynamicstand") %>%
        dplyr::select(-ru) %>%
        collect() %>%
        rename(basal_area_m2 = basalarea_sum, count_ha = if_height_0_1_0_sum) %>%
        mutate(
          climate = master$climate[i], rep = master$rep[i], size = master$sizeMod[i], freq = master$freqMod[i],
          browsing = master$browsingMod[i], fecundity = master$fecundityMod[i],
          identifier = paste0(
            master$climate[i], "_rep", master$rep[i], "_size", master$sizeMod[i], "_freq", master$freqMod[i],
            "_browsing", master$browsingMod[i], "_fecundity", master$fecundityMod[i]
          )
        ) %>%
        mutate(
          size = factor(size, levels = rev(c("10", "5", "2", "1"))),
          freq = factor(freq, levels = rev(c("10", "5", "2", "1"))),
          fecundity = factor(fecundity, levels = c("100", "50", "20", "10")),
          browsing = factor(browsing, levels = rev(c("10", "5", "2", "1"))),
          landscape = landscapename
        )

      # each run compared to "baseline_rep1_size1_freq1_browsing1_fecundity100" for its own rep (also for hot-dry runs)
      # prop of basal area decreased by more than 50 %
      ds.basal.i <- ds.i %>%
        group_by(rid, climate, rep, size, freq, browsing, fecundity, identifier, landscape, year) %>%
        summarise(basal_c = sum(basal_area_m2)) %>%
        ungroup() %>% # basal area sum per RU in 2020, 2050, 2100
        inner_join(
          ds.ref %>%
            filter(rep == master$rep[i]) %>%
            group_by(rid, year) %>%
            summarise(basal_ref = sum(basal_area_m2)) %>% # basal area sum in 2020, 2050, 2100 under reference conditions
            dplyr::select(rid, basal_ref, year),
          by = join_by(rid, year),
          multiple = "all"
        ) %>%
        filter(!is.na(basal_ref)) %>% # filter out RUs for which no reference conditions exist
        group_by(climate, rep, size, freq, browsing, fecundity, identifier, landscape, year) %>%
        mutate(
          basal_diff = (basal_c - basal_ref) / basal_ref, # relative change in basal area sum
          threshold = ifelse(basal_diff < -0.5, "below", "above"), # count RUs where basal area dropped by more than 50% from reference; unidirectional: only if DECREASE > 50%
          n_rid = length(unique(rid))
        ) %>%
        ungroup()

      # prop of dominant species changed
      ds.dom.i <- ds.i %>%
        group_by(rid, climate, rep, size, freq, browsing, fecundity, identifier, landscape, year) %>%
        mutate(
          rel_count = count_ha / sum(count_ha), rel_basal = basal_area_m2 / sum(basal_area_m2),
          iv = rel_count + rel_basal
        ) %>%
        filter(iv == max(iv)) %>%
        ungroup() %>% # species with max IV in 2020, 2050, 2100
        rename(dom_c = species) %>%
        dplyr::select(rid, climate, rep, size, freq, browsing, fecundity, identifier, landscape, dom_c, year) %>%
        inner_join(
          ds.ref %>%
            filter(rep == master$rep[i]) %>%
            group_by(rid, climate, rep, size, freq, browsing, fecundity, identifier, landscape, year) %>%
            mutate(
              rel_count = count_ha / sum(count_ha), rel_basal = basal_area_m2 / sum(basal_area_m2),
              iv = rel_count + rel_basal
            ) %>%
            filter(iv == max(iv)) %>% ungroup() %>% # species with max IV in 2020, 2050, 2100
            rename(dom_ref = species) %>%
            dplyr::select(rid, dom_ref, year),
          by = join_by(rid, year),
          multiple = "all"
        ) %>% # summary()
        filter(!is.na(dom_ref)) %>% # filter out RUs for which no reference conditions exist
        group_by(climate, rep, size, freq, browsing, fecundity, identifier, landscape, year) %>%
        mutate(
          threshold = ifelse(dom_c == dom_ref, "same", "different"), # count RUs where dominant species changed
          n_rid = length(unique(rid))
        ) %>%
        ungroup()


      # prop of trees count <50
      ds.forest.i <- ds.i %>%
        group_by(rid, climate, rep, size, freq, browsing, fecundity, identifier, landscape, year) %>%
        summarise(count_sum_c = sum(count_ha)) %>%
        ungroup() %>% # tree count sum in 2020, 2050, 2100
        inner_join(
          ds.ref %>%
            filter(rep == master$rep[i]) %>%
            group_by(rid, year) %>%
            summarise(count_sum_ref = sum(count_ha)) %>% # tree count in 2020, 2050, 2100 under reference conditions
            dplyr::select(rid, count_sum_ref, year),
          by = join_by(rid, year),
          multiple = "all"
        ) %>%
        filter(!is.na(count_sum_ref)) %>% # filter out RUs for which no reference conditions exist
        group_by(climate, rep, size, freq, browsing, fecundity, identifier, landscape, year) %>%
        mutate(
          threshold = as.factor(ifelse(count_sum_c < 50 & count_sum_ref >= 50, "non-forest", "forest")), # count RUs where tree count dropped below 50 (and reference value is also higher than 50)
          n_rid = length(unique(rid))
        ) %>%
        ungroup()
      dbDisconnect(db.conn)

      # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
      ## patch output ####
      # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
      # requirements for the management intervention to count as a disturbance patch:
      ## needs to be forested (before_ba > 0, n_cells > 0)
      ## trees need to have been killed (killed_ba > 0)

      patch.i <- read_delim(patch_file) %>%
        mutate(
          landscape = landscapename,
          climate = master$climate[i], rep = master$rep[i], size = master$sizeMod[i], freq = master$freqMod[i],
          browsing = master$browsingMod[i], fecundity = master$fecundityMod[i]
        )

      ## regeneration rate ####
      regen.i <- ls.i %>%
        group_by(landscape, year, identifier, size, freq, climate, rep, browsing, fecundity) %>%
        summarise(count.sum = sum(count_ha)) %>%
        ungroup() %>%
        mutate(
          count.previous = lag(count.sum),
          zuwachs = count.sum - count.previous
        ) %>%
        ungroup() %>%
        filter(year > 0) %>%
        full_join(rem.i %>%
          filter(year > 0) %>%
          mutate(count_ha = count / areas[landscape_i, "area"]) %>%
          group_by(landscape, year) %>% # sum up over species + causes of death
          summarise(died = sum(count_ha)) %>% ungroup(), by = c("year", "landscape")) %>%
        mutate(recruited_mean = zuwachs + died)

      # combine all scenarios for the different outputs
      rem <- bind_rows(rem, rem.i)
      ls <- bind_rows(ls, ls.i)
      patch <- bind_rows(patch, patch.i)
      regen <- bind_rows(regen, regen.i)
      ds.basal <- bind_rows(ds.basal, ds.basal.i)
      ds.dom <- bind_rows(ds.dom, ds.dom.i)
      ds.forest <- bind_rows(ds.forest, ds.forest.i)
      rm(db.conn, rem.i, ls.i, patch.i, regen.i, ds.i, ds.basal.i, ds.dom.i, ds.forest.i)
    } else {
      missing <- c(missing, i)
      print(paste0("Oh no! Seems like I (", i, ") am missing"))
    }
    if (i %% 64 == 0) print(paste0(round((i / nrow(master) * 100), 1), " %: rep ", master$rep[i], ", ", i, "; ", format(Sys.time(), "%a %b %d %X")))
    # print(i)
  }

  saveRDS(ls, paste0("results/datasets/intermediates/ls_", landscapename, "_backup.RDATA"))
  saveRDS(rem, paste0("results/datasets/intermediates/rem_", landscapename, "_backup.RDATA"))
  saveRDS(missing, paste0("results/datasets/intermediates/missing_", landscapename, "_backup.RDATA"))
  saveRDS(list(ds.basal, ds.dom, ds.forest), paste0("results/datasets/intermediates/ds.ls_", landscapename, "_backup.RDATA"))
  saveRDS(patch, paste0("results/datasets/intermediates/patch_", landscapename, "_backup.RDATA"))
  saveRDS(regen, paste0("results/datasets/intermediates/regen_", landscapename, "_backup.RDATA"))
  rm(ls, rem, patch, regen, ds.ref, ds.basal, ds.dom, ds.forest, master, dbname, missing)
  gc()
}


### Patch sizes ####
### For Shiretoko and Berchtesgaden

i <- 1
rep.i <- 1
patchlist <- data.frame()
for (i in 1:2) {
  for (rep.i in 1:5) {
    mgmt.full.prelim <- read_csv(paste0("../prep/data/", landscapes[i], "_rep", rep.i, "_level16.csv")) %>%
      group_by(unique, mod.size, mod.freq, mgmt.rep, agent) %>%
      mutate(sum = sum(n())) %>%
      ungroup() %>%
      filter(sum > 4) %>% # filter out patches smaller than 0.5 ha
      dplyr::select(unique, sum, year, mod.size, mod.freq, landscape) %>%
      distinct() %>%
      mutate(rep = rep.i) %>%
      rename(n_cells_soll = sum, size = mod.size, freq = mod.freq, patchID = unique)
    patchlist <- bind_rows(patchlist, mgmt.full.prelim)
    rm(mgmt.full.prelim)
    print(rep.i)
  }
  patchlist %>% summary()
  nrow(patchlist)
  nrow(patchlist %>% ungroup() %>% distinct())
  saveRDS(patchlist %>% distinct(), paste0("results/datasets/intermediates/patchlist_", landscapes[i], ".RDATA"))
}

### For Grand Teton
landscapename <- "grte"
i <- 1
rep <- 1
patchlist <- data.frame()
for (rep in c(1:5)) {
  rep_i <- rep
  level <- data.frame()
  for (i in c(1:16)) {
    mgmt.full <- bind_rows(
      read_csv(paste0("../prep/data/grte_rep", rep_i, "_level", i, "_fire.csv")),
      read_csv(paste0("../prep/data/grte_rep", rep_i, "_level", i, "_bite.csv"))
    ) %>%
      group_by(unique, mod.size, mod.freq, mgmt.rep, agent) %>%
      mutate(sum = sum(n())) %>%
      ungroup() %>%
      filter(sum > 4) %>% # filter out patches smaller than 0.5 ha
      dplyr::select(unique, sum, year, mod.size, mod.freq, landscape) %>%
      mutate(rep = rep_i) %>%
      distinct() %>%
      rename(n_cells_soll = sum, size = mod.size, freq = mod.freq, patchID = unique)
    level <- bind_rows(level, mgmt.full)
    rm(mgmt.full)
    print(i)
  }
  saveRDS(level, paste0("results/datasets/intermediates/grte_patchlist_rep", rep_i, ".RDATA"))
  patchlist <- bind_rows(patchlist, level)
  rm(level)
}
patchlist %>% summary()
nrow(patchlist)
nrow(patchlist %>% distinct())
saveRDS(patchlist %>% distinct(), "results/datasets/intermediates/patchlist_grte.RDATA")



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# 2. Intermediate data sets to final data frames for analysis ###########################################################################################################################################
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

## Based on ds.ls ####

# empty lists and data.frames
overtime.ls <- list(c(), c(), c())
names(overtime.ls) <- landscapes

landscape_i <- 1
for (landscape_i in 1:3) {
  landscapename <- landscapes[landscape_i]

  ds.ls <- readRDS(paste0("results/datasets/ds.ls_", landscapename, "_backup.RDATA")) # bgd: 26 GB
  names(ds.ls) <- c("basal", "dom", "forest")

  ### over time ####
  overtime.ls[[landscapename]] <- ds.ls[["basal"]] %>%
    group_by(threshold, climate, rep, size, freq, browsing, fecundity, identifier, landscape, year) %>%
    summarise(
      mean_basal_c = mean(basal_c), median_basal_c = median(basal_c), sd_basal_c = sd(basal_c), # landscape-wide mean basal area per ha
      mean_basal_ref = mean(basal_ref), median_basal_ref = median(basal_ref), sd_basal_ref = sd(basal_ref),
      prop = mean(sum(n()) / n_rid)
    ) %>% # convert from count to proportion
    filter(threshold == "above") %>% # only keep proportion of unchanged landscape
    ungroup() %>%
    rename("1. Structure\nBasal area decreased by >50 % from reference" = prop) %>%
    dplyr::select(-threshold) %>%
    full_join(
      ds.ls[["dom"]] %>%
        group_by(threshold, climate, rep, size, freq, browsing, fecundity, identifier, landscape, year) %>%
        summarise(prop = mean(sum(n()) / n_rid)) %>% # convert from count to proportion
        filter(threshold == "same") %>% # only keep proportion of unchanged landscape
        ungroup() %>% rename("2. Composition\nDominant species changed from reference" = prop) %>% dplyr::select(-threshold),
      by = join_by(climate, rep, size, freq, browsing, fecundity, identifier, landscape, year)
    ) %>%
    full_join(
      ds.ls[["forest"]] %>% # summary()
        group_by(threshold, climate, rep, size, freq, browsing, fecundity, identifier, landscape, year) %>%
        summarise(prop = mean(sum(n()) / n_rid)) %>% # convert from count to proportion
        filter(threshold == "forest") %>% # only keep proportion of unchanged landscape
        ungroup() %>% rename("3. Remaining forest\nStem density dropping below 50 trees/ha" = prop) %>% dplyr::select(-threshold),
      by = join_by(climate, rep, size, freq, browsing, fecundity, identifier, landscape, year)
    )

  print(overtime.ls[[landscapename]] %>% # summary()
    filter(identifier %in% c(
      "baseline_rep1_size1_freq1_browsing1_fecundity100",
      "hotdry_rep1_size1_freq1_browsing1_fecundity100"
    )))
  # test: props should always be 1 for year 0 (baseline + hotdry)


  if (landscapename == "stoko") overtime.ls[["stoko"]] <- overtime.ls[["stoko"]][-709, ] # fix malformed factor
  if (landscapename == "grte") overtime.ls[["grte"]] <- overtime.ls[["grte"]][c(-7681, -7682), ]

  print("finished a landscape")
  gc()
}
saveRDS(overtime.ls, "results/datasets/overTime.ls.RDATA")
rm(overtime.ls)


## Based on overtime.ls ####
overtime.ls <- readRDS("results/datasets/overtime.ls.RDATA")

### singleProcess.df ####
singleProcess.df <- bind_rows(overtime.ls[["bgd"]], overtime.ls[["grte"]], overtime.ls[["stoko"]]) %>%
  filter(year == 80, climate == "baseline") %>%
  filter(freq == 1, fecundity == 100, browsing == 1) %>%
  pivot_longer(cols = names(response.colors)) %>%
  group_by(landscape, size, name, rep) %>%
  summarise(value = mean(value)) %>%
  ungroup() %>%
  rename(mod = size, "Disturbance size" = value) %>%
  full_join(bind_rows(overtime.ls[["bgd"]], overtime.ls[["grte"]], overtime.ls[["stoko"]]) %>%
    filter(year == 80, climate == "baseline") %>%
    filter(size == 1, fecundity == 100, browsing == 1) %>%
    pivot_longer(cols = names(response.colors)) %>%
    group_by(landscape, freq, name, rep) %>%
    summarise(value = mean(value)) %>% ungroup() %>%
    rename(mod = freq, "Disturbance frequency" = value)) %>%
  full_join(bind_rows(overtime.ls[["bgd"]], overtime.ls[["grte"]], overtime.ls[["stoko"]]) %>%
    filter(year == 80, climate == "baseline") %>%
    filter(size == 1, freq == 1, browsing == 1) %>%
    pivot_longer(cols = names(response.colors)) %>%
    group_by(landscape, fecundity, name, rep) %>%
    summarise(value = mean(value)) %>% ungroup() %>%
    rename(mod = fecundity, "Seed production decrease" = value) %>%
    mutate(mod = case_match(mod, "100" ~ "1", "50" ~ "2", "20" ~ "5", "10" ~ "10"))) %>%
  full_join(bind_rows(overtime.ls[["bgd"]], overtime.ls[["grte"]], overtime.ls[["stoko"]]) %>%
    filter(year == 80, climate == "baseline") %>%
    filter(size == 1, freq == 1, fecundity == 100) %>%
    pivot_longer(cols = names(response.colors)) %>%
    group_by(landscape, browsing, name, rep) %>%
    summarise(value = mean(value)) %>% ungroup() %>%
    rename(mod = browsing, "Sapling growth limitations" = value)) %>%
  pivot_longer(cols = 5:8, names_to = "process") %>%
  mutate(
    value = 100 - value * 100,
    landscape = case_match(landscape, "bgd" ~ "Berchtesgaden", "stoko" ~ "Shiretoko", "grte" ~ "Grand Teton"),
    landscape = factor(landscape, levels = c("Shiretoko", "Berchtesgaden", "Grand Teton")),
    process = factor(process, levels = c(
      "Disturbance size", "Disturbance frequency",
      "Seed production decrease", "Sapling growth limitations"
    ))
  )
write_csv(singleProcess.df, "results/datasets/singleProcess.df.csv")


### dyn.df ####
overtime.ls <- readRDS("results/datasets/overtime.ls.RDATA")
patches <- bind_rows(
  readRDS("results/datasets/patch_bgd_backup.RDATA"),
  readRDS("results/datasets/patch_grte_backup.RDATA"),
  readRDS("results/datasets/patch_stoko_backup.RDATA")
)
head(patches)

patchlist <- bind_rows(
  readRDS("results/datasets/patchlist_bgd.RDATA"),
  readRDS("results/datasets/patchlist_grte.RDATA"),
  readRDS("results/datasets/patchlist_stoko.RDATA")
)
head(patchlist)

# actually forested area: correction factor for GRTE
forest.ha.df <- patches %>%
  inner_join(patchlist) %>%
  inner_join(areas) %>%
  mutate(pct = n_cells / n_cells_soll) %>% # what pct of targeted cells was actually disturbed? (proxy for forested area)
  group_by(year, landscape, climate, size, freq, browsing, fecundity, rep) %>%
  summarise(
    area = mean(area),
    forest.ha = mean(pct) * mean(area),
    n_patches = length(unique(patchID))
  ) %>%
  ungroup() %>%
  mutate(
    size = factor(size, levels = rev(c("10", "5", "2", "1"))),
    freq = factor(freq, levels = rev(c("10", "5", "2", "1"))),
    fecundity = factor(fecundity, levels = c("100", "50", "20", "10")),
    browsing = factor(browsing, levels = rev(c("10", "5", "2", "1"))),
    forest.ha = ifelse(landscape != "grte", area, forest.ha), # only apply this correction for GRTE
    forest.ha = ifelse(is.na(forest.ha), area, forest.ha)
  )
head(forest.ha.df)
summary(forest.ha.df)

patch.df <- patches %>%
  filter(killed_ba > 0) %>%
  group_by(landscape, climate, rep, size, freq, browsing, fecundity, year) %>% # don't group by agent
  summarise(area_disturbed = sum(n_cells)) %>%
  mutate(
    size = factor(size, levels = rev(c("10", "5", "2", "1"))),
    freq = factor(freq, levels = rev(c("10", "5", "2", "1"))),
    fecundity = factor(fecundity, levels = c("100", "50", "20", "10")),
    browsing = factor(browsing, levels = rev(c("10", "5", "2", "1")))
  ) %>%
  inner_join(forest.ha.df) %>%
  mutate(
    dist.rate = area_disturbed / forest.ha, # already in % (because area_disturbed in 100 m^2, forest.ha in ha
    dist.rate = ifelse(dist.rate > 100, 100, dist.rate)
  ) %>%
  mutate(keep = ifelse(forest.ha > area * 0.1, "yes", "no")) %>% # exclude years where forest area drops below 10%
  filter(keep == "yes")
summary(patch.df)
# combine with vegetation data
dist.dyn.df <- bind_rows(overtime.ls[["bgd"]], overtime.ls[["grte"]], overtime.ls[["stoko"]]) %>%
  filter(year == 80) %>%
  dplyr::select(-year) %>%
  full_join(
    patch.df %>%
      group_by(climate, size, freq, browsing, fecundity, landscape, rep, area) %>% # take mean over all simulation years
      summarise(
        n_year = length(unique(year)),
        dist.dyn = mean(dist.rate)
      ),
    by = c("climate", "rep", "size", "freq", "browsing", "fecundity", "landscape")
  ) %>%
  pivot_longer(9:11) %>%
  mutate(dist.dyn = ifelse(is.na(dist.dyn), 0, dist.dyn))
summary(dist.dyn.df)

# recruitment rate
regen.df <- bind_rows(
  readRDS("results/datasets/regen_bgd_backup.RDATA"),
  readRDS("results/datasets/regen_grte_backup.RDATA"),
  readRDS("results/datasets/regen_stoko_backup.RDATA")
) %>%
  mutate(
    size = factor(size, levels = rev(c("10", "5", "2", "1"))),
    freq = factor(freq, levels = rev(c("10", "5", "2", "1"))),
    fecundity = factor(fecundity, levels = c("100", "50", "20", "10")),
    browsing = factor(browsing, levels = rev(c("10", "5", "2", "1")))
  ) %>%
  inner_join(forest.ha.df) %>%
  mutate(
    correction = area / forest.ha,
    recruited_mean = recruited_mean * correction
  )
summary(regen.df)
head(regen.df)
# combine with vegetation data
regen.dyn.df <- bind_rows(overtime.ls[["bgd"]], overtime.ls[["grte"]], overtime.ls[["stoko"]]) %>%
  filter(year == 80) %>%
  dplyr::select(-year) %>%
  full_join(
    regen.df %>%
      group_by(climate, size, freq, browsing, fecundity, landscape, rep, area) %>%
      summarise(
        regen.dyn = mean(recruited_mean),
        n_year = length(unique(year))
      ),
    by = c("climate", "rep", "size", "freq", "browsing", "fecundity", "landscape")
  ) %>%
  pivot_longer(9:11) %>%
  mutate(regen.dyn = ifelse(is.na(regen.dyn), 0,
    ifelse(is.infinite(regen.dyn), 0, regen.dyn)
  ))

dyn.df <- dist.dyn.df %>%
  dplyr::select(-n_year) %>%
  full_join(regen.dyn.df) %>%
  drop_na()
unique(dyn.df$climate) # get rid of 1 run (grte, "baseline_rep1_size1_freq10_browsing1_fecundity10")

rm(patches, patchlist, forest.ha.df)
1 + 1
write_csv(dyn.df, "results/datasets/dyn.df.csv")

### dyn.effect ####
dyn.df <- read_csv("results/datasets/dyn.df.csv")
dyn.effect <- dyn.df %>%
  filter(climate == "baseline") %>%
  dplyr::select(-climate, -identifier, -n_year) %>%
  rename(
    baseline = value,
    dist.dyn_baseline = dist.dyn,
    regen.dyn_baseline = regen.dyn
  ) %>%
  full_join(dyn.df %>% filter(climate == "hotdry") %>%
    dplyr::select(-climate, -identifier, -n_year) %>%
    rename(
      hotdry = value,
      dist.dyn_hotdry = dist.dyn,
      regen.dyn_hotdry = regen.dyn
    )) %>%
  mutate(baseline = 100 - baseline * 100, hotdry = 100 - hotdry * 100) %>%
  mutate(effect = hotdry - baseline)
summary(dyn.effect) # positive when hotdry > baseline, i.e.,  amplifying effect
write_csv(dyn.effect, "results/datasets/dyn.effect.csv")

rm(dyn.df, dyn.effect)
