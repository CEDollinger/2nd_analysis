# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Diagnostic plots ###########################################################################################################################################
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
overtime.ls <- readRDS("results/datasets/overtime.ls.RDATA")
ref.df <- read_csv("raw_data/helper_files/ref.df.csv")

landscape_i <- 3
for (landscape_i in 1:3) {
  landscapename <- landscapes[landscape_i]
  
  ### line plots ####
  png(paste0("results/figures/diagnostics/lineplot_overtime_", landscapename ,".png"), res=200,
      height=1500, width=1500)
  print(
    overtime.ls[[landscapename]] %>% 
      pivot_longer(cols=10:12) %>% 
      ggplot(aes(x=year+2020, y=value*100, group=identifier, col=climate)) +
      geom_line(linewidth=0.2, alpha=0.2) +
      facet_wrap(~name, ncol=1) +
      scale_color_manual(values=c("baseline" ="blue", "hotdry" = "red")) +
      labs(x="Year", y="Proportion of unchanged landscape [%]", col="Climate scenario") +
      theme_bw()
  )
  dev.off()
  
  # structure: basal area
  # var <- "fecundity"
  plot.basal.fc <- function(var) {
    others <- vars[vars %ni% var]
    copy <- overtime.ls[[landscapename]] %>% filter(year==80)
    var_use <- copy %>% pull(var)
    copy$groups <- paste(copy %>% pull(all_of(others[1])),
                         copy %>% pull(all_of(others[2])),
                         copy %>% pull(all_of(others[3])),
                         copy %>% pull(rep),
                         copy %>% pull(climate))
    copy %>% 
      rename(prop = 10) %>% 
      ggplot(aes(x=var_use, y=prop*100, group=groups, col=climate)) +
      geom_line(linewidth=0.2, alpha=0.4) +
      scale_color_manual(values=c("baseline" ="blue", "hotdry" = "red")) +
      labs(x=var, y="", col="Climate scenario") +
      theme_bw()
    
  }; basal.ls <- lapply(vars, plot.basal.fc)
  png(paste0("results/figures/diagnostics/lineplot_basal80_", landscapename ,".png"), res=200,
      height=1000, width=2000)
  print(
    ggpubr::ggarrange(basal.ls[[1]], basal.ls[[2]], basal.ls[[3]], basal.ls[[4]], common.legend = T) %>% 
      annotate_figure(top = "Structure: basal area decreased by >50 % from reference",
                      left = "Proportion of unchanged landscape [%]"
      ))
  dev.off()
  
  # composition: dominant species
  plot.dom.fc <- function(var) {
    others <- vars[vars %ni% var]
    copy <- overtime.ls[[landscapename]] %>% filter(year==80)
    var_use <- copy %>% pull(var)
    copy$groups <- paste(copy %>% pull(all_of(others[1])),
                         copy %>% pull(all_of(others[2])),
                         copy %>% pull(all_of(others[3])),
                         copy %>% pull(rep),
                         copy %>% pull(climate))
    copy %>% 
      rename(prop = 11) %>% 
      ggplot(aes(x=var_use, y=prop*100, group=groups, col=climate)) +
      geom_line(linewidth=0.2, alpha=0.4) +
      scale_color_manual(values=c("baseline" ="blue", "hotdry" = "red")) +
      labs(x=var, y="", col="Climate scenario") +
      theme_bw()
    
  }; dom.ls <- lapply(vars, plot.dom.fc)
  
  png(paste0("results/figures/diagnostics/lineplot_dom80_", landscapename ,".png"), res=200,
      height=1000, width=2000)
  print(
    ggpubr::ggarrange(dom.ls[[1]], dom.ls[[2]], dom.ls[[3]], dom.ls[[4]], common.legend = T) %>% 
      annotate_figure(top = "Composition: dominant species changed from reference",
                      left = "Proportion of unchanged landscape [%]"
      ))
  dev.off()
  
  # forest/non-forest
  plot.forest.fc <- function(var) {
    others <- vars[vars %ni% var]
    copy <- overtime.ls[[landscapename]] %>% filter(year==80)
    var_use <- copy %>% pull(var)
    copy$groups <- paste(copy %>% pull(all_of(others[1])),
                         copy %>% pull(all_of(others[2])),
                         copy %>% pull(all_of(others[3])),
                         copy %>% pull(rep),
                         copy %>% pull(climate))
    copy %>% 
      rename(prop = 12) %>% 
      ggplot(aes(x=var_use, y=prop*100, group=groups, col=climate)) +
      geom_line(linewidth=0.2, alpha=0.4) +
      scale_color_manual(values=c("baseline" ="blue", "hotdry" = "red")) +
      labs(x=var, y="", col="Climate scenario") +
      theme_bw()
    
  }; forest.ls <- lapply(vars, plot.forest.fc)
  png(paste0("results/figures/diagnostics/lineplot_forest80_", landscapename ,".png"), res=200,
      height=1000, width=2000)
  print(
    ggpubr::ggarrange(forest.ls[[1]], forest.ls[[2]], forest.ls[[3]], forest.ls[[4]], common.legend = T) %>% 
      annotate_figure(top = "Remaining forest: stem density dropping below 50 trees/ha",
                      left = "Proportion of unchanged landscape [%]"
      ))
  dev.off()
  
  rm(basal.ls, dom.ls, forest.ls)
  print("finished overtime")
  
  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
  ## maps ####
  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
  
  map.df <- readRDS(paste0("results/datasets/map.df_", landscapename, ".RDATA"))
  
  # structure
  singleIndicators <- map.df %>%
    filter(identifier != "baseline_rep1_size1_freq1_browsing1_fecundity100") %>%
    # sample_frac(0.01) %>%
    group_by(climate) %>%
    mutate(chances = length(unique(paste(rep,size,freq,browsing,fecundity)))) %>% ungroup() %>% # chances = number of runs
    pivot_longer(cols=c(structure_change, composition_change, regime_shift)) %>%
    group_by(rid, climate, name, x, y, value) %>%
    summarise(unchanged = mean(sum(n())/chances)*100) %>% ungroup() %>%
    filter(value=="no") %>%
    full_join(rid.df[rid.df$landscape==landscapename,] %>%
                expand_grid(climate = c("baseline", "hotdry"), value=c("no"),
                            name = c("structure_change", "composition_change", "regime_shift")),
              by = c("rid", "x", "y", "climate", "value", "name")) %>%
    mutate(unchanged = ifelse(is.na(unchanged), 0, unchanged))
  # singleIndicators %>% summary()

  png(paste0("results/figures/diagnostics/map_singleIndicators_", landscapename ,".png"), res=200,
      height=2000, width=2000)
  print(
    singleIndicators %>%
      mutate(name = factor(name, levels=c("composition_change", "structure_change", "regime_shift"))) %>%
      ggplot(aes(x=x, y=y, fill=unchanged)) +
      geom_tile() +
      facet_grid(climate~name) +
      coord_equal() +
      scale_fill_distiller(palette = "PuRd", direction = -1) +
      labs(fill="% unchanged",
           title="Map showing which cells where more susceptible to change\nCell value is the % of scenarios where cells remained unchanged") +
      theme_bw()
  )
  dev.off()

  rm(singleIndicators, map.df); gc()
  print("finished maps")
  
  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
  ## landscape output ####
  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
  
  ls.df <- readRDS(paste0("results/datasets/ls_", landscapename, "_backup.RDATA"))

  # mean basal area over time
  (mean_line <- ls.df %>%
      group_by(year, climate, size, freq, fecundity, browsing, rep) %>%
      summarise(n = sum(basal_area_m2)) %>% ungroup() %>%
      group_by(size, freq, climate, year) %>%
      summarise(mean = mean(n)) %>% ungroup() %>%
      mutate(dist=paste(size, freq), distRate = as.numeric(as.character(size)) * as.numeric(as.character(freq))))

  png(paste0("results/figures/diagnostics/landscapeWide_basalArea_overtime_", landscapename, ".png"), res=200,
      height=2000, width=4000)
  print(
    ls.df %>%
      mutate(groups = paste("size:", size, ", freq:", freq, ", fecundity:", fecundity, ", browsing:", browsing, ", rep:", rep)) %>%
      group_by(groups, year, climate, rep, size, freq) %>%
      summarise(n = sum(basal_area_m2)) %>% ungroup() %>%
      ggplot(aes(x=year, y=n)) +
      geom_line(aes(group=groups), show.legend = F, linewidth=0.2, alpha=0.4) +
      geom_line(data=mean_line, aes(y=mean, col=reorder(dist, distRate)), linewidth=2) +
      facet_grid(~climate) +
      labs(x="Year", y="Mean basal area [m²/ha]", col="Size and frequency modification",
           title=paste0("How does process modfication impact basal area development in ", toupper(landscapename), "?\nColored lines: mean trajectory for the disturbance regime")) +
      theme_bw()
  )
  dev.off()

  # final basal area
  ## stratified by disturbance
  png(paste0("results/figures/diagnostics/landscapeWide_finalBasalArea_disturbances_", landscapename, ".png"), res=200,
      height=1600, width=3400)
  print(
    ls.df %>%
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
      geom_hline(aes(yintercept = ls.df %>% filter(year==0) %>% pull(basal_area_m2) %>% unique() %>% sum()),
                 col="black") +
      facet_grid(~climate) +
      labs(x="Disturbance rate modification", y="Mean basal area in 2100 [m²/ha]",
           col="Fecundity and browsing modification level",
           title="Basal area in 2100: all 256 possible process modification combinations\nSolid line: value in 2020") +
      theme_bw() +
      theme(legend.position="bottom")
  )
  dev.off()

  ## stratified by regeneration
  png(paste0("results/figures/diagnostics/landscapeWide_finalBasalArea_regeneration_", landscapename, ".png"), res=200,
      height=1600, width=3400)
  print(
    ls.df %>%
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
      geom_hline(aes(yintercept = ls.df %>% filter(year==0) %>% pull(basal_area_m2) %>% unique() %>% sum()),
                 col="black") +
      facet_grid(~climate) +
      labs(x="Regeneration modification", y="Mean basal area in 2100 [m²/ha]",
           col="Disturbance size and frequency modification level",
           title="Basal area in 2100: all 256 possible process modification combinations\nSolid line: value in 2020") +
      theme_bw() +
      theme(legend.position="bottom")
  )
  dev.off()

  # regeneration processes: final basal area
  png(paste0("results/figures/diagnostics/landscapeWide_basalArea_regeneration_", landscapename, ".png"), res=200,
      height=2000, width=3000)
  print(
    ls.df %>%
      filter(year > 70) %>% # last simulation decade
      mutate(distRate = as.numeric(size)*as.numeric(freq),
             groups = paste("fecundity:", fecundity, "\nbrowsing:", browsing)) %>%
      filter(distRate==1) %>%
      group_by(fecundity, browsing, climate) %>%
      summarise(n = sum(basal_area_m2)/50) %>% # 50: 10 years x 5 reps
      ggplot() +
      geom_line(aes(x=as.numeric(as.character(browsing)), y=n, col=as.factor(as.numeric(as.character(fecundity)))),
                linewidth=2) +
      facet_grid(~climate) +
      labs(x="browsing modification", y="Mean basal area in last sim. decade [m²/ha]", col="Fecundity modification [%]",
           title=paste0("Modified regeneration: impact on basal area in ", toupper(landscapename), "\nDisturbance rate kept constant at original value\nBlack star: disturbances simulated via modules instead of management")) +
      theme_bw()
  )
  dev.off()


  # dominant species
  mean_line.species <- ls.df %>%
    group_by(year, climate, rep, size, freq, fecundity, browsing) %>%
    mutate(total = sum(basal_area_m2)) %>%
    filter(species == common.species[[landscapename]][1]) %>%
    summarise(n = basal_area_m2/total) %>%
    group_by(size, freq, climate, year) %>%
    summarise(mean = mean(n)) %>% ungroup() %>%
    mutate(dist=paste(size, freq), distRate = as.numeric(size) * as.numeric(freq))
  png(paste0("results/figures/diagnostics/landscapeWide_mostCommonSpecies_overtime_", landscapename, ".png"), res=200,
      height=2000, width=4000)
  print(
    ls.df %>%
      mutate(groups = paste("size:", size, ", freq:", freq, ", fecundity:", fecundity, ", browsing:", browsing, ", rep:", rep)) %>%
      group_by(groups, year, climate, rep) %>%
      mutate(total = sum(basal_area_m2)) %>%
      filter(species == common.species[[landscapename]][1]) %>%
      summarise(n = basal_area_m2/total) %>%
      ggplot(aes(x=year, y=n*100)) +
      geom_line(aes(group=groups), show.legend = F, linewidth=0.2, alpha=0.4) +
      geom_line(data=mean_line.species, aes(y=mean*100, col=reorder(dist, distRate)), linewidth=2) +
      facet_grid(~climate) +
      labs(x="Year", y=paste0("Proportion of ", common.species[[landscapename]][1], " [%, basal area]"), col="",
           title=paste0("How does the most common species fare in ", toupper(landscapename), "?\nDotted red line: simulation with disturbances simulated via modules instead of management\nColored lines: mean trajectory for the disturbance regime")) +
      theme_bw()
  )
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
  png(paste0("results/figures/diagnostics/landscapeWide_speciesComposition_relative_extremes_", landscapename, ".png"), res=200,
      height=2000, width=3000)
  print(
    ls.df %>%
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
  )
  dev.off()

  # absolute
  png(paste0("results/figures/diagnostics/landscapeWide_speciesComposition_absolute_extremes_", landscapename, ".png"), res=200,
      height=2000, width=3000)
  print(
    ls.df %>%
      filter(identifier %in% c("baseline_rep1_size1_freq1_browsing1_fecundity100", "baseline_rep1_size10_freq10_browsing10_fecundity10",
                               "hotdry_rep1_size1_freq1_browsing1_fecundity100", "hotdry_rep1_size10_freq10_browsing10_fecundity10")) %>%
      mutate(species = ifelse(species %in% common.species[[landscapename]], species, "other"),
             groups = paste("size:", size, ", freq:", freq, ", fecundity:", fecundity, ", browsing:", browsing)) %>%
      ggplot(aes(x=year, y=basal_area_m2)) +
      geom_bar(aes(fill=species), stat="identity", width=1.1) +
      facet_grid(groups~climate) +
      labs(x="Year", fill="Species", y="Mean basal area [m²/ha]") +
      theme_bw()
  )
  dev.off()

  rm(ls.df, mean_line, mean_line.species, label.df)
  print("finished ls.df")

  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
  ## trees killed output ####
  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
  
  rem.df <- readRDS(paste0("results/datasets/rem_", landscapename, "_backup.RDATA"))
  
  mean_line.rem <- rem.df %>% 
    group_by(year, climate, rep, size, freq, fecundity, browsing) %>% 
    summarise(n = sum(basal_area_m2)) %>% 
    group_by(size, freq, climate, year) %>% 
    summarise(mean = mean(n)) %>% ungroup() %>% 
    mutate(dist=paste(size, freq), distRate = as.numeric(size) * as.numeric(freq)) 
  
  png(paste0("results/figures/diagnostics/landscapeWide_treesKilled_", landscapename, ".png"), res=200,
      height=2000, width=3000)
  print(
    rem.df %>% 
      mutate(groups = paste("size:", size, ", freq:", freq, ", fecundity:", fecundity, ", browsing:", browsing, ", rep:", rep)) %>% 
      group_by(groups, year, climate, rep) %>% 
      summarise(n = sum(basal_area_m2)) %>% 
      ggplot(aes(x=year, y=n)) +
      geom_line(aes(group=groups), show.legend = F, linewidth=0.2, alpha=0.4) +
      geom_line(data=mean_line.rem, aes(y=mean, col=reorder(dist, distRate)), linewidth=2) +
      scale_y_log10() +
      facet_grid(~climate) +
      labs(x="Year", y="Basal area killed by disturbance [m²]", col="", 
           title=paste0("How does process modfication impact tree mortality in ", toupper(landscapename), "?\nBlack line: simulation with disturbances simulated via modules instead of management\nY-axis log10-transformed")) +
      theme_bw()
  )
  dev.off()
  
  # rem.df %>% filter(climate=="baseline") %>% pull(basal_area_m2) %>% summary()
  # rem.df %>% filter(climate=="hotdry") %>% pull(basal_area_m2) %>% summary()
  # rem.df %>% group_by(climate) %>% summarise(sum_basal = round(sum(basal_area_m2),-3))
  
  rm(rem.df, mean_line.rem)
  print("finished rem.df")
  
  ## patch output ####
  
  patch.df <- readRDS(paste0("results/datasets/patch_", landscapename, "_backup.RDATA"))
  
  png(paste0("results/figures/diagnostics/simulatedDisturbances_relativeDifference_", landscapename, ".png"), res=200,
      height=2000, width=3000)
  print(
    patch.df %>% 
      group_by(landscape, climate, rep, size, freq, browsing, fecundity, agent) %>% 
      summarise(size_actual = mean(n_cells), 
                events_actual = n()) %>% 
      full_join(ref.df[ref.df$landscape==landscapename,], by = join_by(landscape, agent)) %>% 
      mutate(disturbanceRate = as.numeric(as.character(size)) * as.numeric(as.character(freq)),
             size_expected = size_expected * mean(as.numeric(size)),
             events_expected = events_expected * mean(as.numeric(freq)),
             area_expected = size_expected * events_expected/100, # divided by 100 to get area in hectare
             area_actual = size_actual * events_actual/100,
             area_diff = (area_actual-area_expected)/area_expected) %>% #summary()
      ggplot() + 
      geom_histogram(aes(x=area_diff*100), alpha=0.7) + 
      geom_vline(aes(xintercept=0)) + 
      facet_grid(~agent) +
      labs(x="Difference in total area disturbed from expected area disturbed [%]") +
      theme_bw()
  )
  dev.off()
  
  rm(patch.df); gc()
  print(paste0("Finished ", landscapename))
  
}; rm(overtime.ls, ref.df)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# DISCARDED ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# print(
#   basal.map %>% filter(climate=="baseline") %>% dplyr::select(-climate) %>% filter(!is.na(x)) %>% 
#     rast() %>% plot(range=c(0,100), cex.main=0.5, type="continuous",
#                     main="1. Structure: basal area decreased by >50 % from reference\nBaseline climate\nPercentage of runs where cell's structure remained unchanged [%]")
# )
# dev.off()  
# png(paste0("results/figures/diagnostics/map_prop_basal80_hotdry_", landscapename ,".png"), res=200,
#     height=1000, width=1000)
# print(
#   basal.map %>% filter(climate=="hotdry") %>% dplyr::select(-climate) %>% filter(!is.na(x)) %>% 
#     rast() %>% plot(range=c(0,100), cex.main=0.5, type="continuous",
#                     main="1. Structure: basal area decreased by >50 % from reference\nHot-dry climate\nPercentage of runs where cell's structure remained unchanged [%]")
#   
# )
# dev.off()  
# 
# # composition
# dom.map <- map.ls[["dom"]] %>% 
#   group_by(climate) %>% 
#   mutate(chances = length(unique(identifier))) %>% ungroup() %>% 
#   group_by(rid, climate) %>% 
#   summarise(unchanged = mean(sum(n())/chances)*100) %>% ungroup() %>% 
#   full_join(rid.df[rid.df$landscape==landscapename,], by = join_by(rid)) %>% 
#   dplyr::select(x,y,unchanged, climate) %>% 
#   mutate(unchanged = ifelse(is.na(unchanged), 0, unchanged))
# png(paste0("results/figures/diagnostics/map_prop_dom80_baseline_", landscapename ,".png"), res=200,
#     height=1000, width=1000)
# print(
#   dom.map %>% filter(climate=="baseline") %>% dplyr::select(-climate) %>% filter(!is.na(x)) %>% 
#     rast() %>% plot(range=c(0,100), cex.main=0.5, type="continuous",
#                     main="2. Composition: dominant species changed from reference\nBaseline climate\nPercentage of runs where cell's dominant species remained unchanged [%]")
# )
# dev.off()
# png(paste0("results/figures/diagnostics/map_prop_dom80_hotdry_", landscapename ,".png"), res=200,
#     height=1000, width=1000)
# print(
#   dom.map %>% filter(climate=="hotdry") %>% dplyr::select(-climate) %>% filter(!is.na(x)) %>% 
#     rast() %>% plot(range=c(0,100), cex.main=0.5, type="continuous",
#                     main="2. Composition: dominant species changed from reference\nHot-dry climate\nPercentage of runs where cell's dominant species remained unchanged [%]")
# )
# dev.off()
# 
# # forest
# forest.map <- map.ls[["forest"]] %>% 
#   group_by(climate) %>% 
#   mutate(chances = length(unique(identifier))) %>% ungroup() %>% 
#   group_by(rid, climate) %>% 
#   summarise(unchanged = mean(sum(n())/chances)*100) %>% ungroup() %>% 
#   full_join(rid.df[rid.df$landscape==landscapename,], by = join_by(rid)) %>% 
#   dplyr::select(x,y,unchanged, climate)
# png(paste0("results/figures/diagnostics/map_prop_forest80_baseline_", landscapename ,".png"), res=200,
#     height=1000, width=1000)
# print(
#   forest.map %>% filter(climate=="baseline") %>% dplyr::select(-climate) %>% filter(!is.na(x)) %>%  
#     rast() %>% plot(range=c(0,100), cex.main=0.5, type="continuous",
#                     main="3. Remaining forest: stem density dropping below 50 trees/ha\nBaseline climate\nPercentage of runs where cell remained forested [%]")
# )
# dev.off()
# png(paste0("results/figures/diagnostics/map_prop_forest80_hotdry_", landscapename ,".png"), res=200,
#     height=1000, width=1000)
# print(
#   forest.map %>% filter(climate=="hotdry") %>% dplyr::select(-climate) %>% filter(!is.na(x)) %>%  
#     rast() %>% plot(range=c(0,100), cex.main=0.5, type="continuous",
#                     main="3. Remaining forest: stem density dropping below 50 trees/ha\nHot-dry climate\nPercentage of runs where cell remained forested [%]")
# )
# dev.off()

# # reorganization map
# 
# png(paste0("results/figures/diagnostics/map_reorganization_", landscapename ,".png"), res=200,
#     height=2000, width=2000)
# print(
#   map.df %>%
#     #sample_frac(0.01) %>%
#     filter(identifier != "baseline_rep1_size1_freq1_browsing1_fecundity100") %>%
#     mutate(pathway = ifelse(structure_change == "no" & composition_change == "no", "Resilience",
#                             ifelse(structure_change == "yes" & composition_change == "no", "Restructuring",
#                                    ifelse(structure_change == "no" & composition_change == "yes", "Reassembly",
#                                           ifelse(structure_change == "yes" & composition_change == "yes", "Replacement",
#                                                  ifelse(regime_shift=="yes", "Regime shift", NA))))),
#            climate = case_match(climate, "hotdry" ~ "Hot-dry", "baseline" ~ "Baseline")) %>%
#     group_by(rid, climate, pathway, x, y) %>%
#     summarise(count=sum(n())) %>% ungroup() %>%
#     group_by(rid, climate) %>%
#     filter(count == max(count)) %>%
#     # if multiple pathways have an identical frequency: sample one randomly
#     mutate(n_pathway = sum(n()),
#            pathway = ifelse(n_pathway > 1, sample(unique(pathway), 1), pathway)) %>% ungroup() %>%
#     distinct() %>% dplyr::select(-n_pathway) %>%
#     mutate(pathway = factor(pathway, levels=(c("Regime shift", "Replacement", "Restructuring", "Reassembly", "Resilience")))) %>%
#     ggplot(aes(x=x, y=y, fill=pathway)) +
#     geom_tile() +
#     facet_grid(~climate) +
#     coord_equal() +
#     scale_fill_manual(values=c("Reassembly" = "#fed976", "Replacement" = "#bd0026",
#                                "Resilience" = "#a6bddb", "Restructuring" = "#fd8d3c", "Regime shift" = "black")) +
#     labs(fill="Pathway",
#          title="Map showing which cells reorganized\nPathway determined by majority vote over all scenarios") +
#     theme_bw()
# )
# dev.off()
