# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Maps ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


landscape_i <- 1
for (landscape_i in 1:3) {
  landscapename <- landscapes[landscape_i]
  
  ## Indicator maps ####
  
  # map.df <- readRDS(paste0("results/datasets/map.df_", landscapename, ".RDATA"))
  # 
  # # structure
  # singleIndicators <- map.df %>%
  #   filter(identifier %ni% paste0("baseline_rep",c(1:5),"_size1_freq1_browsing1_fecundity100")) %>%
  #   # sample_frac(0.01) %>%
  #   group_by(climate) %>%
  #   mutate(chances = length(unique(paste(rep,size,freq,browsing,fecundity)))) %>% ungroup() %>% # chances = number of runs
  #   pivot_longer(cols=c(10, 14,15)) %>%
  #   group_by(rid, climate, name, x, y, value) %>%
  #   summarise(unchanged = mean(sum(n())/chances)*100) %>% ungroup() %>%
  #   filter(value=="no") %>%
  #   full_join(rid.df[rid.df$landscape==landscapename,] %>%
  #               expand_grid(climate = c("baseline", "hotdry"), value=c("no"),
  #                           name = c("1. Structure\nBasal area decreased by >50 % from reference", 
  #                                    "2. Composition\nDominant species changed from reference", 
  #                                    "3. Remaining forest\nStem density dropping below 50 trees/ha")),
  #             by = c("rid", "x", "y", "climate", "value", "name")) %>%
  #   mutate(unchanged = ifelse(is.na(unchanged), 0, unchanged))
  # singleIndicators %>% summary()
  # 
  # png(paste0("results/figures/Q3_map_singleIndicators_", landscapename ,".png"), res=200,
  #     height=2000, width=2000)
  # print(
  #   singleIndicators %>%
  #     filter(!is.na(x)) %>% 
  #     ggplot(aes(x=x, y=y, fill=unchanged)) +
  #     geom_tile() +
  #     facet_grid(climate~name) +
  #     coord_equal() +
  #     scale_fill_distiller(palette = "PuRd", direction = -1) +
  #     labs(fill="% unchanged",
  #          title="Map showing which cells where more susceptible to change\nCell value is the % of scenarios where cells remained unchanged") +
  #     theme_bw()
  # )
  # dev.off()
  # 
  # rm(singleIndicators, map.df); gc()
  # print("finished indicator maps")
  
  # Endpoint maps ####
  
  endpoint <- readRDS(paste0("results/datasets/map_endpoints_",landscapename,".RDATA"))
  
  # basal area
  png(paste0("results/figures/Q3_map_1_basalArea_", landscapename ,".png"), res=200,
      height=1600, width=2000)
  print(
    endpoint %>% 
    dplyr::select(-basal_ref, -count_sum_c, -count_sum_ref, -dom_c, -dom_ref, -basal_diff) %>% 
    group_by(rid, climate, size, freq, browsing, fecundity, landscape) %>% #everything but rep
    summarise(basal_c=mean(basal_c)) %>% ungroup() %>% 
    mutate(identifier = paste("size", size, "frequency", freq, "\nbrowsing", browsing, "fecundity", fecundity)) %>% 
    full_join(rid.df[rid.df$landscape==landscapename,]) %>% 
    filter(!is.na(size)) %>% 
    mutate(basal_c = ifelse(basal_c > quantile(basal_c, 0.9), quantile(basal_c, 0.9), basal_c)) %>% 
    ggplot(aes(x=x, y=y, fill=basal_c)) +
    geom_tile() +
    coord_equal() +
    scale_fill_distiller(palette="YlGnBu", direction=1) +
    facet_grid(climate~identifier) +
    labs(x="", y="", fill = "Mean basal area [m^2 ha^-1]") + 
    theme_bw() +
    theme(legend.position = "bottom"))
  dev.off()
  
  
  # dominant species
  spec.col <- helper.files[[landscapename]][["species"]] %>% filter(species %in% common.species[[landscapename]]) %>% arrange(species) %>% mutate(displayColor = paste0("#", displayColor)) %>% pull(displayColor)
  names(spec.col)<- sort(common.species[[landscapename]])
  
  png(paste0("results/figures/Q3_map_2_dominantSpecies_", landscapename ,".png"), res=200,
      height=1600, width=2000)
  print(
    endpoint %>% 
    dplyr::select(-dom_ref, -count_sum_c, -count_sum_ref, -basal_c, -basal_ref, -dom_ref, -basal_diff) %>% 
    group_by(rid, climate, size, freq, browsing, fecundity, landscape, dom_c) %>% #everything but rep
    mutate(species.count = n()) %>% ungroup() %>% 
    group_by(rid, climate, size, freq, browsing, fecundity, landscape) %>% 
    filter(species.count==max(species.count)) %>% 
    dplyr::select(-rep, -identifier) %>% distinct() %>% 
    mutate(n.row = n(),
           dom_c = ifelse(n.row > 1, sample(unique(dom_c), 1), dom_c)) %>% 
    dplyr::select(-species.count, -n.row) %>% ungroup() %>% distinct() %>% 
    mutate(identifier = paste("size", size, "frequency", freq, "\nbrowsing", browsing, "fecundity", fecundity)) %>% 
    full_join(rid.df[rid.df$landscape==landscapename,]) %>% 
    filter(!is.na(size)) %>% 
    mutate(dom_c = ifelse(dom_c %in% common.species[[landscapename]], dom_c, "Other")) %>% 
    ggplot(aes(x=x, y=y, fill=dom_c)) +
    geom_tile() +
    coord_equal() +
    scale_fill_manual(values=spec.col) +
    facet_grid(climate~identifier) +
    labs(x="", y="", fill = "Dominant species") + 
    theme_bw() +
    theme(legend.position = "bottom"))
  dev.off()
  
  
  # forest loss
  png(paste0("results/figures/Q3_map_3_forestLoss_", landscapename ,".png"), res=200,
      height=1600, width=2000)
  print(
    endpoint %>% 
    dplyr::select(-basal_c, -basal_ref, -dom_c, -dom_ref, -basal_diff) %>% 
    group_by(rid, climate, size, freq, browsing, fecundity, landscape) %>% #everything but rep
    summarise(count_sum_c=mean(count_sum_c),
              count_sum_ref=mean(count_sum_ref)) %>% ungroup() %>% 
    mutate(identifier = paste("size", size, "frequency", freq, "\nbrowsing", browsing, "fecundity", fecundity)) %>% 
    full_join(rid.df[rid.df$landscape==landscapename,]) %>% 
    filter(!is.na(size)) %>% 
    mutate(forest.loss = as.factor(ifelse(count_sum_c < 50 & count_sum_ref >= 50, "Non-Forest", "Forest"))) %>% 
    ggplot(aes(x=x, y=y, fill=forest.loss)) +
    geom_tile() +
    coord_equal() +
    scale_fill_manual(values=c("Non-Forest" = "#c51b7d", "Forest" = "#7fbc41")) +
    facet_grid(climate~identifier) +
    labs(x="", y="", fill = "Forest Loss") + 
    theme_bw() +
    theme(legend.position = "bottom"))
  dev.off()
  
  rm(endpoint, spec.col)
  
  print("finished endpoint maps")
}

