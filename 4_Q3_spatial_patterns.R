
# line plot ####

landscape_i <- 1
elev.df <- data.frame()
for (landscape_i in 1:3) {
  landscapename <- landscapes[landscape_i]
  map.df <- readRDS(paste0("results/datasets/map.df_", landscapename, ".RDATA"))
  df.i <- rid.df %>% 
    group_by(landscape) %>% 
    mutate(step = round(dist.treeline,-1)) %>% ungroup() %>% # group rids in elevation steps of 10 m
    inner_join(map.df[sample(1:nrow(map.df), nrow(map.df)*1),]) %>% 
    pivot_longer(cols=c(15,17,18)) %>% 
    mutate(value = ifelse(value == "no", 0, 1)) %>% 
    group_by(landscape, step, climate, name) %>% 
    summarise(chances = n(), # total number of rids in this elevation step (varies between elevation steps)
              changed = sum(value), # total number of rids that experienced change per elevation step
              pct = changed/chances*100) # pct of rids that experienced change per elevation step
  png(paste0("results/figures/Q3_distanceFromTreeline_", landscapename ,".png"), res=200,
      height=1600, width=2000)
  p <- df.i %>% 
    ggplot(aes(x=step, y=pct, col = climate)) +
    geom_point(size=0.6, alpha=0.7) +
    geom_smooth(se=F, method="loess") +
    scale_x_reverse() +
    scale_color_manual(values=c("hotdry" = "firebrick2", "baseline"="royalblue3")) +
    facet_grid(~name) +
    labs(x="Distance from tree line [m]", y="Percentage changed [%]", title = toupper(landscapename)) +
    theme_bw(); print(p)
  dev.off(); rm(p)
  elev.df <- bind_rows(elev.df, df.i); rm(df.i, map.df)
  if (landscape_i == 3) write_csv(elev.df, "results/datasets/elev.df.csv")
  print(landscapename); rm(landscapename)
}; gc()

elev.df %>% 
  group_by(landscape) %>% 
  summarise(mean = mean(chances),
            median = median(chances),
            min = min(chances),
            max = max(chances))
# landscape    mean median   min     max
# bgd       112823. 121600  1299  227450
# grte      294618. 202858   612 1387590
# stoko     366431. 417647   887  620800

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# DISCARDED ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

## maps ####

landscape_i <- 1
for (landscape_i in 1:3) {
  landscapename <- landscapes[landscape_i]
  
  ### Indicator maps ####
  
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
  
  ## Endpoint maps ####
  
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
  spec.col <- c(helper.files[[landscapename]][["species"]] %>% filter(species %in% common.species[[landscapename]]) %>% arrange(species) %>% mutate(displayColor = paste0("#", displayColor)) %>% pull(displayColor),
                "#808080")
                names(spec.col)<- c(sort(common.species[[landscapename]]), "Other")
                if (landscapename=="grte") {
                  spec.col[1] <- "#013220"; spec.col[2] <- "#00FF00"
                }
                if (landscapename=="stoko") {
                  spec.col[1] <- "#0504aa"; spec.col[2] <- "#33ff49"; spec.col[3] <- "#FFA500"; spec.col[6] <- "#A020F0"; spec.col[8] <- "#fe019a" 
                }
                
                png(paste0("results/figures/Q3_map_2_dominantSpecies_", landscapename ,".png"), res=200,
                    height=1600, width=2000)
                print(
                  endpoint %>% 
                    dplyr::select(-dom_ref, -count_sum_c, -count_sum_ref, -basal_c, -basal_ref, -basal_diff) %>% 
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
                
                # types of change
                png(paste0("results/figures/Q3_map_2_dominantSpecies_typesOfChange_", landscapename ,".png"), res=200,
                    height=1600, width=2000)
                print(
                  endpoint %>% 
                    dplyr::select(-count_sum_c, -count_sum_ref, -basal_c, -basal_ref, -basal_diff) %>% 
                    filter(identifier %ni% c(paste0("baseline_rep",1:5,"_size1_freq1_browsing1_fecundity100"))) %>% 
                    group_by(climate, size, freq, browsing, fecundity, dom_c, dom_ref) %>% 
                    summarise(count = n()) %>% ungroup() %>% 
                    dplyr::select(climate, size, freq, browsing, fecundity, dom_ref, dom_c, count) %>% 
                    filter(dom_ref != dom_c) %>% 
                    mutate(identifier = paste("size", size, "frequency", freq, "\nbrowsing", browsing, "fecundity", fecundity),
                           species.change = paste(dom_ref, "to", dom_c)) %>% 
                    group_by(identifier, climate) %>% 
                    slice_max(count, n = 5) %>% 
                    ggplot(aes(x=reorder(species.change, count), y=count/5/areas[landscape_i]*100, fill=identifier)) + # count: divided by 5 to get mean over all 5 reps, divided by landscape area to get % of landscape, *100 for %
                    geom_bar(stat="identity", position = position_dodge()) +
                    facet_grid(~climate) +
                    scale_fill_brewer(palette = "Spectral", direction=-1) +
                    coord_flip() +
                    labs(x="Type of change", y="Mean percentage of full landscape [%]", fill = "Scenario") + 
                    theme_bw() +
                    theme(legend.position = "bottom")
                )
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

