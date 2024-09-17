
# Q2 ####

## contour for every landscape ####
dyn.df <- read_csv("results/datasets/dyn.df.csv")

# TO DOs
## mask out missing data space
## add hull around landscapes

i<-1; clim<-"bgd"
for (clim in landscapes) { #, "hotdry"
  for (i in 1:3) {
    a <- dyn.df %>% 
      filter(name == names(response.colors)[i], 
             landscape==clim) %>% 
      dplyr::select(dist.dyn, regen.dyn, value) %>% 
      mutate(value=100-value*100, dist.dyn = log10(dist.dyn))
    print(summary(a))
    
    #### fit loess model and predict 
    data.loess <- loess(value ~ dist.dyn * regen.dyn, data = a)
    # Create a sequence of incrementally increasing (by 0.3 units) values for both wt and hp
    xgrid <- seq(min(a$dist.dyn), quantile(a$dist.dyn, 1), length.out=100)
    ygrid <- seq(min(a$regen.dyn), max(a$regen.dyn), length.out=100)
    # Generate a dataframe with every possible combination of wt and hp
    data.fit <-  expand.grid(dist.dyn = xgrid, regen.dyn = ygrid)
    # Feed the dataframe into the loess model and receive a matrix output with estimates of acceleration for each combination of wt and hp
    mtrx3d <-  predict(data.loess, newdata = data.fit)
    # Abbreviated display of final matrix
    mtrx3d[1:4, 1:4]
    mtrx.melt <- melt(mtrx3d, id.vars = c('dist.dyn', 'regen.dyn'), measure.vars = 'value')
    names(mtrx.melt) <- c('dist.dyn', 'regen.dyn', 'value')
    # Return data to numeric form
    mtrx.melt$dist.dyn <- as.numeric(str_sub(mtrx.melt$dist.dyn, str_locate(mtrx.melt$dist.dyn, '=')[1,1] + 1))
    mtrx.melt$regen.dyn <-  as.numeric(str_sub(mtrx.melt$regen.dyn, str_locate(mtrx.melt$regen.dyn, '=')[1,1] + 1))
    mtrx.melt <- mtrx.melt %>% mutate(value = ifelse(value > 100, 100, 
                                                     ifelse(value < 0, 0, value))) %>% 
      rename(Change = value)
    p1<- plot_ly(mtrx.melt, x = ~dist.dyn, y = ~regen.dyn, z = ~Change, type = "contour",
                 colors = "Spectral", reversescale=T, zmin=0, zmax=100, ncontours=21) %>% 
      layout(title = paste0(clim, ": ", names(response.colors)[i]), 
             xaxis = list(title = 'Disturbance rate [log10-transformed, % yr^-1]',
                          ticktext = c(10^c(-3:1),100),
                          tickvals = c(-3:1, log10(100))),
             yaxis = list(title = 'Regeneration rate [recruited ha^-1 yr^-1]',
                          tickvals = c(1:5*10))) %>% 
      colorbar(title="Landscape changed [%]"); p1
    save_image(p1, file = paste0("results/figures/suppl_figures/Q2_contourPlot_loess_", i, "_", clim, ".png"), scale=1, 
                 width=800, height=700) 
    rm(p1, data.loess, a, mtrx.melt, xgrid, ygrid, data.fit, mtrx3d)
  }
}


## categorical regeneration rate plots ####
dyn.df <- read_csv("results/datasets/dyn.df.csv")

ranges.dist <- c(range(dist.dyn.df[dist.dyn.df$landscape=="stoko", "dist.dyn"]),
                 range(dist.dyn.df[dist.dyn.df$landscape=="bgd", "dist.dyn"]),
                 range(dist.dyn.df[dist.dyn.df$landscape=="grte", "dist.dyn"]))
i<-1; clim <- "baseline"
for (clim in c("baseline")) { #, "hotdry"
  for (i in 1:3) {
    png(paste0("results/figures/suppl_figures/Q2_responseLine_disturbanceRate_categorical_", i, "_", clim, ".png"), res=250,
        height=1300, width=2000)
    p <- dyn.df %>%
      filter(climate == clim,
             name == names(response.colors)[i]) %>%
      mutate(regen.cat = ifelse(regen.dyn <= quantile(regen.dyn, 0.333), "low",
                                ifelse(regen.dyn > quantile(regen.dyn, 0.333) & regen.dyn <= quantile(regen.dyn, 0.667), "intermediate",
                                       ifelse(regen.dyn > quantile(regen.dyn, 0.667), "high", NA))),
             regen.cat = factor(regen.cat, levels=c("low", "intermediate", "high"))) %>%
      ggplot(aes(x = dist.dyn, y = 100-value*100, col = regen.cat)) +
      geom_point(size = 0.1, alpha = 0.2, col="black") +
      geom_smooth(se = FALSE, method="loess") +
      scale_x_log10(breaks = c(0.0001, 0.001, 0.01, 0.1, 1, 10, 50)*2, # year 1:80 *0.5, 1:10 *2, 1:5 *5, 1:2 *10
                    label = c(0.0001, 0.001, 0.01, 0.1, 1, 10, 50)*2) +
      ylim(0, 100) +
      labs(y = "Landscape changed [%]", col = "Regeneration rate", title=paste0(names(response.colors)[i]),
           x = paste0("Simulated disturbance rate [% yr^-1]\nAxis log10-transformed")) +
      theme_bw() +
      coord_cartesian(clip="off") +
      annotate("text", x = mean(ranges.dist[1:2]), y = 100-55, label = "Shiretoko") +
      annotate("segment", x = ranges.dist[1], xend = ranges.dist[2], y = 50, yend = 50) +
      annotate("text", x = mean(ranges.dist[3:4]), y = 100-45, label = "Berchtesgaden") +
      annotate("segment", x = ranges.dist[3], xend = ranges.dist[4], y = 60, yend = 60) +
      annotate("text", x = mean(ranges.dist[5:6]), y = 100-35, label = "Grand Teton") +
      annotate("segment", x = ranges.dist[5], xend = ranges.dist[6], y = 70, yend = 70) +
      theme(legend.position = "top"); print(p)
    dev.off(); rm(p)
  }
}





## spatial pattern ####

### data ####
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
    group_by(landscape, step, climate, name, rep) %>% # , size, freq, browsing, fecundity
    summarise(chances = n(), # total number of rids in this elevation step (varies between elevation steps)
              changed = sum(value), # total number of rids that experienced change per elevation step
              pct = changed/chances*100) %>% ungroup() # pct of rids that experienced change per elevation step
  elev.df <- bind_rows(elev.df, df.i); rm(df.i, map.df)
  if (landscape_i == 3) write_csv(elev.df, "results/datasets/elev.df.csv")
  print(landscapename); rm(landscapename)
}; gc()



### plots ####
elev.df <- read_csv("results/datasets/elev.df.csv") 

png("results/figures/suppl_figures/Q2_distanceFromTreeline_allLandscapes_simplified_baselineClimate.png", res=200,
    height=1000, width=2000)
elev.df %>% 
  filter(climate == "baseline") %>% 
  mutate(step = ifelse(step <= 500, "0-500",
                       ifelse(step > 500 & step <= 1000, "501-1000",
                              ifelse(step > 1000, ">1000", step))),
         step = factor(step, levels=rev(c("0-500", "501-1000", ">1000")))) %>% #summary()
  group_by(rep, step, landscape, name) %>%
  summarise(pct.sd = sd(pct),
            pct = mean(pct)) %>% ungroup() %>% 
  group_by(step, landscape, name) %>%
  summarise(pct = mean(pct), pct.sd = mean(pct.sd)) %>% ungroup() %>% 
  mutate(landscape = case_match(landscape, "bgd"~"Berchtesgaden", "stoko"~"Shiretoko", "grte"~"Grand Teton"),
         landscape = factor(landscape, levels=c("Shiretoko", "Berchtesgaden", "Grand Teton"))) %>% 
  ggplot(aes(x=landscape, y=pct, fill=step)) +
  geom_bar(stat="identity", position = position_dodge()) +
  geom_errorbar(aes(ymin=pct-pct.sd, ymax=pct+pct.sd, group=step), 
                position = position_dodge2(), color="grey7") +
  scale_fill_manual(values=c(">1000"="#668858",
                             "501-1000"= "#983520",
                             "0-500"="#938f8e")) +
  facet_wrap(~name, scales="free") +
  labs(x="", y="Percentage changed [%]", fill="Distance from treeline") +
  theme_bw() +
  theme(legend.position = "bottom")
dev.off()

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

# landscape_i <- 1
# for (landscape_i in 1:3) {
#   landscapename <- landscapes[landscape_i]
  
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
  
  ### Endpoint maps ####
  
#   endpoint <- readRDS(paste0("results/datasets/map_endpoints_",landscapename,".RDATA"))
#   
#   # basal area
#   png(paste0("results/figures/Q3_map_1_basalArea_", landscapename ,".png"), res=200,
#       height=1600, width=2000)
#   print(
#     endpoint %>% 
#       dplyr::select(-basal_ref, -count_sum_c, -count_sum_ref, -dom_c, -dom_ref, -basal_diff) %>% 
#       group_by(rid, climate, size, freq, browsing, fecundity, landscape) %>% #everything but rep
#       summarise(basal_c=mean(basal_c)) %>% ungroup() %>% 
#       mutate(identifier = paste("size", size, "frequency", freq, "\nbrowsing", browsing, "fecundity", fecundity)) %>% 
#       full_join(rid.df[rid.df$landscape==landscapename,]) %>% 
#       filter(!is.na(size)) %>% 
#       mutate(basal_c = ifelse(basal_c > quantile(basal_c, 0.9), quantile(basal_c, 0.9), basal_c)) %>% 
#       ggplot(aes(x=x, y=y, fill=basal_c)) +
#       geom_tile() +
#       coord_equal() +
#       scale_fill_distiller(palette="YlGnBu", direction=1) +
#       facet_grid(climate~identifier) +
#       labs(x="", y="", fill = "Mean basal area [m^2 ha^-1]") + 
#       theme_bw() +
#       theme(legend.position = "bottom"))
#   dev.off()
#   
#   # dominant species
#   spec.col <- c(helper.files[[landscapename]][["species"]] %>% filter(species %in% common.species[[landscapename]]) %>% arrange(species) %>% mutate(displayColor = paste0("#", displayColor)) %>% pull(displayColor),
#                 "#808080")
#                 names(spec.col)<- c(sort(common.species[[landscapename]]), "Other")
#                 if (landscapename=="grte") {
#                   spec.col[1] <- "#013220"; spec.col[2] <- "#00FF00"
#                 }
#                 if (landscapename=="stoko") {
#                   spec.col[1] <- "#0504aa"; spec.col[2] <- "#33ff49"; spec.col[3] <- "#FFA500"; spec.col[6] <- "#A020F0"; spec.col[8] <- "#fe019a" 
#                 }
#                 
#                 png(paste0("results/figures/Q3_map_2_dominantSpecies_", landscapename ,".png"), res=200,
#                     height=1600, width=2000)
#                 print(
#                   endpoint %>% 
#                     dplyr::select(-dom_ref, -count_sum_c, -count_sum_ref, -basal_c, -basal_ref, -basal_diff) %>% 
#                     group_by(rid, climate, size, freq, browsing, fecundity, landscape, dom_c) %>% #everything but rep
#                     mutate(species.count = n()) %>% ungroup() %>% 
#                     group_by(rid, climate, size, freq, browsing, fecundity, landscape) %>% 
#                     filter(species.count==max(species.count)) %>% 
#                     dplyr::select(-rep, -identifier) %>% distinct() %>% 
#                     mutate(n.row = n(),
#                            dom_c = ifelse(n.row > 1, sample(unique(dom_c), 1), dom_c)) %>% 
#                     dplyr::select(-species.count, -n.row) %>% ungroup() %>% distinct() %>% 
#                     mutate(identifier = paste("size", size, "frequency", freq, "\nbrowsing", browsing, "fecundity", fecundity)) %>% 
#                     full_join(rid.df[rid.df$landscape==landscapename,]) %>% 
#                     filter(!is.na(size)) %>% 
#                     mutate(dom_c = ifelse(dom_c %in% common.species[[landscapename]], dom_c, "Other")) %>% 
#                     ggplot(aes(x=x, y=y, fill=dom_c)) +
#                     geom_tile() +
#                     coord_equal() +
#                     scale_fill_manual(values=spec.col) +
#                     facet_grid(climate~identifier) +
#                     labs(x="", y="", fill = "Dominant species") + 
#                     theme_bw() +
#                     theme(legend.position = "bottom"))
#                 dev.off()
#                 
#                 # types of change
#                 png(paste0("results/figures/Q3_map_2_dominantSpecies_typesOfChange_", landscapename ,".png"), res=200,
#                     height=1600, width=2000)
#                 print(
#                   endpoint %>% 
#                     dplyr::select(-count_sum_c, -count_sum_ref, -basal_c, -basal_ref, -basal_diff) %>% 
#                     filter(identifier %ni% c(paste0("baseline_rep",1:5,"_size1_freq1_browsing1_fecundity100"))) %>% 
#                     group_by(climate, size, freq, browsing, fecundity, dom_c, dom_ref) %>% 
#                     summarise(count = n()) %>% ungroup() %>% 
#                     dplyr::select(climate, size, freq, browsing, fecundity, dom_ref, dom_c, count) %>% 
#                     filter(dom_ref != dom_c) %>% 
#                     mutate(identifier = paste("size", size, "frequency", freq, "\nbrowsing", browsing, "fecundity", fecundity),
#                            species.change = paste(dom_ref, "to", dom_c)) %>% 
#                     group_by(identifier, climate) %>% 
#                     slice_max(count, n = 5) %>% 
#                     ggplot(aes(x=reorder(species.change, count), y=count/5/areas[landscape_i]*100, fill=identifier)) + # count: divided by 5 to get mean over all 5 reps, divided by landscape area to get % of landscape, *100 for %
#                     geom_bar(stat="identity", position = position_dodge()) +
#                     facet_grid(~climate) +
#                     scale_fill_brewer(palette = "Spectral", direction=-1) +
#                     coord_flip() +
#                     labs(x="Type of change", y="Mean percentage of full landscape [%]", fill = "Scenario") + 
#                     theme_bw() +
#                     theme(legend.position = "bottom")
#                 )
#                 dev.off()
#                 
#                 # forest loss
#                 png(paste0("results/figures/Q3_map_3_forestLoss_", landscapename ,".png"), res=200,
#                     height=1600, width=2000)
#                 print(
#                   endpoint %>% 
#                     dplyr::select(-basal_c, -basal_ref, -dom_c, -dom_ref, -basal_diff) %>% 
#                     group_by(rid, climate, size, freq, browsing, fecundity, landscape) %>% #everything but rep
#                     summarise(count_sum_c=mean(count_sum_c),
#                               count_sum_ref=mean(count_sum_ref)) %>% ungroup() %>% 
#                     mutate(identifier = paste("size", size, "frequency", freq, "\nbrowsing", browsing, "fecundity", fecundity)) %>% 
#                     full_join(rid.df[rid.df$landscape==landscapename,]) %>% 
#                     filter(!is.na(size)) %>% 
#                     mutate(forest.loss = as.factor(ifelse(count_sum_c < 50 & count_sum_ref >= 50, "Non-Forest", "Forest"))) %>% 
#                     ggplot(aes(x=x, y=y, fill=forest.loss)) +
#                     geom_tile() +
#                     coord_equal() +
#                     scale_fill_manual(values=c("Non-Forest" = "#c51b7d", "Forest" = "#7fbc41")) +
#                     facet_grid(climate~identifier) +
#                     labs(x="", y="", fill = "Forest Loss") + 
#                     theme_bw() +
#                     theme(legend.position = "bottom"))
#                 dev.off()
#                 
#                 rm(endpoint, spec.col)
#                 
#                 print("finished endpoint maps")
# }

# ### by cc scenario ####
# landscape_i <- 3
# for (landscape_i in 1:3) {
#   landscapename <- landscapes[landscape_i]
#   png(paste0("results/figures/Q4_distanceFromTreeline_", landscapename ,".png"), res=200,
#       height=1300, width=2000)
#   p1 <- elev.df %>% 
#     filter(landscape == landscapename) %>% 
#     group_by(step, climate, name) %>%
#     summarise(pct = mean(pct)) %>% ungroup() %>% 
#     ggplot(aes(x=step, y=pct, col = climate)) +
#     geom_point(size=0.6, alpha=0.7) +
#     geom_smooth(se=F, method="loess") +
#     scale_x_reverse() +
#     scale_color_manual(values=c("hotdry" = "firebrick2", "baseline"="royalblue3")) +
#     facet_grid(~name) +
#     labs(x="Distance from tree line [m]", y="Percentage changed [%]", title = toupper(landscapename)) +
#     theme_bw(); print(p1)
#   dev.off(); rm(p1)
#   png(paste0("results/figures/Q4_distanceFromTreeline_", landscapename ,"_byReplicate.png"), res=200,
#       height=1600, width=2000)
#   p2 <- elev.df  %>% 
#     filter(landscape == landscapename) %>% 
#     ggplot(aes(y=pct, col=climate)) +
#     geom_boxplot(aes(x=as.factor(step)), size=0.6, alpha=0.7) +
#     scale_color_manual(values=c("hotdry" = "firebrick2", "baseline"="royalblue3")) +
#     facet_grid(~name) +
#     labs(x="Distance from tree line [m]", y="Percentage changed [%]", title = toupper(landscapename)) +
#     theme_bw(); print(p2)
#   dev.off(); rm(p2)
#   print(landscapename); rm(landscapename)
# }
# 
# ### all landscapes ####
# png("results/figures/Q4_distanceFromTreeline_allLandscapes.png", res=200,
#     height=1200, width=2000)
# elev.df  %>% 
#   group_by(step, landscape, name) %>%
#   summarise(pct = mean(pct)) %>% ungroup() %>% 
#   mutate(landscape = case_match(landscape, "bgd"~"Berchtesgaden", "stoko"~"Shiretoko", "grte"~"Grand Teton"),
#          landscape = factor(landscape, levels=c("Shiretoko", "Berchtesgaden", "Grand Teton"))) %>% 
#   ggplot(aes(x=step, y=pct, col = landscape)) +
#   geom_point(size=0.6, alpha=0.7) +
#   geom_smooth(se=F, method="loess") +
#   scale_x_reverse() +
#   scale_color_manual(values=colors.landscape) +
#   facet_grid(~name) +
#   labs(x="Distance from tree line [m]", y="Percentage changed [%]", col="Landscape") +
#   theme_bw()
# dev.off()
# 
# png("results/figures/suppl_figures/distanceFromTreeline_allLandscapes_scaled.png", res=200,
#     height=1200, width=2000)
# elev.df  %>% 
#   group_by(step, landscape, name) %>%
#   summarise(pct = mean(pct)) %>% ungroup() %>% 
#   group_by(landscape, name) %>% 
#   mutate(pct_scaled = pct/mean(pct),
#          pct_cum = cumsum(pct_scaled)) %>% ungroup() %>% 
#   mutate(landscape = case_match(landscape, "bgd"~"Berchtesgaden", "stoko"~"Shiretoko", "grte"~"Grand Teton"),
#          landscape = factor(landscape, levels=c("Shiretoko", "Berchtesgaden", "Grand Teton"))) %>% 
#   ggplot(aes(x=step, y=pct_scaled, col = landscape)) +
#   geom_point(size=0.6, alpha=0.7) +
#   geom_smooth(se=F, method="loess") +
#   scale_x_reverse() +
#   scale_color_manual(values=colors.landscape) +
#   facet_grid(~name) +
#   labs(x="Distance from tree line [m]", y="Percentage changed [%]", col="Landscape") +
#   theme_bw()
# dev.off()

