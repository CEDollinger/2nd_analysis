# Methods ####

## sensitivity analysis for basal area decline threshold ####
# compare %landscape changed in year 80 between 5 different threshold levels for (3 landscapes x 3 scenarios x 5 levels)
# figure: 3x3 panels with 5 bars each -> mean of pct landscape changed, SD over reps as errorbar

thresh.df <- read_csv("results/datasets/thresh.df.csv")
png(paste0("results/figures/suppl_figures/methods_structureChange_5thresholds.png"), res=250,
    height=1300, width=2000)
thresh.df %>% 
  mutate(fecundity= ifelse(fecundity==50, 2, ifelse(fecundity==20, 5, 10)),
         scenario = paste0("size*", size, "; freq*", freq, "; seed production/", fecundity, "\nsapling height growth limitations*", browsing),
         scenario = case_match(scenario, 
                               "size*2; freq*2; seed production/2\nsapling height growth limitations*2" ~ "Lowest levels of modification",
                               "size*5; freq*5; seed production/5\nsapling height growth limitations*5" ~ "Intermediate levels of modification",
                               "size*10; freq*10; seed production/10\nsapling height growth limitations*10" ~ "Highest levels of modification"),
         scenario = factor(scenario, levels=c("Lowest levels of modification",  "Intermediate levels of modification", "Highest levels of modification")),
         landscape = case_match(landscape, "bgd"~"Berchtesgaden", "stoko"~"Shiretoko", "grte"~"Grand Teton"),
         landscape = factor(landscape, levels=c("Shiretoko", "Berchtesgaden", "Grand Teton"))) %>% 
  ggplot(aes(x=thresh, y=prop*100)) +
  geom_bar(aes(fill=thresh), stat="identity", show.legend = F, col="black") +
  geom_point(aes(x=rep(-50, nrow(thresh.df)), y=rep(8, nrow(thresh.df))), shape=8) +
  geom_errorbar(aes(ymax=(prop+sd)*100, ymin=(prop-sd)*100), width = 4, show.legend = F, col="black") +
  facet_grid(landscape~scenario) +
  #scale_linewidth_manual(values=c("yes"=1, "no"=.5)) +
  scale_fill_distiller(palette = "RdPu", direction=1) +
  scale_x_continuous(breaks=c(-0.8, -0.65, -0.5, -0.35, -0.2)*100) +
  labs(y="Landscape changed [%]", x="Threshold for defining structural decline\n[basal area decline from reference, %]") +
  theme_bw()
# errorbar: mean +/- SD over all 5 reps
dev.off()


# Q2 ####

## contour for every landscape ####
dyn.df <- read_csv("results/datasets/dyn.df.csv")

i<-1; lscp<-"bgd"
for (lscp in landscapes) { #, "hotdry"
  for (i in 1:3) {
    a <- dyn.df %>% 
      filter(name == names(response.colors)[i], 
             landscape==lscp) %>% 
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
      layout(# title = paste0(lscp, ": ", names(response.colors)[i]), 
        xaxis = list(title = 'Disturbance rate [log10-transformed, % yr^-1]',
                     ticktext = c(10^c(-3:1),100),
                     tickvals = c(-3:1, log10(100))),
        yaxis = list(title = 'Recruitment rate [recruited ha^-1 yr^-1]',
                     tickvals = c(1:5*10))) %>% 
      colorbar(title="Change [%]", limits = c(0, 100),
               titlefont = list(size = 50), tickfont = list(size = 50)); p1
    save_image(p1, file = paste0("results/figures/suppl_figures/Q2_contourPlot_loess_singleLandscapes_", 
                                 i, "_", lscp, ".png"), scale=1, 
               width=800, height=700) 
    rm(p1, data.loess, a, mtrx.melt, xgrid, ygrid, data.fit, mtrx3d)
  }
}


## categorical recruitment rate plots ####
dyn.df <- read_csv("results/datasets/dyn.df.csv")

ranges.dist <- c(range(dyn.df[dyn.df$landscape=="stoko", "dist.dyn"]),
                 range(dyn.df[dyn.df$landscape=="bgd", "dist.dyn"]),
                 range(dyn.df[dyn.df$landscape=="grte", "dist.dyn"]))
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
      labs(y = "Landscape changed [%]", col = "Recruitment rate", title=paste0(names(response.colors)[i]),
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
  summarise(pct = mean(pct)) %>% ungroup() %>%
  group_by(step, landscape, name) %>%
  summarise(pct.sd = sd(pct), pct = mean(pct)) %>% ungroup() %>% 
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
  labs(x="", y="Landscape changed [%]", fill="Distance from treeline") +
  theme_bw() +
  theme(legend.position = "bottom")
dev.off()


# Q3 ####

## contour plots for absolute forest change under hotdry runs ####
i<-1; clim <- "baseline"
for (i in 1:3) {
  a <- dyn.effect %>% 
    filter(name == names(response.colors)[i]) %>% 
    dplyr::select(dist.dyn_baseline, regen.dyn_baseline, hotdry, landscape) %>% 
    rename(dist.dyn = 1,
           regen.dyn = 2) %>% 
    mutate(dist.dyn = log10(dist.dyn))
  
  (hulls <- a %>%
      group_by(landscape) %>%
      do(create_hull(.)) %>% 
      mutate(landscape = case_match(landscape, "bgd"~"Berchtesgaden", "stoko"~"Shiretoko", "grte"~"Grand Teton"),
             landscape = factor(landscape, levels=c("Shiretoko", "Berchtesgaden", "Grand Teton")))) 
  
  # Convert your hulls to an sf object for geometric operations
  hulls_sf <- st_as_sf(hulls, coords = c("dist.dyn", "regen.dyn"), crs = 4326)
  # Calculate the convex hull for each landscape if not already done
  hulls_geom <- hulls_sf %>%
    mutate(abbrev = ifelse(landscape == "Shiretoko", "STK",
                           ifelse(landscape == "Berchtesgaden", "BGD", "GTE"))) %>% 
    group_by(landscape, abbrev) %>%
    summarize(geometry = st_combine(geometry)) %>%
    st_convex_hull()  # Compute the convex hull of each landscape
  # Calculate the centroid of each landscape's convex hull
  centroids <- st_centroid(hulls_geom)
  # Extract the coordinates of the centroids
  centroid_coords <- st_coordinates(centroids)
  # Add these centroids to the hulls_geom object for later plotting
  hulls_geom$centroid_x <- centroid_coords[, 1]
  hulls_geom$centroid_y <- centroid_coords[, 2]
  
  data.loess <- loess(hotdry ~ dist.dyn * regen.dyn, data = a)
  summary(data.loess)
  # Create a sequence of incrementally increasing (by 0.3 units) values for both wt and hp
  xgrid <- seq(min(a$dist.dyn), max(a$dist.dyn), length.out=100)
  ygrid <- seq(min(a$regen.dyn), max(a$regen.dyn), length.out=100)
  # Generate a dataframe with every possible combination of wt and hp
  data.fit <-  expand.grid(dist.dyn = xgrid, regen.dyn = ygrid)
  # Feed the dataframe into the loess model and receive a matrix output with estimates of acceleration for each combination of wt and hp
  mtrx3d <-  predict(data.loess, newdata = data.fit)
  # Abbreviated display of final matrix
  mtrx3d[1:4, 1:4]
  mtrx.melt <- melt(mtrx3d, id.vars = c('dist.dyn', 'regen.dyn'), measure.vars = 'hotdry')
  names(mtrx.melt) <- c('dist.dyn', 'regen.dyn', 'hotdry')
  # Return data to numeric form
  mtrx.melt$dist.dyn <- as.numeric(str_sub(mtrx.melt$dist.dyn, str_locate(mtrx.melt$dist.dyn, '=')[1,1] + 1))
  mtrx.melt$regen.dyn <-  as.numeric(str_sub(mtrx.melt$regen.dyn, str_locate(mtrx.melt$regen.dyn, '=')[1,1] + 1))
  head(mtrx.melt)
  
  p1 <- plot_ly(mtrx.melt, x = ~dist.dyn, y = ~regen.dyn, z = ~hotdry, type = "contour",  contours = list(showlines = FALSE),
                colors = "Spectral", reversescale=T, zmin=0, zmax=100, ncontours=21, opacity = 1) %>% 
    layout(#title = names(response.colors)[i], 
      xaxis = list(title = 'Disturbance rate [log10-transformed, % yr^-1]',
                   zerolinecolor=toRGB("grey93"),
                   ticktext = c(10^c(-3:1),100),
                   tickvals = c(-3:1, log10(100)),
                   titlefont = list(size = 50), tickfont = list(size = 50)),
      yaxis = list(title = 'Recruitment rate [recruited ha^-1 yr^-1]',
                   tickvals = c(1:5*10),
                   range=range(mtrx.melt$regen.dyn, na.rm = TRUE),
                   titlefont = list(size = 50), tickfont = list(size = 50))) %>% 
    colorbar(title="Change [%]", limits = c(0, 100),
             titlefont = list(size = 50), tickfont = list(size = 50)); p1
  p2 <- p1 %>%
    add_trace(data = hulls, x = ~dist.dyn, y = ~regen.dyn,
              mode = "lines", type = "scatter",
              line = list(width = 2),  # Line width
              color = ~factor(landscape),
              colors="grey30",
              # colors = c("#ffa214", "#7fff41", "#1d99f9"),
              inherit=F, showlegend=F); p2 
  p3 <- p2 %>%
    add_text(data = hulls_geom, inherit=F,
             x = ~centroid_x, y = ~centroid_y, text = ~abbrev,
             mode = "text", showlegend = FALSE,
             textfont = list(size = 100, bold=TRUE, color=toRGB("grey10"))); p3
  if (i == 1) {
    save_image(p3, file = paste0("results/figures/suppl_figures/Q3_contourPlot_loess_", i, "_hotdry.png"), scale=1, 
               width=1900, height=1700) 
  } else {
    save_image(p2, file = paste0("results/figures/suppl_figures/Q3_contourPlot_loess_", i, "_hotdry.png"), scale=1, 
               width=1900, height=1700) 
  } 
  rm(p1, p2, p3, data.loess, a, mtrx.melt, xgrid, ygrid, data.fit, mtrx3d, hulls, centroids, hulls_geom, hulls_sf, centroid_coords) #, p3
}

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


### maps ####

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

