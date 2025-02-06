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
        xaxis = list(title = 'Disturbance rate [% yr^-1]',
                     ticktext = c(10^c(-3:1),100),
                     tickvals = c(-3:1, log10(100))),
        yaxis = list(title = 'Regeneration rate [recruited ha^-1 yr^-1]',
                     tickvals = c(1:5*10))) %>% 
      colorbar(title="Change [%]", limits = c(0, 100),
               titlefont = list(size = 50), tickfont = list(size = 50)); p1
    save_image(p1, file = paste0("results/figures/suppl_figures/Q2_contourPlot_loess_singleLandscapes_", 
                                 i, "_", lscp, ".png"), scale=1, 
               width=800, height=700) 
    rm(p1, data.loess, a, mtrx.melt, xgrid, ygrid, data.fit, mtrx3d)
  }
}

## raw data ####

dyn.df <- read_csv("results/datasets/dyn.df.csv")
# Create a function to compute the convex hull for a set of points
create_hull <- function(data) {
  hull_indices <- chull(data$dist.dyn, data$regen.dyn)
  hull_indices <- c(hull_indices, hull_indices[1])  # Close the hull by connecting to the first point
  data[hull_indices, ]
}

a <- dyn.df %>% 
  filter(name == names(response.colors)[1],
         climate == "baseline") %>% 
  dplyr::select(dist.dyn, regen.dyn, value, landscape) %>% 
  mutate(value=100-value*100, dist.dyn = log10(dist.dyn))

(hulls <- a %>%  
    group_by(landscape) %>%
    do(create_hull(.)) %>% 
    rename(Change=value) %>% 
    mutate(landscape = case_match(landscape, "bgd"~"Berchtesgaden", "stoko"~"Shiretoko", "grte"~"Grand Teton"),
           landscape = factor(landscape, levels=c("Shiretoko", "Berchtesgaden", "Grand Teton"))))
# Berchtesgaden's and Grand Teton's hulls are calculated using the "concaveman" package
bgd.hull <- a %>% 
  mutate(dist.dyn = 10^dist.dyn) %>% 
  filter(landscape == "bgd") %>% 
  dplyr::select(1:2) %>% as.matrix() %>% 
  concaveman(length_threshold=10) %>% 
  as.data.frame() %>% 
  rename(dist.dyn = V1, regen.dyn = V2) %>% 
  mutate(dist.dyn = log10(dist.dyn))
grte.hull <- a %>% 
  mutate(dist.dyn = 10^dist.dyn) %>% 
  filter(landscape == "grte") %>% 
  dplyr::select(1:2) %>% as.matrix() %>% 
  concaveman(length_threshold=100) %>% 
  as.data.frame() %>% 
  rename(dist.dyn = V1, regen.dyn = V2) %>% 
  mutate(dist.dyn = log10(dist.dyn))
#### fit loess model and predict 
data.loess <- loess(value ~ dist.dyn * regen.dyn, data = a)
# Create a sequence of incrementally increasing (by 0.3 units) values for both wt and hp
xgrid <- seq(min(a$dist.dyn), max(a$dist.dyn), length.out=100)
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
  rename(Change = value); head(mtrx.melt)
p1 <- plot_ly(mtrx.melt, x = ~dist.dyn, y = ~regen.dyn, z = ~Change, type = "contour",  contours = list(showlines = FALSE),
              colors = "Spectral", reversescale=T, zmin=0, zmax=100, ncontours=21, opacity = 1) %>% 
  layout( 
    xaxis = list(title = 'Disturbance rate [% yr^-1]',
                 zerolinecolor=toRGB("grey93"),
                 ticktext = c(10^c(-3:1),100),
                 tickvals = c(-3:1, log10(100)),
                 titlefont = list(size = 50), tickfont = list(size = 50)),
    yaxis = list(title = 'Regeneration rate [recruited ha^-1 yr^-1]',
                 tickvals = c(1:5*10),
                 range=range(mtrx.melt$regen.dyn, na.rm = TRUE),
                 titlefont = list(size = 50), tickfont = list(size = 50))) %>% 
  colorbar(title="Change [%]", limits = c(0, 100),
           titlefont = list(size = 50), tickfont = list(size = 50)); p1
p2 <- p1 %>%
  add_trace(data=bgd.hull, x = ~dist.dyn, y = ~regen.dyn,
            mode = "lines", type = "scatter",
            line = list(width = 2),  # Line width
            color = ~factor(landscape),
            colors="grey30",
            inherit=F, showlegend=F) %>% 
  add_trace(data=grte.hull, x = ~dist.dyn, y = ~regen.dyn,
            mode = "lines", type = "scatter",
            line = list(width = 2),  # Line width
            color = ~factor(landscape),
            inherit=F, showlegend=F) %>% 
  add_trace(data=hulls[hulls$landscape=="Shiretoko",], x = ~dist.dyn, y = ~regen.dyn,
            mode = "lines", type = "scatter",
            line = list(width = 2),  # Line width
            color = ~factor(landscape),
            inherit=F, showlegend=F); p2
p3 <- p2 %>% 
  add_trace(data = a, x = ~dist.dyn, y = ~regen.dyn,
            mode = "markers", type = "scatter", color = ~factor(landscape),
            showlegend=F, inherit=F,
            marker = list(size = 15)); p3
save_image(p3, file = paste0("results/figures/suppl_figures/Q2_contourPlot_loess_rawData.png"), scale=1, 
           width=1900, height=1700) 

## inset figure ####
p4 <- plot_ly(mtrx.melt, x = ~dist.dyn, y = ~regen.dyn, z = ~Change, type = "contour",  contours = list(showlines = FALSE),
              colors = "white", reversescale=T, zmin=0, zmax=100, ncontours=21, opacity = 1)%>% 
  colorbar(title="Change [%]",
           titlefont = list(size = 10), tickfont = list(size = 10)); p4
p5 <- p4 %>%
  add_trace(data=bgd.hull, x = ~dist.dyn, y = ~regen.dyn,
            mode = "lines", type = "scatter",
            line = list(width = 10),  # Line width
            color = ~factor(landscape),
            colors="grey30",
            # colors = c("#ffa214", "#7fff41", "#1d99f9"),
            inherit=F, showlegend=F) %>% 
  add_trace(data=grte.hull, x = ~dist.dyn, y = ~regen.dyn,
            mode = "lines", type = "scatter",
            line = list(width = 10),  # Line width
            color = ~factor(landscape),
            colors="grey30",
            # colors = c("#ffa214", "#7fff41", "#1d99f9"),
            inherit=F, showlegend=F) %>% 
  add_trace(data=hulls[hulls$landscape=="Shiretoko",], x = ~dist.dyn, y = ~regen.dyn,
            mode = "lines", type = "scatter",
            line = list(width = 10),  # Line width
            color = ~factor(landscape),
            colors="grey30",
            # colors = c("#ffa214", "#7fff41", "#1d99f9"),
            inherit=F, showlegend=F); p5
save_image(p5, file = paste0("results/figures/Q2_contourPlot_inset.png"), scale=1, 
           width=1900, height=1700) 
rm(dyn.df, p1, p2, p3, p4, p5, create_hull, data.loess, a, mtrx.melt, xgrid, ygrid, data.fit, mtrx3d, hulls, bgd.hull, grte.hull)


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
dyn.effect <- read_csv("results/datasets/dyn.effect.csv")
## contour plots for absolute forest change under hotdry runs ####
i<-1
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
  # Berchtesgaden's and Grand Teton's hulls are calculated using the "concaveman" package
  bgd.hull <- a %>% 
    mutate(dist.dyn = 10^dist.dyn) %>% 
    filter(landscape == "bgd") %>% 
    dplyr::select(1:2) %>% as.matrix() %>% 
    concaveman(length_threshold=10) %>% 
    as.data.frame() %>% 
    rename(dist.dyn = V1, regen.dyn = V2) %>% 
    mutate(dist.dyn = log10(dist.dyn))
  grte.hull <- a %>% 
    mutate(dist.dyn = 10^dist.dyn) %>% 
    filter(landscape == "grte") %>% 
    dplyr::select(1:2) %>% as.matrix() %>% 
    concaveman(length_threshold=100) %>% 
    as.data.frame() %>% 
    rename(dist.dyn = V1, regen.dyn = V2) %>% 
    mutate(dist.dyn = log10(dist.dyn))
  
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
      xaxis = list(title = 'Disturbance rate [% yr^-1]',
                   zerolinecolor=toRGB("grey93"),
                   ticktext = c(10^c(-3:1),100),
                   tickvals = c(-3:1, log10(100)),
                   titlefont = list(size = 50), tickfont = list(size = 50)),
      yaxis = list(title = 'Regeneration rate [recruited ha^-1 yr^-1]',
                   tickvals = c(1:5*10),
                   range=range(mtrx.melt$regen.dyn, na.rm = TRUE),
                   titlefont = list(size = 50), tickfont = list(size = 50))) %>% 
    colorbar(title="Change [%]", limits = c(0, 100),
             titlefont = list(size = 50), tickfont = list(size = 50)); p1
  p2 <- p1 %>%
    add_trace(data=bgd.hull, x = ~dist.dyn, y = ~regen.dyn,
              mode = "lines", type = "scatter",
              line = list(width = 2),  # Line width
              color = ~factor(landscape),
              colors="grey30",
              # colors = c("#ffa214", "#7fff41", "#1d99f9"),
              inherit=F, showlegend=F) %>% 
    add_trace(data=grte.hull, x = ~dist.dyn, y = ~regen.dyn,
              mode = "lines", type = "scatter",
              line = list(width = 2),  # Line width
              color = ~factor(landscape),
              colors="grey30",
              # colors = c("#ffa214", "#7fff41", "#1d99f9"),
              inherit=F, showlegend=F) %>% 
    add_trace(data=hulls[hulls$landscape=="Shiretoko",], x = ~dist.dyn, y = ~regen.dyn,
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
  rm(p1, p2, p3, data.loess, a, mtrx.melt, xgrid, ygrid, data.fit, mtrx3d, hulls, bgd.hull, grte.hull, centroids, hulls_geom, hulls_sf, centroid_coords) #, p3
}

