# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Q1: how important are the individual processes? ###########################################################################################################################################
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
singleProcess.df <- read_csv("results/datasets/singleProcess.df.csv") %>% 
  mutate(landscape = factor(landscape, levels=c("Shiretoko", "Berchtesgaden", "Grand Teton")))

singleProcess.mean <- singleProcess.df %>% 
  group_by(landscape, mod, name, process) %>% 
  summarise(value=mean(value)) %>% ungroup() %>% 
  mutate(type = ifelse(process %in% c("Disturbance size", "Disturbance frequency"), "Disturbance", "Regeneration"))

png("results/figures/Q1_singleProcesses.png", res=200,
    height=1600, width=2200)
singleProcess.df %>%
  filter(name != names(response.colors)[3]) %>% 
  ggplot(aes(x=as.numeric(mod), y=value, col=process)) +
  geom_line(aes(group=paste(rep, process)),
            linewidth=0.2, alpha=0.6) +
  geom_line(data=singleProcess.mean %>% filter(name != names(response.colors)[3]),
            linewidth=0.5, alpha=1) +
  geom_point(data=singleProcess.mean %>% filter(name != names(response.colors)[3]),
             size=3, show.legend = T) + #aes(shape=type)
  facet_grid(landscape~name, scales="free_y", switch = "y") +
  labs(y="Landscape changed [%]", x="Response level", col="Process", shape="Process") +
  scale_x_continuous(labels=c("Ref.", "*2", "*5", "*10"), breaks=c(1,2,5,10)) +
  scale_color_manual(values=c('Disturbance size' = "#b35806",
                              'Disturbance frequency' = "#f46d43",
                              'Seed production decrease'= "#542788",
                              'Sapling growth limitations'="#2166ac")) +
  theme_bw() +
  theme(legend.position = "top", 
        axis.title = ggplot2::element_text(size = 16),
        axis.text = ggplot2::element_text(size = 14),
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12),
        legend.title = ggplot2::element_text(size = 14, angle = 0),
        legend.text = ggplot2::element_text(size = 12),
        legend.box = 'horizontal',
        legend.box.margin = ggplot2::margin(0, 0, 20, 0))
dev.off()

# forest loss, for the suppl.
png("results/figures/suppl_figures/Q1_singleProcesses_forestloss.png", res=220,
    height=1200, width=1800)
singleProcess.df %>%
  filter(name == names(response.colors)[3]) %>% 
  ggplot(aes(x=as.numeric(mod), y=value, col=process)) +
  geom_line(aes(group=paste(rep, process)),
            linewidth=0.2, alpha=0.6) +
  geom_line(data=singleProcess.mean %>% filter(name == names(response.colors)[3]),
            linewidth=0.5, alpha=1) +
  geom_point(data=singleProcess.mean %>% filter(name == names(response.colors)[3]),
             size=3, show.legend = T) + #aes(shape=type)
  facet_grid(landscape~name, scales="free_y", switch = "y") +
  labs(y="Landscape changed [%]", x="Response level", col="Process", shape="Process") +
  scale_x_continuous(labels=c("Ref.", "*2", "*5", "*10"), breaks=c(1,2,5,10)) +
  scale_color_manual(values=c('Disturbance size' = "#b35806",
                              'Disturbance frequency' = "#f46d43",
                              'Seed production decrease'= "#542788",
                              'Sapling growth limitations'="#2166ac")) +
  theme_bw() +
  theme(legend.position = "top")
dev.off()

rm(singleProcess.df, singleProcess.mean)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Q2: how do disturbances and regeneration interact? ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

dyn.df <- read_csv("results/datasets/dyn.df.csv")

# baseline disturbance and regeneration rate by landscape
baseline.drivers <- dyn.df %>% 
  filter(climate == "baseline", size == 1, freq == 1, fecundity == 100, browsing == 1) %>% 
  group_by(landscape) %>% 
  summarise(dist = log10(mean(dist.dyn)),
            regen = mean(regen.dyn),
            Change = mean(value))

create_hull <- function(data) {
  hull_indices <- chull(data$dist.dyn, data$regen.dyn)
  hull_indices <- c(hull_indices, hull_indices[1])  # Close the hull by connecting to the first point
  data[hull_indices, ]
}

i<-1
for (i in 1:3) {
  a <- dyn.df %>% 
    filter(name == names(response.colors)[i],
           climate == "baseline") %>% 
    dplyr::select(dist.dyn, regen.dyn, value, landscape) %>% 
    mutate(value=100-value*100, 
           dist.dyn = log10(dist.dyn))
  print(summary(a))
  
  (hulls <- a %>%  
      group_by(landscape) %>%
      do(create_hull(.)) %>% 
      rename(Change=value) %>% 
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
  p3a <- p2 %>% 
    add_trace(data = baseline.drivers, x= ~dist, y= ~regen, # baseline driver rates by landscape
              mode = "markers", type = "scatter", name = "Reference", 
              marker = list(size = 45, color = "black", symbol = "circle")) %>%
    layout(legend = list(font = list(size = 50))); p3a
  p3b <- p2 %>%
    add_text(data = hulls_geom, inherit=F,
             x = ~centroid_x, y = ~centroid_y, text = ~abbrev,
             mode = "text", showlegend = FALSE,
             textfont = list(size = 100, bold=TRUE, color=toRGB("grey10"))); p3b
  if (i == 3) {
    save_image(p3b, file = paste0("results/figures/suppl_figures/Q2_contourPlot_loess_", i, "_baseline.png"), scale=1, 
               width=1900, height=1700) 
  } else if(i == 1) {
    save_image(p3a, file = paste0("results/figures/Q2_contourPlot_loess_", i, "_baseline.png"), scale=1, 
               width=1900, height=1700) 
  } else {
    save_image(p2, file = paste0("results/figures/Q2_contourPlot_loess_", i, "_baseline.png"), scale=1, 
               width=1900, height=1700) 
  }
  rm(p1, p2, p3a, p3b, data.loess, a, mtrx.melt, xgrid, ygrid, data.fit, mtrx3d, hulls, bgd.hull, grte.hull, centroids, hulls_geom, hulls_sf, centroid_coords)
}
rm(dyn.df, create_hull)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Q3: is there an interaction with climate? ###########################################################################################################################################
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

dyn.effect <- read_csv("results/datasets/dyn.effect.csv")

# Create a function to compute the convex hull for a set of points
create_hull <- function(data) {
  hull_indices <- chull(data$dist.dyn, data$regen.dyn)
  hull_indices <- c(hull_indices, hull_indices[1])  # Close the hull by connecting to the first point
  data[hull_indices, ]
}

mtrx.ls <- list(NA, NA, NA)
i<-1
for (i in 1:3) {
  a <- dyn.effect %>% 
    filter(name == names(response.colors)[i]) %>% 
    dplyr::select(dist.dyn_baseline, regen.dyn_baseline, effect, landscape) %>% 
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
  
  data.loess <- loess(effect ~ dist.dyn * regen.dyn, data = a)
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
  mtrx.melt <- melt(mtrx3d, id.vars = c('dist.dyn', 'regen.dyn'), measure.vars = 'effect')
  names(mtrx.melt) <- c('dist.dyn', 'regen.dyn', 'effect')
  # Return data to numeric form
  mtrx.melt$dist.dyn <- as.numeric(str_sub(mtrx.melt$dist.dyn, str_locate(mtrx.melt$dist.dyn, '=')[1,1] + 1))
  mtrx.melt$regen.dyn <-  as.numeric(str_sub(mtrx.melt$regen.dyn, str_locate(mtrx.melt$regen.dyn, '=')[1,1] + 1))
  head(mtrx.melt)
  mtrx.ls[[i]] <- mtrx.melt
  rm(mtrx.melt)
}

# create color scale centered on 0
# https://stackoverflow.com/questions/43505146/how-to-force-map-zero-to-white-color-in-r-plotly-color-scale
set.seed(42)
colorlength <- 100
null_value <- (0 - min(bind_rows(mtrx.ls)$effect)) / (max(bind_rows(mtrx.ls)$effect) - min(bind_rows(mtrx.ls)$effect))
border <- as.integer(null_value * colorlength)
colorscale <- as.list(1:colorlength)
if (border < colorlength / 2) {
  ## colorscale below zero
  border_neg <- colorlength - border
  s <- scales::seq_gradient_pal("#4d9221", "#FFFFFF", "Lab")(seq(0,1,length.out=border_neg))
  for (j in 1:border) {
    colorscale[[j]] <- c((j - 1) / colorlength, s[j + border_neg - border - 1])
  }
  
  ## colorscale above zero
  border_pos <- border
  s <- scales::seq_gradient_pal("#FFFFFF", "#c51b7d", "Lab")(seq(0,1,length.out=colorlength - border_pos))
  for (j in 1:(colorlength - border_pos)) {
    colorscale[[j + border_pos]] <- c((j + border) / colorlength, s[j])
  }
} else {
  ## colorscale below zero
  border_neg <- border
  s <- scales::seq_gradient_pal("#4d9221", "#FFFFFF", "Lab")(seq(0,1,length.out=border_neg))
  for (j in 1:border_neg) {
    colorscale[[j]] <- c((j - 1) / colorlength, s[j])
  }
  
  ## colorscale above zero
  border_pos <- colorlength - border
  s <- scales::seq_gradient_pal("#FFFFFF", "#c51b7d", "Lab")(seq(0,1,length.out=border))
  for (j in 1:border_pos) {
    colorscale[[j + border]] <- c((j + border) / colorlength, s[j])
  }
}; rm(j)


for (i in 1:3) {
  p1 <- plot_ly(mtrx.ls[[i]], x = ~dist.dyn, y = ~regen.dyn, z = ~effect, type = "contour",
                colorscale = colorscale, contours = list(showlines = FALSE)
                # colors = "PiYG", reversescale=T
  ) %>% 
    layout(#title = paste0(names(response.colors)[i]), 
      xaxis = list(title = 'Disturbance rate [log10-transformed, % yr^-1]',
                   zerolinecolor=toRGB("grey93"),
                   linecolor = "black",
                   linewidth = 0.5,
                   mirror = T,
                   ticktext = c(10^c(-3:1),100),
                   tickvals = c(-3:1, log10(100)),
                   titlefont = list(size = 50), tickfont = list(size = 50)),
      yaxis = list(title = 'Recruitment rate [recruited ha^-1 yr^-1]',
                   linecolor = "black",
                   linewidth = 0.5,
                   mirror = T,
                   tickvals = c(1:5*10),
                   range=range(mtrx.ls[[i]]$regen.dyn, na.rm = TRUE),
                   titlefont = list(size = 50), tickfont = list(size = 50))) %>% 
    colorbar(title = "Climate effect [%]", limits = c(min(bind_rows(mtrx.ls)$effect), max(bind_rows(mtrx.ls)$effect)),
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
  
  if (i == 3) {
    save_image(p2, file = paste0("results/figures/suppl_figures/Q3_contourPlot_loess_", i, "_climateEffect.png"), scale=1, 
               width=1900, height=1700)
  } else {
    save_image(p2, file = paste0("results/figures/Q3_contourPlot_loess_", i, "_climateEffect.png"), scale=1, 
               width=1900, height=1700)
    
  }
  
}; rm(p1, p2, data.loess, a, xgrid, ygrid, data.fit, mtrx3d, border, border_neg, border_pos, s, i, mtrx.ls, hulls, bgd.hull, grte.hull, colorscale,
      null_value, colorlength)

rm(dyn.effect, create_hull)

