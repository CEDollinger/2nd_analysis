# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Q2: is there an interaction with climate? ###########################################################################################################################################
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

overtime.ls <- readRDS("results/datasets/overtime.ls.RDATA")
dyn.df <- read_csv("results/datasets/dyn.df.csv")

# TO DOs
## 

dyn.effect <- dyn.df %>% 
  filter(climate=="baseline") %>% 
  dplyr::select(-climate, -identifier, -n_year) %>% 
  rename(baseline=value, 
         dist.dyn_baseline=dist.dyn,
         regen.dyn_baseline=regen.dyn) %>% 
  full_join(dyn.df %>% filter(climate=="hotdry") %>% 
              dplyr::select(-climate, -identifier, -n_year, -regen.dyn) %>% 
              rename(hotdry=value, 
                     dist.dyn_hotdry=dist.dyn)) %>% 
  mutate(baseline = 100-baseline*100, hotdry = 100-hotdry*100) %>% 
  mutate(effect = hotdry-baseline); summary(dyn.effect) # positive when hotdry > baseline -> amplifying effect
# Create a function to compute the convex hull for a set of points
create_hull_cc <- function(data) {
  hull_indices <- chull(data$dist.dyn_baseline, data$regen.dyn_baseline)
  hull_indices <- c(hull_indices, hull_indices[1])  # Close the hull by connecting to the first point
  data[hull_indices, ]
}


i<-1
for (i in 1:3) {
  a <- dyn.effect %>% 
    filter(name == names(response.colors)[i]) %>% 
    dplyr::select(dist.dyn_baseline, regen.dyn_baseline, effect, landscape) %>% 
    mutate(dist.dyn_baseline = log10(dist.dyn_baseline))
  
  (hulls <- a %>%
      group_by(landscape) %>%
      do(create_hull_cc(.)) %>% 
      mutate(landscape = case_match(landscape, "bgd"~"Berchtesgaden", "stoko"~"Shiretoko", "grte"~"Grand Teton"),
             landscape = factor(landscape, levels=c("Shiretoko", "Berchtesgaden", "Grand Teton")))) 
  # Convert your hulls to an sf object for geometric operations
  hulls_sf <- st_as_sf(hulls, coords = c("dist.dyn_baseline", "regen.dyn_baseline"), crs = 4326)
  # Calculate the convex hull for each landscape if not already done
  hulls_geom <- hulls_sf %>%
    group_by(landscape) %>%
    summarize(geometry = st_combine(geometry)) %>%
    st_convex_hull()  # Compute the convex hull of each landscape
  # Calculate the centroid of each landscape's convex hull
  centroids <- st_centroid(hulls_geom)
  # Extract the coordinates of the centroids
  centroid_coords <- st_coordinates(centroids)
  # Add these centroids to the hulls_geom object for later plotting
  hulls_geom$centroid_x <- centroid_coords[, 1]
  hulls_geom$centroid_y <- centroid_coords[, 2]
  
  data.loess <- loess(effect ~ dist.dyn_baseline * regen.dyn_baseline, data = a)
  summary(data.loess)
  # Create a sequence of incrementally increasing (by 0.3 units) values for both wt and hp
  xgrid <- seq(min(a$dist.dyn_baseline), max(a$dist.dyn_baseline), length.out=100)
  ygrid <- seq(min(a$regen.dyn_baseline), max(a$regen.dyn_baseline), length.out=100)
  # Generate a dataframe with every possible combination of wt and hp
  data.fit <-  expand.grid(dist.dyn_baseline = xgrid, regen.dyn_baseline = ygrid)
  # Feed the dataframe into the loess model and receive a matrix output with estimates of acceleration for each combination of wt and hp
  mtrx3d <-  predict(data.loess, newdata = data.fit)
  # Abbreviated display of final matrix
  mtrx3d[1:4, 1:4]
  mtrx.melt <- melt(mtrx3d, id.vars = c('dist.dyn_baseline', 'regen.dyn_baseline'), measure.vars = 'effect')
  names(mtrx.melt) <- c('dist.dyn_baseline', 'regen.dyn_baseline', 'effect')
  # Return data to numeric form
  mtrx.melt$dist.dyn_baseline <- as.numeric(str_sub(mtrx.melt$dist.dyn_baseline, str_locate(mtrx.melt$dist.dyn_baseline, '=')[1,1] + 1))
  mtrx.melt$regen.dyn_baseline <-  as.numeric(str_sub(mtrx.melt$regen.dyn_baseline, str_locate(mtrx.melt$regen.dyn_baseline, '=')[1,1] + 1))
  head(mtrx.melt)
  
  # create color scale centered on 0
  # https://stackoverflow.com/questions/43505146/how-to-force-map-zero-to-white-color-in-r-plotly-color-scale
  set.seed(42)
  colorlength <- 100
  null_value <- (0 - min(mtrx.melt$effect)) / (max(mtrx.melt$effect) - min(mtrx.melt$effect))        
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
  
  p1 <- plot_ly(mtrx.melt, x = ~dist.dyn_baseline, y = ~regen.dyn_baseline, z = ~effect, type = "contour",
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
           yaxis = list(title = 'Regeneration rate [recruited ha^-1 yr^-1]',
                        linecolor = "black",
                        linewidth = 0.5,
                        mirror = T,
                        tickvals = c(1:5*10),
                        range=range(mtrx.melt$regen.dyn_baseline, na.rm = TRUE),
                        titlefont = list(size = 50), tickfont = list(size = 50))) %>% 
    colorbar(title = "Climate effect [%]",
             titlefont = list(size = 50), tickfont = list(size = 50)); p1
  p2 <- p1 %>%
    add_trace(data = hulls, x = ~dist.dyn_baseline, y = ~regen.dyn_baseline, 
              mode = "lines", type = "scatter",
              line = list(width = 2),  # Line width
              color = ~factor(landscape),
              colors="grey30",
              # colors = c("#ffa214", "#7fff41", "#1d99f9"),
              inherit=F, showlegend=F); p2 #%>% 
    # add_text(data = hulls_geom, inherit=F,
    #          x = ~centroid_x, y = ~centroid_y, text = ~landscape,
    #          mode = "text", showlegend = FALSE, 
    #          textfont = list(size = 20, bold=TRUE, color=toRGB("grey10")))
  
  if (i < 3) {
    save_image(p2, file = paste0("results/figures/Q3_contourPlot_loess_", i, "_climateEffect.png"), scale=1, 
               width=1900, height=1700)
  } else {
    save_image(p2, file = paste0("results/figures/suppl_figures/Q3_contourPlot_loess_", i, "_climateEffect.png"), scale=1, 
               width=1900, height=1700)
  }
  
  rm(p1, p2, data.loess, a, mtrx.melt, xgrid, ygrid, data.fit, mtrx3d, hulls, centroids, hulls_geom, hulls_sf, centroid_coords, border, border_neg, border_pos, s,
     colorscale, null_value, colorlength)
}






# # # # # # # # # # # # # # # # # # # # # #
# DISCARDED ####
# # # # # # # # # # # # # # # # # # # # # #

## baseline - hotdry as two lines ####
# png("results/figures/Q3_responseLine_disturbanceRate_climate.png", res=200,
#     height=1300, width=2000)
# dyn.df  %>% 
#   ggplot(aes(x = dist.dyn, y = 100-value*100, col=climate)) +
#   geom_point(size = 0.1, alpha = 0.5) +
#   geom_smooth(method = "loess", se = FALSE) +
#   scale_x_log10(breaks = c(0.0001, 0.001, 0.01, 0.1, 1, 10)*2, # year 1:80 *0.5, 1:10 *2, 1:5 *5, 1:2 *10
#                 label = c(0.0001, 0.001, 0.01, 0.1, 1, 10)*2) +
#   facet_grid(~name) +
#   scale_color_manual(values=c("hotdry" = "firebrick2", "baseline"="royalblue3")) +
#   ylim(0, 100) +
#   labs(y = "Landscape changed [%]", col = "Climate scenario",
#        x = paste0("Simulated disturbance rate [% yr^-1]\nAxis log10-transformed")) +
#   theme_bw() +
#   theme(legend.position = "top")
# dev.off()
# 
# # hotdry: 
# ## structure: dampening effect at low-medium disturbance rates
# ## composition: strong amplifying effects at low disturbance rates, dampening effect at high disturbance rates
# ## forest loss: little interaction
# 
# png("results/figures/Q3_responseLine_regenerationRate_climate_80yrs.png", res=200,
#     height=1300, width=2000)
# dyn.df  %>% 
#   ggplot(aes(x = regen.dyn, y = value*100, col=climate)) +
#   geom_point(size = 0.1, alpha = 0.5) +
#   geom_smooth(method = "loess", se = FALSE) +
#   scale_x_reverse() +
#   facet_grid(~name) +
#   scale_color_manual(values=c("hotdry" = "firebrick2", "baseline"="royalblue3")) +
#   ylim(0, 100) +
#   labs(y = "Landscape unchanged [%]", col = "Climate scenario",
#        x = paste0("Simulated regeneration rate [Mean number of tress recruited per ha yr^-1]")) +
#   theme_bw() +
#   theme(legend.position = "top")
# dev.off()
# 


# effect.background <- data.frame(interaction = factor(c("Dampening", "Amplifying"), levels=c("Amplifying", "Dampening")),
#                                 xmin = c(-0, -0), xmax = c(Inf, Inf),
#                                 ymin = c(-Inf, 0), ymax = c(0, Inf)); effect.background
# ### disturbance rate ####
# dist.dyn.effect <- dyn.df %>% 
#   filter(climate=="baseline") %>% 
#   dplyr::select(-climate, -identifier, -n_year, -regen.dyn) %>% 
#   rename(baseline=value, 
#          dist.dyn_baseline=dist.dyn) %>% 
#   full_join(dyn.df %>% filter(climate=="hotdry") %>% 
#               dplyr::select(-climate, -identifier, -n_year, -regen.dyn) %>% 
#               rename(hotdry=value, 
#                      dist.dyn_hotdry=dist.dyn)) %>% 
#   mutate(baseline = 100-baseline*100, hotdry = 100-hotdry*100) %>% 
#   mutate(effect = hotdry-baseline); summary(dist.dyn.effect) # positive when hotdry > baseline -> amplifying effect
# 
# # absolute disturbance rate
# png("results/figures/Q2_climateEffect_disturbanceRate.png", res=200,
#     height=1300, width=2000)
# dist.dyn.effect %>% 
#   mutate(landscape = case_match(landscape, "bgd"~"Berchtesgaden", "stoko"~"Shiretoko", "grte"~"Grand Teton"),
#          landscape = factor(landscape, levels=c("Shiretoko", "Berchtesgaden", "Grand Teton"))) %>% 
#   ggplot() +
#   geom_rect(data=effect.background, 
#             aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=interaction), alpha = 0.3) +
#   geom_point(aes(x=dist.dyn_baseline, y=effect), size=0.05, alpha=0.5) +
#   # geom_smooth(aes(x=dist.dyn_baseline, y=effect, col=landscape), method = "loess", se = F) +
#   geom_hline(aes(yintercept=0)) +
#   facet_grid(~name) +
#   scale_x_log10(breaks = c(0.0001, 0.001, 0.01, 0.1, 1, 10, 50)*2, 
#                 label = c(0.0001, 0.001, 0.01, 0.1, 1, 10, 50)*2) +
#   scale_fill_manual(name = "Interaction type",
#                     values = c("Amplifying"="hotpink1", "Dampening" = "cornflowerblue"),
#                     labels = c("Amplifying:\nmore landscape changed", "Dampening:\nless landscape changed")) +
#   labs(x = "Simulated disturbance rate [% yr^-1]\nAxis log10-transformed",
#        y = "Climate effect [percentage point change, %] ", title="Effect of climate on landscape changed compared to baseline",
#        col="Landscape") +
#   scale_color_manual(values=colors.landscape) +
#   theme_bw()
# dev.off()
# 
# # relative disturbance rate
# png("results/figures/Q2_climateEffect_disturbanceRate_relative.png", res=200,
#     height=1300, width=2000)
# dist.dyn.effect %>%
#   full_join(dist.dyn.effect %>%
#               filter(size==1, freq==1, fecundity==100, browsing==1) %>%
#               group_by(landscape) %>%
#               summarise(dist.dyn_ref = mean(dist.dyn_baseline)) %>%
#               dplyr::select(landscape, dist.dyn_ref), multiple = "all", by = "landscape") %>%
#   mutate(dist.change = (dist.dyn_baseline - dist.dyn_ref)/dist.dyn_ref,
#          landscape = case_match(landscape, "bgd"~"Berchtesgaden", "stoko"~"Shiretoko", "grte"~"Grand Teton"),
#          landscape = factor(landscape, levels=c("Shiretoko", "Berchtesgaden", "Grand Teton"))) %>% 
#   ggplot() +
#   geom_rect(data=effect.background,
#             aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=interaction), alpha = 0.3) +
#   geom_point(aes(x=dist.change, y=effect),
#              size=0.05) +
#   geom_smooth(aes(x=dist.change, y=effect),
#               method = "loess", se = F, col="black") +
#   geom_hline(aes(yintercept=0)) +
#   facet_grid(landscape~name) +
#   scale_fill_manual(name = "Interaction type",
#                     values = c("Amplifying"="hotpink1", "Dampening" = "cornflowerblue"),
#                     labels = c("Amplifying:\nmore landscape broken", "Dampening:\nless landscape broken")) +
#   labs(x = "Percent change in disturbance rate [%]",
#        y = "Climate effect [percentage point change, %] ", title="Effect of climate on landscape unchanged compared to baseline") +
#   theme_bw()
# dev.off()
# 
# 
# ### regeneration rate ####
# regen.dyn.effect <- dyn.df %>% 
#   filter(climate=="baseline") %>% 
#   dplyr::select(-climate, -identifier, -n_year, -dist.dyn) %>% 
#   rename(baseline=value, regen.dyn_baseline=regen.dyn) %>% 
#   full_join(dyn.df %>% filter(climate=="hotdry") %>% 
#               dplyr::select(-climate, -identifier, -n_year, -dist.dyn) %>% 
#               rename(hotdry=value, regen.dyn_hotdry=regen.dyn)) %>% 
#   mutate(baseline = 100-baseline*100, hotdry = 100-hotdry*100) %>% 
#   mutate(effect = hotdry-baseline) # positive when hotdry > baseline -> dampening effect
# summary(regen.dyn.effect)
# 
# # absolute regeneration rate
# png("results/figures/Q2_climateEffect_regenerationRate.png", res=200,
#     height=1300, width=2000)
# regen.dyn.effect %>% 
#   mutate(landscape = case_match(landscape, "bgd"~"Berchtesgaden", "stoko"~"Shiretoko", "grte"~"Grand Teton"),
#          landscape = factor(landscape, levels=c("Shiretoko", "Berchtesgaden", "Grand Teton"))) %>% 
#   ggplot() +
#   geom_rect(data=effect.background, 
#             aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=interaction), alpha = 0.3) +
#   geom_point(aes(x=regen.dyn_baseline, y=effect),
#              size=0.05, alpha=0.5) +
#   # geom_smooth(aes(x=regen.dyn_baseline, y=effect*100, col=landscape), method = "loess", se = F) +
#   geom_hline(aes(yintercept=0)) +
#   facet_grid(~name) +
#   scale_x_reverse() +
#   scale_fill_manual(name = "Interaction type",
#                     values = c("Amplifying"="hotpink1", "Dampening" = "cornflowerblue"),
#                     labels = c("Amplifying:\nmore landscape broken", "Dampening:\nless landscape broken")) +
#   labs(x = "Simulated regeneration rate [Mean number of tress recruited per ha yr^-1]",
#        y = "Climate effect [percentage point change, %] ", title="Effect of climate on landscape unchanged compared to baseline",
#        col="Landscape") +
#   scale_color_manual(values=colors.landscape) +
#   theme_bw()
# dev.off()

# relative regeneration rate
# png("results/figures/Q2_climateEffect_regenerationRate_relative_80yrs.png", res=200,
#     height=1300, width=2000)
# regen.dyn.effect %>% 
#   full_join(regen.dyn.effect %>% 
#               filter(size==1, freq==1, fecundity==100, browsing==1) %>% 
#               group_by(landscape) %>% 
#               summarise(regen.dyn_ref = mean(regen.dyn_baseline)) %>% 
#               dplyr::select(landscape, regen.dyn_ref), multiple = "all", by = "landscape") %>% 
#   mutate(regen.change = (regen.dyn_baseline - regen.dyn_ref)/regen.dyn_ref
# landscape = case_match(landscape, "bgd"~"Berchtesgaden", "stoko"~"Shiretoko", "grte"~"Grand Teton"),
#        landscape = factor(landscape, levels=c("Shiretoko", "Berchtesgaden", "Grand Teton"))) %>% 
#   ggplot() +
#   geom_rect(data=effect.background, 
#             aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=interaction), alpha = 0.3) +
#   geom_point(aes(x=regen.change, y=effect*100),
#              size=0.05) +
#   geom_smooth(aes(x=regen.change, y=effect*100),
#               method = "loess", se = F, col="black") +
#   geom_hline(aes(yintercept=0)) +
#   scale_x_reverse() +
#   facet_grid(landscape~name) +
#   scale_fill_manual(name = "Interaction type",
#                      values = c("Dampening" = "darkseagreen3", "Amplifying"="cornflowerblue")
#                     labels = c("Dampening:\nless landscape broken", "Amplifying:\nmore landscape broken")) +
#   labs(x = "Percent change in disturbance rate [%]\nRates based on only the first 10 simulation years",
#        y = "Climate effect [percentage point change, %] ", title="Effect of climate on landscape unchanged compared to baseline") +
#   theme_bw()
# dev.off()

values = c("Dampening" = "#018571", "Amplifying"="#143d59")



