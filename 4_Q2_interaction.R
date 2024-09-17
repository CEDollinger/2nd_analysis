# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Q2: how do disturbances and regeneration interact? ##########################################################################################################
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

dyn.df <- read_csv("results/datasets/dyn.df.csv")


# TO DOs
## mask out missing data space
## add hull around landscapes

# Create a function to compute the convex hull for a set of points
create_hull <- function(data) {
  hull_indices <- chull(data$dist.dyn, data$regen.dyn)
  hull_indices <- c(hull_indices, hull_indices[1])  # Close the hull by connecting to the first point
  data[hull_indices, ]
}

i<-1; clim<-"baseline"
for (clim in c("baseline")) { #, "hotdry"
  for (i in 1:3) {
    a <- dyn.df %>% 
      filter(name == names(response.colors)[i], 
             climate==clim) %>% 
      dplyr::select(dist.dyn, regen.dyn, value, landscape) %>% 
      mutate(value=100-value*100, dist.dyn = log10(dist.dyn))
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
    p1 <- plot_ly(mtrx.melt, x = ~dist.dyn, y = ~regen.dyn, z = ~Change, type = "contour",
                  colors = "Spectral", reversescale=T, zmin=0, zmax=100, ncontours=21) %>% 
      layout(title = names(response.colors)[i], 
             xaxis = list(title = 'Disturbance rate [log10-transformed, % yr^-1]',
                          ticktext = c(10^c(-3:1),100),
                          tickvals = c(-3:1, log10(100))),
             yaxis = list(title = 'Regeneration rate [recruited ha^-1 yr^-1]',
                          tickvals = c(1:5*10))) %>% 
      colorbar(title="Change [%]"); p1
    p2 <- p1 %>%
      add_trace(data = hulls, x = ~dist.dyn, y = ~regen.dyn, 
                mode = "lines", type = "scatter",
                line = list(width = 2),  # Line width
                color = ~factor(landscape),
                colors="grey30",
                # colors = c("#ffa214", "#7fff41", "#1d99f9"),
                inherit=F, showlegend=F) %>% 
      add_text(data = hulls_geom, inherit=F,
               x = ~centroid_x, y = ~centroid_y, text = ~landscape,
               mode = "text", showlegend = FALSE, 
               textfont = list(size = 20, bold=TRUE, color=toRGB("grey10"))); p2
    if (i < 3) {
      save_image(p2, file = paste0("results/figures/Q2_contourPlot_loess_", i, "_", clim, ".png"), scale=1, 
                 width=900, height=700) 
    } else {
      save_image(p2, file = paste0("results/figures/suppl_figures/Q2_contourPlot_loess_", i, "_", clim, ".png"), scale=1, 
                 width=900, height=700) 
    }
    
    rm(p1, p2, data.loess, a, mtrx.melt, xgrid, ygrid, data.fit, mtrx3d, hulls, centroids, hulls_geom, hulls_sf, centroid_coords)
  }
}


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# DISCARDED ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

## create dyn.df ####
# overtime.ls <- readRDS("results/datasets/overtime.ls.RDATA")
# patches <- bind_rows(readRDS("results/datasets/patch_bgd_backup.RDATA"),
#                      readRDS("results/datasets/patch_grte_backup.RDATA"),
#                      readRDS("results/datasets/patch_stoko_backup.RDATA")); head(patches)
# 
# patchlist <- bind_rows(readRDS("results/datasets/patchlist_bgd.RDATA"),
#                        readRDS("results/datasets/patchlist_grte.RDATA"),
#                        readRDS("results/datasets/patchlist_stoko.RDATA")); head(patchlist)
# 
# # actually forested area: correction factor for GRTE
# forest.ha.df <- patches %>% 
#   inner_join(patchlist) %>% 
#   inner_join(areas) %>%
#   # filter(agent %in% c("wind", "fire")) %>% # only unspecific agents 
#   mutate(pct=n_cells/n_cells_soll) %>% # what pct of targeted cells was actually disturbed? (proxy for forested area)
#   group_by(year, landscape, climate, size, freq, browsing, fecundity, rep) %>% 
#   summarise(area=mean(area),
#             # calculate weighted.mean -> bigger patches should be more reliable
#             # forest.ha=weighted.mean(pct, n_cells)*mean(area), # mean of pct disturbed multiplied by landscape area 
#             forest.ha = mean(pct)*mean(area), 
#             n_patches = length(unique(patchID))) %>% ungroup() %>% 
#   mutate(size = factor(size, levels = rev(c("10", "5", "2", "1"))),
#          freq = factor(freq, levels = rev(c("10", "5", "2", "1"))),
#          fecundity = factor(fecundity, levels = c("100", "50", "20", "10")),
#          browsing = factor(browsing, levels = rev(c("10", "5", "2", "1"))),
#          forest.ha = ifelse(landscape != "grte", area, forest.ha), # only apply this correction for GRTE
#          forest.ha = ifelse(is.na(forest.ha), area, forest.ha)); head(forest.ha.df); summary(forest.ha.df)
# 
# patch.df <- patches %>% 
#   filter(killed_ba > 0) %>% 
#   group_by(landscape, climate, rep, size, freq, browsing, fecundity, year) %>% # don't group by agent
#   summarise(area_disturbed = sum(n_cells)) %>%
#   mutate(size = factor(size, levels = rev(c("10", "5", "2", "1"))),
#          freq = factor(freq, levels = rev(c("10", "5", "2", "1"))),
#          fecundity = factor(fecundity, levels = c("100", "50", "20", "10")),
#          browsing = factor(browsing, levels = rev(c("10", "5", "2", "1")))) %>% 
#   inner_join(forest.ha.df) %>% 
#   mutate(dist.rate = area_disturbed/forest.ha, # already in % (because area_disturbed in 100 m^2, forest.ha in ha
#          dist.rate = ifelse(dist.rate > 100, 100, dist.rate)) %>% 
#   mutate(keep = ifelse(forest.ha > area*0.1, "yes", "no")) %>% # exclude years where forest area drops below 10%
#   filter(keep == "yes"); summary(patch.df) 
# # combine with vegetation data
# dist.dyn.df <- bind_rows(overtime.ls[["bgd"]], overtime.ls[["grte"]],overtime.ls[["stoko"]]) %>% 
#   filter(year==80) %>% dplyr::select(-year) %>% 
#   full_join(patch.df %>% 
#               group_by(climate, size, freq, browsing, fecundity, landscape, rep, area) %>% # take mean over all simulation years
#               summarise(n_year = length(unique(year)),
#                         dist.dyn = mean(dist.rate)), 
#             by=c("climate", "rep", "size", "freq", "browsing", "fecundity", "landscape")) %>% 
#   pivot_longer(9:11) %>% 
#   mutate(dist.dyn = ifelse(is.na(dist.dyn), 0, dist.dyn)); summary(dist.dyn.df)
# 
# # regeneration rate
# regen.df <- bind_rows(readRDS("results/datasets/regen_bgd_backup.RDATA"),
#                       readRDS("results/datasets/regen_grte_backup.RDATA"),
#                       readRDS("results/datasets/regen_stoko_backup.RDATA")) %>% 
#   mutate(size = factor(size, levels = rev(c("10", "5", "2", "1"))),
#          freq = factor(freq, levels = rev(c("10", "5", "2", "1"))),
#          fecundity = factor(fecundity, levels = c("100", "50", "20", "10")),
#          browsing = factor(browsing, levels = rev(c("10", "5", "2", "1")))) %>% 
#   inner_join(forest.ha.df) %>% 
#   mutate(correction = area/forest.ha, 
#          recruited_mean = recruited_mean * correction); summary(regen.df)
# head(regen.df)
# # combine with vegetation data
# regen.dyn.df <- bind_rows(overtime.ls[["bgd"]], overtime.ls[["grte"]],overtime.ls[["stoko"]]) %>% 
#   filter(year==80) %>% dplyr::select(-year) %>% 
#   full_join(regen.df %>% 
#               group_by(climate, size, freq, browsing, fecundity, landscape, rep, area) %>% 
#               summarise(regen.dyn = mean(recruited_mean),
#                         n_year = length(unique(year))), 
#             by=c("climate", "rep", "size", "freq", "browsing", "fecundity", "landscape")) %>% 
#   pivot_longer(9:11) %>% 
#   mutate(regen.dyn = ifelse(is.na(regen.dyn), 0, 
#                             ifelse(is.infinite(regen.dyn), 0, regen.dyn))) 
# 
# dyn.df <- dist.dyn.df %>% 
#   dplyr::select(-n_year) %>% 
#   full_join(regen.dyn.df) %>% 
#   drop_na(); unique(dyn.df$climate) # get rid of 1 run (grte, "baseline_rep1_size1_freq10_browsing1_fecundity10")
# 
# rm(patches, patchlist, forest.ha.df); 1+1

## diagnostics rates ####
# # disturbance processes
# png("results/figures/Q1_pie_disturbance_x.png", res=200,
#     height = 1500, width = 2000)
# bind_rows(overtime.ls[["bgd"]], overtime.ls[["grte"]],overtime.ls[["stoko"]]) %>% 
#   filter(climate=="baseline", year==80) %>% 
#   mutate(across(10:12, ~ abs(.x-1)*100)) %>%  # transform to % landscape changed
#   group_by(size, freq) %>% 
#   summarise(across(8:10, sum)) %>% ungroup() %>% 
#   mutate(across(3:5, ~ .x/24000)) %>% # maximum sum = 24000 (100 % * 5 reps * 16 regen scenarios * 3 landscapes)
#   rename('1. Structure\nBasal area decreased by >50 % from reference' = 3,
#          '2. Composition\nDominant species changed from reference' = 4,
#          '3. Remaining forest\nStem density dropping below 50 trees/ha' = 5) %>% 
#   pivot_longer(3:5) %>% 
#   mutate(size = paste0("Size * ", size), freq = paste0("Frequency * ", freq),
#          size = factor(size, levels=c(paste0("Size * ", c(1, 2, 5, 10)))),
#          freq = factor(freq, levels=c(paste0("Frequency * ", c(1, 2, 5, 10))))) %>% 
#   ggplot(aes(x=0, y=value, fill=name)) +
#   geom_bar(stat="identity") +
#   facet_grid(rows=vars(size), cols=vars(freq), as.table = F, switch = "both") +
#   coord_polar("x") + # "y": pie chart but all same size, "x": weird drop pattern chart, but different sizes
#   labs(fill="Response", x="", y="", title="Breaking the system: disturbance processes") +
#   theme_bw() +
#   theme(axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
#         axis.text.x=element_blank(), axis.text.y=element_blank(),
#         legend.position = "top") 
# dev.off()
# 
# 
# # regeneration processes
# png("results/figures/Q1_pie_regeneration_y.png", res=200,
#     height = 1500, width = 2000)
# bind_rows(overtime.ls[["bgd"]], overtime.ls[["grte"]],overtime.ls[["stoko"]]) %>% 
#   filter(climate=="baseline", year==80) %>% 
#   mutate(across(10:12, ~ abs(.x-1)*100)) %>%  # transform to % landscape changed
#   group_by(fecundity, browsing) %>% 
#   summarise(across(8:10, sum)) %>% ungroup() %>% 
#   mutate(across(3:5, ~ .x/24000)) %>% # maximum sum = 24000 (100 % * 5 reps * 16 regen scenarios * 3 landscapes)
#   rename('1. Structure\nBasal area decreased by >50 % from reference' = 3,
#          '2. Composition\nDominant species changed from reference' = 4,
#          '3. Remaining forest\nStem density dropping below 50 trees/ha' = 5) %>% 
#   pivot_longer(3:5) %>%
#   mutate(fecundity = paste0("Fecundity * ", fecundity, "%"), browsing = paste0("Browsing * ", browsing),
#          fecundity = factor(fecundity, levels=c(paste0("Fecundity * ", c(100, 50, 20, 10), "%"))),
#          browsing = factor(browsing, levels=c(paste0("Browsing * ", c(1, 2, 5, 10))))) %>% 
#   ggplot(aes(x=0, y=value, fill=name)) +
#   geom_bar(stat="identity") +
#   facet_grid(rows=vars(fecundity), cols=vars(browsing), as.table = F, switch = "both") +
#   coord_polar("y") + 
#   labs(fill="Response", x="", y="", title="Breaking the system: regeneration processes") +
#   theme_bw() +
#   theme(axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
#         axis.text.x=element_blank(), axis.text.y=element_blank(),
#         legend.position = "top") 
# dev.off()
# # disturbance rate per year
# png("results/figures/Q0_disturbanceRate_5x5_newMethod.png", res=200,
#     height=1300, width=2000)
# patch.df %>% 
#   mutate(dist.rate = ifelse(dist.rate > 100, 100, dist.rate)) %>% 
#   filter(climate=="baseline", size==5, freq==5, browsing==1, fecundity==100) %>% 
#   group_by(year, landscape, rep) %>% 
#   summarise(dist.rate = mean(dist.rate)) %>% ungroup() %>% 
#   mutate(landscape=factor(landscape, levels=c("stoko", "bgd", "grte"))) %>% 
#   ggplot(aes(x=year, y=dist.rate, group=rep)) +
#   geom_line(linewidth=0.2) +
#   facet_wrap(~landscape, scales="free_y") +
#   labs(x="Sim. year", y="Disturbance rate [% yr^-1]", 
#        title="Simulated disturbance rates\nbaseline size*5 freq*5 browsing*1 fecundity*100 reps 1:5") +
#   theme_bw()
# dev.off()
# # disturbance rate over full sim. period in grte: low (max ~4%)
# # explanation: very little disturbances possible in later simulation years (no forest left to be disturbed) 
# # -> calculate disturbance rate only based on first 5-10 simulation years?
# 
# 
# # regeneration rate
# regen.plot <- regen.df %>% 
#   filter(climate=="baseline", size==1, freq==1, browsing==5, fecundity==20) %>% 
#   group_by(year, landscape, rep) %>% 
#   summarise(regen.rate = mean(recruited_mean)) %>% ungroup() %>% 
#   mutate(landscape=factor(landscape, levels=c("stoko", "bgd", "grte"))) 
# regen.plot.mean <- regen.plot %>% group_by(landscape) %>% summarise(regen.rate = mean(regen.rate)) %>% ungroup()
# png("results/figures/Q0_regenerationRate_5x5_newMethod.png", res=200,
#     height=1300, width=2000)
# regen.plot %>% 
#   ggplot(aes(x=year, y=regen.rate, group=rep)) +
#   geom_line(linewidth=0.2) +
#   geom_hline(data=regen.plot.mean, aes(yintercept=regen.rate)) +
#   facet_wrap(~landscape) +
#   labs(x="Sim. year", y="Regeneration rate [mean n trees recruited ha^-1 yr^-1]", 
#        title="Simulated regeneration rates\nbaseline size*1 freq*1 browsing*5 fecundity*20 reps 1:5") +
#   theme_bw()
# dev.off()
# rm(regen.plot, regen.plot.mean)
# 
# png("results/figures/Q0_regenerationRate_year40.png", res=200,
#     height=1300, width=2000)
# regen.df %>% 
#   filter(climate=="baseline", year==40) %>% 
#   group_by(landscape, rep, browsing, fecundity) %>% 
#   summarise(regen.rate = mean(recruited_mean)) %>% ungroup() %>% 
#   mutate(landscape=factor(landscape, levels=c("stoko", "bgd", "grte"))) %>% 
#   mutate(level=paste0("fecundity x ", as.numeric(as.character(fecundity))/100, "\nbrowsing x ", browsing),
#          regen=1/as.numeric(as.character(fecundity))*as.numeric(as.character(browsing))) %>% 
#   mutate(landscape = case_match(landscape, "bgd"~"Berchtesgaden", "stoko"~"Shiretoko", "grte"~"Grand Teton"),
#          landscape = factor(landscape, levels=c("Shiretoko", "Berchtesgaden", "Grand Teton"))) %>% 
#   ggplot(aes(x=reorder(level, regen), y=regen.rate, col=landscape, group=paste(rep, landscape))) +
#   geom_line(linewidth=1) +
#   labs(x="Regeneration modification", y="Regeneration rate [Mean number of tress recruited per ha yr^-1]", 
#        title="Simulated regeneration rate\nbaseline climate, simulation year 40, reps 1:5",
#        col="Landscape") +
#   scale_color_manual(values=colors.landscape) +
#   theme_bw()
# dev.off()

## other contour plots ####
#### akima interpolation ####
# library(akima)
# # Perform interpolation
# interp_result <- with(a, akima::interp(x = dist.dyn, y = regen.dyn, z = value, 
#                                        duplicate = "mean", extrap = TRUE))
# # Convert the interpolated result into a data frame for ggplot
# interp_df <- with(interp_result, expand.grid(x = x, y = y))
# interp_df$z <- as.vector(interp_result$z)
# interp_df <- na.omit(interp_df)  # Remove any rows with NA values
# ggplot() +
#   geom_contour_filled(data=interp_df, aes(x = x, y = y, z = z)) +
#   geom_point(data=a, aes(x=dist.dyn, y=regen.dyn), col="white", size=0.2) +
#   labs(title = "Contour Plot of Interpolated Values",
#        x = "dist.dyn",
#        y = "regen.dyn",
#        fill = "Value") +
#   theme_minimal() #trial_interpolated_akima
# 
# #### raw data ####
# # expand.grid(dist.dyn=a$dist.dyn, regen.dyn=a$regen.dyn) %>% 
# #   full_join(a) %>% 
# #   mutate(dist.dyn = round(dist.dyn, 2),
# #          regen.dyn = round(regen.dyn, 0)) %>% 
# #   group_by(dist.dyn, regen.dyn) %>% 
# #   summarise(value = mean(value, na.rm=T)) %>% ungroup() %>% 
# #   mutate(value = ifelse(is.na(value), -0.2, value)) %>% 
# #   ggplot(aes(x=dist.dyn, y=regen.dyn, z=value)) +
# #   # geom_contour(aes(colour = after_stat(level)), bins=10) +
# #   geom_contour_filled(binwidth = 20) +
# #   # geom_point(data=a.fil, col="black", size=0.2) +
# #   labs(title = "Contour Plot of Raw Values",
# #        x = "dist.dyn",
# #        y = "regen.dyn",
# #        fill = "Value") +
# #   theme_minimal() 
# 
# a.fil <- a %>% filter(dist.dyn < quantile(dist.dyn, 1)); b <- a.fil; b$dist.dyn <- round(b$dist.dyn, 1); b$regen.dyn <- round(b$regen.dyn, 0)
# b %>% 
#   ggplot(aes(x=dist.dyn, y=regen.dyn, z=value)) +
#   # geom_contour(aes(colour = after_stat(level)), bins=10) +
#   geom_contour_filled(bins=5) +
#   geom_point(data=a.fil, aes(col=landscape), size=0.2) +
#   scale_x_log10() +
#   labs(title = "Contour Plot of Raw Values",
#        x = "dist.dyn",
#        y = "regen.dyn",
#        fill = "Value") +
#   theme_minimal() 
# 
# # really raw data, all responses
# png("results/figures/Q0_relationship_regenRate_distRate_indicators.png", res=200,
#     height=1300, width=2000)
# dyn.df %>% 
#   filter(climate=="baseline") %>% 
#   ggplot(aes(x=dist.dyn, y=regen.dyn, col=value)) +
#   geom_point(size=0.1) +
#   geom_smooth(method="loess", se=F) +
#   scale_x_log10(breaks = c(0.0001, 0.001, 0.01, 0.1, 1, 10)*2,
#                 label = c(0.0001, 0.001, 0.01, 0.1, 1, 10)*2) +
#   facet_grid(~name) +
#   labs(x="Disturbance rate [based on all 80 yrs, % yr^-1]", y="Regeneration rate\n[based on all 80 yrs, recruited trees per ha yr^-1]") +
#   theme_bw()
# dev.off()  

### Surface 3D: both rates ####
# library(plotly)
# library(reticulate); reticulate::use_miniconda('r-reticulate') # to save plotly graphs as png
# 
# response.list <- list(tibble(), tibble(), tibble()); names(response.list) <- names(response.colors); surface.list <- list(response.list, response.list)
# names(surface.list) <- c("baseline", "hotdry"); rm(response.list)
# # loop for filling list with all 6 combinations (2 climates x 3 responses)
# clim <- "baseline"; resp <- names(response.colors)[1]
# for (clim in names(surface.list)) {
#   for (resp in names(response.colors)) {
#     surface.list[[clim]][[resp]] <- dist.dyn.df %>% dplyr::select(-n_year) %>% full_join(regen.dyn.df %>% dplyr::select(-n_year)) %>% drop_na() %>% 
#       filter(climate==clim, 
#              name==resp) %>% 
#       mutate(value=100-value*100) %>% 
#       dplyr::select(dist.dyn, regen.dyn, value) %>% 
#       distinct()
#   }
# }
# 
# # raw data
# p <- plot_ly(x = ~surface.list[["baseline"]][[1]]$dist.dyn, y = ~surface.list[["baseline"]][[1]]$regen.dyn, 
#              z = ~surface.list[["baseline"]][[1]]$value, intensity = ~surface.list[["baseline"]][[1]]$value, type = 'mesh3d') %>%
#   layout(title = 'Raw data', 
#          scene = list(xaxis = list(title = "Disturbance rate"), yaxis = list(title = "Regeneration rate"), 
#                       zaxis = list(title = "Landscape changed [structure, %]"), coloraxis = list(title = "this does NOT work (yet)"))); p
# save_image(p, file = paste0("results/figures/surface_1_baseline_rawData.png"), scale=1, width=1000, height=1000); rm(p)
# 
# # lm model
# mod1 <- lm(surface.list[["baseline"]][[1]]$value ~ surface.list[["baseline"]][[1]]$dist.dyn + surface.list[["baseline"]][[1]]$regen.dyn)
# summary(mod1)
# disturbance_rate <-mod1$model[,1]; regeneration_rate <- mod1$model[,2]; structural_change <-mod1$fitted.values; plot_ly(
#   x = ~disturbance_rate, y = ~regeneration_rate, z = ~structural_change, intensity = ~structural_change, type = 'mesh3d') %>%
#   layout(title = 'Linear model', 
#          scene = list(xaxis = list(title = "Disturbance rate"), yaxis = list(title = "Regeneration rate"), 
#                       zaxis = list(title = "Landscape changed [structure, %]"), coloraxis = list(title = "this does NOT work (yet)")))
# 
# # loess model
# mod2 <- loess(surface.list[["baseline"]][[1]]$value ~ surface.list[["baseline"]][[1]]$dist.dyn + surface.list[["baseline"]][[1]]$regen.dyn)
# disturbance_rate <-mod2$x[,1]; regeneration_rate <- mod2$x[,2]; structural_change <-mod2$fitted
# plot_ly(
#   x = ~disturbance_rate, y = ~regeneration_rate, z = ~structural_change, intensity = ~structural_change, type = 'mesh3d') %>%
#   layout(title = 'Loess model', 
#          scene = list(xaxis = list(title = "Disturbance rate"), yaxis = list(title = "Regeneration rate"), 
#                       zaxis = list(title = "Landscape changed [structure, %]"), coloraxis = list(title = "this does NOT work (yet)")))
# 
# # loop for loess models
# clim <- "baseline"; resp <- 2
# for (clim in names(surface.list)) {
#   for (resp in 1:3) {
#     # raw data
#     p <- plot_ly(x = ~surface.list[[clim]][[resp]]$dist.dyn, y = ~surface.list[[clim]][[resp]]$regen.dyn, 
#                  z = ~surface.list[[clim]][[resp]]$value, intensity = ~surface.list[[clim]][[resp]]$value, type = 'mesh3d') %>%
#       layout(title = paste0(clim, " - ", names(response.colors)[resp], '\nRaw data'),
#              scene = list(xaxis = list(title = "Disturbance rate"), yaxis = list(title = "Regeneration rate"), 
#                           zaxis = list(title = "Landscape changed [structure, %]"), coloraxis = list(title = "this does NOT work (yet)"))); p
#     save_image(p, file = paste0("results/figures/surface_raw_", resp, "_", clim, ".png"), scale=1, width=1000, height=1000); rm(p)
#     
#     # loess model
#     mod2 <- loess(surface.list[[clim]][[resp]]$value ~ surface.list[[clim]][[resp]]$dist.dyn + surface.list[[clim]][[resp]]$regen.dyn)
#     disturbance_rate <-mod2$x[,1]; regeneration_rate <- mod2$x[,2]; change <-mod2$fitted; change[change>100] <- 100; change[change<0] <- 0
#     p1 <- plot_ly(
#       x = ~disturbance_rate, y = ~regeneration_rate, z = ~change, intensity = ~change, type = 'mesh3d') %>%
#       layout(title = paste0(clim, " - ", names(response.colors)[resp], '\nLoess model'), 
#              scene = list(xaxis = list(title = "Disturbance rate"), yaxis = list(title = "Regeneration rate"), 
#                           zaxis = list(title = "Landscape changed [%]"), coloraxis = list(title = "this does NOT work (yet)")))
#     save_image(p1, file = paste0("results/figures/surface_loess_", resp, "_", clim, ".png"), scale=1, width=1000, height=1000)
#     rm(mod2, disturbance_rate, regeneration_rate, change, p1)
#   }
# }


## cont. line plots ####

# ### disturbance rate ####
# # overall mean disturbance rate on x-axis
# dist.dyn.df$dist.dyn %>% summary() # chose sensible labels
# ranges.dist <- c(range(dist.dyn.df[dist.dyn.df$landscape=="stoko", "dist.dyn"]),
#                  range(dist.dyn.df[dist.dyn.df$landscape=="bgd", "dist.dyn"]),
#                  range(dist.dyn.df[dist.dyn.df$landscape=="grte", "dist.dyn"]))
# png("results/figures/Q2_responseLine_disturbanceRate_80yrs.png", res=200,
#     height=1300, width=2000)
# dist.dyn.df %>% 
#   filter(climate == "baseline") %>% 
#   ggplot(aes(x = dist.dyn, y = 100-value*100, col = name)) +
#   geom_point(size = 0.1, alpha = 0.5) +
#   geom_smooth(method = "loess", se = FALSE) +
#   scale_x_log10(breaks = c(0.0001, 0.001, 0.01, 0.1, 1, 10, 50)*2, # year 1:80 *0.5, 1:10 *2, 1:5 *5, 1:2 *10
#                 label = c(0.0001, 0.001, 0.01, 0.1, 1, 10, 50)*2) +
#   ylim(0, 100) +
#   scale_color_manual(values=response.colors) +
#   labs(y = "Landscape changed [%]", col = "Response",
#        x = paste0("Simulated disturbance rate [% yr^-1]\nRate based on only the first ", unique(dist.dyn.df$n_year)," simulation years\nAxis log10-transformed")) +
#   theme_bw() +
#   coord_cartesian(clip="off") +
#   annotate("text", x = mean(ranges.dist[1:2]), y = 100-55, label = "Shiretoko") + 
#   annotate("segment", x = ranges.dist[1], xend = ranges.dist[2], y = 50, yend = 50) +
#   annotate("text", x = mean(ranges.dist[3:4]), y = 100-45, label = "Berchtesgaden") + 
#   annotate("segment", x = ranges.dist[3], xend = ranges.dist[4], y = 60, yend = 60) +
#   annotate("text", x = mean(ranges.dist[5:6]), y = 100-35, label = "Grand Teton") + 
#   annotate("segment", x = ranges.dist[5], xend = ranges.dist[6], y = 70, yend = 70) +
#   # theme(legend.position = "top", panel.grid.major = element_blank(),
#   #       panel.grid.minor = element_blank(),
#   #       panel.border = element_blank(),
#   #       panel.background = element_blank()) # for plotting only the disturbance rate ranges per landscape
#   theme(legend.position = "top")
# dev.off()
# 
# 
# # relative disturbance rate
# png("results/figures/Q2_responseLine_disturbanceRate_relative_80yrs.png", res=200,
#     height=1300, width=2000)
# dist.dyn.df %>% 
#   filter(climate=="baseline") %>% 
#   full_join(dist.dyn.df %>% 
#               filter(size==1, freq==1, fecundity==100, browsing==1) %>% 
#               group_by(landscape) %>% 
#               summarise(dist.dyn_ref = mean(dist.dyn)) %>% 
#               dplyr::select(landscape, dist.dyn_ref), multiple = "all", by = "landscape") %>% 
#   mutate(dist.change = (dist.dyn - dist.dyn_ref)/dist.dyn_ref,
#          landscape = factor(landscape, levels=c("stoko", "bgd", "grte"))) %>% 
#   ggplot(aes(x = dist.change, y = value*100, col = name)) +
#   geom_point(size = 0.1, alpha = 0.5) +
#   geom_smooth(method = "loess", se = FALSE) +
#   facet_grid(~landscape) +
#   ylim(0, 100) +
#   scale_color_manual(values=response.colors) +
#   labs(y = "Landscape unchanged [%]", col = "Response",
#        x = paste0("Percent change in disturbance rate [%]\nRates based on only the first ", unique(dist.dyn.df$n_year)," simulation years")) +
#   theme_bw() +
#   theme(legend.position = "top")
# dev.off()
# 
# 
# ### regeneration rate ####
# # overall mean regeneration rate on x-axis
# regen.dyn.df$regen.dyn %>% summary() 
# ranges.regen <- c(range(regen.dyn.df[regen.dyn.df$landscape=="stoko", "regen.dyn"]),
#                   range(regen.dyn.df[regen.dyn.df$landscape=="bgd", "regen.dyn"]),
#                   range(regen.dyn.df[regen.dyn.df$landscape=="grte", "regen.dyn"]))
# 
# png("results/figures/Q2_responseLine_regenerationRate_80yrs.png", res=200,
#     height=1300, width=2000)
# regen.dyn.df %>% 
#   filter(climate=="baseline") %>% 
#   ggplot(aes(x = regen.dyn, y = 100-value*100, col = name)) +
#   geom_point(size = 0.1, alpha = 0.5) +
#   geom_smooth(method = "loess", se = FALSE) +
#   ylim(0, 100) +
#   scale_x_reverse() +
#   scale_color_manual(values=response.colors) +
#   labs(y = "Landscape changed [%]", col = "Response",
#        x = paste0("Simulated regeneration rate [Mean number of tress recruited per ha yr^-1]\nRate based on the first ", unique(regen.dyn.df$n_year)," simulation years")) +
#   theme_bw() +
#   coord_cartesian(clip="off") +
#   annotate("text", x = mean(ranges.regen[1:2]), y = 45, label = "Shiretoko") + 
#   annotate("segment", x = ranges.regen[1], xend = ranges.regen[2], y = 50, yend = 50) +
#   annotate("text", x = mean(ranges.regen[3:4]), y = 55, label = "Berchtesgaden") + 
#   annotate("segment", x = ranges.regen[3], xend = ranges.regen[4], y = 60, yend = 60) +
#   annotate("text", x = mean(ranges.regen[5:6]), y = 65, label = "Grand Teton") + 
#   annotate("segment", x = ranges.regen[5], xend = ranges.regen[6], y = 70, yend = 70) +
#   theme(legend.position = "top")
# dev.off()
# 
# 
# # relative regeneration rate
# regenIncrease.id <- regen.dyn.df %>%  
#   filter(climate=="baseline") %>% 
#   full_join(regen.dyn.df %>% 
#               filter(size==1, freq==1, fecundity==100, browsing==1) %>% 
#               group_by(landscape) %>% 
#               summarise(regen.dyn_ref = mean(regen.dyn)) %>% 
#               dplyr::select(landscape, regen.dyn_ref), multiple = "all", by = "landscape") %>% 
#   mutate(regen.change = (regen.dyn - regen.dyn_ref)/regen.dyn_ref,
#          landscape = factor(landscape, levels=c("stoko", "bgd", "grte"))) %>% 
#   filter(regen.change>0) %>% 
#   mutate(full.id = paste0(landscape, "_", identifier)) %>% 
#   pull(full.id) 
# 
# png("results/figures/Q2_responseLine_regenerationRate_relative_80yrs.png", res=200,
#     height=1300, width=2000)
# regen.dyn.df %>%  
#   filter(climate=="baseline") %>% 
#   full_join(regen.dyn.df %>% 
#               filter(size==1, freq==1, fecundity==100, browsing==1) %>% 
#               group_by(landscape) %>% 
#               summarise(regen.dyn_ref = mean(regen.dyn)) %>% 
#               dplyr::select(landscape, regen.dyn_ref), multiple = "all", by = "landscape") %>% 
#   mutate(regen.change = (regen.dyn - regen.dyn_ref)/regen.dyn_ref,
#          landscape = factor(landscape, levels=c("stoko", "bgd", "grte"))) %>% 
#   mutate(full.id = paste0(landscape, "_", identifier)) %>%
#   # filter(full.id %ni% regenIncrease.id) %>%
#   ggplot(aes(x = regen.change*100, y = 100-(value*100), col = name)) +
#   geom_point(size = 0.1, alpha = 0.5) +
#   geom_smooth(method = "loess", se = FALSE) +
#   facet_grid(~landscape) +
#   scale_x_reverse() +
#   ylim(0, 100) +
#   scale_color_manual(values=response.colors) +
#   labs(y = "Landscape changed [%]", col = "Response",
#        x = paste0("Percent change in regeneration rate [%]\nRates based on the first ", unique(regen.dyn.df$n_year)," simulation years"),
#        title="All runs with an increase in regeneration rate (looking at you BGD...) filtered out"
#   ) +
#   theme_bw() +
#   theme(legend.position = "top")
# dev.off()
# 
# 
# ## Relationship dist and regen rate ####
# 
# png("results/figures/Q0_relationship_regenRate_distRate.png", res=200,
#     height=1300, width=2000)
# dyn.df %>% 
#   filter(name == "1. Structure\nBasal area decreased by >50 % from reference",
#          climate=="baseline") %>%  
#   mutate(landscape = factor(landscape, levels=c("stoko", "bgd", "grte"))) %>% 
#   mutate(landscape = case_match(landscape, "bgd"~"Berchtesgaden", "stoko"~"Shiretoko", "grte"~"Grand Teton"),
#          landscape = factor(landscape, levels=c("Shiretoko", "Berchtesgaden", "Grand Teton"))) %>% 
#   ggplot(aes(x=dist.dyn, y=regen.dyn, col=landscape)) +
#   geom_point() +
#   facet_grid(~landscape, scales="free") +
#   # scale_y_reverse() +
#   labs(x="Disturbance rate [based on first 80 yrs, % yr^-1]", y="Regeneration rate\n[based on all 80 yrs, recruited trees per ha yr^-1]") +
#   theme_bw() +
#   scale_color_manual(values=colors.landscape) +
#   theme(legend.position = "none")
# dev.off()
# 
# # no facet
# png("results/figures/Q0_relationship_regenRate_distRate_noFacet.png", res=200,
#     height=1300, width=2000)
# dyn.df %>% 
#   filter(name == "1. Structure\nBasal area decreased by >50 % from reference",
#          climate=="baseline") %>%  
#   # mutate(full.id = paste0(landscape, "_", identifier)) %>% 
#   # filter(full.id %ni% regenIncrease.id) %>% 
#   mutate(landscape = case_match(landscape, "bgd"~"Berchtesgaden", "stoko"~"Shiretoko", "grte"~"Grand Teton"),
#          landscape = factor(landscape, levels=c("Shiretoko", "Berchtesgaden", "Grand Teton"))) %>% 
#   ggplot(aes(x=dist.dyn, y=regen.dyn, col=landscape)) +
#   geom_point(col="black", size=0.7) +
#   geom_point(alpha=0.6, size=0.5) +
#   geom_smooth(linewidth=2, method="lm") +
#   scale_x_log10(breaks = c(0.0001, 0.001, 0.01, 0.1, 1, 10)*2, 
#                 label = c(0.0001, 0.001, 0.01, 0.1, 1, 10)*2) +
#   labs(x="Disturbance rate [based on all 80 yrs, % yr^-1]", col="Landscape",
#        y="Regeneration rate\n[based on all 80 yrs, recruited trees per ha yr^-1]") +
#   scale_color_manual(values=colors.landscape) +
#   theme_bw() 
# dev.off()
# 
# 

