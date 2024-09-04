# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Q1: how important are the processes? ###########################################################################################################################################
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Each process by themselves ####
overtime.ls <- readRDS("results/datasets/overtime.ls.RDATA")

### baseline climate ####

# all processes in one panel

singleProcess.df <- bind_rows(overtime.ls[["bgd"]], overtime.ls[["grte"]],overtime.ls[["stoko"]]) %>% 
  filter(year==80, climate=="baseline") %>% 
  filter(freq==1, fecundity==100, browsing==1) %>% 
  pivot_longer(cols=10:12) %>% 
  group_by(landscape, size, name, rep) %>% 
  summarise(value=mean(value)) %>% ungroup() %>% 
  rename(mod = size, 'Disturbance size'=value) %>% 
  full_join(bind_rows(overtime.ls[["bgd"]], overtime.ls[["grte"]],overtime.ls[["stoko"]]) %>% 
              filter(year==80, climate=="baseline") %>% 
              filter(size==1, fecundity==100, browsing==1) %>% 
              pivot_longer(cols=10:12) %>% 
              group_by(landscape, freq, name, rep) %>% 
              summarise(value=mean(value)) %>% ungroup() %>% 
              rename(mod=freq, 'Disturbance frequency'=value)) %>% 
  full_join(bind_rows(overtime.ls[["bgd"]], overtime.ls[["grte"]],overtime.ls[["stoko"]]) %>% 
              filter(year==80, climate=="baseline") %>% 
              filter(size==1, freq==1, browsing==1) %>% 
              pivot_longer(cols=10:12) %>% 
              group_by(landscape, fecundity, name, rep) %>% 
              summarise(value=mean(value)) %>% ungroup() %>% 
              rename(mod=fecundity, 'Seed availability decrease'=value) %>% 
              mutate(mod=case_match(mod, "100"~"1", "50"~"2", "20"~"5", "10"~"10"))) %>% 
  full_join(bind_rows(overtime.ls[["bgd"]], overtime.ls[["grte"]],overtime.ls[["stoko"]]) %>% 
              filter(year==80, climate=="baseline") %>% 
              filter(size==1, freq==1, fecundity==100) %>% 
              pivot_longer(cols=10:12) %>% 
              group_by(landscape, browsing, name, rep) %>% 
              summarise(value=mean(value)) %>% ungroup() %>% 
              rename(mod=browsing, 'Sapling height growth limitations'=value)) %>%
  pivot_longer(cols=5:8, names_to = "process") %>% 
  mutate(value=100-value*100,
         landscape = case_match(landscape, "bgd"~"Berchtesgaden", "stoko"~"Shiretoko", "grte"~"Grand Teton"),
         landscape = factor(landscape, levels=c("Shiretoko", "Berchtesgaden", "Grand Teton"))) 
singleProcess.mean <- singleProcess.df %>% 
  group_by(landscape, mod, name, process) %>% 
  summarise(value=mean(value)) %>% ungroup() 

png("results/figures/Q1_singleProcesses.png", res=200,
    height=1200, width=2000)
singleProcess.df %>%
  ggplot(aes(x=as.numeric(mod), y=value, col=process)) +
  geom_line(aes(group=paste(rep, process)),
            linewidth=0.2, alpha=0.6, show.legend=T) +
  geom_line(data=singleProcess.mean, linewidth=1, alpha=1, show.legend=T) +
  facet_grid(landscape~name, scales="free_y", switch = "y") +
  labs(y="Landscape changed [%]", x="Modification", col="Landscape") +
  scale_x_continuous(labels=c("Ref.", "*2", "*5", "*10"), breaks=c(1,2,5,10)) +
  scale_color_manual(values=c('Disturbance size' = "#b35806",
                              'Disturbance frequency' = "#f46d43",
                              'Seed availability decrease'= "#542788",
                              'Sapling height growth limitations'="#2166ac")) +
  theme_bw()
dev.off()
rm(singleProcess.df, singleProcess.mean)

# Continuous rate plots (formerly response line plots) ####

overtime.ls <- readRDS("results/datasets/overtime.ls.RDATA")
patches <- bind_rows(readRDS("results/datasets/patch_bgd_backup.RDATA"),
                     readRDS("results/datasets/patch_grte_backup.RDATA"),
                     readRDS("results/datasets/patch_stoko_backup.RDATA")); head(patches)

patchlist <- bind_rows(readRDS("results/datasets/patchlist_bgd.RDATA"),
                       readRDS("results/datasets/patchlist_grte.RDATA"),
                       readRDS("results/datasets/patchlist_stoko.RDATA")); head(patchlist)

# actually forested area: correction factor for GRTE
forest.ha.df <- patches %>% 
  inner_join(patchlist) %>% 
  inner_join(areas) %>%
  # filter(agent %in% c("wind", "fire")) %>% # only unspecific agents 
  mutate(pct=n_cells/n_cells_soll) %>% # what pct of targeted cells was actually disturbed? (proxy for forested area)
  group_by(year, landscape, climate, size, freq, browsing, fecundity, rep) %>% 
  summarise(area=mean(area),
            # calculate weighted.mean -> bigger patches should be more reliable
            # forest.ha=weighted.mean(pct, n_cells)*mean(area), # mean of pct disturbed multiplied by landscape area 
            forest.ha = mean(pct)*mean(area), 
            n_patches = length(unique(patchID))) %>% ungroup() %>% 
  mutate(size = factor(size, levels = rev(c("10", "5", "2", "1"))),
         freq = factor(freq, levels = rev(c("10", "5", "2", "1"))),
         fecundity = factor(fecundity, levels = c("100", "50", "20", "10")),
         browsing = factor(browsing, levels = rev(c("10", "5", "2", "1"))),
         forest.ha = ifelse(landscape != "grte", area, forest.ha), # only apply this correction for GRTE
         forest.ha = ifelse(is.na(forest.ha), area, forest.ha)); head(forest.ha.df); summary(forest.ha.df)

patch.df <- patches %>% 
  filter(killed_ba > 0) %>% 
  group_by(landscape, climate, rep, size, freq, browsing, fecundity, year) %>% # don't group by agent
  summarise(area_disturbed = sum(n_cells)) %>%
  mutate(size = factor(size, levels = rev(c("10", "5", "2", "1"))),
         freq = factor(freq, levels = rev(c("10", "5", "2", "1"))),
         fecundity = factor(fecundity, levels = c("100", "50", "20", "10")),
         browsing = factor(browsing, levels = rev(c("10", "5", "2", "1")))) %>% 
  inner_join(forest.ha.df) %>% 
  mutate(dist.rate = area_disturbed/forest.ha, # already in % (because area_disturbed in 100 m^2, forest.ha in ha
         dist.rate = ifelse(dist.rate > 100, 100, dist.rate)) %>% 
  mutate(keep = ifelse(forest.ha > area*0.1, "yes", "no")) %>% # exclude years where forest area drops below 10%
  filter(keep == "yes"); summary(patch.df) 
# combine with vegetation data
dist.dyn.df <- bind_rows(overtime.ls[["bgd"]], overtime.ls[["grte"]],overtime.ls[["stoko"]]) %>% 
  filter(year==80) %>% dplyr::select(-year) %>% 
  full_join(patch.df %>% 
              group_by(climate, size, freq, browsing, fecundity, landscape, rep, area) %>% # take mean over all simulation years
              summarise(n_year = length(unique(year)),
                        dist.dyn = mean(dist.rate)), 
            by=c("climate", "rep", "size", "freq", "browsing", "fecundity", "landscape")) %>% 
  pivot_longer(9:11) %>% 
  mutate(dist.dyn = ifelse(is.na(dist.dyn), 0, dist.dyn)); summary(dist.dyn.df)

# regeneration rate
regen.df <- bind_rows(readRDS("results/datasets/regen_bgd_backup.RDATA"),
                      readRDS("results/datasets/regen_grte_backup.RDATA"),
                      readRDS("results/datasets/regen_stoko_backup.RDATA")) %>% 
  mutate(size = factor(size, levels = rev(c("10", "5", "2", "1"))),
         freq = factor(freq, levels = rev(c("10", "5", "2", "1"))),
         fecundity = factor(fecundity, levels = c("100", "50", "20", "10")),
         browsing = factor(browsing, levels = rev(c("10", "5", "2", "1")))) %>% 
  inner_join(forest.ha.df) %>% 
  mutate(correction = area/forest.ha, 
         recruited_mean = recruited_mean * correction); summary(regen.df)
head(regen.df)
# combine with vegetation data
regen.dyn.df <- bind_rows(overtime.ls[["bgd"]], overtime.ls[["grte"]],overtime.ls[["stoko"]]) %>% 
  filter(year==80) %>% dplyr::select(-year) %>% 
  full_join(regen.df %>% 
              group_by(climate, size, freq, browsing, fecundity, landscape, rep, area) %>% 
              summarise(regen.dyn = mean(recruited_mean),
                        n_year = length(unique(year))), 
            by=c("climate", "rep", "size", "freq", "browsing", "fecundity", "landscape")) %>% 
  pivot_longer(9:11) %>% 
  mutate(regen.dyn = ifelse(is.na(regen.dyn), 0, 
                            ifelse(is.infinite(regen.dyn), 0, regen.dyn))) 

dyn.df <- dist.dyn.df %>% 
  dplyr::select(-n_year) %>% 
  full_join(regen.dyn.df) %>% 
  drop_na(); unique(dyn.df$climate) # get rid of 1 run (grte, "baseline_rep1_size1_freq10_browsing1_fecundity10")

rm(patches, patchlist, forest.ha.df)


# disturbance rate per year
png("results/figures/Q0_disturbanceRate_5x5_newMethod.png", res=200,
    height=1300, width=2000)
patch.df %>% 
  mutate(dist.rate = ifelse(dist.rate > 100, 100, dist.rate)) %>% 
  filter(climate=="baseline", size==5, freq==5, browsing==1, fecundity==100) %>% 
  group_by(year, landscape, rep) %>% 
  summarise(dist.rate = mean(dist.rate)) %>% ungroup() %>% 
  mutate(landscape=factor(landscape, levels=c("stoko", "bgd", "grte"))) %>% 
  ggplot(aes(x=year, y=dist.rate, group=rep)) +
  geom_line(linewidth=0.2) +
  facet_wrap(~landscape, scales="free_y") +
  labs(x="Sim. year", y="Disturbance rate [% yr^-1]", 
       title="Simulated disturbance rates\nbaseline size*5 freq*5 browsing*1 fecundity*100 reps 1:5") +
  theme_bw()
dev.off()
# disturbance rate over full sim. period in grte: low (max ~4%)
# explanation: very little disturbances possible in later simulation years (no forest left to be disturbed) 
# -> calculate disturbance rate only based on first 5-10 simulation years?


# regeneration rate
regen.plot <- regen.df %>% 
  filter(climate=="baseline", size==1, freq==1, browsing==5, fecundity==20) %>% 
  group_by(year, landscape, rep) %>% 
  summarise(regen.rate = mean(recruited_mean)) %>% ungroup() %>% 
  mutate(landscape=factor(landscape, levels=c("stoko", "bgd", "grte"))) 
regen.plot.mean <- regen.plot %>% group_by(landscape) %>% summarise(regen.rate = mean(regen.rate)) %>% ungroup()
png("results/figures/Q0_regenerationRate_5x5_newMethod.png", res=200,
    height=1300, width=2000)
regen.plot %>% 
  ggplot(aes(x=year, y=regen.rate, group=rep)) +
  geom_line(linewidth=0.2) +
  geom_hline(data=regen.plot.mean, aes(yintercept=regen.rate)) +
  facet_wrap(~landscape) +
  labs(x="Sim. year", y="Regeneration rate [mean n trees recruited ha^-1 yr^-1]", 
       title="Simulated regeneration rates\nbaseline size*1 freq*1 browsing*5 fecundity*20 reps 1:5") +
  theme_bw()
dev.off()
rm(regen.plot, regen.plot.mean)

png("results/figures/Q0_regenerationRate_year40.png", res=200,
    height=1300, width=2000)
regen.df %>% 
  filter(climate=="baseline", year==40) %>% 
  group_by(landscape, rep, browsing, fecundity) %>% 
  summarise(regen.rate = mean(recruited_mean)) %>% ungroup() %>% 
  mutate(landscape=factor(landscape, levels=c("stoko", "bgd", "grte"))) %>% 
  mutate(level=paste0("fecundity x ", as.numeric(as.character(fecundity))/100, "\nbrowsing x ", browsing),
         regen=1/as.numeric(as.character(fecundity))*as.numeric(as.character(browsing))) %>% 
  mutate(landscape = case_match(landscape, "bgd"~"Berchtesgaden", "stoko"~"Shiretoko", "grte"~"Grand Teton"),
         landscape = factor(landscape, levels=c("Shiretoko", "Berchtesgaden", "Grand Teton"))) %>% 
  ggplot(aes(x=reorder(level, regen), y=regen.rate, col=landscape, group=paste(rep, landscape))) +
  geom_line(linewidth=1) +
  labs(x="Regeneration modification", y="Regeneration rate [Mean number of tress recruited per ha yr^-1]", 
       title="Simulated regeneration rate\nbaseline climate, simulation year 40, reps 1:5",
       col="Landscape") +
  scale_color_manual(values=colors.landscape) +
  theme_bw()
dev.off()


### Contour map ####
library(plotly)
# https://plotly.com/r/contour-plots/
library(stringr)
library(reshape2)

a <- dyn.df %>% 
  filter(name == names(response.colors)[1], 
         climate=="baseline") %>% 
  dplyr::select(dist.dyn, regen.dyn, value) %>% 
  mutate(value=100-value*100, dist.dyn = log10(dist.dyn)); summary(a) # dist.dyn = log10(dist.dyn)

# beta regression? https://cran.r-project.org/web/packages/betareg/betareg.pdf
gam::gam()


#### fit loess model and predict ####
data.loess <- loess(value ~ dist.dyn * regen.dyn, data = a)
summary(data.loess)
# Create a sequence of incrementally increasing (by 0.3 units) values for both wt and hp
xgrid <-  seq(min(a$dist.dyn), max(a$dist.dyn), 0.3)
ygrid <-  seq(min(a$regen.dyn), max(a$regen.dyn), 0.3)
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
                                                 ifelse(value < 0, 0, value)))
plot_ly(mtrx.melt, x = ~dist.dyn, y = ~regen.dyn, z = ~value, type = "contour") # "trial_loess_predictions.png"

i<-1; clim<-"baseline"
for (clim in c("baseline", "hotdry")) {
  for (i in 1:3) {
    a <- dyn.df %>% 
      filter(name == names(response.colors)[i], 
             climate==clim) %>% 
      dplyr::select(dist.dyn, regen.dyn, value) %>% 
      mutate(value=100-value*100, dist.dyn = log10(dist.dyn))
    print(summary(a))
    
    #### fit loess model and predict ####
    data.loess <- loess(value ~ dist.dyn * regen.dyn, data = a)
    # Create a sequence of incrementally increasing (by 0.3 units) values for both wt and hp
    xgrid <-  seq(min(a$dist.dyn), max(a$dist.dyn), 0.3)
    ygrid <-  seq(min(a$regen.dyn), max(a$regen.dyn), 0.3)
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
                                                     ifelse(value < 0, 0, value)))
    png(paste0("results/figures/Q1_contourPlot_loess_", i, "_", clim, ".png"), res=200,
        height=1300, width=2000)
    p <- ggplot2::ggplot(data = mtrx.melt, aes(x = dist.dyn, y = regen.dyn, fill = value)) +
      ggplot2::geom_tile(alpha = 0.75) +
      ggplot2::scale_fill_viridis_c(option = 'turbo', name = "Predicted response") +
      ggplot2::geom_point(data = a, aes(x = dist.dyn, y = regen.dyn), color = 'white', size = 0.2, inherit.aes = FALSE) +
      ggplot2::geom_contour(ggplot2::aes(z = value), color = 'black', linewidth = 0.75, linetype = 'dashed') +
      ggplot2::labs(x = "Disturbance rate",
                    y = "Regeneration rate",
                    title = paste0(clim, " climate: ", names(response.colors)[i])) +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.title = ggplot2::element_text(size = 16),
                     axis.text = ggplot2::element_text(size = 14),
                     legend.title = ggplot2::element_text(size = 14, angle = 0),
                     legend.text = ggplot2::element_text(size = 12),
                     legend.position = 'bottom',
                     legend.box = 'horizontal',
                     legend.box.margin = ggplot2::margin(0, 0, 20, 0)); print(p)
    dev.off(); rm(p, data.loess, a, mtrx.melt, xgrid, ygrid, data.fit, mtrx3d)
  }
}

#### akima interpolation ####
library(akima)
# Perform interpolation
interp_result <- with(a, akima::interp(x = dist.dyn, y = regen.dyn, z = value, 
                                       duplicate = "mean", extrap = TRUE))
# Convert the interpolated result into a data frame for ggplot
interp_df <- with(interp_result, expand.grid(x = x, y = y))
interp_df$z <- as.vector(interp_result$z)
interp_df <- na.omit(interp_df)  # Remove any rows with NA values
ggplot() +
  geom_contour_filled(data=interp_df, aes(x = x, y = y, z = z)) +
  geom_point(data=a, aes(x=dist.dyn, y=regen.dyn), col="white", size=0.2) +
  labs(title = "Contour Plot of Interpolated Values",
       x = "dist.dyn",
       y = "regen.dyn",
       fill = "Value") +
  theme_minimal() #trial_interpolated_akima

#### raw data ####
# expand.grid(dist.dyn=a$dist.dyn, regen.dyn=a$regen.dyn) %>% 
#   full_join(a) %>% 
#   mutate(dist.dyn = round(dist.dyn, 2),
#          regen.dyn = round(regen.dyn, 0)) %>% 
#   group_by(dist.dyn, regen.dyn) %>% 
#   summarise(value = mean(value, na.rm=T)) %>% ungroup() %>% 
#   mutate(value = ifelse(is.na(value), -0.2, value)) %>% 
#   ggplot(aes(x=dist.dyn, y=regen.dyn, z=value)) +
#   # geom_contour(aes(colour = after_stat(level)), bins=10) +
#   geom_contour_filled(binwidth = 20) +
#   # geom_point(data=a.fil, col="black", size=0.2) +
#   labs(title = "Contour Plot of Raw Values",
#        x = "dist.dyn",
#        y = "regen.dyn",
#        fill = "Value") +
#   theme_minimal() 

a.fil <- a %>% filter(dist.dyn < quantile(dist.dyn, 1)); b <- a.fil; b$dist.dyn <- round(b$dist.dyn, 1); b$regen.dyn <- round(b$regen.dyn, 0)
b %>% 
  ggplot(aes(x=dist.dyn, y=regen.dyn, z=value)) +
  # geom_contour(aes(colour = after_stat(level)), bins=10) +
  geom_contour_filled(bins=5) +
  geom_point(data=a.fil, col="black", size=0.2) +
  scale_x_log10() +
  labs(title = "Contour Plot of Raw Values",
       x = "dist.dyn",
       y = "regen.dyn",
       fill = "Value") +
  theme_minimal() 

# really raw data, all responses
png("results/figures/Q0_relationship_regenRate_distRate_indicators.png", res=200,
    height=1300, width=2000)
dyn.df %>% 
  filter(climate=="baseline") %>% 
  ggplot(aes(x=dist.dyn, y=regen.dyn, col=value)) +
  geom_point(size=0.1) +
  geom_smooth(method="loess", se=F) +
  scale_x_log10(breaks = c(0.0001, 0.001, 0.01, 0.1, 1, 10)*2,
                label = c(0.0001, 0.001, 0.01, 0.1, 1, 10)*2) +
  facet_grid(~name) +
  labs(x="Disturbance rate [based on all 80 yrs, % yr^-1]", y="Regeneration rate\n[based on all 80 yrs, recruited trees per ha yr^-1]") +
  theme_bw()
dev.off()  

### categorical regeneration rate ####

i<-1
for (clim in c("baseline", "hotdry")) {
  for (i in 1:3) {
    png(paste0("results/figures/Q1_responseLine_disturbanceRate_categorical_", i, "_", clim, ".png"), res=200,
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
      theme(legend.position = "top")
    print(p)
    dev.off(); rm(p)
  }
}


### disturbance rate ####
# overall mean disturbance rate on x-axis
dist.dyn.df$dist.dyn %>% summary() # chose sensible labels
ranges.dist <- c(range(dist.dyn.df[dist.dyn.df$landscape=="stoko", "dist.dyn"]),
                 range(dist.dyn.df[dist.dyn.df$landscape=="bgd", "dist.dyn"]),
                 range(dist.dyn.df[dist.dyn.df$landscape=="grte", "dist.dyn"]))
png("results/figures/Q1_responseLine_disturbanceRate_80yrs.png", res=200,
    height=1300, width=2000)
dist.dyn.df %>% 
  filter(climate == "baseline") %>% 
  ggplot(aes(x = dist.dyn, y = 100-value*100, col = name)) +
  geom_point(size = 0.1, alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE) +
  scale_x_log10(breaks = c(0.0001, 0.001, 0.01, 0.1, 1, 10, 50)*2, # year 1:80 *0.5, 1:10 *2, 1:5 *5, 1:2 *10
                label = c(0.0001, 0.001, 0.01, 0.1, 1, 10, 50)*2) +
  ylim(0, 100) +
  scale_color_manual(values=response.colors) +
  labs(y = "Landscape changed [%]", col = "Response",
       x = paste0("Simulated disturbance rate [% yr^-1]\nRate based on only the first ", unique(dist.dyn.df$n_year)," simulation years\nAxis log10-transformed")) +
  theme_bw() +
  coord_cartesian(clip="off") +
  annotate("text", x = mean(ranges.dist[1:2]), y = 100-55, label = "Shiretoko") + 
  annotate("segment", x = ranges.dist[1], xend = ranges.dist[2], y = 50, yend = 50) +
  annotate("text", x = mean(ranges.dist[3:4]), y = 100-45, label = "Berchtesgaden") + 
  annotate("segment", x = ranges.dist[3], xend = ranges.dist[4], y = 60, yend = 60) +
  annotate("text", x = mean(ranges.dist[5:6]), y = 100-35, label = "Grand Teton") + 
  annotate("segment", x = ranges.dist[5], xend = ranges.dist[6], y = 70, yend = 70) +
  theme(legend.position = "top")
dev.off()


# relative disturbance rate
png("results/figures/Q1_responseLine_disturbanceRate_relative_80yrs.png", res=200,
    height=1300, width=2000)
dist.dyn.df %>% 
  filter(climate=="baseline") %>% 
  full_join(dist.dyn.df %>% 
              filter(size==1, freq==1, fecundity==100, browsing==1) %>% 
              group_by(landscape) %>% 
              summarise(dist.dyn_ref = mean(dist.dyn)) %>% 
              dplyr::select(landscape, dist.dyn_ref), multiple = "all", by = "landscape") %>% 
  mutate(dist.change = (dist.dyn - dist.dyn_ref)/dist.dyn_ref,
         landscape = factor(landscape, levels=c("stoko", "bgd", "grte"))) %>% 
  ggplot(aes(x = dist.change, y = value*100, col = name)) +
  geom_point(size = 0.1, alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE) +
  facet_grid(~landscape) +
  ylim(0, 100) +
  scale_color_manual(values=response.colors) +
  labs(y = "Landscape unchanged [%]", col = "Response",
       x = paste0("Percent change in disturbance rate [%]\nRates based on only the first ", unique(dist.dyn.df$n_year)," simulation years")) +
  theme_bw() +
  theme(legend.position = "top")
dev.off()


### regeneration rate ####
# overall mean regeneration rate on x-axis
regen.dyn.df$regen.dyn %>% summary() 
ranges.regen <- c(range(regen.dyn.df[regen.dyn.df$landscape=="stoko", "regen.dyn"]),
                  range(regen.dyn.df[regen.dyn.df$landscape=="bgd", "regen.dyn"]),
                  range(regen.dyn.df[regen.dyn.df$landscape=="grte", "regen.dyn"]))

png("results/figures/Q1_responseLine_regenerationRate_80yrs.png", res=200,
    height=1300, width=2000)
regen.dyn.df %>% 
  filter(climate=="baseline") %>% 
  ggplot(aes(x = regen.dyn, y = 100-value*100, col = name)) +
  geom_point(size = 0.1, alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE) +
  ylim(0, 100) +
  scale_x_reverse() +
  scale_color_manual(values=response.colors) +
  labs(y = "Landscape changed [%]", col = "Response",
       x = paste0("Simulated regeneration rate [Mean number of tress recruited per ha yr^-1]\nRate based on the first ", unique(regen.dyn.df$n_year)," simulation years")) +
  theme_bw() +
  coord_cartesian(clip="off") +
  annotate("text", x = mean(ranges.regen[1:2]), y = 45, label = "Shiretoko") + 
  annotate("segment", x = ranges.regen[1], xend = ranges.regen[2], y = 50, yend = 50) +
  annotate("text", x = mean(ranges.regen[3:4]), y = 55, label = "Berchtesgaden") + 
  annotate("segment", x = ranges.regen[3], xend = ranges.regen[4], y = 60, yend = 60) +
  annotate("text", x = mean(ranges.regen[5:6]), y = 65, label = "Grand Teton") + 
  annotate("segment", x = ranges.regen[5], xend = ranges.regen[6], y = 70, yend = 70) +
  theme(legend.position = "top")
dev.off()


# relative regeneration rate
regenIncrease.id <- regen.dyn.df %>%  
  filter(climate=="baseline") %>% 
  full_join(regen.dyn.df %>% 
              filter(size==1, freq==1, fecundity==100, browsing==1) %>% 
              group_by(landscape) %>% 
              summarise(regen.dyn_ref = mean(regen.dyn)) %>% 
              dplyr::select(landscape, regen.dyn_ref), multiple = "all", by = "landscape") %>% 
  mutate(regen.change = (regen.dyn - regen.dyn_ref)/regen.dyn_ref,
         landscape = factor(landscape, levels=c("stoko", "bgd", "grte"))) %>% 
  filter(regen.change>0) %>% 
  mutate(full.id = paste0(landscape, "_", identifier)) %>% 
  pull(full.id) 

png("results/figures/Q1_responseLine_regenerationRate_relative_80yrs.png", res=200,
    height=1300, width=2000)
regen.dyn.df %>%  
  filter(climate=="baseline") %>% 
  full_join(regen.dyn.df %>% 
              filter(size==1, freq==1, fecundity==100, browsing==1) %>% 
              group_by(landscape) %>% 
              summarise(regen.dyn_ref = mean(regen.dyn)) %>% 
              dplyr::select(landscape, regen.dyn_ref), multiple = "all", by = "landscape") %>% 
  mutate(regen.change = (regen.dyn - regen.dyn_ref)/regen.dyn_ref,
         landscape = factor(landscape, levels=c("stoko", "bgd", "grte"))) %>% 
  mutate(full.id = paste0(landscape, "_", identifier)) %>%
  # filter(full.id %ni% regenIncrease.id) %>%
  ggplot(aes(x = regen.change*100, y = 100-(value*100), col = name)) +
  geom_point(size = 0.1, alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE) +
  facet_grid(~landscape) +
  scale_x_reverse() +
  ylim(0, 100) +
  scale_color_manual(values=response.colors) +
  labs(y = "Landscape changed [%]", col = "Response",
       x = paste0("Percent change in regeneration rate [%]\nRates based on the first ", unique(regen.dyn.df$n_year)," simulation years"),
       title="All runs with an increase in regeneration rate (looking at you BGD...) filtered out"
  ) +
  theme_bw() +
  theme(legend.position = "top")
dev.off()


## Relationship dist and regen rate ####

png("results/figures/Q0_relationship_regenRate_distRate.png", res=200,
    height=1300, width=2000)
dyn.df %>% 
  filter(name == "1. Structure\nBasal area decreased by >50 % from reference",
         climate=="baseline") %>%  
  mutate(landscape = factor(landscape, levels=c("stoko", "bgd", "grte"))) %>% 
  mutate(landscape = case_match(landscape, "bgd"~"Berchtesgaden", "stoko"~"Shiretoko", "grte"~"Grand Teton"),
         landscape = factor(landscape, levels=c("Shiretoko", "Berchtesgaden", "Grand Teton"))) %>% 
  ggplot(aes(x=dist.dyn, y=regen.dyn, col=landscape)) +
  geom_point() +
  facet_grid(~landscape, scales="free") +
  # scale_y_reverse() +
  labs(x="Disturbance rate [based on first 80 yrs, % yr^-1]", y="Regeneration rate\n[based on all 80 yrs, recruited trees per ha yr^-1]") +
  theme_bw() +
  scale_color_manual(values=colors.landscape) +
  theme(legend.position = "none")
dev.off()

# no facet
png("results/figures/Q0_relationship_regenRate_distRate_noFacet.png", res=200,
    height=1300, width=2000)
dyn.df %>% 
  filter(name == "1. Structure\nBasal area decreased by >50 % from reference",
         climate=="baseline") %>%  
  # mutate(full.id = paste0(landscape, "_", identifier)) %>% 
  # filter(full.id %ni% regenIncrease.id) %>% 
  mutate(landscape = case_match(landscape, "bgd"~"Berchtesgaden", "stoko"~"Shiretoko", "grte"~"Grand Teton"),
         landscape = factor(landscape, levels=c("Shiretoko", "Berchtesgaden", "Grand Teton"))) %>% 
  ggplot(aes(x=dist.dyn, y=regen.dyn, col=landscape)) +
  geom_point(col="black", size=0.7) +
  geom_point(alpha=0.6, size=0.5) +
  geom_smooth(linewidth=2, method="lm") +
  scale_x_log10(breaks = c(0.0001, 0.001, 0.01, 0.1, 1, 10)*2, 
                label = c(0.0001, 0.001, 0.01, 0.1, 1, 10)*2) +
  labs(x="Disturbance rate [based on all 80 yrs, % yr^-1]", col="Landscape",
       y="Regeneration rate\n[based on all 80 yrs, recruited trees per ha yr^-1]") +
  scale_color_manual(values=colors.landscape) +
  theme_bw() 
dev.off()


### Surface 3D: both rates ####
library(plotly)
library(reticulate); reticulate::use_miniconda('r-reticulate') # to save plotly graphs as png

response.list <- list(tibble(), tibble(), tibble()); names(response.list) <- names(response.colors); surface.list <- list(response.list, response.list)
names(surface.list) <- c("baseline", "hotdry"); rm(response.list)
# loop for filling list with all 6 combinations (2 climates x 3 responses)
clim <- "baseline"; resp <- names(response.colors)[1]
for (clim in names(surface.list)) {
  for (resp in names(response.colors)) {
    surface.list[[clim]][[resp]] <- dist.dyn.df %>% dplyr::select(-n_year) %>% full_join(regen.dyn.df %>% dplyr::select(-n_year)) %>% drop_na() %>% 
      filter(climate==clim, 
             name==resp) %>% 
      mutate(value=100-value*100) %>% 
      dplyr::select(dist.dyn, regen.dyn, value) %>% 
      distinct()
  }
}

# raw data
p <- plot_ly(x = ~surface.list[["baseline"]][[1]]$dist.dyn, y = ~surface.list[["baseline"]][[1]]$regen.dyn, 
             z = ~surface.list[["baseline"]][[1]]$value, intensity = ~surface.list[["baseline"]][[1]]$value, type = 'mesh3d') %>%
  layout(title = 'Raw data', 
         scene = list(xaxis = list(title = "Disturbance rate"), yaxis = list(title = "Regeneration rate"), 
                      zaxis = list(title = "Landscape changed [structure, %]"), coloraxis = list(title = "this does NOT work (yet)"))); p
save_image(p, file = paste0("results/figures/surface_1_baseline_rawData.png"), scale=1, width=1000, height=1000); rm(p)

# lm model
mod1 <- lm(surface.list[["baseline"]][[1]]$value ~ surface.list[["baseline"]][[1]]$dist.dyn + surface.list[["baseline"]][[1]]$regen.dyn)
summary(mod1)
disturbance_rate <-mod1$model[,1]; regeneration_rate <- mod1$model[,2]; structural_change <-mod1$fitted.values; plot_ly(
  x = ~disturbance_rate, y = ~regeneration_rate, z = ~structural_change, intensity = ~structural_change, type = 'mesh3d') %>%
  layout(title = 'Linear model', 
         scene = list(xaxis = list(title = "Disturbance rate"), yaxis = list(title = "Regeneration rate"), 
                      zaxis = list(title = "Landscape changed [structure, %]"), coloraxis = list(title = "this does NOT work (yet)")))

# loess model
mod2 <- loess(surface.list[["baseline"]][[1]]$value ~ surface.list[["baseline"]][[1]]$dist.dyn + surface.list[["baseline"]][[1]]$regen.dyn)
disturbance_rate <-mod2$x[,1]; regeneration_rate <- mod2$x[,2]; structural_change <-mod2$fitted
plot_ly(
  x = ~disturbance_rate, y = ~regeneration_rate, z = ~structural_change, intensity = ~structural_change, type = 'mesh3d') %>%
  layout(title = 'Loess model', 
         scene = list(xaxis = list(title = "Disturbance rate"), yaxis = list(title = "Regeneration rate"), 
                      zaxis = list(title = "Landscape changed [structure, %]"), coloraxis = list(title = "this does NOT work (yet)")))

# loop for loess models
clim <- "baseline"; resp <- 2
for (clim in names(surface.list)) {
  for (resp in 1:3) {
    # raw data
    p <- plot_ly(x = ~surface.list[[clim]][[resp]]$dist.dyn, y = ~surface.list[[clim]][[resp]]$regen.dyn, 
                 z = ~surface.list[[clim]][[resp]]$value, intensity = ~surface.list[[clim]][[resp]]$value, type = 'mesh3d') %>%
      layout(title = paste0(clim, " - ", names(response.colors)[resp], '\nRaw data'),
             scene = list(xaxis = list(title = "Disturbance rate"), yaxis = list(title = "Regeneration rate"), 
                          zaxis = list(title = "Landscape changed [structure, %]"), coloraxis = list(title = "this does NOT work (yet)"))); p
    save_image(p, file = paste0("results/figures/surface_raw_", resp, "_", clim, ".png"), scale=1, width=1000, height=1000); rm(p)
    
    # loess model
    mod2 <- loess(surface.list[[clim]][[resp]]$value ~ surface.list[[clim]][[resp]]$dist.dyn + surface.list[[clim]][[resp]]$regen.dyn)
    disturbance_rate <-mod2$x[,1]; regeneration_rate <- mod2$x[,2]; change <-mod2$fitted; change[change>100] <- 100; change[change<0] <- 0
    p1 <- plot_ly(
      x = ~disturbance_rate, y = ~regeneration_rate, z = ~change, intensity = ~change, type = 'mesh3d') %>%
      layout(title = paste0(clim, " - ", names(response.colors)[resp], '\nLoess model'), 
             scene = list(xaxis = list(title = "Disturbance rate"), yaxis = list(title = "Regeneration rate"), 
                          zaxis = list(title = "Landscape changed [%]"), coloraxis = list(title = "this does NOT work (yet)")))
    save_image(p1, file = paste0("results/figures/surface_loess_", resp, "_", clim, ".png"), scale=1, width=1000, height=1000)
    rm(mod2, disturbance_rate, regeneration_rate, change, p1)
  }
}


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# DISCARDED ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

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

# pie.sizeBrowsing.df <- bind_rows(overtime.ls[["bgd"]], overtime.ls[["grte"]],overtime.ls[["stoko"]]) %>% 
#   filter(climate=="baseline", year==80) %>% 
#   mutate(across(10:12, ~ abs(.x-1)*100)) %>%  # transform to % landscape changed
#   group_by(size, browsing) %>% 
#   summarise(across(8:10, sum)) %>% ungroup() %>% 
#   mutate(across(3:5, ~ .x/24000)) %>% # maximum sum = 24000 (100 % * 5 reps * 16 regen scenarios * 3 landscapes)
#   rename('1. Structure\nBasal area decreased by >50 % from reference' = 3,
#          '2. Composition\nDominant species changed from reference' = 4,
#          '3. Remaining forest\nStem density dropping below 50 trees/ha' = 5) %>% 
#   pivot_longer(3:5) %>% 
#   mutate(size = paste0("Size * ", size), browsing = paste0("Browsing * ", browsing),
#          size = factor(size, levels=c(paste0("Size * ", c(1, 2, 5, 10)))),
#          browsing = factor(browsing, levels=c(paste0("Browsing * ", c(1, 2, 5, 10)))))
# 
# pie.sizeFecundity.df <- bind_rows(overtime.ls[["bgd"]], overtime.ls[["grte"]],overtime.ls[["stoko"]]) %>% 
#   filter(climate=="baseline", year==80) %>% 
#   mutate(across(10:12, ~ abs(.x-1)*100)) %>%  # transform to % landscape changed
#   group_by(size, fecundity) %>% 
#   summarise(across(8:10, sum)) %>% ungroup() %>% 
#   mutate(across(3:5, ~ .x/24000)) %>% # maximum sum = 24000 (100 % * 5 reps * 16 regen scenarios * 3 landscapes)
#   rename('1. Structure\nBasal area decreased by >50 % from reference' = 3,
#          '2. Composition\nDominant species changed from reference' = 4,
#          '3. Remaining forest\nStem density dropping below 50 trees/ha' = 5) %>% 
#   pivot_longer(3:5) %>% 
#   mutate(size = paste0("Size * ", size), fecundity = paste0("Fecundity * ", fecundity, "%"),
#          size = factor(size, levels=c(paste0("Size * ", c(1, 2, 5, 10)))),
#          fecundity = factor(fecundity, levels=c(paste0("Fecundity * ", c(100, 50, 20, 10), "%"))))
# 
# pie.freqBrowsing.df <- bind_rows(overtime.ls[["bgd"]], overtime.ls[["grte"]],overtime.ls[["stoko"]]) %>% 
#   filter(climate=="baseline", year==80) %>% 
#   mutate(across(10:12, ~ abs(.x-1)*100)) %>%  # transform to % landscape changed
#   group_by(freq, browsing) %>% 
#   summarise(across(8:10, sum)) %>% ungroup() %>% 
#   mutate(across(3:5, ~ .x/24000)) %>% # maximum sum = 24000 (100 % * 5 reps * 16 regen scenarios * 3 landscapes)
#   rename('1. Structure\nBasal area decreased by >50 % from reference' = 3,
#          '2. Composition\nDominant species changed from reference' = 4,
#          '3. Remaining forest\nStem density dropping below 50 trees/ha' = 5) %>% 
#   pivot_longer(3:5) %>% 
#   mutate(freq = paste0("Frequency * ", freq), browsing = paste0("Browsing * ", browsing),
#          freq = factor(freq, levels=c(paste0("Frequency * ", c(1, 2, 5, 10)))),
#          browsing = factor(browsing, levels=c(paste0("Browsing * ", c(1, 2, 5, 10)))))
# 
# pie.freqFecundity.df <- bind_rows(overtime.ls[["bgd"]], overtime.ls[["grte"]],overtime.ls[["stoko"]]) %>% 
#   filter(climate=="baseline", year==80) %>% 
#   mutate(across(10:12, ~ abs(.x-1)*100)) %>%  # transform to % landscape changed
#   group_by(freq, fecundity) %>% 
#   summarise(across(8:10, sum)) %>% ungroup() %>% 
#   mutate(across(3:5, ~ .x/24000)) %>% # maximum sum = 24000 (100 % * 5 reps * 16 regen scenarios * 3 landscapes)
#   rename('1. Structure\nBasal area decreased by >50 % from reference' = 3,
#          '2. Composition\nDominant species changed from reference' = 4,
#          '3. Remaining forest\nStem density dropping below 50 trees/ha' = 5) %>% 
#   pivot_longer(3:5) %>% 
#   mutate(freq = paste0("Frequency * ", freq), fecundity = paste0("Fecundity * ", fecundity, "%"),
#          freq = factor(freq, levels=c(paste0("Frequency * ", c(1, 2, 5, 10)))),
#          fecundity = factor(fecundity, levels=c(paste0("Fecundity * ", c(100, 50, 20, 10), "%"))))

# ### Pixels ###
# index<-1
# pixel.dist.fc <- function(index) {
#   pie.dist.df %>% 
#     filter(name == names(response.colors)[index]) %>% 
#     ggplot(aes(x=1, y=1, fill=value*100)) +
#     geom_tile() +
#     geom_text(aes(y=1,label=round(value*100,1))) +
#     facet_grid(rows=vars(size), cols=vars(freq), as.table = F, switch = "both") +
#     scale_fill_gradient(low="white", high=response.colors[index], limits=c(0,max(pie.dist.df$value)*100)) +
#     theme_minimal() +
#     coord_equal() +
#     scale_x_discrete(position = "top") +
#     labs(fill="", x=as.character(names(response.colors)[index]), y="") +
#     theme(axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
#           axis.text.x=element_blank(), axis.text.y=element_blank(),
#           legend.position = "bottom") 
# }; pixel.dist.ls <- lapply(1:3, pixel.dist.fc)
# pixel.dist <- ggpubr::ggarrange(pixel.dist.ls[[1]], pixel.dist.ls[[2]], pixel.dist.ls[[3]], nrow=1) %>% 
#   annotate_figure(top="Disturbance processes")
# pixel.regen.fc <- function(index) {
#   pie.regen.df %>% 
#     filter(name == names(response.colors)[index]) %>% 
#     ggplot(aes(x=1, y=1, fill=value*100)) +
#     geom_tile() +
#     geom_text(aes(y=1,label=round(value*100,1))) +
#     facet_grid(rows=vars(fecundity), cols=vars(browsing), as.table = F, switch = "both") +
#     scale_fill_gradient(low="white", high=response.colors[index], limits=c(0,max(pie.dist.df$value)*100)) +
#     theme_minimal() +
#     coord_equal() +
#     scale_x_discrete(position = "top") +
#     labs(fill="", x=as.character(names(response.colors)[index]), y="") +
#     theme(axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
#           axis.text.x=element_blank(), axis.text.y=element_blank(),
#           legend.position = "bottom") 
# }; pixel.regen.ls <- lapply(1:3, pixel.regen.fc)
# pixel.regen <- ggpubr::ggarrange(pixel.regen.ls[[1]], pixel.regen.ls[[2]], pixel.regen.ls[[3]], nrow=1) %>% 
#   annotate_figure(top="Regeneration processes")
# 
# png("results/figures/Q1_pixel.png", res=200,
#     height = 2000, width = 2800)
# print(
#   ggpubr::ggarrange(pixel.dist, pixel.regen, nrow = 2)
# )
# rm(pixel.dist.fc, pixel.dist.ls, pixel.regen.fc, pixel.regen.ls, pixel.dist, pixel.regen)
# dev.off()

# # other combinations
# pie.sizeBrowsing.fc <- function(var) {
#   pie.sizeBrowsing.df %>% 
#     filter(name == var) %>%
#     ggplot(aes(x=0, y=value*100, fill=name)) +
#     geom_bar(stat="identity") +
#     geom_text(aes(y=1,label=round(value*100,1))) +
#     facet_grid(rows=vars(size), cols=vars(browsing), as.table = F, switch = "both") +
#     coord_polar("x") + # "y": pie chart but all same size, "x": weird drop pattern chart, but different sizes
#     labs(fill="", x="", y="") +
#     ylim(0,100) + # max value: 0.55
#     scale_fill_manual(values=response.colors) +
#     theme_bw() +
#     theme(axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
#           axis.text.x=element_blank(), axis.text.y=element_blank(),
#           legend.position = "top") 
# }; pie.sizeBrowsing.ls <- lapply(names(response.colors), pie.sizeBrowsing.fc)
# 
# png("results/figures/Q1_pie_sizeBrowsing.png", res=200,
#     height = 1000, width = 2500)
# print(
#   ggpubr::ggarrange(pie.sizeBrowsing.ls[[1]], pie.sizeBrowsing.ls[[2]], pie.sizeBrowsing.ls[[3]], nrow=1) %>% 
#     annotate_figure()
# ); rm(pie.sizeBrowsing.fc, pie.sizeBrowsing.ls)
# dev.off()
# 
# pie.sizeFecundity.fc <- function(var) {
#   pie.sizeFecundity.df %>% 
#     filter(name == var) %>%
#     ggplot(aes(x=0, y=value*100, fill=name)) +
#     geom_bar(stat="identity") +
#     geom_text(aes(y=1,label=round(value*100,1))) +
#     facet_grid(rows=vars(size), cols=vars(fecundity), as.table = F, switch = "both") +
#     coord_polar("x") + # "y": pie chart but all same size, "x": weird drop pattern chart, but different sizes
#     labs(fill="", x="", y="") +
#     ylim(0,100) + # max value: 0.55
#     scale_fill_manual(values=response.colors) +
#     theme_bw() +
#     theme(axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
#           axis.text.x=element_blank(), axis.text.y=element_blank(),
#           legend.position = "top") 
# }; pie.sizeFecundity.ls <- lapply(names(response.colors), pie.sizeFecundity.fc)
# 
# png("results/figures/Q1_pie_sizeFecundity.png", res=200,
#     height = 1000, width = 2500)
# print(
#   ggpubr::ggarrange(pie.sizeFecundity.ls[[1]], pie.sizeFecundity.ls[[2]], pie.sizeFecundity.ls[[3]], nrow=1) %>% 
#     annotate_figure()
# ); rm(pie.sizeFecundity.fc, pie.sizeFecundity.ls)
# dev.off()
# 
# pie.freqBrowsing.fc <- function(var) {
#   pie.freqBrowsing.df %>% 
#     filter(name == var) %>%
#     ggplot(aes(x=0, y=value*100, fill=name)) +
#     geom_bar(stat="identity") +
#     geom_text(aes(y=1,label=round(value*100,1))) +
#     facet_grid(rows=vars(freq), cols=vars(browsing), as.table = F, switch = "both") +
#     coord_polar("x") + # "y": pie chart but all same size, "x": weird drop pattern chart, but different sizes
#     labs(fill="", x="", y="") +
#     ylim(0,100) + # max value: 0.55
#     scale_fill_manual(values=response.colors) +
#     theme_bw() +
#     theme(axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
#           axis.text.x=element_blank(), axis.text.y=element_blank(),
#           legend.position = "top") 
# }; pie.freqBrowsing.ls <- lapply(names(response.colors), pie.freqBrowsing.fc)
# 
# png("results/figures/Q1_pie_freqBrowsing.png", res=200,
#     height = 1000, width = 2500)
# print(
#   ggpubr::ggarrange(pie.freqBrowsing.ls[[1]], pie.freqBrowsing.ls[[2]], pie.freqBrowsing.ls[[3]], nrow=1) %>% 
#     annotate_figure()
# ); rm(pie.freqBrowsing.fc, pie.freqBrowsing.ls)
# dev.off()
# 
# pie.freqFecundity.fc <- function(var) {
#   pie.freqFecundity.df %>% 
#     filter(name == var) %>%
#     ggplot(aes(x=0, y=value*100, fill=name)) +
#     geom_bar(stat="identity") +
#     geom_text(aes(y=1,label=round(value*100,1))) +
#     facet_grid(rows=vars(freq), cols=vars(fecundity), as.table = F, switch = "both") +
#     coord_polar("x") + # "y": pie chart but all same size, "x": weird drop pattern chart, but different sizes
#     labs(fill="", x="", y="") +
#     ylim(0,100) + # max value: 0.55
#     scale_fill_manual(values=response.colors) +
#     theme_bw() +
#     theme(axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
#           axis.text.x=element_blank(), axis.text.y=element_blank(),
#           legend.position = "top") 
# }; pie.freqFecundity.ls <- lapply(names(response.colors), pie.freqFecundity.fc)
# 
# png("results/figures/Q1_pie_freqFecundity.png", res=200,
#     height = 1000, width = 2500)
# print(
#   ggpubr::ggarrange(pie.freqFecundity.ls[[1]], pie.freqFecundity.ls[[2]], pie.freqFecundity.ls[[3]], nrow=1) %>% 
#     annotate_figure()
# ); rm(pie.freqFecundity.fc, pie.freqFecundity.ls)
# dev.off()
# 

### Pies ####
# overtime.ls <- readRDS("results/datasets/overtime.ls.RDATA")
# 
# pie.dist.df <- bind_rows(overtime.ls[["bgd"]], overtime.ls[["grte"]],overtime.ls[["stoko"]]) %>% 
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
#          freq = factor(freq, levels=c(paste0("Frequency * ", c(1, 2, 5, 10)))))
# 
# pie.regen.df <- bind_rows(overtime.ls[["bgd"]], overtime.ls[["grte"]],overtime.ls[["stoko"]]) %>% 
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
#          browsing = factor(browsing, levels=c(paste0("Browsing * ", c(1, 2, 5, 10)))))
# 
# 
# ### Mean over landscapes ## ###
# var <- "1. Structure\nBasal area decreased by >50 % from reference"
# # disturbance processes
# pie.dist.fc <- function(var) {
#   pie.dist.df %>% 
#     filter(name == var) %>%
#     ggplot(aes(x=0, y=value*100, fill=name)) +
#     geom_bar(stat="identity") +
#     geom_text(aes(y=1, label = sprintf('%.1f', value*100))) +
#     facet_grid(rows=vars(size), cols=vars(freq), as.table = F, switch = "y") +
#     coord_polar("x") + # "y": pie chart but all same size, "x": weird drop pattern chart, but different sizes
#     labs(fill="", x="", y="") +
#     ylim(0,55) + # max value: 0.55
#     scale_fill_manual(values=response.colors) +
#     theme_bw() +
#     theme(axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
#           axis.text.x=element_blank(), axis.text.y=element_blank(),
#           legend.position = "bottom") 
# }; pie.dist.ls <- lapply(names(response.colors), pie.dist.fc)
# 
# png("results/figures/Q1_pie_disturbance.png", res=200,
#     height = 1000, width = 2500)
# print(
#   ggpubr::ggarrange(pie.dist.ls[[1]], pie.dist.ls[[2]], pie.dist.ls[[3]], nrow=1) %>% 
#     annotate_figure()
# ); rm(pie.dist.fc, pie.dist.ls)
# dev.off()
# 
# 
# # regeneration processes
# pie.regen.fc <- function(var) {
#   pie.regen.df %>% 
#     filter(name == var) %>%
#     ggplot(aes(x=0, y=value*100, fill=name)) +
#     geom_bar(stat="identity") +
#     geom_text(aes(y=1,label= sprintf('%.1f', value*100))) +
#     facet_grid(rows=vars(fecundity), cols=vars(browsing), as.table = F, switch = "y") +
#     coord_polar("x") + 
#     labs(fill="", x="", y="") +
#     ylim(0,55) + # max value: 0.55
#     scale_fill_manual(values=response.colors) +
#     theme_bw() +
#     theme(axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
#           axis.text.x=element_blank(), axis.text.y=element_blank(),
#           legend.position = "bottom") 
# }; pie.regen.ls <- lapply(names(response.colors), pie.regen.fc)
# 
# png("results/figures/Q1_pie_regeneration.png", res=200,
#     height = 1000, width = 2500)
# print(
#   ggpubr::ggarrange(pie.regen.ls[[1]], pie.regen.ls[[2]], pie.regen.ls[[3]], nrow=1) %>% 
#     annotate_figure()
# )
# rm(pie.regen.fc, pie.regen.ls)
# dev.off()
# 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# 
# ## Per landscape # ###
# overtime.ls <- readRDS("results/datasets/overtime.ls.RDATA")
# 
# pie.dist.ls.df <- bind_rows(overtime.ls[["bgd"]], overtime.ls[["grte"]],overtime.ls[["stoko"]]) %>% 
#   filter(climate=="baseline", year==80) %>% 
#   mutate(across(10:12, ~ abs(.x-1)*100)) %>%  # transform to % landscape changed
#   group_by(size, freq, landscape) %>% 
#   summarise(across(7:9, sum)) %>% ungroup() %>% 
#   mutate(across(4:6, ~ .x/8000)) %>% # maximum sum = 8000 (100 % * 5 reps * 16 regen scenarios)
#   rename('1. Structure\nBasal area decreased by >50 % from reference' = 4,
#          '2. Composition\nDominant species changed from reference' = 5,
#          '3. Remaining forest\nStem density dropping below 50 trees/ha' = 6) %>% 
#   pivot_longer(4:6) %>% 
#   mutate(size = paste0("Size * ", size), freq = paste0("Frequency * ", freq),
#          size = factor(size, levels=c(paste0("Size * ", c(1, 2, 5, 10)))),
#          freq = factor(freq, levels=c(paste0("Frequency * ", c(1, 2, 5, 10)))))
# 
# pie.regen.ls.df <- bind_rows(overtime.ls[["bgd"]], overtime.ls[["grte"]],overtime.ls[["stoko"]]) %>% 
#   filter(climate=="baseline", year==80) %>% 
#   mutate(across(10:12, ~ abs(.x-1)*100)) %>%  # transform to % landscape changed
#   group_by(fecundity, browsing, landscape) %>% 
#   summarise(across(7:9, sum)) %>% ungroup() %>% 
#   mutate(across(4:6, ~ .x/8000)) %>% # maximum sum = 8000 (100 % * 5 reps * 16 regen scenarios)
#   rename('1. Structure\nBasal area decreased by >50 % from reference' = 4,
#          '2. Composition\nDominant species changed from reference' = 5,
#          '3. Remaining forest\nStem density dropping below 50 trees/ha' = 6) %>% 
#   pivot_longer(4:6) %>% 
#   mutate(fecundity = paste0("Fecundity * ", fecundity, "%"), browsing = paste0("Browsing * ", browsing),
#          fecundity = factor(fecundity, levels=c(paste0("Fecundity * ", c(100, 50, 20, 10), "%"))),
#          browsing = factor(browsing, levels=c(paste0("Browsing * ", c(1, 2, 5, 10)))))
# 
# var <- "1. Structure\nBasal area decreased by >50 % from reference"
# Landscape <- "stoko"
# i<-1
# for (i in 1:3) {
#   
#   # disturbance processes
#   max.value <- pie.dist.ls.df %>% 
#     filter(landscape == landscapes[i]) %>% 
#     summarise(max = max(value)) %>% pull(max)
#   pie.dist.ls.fc <- function(var, Landscape=landscapes[i]) {
#     pie.dist.ls.df %>% 
#       filter(name == var,
#              landscape == Landscape) %>% 
#       ggplot(aes(x=0, y=value*100, fill=name)) +
#       geom_bar(stat="identity") +
#       geom_text(aes(y=1, label = sprintf('%.1f', value*100))) +
#       facet_grid(rows=vars(size), cols=vars(freq), as.table = F, switch = "both") +
#       coord_polar("x") + # "y": pie chart but all same size, "x": weird drop pattern chart, but different sizes
#       labs(fill="", x="", y="", title=toupper(landscapes[i])) +
#       ylim(0,max.value*100) + # max value: 0.55
#       scale_fill_manual(values=response.colors) +
#       theme_bw() +
#       theme(axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
#             axis.text.x=element_blank(), axis.text.y=element_blank(),
#             legend.position = "top") 
#   }; pie.dist.ls.ls <- lapply(names(response.colors), pie.dist.ls.fc)
#   
#   png(paste0("results/figures/Q1_pie_disturbance_", landscapes[i], ".png"), res=200,
#       height = 1000, width = 2500)
#   print(
#     ggpubr::ggarrange(pie.dist.ls.ls[[1]], pie.dist.ls.ls[[2]], pie.dist.ls.ls[[3]], nrow=1) %>% 
#       annotate_figure()
#   )
#   rm(pie.dist.ls.fc, pie.dist.ls.ls, max.value)
#   dev.off()
#   
#   # regeneration processes
#   max.value <- pie.regen.ls.df %>% 
#     filter(landscape == landscapes[i]) %>% 
#     summarise(max = max(value)) %>% pull(max)
#   pie.regen.ls.fc <- function(var, Landscape=landscapes[i]) {
#     pie.regen.ls.df %>% 
#       filter(name == var,
#              landscape == Landscape) %>% 
#       ggplot(aes(x=0, y=value*100, fill=name)) +
#       geom_bar(stat="identity") +
#       geom_text(aes(y=1, label = sprintf('%.1f', value*100))) +
#       facet_grid(rows=vars(fecundity), cols=vars(browsing), as.table = F, switch = "both") +
#       coord_polar("x") + 
#       labs(fill="", x="", y="") +
#       ylim(0,max.value*100) + # max value: 0.55
#       scale_fill_manual(values=response.colors) +
#       theme_bw() +
#       theme(axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
#             axis.text.x=element_blank(), axis.text.y=element_blank(),
#             legend.position = "top") 
#   }; pie.regen.ls.ls <- lapply(names(response.colors), pie.regen.ls.fc)
#   
#   png(paste0("results/figures/Q1_pie_regeneration_", landscapes[i], ".png"), res=200,
#       height = 1000, width = 2500)
#   print(
#     ggpubr::ggarrange(pie.regen.ls.ls[[1]], pie.regen.ls.ls[[2]], pie.regen.ls.ls[[3]], nrow=1) %>% 
#       annotate_figure()
#   )
#   rm(pie.regen.ls.fc, pie.regen.ls.ls, max.value)
#   dev.off()
#   
# }
# 

# ### Single processes: hot-dry conditions ####
# # disturbances
# png("results/figures/Q1_singleProcesses_disturbance_hotdry.png", res=200,
#     height=1600, width=2200)
# ggpubr::ggarrange(
#   bind_rows(overtime.ls[["bgd"]], overtime.ls[["grte"]],overtime.ls[["stoko"]]) %>% 
#     filter(climate=="hotdry", year==80) %>% 
#     filter(freq==1, fecundity==100, browsing==1) %>% 
#     mutate(size=as.numeric(size)) %>% 
#     pivot_longer(cols=10:12) %>% 
#     full_join(bind_rows(overtime.ls[["bgd"]], overtime.ls[["grte"]],overtime.ls[["stoko"]]) %>% 
#                 filter(climate=="baseline", year==80) %>% 
#                 filter(freq==1, fecundity==100, browsing==1, size==1) %>% 
#                 pivot_longer(cols=10:12) %>% 
#                 mutate(size=0)) %>% 
#     mutate(landscape = case_match(landscape, "bgd"~"Berchtesgaden", "stoko"~"Shiretoko", "grte"~"Grand Teton"),
#            landscape = factor(landscape, levels=c("Shiretoko", "Berchtesgaden", "Grand Teton"))) %>% 
#     ggplot(aes(x=as.numeric(size), y=value*100, col=landscape, group=paste(rep, landscape))) +
#     geom_line() +
#     facet_wrap(~name) +
#     ylim(0,100) +
#     labs(y="Landscape unchanged [%]", x="Disturbance size modification", col="Landscape") +
#     scale_x_continuous(labels=c("*1\nbaseline","*1\nhotdry", "*2\nhotdry", "*5\nhotdry", "*10\nhotdry")) +
#     scale_color_manual(values=colors.landscape) +
#     theme_bw(),
#   bind_rows(overtime.ls[["bgd"]], overtime.ls[["grte"]],overtime.ls[["stoko"]]) %>% 
#     filter(climate=="hotdry", year==80) %>% 
#     filter(size==1, fecundity==100, browsing==1) %>% 
#     mutate(freq=as.numeric(freq)) %>% 
#     pivot_longer(cols=10:12) %>% 
#     full_join(bind_rows(overtime.ls[["bgd"]], overtime.ls[["grte"]],overtime.ls[["stoko"]]) %>% 
#                 filter(climate=="baseline", year==80) %>% 
#                 filter(freq==1, fecundity==100, browsing==1, size==1) %>% 
#                 pivot_longer(cols=10:12) %>% 
#                 mutate(freq=0)) %>% 
#     mutate(landscape = case_match(landscape, "bgd"~"Berchtesgaden", "stoko"~"Shiretoko", "grte"~"Grand Teton"),
#            landscape = factor(landscape, levels=c("Shiretoko", "Berchtesgaden", "Grand Teton"))) %>% 
#     ggplot(aes(x=as.numeric(freq), y=value*100, col=landscape, group=paste(rep, landscape))) +
#     geom_line() +
#     facet_wrap(~name) +
#     ylim(0,100) +
#     labs(y="Landscape unchanged [%]", x="Disturbance frequency modification", col="Landscape") +
#     scale_x_continuous(labels=c("*1\nbaseline","*1\nhotdry", "*2\nhotdry", "*5\nhotdry", "*10\nhotdry")) +
#     scale_color_manual(values=colors.landscape) +
#     theme_bw(),
#   nrow=2, common.legend=T
# )
# dev.off()
# 
# # regeneration
# png("results/figures/Q1_singleProcesses_regeneration_hotdry.png", res=200,
#     height=1600, width=2200)
# ggpubr::ggarrange(
#   bind_rows(overtime.ls[["bgd"]], overtime.ls[["grte"]],overtime.ls[["stoko"]]) %>% 
#     filter(climate=="hotdry", year==80) %>% 
#     filter(freq==1, size==1, browsing==1) %>% 
#     mutate(fecundity=as.numeric(fecundity)) %>% 
#     pivot_longer(cols=10:12) %>% 
#     full_join(bind_rows(overtime.ls[["bgd"]], overtime.ls[["grte"]],overtime.ls[["stoko"]]) %>% 
#                 filter(climate=="baseline", year==80) %>% 
#                 filter(freq==1, fecundity==100, browsing==1, size==1) %>% 
#                 pivot_longer(cols=10:12) %>% 
#                 mutate(fecundity=0)) %>% 
#     mutate(landscape = case_match(landscape, "bgd"~"Berchtesgaden", "stoko"~"Shiretoko", "grte"~"Grand Teton"),
#            landscape = factor(landscape, levels=c("Shiretoko", "Berchtesgaden", "Grand Teton"))) %>% 
#     ggplot(aes(x=as.numeric(fecundity), y=value*100, col=landscape, group=paste(rep, landscape))) +
#     geom_line() +
#     facet_wrap(~name) +
#     ylim(0,100) +
#     labs(y="Landscape unchanged [%]", x="Fecundity modification", col="Landscape") +
#     scale_x_continuous(labels=c("/1\nbaseline","/1\nhotdry", "/2\nhotdry", "/5\nhotdry", "/10\nhotdry")) +
#     scale_color_manual(values=colors.landscape) +
#     theme_bw(),
#   bind_rows(overtime.ls[["bgd"]], overtime.ls[["grte"]],overtime.ls[["stoko"]]) %>% 
#     filter(climate=="hotdry", year==80) %>% 
#     filter(size==1, freq==1, fecundity==100) %>% 
#     mutate(browsing=as.numeric(browsing)) %>% 
#     pivot_longer(cols=10:12) %>% 
#     full_join(bind_rows(overtime.ls[["bgd"]], overtime.ls[["grte"]],overtime.ls[["stoko"]]) %>% 
#                 filter(climate=="baseline", year==80) %>% 
#                 filter(freq==1, fecundity==100, browsing==1, size==1) %>% 
#                 pivot_longer(cols=10:12) %>% 
#                 mutate(browsing=0)) %>% 
#     mutate(landscape = case_match(landscape, "bgd"~"Berchtesgaden", "stoko"~"Shiretoko", "grte"~"Grand Teton"),
#            landscape = factor(landscape, levels=c("Shiretoko", "Berchtesgaden", "Grand Teton"))) %>% 
#     ggplot(aes(x=as.numeric(browsing), y=value*100, col=landscape, group=paste(rep, landscape))) +
#     geom_line() +
#     facet_wrap(~name) +
#     ylim(0,100) +
#     labs(y="Landscape unchanged [%]", x="Browsing modification", col="Landscape") +
#     scale_x_continuous(labels=c("*1\nbaseline","*1\nhotdry", "*2\nhotdry", "*5\nhotdry", "*10\nhotdry")) +
#     scale_color_manual(values=colors.landscape) +
#     theme_bw(),
#   nrow=2, common.legend=T
# )
# dev.off()
# 
# ### Single processes: both climates ####
# # disturbance
# mean.size <- bind_rows(overtime.ls[["bgd"]], overtime.ls[["grte"]],overtime.ls[["stoko"]]) %>% 
#   filter(year==80) %>% 
#   filter(freq==1, fecundity==100, browsing==1) %>% 
#   pivot_longer(cols=10:12) %>% 
#   group_by(landscape, climate, size, name) %>% 
#   summarise(value=mean(value)) %>% ungroup() %>% 
#   mutate(landscape = case_match(landscape, "bgd"~"Berchtesgaden", "stoko"~"Shiretoko", "grte"~"Grand Teton"),
#          landscape = factor(landscape, levels=c("Shiretoko", "Berchtesgaden", "Grand Teton"))) 
# mean.freq <- bind_rows(overtime.ls[["bgd"]], overtime.ls[["grte"]],overtime.ls[["stoko"]]) %>% 
#   filter(year==80) %>% 
#   filter(size==1, fecundity==100, browsing==1) %>% 
#   pivot_longer(cols=10:12) %>% 
#   group_by(landscape, climate, freq, name) %>% 
#   summarise(value=mean(value)) %>% ungroup() %>% 
#   mutate(landscape = case_match(landscape, "bgd"~"Berchtesgaden", "stoko"~"Shiretoko", "grte"~"Grand Teton"),
#          landscape = factor(landscape, levels=c("Shiretoko", "Berchtesgaden", "Grand Teton"))) 
# png("results/figures/Q1_singleProcesses_disturbance_bothClimates.png", res=200,
#     height=1200, width=2000)
# ggpubr::ggarrange(
#   bind_rows(overtime.ls[["bgd"]], overtime.ls[["grte"]],overtime.ls[["stoko"]]) %>% 
#     filter(year==80) %>% 
#     filter(freq==1, fecundity==100, browsing==1) %>% 
#     pivot_longer(cols=10:12) %>% 
#     mutate(landscape = case_match(landscape, "bgd"~"Berchtesgaden", "stoko"~"Shiretoko", "grte"~"Grand Teton"),
#            landscape = factor(landscape, levels=c("Shiretoko", "Berchtesgaden", "Grand Teton"))) %>% 
#     ggplot(aes(x=as.numeric(size), y=value*100, col=landscape, linetype=climate)) +
#     geom_line(aes(group=interaction(rep, landscape, climate)), linewidth=0.3, alpha=0.4, show.legend=F) +
#     geom_line(data=mean.size, linewidth=1.2) +
#     facet_wrap(~name) +
#     ylim(0,100) +
#     labs(y="Landscape unchanged [%]", x="Disturbance size modification", col="Landscape", linetype="Climate") +
#     scale_x_continuous(labels=c("Reference", "*2", "*5", "*10")) +
#     scale_color_manual(values=colors.landscape) +
#     theme_bw(),
#   bind_rows(overtime.ls[["bgd"]], overtime.ls[["grte"]],overtime.ls[["stoko"]]) %>% 
#     filter(year==80) %>% 
#     filter(size==1, fecundity==100, browsing==1) %>% 
#     pivot_longer(cols=10:12) %>% 
#     mutate(landscape = case_match(landscape, "bgd"~"Berchtesgaden", "stoko"~"Shiretoko", "grte"~"Grand Teton"),
#            landscape = factor(landscape, levels=c("Shiretoko", "Berchtesgaden", "Grand Teton"))) %>% 
#     ggplot(aes(x=as.numeric(freq), y=value*100, col=landscape, linetype=climate)) +
#     geom_line(aes(group=interaction(rep, landscape, climate)), linewidth=0.3, alpha=0.4, show.legend=F) +
#     geom_line(data=mean.freq, linewidth=1.2) +
#     facet_wrap(~name) +
#     ylim(0,100) +
#     labs(y="Landscape unchanged [%]", x="Disturbance frequency modification", col="Landscape", linetype="Climate") +
#     scale_x_continuous(labels=c("Reference", "*2", "*5", "*10")) +
#     scale_color_manual(values=colors.landscape) +
#     theme_bw(),
#   nrow=2, common.legend=T
# )
# dev.off(); rm(mean.size, mean.freq)
# 
# # regeneration
# mean.fecundity <- bind_rows(overtime.ls[["bgd"]], overtime.ls[["grte"]],overtime.ls[["stoko"]]) %>% 
#   filter(year==80) %>% 
#   filter(freq==1, size==1, browsing==1) %>% 
#   pivot_longer(cols=10:12) %>% 
#   group_by(landscape, climate, fecundity, name) %>% 
#   summarise(value=mean(value)) %>% ungroup() %>% 
#   mutate(landscape = case_match(landscape, "bgd"~"Berchtesgaden", "stoko"~"Shiretoko", "grte"~"Grand Teton"),
#          landscape = factor(landscape, levels=c("Shiretoko", "Berchtesgaden", "Grand Teton"))) 
# mean.browsing <- bind_rows(overtime.ls[["bgd"]], overtime.ls[["grte"]],overtime.ls[["stoko"]]) %>% 
#   filter(year==80) %>% 
#   filter(size==1, freq==1, fecundity==100) %>% 
#   pivot_longer(cols=10:12) %>% 
#   group_by(landscape, climate, browsing, name) %>% 
#   summarise(value=mean(value)) %>% ungroup() %>% 
#   mutate(landscape = case_match(landscape, "bgd"~"Berchtesgaden", "stoko"~"Shiretoko", "grte"~"Grand Teton"),
#          landscape = factor(landscape, levels=c("Shiretoko", "Berchtesgaden", "Grand Teton"))) 
# png("results/figures/Q1_singleProcesses_regeneration_bothClimates.png", res=200,
#     height=1200, width=2000)
# ggpubr::ggarrange(
#   bind_rows(overtime.ls[["bgd"]], overtime.ls[["grte"]],overtime.ls[["stoko"]]) %>% 
#     filter(year==80) %>% 
#     filter(freq==1, size==1, browsing==1) %>% 
#     pivot_longer(cols=10:12) %>% 
#     mutate(landscape = case_match(landscape, "bgd"~"Berchtesgaden", "stoko"~"Shiretoko", "grte"~"Grand Teton"),
#            landscape = factor(landscape, levels=c("Shiretoko", "Berchtesgaden", "Grand Teton"))) %>% 
#     ggplot(aes(x=as.numeric(fecundity), y=value*100, col=landscape, linetype=climate)) +
#     geom_line(aes(group=interaction(rep, landscape, climate)), linewidth=0.3, alpha=0.4, show.legend=F) +
#     geom_line(data=mean.fecundity, linewidth=1.2) +
#     facet_wrap(~name) +
#     ylim(0,100) +
#     labs(y="Landscape unchanged [%]", x="Seed availability modification", col="Landscape", linetype="Climate") +
#     scale_x_continuous(labels=c("Reference", "/2", "/5", "/10")) +
#     scale_color_manual(values=colors.landscape) +
#     theme_bw(),
#   bind_rows(overtime.ls[["bgd"]], overtime.ls[["grte"]],overtime.ls[["stoko"]]) %>% 
#     filter(year==80) %>% 
#     filter(size==1, freq==1, fecundity==100) %>% 
#     pivot_longer(cols=10:12) %>% 
#     mutate(landscape = case_match(landscape, "bgd"~"Berchtesgaden", "stoko"~"Shiretoko", "grte"~"Grand Teton"),
#            landscape = factor(landscape, levels=c("Shiretoko", "Berchtesgaden", "Grand Teton"))) %>% 
#     ggplot(aes(x=as.numeric(browsing), y=value*100, col=landscape, linetype=climate)) +
#     geom_line(aes(group=interaction(rep, landscape, climate)), linewidth=0.3, alpha=0.4, show.legend=F) +
#     geom_line(data=mean.browsing, linewidth=1.2) +
#     facet_wrap(~name) +
#     ylim(0,100) +
#     labs(y="Landscape unchanged [%]", x="Sapling height growth modification", col="Landscape", linetype="Climate") +
#     scale_x_continuous(labels=c("Reference", "*2", "*5", "*10")) +
#     scale_color_manual(values=colors.landscape) +
#     theme_bw(),
#   nrow=2, common.legend=T
# )
# dev.off(); rm(mean.fecundity, mean.browsing)
# 

# ### Single processes: baseline climate ####
# # disturbance
# mean.size <- bind_rows(overtime.ls[["bgd"]], overtime.ls[["grte"]],overtime.ls[["stoko"]]) %>% 
#   filter(year==80, climate=="baseline") %>% 
#   filter(freq==1, fecundity==100, browsing==1) %>% 
#   pivot_longer(cols=10:12) %>% 
#   group_by(landscape, climate, size, name) %>% 
#   summarise(value=mean(value)) %>% ungroup() %>% 
#   mutate(landscape = case_match(landscape, "bgd"~"Berchtesgaden", "stoko"~"Shiretoko", "grte"~"Grand Teton"),
#          landscape = factor(landscape, levels=c("Shiretoko", "Berchtesgaden", "Grand Teton"))) 
# mean.freq <- bind_rows(overtime.ls[["bgd"]], overtime.ls[["grte"]],overtime.ls[["stoko"]]) %>% 
#   filter(year==80, climate=="baseline") %>% 
#   filter(size==1, fecundity==100, browsing==1) %>% 
#   pivot_longer(cols=10:12) %>% 
#   group_by(landscape, climate, freq, name) %>% 
#   summarise(value=mean(value)) %>% ungroup() %>% 
#   mutate(landscape = case_match(landscape, "bgd"~"Berchtesgaden", "stoko"~"Shiretoko", "grte"~"Grand Teton"),
#          landscape = factor(landscape, levels=c("Shiretoko", "Berchtesgaden", "Grand Teton"))) 
# png("results/figures/Q1_singleProcesses_disturbance.png", res=180,
#     height=1200, width=2000)
# ggpubr::ggarrange(
#   bind_rows(overtime.ls[["bgd"]], overtime.ls[["grte"]],overtime.ls[["stoko"]]) %>% 
#     filter(climate=="baseline", year==80) %>% 
#     filter(freq==1, fecundity==100, browsing==1) %>% 
#     pivot_longer(cols=10:12) %>% 
#     mutate(landscape = case_match(landscape, "bgd"~"Berchtesgaden", "stoko"~"Shiretoko", "grte"~"Grand Teton"),
#            landscape = factor(landscape, levels=c("Shiretoko", "Berchtesgaden", "Grand Teton"))) %>% 
#     ggplot(aes(x=as.numeric(size), y=100-(value*100), col=landscape)) +
#     geom_line(aes(group=interaction(rep, landscape, climate)), linewidth=0.3, alpha=0.4, show.legend=F) +
#     geom_line(data=mean.size, linewidth=1.2) +
#     facet_wrap(~name) +
#     ylim(0,100) +
#     labs(y="System broken [%]", x="Disturbance size modification", col="Landscape") +
#     scale_x_continuous(labels=c("Reference", "*2", "*5", "*10")) +
#     scale_color_manual(values=colors.landscape) +
#     theme_bw(),
#   bind_rows(overtime.ls[["bgd"]], overtime.ls[["grte"]],overtime.ls[["stoko"]]) %>% 
#     filter(climate=="baseline", year==80) %>% 
#     filter(size==1, fecundity==100, browsing==1) %>% 
#     pivot_longer(cols=10:12) %>% 
#     mutate(landscape = case_match(landscape, "bgd"~"Berchtesgaden", "stoko"~"Shiretoko", "grte"~"Grand Teton"),
#            landscape = factor(landscape, levels=c("Shiretoko", "Berchtesgaden", "Grand Teton"))) %>% 
#     ggplot(aes(x=as.numeric(freq), y=100-(value*100), col=landscape, linetype=climate)) +
#     geom_line(aes(group=interaction(rep, landscape, climate)), linewidth=0.3, alpha=0.4, show.legend=F) +
#     geom_line(data=mean.freq, linewidth=1.2) +
#     facet_wrap(~name) +
#     ylim(0,100) +
#     labs(y="System broken [%]", x="Disturbance frequency modification", col="Landscape") +
#     scale_x_continuous(labels=c("Reference", "*2", "*5", "*10")) +
#     scale_color_manual(values=colors.landscape) +
#     theme_bw(),
#   nrow=2, common.legend=T
# )
# dev.off(); rm(mean.size, mean.freq)
# 
# # regeneration
# mean.fecundity <- bind_rows(overtime.ls[["bgd"]], overtime.ls[["grte"]],overtime.ls[["stoko"]]) %>% 
#   filter(year==80, climate=="baseline") %>% 
#   filter(freq==1, size==1, browsing==1) %>% 
#   pivot_longer(cols=10:12) %>% 
#   group_by(landscape, climate, fecundity, name) %>% 
#   summarise(value=mean(value)) %>% ungroup() %>% 
#   mutate(landscape = case_match(landscape, "bgd"~"Berchtesgaden", "stoko"~"Shiretoko", "grte"~"Grand Teton"),
#          landscape = factor(landscape, levels=c("Shiretoko", "Berchtesgaden", "Grand Teton"))) 
# mean.browsing <- bind_rows(overtime.ls[["bgd"]], overtime.ls[["grte"]],overtime.ls[["stoko"]]) %>% 
#   filter(year==80, climate=="baseline") %>% 
#   filter(size==1, freq==1, fecundity==100) %>% 
#   pivot_longer(cols=10:12) %>% 
#   group_by(landscape, climate, browsing, name) %>% 
#   summarise(value=mean(value)) %>% ungroup() %>% 
#   mutate(landscape = case_match(landscape, "bgd"~"Berchtesgaden", "stoko"~"Shiretoko", "grte"~"Grand Teton"),
#          landscape = factor(landscape, levels=c("Shiretoko", "Berchtesgaden", "Grand Teton"))) 
# png("results/figures/Q1_singleProcesses_regeneration.png", res=180,
#     height=1200, width=2000)
# ggpubr::ggarrange(
#   bind_rows(overtime.ls[["bgd"]], overtime.ls[["grte"]],overtime.ls[["stoko"]]) %>% 
#     filter(climate=="baseline", year==80) %>% 
#     filter(freq==1, size==1, browsing==1) %>% 
#     pivot_longer(cols=10:12) %>% 
#     mutate(landscape = case_match(landscape, "bgd"~"Berchtesgaden", "stoko"~"Shiretoko", "grte"~"Grand Teton"),
#            landscape = factor(landscape, levels=c("Shiretoko", "Berchtesgaden", "Grand Teton"))) %>% 
#     ggplot(aes(x=as.numeric(fecundity), y=100-(value*100), col=landscape)) +
#     geom_line(aes(group=interaction(rep, landscape, climate)), linewidth=0.3, alpha=0.4, show.legend=F) +
#     geom_line(data=mean.fecundity, linewidth=1.2) +
#     facet_wrap(~name) +
#     ylim(0,100) +
#     labs(y="System broken [%]", x="Seed availability modification", col="Landscape") +
#     scale_x_continuous(labels=c("Reference", "/2", "/5", "/10")) +
#     scale_color_manual(values=colors.landscape) +
#     theme_bw(),
#   bind_rows(overtime.ls[["bgd"]], overtime.ls[["grte"]],overtime.ls[["stoko"]]) %>% 
#     filter(climate=="baseline", year==80) %>% 
#     filter(size==1, freq==1, fecundity==100) %>% 
#     pivot_longer(cols=10:12) %>% 
#     mutate(landscape = case_match(landscape, "bgd"~"Berchtesgaden", "stoko"~"Shiretoko", "grte"~"Grand Teton"),
#            landscape = factor(landscape, levels=c("Shiretoko", "Berchtesgaden", "Grand Teton"))) %>% 
#     ggplot(aes(x=as.numeric(browsing), y=100-(value*100), col=landscape, linetype=climate)) +
#     geom_line(aes(group=interaction(rep, landscape, climate)), linewidth=0.3, alpha=0.4, show.legend=F) +
#     geom_line(data=mean.browsing, linewidth=1.2) +
#     facet_wrap(~name) +
#     ylim(0,100) +
#     labs(y="System broken [%]", x="Sapling height growth modification", col="Landscape") +
#     scale_x_continuous(labels=c("Reference", "*2", "*5", "*10")) +
#     scale_color_manual(values=colors.landscape) +
#     theme_bw(),
#   nrow=2, common.legend=T
# )
# dev.off(); rm(mean.fecundity, mean.browsing)

# forest area plot
# ggplot(aes(x=decade, y=forest.ha, group=paste(climate, size, freq, browsing, fecundity,rep))) +
# geom_line(linewidth=0.2, alpha=0.2) +
# geom_hline(aes(yintercept=area), col="red") +
# facet_wrap(~landscape, scales="free_y") +
# theme_bw()
# bind_rows(overtime.ls[["stoko"]], overtime.ls[["bgd"]], overtime.ls[["grte"]]) %>% 
#   full_join(areas) %>% 
#   ggplot(aes(x=year, y=`3. Remaining forest\nStem density dropping below 50 trees/ha`*area, 
#              group=paste(climate, size, freq, browsing, fecundity,rep))) +
#   geom_line(linewidth=0.2, alpha=0.2) +
#   geom_hline(aes(yintercept=area), col="red") +
#   facet_wrap(~landscape, scales="free_y") +
#   labs(y="Forested area") +
#   theme_bw()
