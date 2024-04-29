# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Q1: how important are the processes? ###########################################################################################################################################
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


## Pie chart matrix ####
overtime.ls <- readRDS("results/datasets/overtime.ls.RDATA")

# disturbance processes
png("results/figures/Q1_pie_disturbance_x.png", res=200,
    height = 1500, width = 2000)
bind_rows(overtime.ls[["bgd"]], overtime.ls[["grte"]],overtime.ls[["stoko"]]) %>% 
  filter(climate=="baseline", year==80) %>% 
  mutate(across(10:12, ~ abs(.x-1)*100)) %>%  # transform to % landscape changed
  group_by(size, freq) %>% 
  summarise(across(8:10, sum)) %>% ungroup() %>% 
  mutate(across(3:5, ~ .x/24000)) %>% # maximum sum = 24000 (100 % * 5 reps * 16 regen scenarios * 3 landscapes)
  rename('1. Structure\nBasal area decreased by >50 % from reference' = 3,
         '2. Composition\nDominant species changed from reference' = 4,
         '3. Remaining forest\nStem density dropping below 50 trees/ha' = 5) %>% 
  pivot_longer(3:5) %>% 
  mutate(size = paste0("Size * ", size), freq = paste0("Frequency * ", freq),
         size = factor(size, levels=c(paste0("Size * ", c(1, 2, 5, 10)))),
         freq = factor(freq, levels=c(paste0("Frequency * ", c(1, 2, 5, 10))))) %>% 
  ggplot(aes(x=0, y=value, fill=name)) +
  geom_bar(stat="identity") +
  facet_grid(rows=vars(size), cols=vars(freq), as.table = F, switch = "both") +
  coord_polar("x") + # "y": pie chart but all same size, "x": weird drop pattern chart, but different sizes
  labs(fill="Response", x="", y="", title="Breaking the system: disturbance processes") +
  theme_bw() +
  theme(axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
        axis.text.x=element_blank(), axis.text.y=element_blank(),
        legend.position = "top") 
dev.off()


# regeneration processes
png("results/figures/Q1_pie_regeneration_y.png", res=200,
    height = 1500, width = 2000)
bind_rows(overtime.ls[["bgd"]], overtime.ls[["grte"]],overtime.ls[["stoko"]]) %>% 
  filter(climate=="baseline", year==80) %>% 
  mutate(across(10:12, ~ abs(.x-1)*100)) %>%  # transform to % landscape changed
  group_by(fecundity, browsing) %>% 
  summarise(across(8:10, sum)) %>% ungroup() %>% 
  mutate(across(3:5, ~ .x/24000)) %>% # maximum sum = 24000 (100 % * 5 reps * 16 regen scenarios * 3 landscapes)
  rename('1. Structure\nBasal area decreased by >50 % from reference' = 3,
         '2. Composition\nDominant species changed from reference' = 4,
         '3. Remaining forest\nStem density dropping below 50 trees/ha' = 5) %>% 
  pivot_longer(3:5) %>%
  mutate(fecundity = paste0("Fecundity * ", fecundity, "%"), browsing = paste0("Browsing * ", browsing),
         fecundity = factor(fecundity, levels=c(paste0("Fecundity * ", c(100, 50, 20, 10), "%"))),
         browsing = factor(browsing, levels=c(paste0("Browsing * ", c(1, 2, 5, 10))))) %>% 
  ggplot(aes(x=0, y=value, fill=name)) +
  geom_bar(stat="identity") +
  facet_grid(rows=vars(fecundity), cols=vars(browsing), as.table = F, switch = "both") +
  coord_polar("y") + 
  labs(fill="Response", x="", y="", title="Breaking the system: regeneration processes") +
  theme_bw() +
  theme(axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
        axis.text.x=element_blank(), axis.text.y=element_blank(),
        legend.position = "top") 
dev.off()


## line plots ####
overtime.ls <- readRDS("results/datasets/overtime.ls.RDATA")

patch.df <- bind_rows(readRDS("results/datasets/patch_bgd_backup.RDATA"),
                      readRDS("results/datasets/patch_grte_backup.RDATA"),
                      readRDS("results/datasets/patch_stoko_backup.RDATA")) %>% 
  group_by(landscape, climate, rep, size, freq, browsing, fecundity, year) %>% # don't group by agent
  summarise(area_disturbed = sum(n_cells)) %>% 
  mutate(size = factor(size, levels = rev(c("10", "5", "2", "1"))),
         freq = factor(freq, levels = rev(c("10", "5", "2", "1"))),
         fecundity = factor(fecundity, levels = c("100", "50", "20", "10")),
         browsing = factor(browsing, levels = rev(c("10", "5", "2", "1")))) %>% 
  full_join(areas, by="landscape") 

# disturbance rate per year
png("results/figures/Q0_disturbanceRate_10x10.png", res=200,
    height=1300, width=2000)
patch.df %>% 
  filter(climate=="baseline", size==10, freq==10, browsing==1, fecundity==100) %>% 
  group_by(year, landscape, rep) %>% 
  summarise(dist.rate = mean(area_disturbed/area)) %>% 
  ggplot(aes(x=year, y=dist.rate, group=rep)) +
  geom_line(linewidth=0.2) +
  facet_wrap(~landscape, scales="free_y") +
  labs(x="Sim. year", y="Disturbance rate [% yr^-1]", 
       title="Simulated disturbance rate in GRTE\nbaseline size*10 freq*10 browsing*1 fecundity*100 reps 1:5") +
  theme_bw()
dev.off()
# disturbance rate over full sim. period in grte: low (max ~4%)
# explanation: very little disturbances possible in later simulation years (no forest left to be disturbed) 
# -> calculate disturbance rate only based on first 5-10 simulation years?



# overall mean disturbance rate 
dist.dyn.df <- bind_rows(overtime.ls[["bgd"]], overtime.ls[["grte"]],overtime.ls[["stoko"]]) %>% 
  filter(climate=="baseline", year==80) %>% dplyr::select(-year) %>% 
  full_join(patch.df %>% 
              filter(climate=="baseline", 
                     year %in% 1:10) %>% 
              group_by(climate, size, freq, browsing, fecundity, landscape, rep, area) %>% 
              # mean yearly disturbance rate: area_disturbed to ha -> divide by landscape area -> convert to %
              summarise(dist.dyn = mean(area_disturbed/100/area*100),
                        n_year = length(unique(year))) , 
            by=c("climate", "rep", "size", "freq", "browsing", "fecundity", "landscape")) %>% 
  rename('1. Structure\nBasal area decreased by >50 % from reference' = 9,
         '2. Composition\nDominant species changed from reference' = 10,
         '3. Remaining forest\nStem density dropping below 50 trees/ha' = 11) %>%  
  pivot_longer(9:11) %>% 
  mutate(dist.dyn = ifelse(is.na(dist.dyn), 0, dist.dyn))

dist.dyn.df$dist.dyn %>% summary() # chose sensible labels

png("results/figures/Q1_responseLine_disturbanceRate_10yrs.png", res=200,
    height=1300, width=2000)
dist.dyn.df %>% 
  ggplot(aes(x = dist.dyn, y = value*100, col = name)) +
  geom_point(size = 0.1, alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE) +
  scale_x_log10(breaks = c(0.0001, 0.001, 0.01, 0.1, 1, 10)*2, # year 1:80 *0.5, 1:10 *2, 1:5 *5, 1:2 *10
                label = c(0.0001, 0.001, 0.01, 0.1, 1, 10)*2) +
  ylim(0, 100) +
  labs(y = "Landscape unchanged [%]", col = "Response",
       x = paste0("Simulated disturbance rate [% yr^-1]\nRate based on only the first ", unique(dist.dyn.df$n_year)," simulation years\nAxis log10-transformed")) +
  theme_bw() +
  theme(legend.position = "top")
dev.off()

# interaction plots (actually Q2) ####

# needed data frames see "line plots"

# baseline - hotdry as two lines

dist.dyn.df_climate <- bind_rows(overtime.ls[["bgd"]], overtime.ls[["grte"]],overtime.ls[["stoko"]]) %>% 
  filter(year==80) %>% dplyr::select(-year) %>% 
  full_join(patch.df %>% 
              filter(year %in% 1:10) %>% 
              group_by(climate, size, freq, browsing, fecundity, landscape, rep, area) %>% 
              # mean yearly disturbance rate: area_disturbed to ha -> divide by landscape area -> convert to %
              summarise(dist.dyn = mean(area_disturbed/100/area*100),
                        n_year = length(unique(year))) , 
            by=c("climate", "rep", "size", "freq", "browsing", "fecundity", "landscape")) %>% 
  rename('1. Structure\nBasal area decreased by >50 % from reference' = 9,
         '2. Composition\nDominant species changed from reference' = 10,
         '3. Remaining forest\nStem density dropping below 50 trees/ha' = 11) %>%  
  pivot_longer(9:11) %>% 
  mutate(dist.dyn = ifelse(is.na(dist.dyn), 0, dist.dyn))

head(dist.dyn.df_climate)

dist.dyn.df_climate$dist.dyn %>% summary() # chose sensible labels

png("results/figures/Q2_responseLine_disturbanceRate_climate_10yrs.png", res=200,
    height=1300, width=2000)
dist.dyn.df_climate  %>% 
  ggplot(aes(x = dist.dyn, y = value*100, col=climate)) +
  geom_point(size = 0.1, alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE) +
  scale_x_log10(breaks = c(0.0001, 0.001, 0.01, 0.1, 1, 10)*2, # year 1:80 *0.5, 1:10 *2, 1:5 *5, 1:2 *10
                label = c(0.0001, 0.001, 0.01, 0.1, 1, 10)*2) +
  facet_grid(~name) +
  scale_color_manual(values=c("hotdry" = "firebrick2", "baseline"="royalblue3")) +
  ylim(0, 100) +
  labs(y = "Landscape unchanged [%]", col = "Climate scenario",
       x = paste0("Simulated disturbance rate [% yr^-1]\nRate based on only the first ", unique(dist.dyn.df_climate$n_year)," simulation years\nAxis log10-transformed")) +
  theme_bw() +
  theme(legend.position = "top")
dev.off()

# hotdry: 
## structure: dampening effect at low-medium disturbance rates
## composition: strong amplifying effects at low disturbance rates, dampening effect at high disturbance rates
## forest loss: little interaction

# effect plot

dist.dyn.effect <- dist.dyn.df_climate %>% 
  filter(climate=="baseline") %>% 
  dplyr::select(-climate, -identifier, -n_year) %>% 
  rename(baseline=value, dist.dyn_baseline=dist.dyn) %>% 
  full_join(dist.dyn.df_climate %>% filter(climate=="hotdry") %>% 
              dplyr::select(-climate, -identifier, -n_year) %>% 
              rename(hotdry=value, dist.dyn_hotdry=dist.dyn)) %>% 
  mutate(effect = hotdry-baseline) # positive when hotdry > baseline -> dampening effect

summary(dist.dyn.effect)
effect.background <- data.frame(interaction = factor(c("Dampening", "Amplifying"), levels=c("Dampening", "Amplifying")),
                         xmin = c(0, 0), xmax = c(Inf, Inf),
                         ymin = c(0, -Inf), ymax = c(Inf, 0))

png("results/figures/Q2_climateEffect_disturbanceRate_10yrs.png", res=200,
    height=1300, width=2000)
dist.dyn.effect %>% 
  ggplot() +
  geom_rect(data=effect.background, 
            aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=interaction), alpha = 0.5) +
  geom_point(aes(x=dist.dyn_baseline, y=effect*100, col=landscape),
             size=0.05) +
  geom_smooth(aes(x=dist.dyn_baseline, y=effect*100),
              method = "loess", se = F, col="black") +
  geom_hline(aes(yintercept=0)) +
  facet_grid(~name) +
  scale_x_log10(breaks = c(0.0001, 0.001, 0.01, 0.1, 1, 10)*2, 
                label = c(0.0001, 0.001, 0.01, 0.1, 1, 10)*2) +
  scale_fill_manual(name = "Interaction type",
                    values = c("Dampening" = "#266867", "Amplifying"="#f55800"),
                    labels = c("Dampening:\nless landscape changed", "Amplifying:\nmore landscape changed")) +
  labs(x = "Simulated disturbance rate [% yr^-1]\nRate based on only the first 10 simulation years\nAxis log10-transformed",
       y = "Climate effect [%] ", title="Effect of climate on landscape unchanged compared to baseline") +
  theme_bw()
dev.off()
