# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Q2: is there an interaction with climate? ###########################################################################################################################################
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

## Interaction plot ####
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

# baseline - hotdry as two lines

### disturbance rate ####
dist.dyn.df_climate <- bind_rows(overtime.ls[["bgd"]], overtime.ls[["grte"]],overtime.ls[["stoko"]]) %>% 
  filter(year==80) %>% dplyr::select(-year) %>% 
  full_join(patch.df %>% 
              filter(year %in% 1:10) %>% 
              group_by(climate, size, freq, browsing, fecundity, landscape, rep, area) %>% 
              # mean yearly disturbance rate: area_disturbed to ha -> divide by landscape area -> convert to %
              summarise(dist.dyn = mean(area_disturbed/100/area*100),
                        n_year = length(unique(year))) , 
            by=c("climate", "rep", "size", "freq", "browsing", "fecundity", "landscape")) %>%  
  pivot_longer(9:11) %>% 
  mutate(dist.dyn = ifelse(is.na(dist.dyn), 0, dist.dyn))

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

### regeneration rate ####
regen.dyn.df_climate <- bind_rows(overtime.ls[["bgd"]], overtime.ls[["grte"]],overtime.ls[["stoko"]]) %>% 
  filter(year==80) %>% dplyr::select(-year) %>% 
  full_join(regen.df %>% 
              filter(year %in% 1:80) %>% 
              group_by(climate, size, freq, browsing, fecundity, landscape, rep, area) %>% 
              # mean yearly disturbance rate: area_disturbed to ha -> divide by landscape area -> convert to %
              summarise(regen.dyn = mean(born),
                        n_year = length(unique(year))) , 
            by=c("climate", "rep", "size", "freq", "browsing", "fecundity", "landscape")) %>%  
  pivot_longer(9:11) %>% 
  mutate(regen.dyn = ifelse(is.na(regen.dyn), 0, regen.dyn))
regen.dyn.df_climate$regen.dyn %>% summary() # chose sensible labels

png("results/figures/Q2_responseLine_regenerationRate_climate_80yrs.png", res=200,
    height=1300, width=2000)
regen.dyn.df_climate  %>% 
  ggplot(aes(x = regen.dyn, y = value*100, col=climate)) +
  geom_point(size = 0.1, alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE) +
  scale_x_reverse() +
  facet_grid(~name) +
  scale_color_manual(values=c("hotdry" = "firebrick2", "baseline"="royalblue3")) +
  ylim(0, 100) +
  labs(y = "Landscape unchanged [%]", col = "Climate scenario",
       x = paste0("Simulated regeneration rate [Mean number of tress recruited per ha yr^-1]\nRate based on the first ", unique(regen.dyn.df_climate$n_year)," simulation years")) +
  theme_bw() +
  theme(legend.position = "top")
dev.off()


## Effect plot ####
effect.background <- data.frame(interaction = factor(c("Dampening", "Amplifying"), levels=c("Dampening", "Amplifying")),
                                xmin = c(-0, -0), xmax = c(Inf, Inf),
                                ymin = c(0, -Inf), ymax = c(Inf, 0))
### disturbance rate ####
dist.dyn.effect <- dist.dyn.df_climate %>% 
  filter(climate=="baseline") %>% 
  dplyr::select(-climate, -identifier, -n_year) %>% 
  rename(baseline=value, dist.dyn_baseline=dist.dyn) %>% 
  full_join(dist.dyn.df_climate %>% filter(climate=="hotdry") %>% 
              dplyr::select(-climate, -identifier, -n_year) %>% 
              rename(hotdry=value, dist.dyn_hotdry=dist.dyn)) %>% 
  mutate(effect = hotdry-baseline) # positive when hotdry > baseline -> dampening effect
summary(dist.dyn.effect)

# absolute disturbance rate
png("results/figures/Q2_climateEffect_disturbanceRate_10yrs.png", res=200,
    height=1300, width=2000)
dist.dyn.effect %>% 
  ggplot() +
  geom_rect(data=effect.background, 
            aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=interaction), alpha = 0.3) +
  geom_point(aes(x=dist.dyn_baseline, y=effect*100),
             size=0.05) +
  geom_smooth(aes(x=dist.dyn_baseline, y=effect*100, col=landscape),
              method = "loess", se = F) +
  geom_hline(aes(yintercept=0)) +
  facet_grid(~name) +
  scale_x_log10(breaks = c(0.0001, 0.001, 0.01, 0.1, 1, 10)*2, 
                label = c(0.0001, 0.001, 0.01, 0.1, 1, 10)*2) +
  scale_fill_manual(name = "Interaction type",
                    values = c("Dampening" = "#A1BE95", "Amplifying"="#F98866"),
                    labels = c("Dampening:\nless landscape broken", "Amplifying:\nmore landscape broken")) +
  labs(x = "Simulated disturbance rate [% yr^-1]\nRate based on only the first 10 simulation years\nAxis log10-transformed",
       y = "Climate effect [percentage point change, %] ", title="Effect of climate on landscape unchanged compared to baseline") +
  theme_bw()
dev.off()

# relative disturbance rate
png("results/figures/Q2_climateEffect_disturbanceRate_relative_10yrs.png", res=200,
    height=1300, width=2000)
dist.dyn.effect %>%
  full_join(dist.dyn.effect %>%
              filter(size==1, freq==1, fecundity==100, browsing==1) %>%
              group_by(landscape) %>%
              summarise(dist.dyn_ref = mean(dist.dyn_baseline)) %>%
              dplyr::select(landscape, dist.dyn_ref), multiple = "all", by = "landscape") %>%
  mutate(dist.change = (dist.dyn_baseline - dist.dyn_ref)/dist.dyn_ref,
         landscape = factor(landscape, levels=c("stoko", "bgd", "grte"))) %>%
  ggplot() +
  geom_rect(data=effect.background,
            aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=interaction), alpha = 0.3) +
  geom_point(aes(x=dist.change, y=effect*100),
             size=0.05) +
  geom_smooth(aes(x=dist.change, y=effect*100),
              method = "loess", se = F, col="black") +
  geom_hline(aes(yintercept=0)) +
  facet_grid(landscape~name) +
  scale_fill_manual(name = "Interaction type",
                    values = c("Dampening" = "#A1BE95", "Amplifying"="#F98866"),
                    labels = c("Dampening:\nless landscape broken", "Amplifying:\nmore landscape broken")) +
  labs(x = "Percent change in disturbance rate [%]\nRates based on only the first 10 simulation years",
       y = "Climate effect [percentage point change, %] ", title="Effect of climate on landscape unchanged compared to baseline") +
  theme_bw()
dev.off()


### regeneration rate ####
regen.dyn.effect <- regen.dyn.df_climate %>% 
  filter(climate=="baseline") %>% 
  dplyr::select(-climate, -identifier, -n_year) %>% 
  rename(baseline=value, regen.dyn_baseline=regen.dyn) %>% 
  full_join(regen.dyn.df_climate %>% filter(climate=="hotdry") %>% 
              dplyr::select(-climate, -identifier, -n_year) %>% 
              rename(hotdry=value, regen.dyn_hotdry=regen.dyn)) %>% 
  mutate(effect = hotdry-baseline) # positive when hotdry > baseline -> dampening effect
summary(regen.dyn.effect)

# absolute regeneration rate
png("results/figures/Q2_climateEffect_regenerationRate_80yrs.png", res=200,
    height=1300, width=2000)
regen.dyn.effect %>% 
  ggplot() +
  geom_rect(data=effect.background, 
            aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=interaction), alpha = 0.3) +
  geom_point(aes(x=regen.dyn_baseline, y=effect*100),
             size=0.05) +
  geom_smooth(aes(x=regen.dyn_baseline, y=effect*100, col=landscape),
              method = "loess", se = F) +
  geom_hline(aes(yintercept=0)) +
  facet_grid(~name) +
  scale_x_reverse() +
  scale_fill_manual(name = "Interaction type",
                    values = c("Dampening" = "#A1BE95", "Amplifying"="#F98866"),
                    labels = c("Dampening:\nless landscape broken", "Amplifying:\nmore landscape broken")) +
  labs(x = "Simulated regeneration rate [Mean number of tress recruited per ha yr^-1]\nRate based on all 80 simulation years",
       y = "Climate effect [percentage point change, %] ", title="Effect of climate on landscape unchanged compared to baseline") +
  theme_bw()
dev.off()

# relative regeneration rate
# png("results/figures/Q2_climateEffect_regenerationRate_relative_80yrs.png", res=200,
#     height=1300, width=2000)
# regen.dyn.effect %>% 
#   full_join(regen.dyn.effect %>% 
#               filter(size==1, freq==1, fecundity==100, browsing==1) %>% 
#               group_by(landscape) %>% 
#               summarise(regen.dyn_ref = mean(regen.dyn_baseline)) %>% 
#               dplyr::select(landscape, regen.dyn_ref), multiple = "all", by = "landscape") %>% 
#   mutate(regen.change = (regen.dyn_baseline - regen.dyn_ref)/regen.dyn_ref,
#          landscape = factor(landscape, levels=c("stoko", "bgd", "grte"))) %>% 
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
#                     values = c("Dampening" = "#018571", "Amplifying"="#143d59"),
#                     labels = c("Dampening:\nless landscape broken", "Amplifying:\nmore landscape broken")) +
#   labs(x = "Percent change in disturbance rate [%]\nRates based on only the first 10 simulation years",
#        y = "Climate effect [percentage point change, %] ", title="Effect of climate on landscape unchanged compared to baseline") +
#   theme_bw()
# dev.off()

values = c("Dampening" = "#018571", "Amplifying"="#143d59")



