# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Q1: how important are the processes? ###########################################################################################################################################
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


## Matrix: pies or pixels ####
overtime.ls <- readRDS("results/datasets/overtime.ls.RDATA")

pie.dist.df <- bind_rows(overtime.ls[["bgd"]], overtime.ls[["grte"]],overtime.ls[["stoko"]]) %>% 
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
         freq = factor(freq, levels=c(paste0("Frequency * ", c(1, 2, 5, 10)))))

pie.regen.df <- bind_rows(overtime.ls[["bgd"]], overtime.ls[["grte"]],overtime.ls[["stoko"]]) %>% 
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
         browsing = factor(browsing, levels=c(paste0("Browsing * ", c(1, 2, 5, 10)))))

### Pies #####
# disturbance processes
pie.dist.fc <- function(var) {
  pie.dist.df %>% 
    filter(name == var) %>%
    ggplot(aes(x=0, y=value, fill=name)) +
    geom_bar(stat="identity") +
    facet_grid(rows=vars(size), cols=vars(freq), as.table = F, switch = "both") +
    coord_polar("x") + # "y": pie chart but all same size, "x": weird drop pattern chart, but different sizes
    labs(fill="", x="", y="") +
    ylim(0,1) + # max value: 0.55
    scale_fill_manual(values=response.colors) +
    theme_bw() +
    theme(axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
          axis.text.x=element_blank(), axis.text.y=element_blank(),
          legend.position = "top") 
}; pie.dist.ls <- lapply(names(response.colors), pie.dist.fc)

png("results/figures/Q1_pie_disturbance.png", res=200,
    height = 1000, width = 2500)
print(
  ggpubr::ggarrange(pie.dist.ls[[1]], pie.dist.ls[[2]], pie.dist.ls[[3]], nrow=1) %>% 
    annotate_figure()
); rm(pie.dist.fc, pie.dist.ls)
dev.off()


# regeneration processes

pie.regen.fc <- function(var) {
  pie.regen.df %>% 
    filter(name == var) %>%
    ggplot(aes(x=0, y=value, fill=name)) +
    geom_bar(stat="identity") +
    facet_grid(rows=vars(fecundity), cols=vars(browsing), as.table = F, switch = "both") +
    coord_polar("x") + 
    labs(fill="", x="", y="") +
    ylim(0,1) + # max value: 0.55
    scale_fill_manual(values=response.colors) +
    theme_bw() +
    theme(axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
          axis.text.x=element_blank(), axis.text.y=element_blank(),
          legend.position = "top") 
}; pie.regen.ls <- lapply(names(response.colors), pie.regen.fc)

png("results/figures/Q1_pie_regeneration.png", res=200,
    height = 1000, width = 2500)
print(
  ggpubr::ggarrange(pie.regen.ls[[1]], pie.regen.ls[[2]], pie.regen.ls[[3]], nrow=1) %>% 
    annotate_figure()
)
rm(pie.regen.fc, pie.regen.ls)
dev.off()

### Pixels ####
pixel.dist.fc <- function(index) {
  pie.dist.df %>% 
    filter(name == names(response.colors)[index]) %>% 
    ggplot(aes(x=1, y=1, fill=value)) +
    geom_tile() +
    facet_grid(rows=vars(size), cols=vars(freq), as.table = F, switch = "both") +
    scale_fill_gradient(low="white", high=response.colors[index], limits=c(0,max(pie.dist.df$value))) +
    theme_minimal() +
    coord_equal() +
    scale_x_discrete(position = "top") +
    labs(fill="", x=as.character(names(response.colors)[index]), y="") +
    theme(axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
          axis.text.x=element_blank(), axis.text.y=element_blank(),
          legend.position = "bottom") 
}; pixel.dist.ls <- lapply(1:3, pixel.dist.fc)
pixel.dist <- ggpubr::ggarrange(pixel.dist.ls[[1]], pixel.dist.ls[[2]], pixel.dist.ls[[3]], nrow=1) %>% 
  annotate_figure(top="Disturbance processes")
pixel.regen.fc <- function(index) {
  pie.regen.df %>% 
    filter(name == names(response.colors)[index]) %>% 
    ggplot(aes(x=1, y=1, fill=value)) +
    geom_tile() +
    facet_grid(rows=vars(fecundity), cols=vars(browsing), as.table = F, switch = "both") +
    scale_fill_gradient(low="white", high=response.colors[index], limits=c(0,max(pie.dist.df$value))) +
    theme_minimal() +
    coord_equal() +
    scale_x_discrete(position = "top") +
    labs(fill="", x=as.character(names(response.colors)[index]), y="") +
    theme(axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
          axis.text.x=element_blank(), axis.text.y=element_blank(),
          legend.position = "bottom") 
}; pixel.regen.ls <- lapply(1:3, pixel.regen.fc)
pixel.regen <- ggpubr::ggarrange(pixel.regen.ls[[1]], pixel.regen.ls[[2]], pixel.regen.ls[[3]], nrow=1) %>% 
  annotate_figure(top="Regeneration processes")

png("results/figures/Q1_pixel.png", res=200,
    height = 2000, width = 2800)
print(
  ggpubr::ggarrange(pixel.dist, pixel.regen, nrow = 2)
)
rm(pixel.dist.fc, pixel.dist.ls, pixel.regen.fc, pixel.regen.ls, pixel.dist, pixel.regen)
dev.off()

## Response line plots ####
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

regen.df <- bind_rows(readRDS("results/datasets/regen_bgd_backup.RDATA"),
                       readRDS("results/datasets/regen_grte_backup.RDATA"),
                       readRDS("results/datasets/regen_stoko_backup.RDATA")) %>% 
  mutate(size = factor(size, levels = rev(c("10", "5", "2", "1"))),
         freq = factor(freq, levels = rev(c("10", "5", "2", "1"))),
         fecundity = factor(fecundity, levels = c("100", "50", "20", "10")),
         browsing = factor(browsing, levels = rev(c("10", "5", "2", "1")))) %>% 
  full_join(areas, by="landscape") 

# disturbance and regeneration rate per year
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
       title="Simulated disturbance rates\nbaseline size*10 freq*10 browsing*1 fecundity*100 reps 1:5") +
  theme_bw()
dev.off()
# disturbance rate over full sim. period in grte: low (max ~4%)
# explanation: very little disturbances possible in later simulation years (no forest left to be disturbed) 
# -> calculate disturbance rate only based on first 5-10 simulation years?
png("results/figures/Q0_regenerationRate_10x10.png", res=200,
    height=1300, width=2000)
regen.df %>% 
  filter(climate=="baseline", size==1, freq==1, browsing==10, fecundity==10) %>% 
  group_by(year, landscape, rep) %>% 
  summarise(regen.rate = mean(recruited_mean)) %>% 
  ggplot(aes(x=year, y=regen.rate, group=rep)) +
  geom_line(linewidth=0.2) +
  facet_wrap(~landscape, scales="free_y") +
  labs(x="Sim. year", y="Disturbance rate [% yr^-1]", 
       title="Simulated disturbance rate\nbaseline size*1 freq*1 browsing*10 fecundity*10 reps 1:5") +
  theme_bw()
dev.off()

### disturbance rate ####
# overall mean disturbance rate on x-axis
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
  scale_color_manual(values=response.colors) +
  labs(y = "Landscape unchanged [%]", col = "Response",
       x = paste0("Simulated disturbance rate [% yr^-1]\nRate based on only the first ", unique(dist.dyn.df$n_year)," simulation years\nAxis log10-transformed")) +
  theme_bw() +
  theme(legend.position = "top")
dev.off()

# relative disturbance rate
png("results/figures/Q1_responseLine_disturbanceRate_relative_10yrs.png", res=200,
    height=1300, width=2000)
dist.dyn.df %>% 
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
regen.dyn.df <- bind_rows(overtime.ls[["bgd"]], overtime.ls[["grte"]],overtime.ls[["stoko"]]) %>% 
  filter(climate=="baseline", year==80) %>% dplyr::select(-year) %>% 
  full_join(regen.df %>% 
              filter(climate=="baseline", 
                     year %in% 1:80) %>% 
              group_by(climate, size, freq, browsing, fecundity, landscape, rep, area) %>% 
              summarise(regen.dyn = mean(born),
                        n_year = length(unique(year))), 
            by=c("climate", "rep", "size", "freq", "browsing", "fecundity", "landscape")) %>% 
  pivot_longer(9:11) %>% 
  mutate(regen.dyn = ifelse(is.na(regen.dyn), 0, regen.dyn))
regen.dyn.df$regen.dyn %>% summary() 

png("results/figures/Q1_responseLine_regenerationRate_80yrs.png", res=200,
    height=1300, width=2000)
regen.dyn.df %>% 
  ggplot(aes(x = regen.dyn, y = value*100, col = name)) +
  geom_point(size = 0.1, alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE) +
  ylim(0, 100) +
  scale_x_reverse() +
  scale_color_manual(values=response.colors) +
  labs(y = "Landscape unchanged [%]", col = "Response",
       x = paste0("Simulated regeneration rate [Mean number of tress recruited per ha yr^-1]\nRate based on the first ", unique(regen.dyn.df$n_year)," simulation years")) +
  theme_bw() +
  theme(legend.position = "top")
dev.off()


# relative regeneration rate
png("results/figures/Q1_responseLine_regenerationRate_relative_80yrs.png", res=200,
    height=1300, width=2000)
regen.dyn.df %>% 
  full_join(regen.dyn.df %>% 
              filter(size==1, freq==1, fecundity==100, browsing==1) %>% 
              group_by(landscape) %>% 
              summarise(regen.dyn_ref = mean(regen.dyn)) %>% 
              dplyr::select(landscape, regen.dyn_ref), multiple = "all", by = "landscape") %>% 
  mutate(regen.change = (regen.dyn - regen.dyn_ref)/regen.dyn_ref,
         landscape = factor(landscape, levels=c("stoko", "bgd", "grte"))) %>% 
  ggplot(aes(x = regen.change*100, y = value*100, col = name)) +
  geom_point(size = 0.1, alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE) +
  facet_grid(~landscape) +
  scale_x_reverse() +
  ylim(0, 100) +
  scale_color_manual(values=response.colors) +
  labs(y = "Landscape unchanged [%]", col = "Response",
       x = paste0("Percent change in regeneration rate [%]\nRates based on the first ", unique(regen.dyn.df$n_year)," simulation years")) +
  theme_bw() +
  theme(legend.position = "top")
dev.off()




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


