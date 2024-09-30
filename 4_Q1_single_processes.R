# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Q1: how important are the processes? ###########################################################################################################################################
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

overtime.ls <- readRDS("results/datasets/overtime.ls.RDATA")

### baseline climate 
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
  summarise(value=mean(value)) %>% ungroup() %>% 
  mutate(type = ifelse(process %in% c("Disturbance size", "Disturbance frequency"), "Disturbance", "Regeneration"))


# text calculations ####
singleProcess.df %>% 
  group_by(landscape, name) %>% 
  summarise(mean=mean(value),
            median=median(value),
            max=max(value))


# Plot ####

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
  labs(y="Landscape changed [%]", x="Modification", col="Process", shape="Process") +
  scale_x_continuous(labels=c("Ref.", "*2", "*5", "*10"), breaks=c(1,2,5,10)) +
  scale_color_manual(values=c('Disturbance size' = "#b35806",
                              'Disturbance frequency' = "#f46d43",
                              'Seed availability decrease'= "#542788",
                              'Sapling height growth limitations'="#2166ac")) +
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
png("results/figures/suppl_figures/Q1_singleProcesses_forestloss.png", res=200,
    height=1600, width=1000)
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
  labs(y="Landscape changed [%]", x="Modification", col="Process", shape="Process") +
  scale_x_continuous(labels=c("Ref.", "*2", "*5", "*10"), breaks=c(1,2,5,10)) +
  scale_color_manual(values=c('Disturbance size' = "#b35806",
                              'Disturbance frequency' = "#f46d43",
                              'Seed availability decrease'= "#542788",
                              'Sapling height growth limitations'="#2166ac")) +
  theme_bw() +
  theme(legend.position = "top")
dev.off()
rm(singleProcess.df, singleProcess.mean)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# DISCARDED ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


## PIES ####
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

# felix contour plot
# png(paste0("results/figures/Q1_contourPlot_loess_", i, "_", clim, ".png"), res=200,
#     height=1300, width=2000)
# p <- ggplot2::ggplot(data = mtrx.melt, aes(x = dist.dyn, y = regen.dyn, fill = value)) +
#   ggplot2::geom_tile(alpha = 0.75) +
#   ggplot2::scale_fill_viridis_c(option = 'turbo', name = "Predicted response") +
#   ggplot2::geom_point(data = a, aes(x = dist.dyn, y = regen.dyn), color = 'white', size = 0.2, inherit.aes = FALSE) +
#   ggplot2::geom_contour(ggplot2::aes(z = value), color = 'black', linewidth = 0.75, linetype = 'dashed') +
#   ggplot2::labs(x = "Disturbance rate",
#                 y = "Regeneration rate",
#                 title = paste0(clim, " climate: ", names(response.colors)[i])) +
#   ggplot2::theme_minimal() +
#   ggplot2::theme(axis.title = ggplot2::element_text(size = 16),
#                  axis.text = ggplot2::element_text(size = 14),
#                  legend.title = ggplot2::element_text(size = 14, angle = 0),
#                  legend.text = ggplot2::element_text(size = 12),
#                  legend.position = 'bottom',
#                  legend.box = 'horizontal',
#                  legend.box.margin = ggplot2::margin(0, 0, 20, 0)); print(p)
# dev.off()

