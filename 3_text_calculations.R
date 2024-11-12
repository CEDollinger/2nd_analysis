

# Q1: single processes ####
single.Process.df <- read_csv("results/datasets/singleProcess.df.csv")
singleProcess.df %>% 
  group_by(landscape, name) %>% 
  summarise(mean=round(mean(value),1),
            #median=round(median(value),1)
            max=round(max(value),1))

rm(single.Process.df)

# Q2: interaction ####
dyn.df <- read_csv("results/datasets/dyn.df.csv")

# mean, min/max of percentage landscape changed
dyn.df %>% 
  filter(climate=="baseline") %>% 
  dplyr::select(dist.dyn, regen.dyn, value, landscape, name) %>% 
  mutate(value=100-value*100) %>% 
  group_by(landscape, name) %>% 
  summarise(mean=round(mean(value),1),
            #median=round(median(value),1)
            max=round(max(value),1)) 

# reference disturbance and recruitment rate
dyn.df %>% 
  filter(climate == "baseline", size == 1, freq == 1, fecundity == 100, browsing == 1) %>% 
  group_by(landscape) %>% 
  summarise(dist = mean(dist.dyn),
            regen = mean(regen.dyn))

# ranges of disturbance and recruitment rates
dyn.df %>% 
  filter(climate == "baseline") %>% 
  group_by(landscape) %>% 
  summarise(dist = mean(dist.dyn),
            dist.min = min(dist.dyn),
            dist.max = max(dist.dyn),
            regen = round(mean(regen.dyn)),
            regen.min = round(min(regen.dyn)),
            regen.max = round(max(regen.dyn)))

# tipping point in disturbance rate
dyn.df %>% 
  mutate(value=100-value*100) %>% 
  filter(climate=="baseline", 
         name == names(response.colors)[1],
         dist.dyn >= 2) %>% 
  # arrange(value) %>% 
  summary()

# reduction in regeneration rate in Grand Teton (no disturbance change)
dyn.df %>% 
  filter(landscape == "grte") %>% 
  filter(size == 1, freq == 1) %>% 
  summarise(drop = 1-min(regen.dyn)/max(regen.dyn))

rm(dyn.df)


# Q3: climate effect ####
dyn.effect <- read_csv("results/datasets/dyn.effect.csv"); head(dyn.effect)

# mean climate effect by response variable
dyn.effect %>% 
  group_by(name) %>% 
  summarise(mean_effect_abs = mean(abs(effect)),
            mean_effect = mean(effect))

# mean climate effect by landscape
dyn.effect %>% 
  group_by(landscape) %>% 
  summarise(mean_effect_abs = mean(abs(effect)),
            mean_effect = mean(effect)) %>% 
  arrange(landscape)

# Misc ####
# hectare-years
mean(c(8645, 42586, 35676))*7680*80
# 17798553600
