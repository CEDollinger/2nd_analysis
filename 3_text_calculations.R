# Q1: single processes ####
singleProcess.df <- read_csv("results/datasets/singleProcess.df.csv")
singleProcess.df %>%
  group_by(landscape, name) %>% # average over all 5 replicates, 4 processes, and 4 modification levels
  summarise(
    mean = round(mean(value), 1),
    max = round(max(value), 1)
  )

rm(singleProcess.df)

# Q2: interaction ####
dyn.df <- read_csv("results/datasets/dyn.df.csv")

# variability in disturbance versus regeneration rates under reference conditions
dyn.df %>%
  filter(
    name == "1. Structure\nBasal area decreased by >50 % from reference",
    climate == "baseline",
    size == 1,
    freq == 1,
    browsing == 1,
    fecundity == 100
  ) %>%
  group_by(landscape) %>% # calculate coefficient of variation over all 5 replicates
  summarise(
    dist.cv = sd(dist.dyn) / mean(dist.dyn),
    regen.cv = sd(regen.dyn) / mean(regen.dyn)
  )

# mean, min/max of percentage landscape changed
dyn.df %>%
  filter(climate == "baseline") %>%
  dplyr::select(dist.dyn, regen.dyn, value, landscape, name) %>%
  mutate(value = 100 - value * 100) %>% # convert from proportion of unchanged landscape to % of changed landscape
  group_by(landscape, name) %>% # average over all runs under reference climate
  summarise(
    mean = round(mean(value), 1),
    max = round(max(value), 1)
  )

# reference disturbance and recruitment rate
dyn.df %>%
  filter(
    climate == "baseline",
    size == 1,
    freq == 1,
    fecundity == 100,
    browsing == 1
  ) %>%
  group_by(landscape) %>% # average over all 5 replicates
  summarise(
    dist = mean(dist.dyn),
    regen = mean(regen.dyn)
  )

# ranges of disturbance and recruitment rates
dyn.df %>%
  filter(climate == "baseline") %>%
  group_by(landscape) %>% # calculations over all runs under reference climate
  summarise(
    dist = mean(dist.dyn),
    dist.min = min(dist.dyn),
    dist.max = max(dist.dyn),
    regen = round(mean(regen.dyn)),
    regen.min = round(min(regen.dyn)),
    regen.max = round(max(regen.dyn))
  )

rm(dyn.df)


# Q3: climate effect ####
dyn.effect <- read_csv("results/datasets/dyn.effect.csv")
head(dyn.effect)

# mean climate effect by response variable
dyn.effect %>%
  group_by(name) %>% # average over all runs under climate change
  summarise(
    mean_effect_abs = mean(abs(effect)),
    mean_effect = mean(effect)
  )

# mean climate effect by landscape
dyn.effect %>%
  group_by(landscape) %>% # average over all runs under climate change
  summarise(
    mean_effect_abs = mean(abs(effect)),
    mean_effect = mean(effect)
  ) %>%
  arrange(landscape)

# Misc ####
# hectare-years
mean(c(8645, 42586, 35676)) * 7680 * 80
# 17798553600
