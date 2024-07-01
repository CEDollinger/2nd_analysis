library(plotly)
library(rgl)

unique(dist.dyn.df$name)
rate.df <- dist.dyn.df %>% dplyr::select(-n_year) %>% full_join(regen.dyn.df %>% dplyr::select(-n_year)) %>% drop_na() %>% 
  # mutate(dist.dyn = round(dist.dyn, 3),
  #        regen.dyn = round(regen.dyn, 1)) %>% 
  # group_by(dist.dyn, regen.dyn, name) %>% 
  # summarise(value = mean(value)) %>% ungroup() %>% 
  filter(name=="1. Structure\nBasal area decreased by >50 % from reference") %>% 
  dplyr::select(dist.dyn, regen.dyn, value) %>% 
  distinct()

# raw data
plot_ly(x = ~rate.df$dist.dyn, y = ~rate.df$regen.dyn, z = ~rate.df$value, intensity = ~rate.df$value, type = 'mesh3d') %>%
  layout(title = 'Manually Specified Labels', 
         scene = list(xaxis = list(title = "Disturbance rate"), yaxis = list(title = "Regeneration rate"), 
                      zaxis = list(title = "Landscape unchanged [%]"), coloraxis = list(title = "this does NOT work (yet)")))

# lm model
mod1 <- lm(rate.df$value ~ rate.df$dist.dyn + rate.df$regen.dyn)
summary(mod1)
plot_ly(x = ~mod1$model[,1], y = ~mod1$model[,2], z = ~mod1$fitted.values, intensity = ~mod1$fitted.values, type = 'mesh3d')

# loess model
mod2 <- loess(rate.df$value ~ rate.df$dist.dyn + rate.df$regen.dyn)
plot_ly(x = ~mod2$x[,1], y = ~mod2$x[,2], z = ~mod2$fitted, intensity = ~mod2$fitted, type = 'mesh3d')


