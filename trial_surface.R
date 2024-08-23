library(plotly)

unique(dist.dyn.df$name)
rate.df <- dist.dyn.df %>% dplyr::select(-n_year) %>% full_join(regen.dyn.df %>% dplyr::select(-n_year)) %>% drop_na() %>% 
  # mutate(dist.dyn = round(dist.dyn, 3),
  #        regen.dyn = round(regen.dyn, 1)) %>% 
  # group_by(dist.dyn, regen.dyn, name) %>% 
  # summarise(value = mean(value)) %>% ungroup() %>% 
  filter(name=="1. Structure\nBasal area decreased by >50 % from reference") %>% 
  mutate(value=100-value*100) %>% 
  dplyr::select(dist.dyn, regen.dyn, value) %>% 
  distinct()

# raw data
plot_ly(x = ~rate.df$dist.dyn, y = ~rate.df$regen.dyn, z = ~rate.df$value, intensity = ~rate.df$value, type = 'mesh3d') %>%
  layout(title = 'Raw data', 
         scene = list(xaxis = list(title = "Disturbance rate"), yaxis = list(title = "Regeneration rate"), 
                      zaxis = list(title = "Landscape changed [structure, %]"), coloraxis = list(title = "this does NOT work (yet)")))

# lm model
mod1 <- lm(rate.df$value ~ rate.df$dist.dyn + rate.df$regen.dyn)
summary(mod1)
disturbance_rate <-mod1$model[,1]; regeneration_rate <- mod1$model[,2]; structural_change <-mod1$fitted.values; plot_ly(
  x = ~disturbance_rate, y = ~regeneration_rate, z = ~structural_change, intensity = ~structural_change, type = 'mesh3d') %>%
  layout(title = 'Linear model', 
         scene = list(xaxis = list(title = "Disturbance rate"), yaxis = list(title = "Regeneration rate"), 
                      zaxis = list(title = "Landscape changed [structure, %]"), coloraxis = list(title = "this does NOT work (yet)")))

# loess model
mod2 <- loess(rate.df$value ~ rate.df$dist.dyn + rate.df$regen.dyn)
disturbance_rate <-mod2$x[,1]; regeneration_rate <- mod2$x[,2]; structural_change <-mod2$fitted; plot_ly(
  x = ~disturbance_rate, y = ~regeneration_rate, z = ~structural_change, intensity = ~structural_change, type = 'mesh3d') %>%
  layout(title = 'Loess model', 
         scene = list(xaxis = list(title = "Disturbance rate"), yaxis = list(title = "Regeneration rate"), 
                      zaxis = list(title = "Landscape changed [structure, %]"), coloraxis = list(title = "this does NOT work (yet)")))

mod2.pred <- predict(mod2, newdata=expand_grid(dist.dyn=seq(min(disturbance_rate), max(disturbance_rate), length.out=10),
                                  regen.dyn=seq(min(regeneration_rate), max(regeneration_rate), length.out=10)),
                     se=T)

# data.frame(x=disturbance_rate, y=regeneration_rate, fill=structural_change) %>% 
#   ggplot(aes(x=x, y=y, col=fill)) +
#   geom_point()

# mod1 <- lm(rate.df$value ~ rate.df$dist.dyn + rate.df$regen.dyn)
# library(scatterplot3d)
# G  <- scatterplot3d(rate.df$dist.dyn, rev(rate.df$regen.dyn), rate.df$value, type = "p", angle=225)
# G$plane3d(mod1, draw_polygon = TRUE, draw_lines = FALSE)

