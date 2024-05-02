# regeneration rate gradient? ####

landscapename <- "bgd"
db.conn <- dbConnect(RSQLite::SQLite(), paste0("raw_data/breakingTheSystem_", toupper(landscapename) ,"_Results/data1/output.sqlite"))
ls.bgd <- tbl(db.conn, "landscape") %>% collect() %>% mutate(landscape = landscapename)
dbDisconnect(db.conn); rm(db.conn)
landscapename <- "stoko"
db.conn <- dbConnect(RSQLite::SQLite(), paste0("raw_data/breakingTheSystem_", toupper(landscapename) ,"_Results/data1/output.sqlite"))
ls.stoko <- tbl(db.conn, "landscape") %>% collect() %>% mutate(landscape = landscapename)
dbDisconnect(db.conn); rm(db.conn)
landscapename <- "grte"
db.conn <- dbConnect(RSQLite::SQLite(), paste0("raw_data/breakingTheSystem_", toupper(landscapename) ,"_Results/data1/output.sqlite"))
ls.grte <- tbl(db.conn, "landscape") %>% collect() %>% mutate(landscape = landscapename)
dbDisconnect(db.conn); rm(db.conn)

bind_rows(ls.bgd, ls.stoko, ls.grte) %>%
  filter(species %ni% c("pice", "weer", "pimu")) %>% # filter shrub species
  group_by(year, landscape) %>% 
  summarise(regen = sum(cohort_count_ha),
            density = sum(count_ha),
            basal = sum(basal_area_m2)) %>% ungroup() %>% 
  group_by(landscape) %>% 
  mutate(count.previous = lag(density),
         zuwachs = density - count.previous) %>% 
  filter(year>0) %>% 
  # ggplot(aes(x=year, y=zuwachs)) +
  # geom_line() +
  # facet_grid(~landscape) +
  # theme_bw()
  summarise(regen_mean = round(mean(regen)),
            density_mean = round(mean(density)),
            basal_mean = round(mean(basal)),
            zuwachs_sd = round(sd(zuwachs)),
            zuwachs_mean = round(mean(zuwachs))) 

bind_rows(ls.bgd, ls.stoko, ls.grte) %>% 
  filter(species %ni% c("pice", "weer", "pimu")) %>% # filter shrub species
  ggplot(aes(x=year, y=cohort_count_ha, fill=species)) +
  geom_bar(stat="identity", width=1.01) +
  facet_wrap(~landscape) +
  theme_bw()

ls.stoko %>% 
  filter(species %ni% c("pice", "weer", "pimu")) %>% # filter shrub species
  group_by(species, landscape) %>% 
  summarise(regen = mean(cohort_count_ha)) %>% ungroup() %>% 
  arrange(desc(regen))


# recruitment rate ####

# idea: get recruitment rate from landscape + landscape_removed output
## 1. zuwachs = change in total number of trees per year
## 2. died = trees killed by natural causes or disturbance
## 3. zuwachs = born - died -> born = zuwachs + died

ls <- data.frame(); rem <- data.frame()
for (i in c(1:3)) {
  landscapename <- landscapes[i]
  dbname <- paste0("raw_data/helper_files/", landscapename,"_recruitmentRate.sqlite")
  db.conn <- dbConnect(RSQLite::SQLite(), dbname)
  ls_i <- tbl(db.conn, "landscape") %>% collect() %>% mutate(landscape = landscapename)
  rem_i <- tbl(db.conn, "landscape_removed") %>% collect() %>% mutate(landscape = landscapename,
                                                                      count_ha = count/areas[i, "area"])
  dbDisconnect(db.conn); rm(db.conn)
  ls <- bind_rows(ls, ls_i); rm(ls_i)
  rem <- bind_rows(rem, rem_i); rm(rem_i)
}

ls %>% 
  group_by(landscape, year) %>% 
  summarise(count.sum = sum(count_ha)) %>% ungroup() %>% 
  group_by(landscape) %>% 
  mutate(count.previous = lag(count.sum),
         zuwachs = count.sum - count.previous) %>% ungroup() %>% 
  filter(year %in% 1:20) %>% 
  full_join(rem %>% 
              filter(year %in% 1:20) %>% 
              group_by(landscape, year) %>% # sum up over species + causes of death
              summarise(died = sum(count_ha)) %>% ungroup(), by = c("year", "landscape")) %>% 
  mutate(born = zuwachs + died) %>% 
  group_by(landscape) %>% 
  summarise(recruited_mean = mean(born)) # mean number of trees that are recruited per year and hectare
  
# pie chart
ggplot(aes(x=1, y=born_mean, fill=landscape)) + geom_bar(stat="identity") + coord_polar("y") + theme_minimal()