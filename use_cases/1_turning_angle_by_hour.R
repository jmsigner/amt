library(amt)
library(lubridate)

data("animal_sim")
data("habitat")

trk <- track(x = animal_sim$x_, y = animal_sim$y_, t = animal_sim$t_)
trk <- filter(trk, year(t_) == 2017)
stp <- steps(trk) %>% extract_covariates(habitat) %>%
  mutate(
    month = month(t1_),
    hour = hour(t1_),
    hab_1 = layer > 0
  )

# sl by hour
ggplot(stp, aes(x = sl_)) + geom_density() +
  facet_wrap(~ hour)

# hab > 0 or not
ggplot(stp, aes(x = sl_)) + geom_density() +
  facet_wrap(~ hab_1)

# hab > 0 or not, and month
ggplot(stp, aes(x = sl_)) + geom_density() +
  facet_grid(month ~ hab_1)

