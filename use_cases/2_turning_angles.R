library(amt)

# Multiple animals --------------------------------------------------------

# some dummy data
dat <- data_frame(
  x = rnorm(500, mean = rep(1:5, each = 100), sd = rep(1:5, each = 100)),
  y = rnorm(500, mean = rep(1:5, each = 100), sd = rep(1:5, each = 100)),
  id = rep(1:5, each = 100)
)

plot(dat$x, dat$y, col = dat$id, asp = 1)


trk <- track(x = dat$x, y = dat$y, id = dat$id)

sumfoo(trk, by = ~id)

sumfoo <- function(x, by = NULL) {
  x %>% group_by_(.dots = by) %>%
    summarize(n = n())
}


# addint truning angles per id, i like to use tidyr::nest
trk %>% nest(-id)

# now we can add a column to data using purrr::map
trk %>% nest(-id) %>% mutate(dir_abs = map(data, direction_abs),
                             dir_rel = map(data, direction_rel),
                             sl = map(data, step_lengths))

# and we can unnest it again
trk %>% nest(-id) %>% mutate(dir_abs = map(data, direction_abs),
                             dir_rel = map(data, direction_rel),
                             sl = map(data, step_lengths)) %>% unnest()

# and save into a object or pipe it directly to ggplot2
# and we can unnest it again
trk %>% nest(-id) %>% mutate(dir_abs = map(data, direction_abs),
                             dir_rel = map(data, direction_rel),
                             sl = map(data, step_lengths)) %>% unnest() %>%
  dplyr::select(id, dir_abs, sl) %>%
  gather(metric, val, -id) %>%
  ggplot(., aes(x = val, group = id, fill = factor(id))) + geom_density(alpha = 0.5) + facet_wrap(~ metric, scale = "free")


