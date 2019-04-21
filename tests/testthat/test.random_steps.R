library(amt)

context("random steps")

# Set up some dummy data

tad <- make_unif_distr()
sld <- make_exp_distr()

random_steps(1, tad, sld)
