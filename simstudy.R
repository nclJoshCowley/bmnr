#!/usr/bin/env Rscript

library(bmnr)
options(mc.cores = parallel::detectCores())

sim_obj <- example_simulate_bmnr()

object <-
  bmnr(
    sim_obj,
    prior = list(NULL),
    chains = 2, iter = 1500, warmup = 500,
    refresh = 15, seed = 1
  )

render_bmnr(object, "results/latest")
