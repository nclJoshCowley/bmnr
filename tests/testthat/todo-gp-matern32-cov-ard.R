### ----
### Script used to extend GP kernel to different inputs. Use to make unit test.
### Josh Cowley. 2023-11-28
### ----

library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

sm <- rstan::stan_model("inst/stan/interface_gp_matern32_cov_ard.stan")

## Data
x_r <- matrix(stats::runif(10), 5, 2)
x_c <- matrix(stats::runif(6), 3, 2)

gp_scale <- 1
gp_length <- c(0.1, 4)

## Cross

stan_data <-
  list(
    n_inputs = ncol(x_r), n_r = nrow(x_r), n_c = nrow(x_c),
    x_r = x_r, x_c = x_c,
    gp_scale = gp_scale, gp_length = gp_length
  )

out_stanfit <-
  rstan::sampling(
    sm, data = stan_data,
    chains = 1, iter = 1, warmup = 0, algorithm = "Fixed_param", refresh = 0
  )

out <-
  rstan::extract(out_stanfit, pars = "out", permuted = FALSE) |>
  array(dim = out_stanfit@par_dims$out)

out

## Self

x_c_spoof <- matrix(nrow = 0, ncol = ncol(x_r))

stan_data <-
  list(
    n_inputs = ncol(x_r), n_r = nrow(x_r), n_c = 0,
    x_r = x_r, x_c = x_c_spoof,
    gp_scale = gp_scale, gp_length = gp_length
  )

out_stanfit <-
  rstan::sampling(
    sm, data = stan_data,
    chains = 1, iter = 1, warmup = 0, algorithm = "Fixed_param", refresh = 0
  )

out <-
  rstan::extract(out_stanfit, pars = "out", permuted = FALSE) |>
  array(dim = out_stanfit@par_dims$out)

out
