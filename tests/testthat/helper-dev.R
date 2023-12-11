# object <<-
#   xfun::cache_rds(
#     dir = file.path(rprojroot::find_root("DESCRIPTION"), "_cache/"),
#     file = "/bmnr-example",
#     {
#       bmnr::bmnr(
#         bmnr::example_simulate_bmnr(
#           coords = bmnr:::sim_unif_spatiotemporal_coords(n_locations = 10, n_years = 1)
#         ),
#         prior = bmnr::bmnr_prior(n_y = 2),
#         warmup = 5e3,
#         iter = 1e4,
#         chains = 1,
#         refresh = 100
#       )
#     })

# object <<-
#   xfun::cache_rds(
#     dir = file.path(rprojroot::find_root("DESCRIPTION"), "_cache/"),
#     file = "/bmnr-example",
#     {
#       bmnr::bmnr(
#         bmnr::example_simulate_bmnr(
#           coords = bmnr:::sim_unif_spatiotemporal_coords(n_locations = 10, n_years = 1)
#         ),
#         prior = bmnr::bmnr_prior(n_y = 2),
#         mvnorm = TRUE,
#         chains = 1
#       )
#     })

# new_data <<- globalenv()$object@data[1:2, ]
