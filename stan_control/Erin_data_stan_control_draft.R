library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

library(readr)
library(tidyverse)
peck_cores <- read_csv("peck_et_al_2020_depthseries.csv")
ABOR1309_01PU <- peck_cores %>% 
  filter(core_id == "ABOR1309_01PU", depth_max <= 38) %>%
  select(depth_max, dry_bulk_density_modeled, total_pb210_activity, total_pb210_activity_se,
         pb214_activity, pb214_activity_se, excess_pb210_activity, excess_pb210_activity_se)	
N <- 19
M <- 20
L <- 19
delta <- 2
x <- ABOR1309_01PU$depth_max
xL <- ABOR1309_01PU$depth_max
rho <- ABOR1309_01PU$dry_bulk_density_modeled
rhoL <- ABOR1309_01PU$dry_bulk_density_modeled
sigma <- ABOR1309_01PU$total_pb210_activity_se
sigmaL <- ABOR1309_01PU$pb214_activity_se
p <- ABOR1309_01PU$total_pb210_activity
pL <- ABOR1309_01PU$pb214_activity
lambda <- 0.03114 
#omega <- 0.5
#priors
alpha_phi <- 2
beta_phi <- 0.015
a_omega <- 3
b_omega <- 7
alpha_P <- 2 
beta_P <- 0.25
alpha_a <- 0.001
beta_a <- 0.001

ABOR1309_01PU_dat <- list(N = N, L = L, x = x, xL = xL, delta = delta, M=M,
     rho = rho, rhoL = rhoL, sigma = sigma, sigmaL = sigmaL, 
     p = p, pL = pL, lambda = lambda,  alpha_phi = alpha_phi, 
     beta_phi = beta_phi, a_omega = a_omega, b_omega = b_omega, alpha_P = alpha_P, 
     beta_P = beta_P, alpha_a = alpha_a, beta_a = beta_a)

#Run model

first_run <- stan(file = 'Pb210_draft.stan', data = ABOR1309_01PU_dat, chains = 4, iter = 100000)
saveRDS(first_run, "first_run.rds")
first_run_fit <- rstan::extract(first_run)
plot(first_run, pars = "ages")
plot(first_run, pars = "m")
plot(first_run, pars = "alpha")
#plot(first_run,  plotfun = "hist", pars = "accu_rates")
plot(first_run, pars = "mN", plotfun = "hist")
plot(first_run, plotfun = "hist", pars = "phi")
plot(first_run, plotfun = "hist", pars = "Ps")
plot(first_run, plotfun = "hist", pars = "omega")
plot(first_run, plotfun = "hist", pars = "ages")
plot(first_run, plotfun = "hist", pars = "ages", binwidth = 0.5)
plot(first_run, plotfun = "hist", pars = "alpha", binwidth = 0.5)
plot(first_run, plotfun = "hist", pars = "mN")
plot(first_run, plotfun = "trace", pars = "omega")
plot(first_run, plotfun = "trace", pars = "phi")
plot(first_run, plotfun = "trace", pars = "Ps")
plot(first_run, plotfun = "trace", pars = "ages")
plot(first_run, plotfun = "trace", pars = "mN")
plot(first_run, plotfun = "trace", pars = "alpha")
save(first_run_fit, file="first_run_fit.RData")
#check units on density
check_run <- stan(file = 'Pb210_draft.stan', data = ABOR1309_01PU_dat, chains = 4, iter = 20000)
check_run_fit <- rstan::extract(check_run)
plot(check_run, pars = "ages")
plot(check_run, pars = "m")
plot(check_run, pars = "alpha")
plot(check_run,  plotfun = "hist", pars = "accu_rates")
plot(check_run, pars = "mN", plotfun = "hist")
plot(check_run, plotfun = "hist", pars = "phi")
plot(check_run, plotfun = "hist", pars = "Ps")
plot(check_run, plotfun = "hist", pars = "omega")
plot(check_run, plotfun = "hist", pars = "ages")
plot(check_run, plotfun = "trace", pars = "phi")
plot(check_run, plotfun = "trace", pars = "Ps")
plot(check_run, plotfun = "trace", pars = "ages")
#plot(check_run, plotfun = "trace", pars = "mN")
save(check_run_fit, file="check_run_fit.RData")
#big second run
second_run <- stan(file = 'Pb210_draft.stan', data = ABOR1309_01PU_dat, chains = 4, iter = 2000000)
second_run_fit <- rstan::extract(second_run)
plot(second_run, pars = "ages")
plot(second_run, pars = "m")
plot(second_run, pars = "alpha")
plot(second_run,  plotfun = "hist", pars = "accu_rates")
plot(second_run, pars = "mN", plotfun = "hist")
plot(second_run, plotfun = "hist", pars = "phi")
plot(second_run, plotfun = "hist", pars = "Ps")
plot(second_run, plotfun = "hist", pars = "omega")
plot(second_run, plotfun = "hist", pars = "ages")
plot(second_run, plotfun = "trace", pars = "phi")
plot(second_run, plotfun = "trace", pars = "Ps")
plot(second_run, plotfun = "trace", pars = "ages")
save(second_run_fit, file="second_run_fit.RData")
