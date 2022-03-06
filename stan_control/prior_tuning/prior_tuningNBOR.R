#prior tuning NBOR1606_03PU
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

library(readr)
library(tidyverse)

NBOR1606_03PU <- peck_cores %>% 
  filter(core_id == "NBOR1606_03PU", depth_max <= 30) %>%
  select(depth_max, dry_bulk_density_modeled, total_pb210_activity, total_pb210_activity_se,
         pb214_activity, pb214_activity_se, excess_pb210_activity, excess_pb210_activity_se)

N <- 15
M <- 16
L <- 15
delta <- 2
x <- NBOR1606_03PU$depth_max
xL <- NBOR1606_03PU$depth_max
rho <- NBOR1606_03PU$dry_bulk_density_modeled*gcm3_gcm2
rhoL <- NBOR1606_03PU$dry_bulk_density_modeled*gcm3_gcm2
sigma <- NBOR1606_03PU$total_pb210_activity_se
sigmaL <- NBOR1606_03PU$pb214_activity_se
p <- NBOR1606_03PU$total_pb210_activity
pL <- NBOR1606_03PU$pb214_activity
lambda <- 0.03114 
#omega <- 0.5
#more specific priors
#priors
#yearly accumulation
alpha_phi <- 1
beta_phi <- 0.5
#memory parameter
a_omega <- 0.5
b_omega <- 0.5
#supported activity
alpha_P <- 1
beta_P <- 0.5
#accumulation rate/gamma noise
alpha_a <- 1
beta_a <- 0.2

NBOR1606_03PU_dat <- list(N = N, L = L, x = x, xL = xL, delta = delta, M=M,
                          rho = rho, rhoL = rhoL, sigma = sigma, sigmaL = sigmaL, 
                          p = p, pL = pL, lambda = lambda,  alpha_phi = alpha_phi, 
                          beta_phi = beta_phi, a_omega = a_omega, b_omega = b_omega, alpha_P = alpha_P, 
                          beta_P = beta_P, alpha_a = alpha_a, beta_a = beta_a)
#run1: alpha_phi = 1, beta_phi = 0.125, a_omega = 0.5, b_omega = 0.5
#alpha_P = 1, beta_P = 0.5, alpha_a = 1, beta_a = 0.2, iter = 20000
#run2: alpha_phi = 1, beta_phi = 0.5, a_omega = 0.5, b_omega = 0.5
#alpha_P = 1, beta_P = 0.5, alpha_a = 1, beta_a = 0.2, iter = 100000

NBOR1606_03PU_run2 <- stan(file = 'Pb210_draft.stan', data = NBOR1606_03PU_dat, chains = 4, 
                           iter = 100000, control = list(adapt_delta = 0.99))
saveRDS(NBOR1606_03PU_run2, "NBOR1606_03PU_run2.rds")

plot(NBOR1606_03PU_run2, pars = "ages")
plot(NBOR1606_03PU_run2, pars = "m")
plot(NBOR1606_03PU_run2, pars = "alpha")
plot(NBOR1606_03PU_run2, plotfun = "hist", pars = "phi")
plot(NBOR1606_03PU_run2, plotfun = "hist", pars = "Ps")
plot(NBOR1606_03PU_run2, plotfun = "hist", pars = "omega")
plot(NBOR1606_03PU_run2, plotfun = "hist", pars = "ages")
plot(NBOR1606_03PU_run2, plotfun = "hist", pars = "ages", binwidth = 0.5)
plot(NBOR1606_03PU_run2, plotfun = "hist", pars = "alpha", binwidth = 0.5)
plot(NBOR1606_03PU_run2, plotfun = "hist", pars = "mN")
plot(NBOR1606_03PU_run2, plotfun = "trace", pars = "omega")
plot(NBOR1606_03PU_run2, plotfun = "trace", pars = "phi")
plot(NBOR1606_03PU_run2, plotfun = "trace", pars = "Ps")
plot(NBOR1606_03PU_run2, plotfun = "trace", pars = "ages")
plot(NBOR1606_03PU_run2, plotfun = "trace", pars = "mN")
plot(NBOR1606_03PU_run2, plotfun = "trace", pars = "alpha")
