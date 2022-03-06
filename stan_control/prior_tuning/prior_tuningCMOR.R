#prior tuning cmor1706_01pu
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

library(readr)
library(tidyverse)

CMOR1706_01PU <- peck_cores %>% 
  filter(core_id == "CMOR1706_01PU", depth_max <= 28) %>%
  select(depth_max, dry_bulk_density_modeled, total_pb210_activity, total_pb210_activity_se,
         pb214_activity, pb214_activity_se, excess_pb210_activity, excess_pb210_activity_se)

N <- 14
M <- 15
L <- 14
delta <- 2
x <- CMOR1706_01PU$depth_max
xL <- CMOR1706_01PU$depth_max
rho <- CMOR1706_01PU$dry_bulk_density_modeled*gcm3_gcm2
rhoL <- CMOR1706_01PU$dry_bulk_density_modeled*gcm3_gcm2
sigma <- CMOR1706_01PU$total_pb210_activity_se
sigmaL <- CMOR1706_01PU$pb214_activity_se
p <- CMOR1706_01PU$total_pb210_activity
pL <- CMOR1706_01PU$pb214_activity
lambda <- 0.03114 
#omega <- 0.5
#more specific priors
#priors
#yearly accumulation
alpha_phi <- 1
beta_phi <- 0.125
#memory parameter
a_omega <- 0.5
b_omega <- 0.5
#supported activity
alpha_P <- 1
beta_P <- 0.5
#accumulation rate/gamma noise
alpha_a <- 1
beta_a <- 0.2

CMOR1706_01PU_dat <- list(N = N, L = L, x = x, xL = xL, delta = delta, M=M,
                          rho = rho, rhoL = rhoL, sigma = sigma, sigmaL = sigmaL, 
                          p = p, pL = pL, lambda = lambda,  alpha_phi = alpha_phi, 
                          beta_phi = beta_phi, a_omega = a_omega, b_omega = b_omega, alpha_P = alpha_P, 
                          beta_P = beta_P, alpha_a = alpha_a, beta_a = beta_a)

#run1: alpha_phi = 110, beta_phi = 0.5, a_omega = 0.5, b_omega = 0.5
#alpha_P = 12, beta_P = 0.5, alpha_a = 250, beta_a = 0.5, 20000 iterations
CMOR1706_01PU_run1 <- stan(file = 'Pb210_draft.stan', data = CMOR1706_01PU_dat, chains = 4, iter = 20000, control = list(adapt_delta = 0.99))
saveRDS(CMOR1706_01PU_run1, "CMOR1706_01PU_run1a.rds")
plot(CMOR1706_01PU_run1a, pars = "ages")
plot(CMOR1706_01PU_run1, pars = "m")
plot(CMOR1706_01PU_run1, pars = "alpha")
plot(CMOR1706_01PU_run1, pars = "omega")
plot(CMOR1706_01PU_run1, plotfun = "hist", pars = "phi")
plot(CMOR1706_01PU_run1, plotfun = "hist", pars = "Ps")
plot(CMOR1706_01PU_run1, plotfun = "hist", pars = "omega")
plot(CMOR1706_01PU_run1, plotfun = "hist", pars = "ages")
plot(CMOR1706_01PU_run1, plotfun = "hist", pars = "ages", binwidth = 0.5)
plot(CMOR1706_01PU_run1, plotfun = "hist", pars = "alpha")
plot(CMOR1706_01PU_run1, plotfun = "hist", pars = "m")
plot(CMOR1706_01PU_run1, plotfun = "hist", pars = "mN")
plot(CMOR1706_01PU_run1, plotfun = "trace", pars = "omega")
plot(CMOR1706_01PU_run1, plotfun = "trace", pars = "phi")
plot(CMOR1706_01PU_run1, plotfun = "trace", pars = "Ps")
plot(CMOR1706_01PU_run1, plotfun = "trace", pars = "ages")
plot(CMOR1706_01PU_run1, plotfun = "trace", pars = "mN")
plot(CMOR1706_01PU_run1, plotfun = "trace", pars = "alpha")
