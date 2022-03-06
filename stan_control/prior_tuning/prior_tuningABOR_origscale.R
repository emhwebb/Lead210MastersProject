#prior tuning

library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

library(readr)
library(tidyverse)

#conversion factors
#dpm/g to bq/g
dpmg_bqg <- 1/60
#g/cm^3 to g/cm^2
gcm3_gcm2 <- 2


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
rho <- ABOR1309_01PU$dry_bulk_density_modeled*gcm3_gcm2
rhoL <- ABOR1309_01PU$dry_bulk_density_modeled*gcm3_gcm2
sigma <- ABOR1309_01PU$total_pb210_activity_se
sigmaL <- ABOR1309_01PU$pb214_activity_se
p <- ABOR1309_01PU$total_pb210_activity
pL <- ABOR1309_01PU$pb214_activity
lambda <- 0.03114 
#omega <- 0.5
#priors
#yearly accumulation
alpha_phi <- 1
beta_phi <- 0.25
#memory parameter #try beta(1,5)
a_omega <- 0.5
b_omega <- 0.5
#plot(0, 0, xlim = c(0, 1), ylim = c(0, 1), type = "n")
#curve(dbeta(x, shape1 = 0.5, shape2 = 0.5), from = 0, to = 1, add = TRUE)
#supported activity
alpha_P <- 1
beta_P <- 0.5
#accumulation rate/gamma noise
alpha_a <- 1
beta_a <- 0.2
#plot(0, 0, xlim = c(0, 50), ylim = c(0, 0.4), type = "n")
#curve(dgamma(x, shape = 1, rate = 0.2), from = 0, to = 50, add = TRUE)

ABOR1309_01PU_dat <- list(N = N, L = L, x = x, xL = xL, delta = delta, M=M,
                          rho = rho, rhoL = rhoL, sigma = sigma, sigmaL = sigmaL, 
                          p = p, pL = pL, lambda = lambda,  alpha_phi = alpha_phi, 
                          beta_phi = beta_phi, a_omega = a_omega, b_omega = b_omega, alpha_P = alpha_P, 
                          beta_P = beta_P, alpha_a = alpha_a, beta_a = beta_a)

#run1: alpha_phi = 10, beta_phi = 0.5, a_omega = 0.5, b_omega = 0.5
#alpha_P = 2, beta_P = 0.5, alpha_a = 5, beta_a = 1, 20000 iterations
#run2: alpha_phi = 2, beta_phi = 0.5, a_omega = 0.5, b_omega = 0.5
#alpha_P = 1, beta_P = 0.5, alpha_a = 5, beta_a = 1, 20000 iterations
#run3: alpha_phi = 2, beta_phi = 0.5, a_omega = 1, b_omega = 1
#alpha_P = 1, beta_P = 0.5, alpha_a = 5, beta_a = 1, 20000 iterations
#run4: alpha_phi = 2, beta_phi = 0.5, a_omega = 1, b_omega = 1
#alpha_P = 1, beta_P = 0.5, alpha_a = 5, beta_a = 1, 20000 iterations, dpm/g
#run5: alpha_phi = 2, beta_phi = 0.5, a_omega = 0.5, b_omega = 0.5
#alpha_P = 1, beta_P = 0.5, alpha_a = 5, beta_a = 1, 20000 iterations, dpm/g
#run6: alpha_phi = 2, beta_phi = 0.5, a_omega = 0.5, b_omega = 0.5
#alpha_P = 1, beta_P = 0.5, alpha_a = 1, beta_a = .2, 20000 iterations, dpm/g
#run7: alpha_phi = 1, beta_phi = 0.125, a_omega = 0.5, b_omega = 0.5
#alpha_P = 1, beta_P = 0.5, alpha_a = 1, beta_a = .2, 20000 iterations, dpm/g
#run8: alpha_phi = 1, beta_phi = 0.25, a_omega = 0.5, b_omega = 0.5
#alpha_P = 1, beta_P = 0.5, alpha_a = 1, beta_a = .2, 100000 iterations, dpm/g

ABOR1309_01_orig8 <- stan(file = 'Pb210_draft.stan', data = ABOR1309_01PU_dat, 
                          chains = 4, iter = 100000, control = list(adapt_delta = 0.99))
saveRDS(ABOR1309_01_orig8, "ABOR1309_01_orig8.rds")
plot(ABOR1309_01_orig8, pars = "ages")
plot(ABOR1309_01_orig8, pars = "m")
plot(ABOR1309_01_orig8, pars = "alpha")
plot(ABOR1309_01_orig8, pars = "omega")
plot(ABOR1309_01_orig8, plotfun = "hist", pars = "phi")
plot(ABOR1309_01_orig8, plotfun = "hist", pars = "Ps")
plot(ABOR1309_01_orig8, plotfun = "hist", pars = "omega")
plot(ABOR1309_01_orig8, plotfun = "hist", pars = "ages")
plot(ABOR1309_01_orig8, plotfun = "hist", pars = "ages", binwidth = 0.5)
plot(ABOR1309_01_orig8, plotfun = "hist", pars = "alpha")
plot(ABOR1309_01_orig8, plotfun = "hist", pars = "m")
plot(ABOR1309_01_orig8, plotfun = "hist", pars = "mN")
plot(ABOR1309_01_orig8, plotfun = "trace", pars = "omega")
plot(ABOR1309_01_orig8, plotfun = "trace", pars = "phi")
plot(ABOR1309_01_orig8, plotfun = "trace", pars = "Ps")
plot(ABOR1309_01_orig8, plotfun = "trace", pars = "ages")
plot(ABOR1309_01_orig8, plotfun = "trace", pars = "mN")
plot(ABOR1309_01_orig8, plotfun = "trace", pars = "alpha")
