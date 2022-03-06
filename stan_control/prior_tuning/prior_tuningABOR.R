#prior tuning

library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

library(readr)
library(tidyverse)

#conversion factors
#dpm/g to bq/kg 
dpmg_bqkg <- 50/3
#g/cm^23 to kg/m^3
gcm3_kgm3 <- 1000
#kg/m^3 to kg/m^2
kgm3_kgm2 <- gcm3_kgm3*0.02
#cm to m
cm_m <- 0.01

peck_cores <- read_csv("peck_et_al_2020_depthseries.csv")
ABOR1309_01PU <- peck_cores %>% 
  filter(core_id == "ABOR1309_01PU", depth_max <= 38) %>%
  select(depth_max, dry_bulk_density_modeled, total_pb210_activity, total_pb210_activity_se,
         pb214_activity, pb214_activity_se, excess_pb210_activity, excess_pb210_activity_se)	
N <- 19
M <- 20
L <- 19
delta <- 2*cm_m
x <- ABOR1309_01PU$depth_max*cm_m
xL <- ABOR1309_01PU$depth_max*cm_m
rho <- ABOR1309_01PU$dry_bulk_density_modeled*kgm3_kgm2
rhoL <- ABOR1309_01PU$dry_bulk_density_modeled*kgm3_kgm2
sigma <- ABOR1309_01PU$total_pb210_activity_se*dpmg_bqkg
sigmaL <- ABOR1309_01PU$pb214_activity_se*dpmg_bqkg
p <- ABOR1309_01PU$total_pb210_activity*dpmg_bqkg
pL <- ABOR1309_01PU$pb214_activity*dpmg_bqkg
lambda <- 0.03114 
#omega <- 0.5
#priors
#yearly accumulation
alpha_phi <- 110
beta_phi <- 0.5
#memory parameter
a_omega <- 0.5
b_omega <- 0.5
#supported activity
alpha_P <- 12
beta_P <- 0.5
#accumulation rate/gamma noise
alpha_a <- 1000
beta_a <- 1
#plot(0, 0, xlim = c(0, 700), ylim = c(0, 0.4), type = "n")
#curve(dgamma(x, shape = 62.5, rate = 0.125), from = 0, to = 700, add = TRUE)

ABOR1309_01PU_dat <- list(N = N, L = L, x = x, xL = xL, delta = delta, M=M,
                          rho = rho, rhoL = rhoL, sigma = sigma, sigmaL = sigmaL, 
                          p = p, pL = pL, lambda = lambda,  alpha_phi = alpha_phi, 
                          beta_phi = beta_phi, a_omega = a_omega, b_omega = b_omega, alpha_P = alpha_P, 
                          beta_P = beta_P, alpha_a = alpha_a, beta_a = beta_a)

#run1: alpha_phi = 2, beta_phi = 0.015, a_omega = 3, b_omega = 7
#alpha_P = 2, beta_P = 0.25, alpha_a = 0.001, beta_a = 0.001
#run2: alpha_phi = 2, beta_phi = 0.015, a_omega = 3, b_omega = 7
#alpha_P = 2, beta_P = 0.25, alpha_a = 0.1, beta_a = 0.001
#run3: alpha_phi = 2, beta_phi = 0.015, a_omega = 3, b_omega = 7
#alpha_P = 2, beta_P = 0.25, alpha_a = 0.1, beta_a = 0.001, 10000 iterations
#run4: alpha_phi = 2, beta_phi = 0.015, a_omega = 3, b_omega = 7
#alpha_P = 2, beta_P = 0.25, alpha_a = 1000, beta_a = 2, 20000 iterations
#run5: alpha_phi = 110, beta_phi = 0.5, a_omega = 3, b_omega = 7
#alpha_P = 2, beta_P = 0.25, alpha_a = 1000, beta_a = 2, 20000 iterations
#run6: alpha_phi = 110, beta_phi = 0.5, a_omega = 0.5, b_omega = 0.5
#alpha_P = 2, beta_P = 0.25, alpha_a = 1000, beta_a = 2, 20000 iterations
#run7: alpha_phi = 110, beta_phi = 0.5, a_omega = 0.5, b_omega = 0.5
#alpha_P = 12, beta_P = 0.5, alpha_a = 1000, beta_a = 2, 20000 iterations
#run8: alpha_phi = 110, beta_phi = 0.5, a_omega = 0.5, b_omega = 0.5
#alpha_P = 12, beta_P = 0.5, alpha_a = 250, beta_a = 0.5, 20000 iterations
#run9: alpha_phi = 110, beta_phi = 0.5, a_omega = 0.5, b_omega = 0.5
#alpha_P = 12, beta_P = 0.5, alpha_a = 250, beta_a = 0.5, 100000 iterations
#run10: alpha_phi = 110, beta_phi = 0.5, a_omega = 0.5, b_omega = 0.5
#alpha_P = 12, beta_P = 0.5, alpha_a = 125, beta_a = 0.25, 20000 iterations
#run11: alpha_phi = 110, beta_phi = 0.5, a_omega = 0.5, b_omega = 0.5
#alpha_P = 12, beta_P = 0.5, alpha_a = 62.5, beta_a = 0.125, 20000 iterations
#run12: alpha_phi = 110, beta_phi = 0.5, a_omega = 0.5, b_omega = 0.5
#alpha_P = 12, beta_P = 0.5, alpha_a = 1000, beta_a = 1, 20000 iterations
ABOR1309_01_run11 <- stan(file = 'Pb210_draft.stan', data = ABOR1309_01PU_dat, 
                         chains = 4, iter = 20000, control = list(adapt_delta = 0.99))
saveRDS(ABOR1309_01_run11, "ABOR1309_01_run11.rds")
plot(ABOR1309_01_run11, pars = "ages")
plot(ABOR1309_01_run11, pars = "m")
plot(ABOR1309_01_run11, pars = "alpha")
plot(ABOR1309_01_run11, plotfun = "hist", pars = "phi")
plot(ABOR1309_01_run11, plotfun = "hist", pars = "Ps")
plot(ABOR1309_01_run11, plotfun = "hist", pars = "omega")
plot(ABOR1309_01_run11, plotfun = "hist", pars = "ages")
plot(ABOR1309_01_run11, plotfun = "hist", pars = "ages", binwidth = 0.5)
plot(ABOR1309_01_run11, plotfun = "hist", pars = "alpha", binwidth = 0.5)
plot(ABOR1309_01_run11, plotfun = "hist", pars = "mN")
plot(ABOR1309_01_run11, plotfun = "trace", pars = "omega")
plot(ABOR1309_01_run11, plotfun = "trace", pars = "phi")
plot(ABOR1309_01_run11, plotfun = "trace", pars = "Ps")
plot(ABOR1309_01_run11, plotfun = "trace", pars = "ages")
plot(ABOR1309_01_run11, plotfun = "trace", pars = "mN")
plot(ABOR1309_01_run11, plotfun = "trace", pars = "alpha")
