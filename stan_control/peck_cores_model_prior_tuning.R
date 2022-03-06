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
#more specific priors
#priors
alpha_phi <- 2
#alpha_phi <- 35
beta_phi <- 0.05
a_omega <- 5
b_omega <- 5
alpha_P <- 3
beta_P <- 0.25
#alpha_a <- 0.005
#beta_a <- 0.01
#alpha_a <- 0.05
#beta_a <- 0.1
alpha_a <- 1
beta_a <- 0.04

ABOR1309_01PU_dat <- list(N = N, L = L, x = x, xL = xL, delta = delta, M=M,
                          rho = rho, rhoL = rhoL, sigma = sigma, sigmaL = sigmaL, 
                          p = p, pL = pL, lambda = lambda,  alpha_phi = alpha_phi, 
                          beta_phi = beta_phi, a_omega = a_omega, b_omega = b_omega, alpha_P = alpha_P, 
                          beta_P = beta_P, alpha_a = alpha_a, beta_a = beta_a)

#Run model

#large variance beta_a <- 0.001, alpha_a <- 0.001, o.w. same as in erin_data_stan_control_draft
run2 <- stan(file = 'Pb210_draft.stan', data = ABOR1309_01PU_dat, chains = 4, iter = 100000)
saveRDS(run2, "run2.rds")
run2_fit <- rstan::extract(run2)
save(run2_fit, file="run2_fit.RData")
plot(run2, pars = "ages")
plot(run2, pars = "m")
plot(run2, pars = "alpha")
plot(run2, plotfun = "hist", pars = "phi")
plot(run2, plotfun = "hist", pars = "Ps")
plot(run2, plotfun = "hist", pars = "omega")
plot(run2, plotfun = "hist", pars = "ages")
plot(run2, plotfun = "hist", pars = "ages", binwidth = 0.5)
plot(run2, plotfun = "hist", pars = "alpha", binwidth = 0.5)
plot(run2, plotfun = "hist", pars = "mN")
plot(run2, plotfun = "trace", pars = "omega")
plot(run2, plotfun = "trace", pars = "phi")
plot(run2, plotfun = "trace", pars = "Ps")
plot(run2, plotfun = "trace", pars = "ages")
plot(run2, plotfun = "trace", pars = "mN")
plot(run2, plotfun = "trace", pars = "alpha")

run3 <- stan(file = 'Pb210_draft.stan', data = ABOR1309_01PU_dat, chains = 4, iter = 100000)
run3_fit <- rstan::extract(run3)
saveRDS(run3, "run3.rds")
save(run3_fit, file="run3_fit.RData")
plot(run3, pars = "ages")
plot(run3, pars = "m")
plot(run3, pars = "alpha")
plot(run3, plotfun = "hist", pars = "phi")
plot(run3, plotfun = "hist", pars = "Ps")
plot(run3, plotfun = "hist", pars = "omega")
plot(run3, plotfun = "hist", pars = "ages")
plot(run3, plotfun = "hist", pars = "ages", binwidth = 0.5)
plot(run3, plotfun = "hist", pars = "alpha", binwidth = 0.5)
 plot(run3, plotfun = "hist", pars = "mN")
plot(run3, plotfun = "trace", pars = "omega")
plot(run3, plotfun = "trace", pars = "phi")
plot(run3, plotfun = "trace", pars = "Ps")
plot(run3, plotfun = "trace", pars = "ages")
plot(run3, plotfun = "trace", pars = "mN")
plot(run3, plotfun = "trace", pars = "alpha")

run4 <- stan(file = 'Pb210_draft.stan', data = ABOR1309_01PU_dat, chains = 4, iter = 100000)
saveRDS(run4, "run4.rds")
run4_fit <- rstan::extract(run4)
save(run4_fit, file="run4_fit.RData")
plot(run4, pars = "ages")
plot(run4, pars = "m")
plot(run4, pars = "alpha")
plot(run4, plotfun = "hist", pars = "phi")
plot(run4, plotfun = "hist", pars = "Ps")
plot(run4, plotfun = "hist", pars = "omega")
plot(run4, plotfun = "hist", pars = "ages")
plot(run4, plotfun = "hist", pars = "ages", binwidth = 0.5)
plot(run4, plotfun = "hist", pars = "alpha", binwidth = 0.5)
plot(run4, plotfun = "hist", pars = "mN")
plot(run4, plotfun = "trace", pars = "omega")
plot(run4, plotfun = "trace", pars = "phi")
plot(run4, plotfun = "trace", pars = "Ps")
plot(run4, plotfun = "trace", pars = "ages")
plot(run4, plotfun = "trace", pars = "mN")
plot(run4, plotfun = "trace", pars = "alpha")

run5 <- stan(file = 'Pb210_draft.stan', 
             data = ABOR1309_01PU_dat, chains = 4, iter = 100000, control = list(adapt_delta = 0.99))
saveRDS(run5, "run5.rds")
run5_fit <- rstan::extract(run5)
save(run5_fit, file="run5_fit.RData")
plot(run5, pars = "ages")
plot(run5, pars = "m")
plot(run5, pars = "alpha")
plot(run5, plotfun = "hist", pars = "phi")
plot(run5, plotfun = "hist", pars = "Ps")
plot(run5, plotfun = "hist", pars = "omega")
plot(run5, plotfun = "hist", pars = "ages")
plot(run5, plotfun = "hist", pars = "ages", binwidth = 0.5)
plot(run5, plotfun = "hist", pars = "alpha", binwidth = 0.5)
plot(run5, plotfun = "hist", pars = "mN")
plot(run5, plotfun = "trace", pars = "omega")
plot(run5, plotfun = "trace", pars = "phi")
plot(run5, plotfun = "trace", pars = "Ps")
plot(run5, plotfun = "trace", pars = "ages")
plot(run5, plotfun = "trace", pars = "mN")
plot(run5, plotfun = "trace", pars = "alpha")

run6 <- stan(file = 'Pb210_draft.stan', 
             data = ABOR1309_01PU_dat, chains = 4, iter = 100000, control = list(adapt_delta = 0.99))
saveRDS(run6, "run6.rds")
run6_fit <- rstan::extract(run6)
save(run6_fit, file="run6_fit.RData")
plot(run6, pars = "ages")
plot(run6, pars = "m")
plot(run6, pars = "alpha")
plot(run6, plotfun = "hist", pars = "phi")
plot(run6, plotfun = "hist", pars = "Ps")
plot(run6, plotfun = "hist", pars = "omega")
plot(run6, plotfun = "hist", pars = "ages")
plot(run6, plotfun = "hist", pars = "ages", binwidth = 0.5)
plot(run6, plotfun = "hist", pars = "alpha", binwidth = 0.5)
plot(run6, plotfun = "hist", pars = "mN")
plot(run6, plotfun = "trace", pars = "omega")
plot(run6, plotfun = "trace", pars = "phi")
plot(run6, plotfun = "trace", pars = "Ps")
plot(run6, plotfun = "trace", pars = "ages")
plot(run6, plotfun = "trace", pars = "mN")
plot(run6, plotfun = "trace", pars = "alpha")

run7 <- stan(file = 'Pb210_draft.stan', 
             data = ABOR1309_01PU_dat, chains = 4, iter = 100000, control = list(adapt_delta = 0.9))
saveRDS(run7, "run7.rds")
run7_fit <- rstan::extract(run7)
save(run7_fit, file="run7_fit.RData")
plot(run7, pars = "ages")
plot(run7, pars = "m")
plot(run7, pars = "alpha")
plot(run7, plotfun = "hist", pars = "phi")
plot(run7, plotfun = "hist", pars = "Ps")
plot(run7, plotfun = "hist", pars = "omega")
plot(run7, plotfun = "hist", pars = "ages")
plot(run7, plotfun = "hist", pars = "ages", binwidth = 0.5)
plot(run7, plotfun = "hist", pars = "alpha", binwidth = 0.5)
plot(run7, plotfun = "hist", pars = "mN")
plot(run7, plotfun = "trace", pars = "omega")
plot(run7, plotfun = "trace", pars = "phi")
plot(run7, plotfun = "trace", pars = "Ps")
plot(run7, plotfun = "trace", pars = "ages")
plot(run7, plotfun = "trace", pars = "mN")
plot(run7, plotfun = "trace", pars = "alpha")

#alpha_phi = 5,  alpha_a = .15
run8 <- stan(file = 'Pb210_draft.stan', 
             data = ABOR1309_01PU_dat, chains = 4, iter = 100000, control = list(adapt_delta = 0.99))
saveRDS(run8, "run8.rds")
run8_fit <- rstan::extract(run8)
save(run8_fit, file="run8_fit.RData")
plot(run8, pars = "ages")
plot(run8, pars = "m")
plot(run8, pars = "alpha")
plot(run8, plotfun = "hist", pars = "phi")
plot(run8, plotfun = "hist", pars = "Ps")
plot(run8, plotfun = "hist", pars = "omega")
plot(run8, plotfun = "hist", pars = "ages")
plot(run8, plotfun = "hist", pars = "ages", binwidth = 0.5)
plot(run8, plotfun = "hist", pars = "alpha", binwidth = 0.5)
plot(run8, plotfun = "hist", pars = "mN")
plot(run8, plotfun = "trace", pars = "omega")
plot(run8, plotfun = "trace", pars = "phi")
plot(run8, plotfun = "trace", pars = "Ps")
plot(run8, plotfun = "trace", pars = "ages")
plot(run8, plotfun = "trace", pars = "mN")
plot(run8, plotfun = "trace", pars = "alpha")

#alpha_a <- 0.02, beta_a <- 0.04, alpha_phi <- 35, beta_phi <- 0.95
run9 <- stan(file = 'Pb210_draft.stan', 
             data = ABOR1309_01PU_dat, chains = 4, iter = 100000, control = list(adapt_delta = 0.99))
saveRDS(run9, "run9.rds")
run9_fit <- rstan::extract(run9)
save(run9_fit, file="run9_fit.RData")
plot(run9, pars = "ages")
plot(run9, pars = "m")
plot(run9, pars = "alpha")
plot(run9, plotfun = "hist", pars = "phi")
plot(run9, plotfun = "hist", pars = "Ps")
plot(run9, plotfun = "hist", pars = "omega")
plot(run9, plotfun = "hist", pars = "ages")
plot(run9, plotfun = "hist", pars = "ages", binwidth = 0.5)
plot(run9, plotfun = "hist", pars = "alpha", binwidth = 0.5)
plot(run9, plotfun = "hist", pars = "mN")
plot(run9, plotfun = "trace", pars = "omega")
plot(run9, plotfun = "trace", pars = "phi")
plot(run9, plotfun = "trace", pars = "Ps")
plot(run9, plotfun = "trace", pars = "ages")
plot(run9, plotfun = "trace", pars = "mN")
plot(run9, plotfun = "trace", pars = "alpha")

#alpha_phi = 2, beta_phi = 0.05, alpha_a <- 0.02, beta_a <- 0.04, 
run10 <- stan(file = 'Pb210_draft.stan', 
             data = ABOR1309_01PU_dat, chains = 4, iter = 100000, control = list(adapt_delta = 0.99))
saveRDS(run10, "run10.rds")
run10_fit <- rstan::extract(run10)
save(run10_fit, file="run10_fit.RData")
plot(run10, pars = "ages")
plot(run10, pars = "m")
plot(run10, pars = "alpha")
plot(run10, plotfun = "hist", pars = "phi")
plot(run10, plotfun = "hist", pars = "Ps")
plot(run10, plotfun = "hist", pars = "omega")
plot(run10, plotfun = "hist", pars = "ages")
plot(run10, plotfun = "hist", pars = "ages", binwidth = 0.5)
plot(run10, plotfun = "hist", pars = "alpha", binwidth = 0.5)
plot(run10, plotfun = "hist", pars = "mN")
plot(run10, plotfun = "trace", pars = "omega")
plot(run10, plotfun = "trace", pars = "phi")
plot(run10, plotfun = "trace", pars = "Ps")
plot(run10, plotfun = "trace", pars = "ages")
plot(run10, plotfun = "trace", pars = "mN")
plot(run10, plotfun = "trace", pars = "alpha")

#alpha_a = 1, beta_phi = 0.05, alpha_a <- 0.02, beta_a <- 0.04,
run11 <- stan(file = 'Pb210_draft.stan', 
              data = ABOR1309_01PU_dat, chains = 4, iter = 100000, control = list(adapt_delta = 0.99))
saveRDS(run11, "run11.rds")
run11_fit <- rstan::extract(run11)
save(run11_fit, file="run11_fit.RData")
plot(run11, pars = "ages")
plot(run11, pars = "m")
plot(run11, pars = "alpha")
plot(run11, plotfun = "hist", pars = "phi")
plot(run11, plotfun = "hist", pars = "Ps")
plot(run11, plotfun = "hist", pars = "omega")
plot(run11, plotfun = "hist", pars = "ages")
plot(run11, plotfun = "hist", pars = "ages", binwidth = 0.5)
plot(run11, plotfun = "hist", pars = "alpha", binwidth = 0.5)
plot(run11, plotfun = "hist", pars = "mN", binwidth = 10)
plot(run11, plotfun = "trace", pars = "omega")
plot(run11, plotfun = "trace", pars = "phi")
plot(run11, plotfun = "trace", pars = "Ps")
plot(run11, plotfun = "trace", pars = "ages")
plot(run11, plotfun = "trace", pars = "mN")
plot(run11, plotfun = "trace", pars = "alpha")

#beta_phi = 0.05, alpha_a <- 0.02, beta_a <- 0.04,
run11 <- stan(file = 'Pb210_draft.stan', 
              data = ABOR1309_01PU_dat, chains = 4, iter = 100000, control = list(adapt_delta = 0.99))
saveRDS(run11, "run11.rds")
run11_fit <- rstan::extract(run11)
save(run11_fit, file="run11_fit.RData")
plot(run11, pars = "ages")
plot(run11, pars = "m")
plot(run11, pars = "alpha")
plot(run11, plotfun = "hist", pars = "phi")
plot(run11, plotfun = "hist", pars = "Ps")
plot(run11, plotfun = "hist", pars = "omega")
plot(run11, plotfun = "hist", pars = "ages")
plot(run11, plotfun = "hist", pars = "ages", binwidth = 0.5)
plot(run11, plotfun = "hist", pars = "alpha", binwidth = 0.5)
plot(run11, plotfun = "hist", pars = "mN", binwidth = 10)
plot(run11, plotfun = "trace", pars = "omega")
plot(run11, plotfun = "trace", pars = "phi")
plot(run11, plotfun = "trace", pars = "Ps")
plot(run11, plotfun = "trace", pars = "ages")
plot(run11, plotfun = "trace", pars = "mN")
plot(run11, plotfun = "trace", pars = "alpha")

