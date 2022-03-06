#cmor1706_01pu

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
rho <- CMOR1706_01PU$dry_bulk_density_modeled
rhoL <- CMOR1706_01PU$dry_bulk_density_modeled
sigma <- CMOR1706_01PU$total_pb210_activity_se
sigmaL <- CMOR1706_01PU$pb214_activity_se
p <- CMOR1706_01PU$total_pb210_activity
pL <- CMOR1706_01PU$pb214_activity
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

CMOR1706_01PU_dat <- list(N = N, L = L, x = x, xL = xL, delta = delta, M=M,
                          rho = rho, rhoL = rhoL, sigma = sigma, sigmaL = sigmaL, 
                          p = p, pL = pL, lambda = lambda,  alpha_phi = alpha_phi, 
                          beta_phi = beta_phi, a_omega = a_omega, b_omega = b_omega, alpha_P = alpha_P, 
                          beta_P = beta_P, alpha_a = alpha_a, beta_a = beta_a)
CMOR1706_01PU_run1 <- stan(file = 'Pb210_draft.stan', data = CMOR1706_01PU_dat, chains = 4, iter = 100000, control = list(adapt_delta = 0.99))
saveRDS(CMOR1706_01PU_run1, "CMOR1706_01PU_run1.rds")
plot(CMOR1706_01PU_run1, pars = "ages")
plot(CMOR1706_01PU_run1, pars = "m")
plot(CMOR1706_01PU_run1, pars = "alpha")
plot(CMOR1706_01PU_run1, plotfun = "hist", pars = "phi")
plot(CMOR1706_01PU_run1, plotfun = "hist", pars = "Ps")
plot(CMOR1706_01PU_run1, plotfun = "hist", pars = "omega")
plot(CMOR1706_01PU_run1, plotfun = "hist", pars = "ages")
plot(CMOR1706_01PU_run1, plotfun = "hist", pars = "ages", binwidth = 0.5)
plot(CMOR1706_01PU_run1, plotfun = "hist", pars = "alpha", binwidth = 0.5)
plot(CMOR1706_01PU_run1, plotfun = "hist", pars = "mN")
plot(CMOR1706_01PU_run1, plotfun = "trace", pars = "omega")
plot(CMOR1706_01PU_run1, plotfun = "trace", pars = "phi")
plot(CMOR1706_01PU_run1, plotfun = "trace", pars = "Ps")
plot(CMOR1706_01PU_run1, plotfun = "trace", pars = "ages")
plot(CMOR1706_01PU_run1, plotfun = "trace", pars = "mN")
plot(CMOR1706_01PU_run1, plotfun = "trace", pars = "alpha")

#NBOR1606_03PU

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
rho <- NBOR1606_03PU$dry_bulk_density_modeled
rhoL <- NBOR1606_03PU$dry_bulk_density_modeled
sigma <- NBOR1606_03PU$total_pb210_activity_se
sigmaL <- NBOR1606_03PU$pb214_activity_se
p <- NBOR1606_03PU$total_pb210_activity
pL <- NBOR1606_03PU$pb214_activity
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

NBOR1606_03PU_dat <- list(N = N, L = L, x = x, xL = xL, delta = delta, M=M,
                          rho = rho, rhoL = rhoL, sigma = sigma, sigmaL = sigmaL, 
                          p = p, pL = pL, lambda = lambda,  alpha_phi = alpha_phi, 
                          beta_phi = beta_phi, a_omega = a_omega, b_omega = b_omega, alpha_P = alpha_P, 
                          beta_P = beta_P, alpha_a = alpha_a, beta_a = beta_a)
NBOR1606_03PU_run1 <- stan(file = 'Pb210_draft.stan', data = NBOR1606_03PU_dat, chains = 4, iter = 100000, control = list(adapt_delta = 0.99))
saveRDS(NBOR1606_03PU_run1, "NBOR1606_03PU_run1.rds")

plot(NBOR1606_03PU_run1, pars = "ages")
plot(NBOR1606_03PU_run1, pars = "m")
plot(NBOR1606_03PU_run1, pars = "alpha")
plot(NBOR1606_03PU_run1, plotfun = "hist", pars = "phi")
plot(NBOR1606_03PU_run1, plotfun = "hist", pars = "Ps")
plot(NBOR1606_03PU_run1, plotfun = "hist", pars = "omega")
plot(NBOR1606_03PU_run1, plotfun = "hist", pars = "ages")
plot(NBOR1606_03PU_run1, plotfun = "hist", pars = "ages", binwidth = 0.5)
plot(NBOR1606_03PU_run1, plotfun = "hist", pars = "alpha", binwidth = 0.5)
plot(NBOR1606_03PU_run1, plotfun = "hist", pars = "mN")
plot(NBOR1606_03PU_run1, plotfun = "trace", pars = "omega")
plot(NBOR1606_03PU_run1, plotfun = "trace", pars = "phi")
plot(NBOR1606_03PU_run1, plotfun = "trace", pars = "Ps")
plot(NBOR1606_03PU_run1, plotfun = "trace", pars = "ages")
plot(NBOR1606_03PU_run1, plotfun = "trace", pars = "mN")
plot(NBOR1606_03PU_run1, plotfun = "trace", pars = "alpha")

