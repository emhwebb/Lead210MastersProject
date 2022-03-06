library(readr)
library(tidyverse)
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

#conversion factors
#dpm/g to bq/g
dpmg_bqg <- 1/60
#g/cm^3 to g/cm^2
gcm3_gcm2 <- 2

#try with new priors
peck_cores <- read_csv("peck_et_al_2020_depthseries.csv")

#ABOR1309_02PU
ABOR1309_02PU <- peck_cores %>% 
  filter(core_id == "ABOR1309_02PU", depth_max <= 24) %>%
  select(depth_max, dry_bulk_density_modeled, total_pb210_activity, total_pb210_activity_se,
         pb214_activity, pb214_activity_se, excess_pb210_activity, excess_pb210_activity_se)	
N <- 12
M <- 13
L <- 12
#Tot <- N+L
delta <- 2
x <- ABOR1309_02PU$depth_max
xL <- ABOR1309_02PU$depth_max
rho <- ABOR1309_02PU$dry_bulk_density_modeled*gcm3_gcm2
rhoL <- ABOR1309_02PU$dry_bulk_density_modeled*gcm3_gcm2
sigma <- ABOR1309_02PU$total_pb210_activity_se
sigmaL <- ABOR1309_02PU$pb214_activity_se
p <- ABOR1309_02PU$total_pb210_activity
pL <- ABOR1309_02PU$pb214_activity
lambda <- 0.03114 
#omega <- 0.5
#priors
#yearly accumulation
alpha_phi <- 1
beta_phi <- 0.5
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
beta_a <- 0.12
#plot(0, 0, xlim = c(0, 50), ylim = c(0, 0.4), type = "n")
#curve(dgamma(x, shape = 1, rate = 0.2), from = 0, to = 50, add = TRUE)
a <- 3
b <- 4

ABOR1309_02PU_dat <- list(N = N, L = L, x = x, xL = xL, delta = delta, M=M,
                          rho = rho, rhoL = rhoL, a = 3, b = 4, sigma = sigma, sigmaL = sigmaL, 
                          p = p, pL = pL, lambda = lambda,  alpha_phi = alpha_phi, 
                          beta_phi = beta_phi, a_omega = a_omega, b_omega = b_omega, alpha_P = alpha_P, 
                          beta_P = beta_P, alpha_a = alpha_a, beta_a = beta_a)

ABOR1309_02_alttest3June16 <- stan(file = 'Pb210_tdistr_alt.stan', data = ABOR1309_02PU_dat, 
                                chains = 4, iter = 200000, control = list(adapt_delta = 0.99))
saveRDS(ABOR1309_02_alttest3June16, "ABOR1309_02_alttest3June16.rds")

#save to .csv
ABOR1309_02_alttest3June16_list <- extract(ABOR1309_02_alttest3June16)
ABOR1309_02_alttest3June16_ages <- data.frame(ABOR1309_02_alttest3June16_list$ages)
colnames(ABOR1309_02_alttest3June16_ages) <- paste0("Age", 1:ncol(ABOR1309_02_alttest3June16_ages))
ABOR1309_02_alttest3June16_accurate <- data.frame(ABOR1309_02_alttest3June16_list$m)
colnames(ABOR1309_02_alttest3June16_accurate) <- paste0("m", 1:ncol(ABOR1309_02_alttest3June16_accurate))
ABOR1309_02_alttest3June16_param <- data.frame(cbind(ABOR1309_02_alttest3June16_list$phi, 
                                                  ABOR1309_02_alttest3June16_list$Ps,
                                                  ABOR1309_02_alttest3June16_list$omega))
colnames(ABOR1309_02_alttest3June16_param) <- c("Supply", "SuppAct", "Memory")
write.csv(ABOR1309_02_alttest3June16_ages, "ABOR1309_02_alttest3June16_ages.csv")
write.csv(ABOR1309_02_alttest3June16_accurate, "ABOR1309_02_alttest3June16_accurate.csv")
write.csv(ABOR1309_02_alttest3June16_param, "ABOR1309_02_alttest3June16_param.csv")

#save summaries to .csv
ABOR1309_02_alttest3June16_ages_summ <- t(apply(X = ABOR1309_02_alttest3June16_ages[,1:ncol(ABOR1309_02_alttest3June16_ages)], 
                                             MARGIN = 2, 
                                             FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(ABOR1309_02_alttest3June16_ages_summ)[1:2] <- c("mean", "sd")

ABOR1309_02_alttest3June16_accurate_summ <- t(apply(X = ABOR1309_02_alttest3June16_accurate[,1:ncol(ABOR1309_02_alttest3June16_accurate)], 
                                                 MARGIN = 2, 
                                                 FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(ABOR1309_02_alttest3June16_accurate_summ)[1:2] <- c("mean", "sd")
write.csv(ABOR1309_02_alttest3June16_ages_summ, "ABOR1309_02_alttest3June16_ages_summ.csv", row.names = F)
write.csv(ABOR1309_02_alttest3June16_accurate_summ, "ABOR1309_02_alttest3June16_accurate_summ.csv", row.names = F)

plot(ABOR1309_02_alttest3June16, pars = "ages")
plot(ABOR1309_02_alttest3June16, pars = "m")
plot(ABOR1309_02_alttest3June16, pars = "alpha")
plot(ABOR1309_02_alttest3June16, pars = "omega")
plot(ABOR1309_02_alttest3June16, plotfun = "hist", pars = "phi")
plot(ABOR1309_02_alttest3June16, plotfun = "hist", pars = "Ps")
plot(ABOR1309_02_alttest3June16, plotfun = "hist", pars = "omega")
plot(ABOR1309_02_alttest3June16, plotfun = "hist", pars = "ages")
plot(ABOR1309_02_alttest3June16, plotfun = "hist", pars = "ages", binwidth = 0.5)
plot(ABOR1309_02_alttest3June16, plotfun = "hist", pars = "alpha")
plot(ABOR1309_02_alttest3June16, plotfun = "hist", pars = "m")
plot(ABOR1309_02_alttest3June16, plotfun = "hist", pars = "mN")
plot(ABOR1309_02_alttest3June16, plotfun = "trace", pars = "omega")
plot(ABOR1309_02_alttest3June16, plotfun = "trace", pars = "phi")
plot(ABOR1309_02_alttest3June16, plotfun = "trace", pars = "Ps")
plot(ABOR1309_02_alttest3June16, plotfun = "trace", pars = "ages")
plot(ABOR1309_02_alttest3June16, plotfun = "trace", pars = "mN")
plot(ABOR1309_02_alttest3June16, plotfun = "trace", pars = "alpha")
