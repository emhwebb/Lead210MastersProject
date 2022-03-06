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


peck_cores <- read_csv("peck_et_al_2020_depthseries.csv")

#nonconstant supported activity
#NTOR1802_10PU
NTOR1802_10PU <- peck_cores %>% 
  filter(core_id == "NTOR1802_10PU", depth_max <= 22) %>%
  select(depth_max, dry_bulk_density_modeled, total_pb210_activity, total_pb210_activity_se,
         pb214_activity, pb214_activity_se, excess_pb210_activity, excess_pb210_activity_se)	
N <- 11
M <- 12
L <- 11
Tot <- N+L
delta <- 2
x <- NTOR1802_10PU$depth_max
xL <- NTOR1802_10PU$depth_max
rho <- NTOR1802_10PU$dry_bulk_density_modeled*gcm3_gcm2
rhoL <- NTOR1802_10PU$dry_bulk_density_modeled*gcm3_gcm2
sigma <- NTOR1802_10PU$total_pb210_activity_se
sigmaL <- NTOR1802_10PU$pb214_activity_se
p <- NTOR1802_10PU$total_pb210_activity
pL <- NTOR1802_10PU$pb214_activity
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
beta_a <- 0.23
#plot(0, 0, xlim = c(0, 50), ylim = c(0, 0.4), type = "n")
#curve(dgamma(x, shape = 1, rate = 0.2), from = 0, to = 50, add = TRUE)

NTOR1802_10PU_dat <- list(N = N, L = L, Tot = Tot, x = x, xL = xL, delta = delta, M=M,
                          rho = rho, rhoL = rhoL, a = 3, b = 4, sigma = sigma, sigmaL = sigmaL, 
                          p = p, pL = pL, lambda = lambda,  alpha_phi = alpha_phi, 
                          beta_phi = beta_phi, a_omega = a_omega, b_omega = b_omega, alpha_P = alpha_P, 
                          beta_P = beta_P, alpha_a = alpha_a, beta_a = beta_a)


NTOR1802_10_initAugust6nonconst <- stan(file = 'Pb210_nonconst_tdistr.stan', data = NTOR1802_10PU_dat, 
                                        chains = 4, iter = 200000, control = list(adapt_delta = 0.99))
saveRDS(NTOR1802_10_initAugust6nonconst, "NTOR1802_10_initAugust6nonconst.rds")

#save to .csv
NTOR1802_10_initAugust6nonconst_list <- extract(NTOR1802_10_initAugust6nonconst)
#ages
NTOR1802_10_initAugust6nonconst_ages <- data.frame(NTOR1802_10_initAugust6nonconst_list$ages)
colnames(NTOR1802_10_initAugust6nonconst_ages) <- paste0("Age", 1:ncol(NTOR1802_10_initAugust6nonconst_ages))
#sedi rates
NTOR1802_10_initAugust6nonconst_sedirate <- data.frame(NTOR1802_10_initAugust6nonconst_list$m)
colnames(NTOR1802_10_initAugust6nonconst_sedirate) <- paste0("m", 1:ncol(NTOR1802_10_initAugust6nonconst_sedirate))
#accu rates
NTOR1802_10_initAugust6nonconst_accurate <- data.frame(NTOR1802_10_initAugust6nonconst_list$accu_rates)
colnames(NTOR1802_10_initAugust6nonconst_accurate) <- paste0("m", 1:ncol(NTOR1802_10_initAugust6nonconst_accurate))
NTOR1802_10_initAugust6nonconst_param <- data.frame(cbind(NTOR1802_10_initAugust6nonconst_list$phi, 
                                                          NTOR1802_10_initAugust6nonconst_list$Ps,
                                                          NTOR1802_10_initAugust6nonconst_list$omega))
colnames(NTOR1802_10_initAugust6nonconst_param) <- c("Supply", "SuppAct", "Memory")
write.csv(NTOR1802_10_initAugust6nonconst_ages, "NTOR1802_10_initAugust6nonconst_ages.csv")
write.csv(NTOR1802_10_initAugust6nonconst_sedirate, "NTOR1802_10_initAugust6nonconst_sedirate.csv")
write.csv(NTOR1802_10_initAugust6nonconst_accurate, "NTOR1802_10_initAugust6nonconst_accurate.csv")
write.csv(NTOR1802_10_initAugust6nonconst_param, "NTOR1802_10_initAugust6nonconst_param.csv")

#save summaries to .csv

#ages
NTOR1802_10_initAugust6nonconst_ages_summ <- t(apply(X = NTOR1802_10_initAugust6nonconst_ages[,1:ncol(NTOR1802_10_initAugust6nonconst_ages)], 
                                                     MARGIN = 2, 
                                                     FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(NTOR1802_10_initAugust6nonconst_ages_summ)[1:2] <- c("mean", "sd")

#sedi rates
NTOR1802_10_initAugust6nonconst_sedirate_summ <- t(apply(X = NTOR1802_10_initAugust6nonconst_sedirate[,1:ncol(NTOR1802_10_initAugust6nonconst_sedirate)], 
                                                         MARGIN = 2, 
                                                         FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(NTOR1802_10_initAugust6nonconst_sedirate_summ)[1:2] <- c("mean", "sd")

#accu rates
NTOR1802_10_initAugust6nonconst_accurate_summ <- t(apply(X = NTOR1802_10_initAugust6nonconst_accurate[,1:ncol(NTOR1802_10_initAugust6nonconst_accurate)], 
                                                         MARGIN = 2, 
                                                         FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(NTOR1802_10_initAugust6nonconst_accurate_summ)[1:2] <- c("mean", "sd")
write.csv(NTOR1802_10_initAugust6nonconst_ages_summ, "NTOR1802_10_initAugust6nonconst_ages_summ.csv", row.names = F)
write.csv(NTOR1802_10_initAugust6nonconst_sedirate_summ, "NTOR1802_10_initAugust6nonconst_sedirate_summ.csv", row.names = F)
write.csv(NTOR1802_10_initAugust6nonconst_accurate_summ, "NTOR1802_10_initAugust6nonconst_accurate_summ.csv", row.names = F)
