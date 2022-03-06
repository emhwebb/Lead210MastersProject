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
#SROR1509_03PU
SROR1509_03PU <- peck_cores %>% 
  filter(core_id == "SROR1509_03PU", depth_max <= 25) %>%
  select(depth_max, dry_bulk_density_modeled, total_pb210_activity, total_pb210_activity_se,
         pb214_activity, pb214_activity_se, excess_pb210_activity, excess_pb210_activity_se)	
N <- 11
M <- 12
L <- 11
Tot <- N+L
delta <- 2
x <- SROR1509_03PU$depth_max
xL <- SROR1509_03PU$depth_max
rho <- SROR1509_03PU$dry_bulk_density_modeled*gcm3_gcm2
rhoL <- SROR1509_03PU$dry_bulk_density_modeled*gcm3_gcm2
sigma <- SROR1509_03PU$total_pb210_activity_se
sigmaL <- SROR1509_03PU$pb214_activity_se
p <- SROR1509_03PU$total_pb210_activity
pL <- SROR1509_03PU$pb214_activity
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
beta_a <- 0.14
#plot(0, 0, xlim = c(0, 50), ylim = c(0, 0.4), type = "n")
#curve(dgamma(x, shape = 1, rate = 0.2), from = 0, to = 50, add = TRUE)

SROR1509_03PU_dat <- list(N = N, L = L, Tot = Tot, x = x, xL = xL, delta = delta, M=M,
                          rho = rho, rhoL = rhoL, a = 3, b = 4, sigma = sigma, sigmaL = sigmaL, 
                          p = p, pL = pL, lambda = lambda,  alpha_phi = alpha_phi, 
                          beta_phi = beta_phi, a_omega = a_omega, b_omega = b_omega, alpha_P = alpha_P, 
                          beta_P = beta_P, alpha_a = alpha_a, beta_a = beta_a)


SROR1509_03_initAugust6nonconst <- stan(file = 'Pb210_nonconst_tdistr.stan', data = SROR1509_03PU_dat, 
                                        chains = 4, iter = 200000, control = list(adapt_delta = 0.99))
saveRDS(SROR1509_03_initAugust6nonconst, "SROR1509_03_initAugust6nonconst.rds")

#save to .csv
SROR1509_03_initAugust6nonconst_list <- extract(SROR1509_03_initAugust6nonconst)
#ages
SROR1509_03_initAugust6nonconst_ages <- data.frame(SROR1509_03_initAugust6nonconst_list$ages)
colnames(SROR1509_03_initAugust6nonconst_ages) <- paste0("Age", 1:ncol(SROR1509_03_initAugust6nonconst_ages))
#sedi rates
SROR1509_03_initAugust6nonconst_sedirate <- data.frame(SROR1509_03_initAugust6nonconst_list$m)
colnames(SROR1509_03_initAugust6nonconst_sedirate) <- paste0("m", 1:ncol(SROR1509_03_initAugust6nonconst_sedirate))
#accu rates
SROR1509_03_initAugust6nonconst_accurate <- data.frame(SROR1509_03_initAugust6nonconst_list$accu_rates)
colnames(SROR1509_03_initAugust6nonconst_accurate) <- paste0("m", 1:ncol(SROR1509_03_initAugust6nonconst_accurate))
SROR1509_03_initAugust6nonconst_param <- data.frame(cbind(SROR1509_03_initAugust6nonconst_list$phi, 
                                                          SROR1509_03_initAugust6nonconst_list$Ps,
                                                          SROR1509_03_initAugust6nonconst_list$omega))
colnames(SROR1509_03_initAugust6nonconst_param) <- c("Supply", "SuppAct", "Memory")
write.csv(SROR1509_03_initAugust6nonconst_ages, "SROR1509_03_initAugust6nonconst_ages.csv")
write.csv(SROR1509_03_initAugust6nonconst_sedirate, "SROR1509_03_initAugust6nonconst_sedirate.csv")
write.csv(SROR1509_03_initAugust6nonconst_accurate, "SROR1509_03_initAugust6nonconst_accurate.csv")
write.csv(SROR1509_03_initAugust6nonconst_param, "SROR1509_03_initAugust6nonconst_param.csv")

#save summaries to .csv

#ages
SROR1509_03_initAugust6nonconst_ages_summ <- t(apply(X = SROR1509_03_initAugust6nonconst_ages[,1:ncol(SROR1509_03_initAugust6nonconst_ages)], 
                                                     MARGIN = 2, 
                                                     FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(SROR1509_03_initAugust6nonconst_ages_summ)[1:2] <- c("mean", "sd")

#sedi rates
SROR1509_03_initAugust6nonconst_sedirate_summ <- t(apply(X = SROR1509_03_initAugust6nonconst_sedirate[,1:ncol(SROR1509_03_initAugust6nonconst_sedirate)], 
                                                         MARGIN = 2, 
                                                         FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(SROR1509_03_initAugust6nonconst_sedirate_summ)[1:2] <- c("mean", "sd")

#accu rates
SROR1509_03_initAugust6nonconst_accurate_summ <- t(apply(X = SROR1509_03_initAugust6nonconst_accurate[,1:ncol(SROR1509_03_initAugust6nonconst_accurate)], 
                                                         MARGIN = 2, 
                                                         FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(SROR1509_03_initAugust6nonconst_accurate_summ)[1:2] <- c("mean", "sd")
write.csv(SROR1509_03_initAugust6nonconst_ages_summ, "SROR1509_03_initAugust6nonconst_ages_summ.csv", row.names = F)
write.csv(SROR1509_03_initAugust6nonconst_sedirate_summ, "SROR1509_03_initAugust6nonconst_sedirate_summ.csv", row.names = F)
write.csv(SROR1509_03_initAugust6nonconst_accurate_summ, "SROR1509_03_initAugust6nonconst_accurate_summ.csv", row.names = F)
