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

#CMOR1706_03PU
CMOR1706_03PU <- peck_cores %>% 
  filter(core_id == "CMOR1706_03PU", depth_max <= 18) %>%
  select(depth_max, dry_bulk_density_modeled, total_pb210_activity, total_pb210_activity_se,
         pb214_activity, pb214_activity_se, excess_pb210_activity, excess_pb210_activity_se)

N <- 9
M <- 10
L <- 9
delta <- 2
x <- CMOR1706_03PU$depth_max
xL <- CMOR1706_03PU$depth_max
rho <- CMOR1706_03PU$dry_bulk_density_modeled*gcm3_gcm2
rhoL <- CMOR1706_03PU$dry_bulk_density_modeled*gcm3_gcm2
sigma <- CMOR1706_03PU$total_pb210_activity_se
sigmaL <- CMOR1706_03PU$pb214_activity_se
p <- CMOR1706_03PU$total_pb210_activity
pL <- CMOR1706_03PU$pb214_activity
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
beta_a <- 0.11

CMOR1706_03PU_dat <- list(N = N, L = L, x = x, xL = xL, delta = delta, M=M,
                          rho = rho, rhoL = rhoL, sigma = sigma, sigmaL = sigmaL, 
                          p = p, pL = pL, lambda = lambda,  alpha_phi = alpha_phi, 
                          beta_phi = beta_phi, a_omega = a_omega, b_omega = b_omega, alpha_P = alpha_P, 
                          beta_P = beta_P, alpha_a = alpha_a, beta_a = beta_a)

CMOR1706_03PU_initMay27 <- stan(file = 'Pb210_draft.stan', data = CMOR1706_03PU_dat, 
                               chains = 4, iter = 200000, control = list(adapt_delta = 0.99))
saveRDS(CMOR1706_03PU_initMay27, "CMOR1706_03PU_initMay27.rds")

CMOR1706_03PU_initMay27_list <- extract(CMOR1706_03PU_initMay27)
CMOR1706_03PU_initMay27_ages <- data.frame(CMOR1706_03PU_initMay27_list$ages)
colnames(CMOR1706_03PU_initMay27_ages) <- paste0("Age", 1:ncol(CMOR1706_03PU_initMay27_ages))
CMOR1706_03PU_initMay27_accurate <- data.frame(CMOR1706_03PU_initMay27_list$m)
colnames(CMOR1706_03PU_initMay27_accurate) <- paste0("m", 1:ncol(CMOR1706_03PU_initMay27_accurate))
CMOR1706_03PU_initMay27_param <- data.frame(cbind(CMOR1706_03PU_initMay27_list$phi, 
                                                 CMOR1706_03PU_initMay27_list$Ps,
                                                 CMOR1706_03PU_initMay27_list$omega))
colnames(CMOR1706_03PU_initMay27_param) <- c("Supply", "SuppAct", "Memory")
write.csv(CMOR1706_03PU_initMay27_ages, "CMOR1706_03PU_initMay27_ages.csv")
write.csv(CMOR1706_03PU_initMay27_accurate, "CMOR1706_03PU_initMay27_accurate.csv")
write.csv(CMOR1706_03PU_initMay27_param, "CMOR1706_03PU_initMay27_param.csv")

CMOR1706_03PU_initMay27_ages_summ <- t(apply(X = CMOR1706_03PU_initMay27_ages[,1:ncol(CMOR1706_03PU_initMay27_ages)], 
                                            MARGIN = 2, 
                                            FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(CMOR1706_03PU_initMay27_ages_summ)[1:2] <- c("mean", "sd")

CMOR1706_03PU_initMay27_accurate_summ <- t(apply(X = CMOR1706_03PU_initMay27_accurate[,1:ncol(CMOR1706_03PU_initMay27_accurate)], 
                                                MARGIN = 2, 
                                                FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(CMOR1706_03PU_initMay27_accurate_summ)[1:2] <- c("mean", "sd")
write.csv(CMOR1706_03PU_initMay27_ages_summ, "CMOR1706_03PU_initMay27_ages_summ.csv", row.names = F)
write.csv(CMOR1706_03PU_initMay27_accurate_summ, "CMOR1706_03PU_initMay27_accurate_summ.csv", row.names = F)

#nonconstant support
#CMOR1706_03PU
CMOR1706_03PU <- peck_cores %>% 
  filter(core_id == "CMOR1706_03PU", depth_max <= 18) %>%
  select(depth_max, dry_bulk_density_modeled, total_pb210_activity, total_pb210_activity_se,
         pb214_activity, pb214_activity_se, excess_pb210_activity, excess_pb210_activity_se)

N <- 9
M <- 10
L <- 9
delta <- 2
x <- CMOR1706_03PU$depth_max
xL <- CMOR1706_03PU$depth_max
rho <- CMOR1706_03PU$dry_bulk_density_modeled*gcm3_gcm2
rhoL <- CMOR1706_03PU$dry_bulk_density_modeled*gcm3_gcm2
sigma <- CMOR1706_03PU$total_pb210_activity_se
sigmaL <- CMOR1706_03PU$pb214_activity_se
p <- CMOR1706_03PU$total_pb210_activity
pL <- CMOR1706_03PU$pb214_activity
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
beta_a <- 0.11

CMOR1706_03PU_dat <- list(N = N, L = L, x = x, xL = xL, delta = delta, M=M,
                          rho = rho, rhoL = rhoL, sigma = sigma, sigmaL = sigmaL, 
                          p = p, pL = pL, lambda = lambda,  alpha_phi = alpha_phi, 
                          beta_phi = beta_phi, a_omega = a_omega, b_omega = b_omega, alpha_P = alpha_P, 
                          beta_P = beta_P, alpha_a = alpha_a, beta_a = beta_a)

CMOR1706_03PU_initMay27nonconst<- stan(file = 'Pb210_nonconst_supp.stan', data = CMOR1706_03PU_dat, 
                                chains = 4, iter = 200000, control = list(adapt_delta = 0.99))
saveRDS(CMOR1706_03PU_initMay27nonconst, "CMOR1706_03PU_initMay27nonconst.rds")

CMOR1706_03PU_initMay27nonconst_list <- extract(CMOR1706_03PU_initMay27nonconst)
CMOR1706_03PU_initMay27nonconst_ages <- data.frame(CMOR1706_03PU_initMay27nonconst_list$ages)
colnames(CMOR1706_03PU_initMay27nonconst_ages) <- paste0("Age", 1:ncol(CMOR1706_03PU_initMay27nonconst_ages))
CMOR1706_03PU_initMay27nonconst_accurate <- data.frame(CMOR1706_03PU_initMay27nonconst_list$m)
colnames(CMOR1706_03PU_initMay27nonconst_accurate) <- paste0("m", 1:ncol(CMOR1706_03PU_initMay27nonconst_accurate))
CMOR1706_03PU_initMay27nonconst_param <- data.frame(cbind(CMOR1706_03PU_initMay27nonconst_list$phi, 
                                                  CMOR1706_03PU_initMay27nonconst_list$Ps,
                                                  CMOR1706_03PU_initMay27nonconst_list$omega))
colnames(CMOR1706_03PU_initMay27nonconst_param) <- c("Supply", "SuppAct", "Memory")
write.csv(CMOR1706_03PU_initMay27nonconst_ages, "CMOR1706_03PU_initMay27nonconst_ages.csv")
write.csv(CMOR1706_03PU_initMay27nonconst_accurate, "CMOR1706_03PU_initMay27nonconst_accurate.csv")
write.csv(CMOR1706_03PU_initMay27nonconst_param, "CMOR1706_03PU_initMay27nonconst_param.csv")

CMOR1706_03PU_initMay27nonconst_ages_summ <- t(apply(X = CMOR1706_03PU_initMay27nonconst_ages[,1:ncol(CMOR1706_03PU_initMay27nonconst_ages)], 
                                             MARGIN = 2, 
                                             FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(CMOR1706_03PU_initMay27nonconst_ages_summ)[1:2] <- c("mean", "sd")

CMOR1706_03PU_initMay27nonconst_accurate_summ <- t(apply(X = CMOR1706_03PU_initMay27nonconst_accurate[,1:ncol(CMOR1706_03PU_initMay27nonconst_accurate)], 
                                                 MARGIN = 2, 
                                                 FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(CMOR1706_03PU_initMay27nonconst_accurate_summ)[1:2] <- c("mean", "sd")
write.csv(CMOR1706_03PU_initMay27nonconst_ages_summ, "CMOR1706_03PU_initMay27nonconst_ages_summ.csv", row.names = F)
write.csv(CMOR1706_03PU_initMay27nonconst_accurate_summ, "CMOR1706_03PU_initMay27nonconst_accurate_summ.csv", row.names = F)
