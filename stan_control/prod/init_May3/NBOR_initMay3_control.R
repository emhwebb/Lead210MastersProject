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

NBOR1606_03PU_initMay3 <- stan(file = 'Pb210_draft.stan', data = NBOR1606_03PU_dat, chains = 4, 
                           iter = 200000, control = list(adapt_delta = 0.99))
saveRDS(NBOR1606_03PU_initMay3, "NBOR1606_03PU_initMay3.rds")

#NBOR1606_04PU
NBOR1606_04PU <- peck_cores %>% 
  filter(core_id == "NBOR1606_04PU", depth_max <= 28) %>%
  select(depth_max, dry_bulk_density_modeled, total_pb210_activity, total_pb210_activity_se,
         pb214_activity, pb214_activity_se, excess_pb210_activity, excess_pb210_activity_se)

N <- 14
M <- 15
L <- 14
delta <- 2
x <- NBOR1606_04PU$depth_max
xL <- NBOR1606_04PU$depth_max
rho <- NBOR1606_04PU$dry_bulk_density_modeled*gcm3_gcm2
rhoL <- NBOR1606_04PU$dry_bulk_density_modeled*gcm3_gcm2
sigma <- NBOR1606_04PU$total_pb210_activity_se
sigmaL <- NBOR1606_04PU$pb214_activity_se
p <- NBOR1606_04PU$total_pb210_activity
pL <- NBOR1606_04PU$pb214_activity
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

NBOR1606_04PU_dat <- list(N = N, L = L, x = x, xL = xL, delta = delta, M=M,
                          rho = rho, rhoL = rhoL, sigma = sigma, sigmaL = sigmaL, 
                          p = p, pL = pL, lambda = lambda,  alpha_phi = alpha_phi, 
                          beta_phi = beta_phi, a_omega = a_omega, b_omega = b_omega, alpha_P = alpha_P, 
                          beta_P = beta_P, alpha_a = alpha_a, beta_a = beta_a)
#run1: alpha_phi = 1, beta_phi = 0.125, a_omega = 0.5, b_omega = 0.5
#alpha_P = 1, beta_P = 0.5, alpha_a = 1, beta_a = 0.2, iter = 20000
#run2: alpha_phi = 1, beta_phi = 0.5, a_omega = 0.5, b_omega = 0.5
#alpha_P = 1, beta_P = 0.5, alpha_a = 1, beta_a = 0.2, iter = 100000

NBOR1606_04PU_initMay3 <- stan(file = 'Pb210_draft.stan', data = NBOR1606_04PU_dat, chains = 4, 
                               iter = 200000, control = list(adapt_delta = 0.99))
saveRDS(NBOR1606_04PU_initMay3, "NBOR1606_04PU_initMay3.rds")

#NBOR1606_08PU had divergent transitions
NBOR1606_08PU <- peck_cores %>% 
  filter(core_id == "NBOR1606_08PU", depth_max <= 44) %>%
  select(depth_max, dry_bulk_density_modeled, total_pb210_activity, total_pb210_activity_se,
         pb214_activity, pb214_activity_se, excess_pb210_activity, excess_pb210_activity_se)

N <- 22
M <- 23
L <- 22
delta <- 2
x <- NBOR1606_08PU$depth_max
xL <- NBOR1606_08PU$depth_max
rho <- NBOR1606_08PU$dry_bulk_density_modeled*gcm3_gcm2
rhoL <- NBOR1606_08PU$dry_bulk_density_modeled*gcm3_gcm2
sigma <- NBOR1606_08PU$total_pb210_activity_se
sigmaL <- NBOR1606_08PU$pb214_activity_se
p <- NBOR1606_08PU$total_pb210_activity
pL <- NBOR1606_08PU$pb214_activity
lambda <- 0.03114 
#omega <- 0.5
#more specific priors
#priors
#yearly accumulation
alpha_phi <- 1
beta_phi <- 0.5
#memory parameter
a_omega <-  1
b_omega <- 5
#supported activity
alpha_P <- 1
beta_P <- 0.5
#accumulation rate/gamma noise
alpha_a <- 1
beta_a <- 0.2

NBOR1606_08PU_dat <- list(N = N, L = L, x = x, xL = xL, delta = delta, M=M,
                          rho = rho, rhoL = rhoL, sigma = sigma, sigmaL = sigmaL, 
                          p = p, pL = pL, lambda = lambda,  alpha_phi = alpha_phi, 
                          beta_phi = beta_phi, a_omega = a_omega, b_omega = b_omega, alpha_P = alpha_P, 
                          beta_P = beta_P, alpha_a = alpha_a, beta_a = beta_a)
#run1: alpha_phi = 1, beta_phi = 0.125, a_omega = 0.5, b_omega = 0.5
#alpha_P = 1, beta_P = 0.5, alpha_a = 1, beta_a = 0.2, iter = 20000
#run2: alpha_phi = 1, beta_phi = 0.5, a_omega = 0.5, b_omega = 0.5
#alpha_P = 1, beta_P = 0.5, alpha_a = 1, beta_a = 0.2, iter = 100000

NBOR1606_08PU_initMay3_run2 <- stan(file = 'Pb210_draft.stan', data = NBOR1606_08PU_dat, chains = 4, 
                               iter = 200000, control = list(adapt_delta = 0.99))
saveRDS(NBOR1606_08PU_initMay3_run2, "NBOR1606_08PU_initMay3_run2.rds")

#NBOR1609_01PU
NBOR1609_01PU <- peck_cores %>% 
  filter(core_id == "NBOR1609_01PU", depth_max <= 50) %>%
  select(depth_max, dry_bulk_density_modeled, total_pb210_activity, total_pb210_activity_se,
         pb214_activity, pb214_activity_se, excess_pb210_activity, excess_pb210_activity_se)

N <- 25
M <- 26
L <- 25
delta <- 2
x <- NBOR1609_01PU$depth_max
xL <- NBOR1609_01PU$depth_max
rho <- NBOR1609_01PU$dry_bulk_density_modeled*gcm3_gcm2
rhoL <- NBOR1609_01PU$dry_bulk_density_modeled*gcm3_gcm2
sigma <- NBOR1609_01PU$total_pb210_activity_se
sigmaL <- NBOR1609_01PU$pb214_activity_se
p <- NBOR1609_01PU$total_pb210_activity
pL <- NBOR1609_01PU$pb214_activity
lambda <- 0.03114 
#omega <- 0.5
#more specific priors
#priors
#yearly accumulation
alpha_phi <- 1
beta_phi <- 0.5
#memory parameter
a_omega <- 1
b_omega <- 5
#supported activity
alpha_P <- 1
beta_P <- 0.5
#accumulation rate/gamma noise
alpha_a <- 1
beta_a <- 0.2

NBOR1609_01PU_dat <- list(N = N, L = L, x = x, xL = xL, delta = delta, M=M,
                          rho = rho, rhoL = rhoL, sigma = sigma, sigmaL = sigmaL, 
                          p = p, pL = pL, lambda = lambda,  alpha_phi = alpha_phi, 
                          beta_phi = beta_phi, a_omega = a_omega, b_omega = b_omega, alpha_P = alpha_P, 
                          beta_P = beta_P, alpha_a = alpha_a, beta_a = beta_a)
#run1: alpha_phi = 1, beta_phi = 0.125, a_omega = 0.5, b_omega = 0.5
#alpha_P = 1, beta_P = 0.5, alpha_a = 1, beta_a = 0.2, iter = 20000
#run2: alpha_phi = 1, beta_phi = 0.5, a_omega = 0.5, b_omega = 0.5
#alpha_P = 1, beta_P = 0.5, alpha_a = 1, beta_a = 0.2, iter = 100000

NBOR1609_01PU_initMay3_run2 <- stan(file = 'Pb210_draft.stan', data = NBOR1609_01PU_dat, chains = 4, 
                               iter = 200000, control = list(adapt_delta = 0.99))
saveRDS(NBOR1609_01PU_initMay3_run2, "NBOR1609_01PU_initMay3_run2.rds")

#NBOR1609_03PU
NBOR1609_03PU <- peck_cores %>% 
  filter(core_id == "NBOR1609_03PU", depth_max <= 44) %>%
  select(depth_max, dry_bulk_density_modeled, total_pb210_activity, total_pb210_activity_se,
         pb214_activity, pb214_activity_se, excess_pb210_activity, excess_pb210_activity_se)

N <- 22
M <- 23
L <- 22
delta <- 2
x <- NBOR1609_03PU$depth_max
xL <- NBOR1609_03PU$depth_max
rho <- NBOR1609_03PU$dry_bulk_density_modeled*gcm3_gcm2
rhoL <- NBOR1609_03PU$dry_bulk_density_modeled*gcm3_gcm2
sigma <- NBOR1609_03PU$total_pb210_activity_se
sigmaL <- NBOR1609_03PU$pb214_activity_se
p <- NBOR1609_03PU$total_pb210_activity
pL <- NBOR1609_03PU$pb214_activity
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

NBOR1609_03PU_dat <- list(N = N, L = L, x = x, xL = xL, delta = delta, M=M,
                          rho = rho, rhoL = rhoL, sigma = sigma, sigmaL = sigmaL, 
                          p = p, pL = pL, lambda = lambda,  alpha_phi = alpha_phi, 
                          beta_phi = beta_phi, a_omega = a_omega, b_omega = b_omega, alpha_P = alpha_P, 
                          beta_P = beta_P, alpha_a = alpha_a, beta_a = beta_a)
#run1: alpha_phi = 1, beta_phi = 0.125, a_omega = 0.5, b_omega = 0.5
#alpha_P = 1, beta_P = 0.5, alpha_a = 1, beta_a = 0.2, iter = 20000
#run2: alpha_phi = 1, beta_phi = 0.5, a_omega = 0.5, b_omega = 0.5
#alpha_P = 1, beta_P = 0.5, alpha_a = 1, beta_a = 0.2, iter = 100000

NBOR1609_03PU_initMay3 <- stan(file = 'Pb210_draft.stan', data = NBOR1609_03PU_dat, chains = 4, 
                               iter = 200000, control = list(adapt_delta = 0.99))
saveRDS(NBOR1609_03PU_initMay3, "NBOR1609_03PU_initMay3.rds")

#saving data as csv

#NBOR1606_03PU
NBOR1606_03PU_initMay3_list <- extract(NBOR1606_03PU_initMay3)
NBOR1606_03PU_initMay3_ages <- data.frame(NBOR1606_03PU_initMay3_list$ages)
colnames(NBOR1606_03PU_initMay3_ages) <- paste0("Age", 1:ncol(NBOR1606_03PU_initMay3_ages))
NBOR1606_03PU_initMay3_accurate <- data.frame(NBOR1606_03PU_initMay3_list$m)
colnames(NBOR1606_03PU_initMay3_accurate) <- paste0("m", 1:ncol(NBOR1606_03PU_initMay3_accurate))
NBOR1606_03PU_initMay3_param <- data.frame(cbind(NBOR1606_03PU_initMay3_list$phi, 
                                               NBOR1606_03PU_initMay3_list$Ps,
                                               NBOR1606_03PU_initMay3_list$omega))
colnames(NBOR1606_03PU_initMay3_param) <- c("Supply", "SuppAct", "Memory")
write.csv(NBOR1606_03PU_initMay3_ages, "NBOR1606_03PU_initMay3_ages.csv")
write.csv(NBOR1606_03PU_initMay3_accurate, "NBOR1606_03PU_initMay3_accurate.csv")
write.csv(NBOR1606_03PU_initMay3_param, "NBOR1606_03PU_initMay3_param.csv")

#NBOR1606_04PU
NBOR1606_04PU_initMay3_list <- extract(NBOR1606_04PU_initMay3)
NBOR1606_04PU_initMay3_ages <- data.frame(NBOR1606_04PU_initMay3_list$ages)
colnames(NBOR1606_04PU_initMay3_ages) <- paste0("Age", 1:ncol(NBOR1606_04PU_initMay3_ages))
NBOR1606_04PU_initMay3_accurate <- data.frame(NBOR1606_04PU_initMay3_list$m)
colnames(NBOR1606_04PU_initMay3_accurate) <- paste0("m", 1:ncol(NBOR1606_04PU_initMay3_accurate))
NBOR1606_04PU_initMay3_param <- data.frame(cbind(NBOR1606_04PU_initMay3_list$phi, 
                                               NBOR1606_04PU_initMay3_list$Ps,
                                               NBOR1606_04PU_initMay3_list$omega))
colnames(NBOR1606_04PU_initMay3_param) <- c("Supply", "SuppAct", "Memory")
write.csv(NBOR1606_04PU_initMay3_ages, "NBOR1606_04PU_initMay3_ages.csv")
write.csv(NBOR1606_04PU_initMay3_accurate, "NBOR1606_04PU_initMay3_accurate.csv")
write.csv(NBOR1606_04PU_initMay3_param, "NBOR1606_04PU_initMay3_param.csv")

#NBOR1606_08PU RUN 2
NBOR1606_08PU_initMay3_run2_list <- extract(NBOR1606_08PU_initMay3_run2)
NBOR1606_08PU_initMay3_run2_ages <- data.frame(NBOR1606_08PU_initMay3_run2_list$ages)
colnames(NBOR1606_08PU_initMay3_run2_ages) <- paste0("Age", 1:ncol(NBOR1606_08PU_initMay3_run2_ages))
NBOR1606_08PU_initMay3_run2_accurate <- data.frame(NBOR1606_08PU_initMay3_run2_list$m)
colnames(NBOR1606_08PU_initMay3_run2_accurate) <- paste0("m", 1:ncol(NBOR1606_08PU_initMay3_run2_accurate))
NBOR1606_08PU_initMay3_run2_param <- data.frame(cbind(NBOR1606_08PU_initMay3_run2_list$phi, 
                                               NBOR1606_08PU_initMay3_run2_list$Ps,
                                               NBOR1606_08PU_initMay3_run2_list$omega))
colnames(NBOR1606_08PU_initMay3_run2_param) <- c("Supply", "SuppAct", "Memory")
write.csv(NBOR1606_08PU_initMay3_run2_ages, "NBOR1606_08PU_initMay3_run2_ages.csv")
write.csv(NBOR1606_08PU_initMay3_run2_accurate, "NBOR1606_08PU_initMay3_run2_accurate.csv")
write.csv(NBOR1606_08PU_initMay3_run2_param, "NBOR1606_08PU_initMay3_run2_param.csv")

#NBOR1609_01PU RUN 2
NBOR1609_01PU_initMay3_run2_list <- extract(NBOR1609_01PU_initMay3_run2)
NBOR1609_01PU_initMay3_run2_ages <- data.frame(NBOR1609_01PU_initMay3_run2_list$ages)
colnames(NBOR1609_01PU_initMay3_run2_ages) <- paste0("Age", 1:ncol(NBOR1609_01PU_initMay3_run2_ages))
NBOR1609_01PU_initMay3_run2_accurate <- data.frame(NBOR1609_01PU_initMay3_run2_list$m)
colnames(NBOR1609_01PU_initMay3_run2_accurate) <- paste0("m", 1:ncol(NBOR1609_01PU_initMay3_run2_accurate))
NBOR1609_01PU_initMay3_run2_param <- data.frame(cbind(NBOR1609_01PU_initMay3_run2_list$phi, 
                                               NBOR1609_01PU_initMay3_run2_list$Ps,
                                               NBOR1609_01PU_initMay3_run2_list$omega))
colnames(NBOR1609_01PU_initMay3_run2_param) <- c("Supply", "SuppAct", "Memory")
write.csv(NBOR1609_01PU_initMay3_run2_ages, "NBOR1609_01PU_initMay3_run2_ages.csv")
write.csv(NBOR1609_01PU_initMay3_run2_accurate, "NBOR1609_01PU_initMay3_run2_accurate.csv")
write.csv(NBOR1609_01PU_initMay3_run2_param, "NBOR1609_01PU_initMay3_run2_param.csv")

#NBOR1609_03PU
NBOR1609_03PU_initMay3_list <- extract(NBOR1609_03PU_initMay3)
NBOR1609_03PU_initMay3_ages <- data.frame(NBOR1609_03PU_initMay3_list$ages)
colnames(NBOR1609_03PU_initMay3_ages) <- paste0("Age", 1:ncol(NBOR1609_03PU_initMay3_ages))
NBOR1609_03PU_initMay3_accurate <- data.frame(NBOR1609_03PU_initMay3_list$m)
colnames(NBOR1609_03PU_initMay3_accurate) <- paste0("m", 1:ncol(NBOR1609_03PU_initMay3_accurate))
NBOR1609_03PU_initMay3_param <- data.frame(cbind(NBOR1609_03PU_initMay3_list$phi, 
                                               NBOR1609_03PU_initMay3_list$Ps,
                                               NBOR1609_03PU_initMay3_list$omega))
colnames(NBOR1609_03PU_initMay3_param) <- c("Supply", "SuppAct", "Memory")
write.csv(NBOR1609_03PU_initMay3_ages, "NBOR1609_03PU_initMay3_ages.csv")
write.csv(NBOR1609_03PU_initMay3_accurate, "NBOR1609_03PU_initMay3_accurate.csv")
write.csv(NBOR1609_03PU_initMay3_param, "NBOR1609_03PU_initMay3_param.csv")


#NBOR1606_03PU
NBOR1606_03PU_initMay3_ages_summ <- t(apply(X = NBOR1606_03PU_initMay3_ages[,2:ncol(NBOR1606_03PU_initMay3_ages)], 
                                          MARGIN = 2, 
                                          FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(NBOR1606_03PU_initMay3_ages_summ)[1:2] <- c("mean", "sd")

NBOR1606_03PU_initMay3_accurate_summ <- t(apply(X = NBOR1606_03PU_initMay3_accurate[,2:ncol(NBOR1606_03PU_initMay3_accurate)], 
                                              MARGIN = 2, 
                                              FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(NBOR1606_03PU_initMay3_accurate_summ)[1:2] <- c("mean", "sd")
write.csv(NBOR1606_03PU_initMay3_ages_summ, "NBOR1606_03PU_initMay3_ages_summ.csv", row.names = F)
write.csv(NBOR1606_03PU_initMay3_accurate_summ, "NBOR1606_03PU_initMay3_accurate_summ.csv", row.names = F)
 
#NBOR1606_04PU
NBOR1606_04PU_initMay3_ages_summ <- t(apply(X = NBOR1606_04PU_initMay3_ages[,2:ncol(NBOR1606_04PU_initMay3_ages)], 
                                          MARGIN = 2, 
                                          FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(NBOR1606_04PU_initMay3_ages_summ)[1:2] <- c("mean", "sd")

NBOR1606_04PU_initMay3_accurate_summ <- t(apply(X = NBOR1606_04PU_initMay3_accurate[,2:ncol(NBOR1606_04PU_initMay3_accurate)], 
                                              MARGIN = 2, 
                                              FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(NBOR1606_04PU_initMay3_accurate_summ)[1:2] <- c("mean", "sd")
write.csv(NBOR1606_04PU_initMay3_ages_summ, "NBOR1606_04PU_initMay3_ages_summ.csv", row.names = F)
write.csv(NBOR1606_04PU_initMay3_accurate_summ, "NBOR1606_04PU_initMay3_accurate_summ.csv", row.names = F)

#NBOR1606_08PU
NBOR1606_08PU_initMay3_run2_ages_summ <- t(apply(X = NBOR1606_08PU_initMay3_run2_ages[,2:ncol(NBOR1606_08PU_initMay3_run2_ages)], 
                                          MARGIN = 2, 
                                          FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(NBOR1606_08PU_initMay3_run2_ages_summ)[1:2] <- c("mean", "sd")

NBOR1606_08PU_initMay3_run2_accurate_summ <- t(apply(X = NBOR1606_08PU_initMay3_run2_accurate[,2:ncol(NBOR1606_08PU_initMay3_run2_accurate)], 
                                              MARGIN = 2, 
                                              FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(NBOR1606_08PU_initMay3_run2_accurate_summ)[1:2] <- c("mean", "sd")
write.csv(NBOR1606_08PU_initMay3_run2_ages_summ, "NBOR1606_08PU_initMay3_run2_ages_summ.csv", row.names = F)
write.csv(NBOR1606_08PU_initMay3_run2_accurate_summ, "NBOR1606_08PU_initMay3_run2_accurate_summ.csv", row.names = F)

#NBOR1609_01PU
NBOR1609_01PU_initMay3_run2_ages_summ <- t(apply(X = NBOR1609_01PU_initMay3_run2_ages[,2:ncol(NBOR1609_01PU_initMay3_run2_ages)], 
                                          MARGIN = 2, 
                                          FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(NBOR1609_01PU_initMay3_run2_ages_summ)[1:2] <- c("mean", "sd")

NBOR1609_01PU_initMay3_run2_accurate_summ <- t(apply(X = NBOR1609_01PU_initMay3_run2_accurate[,2:ncol(NBOR1609_01PU_initMay3_run2_accurate)], 
                                              MARGIN = 2, 
                                              FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(NBOR1609_01PU_initMay3_run2_accurate_summ)[1:2] <- c("mean", "sd")
write.csv(NBOR1609_01PU_initMay3_run2_ages_summ, "NBOR1609_01PU_initMay3_run2_ages_summ.csv", row.names = F)
write.csv(NBOR1609_01PU_initMay3_run2_accurate_summ, "NBOR1609_01PU_initMay3_run2_accurate_summ.csv", row.names = F)

#NBOR1609_03PU
NBOR1609_03PU_initMay3_ages_summ <- t(apply(X = NBOR1609_03PU_initMay3_ages[,2:ncol(NBOR1609_03PU_initMay3_ages)], 
                                          MARGIN = 2, 
                                          FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(NBOR1609_03PU_initMay3_ages_summ)[1:2] <- c("mean", "sd")

NBOR1609_03PU_initMay3_accurate_summ <- t(apply(X = NBOR1609_03PU_initMay3_accurate[,2:ncol(NBOR1609_03PU_initMay3_accurate)], 
                                              MARGIN = 2, 
                                              FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(NBOR1609_03PU_initMay3_accurate_summ)[1:2] <- c("mean", "sd")
write.csv(NBOR1609_03PU_initMay3_ages_summ, "NBOR1609_03PU_initMay3_ages_summ.csv", row.names = F)
write.csv(NBOR1609_03PU_initMay3_accurate_summ, "NBOR1609_03PU_initMay3_accurate_summ.csv", row.names = F)



