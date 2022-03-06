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

#ABOR1702_02PU
NBOR1609_02PU <- peck_cores %>% 
  filter(core_id == "NBOR1609_02PU", depth_max <= 26) %>%
  select(depth_max, dry_bulk_density_modeled, total_pb210_activity, total_pb210_activity_se,
         pb214_activity, pb214_activity_se, excess_pb210_activity, excess_pb210_activity_se)	
N <- 13
M <- 14
L <- 13
delta <- 2
x <- NBOR1609_02PU$depth_max
xL <- NBOR1609_02PU$depth_max
rho <- NBOR1609_02PU$dry_bulk_density_modeled*gcm3_gcm2
rhoL <- NBOR1609_02PU$dry_bulk_density_modeled*gcm3_gcm2
sigma <- NBOR1609_02PU$total_pb210_activity_se
sigmaL <- NBOR1609_02PU$pb214_activity_se
p <- NBOR1609_02PU$total_pb210_activity
pL <- NBOR1609_02PU$pb214_activity
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
beta_a <- 0.29
#plot(0, 0, xlim = c(0, 50), ylim = c(0, 0.4), type = "n")
#curve(dgamma(x, shape = 1, rate = 0.2), from = 0, to = 50, add = TRUE)

NBOR1609_02PU_dat <- list(N = N, L = L, x = x, xL = xL, delta = delta, M=M,
                          rho = rho, rhoL = rhoL, sigma = sigma, sigmaL = sigmaL, 
                          p = p, pL = pL, lambda = lambda,  alpha_phi = alpha_phi, 
                          beta_phi = beta_phi, a_omega = a_omega, b_omega = b_omega, alpha_P = alpha_P, 
                          beta_P = beta_P, alpha_a = alpha_a, beta_a = beta_a)

NBOR1609_02_initJune3 <- stan(file = 'Pb210_draft.stan', data = NBOR1609_02PU_dat, 
                              chains = 4, iter = 200000, control = list(adapt_delta = 0.99))
saveRDS(NBOR1609_02_initJune3, "NBOR1609_02_initJune3.rds")

#save to .csv
NBOR1609_02_initJune3_list <- extract(NBOR1609_02_initJune3)
NBOR1609_02_initJune3_ages <- data.frame(NBOR1609_02_initJune3_list$ages)
colnames(NBOR1609_02_initJune3_ages) <- paste0("Age", 1:ncol(NBOR1609_02_initJune3_ages))
NBOR1609_02_initJune3_accurate <- data.frame(NBOR1609_02_initJune3_list$m)
colnames(NBOR1609_02_initJune3_accurate) <- paste0("m", 1:ncol(NBOR1609_02_initJune3_accurate))
NBOR1609_02_initJune3_param <- data.frame(cbind(NBOR1609_02_initJune3_list$phi, 
                                                NBOR1609_02_initJune3_list$Ps,
                                                NBOR1609_02_initJune3_list$omega))
colnames(NBOR1609_02_initJune3_param) <- c("Supply", "SuppAct", "Memory")
write.csv(NBOR1609_02_initJune3_ages, "NBOR1609_02_initJune3_ages.csv")
write.csv(NBOR1609_02_initJune3_accurate, "NBOR1609_02_initJune3_accurate.csv")
write.csv(NBOR1609_02_initJune3_param, "NBOR1609_02_initJune3_param.csv")

#save summaries to .csv
NBOR1609_02_initJune3_ages_summ <- t(apply(X = NBOR1609_02_initJune3_ages[,1:ncol(NBOR1609_02_initJune3_ages)], 
                                           MARGIN = 2, 
                                           FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(NBOR1609_02_initJune3_ages_summ)[1:2] <- c("mean", "sd")

NBOR1609_02_initJune3_accurate_summ <- t(apply(X = NBOR1609_02_initJune3_accurate[,1:ncol(NBOR1609_02_initJune3_accurate)], 
                                               MARGIN = 2, 
                                               FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(NBOR1609_02_initJune3_accurate_summ)[1:2] <- c("mean", "sd")
write.csv(NBOR1609_02_initJune3_ages_summ, "NBOR1609_02_initJune3_ages_summ.csv", row.names = F)
write.csv(NBOR1609_02_initJune3_accurate_summ, "NBOR1609_02_initJune3_accurate_summ.csv", row.names = F)

#nonconstant supported activity
#NBOR1609_02PU
NBOR1609_02PU <- peck_cores %>% 
  filter(core_id == "NBOR1609_02PU", depth_max <= 26) %>%
  select(depth_max, dry_bulk_density_modeled, total_pb210_activity, total_pb210_activity_se,
         pb214_activity, pb214_activity_se, excess_pb210_activity, excess_pb210_activity_se)	
N <- 13
M <- 14
L <- 13
delta <- 2
x <- NBOR1609_02PU$depth_max
xL <- NBOR1609_02PU$depth_max
rho <- NBOR1609_02PU$dry_bulk_density_modeled*gcm3_gcm2
rhoL <- NBOR1609_02PU$dry_bulk_density_modeled*gcm3_gcm2
sigma <- NBOR1609_02PU$total_pb210_activity_se
sigmaL <- NBOR1609_02PU$pb214_activity_se
p <- NBOR1609_02PU$total_pb210_activity
pL <- NBOR1609_02PU$pb214_activity
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
beta_a <- 0.29
#plot(0, 0, xlim = c(0, 50), ylim = c(0, 0.4), type = "n")
#curve(dgamma(x, shape = 1, rate = 0.2), from = 0, to = 50, add = TRUE)

NBOR1609_02PU_dat <- list(N = N, L = L, x = x, xL = xL, delta = delta, M=M,
                          rho = rho, rhoL = rhoL, sigma = sigma, sigmaL = sigmaL, 
                          p = p, pL = pL, lambda = lambda,  alpha_phi = alpha_phi, 
                          beta_phi = beta_phi, a_omega = a_omega, b_omega = b_omega, alpha_P = alpha_P, 
                          beta_P = beta_P, alpha_a = alpha_a, beta_a = beta_a)

NBOR1609_02_initJune3nonconst <- stan(file = 'Pb210_nonconst_supp.stan', data = NBOR1609_02PU_dat, 
                                      chains = 4, iter = 200000, control = list(adapt_delta = 0.99))
saveRDS(NBOR1609_02_initJune3nonconst, "NBOR1609_02_initJune3nonconst.rds")

#save to .csv
NBOR1609_02_initJune3nonconst_list <- extract(NBOR1609_02_initJune3nonconst)
NBOR1609_02_initJune3nonconst_ages <- data.frame(NBOR1609_02_initJune3nonconst_list$ages)
colnames(NBOR1609_02_initJune3nonconst_ages) <- paste0("Age", 1:ncol(NBOR1609_02_initJune3nonconst_ages))
NBOR1609_02_initJune3nonconst_accurate <- data.frame(NBOR1609_02_initJune3nonconst_list$m)
colnames(NBOR1609_02_initJune3nonconst_accurate) <- paste0("m", 1:ncol(NBOR1609_02_initJune3nonconst_accurate))
NBOR1609_02_initJune3nonconst_param <- data.frame(cbind(NBOR1609_02_initJune3nonconst_list$phi, 
                                                        NBOR1609_02_initJune3nonconst_list$Ps,
                                                        NBOR1609_02_initJune3nonconst_list$omega))
colnames(NBOR1609_02_initJune3nonconst_param) <- c("Supply", "SuppAct", "Memory")
write.csv(NBOR1609_02_initJune3nonconst_ages, "NBOR1609_02_initJune3nonconst_ages.csv")
write.csv(NBOR1609_02_initJune3nonconst_accurate, "NBOR1609_02_initJune3nonconst_accurate.csv")
write.csv(NBOR1609_02_initJune3nonconst_param, "NBOR1609_02_initJune3nonconst_param.csv")

#save summaries to .csv
NBOR1609_02_initJune3nonconst_ages_summ <- t(apply(X = NBOR1609_02_initJune3nonconst_ages[,1:ncol(NBOR1609_02_initJune3nonconst_ages)], 
                                                   MARGIN = 2, 
                                                   FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(NBOR1609_02_initJune3nonconst_ages_summ)[1:2] <- c("mean", "sd")

NBOR1609_02_initJune3nonconst_accurate_summ <- t(apply(X = NBOR1609_02_initJune3nonconst_accurate[,1:ncol(NBOR1609_02_initJune3nonconst_accurate)], 
                                                       MARGIN = 2, 
                                                       FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(NBOR1609_02_initJune3nonconst_accurate_summ)[1:2] <- c("mean", "sd")
write.csv(NBOR1609_02_initJune3nonconst_ages_summ, "NBOR1609_02_initJune3nonconst_ages_summ.csv", row.names = F)
write.csv(NBOR1609_02_initJune3nonconst_accurate_summ, "NBOR1609_02_initJune3nonconst_accurate_summ.csv", row.names = F)

