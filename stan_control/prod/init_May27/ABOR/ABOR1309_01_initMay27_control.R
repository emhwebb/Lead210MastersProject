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

#ABOR1309_01PU
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
beta_a <- 0.2
#plot(0, 0, xlim = c(0, 50), ylim = c(0, 0.4), type = "n")
#curve(dgamma(x, shape = 1, rate = 0.2), from = 0, to = 50, add = TRUE)

ABOR1309_01PU_dat <- list(N = N, L = L, x = x, xL = xL, delta = delta, M=M,
                          rho = rho, rhoL = rhoL, sigma = sigma, sigmaL = sigmaL, 
                          p = p, pL = pL, lambda = lambda,  alpha_phi = alpha_phi, 
                          beta_phi = beta_phi, a_omega = a_omega, b_omega = b_omega, alpha_P = alpha_P, 
                          beta_P = beta_P, alpha_a = alpha_a, beta_a = beta_a)

ABOR1309_01_initMay27 <- stan(file = 'Pb210_draft.stan', data = ABOR1309_01PU_dat, 
                             chains = 4, iter = 200000, control = list(adapt_delta = 0.99))
saveRDS(ABOR1309_01_initMay27, "ABOR1309_01_initMay27.rds")

#save to .csv
ABOR1309_01_initMay27_list <- extract(ABOR1309_01_initMay27)
ABOR1309_01_initMay27_ages <- data.frame(ABOR1309_01_initMay27_list$ages)
colnames(ABOR1309_01_initMay27_ages) <- paste0("Age", 1:ncol(ABOR1309_01_initMay27_ages))
ABOR1309_01_initMay27_accurate <- data.frame(ABOR1309_01_initMay27_list$m)
colnames(ABOR1309_01_initMay27_accurate) <- paste0("m", 1:ncol(ABOR1309_01_initMay27_accurate))
ABOR1309_01_initMay27_param <- data.frame(cbind(ABOR1309_01_initMay27_list$phi, 
                                               ABOR1309_01_initMay27_list$Ps,
                                               ABOR1309_01_initMay27_list$omega))
colnames(ABOR1309_01_initMay27_param) <- c("Supply", "SuppAct", "Memory")
write.csv(ABOR1309_01_initMay27_ages, "ABOR1309_01_initMay27_ages.csv")
write.csv(ABOR1309_01_initMay27_accurate, "ABOR1309_01_initMay27_accurate.csv")
write.csv(ABOR1309_01_initMay27_param, "ABOR1309_01_initMay27_param.csv")

#save summaries to .csv
ABOR1309_01_initMay27_ages_summ <- t(apply(X = ABOR1309_01_initMay27_ages[,1:ncol(ABOR1309_01_initMay27_ages)], 
                                          MARGIN = 2, 
                                          FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(ABOR1309_01_initMay27_ages_summ)[1:2] <- c("mean", "sd")

ABOR1309_01_initMay27_accurate_summ <- t(apply(X = ABOR1309_01_initMay27_accurate[,1:ncol(ABOR1309_01_initMay27_accurate)], 
                                              MARGIN = 2, 
                                              FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(ABOR1309_01_initMay27_accurate_summ)[1:2] <- c("mean", "sd")
write.csv(ABOR1309_01_initMay27_ages_summ, "ABOR1309_01_initMay27_ages_summ.csv", row.names = F)
write.csv(ABOR1309_01_initMay27_accurate_summ, "ABOR1309_01_initMay27_accurate_summ.csv", row.names = F)

#nonconstant supported activity
#ABOR1309_01PU
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
beta_a <- 0.2
#plot(0, 0, xlim = c(0, 50), ylim = c(0, 0.4), type = "n")
#curve(dgamma(x, shape = 1, rate = 0.2), from = 0, to = 50, add = TRUE)

ABOR1309_01PU_dat <- list(N = N, L = L, x = x, xL = xL, delta = delta, M=M,
                          rho = rho, rhoL = rhoL, sigma = sigma, sigmaL = sigmaL, 
                          p = p, pL = pL, lambda = lambda,  alpha_phi = alpha_phi, 
                          beta_phi = beta_phi, a_omega = a_omega, b_omega = b_omega, alpha_P = alpha_P, 
                          beta_P = beta_P, alpha_a = alpha_a, beta_a = beta_a)

ABOR1309_01_initMay27nonconst <- stan(file = 'Pb210_nonconst_supp.stan', data = ABOR1309_01PU_dat, 
                             chains = 4, iter = 200000, control = list(adapt_delta = 0.99))
saveRDS(ABOR1309_01_initMay27nonconst, "ABOR1309_01_initMay27nonconst.rds")

#save to .csv
ABOR1309_01_initMay27nonconst_list <- extract(ABOR1309_01_initMay27nonconst)
ABOR1309_01_initMay27nonconst_ages <- data.frame(ABOR1309_01_initMay27nonconst_list$ages)
colnames(ABOR1309_01_initMay27nonconst_ages) <- paste0("Age", 1:ncol(ABOR1309_01_initMay27nonconst_ages))
ABOR1309_01_initMay27nonconst_accurate <- data.frame(ABOR1309_01_initMay27nonconst_list$m)
colnames(ABOR1309_01_initMay27nonconst_accurate) <- paste0("m", 1:ncol(ABOR1309_01_initMay27nonconst_accurate))
ABOR1309_01_initMay27nonconst_param <- data.frame(cbind(ABOR1309_01_initMay27nonconst_list$phi, 
                                               ABOR1309_01_initMay27nonconst_list$Ps,
                                               ABOR1309_01_initMay27nonconst_list$omega))
colnames(ABOR1309_01_initMay27nonconst_param) <- c("Supply", "SuppAct", "Memory")
write.csv(ABOR1309_01_initMay27nonconst_ages, "ABOR1309_01_initMay27nonconst_ages.csv")
write.csv(ABOR1309_01_initMay27nonconst_accurate, "ABOR1309_01_initMay27nonconst_accurate.csv")
write.csv(ABOR1309_01_initMay27nonconst_param, "ABOR1309_01_initMay27nonconst_param.csv")

#save summaries to .csv
ABOR1309_01_initMay27nonconst_ages_summ <- t(apply(X = ABOR1309_01_initMay27nonconst_ages[,1:ncol(ABOR1309_01_initMay27nonconst_ages)], 
                                          MARGIN = 2, 
                                          FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(ABOR1309_01_initMay27nonconst_ages_summ)[1:2] <- c("mean", "sd")

ABOR1309_01_initMay27nonconst_accurate_summ <- t(apply(X = ABOR1309_01_initMay27nonconst_accurate[,1:ncol(ABOR1309_01_initMay27nonconst_accurate)], 
                                              MARGIN = 2, 
                                              FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(ABOR1309_01_initMay27nonconst_accurate_summ)[1:2] <- c("mean", "sd")
write.csv(ABOR1309_01_initMay27nonconst_ages_summ, "ABOR1309_01_initMay27nonconst_ages_summ.csv", row.names = F)
write.csv(ABOR1309_01_initMay27nonconst_accurate_summ, "ABOR1309_01_initMay27nonconst_accurate_summ.csv", row.names = F)

