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


ABOR1702_07PU <- peck_cores %>% 
  filter(core_id == "ABOR1702_07PU", depth_max <= 30) %>%
  select(depth_max, dry_bulk_density_modeled, total_pb210_activity, total_pb210_activity_se,
         pb214_activity, pb214_activity_se, excess_pb210_activity, excess_pb210_activity_se)	
N <- 15
M <- 16
L <- 15
delta <- 2
x <- ABOR1702_07PU$depth_max
xL <- ABOR1702_07PU$depth_max
rho <- ABOR1702_07PU$dry_bulk_density_modeled*gcm3_gcm2
rhoL <- ABOR1702_07PU$dry_bulk_density_modeled*gcm3_gcm2
sigma <- ABOR1702_07PU$total_pb210_activity_se
sigmaL <- ABOR1702_07PU$pb214_activity_se
p <- ABOR1702_07PU$total_pb210_activity
pL <- ABOR1702_07PU$pb214_activity
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
beta_a <- 0.19
#plot(0, 0, xlim = c(0, 50), ylim = c(0, 0.4), type = "n")
#curve(dgamma(x, shape = 1, rate = 0.2), from = 0, to = 50, add = TRUE)

ABOR1702_07PU_dat <- list(N = N, L = L, x = x, xL = xL, delta = delta, M=M,
                          rho = rho, rhoL = rhoL, sigma = sigma, sigmaL = sigmaL, 
                          p = p, pL = pL, lambda = lambda,  alpha_phi = alpha_phi, 
                          beta_phi = beta_phi, a_omega = a_omega, b_omega = b_omega, alpha_P = alpha_P, 
                          beta_P = beta_P, alpha_a = alpha_a, beta_a = beta_a)

ABOR1702_07_initMay27 <- stan(file = 'Pb210_draft.stan', data = ABOR1702_07PU_dat, 
                             chains = 4, iter = 200000, control = list(adapt_delta = 0.99))
saveRDS(ABOR1702_07_initMay27, "ABOR1702_07_initMay27.rds")

#saving simulation results to .csv
ABOR1702_07_initMay27_list <- extract(ABOR1702_07_initMay27)
ABOR1702_07_initMay27_ages <- data.frame(ABOR1702_07_initMay27_list$ages)
colnames(ABOR1702_07_initMay27_ages) <- paste0("Age", 1:ncol(ABOR1702_07_initMay27_ages))
ABOR1702_07_initMay27_accurate <- data.frame(ABOR1702_07_initMay27_list$m)
colnames(ABOR1702_07_initMay27_accurate) <- paste0("m", 1:ncol(ABOR1702_07_initMay27_accurate))
ABOR1702_07_initMay27_param <- data.frame(cbind(ABOR1702_07_initMay27_list$phi, 
                                               ABOR1702_07_initMay27_list$Ps,
                                               ABOR1702_07_initMay27_list$omega))
colnames(ABOR1702_07_initMay27_param) <- c("Supply", "SuppAct", "Memory")
write.csv(ABOR1702_07_initMay27_ages, "ABOR1702_07_initMay27_ages.csv")
write.csv(ABOR1702_07_initMay27_accurate, "ABOR1702_07_initMay27_accurate.csv")
write.csv(ABOR1702_07_initMay27_param, "ABOR1702_07_initMay27_param.csv")

#saving summaries to .csv
ABOR1702_07_initMay27_ages_summ <- t(apply(X = ABOR1702_07_initMay27_ages[,1:ncol(ABOR1702_07_initMay27_ages)], 
                                          MARGIN = 2, 
                                          FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(ABOR1702_07_initMay27_ages_summ)[1:2] <- c("mean", "sd")

ABOR1702_07_initMay27_accurate_summ <- t(apply(X = ABOR1702_07_initMay27_accurate[,1:ncol(ABOR1702_07_initMay27_accurate)], 
                                              MARGIN = 2, 
                                              FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(ABOR1702_07_initMay27_accurate_summ)[1:2] <- c("mean", "sd")
write.csv(ABOR1702_07_initMay27_ages_summ, "ABOR1702_07_initMay27_ages_summ.csv", row.names = F)
write.csv(ABOR1702_07_initMay27_accurate_summ, "ABOR1702_07_initMay27_accurate_summ.csv", row.names = F)

#trying with nonconst supported activity
ABOR1702_07PU <- peck_cores %>% 
  filter(core_id == "ABOR1702_07PU", depth_max <= 30) %>%
  select(depth_max, dry_bulk_density_modeled, total_pb210_activity, total_pb210_activity_se,
         pb214_activity, pb214_activity_se, excess_pb210_activity, excess_pb210_activity_se)	
N <- 15
M <- 16
L <- 15
delta <- 2
x <- ABOR1702_07PU$depth_max
xL <- ABOR1702_07PU$depth_max
rho <- ABOR1702_07PU$dry_bulk_density_modeled*gcm3_gcm2
rhoL <- ABOR1702_07PU$dry_bulk_density_modeled*gcm3_gcm2
sigma <- ABOR1702_07PU$total_pb210_activity_se
sigmaL <- ABOR1702_07PU$pb214_activity_se
p <- ABOR1702_07PU$total_pb210_activity
pL <- ABOR1702_07PU$pb214_activity
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
beta_a <- 0.19
#plot(0, 0, xlim = c(0, 50), ylim = c(0, 0.4), type = "n")
#curve(dgamma(x, shape = 1, rate = 0.2), from = 0, to = 50, add = TRUE)

ABOR1702_07PU_dat <- list(N = N, L = L, x = x, xL = xL, delta = delta, M=M,
                          rho = rho, rhoL = rhoL, sigma = sigma, sigmaL = sigmaL, 
                          p = p, pL = pL, lambda = lambda,  alpha_phi = alpha_phi, 
                          beta_phi = beta_phi, a_omega = a_omega, b_omega = b_omega, alpha_P = alpha_P, 
                          beta_P = beta_P, alpha_a = alpha_a, beta_a = beta_a)

ABOR1702_07_initMay27nonconst <- stan(file = 'Pb210_nonconst_supp.stan', data = ABOR1702_07PU_dat, 
                             chains = 4, iter = 200000, control = list(adapt_delta = 0.99))
saveRDS(ABOR1702_07_initMay27nonconst, "ABOR1702_07_initMay27nonconst.rds")

#saving simulation results to .csv
ABOR1702_07_initMay27nonconst_list <- extract(ABOR1702_07_initMay27nonconst)
ABOR1702_07_initMay27nonconst_ages <- data.frame(ABOR1702_07_initMay27nonconst_list$ages)
colnames(ABOR1702_07_initMay27nonconst_ages) <- paste0("Age", 1:ncol(ABOR1702_07_initMay27nonconst_ages))
ABOR1702_07_initMay27nonconst_accurate <- data.frame(ABOR1702_07_initMay27nonconst_list$m)
colnames(ABOR1702_07_initMay27nonconst_accurate) <- paste0("m", 1:ncol(ABOR1702_07_initMay27nonconst_accurate))
ABOR1702_07_initMay27nonconst_param <- data.frame(cbind(ABOR1702_07_initMay27nonconst_list$phi, 
                                               ABOR1702_07_initMay27nonconst_list$Ps,
                                               ABOR1702_07_initMay27nonconst_list$omega))
colnames(ABOR1702_07_initMay27nonconst_param) <- c("Supply", "SuppAct", "Memory")
write.csv(ABOR1702_07_initMay27nonconst_ages, "ABOR1702_07_initMay27nonconst_ages.csv")
write.csv(ABOR1702_07_initMay27nonconst_accurate, "ABOR1702_07_initMay27nonconst_accurate.csv")
write.csv(ABOR1702_07_initMay27nonconst_param, "ABOR1702_07_initMay27nonconst_param.csv")

#saving summaries to .csv
ABOR1702_07_initMay27nonconst_ages_summ <- t(apply(X = ABOR1702_07_initMay27nonconst_ages[,1:ncol(ABOR1702_07_initMay27nonconst_ages)], 
                                          MARGIN = 2, 
                                          FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(ABOR1702_07_initMay27nonconst_ages_summ)[1:2] <- c("mean", "sd")

ABOR1702_07_initMay27nonconst_accurate_summ <- t(apply(X = ABOR1702_07_initMay27nonconst_accurate[,1:ncol(ABOR1702_07_initMay27nonconst_accurate)], 
                                              MARGIN = 2, 
                                              FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(ABOR1702_07_initMay27nonconst_accurate_summ)[1:2] <- c("mean", "sd")
write.csv(ABOR1702_07_initMay27nonconst_ages_summ, "ABOR1702_07_initMay27nonconst_ages_summ.csv", row.names = F)
write.csv(ABOR1702_07_initMay27nonconst_accurate_summ, "ABOR1702_07_initMay27nonconst_accurate_summ.csv", row.names = F)

