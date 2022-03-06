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

ABOR1309_01_initMay3 <- stan(file = 'Pb210_draft.stan', data = ABOR1309_01PU_dat, 
                          chains = 4, iter = 200000, control = list(adapt_delta = 0.99))
saveRDS(ABOR1309_01_initMay3, "ABOR1309_01_initMay3.rds")

#ABOR1309_03PU
#filter out less than equal to 22 instead
ABOR1309_03PU <- peck_cores %>% 
  filter(core_id == "ABOR1309_03PU", depth_max <= 22) %>%
  select(depth_max, dry_bulk_density_modeled, total_pb210_activity, total_pb210_activity_se,
         pb214_activity, pb214_activity_se, excess_pb210_activity, excess_pb210_activity_se)	
N <- 11
M <- 12
L <- 11
delta <- 2
x <- ABOR1309_03PU$depth_max
xL <- ABOR1309_03PU$depth_max
rho <- ABOR1309_03PU$dry_bulk_density_modeled*gcm3_gcm2
rhoL <- ABOR1309_03PU$dry_bulk_density_modeled*gcm3_gcm2
sigma <- ABOR1309_03PU$total_pb210_activity_se
sigmaL <- ABOR1309_03PU$pb214_activity_se
p <- ABOR1309_03PU$total_pb210_activity
pL <- ABOR1309_03PU$pb214_activity
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

#run2 is with correct number filtered out, run1 had too many 0's. 
ABOR1309_03PU_run2_dat <- list(N = N, L = L, x = x, xL = xL, delta = delta, M=M,
                          rho = rho, rhoL = rhoL, sigma = sigma, sigmaL = sigmaL, 
                          p = p, pL = pL, lambda = lambda,  alpha_phi = alpha_phi, 
                          beta_phi = beta_phi, a_omega = a_omega, b_omega = b_omega, alpha_P = alpha_P, 
                          beta_P = beta_P, alpha_a = alpha_a, beta_a = beta_a)

ABOR1309_03_run2_initMay14 <- stan(file = 'Pb210_draft.stan', data = ABOR1309_03PU_run2_dat, 
                             chains = 4, iter = 200000, control = list(adapt_delta = 0.99))
saveRDS(ABOR1309_03_run2_initMay14, "ABOR1309_03_run2_initMay14.rds")

#ABOR1309_04PU
ABOR1309_04PU <- peck_cores %>% 
  filter(core_id == "ABOR1309_04PU", depth_max <= 44) %>%
  select(depth_max, dry_bulk_density_modeled, total_pb210_activity, total_pb210_activity_se,
         pb214_activity, pb214_activity_se, excess_pb210_activity, excess_pb210_activity_se)	
N <- 22
M <- 23
L <- 22
delta <- 2
x <- ABOR1309_04PU$depth_max
xL <- ABOR1309_04PU$depth_max
rho <- ABOR1309_04PU$dry_bulk_density_modeled*gcm3_gcm2
rhoL <- ABOR1309_04PU$dry_bulk_density_modeled*gcm3_gcm2
sigma <- ABOR1309_04PU$total_pb210_activity_se
sigmaL <- ABOR1309_04PU$pb214_activity_se
p <- ABOR1309_04PU$total_pb210_activity
pL <- ABOR1309_04PU$pb214_activity
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

ABOR1309_04PU_dat <- list(N = N, L = L, x = x, xL = xL, delta = delta, M=M,
                          rho = rho, rhoL = rhoL, sigma = sigma, sigmaL = sigmaL, 
                          p = p, pL = pL, lambda = lambda,  alpha_phi = alpha_phi, 
                          beta_phi = beta_phi, a_omega = a_omega, b_omega = b_omega, alpha_P = alpha_P, 
                          beta_P = beta_P, alpha_a = alpha_a, beta_a = beta_a)

ABOR1309_04_initMay3 <- stan(file = 'Pb210_draft.stan', data = ABOR1309_04PU_dat, 
                             chains = 4, iter = 200000, control = list(adapt_delta = 0.99))
saveRDS(ABOR1309_04_initMay3, "ABOR1309_04_initMay3.rds")

#ABOR1702_01PU
ABOR1702_01PU <- peck_cores %>% 
  filter(core_id == "ABOR1702_01PU", depth_max <= 30) %>%
  select(depth_max, dry_bulk_density_modeled, total_pb210_activity, total_pb210_activity_se,
         pb214_activity, pb214_activity_se, excess_pb210_activity, excess_pb210_activity_se)	
N <- 15
M <- 16
L <- 15
delta <- 2
x <- ABOR1702_01PU$depth_max
xL <- ABOR1702_01PU$depth_max
rho <- ABOR1702_01PU$dry_bulk_density_modeled*gcm3_gcm2
rhoL <- ABOR1702_01PU$dry_bulk_density_modeled*gcm3_gcm2
sigma <- ABOR1702_01PU$total_pb210_activity_se
sigmaL <- ABOR1702_01PU$pb214_activity_se
p <- ABOR1702_01PU$total_pb210_activity
pL <- ABOR1702_01PU$pb214_activity
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

ABOR1702_01PU_dat <- list(N = N, L = L, x = x, xL = xL, delta = delta, M=M,
                          rho = rho, rhoL = rhoL, sigma = sigma, sigmaL = sigmaL, 
                          p = p, pL = pL, lambda = lambda,  alpha_phi = alpha_phi, 
                          beta_phi = beta_phi, a_omega = a_omega, b_omega = b_omega, alpha_P = alpha_P, 
                          beta_P = beta_P, alpha_a = alpha_a, beta_a = beta_a)

ABOR1702_01_initMay3 <- stan(file = 'Pb210_draft.stan', data = ABOR1702_01PU_dat, 
                             chains = 4, iter = 200000, control = list(adapt_delta = 0.99))
saveRDS(ABOR1702_01_initMay3, "ABOR1702_01_initMay3.rds")

#ABOR1702_07PU
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
beta_a <- 0.2
#plot(0, 0, xlim = c(0, 50), ylim = c(0, 0.4), type = "n")
#curve(dgamma(x, shape = 1, rate = 0.2), from = 0, to = 50, add = TRUE)

ABOR1702_07PU_dat <- list(N = N, L = L, x = x, xL = xL, delta = delta, M=M,
                          rho = rho, rhoL = rhoL, sigma = sigma, sigmaL = sigmaL, 
                          p = p, pL = pL, lambda = lambda,  alpha_phi = alpha_phi, 
                          beta_phi = beta_phi, a_omega = a_omega, b_omega = b_omega, alpha_P = alpha_P, 
                          beta_P = beta_P, alpha_a = alpha_a, beta_a = beta_a)

ABOR1702_07_initMay3 <- stan(file = 'Pb210_draft.stan', data = ABOR1702_07PU_dat, 
                             chains = 4, iter = 200000, control = list(adapt_delta = 0.99))
saveRDS(ABOR1702_07_initMay3, "ABOR1702_07_initMay3.rds")

#plots
#ABOR1309_01PU


#ABOR1309_03PU
#ABOR1309_04PU
#ABOR1702_01PU
#ABOR1702_07PU

plot(ABOR1309_03_run2_initMay14, pars = "ages")
plot(ABOR1309_03_run2_initMay14, pars = "m")
plot(ABOR1309_03_run2_initMay14, pars = "alpha")
plot(ABOR1309_03_run2_initMay14, pars = "omega")
plot(ABOR1309_03_run2_initMay14, plotfun = "hist", pars = "phi")
plot(ABOR1309_03_run2_initMay14, plotfun = "hist", pars = "Ps")
plot(ABOR1309_03_run2_initMay14, plotfun = "hist", pars = "omega")
plot(ABOR1309_03_run2_initMay14, plotfun = "hist", pars = "ages")
plot(ABOR1309_03_run2_initMay14, plotfun = "hist", pars = "ages", binwidth = 0.5)
plot(ABOR1309_03_run2_initMay14, plotfun = "hist", pars = "alpha")
plot(ABOR1309_03_run2_initMay14, plotfun = "hist", pars = "m")
plot(ABOR1309_03_run2_initMay14, plotfun = "hist", pars = "mN")
plot(ABOR1309_03_run2_initMay14, plotfun = "trace", pars = "omega")
plot(ABOR1309_03_run2_initMay14, plotfun = "trace", pars = "phi")
plot(ABOR1309_03_run2_initMay14, plotfun = "trace", pars = "Ps")
plot(ABOR1309_03_run2_initMay14, plotfun = "trace", pars = "ages")
plot(ABOR1309_03_run2_initMay14, plotfun = "trace", pars = "mN")
plot(ABOR1309_03_run2_initMay14, plotfun = "trace", pars = "alpha")

#ABOR1309_01PU
ABOR1309_01_initMay3_list <- extract(ABOR1309_01_initMay3)
ABOR1309_01_initMay3_ages <- data.frame(ABOR1309_01_initMay3_list$ages)
colnames(ABOR1309_01_initMay3_ages) <- paste0("Age", 1:ncol(ABOR1309_01_initMay3_ages))
ABOR1309_01_initMay3_accurate <- data.frame(ABOR1309_01_initMay3_list$m)
colnames(ABOR1309_01_initMay3_accurate) <- paste0("m", 1:ncol(ABOR1309_01_initMay3_accurate))
ABOR1309_01_initMay3_param <- data.frame(cbind(ABOR1309_01_initMay3_list$phi, 
                                    ABOR1309_01_initMay3_list$Ps,
                                    ABOR1309_01_initMay3_list$omega))
colnames(ABOR1309_01_initMay3_param) <- c("Supply", "SuppAct", "Memory")
write.csv(ABOR1309_01_initMay3_ages, "ABOR1309_01_initMay3_ages.csv")
write.csv(ABOR1309_01_initMay3_accurate, "ABOR1309_01_initMay3_accurate.csv")
write.csv(ABOR1309_01_initMay3_param, "ABOR1309_01_initMay3_param.csv")

#ABOR1309_03PU
ABOR1309_03_initMay3_list <- extract(ABOR1309_03_initMay3)
ABOR1309_03_initMay3_ages <- data.frame(ABOR1309_03_initMay3_list$ages)
colnames(ABOR1309_03_initMay3_ages) <- paste0("Age", 1:ncol(ABOR1309_03_initMay3_ages))
ABOR1309_03_initMay3_accurate <- data.frame(ABOR1309_03_initMay3_list$m)
colnames(ABOR1309_03_initMay3_accurate) <- paste0("m", 1:ncol(ABOR1309_03_initMay3_accurate))
ABOR1309_03_initMay3_param <- data.frame(cbind(ABOR1309_03_initMay3_list$phi, 
                                               ABOR1309_03_initMay3_list$Ps,
                                               ABOR1309_03_initMay3_list$omega))
colnames(ABOR1309_03_initMay3_param) <- c("Supply", "SuppAct", "Memory")
write.csv(ABOR1309_03_initMay3_ages, "ABOR1309_03_initMay3_ages.csv")
write.csv(ABOR1309_03_initMay3_accurate, "ABOR1309_03_initMay3_accurate.csv")
write.csv(ABOR1309_03_initMay3_param, "ABOR1309_03_initMay3_param.csv")

#ABOR1309_04
ABOR1309_04_initMay3_list <- extract(ABOR1309_04_initMay3)
ABOR1309_04_initMay3_ages <- data.frame(ABOR1309_04_initMay3_list$ages)
colnames(ABOR1309_04_initMay3_ages) <- paste0("Age", 1:ncol(ABOR1309_04_initMay3_ages))
ABOR1309_04_initMay3_accurate <- data.frame(ABOR1309_04_initMay3_list$m)
colnames(ABOR1309_04_initMay3_accurate) <- paste0("m", 1:ncol(ABOR1309_04_initMay3_accurate))
ABOR1309_04_initMay3_param <- data.frame(cbind(ABOR1309_04_initMay3_list$phi, 
                                               ABOR1309_04_initMay3_list$Ps,
                                               ABOR1309_04_initMay3_list$omega))
colnames(ABOR1309_04_initMay3_param) <- c("Supply", "SuppAct", "Memory")
write.csv(ABOR1309_04_initMay3_ages, "ABOR1309_04_initMay3_ages.csv")
write.csv(ABOR1309_04_initMay3_accurate, "ABOR1309_04_initMay3_accurate.csv")
write.csv(ABOR1309_04_initMay3_param, "ABOR1309_04_initMay3_param.csv")

#ABOR1702_01
ABOR1702_01_initMay3_list <- extract(ABOR1702_01_initMay3)
ABOR1702_01_initMay3_ages <- data.frame(ABOR1702_01_initMay3_list$ages)
colnames(ABOR1702_01_initMay3_ages) <- paste0("Age", 1:ncol(ABOR1702_01_initMay3_ages))
ABOR1702_01_initMay3_accurate <- data.frame(ABOR1702_01_initMay3_list$m)
colnames(ABOR1702_01_initMay3_accurate) <- paste0("m", 1:ncol(ABOR1702_01_initMay3_accurate))
ABOR1702_01_initMay3_param <- data.frame(cbind(ABOR1702_01_initMay3_list$phi, 
                                               ABOR1702_01_initMay3_list$Ps,
                                               ABOR1702_01_initMay3_list$omega))
colnames(ABOR1702_01_initMay3_param) <- c("Supply", "SuppAct", "Memory")
write.csv(ABOR1702_01_initMay3_ages, "ABOR1702_01_initMay3_ages.csv")
write.csv(ABOR1702_01_initMay3_accurate, "ABOR1702_01_initMay3_accurate.csv")
write.csv(ABOR1702_01_initMay3_param, "ABOR1702_01_initMay3_param.csv")

#ABOR1702_07
ABOR1702_07_initMay3_list <- extract(ABOR1702_07_initMay3)
ABOR1702_07_initMay3_ages <- data.frame(ABOR1702_07_initMay3_list$ages)
colnames(ABOR1702_07_initMay3_ages) <- paste0("Age", 1:ncol(ABOR1702_07_initMay3_ages))
ABOR1702_07_initMay3_accurate <- data.frame(ABOR1702_07_initMay3_list$m)
colnames(ABOR1702_07_initMay3_accurate) <- paste0("m", 1:ncol(ABOR1702_07_initMay3_accurate))
ABOR1702_07_initMay3_param <- data.frame(cbind(ABOR1702_07_initMay3_list$phi, 
                                               ABOR1702_07_initMay3_list$Ps,
                                               ABOR1702_07_initMay3_list$omega))
colnames(ABOR1702_07_initMay3_param) <- c("Supply", "SuppAct", "Memory")
write.csv(ABOR1702_07_initMay3_ages, "ABOR1702_07_initMay3_ages.csv")
write.csv(ABOR1702_07_initMay3_accurate, "ABOR1702_07_initMay3_accurate.csv")
write.csv(ABOR1702_07_initMay3_param, "ABOR1702_07_initMay3_param.csv")


#ABOR1309_01PU
ABOR1309_01_initMay3_ages_summ <- t(apply(X = ABOR1309_01_initMay3_ages[,2:ncol(ABOR1309_01_initMay3_ages)], 
      MARGIN = 2, 
      FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(ABOR1309_01_initMay3_ages_summ)[1:2] <- c("mean", "sd")

ABOR1309_01_initMay3_accurate_summ <- t(apply(X = ABOR1309_01_initMay3_accurate[,2:ncol(ABOR1309_01_initMay3_accurate)], 
                                          MARGIN = 2, 
                                          FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(ABOR1309_01_initMay3_accurate_summ)[1:2] <- c("mean", "sd")
write.csv(ABOR1309_01_initMay3_ages_summ, "ABOR1309_01_initMay3_ages_summ.csv", row.names = F)
write.csv(ABOR1309_01_initMay3_accurate_summ, "ABOR1309_01_initMay3_accurate_summ.csv", row.names = F)


#ABOR1309_03PU
ABOR1309_03_initMay3_ages_summ <- t(apply(X = ABOR1309_03_initMay3_ages[,2:ncol(ABOR1309_03_initMay3_ages)], 
                                          MARGIN = 2, 
                                          FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(ABOR1309_03_initMay3_ages_summ)[1:2] <- c("mean", "sd")

ABOR1309_03_initMay3_accurate_summ <- t(apply(X = ABOR1309_03_initMay3_accurate[,2:ncol(ABOR1309_03_initMay3_accurate)], 
                                              MARGIN = 2, 
                                              FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(ABOR1309_03_initMay3_accurate_summ)[1:2] <- c("mean", "sd")
write.csv(ABOR1309_03_initMay3_ages_summ, "ABOR1309_03_initMay3_ages_summ.csv", row.names = F)
write.csv(ABOR1309_03_initMay3_accurate_summ, "ABOR1309_03_initMay3_accurate_summ.csv", row.names = F)

#ABOR1309_04PU
ABOR1309_04_initMay3_ages_summ <- t(apply(X = ABOR1309_04_initMay3_ages[,2:ncol(ABOR1309_04_initMay3_ages)], 
                                          MARGIN = 2, 
                                          FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(ABOR1309_04_initMay3_ages_summ)[1:2] <- c("mean", "sd")

ABOR1309_04_initMay3_accurate_summ <- t(apply(X = ABOR1309_04_initMay3_accurate[,2:ncol(ABOR1309_04_initMay3_accurate)], 
                                              MARGIN = 2, 
                                              FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(ABOR1309_04_initMay3_accurate_summ)[1:2] <- c("mean", "sd")
write.csv(ABOR1309_04_initMay3_ages_summ, "ABOR1309_04_initMay3_ages_summ.csv", row.names = F)
write.csv(ABOR1309_04_initMay3_accurate_summ, "ABOR1309_04_initMay3_accurate_summ.csv", row.names = F)

#ABOR1702_01PU
ABOR1702_01_initMay3_ages_summ <- t(apply(X = ABOR1702_01_initMay3_ages[,2:ncol(ABOR1702_01_initMay3_ages)], 
                                          MARGIN = 2, 
                                          FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(ABOR1702_01_initMay3_ages_summ)[1:2] <- c("mean", "sd")

ABOR1702_01_initMay3_accurate_summ <- t(apply(X = ABOR1702_01_initMay3_accurate[,2:ncol(ABOR1702_01_initMay3_accurate)], 
                                              MARGIN = 2, 
                                              FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(ABOR1702_01_initMay3_accurate_summ)[1:2] <- c("mean", "sd")
write.csv(ABOR1702_01_initMay3_ages_summ, "ABOR1702_01_initMay3_ages_summ.csv", row.names = F)
write.csv(ABOR1702_01_initMay3_accurate_summ, "ABOR1702_01_initMay3_accurate_summ.csv", row.names = F)

#ABOR1702_07PU
ABOR1702_07_initMay3_ages_summ <- t(apply(X = ABOR1702_07_initMay3_ages[,2:ncol(ABOR1702_07_initMay3_ages)], 
                                          MARGIN = 2, 
                                          FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(ABOR1702_07_initMay3_ages_summ)[1:2] <- c("mean", "sd")

ABOR1702_07_initMay3_accurate_summ <- t(apply(X = ABOR1702_07_initMay3_accurate[,2:ncol(ABOR1702_07_initMay3_accurate)], 
                                              MARGIN = 2, 
                                              FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(ABOR1702_07_initMay3_accurate_summ)[1:2] <- c("mean", "sd")
write.csv(ABOR1702_07_initMay3_ages_summ, "ABOR1702_07_initMay3_ages_summ.csv", row.names = F)
write.csv(ABOR1702_07_initMay3_accurate_summ, "ABOR1702_07_initMay3_accurate_summ.csv", row.names = F)

#ABOR1309_03_run2_initMay14 
ABOR1309_03_run2_initMay14_list <- extract(ABOR1309_03_run2_initMay14)
ABOR1309_03_run2_initMay14_ages <- data.frame(ABOR1309_03_run2_initMay14_list$ages)
colnames(ABOR1309_03_run2_initMay14_ages) <- paste0("Age", 1:ncol(ABOR1309_03_run2_initMay14_ages))
ABOR1309_03_run2_initMay14_accurate <- data.frame(ABOR1309_03_run2_initMay14_list$m)
colnames(ABOR1309_03_run2_initMay14_accurate) <- paste0("m", 1:ncol(ABOR1309_03_run2_initMay14_accurate))
ABOR1309_03_run2_initMay14_param <- data.frame(cbind(ABOR1309_03_run2_initMay14_list$phi, 
                                               ABOR1309_03_run2_initMay14_list$Ps,
                                               ABOR1309_03_run2_initMay14_list$omega))
colnames(ABOR1309_03_run2_initMay14_param) <- c("Supply", "SuppAct", "Memory")
write.csv(ABOR1309_03_run2_initMay14_ages, "ABOR1309_03_run2_initMay14_ages.csv")
write.csv(ABOR1309_03_run2_initMay14_accurate, "ABOR1309_03_run2_initMay14_accurate.csv")
write.csv(ABOR1309_03_run2_initMay14_param, "ABOR1309_03_run2_initMay14_param.csv")

ABOR1309_03_run2_initMay14_ages_summ <- t(apply(X = ABOR1309_03_run2_initMay14_ages[,2:ncol(ABOR1309_03_run2_initMay14_ages)], 
                                          MARGIN = 2, 
                                          FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(ABOR1309_03_run2_initMay14_ages_summ)[1:2] <- c("mean", "sd")

ABOR1309_03_run2_initMay14_accurate_summ <- t(apply(X = ABOR1309_03_run2_initMay14_accurate[,2:ncol(ABOR1309_03_run2_initMay14_accurate)], 
                                              MARGIN = 2, 
                                              FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(ABOR1309_03_run2_initMay14_accurate_summ)[1:2] <- c("mean", "sd")
write.csv(ABOR1309_03_run2_initMay14_ages_summ, "ABOR1309_03_run2_initMay14_ages_summ.csv", row.names = F)
write.csv(ABOR1309_03_run2_initMay14_accurate_summ, "ABOR1309_03_run2_initMay14_accurate_summ.csv", row.names = F)

