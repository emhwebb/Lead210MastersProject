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
#NBOR1606_04PU
NBOR1606_04PU <- peck_cores %>% 
  filter(core_id == "NBOR1606_04PU", depth_max <= 28) %>%
  select(depth_max, dry_bulk_density_modeled, total_pb210_activity, total_pb210_activity_se,
         pb214_activity, pb214_activity_se, excess_pb210_activity, excess_pb210_activity_se)	
N <- 14
M <- 15
L <- 14
Tot <- N+L
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
beta_P <- 1
#accumulation rate/gamma noise
alpha_a <- 1
beta_a <- 0.21
#plot(0, 0, xlim = c(0, 50), ylim = c(0, 0.4), type = "n")
#curve(dgamma(x, shape = 1, rate = 0.2), from = 0, to = 50, add = TRUE)

NBOR1606_04PU_dat <- list(N = N, L = L, Tot = Tot, x = x, xL = xL, delta = delta, M=M,
                          rho = rho, rhoL = rhoL, a = 3, b = 4, sigma = sigma, sigmaL = sigmaL, 
                          p = p, pL = pL, lambda = lambda,  alpha_phi = alpha_phi, 
                          beta_phi = beta_phi, a_omega = a_omega, b_omega = b_omega, alpha_P = alpha_P, 
                          beta_P = beta_P, alpha_a = alpha_a, beta_a = beta_a)


NBOR1606_04_initJuly29nonconst <- stan(file = 'Pb210_nonconst_tdistr.stan', data = NBOR1606_04PU_dat, 
                                       chains = 4, iter = 200000, control = list(adapt_delta = 0.99))
saveRDS(NBOR1606_04_initJuly29nonconst, "NBOR1606_04_initJuly29nonconst.rds")

#save to .csv
NBOR1606_04_initJuly29nonconst_list <- extract(NBOR1606_04_initJuly29nonconst)
#ages
NBOR1606_04_initJuly29nonconst_ages <- data.frame(NBOR1606_04_initJuly29nonconst_list$ages)
colnames(NBOR1606_04_initJuly29nonconst_ages) <- paste0("Age", 1:ncol(NBOR1606_04_initJuly29nonconst_ages))
#sedi rates
NBOR1606_04_initJuly29nonconst_sedirate <- data.frame(NBOR1606_04_initJuly29nonconst_list$m)
colnames(NBOR1606_04_initJuly29nonconst_sedirate) <- paste0("m", 1:ncol(NBOR1606_04_initJuly29nonconst_sedirate))
#accu rates
NBOR1606_04_initJuly29nonconst_accurate <- data.frame(NBOR1606_04_initJuly29nonconst_list$accu_rates)
colnames(NBOR1606_04_initJuly29nonconst_accurate) <- paste0("m", 1:ncol(NBOR1606_04_initJuly29nonconst_accurate))
NBOR1606_04_initJuly29nonconst_param <- data.frame(cbind(NBOR1606_04_initJuly29nonconst_list$phi, 
                                                         NBOR1606_04_initJuly29nonconst_list$Ps,
                                                         NBOR1606_04_initJuly29nonconst_list$omega))
colnames(NBOR1606_04_initJuly29nonconst_param) <- c("Supply", "SuppAct", "Memory")
write.csv(NBOR1606_04_initJuly29nonconst_ages, "NBOR1606_04_initJuly29nonconst_ages.csv")
write.csv(NBOR1606_04_initJuly29nonconst_sedirate, "NBOR1606_04_initJuly29nonconst_sedirate.csv")
write.csv(NBOR1606_04_initJuly29nonconst_accurate, "NBOR1606_04_initJuly29nonconst_accurate.csv")
write.csv(NBOR1606_04_initJuly29nonconst_param, "NBOR1606_04_initJuly29nonconst_param.csv")

#save summaries to .csv

#ages
NBOR1606_04_initJuly29nonconst_ages_summ <- t(apply(X = NBOR1606_04_initJuly29nonconst_ages[,1:ncol(NBOR1606_04_initJuly29nonconst_ages)], 
                                                    MARGIN = 2, 
                                                    FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(NBOR1606_04_initJuly29nonconst_ages_summ)[1:2] <- c("mean", "sd")

#sedi rates
NBOR1606_04_initJuly29nonconst_sedirate_summ <- t(apply(X = NBOR1606_04_initJuly29nonconst_sedirate[,1:ncol(NBOR1606_04_initJuly29nonconst_sedirate)], 
                                                        MARGIN = 2, 
                                                        FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(NBOR1606_04_initJuly29nonconst_sedirate_summ)[1:2] <- c("mean", "sd")

#accu rates
NBOR1606_04_initJuly29nonconst_accurate_summ <- t(apply(X = NBOR1606_04_initJuly29nonconst_accurate[,1:ncol(NBOR1606_04_initJuly29nonconst_accurate)], 
                                                        MARGIN = 2, 
                                                        FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(NBOR1606_04_initJuly29nonconst_accurate_summ)[1:2] <- c("mean", "sd")
write.csv(NBOR1606_04_initJuly29nonconst_ages_summ, "NBOR1606_04_initJuly29nonconst_ages_summ.csv", row.names = F)
write.csv(NBOR1606_04_initJuly29nonconst_sedirate_summ, "NBOR1606_04_initJuly29nonconst_sedirate_summ.csv", row.names = F)
write.csv(NBOR1606_04_initJuly29nonconst_accurate_summ, "NBOR1606_04_initJuly29nonconst_accurate_summ.csv", row.names = F)
