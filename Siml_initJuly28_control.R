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

#Simulation Set-Up: Use core_sec_siml_2_nonconst_supp.R file
#testing with nonconst support and also with constant support

###nonconst supply
#normal alpha
N <- 27
M <- 28
L <- 3
delta <- 1
x <- 1:27
xL <- 28:30
rho <- siml_coredens(1:27)
rhoL <- siml_coredens(28:30)
sigma <- sigmas[1:27]
sigmaL <- sigmas[28:30]
p <- siml_conc2[1:27]
pL <- siml_conc2[28:30]
lambda <- 0.03114 
#omega <- 0.5
#priors
#yearly accumulation
alpha_phi <- 1
beta_phi <- 0.007
#memory parameter #try beta(1,5)
a_omega <- 1.1
b_omega <- 1.2
#plot(0, 0, xlim = c(0, 1), ylim = c(0, 5), type = "n")
#curve(dbeta(x, shape1 = 2*.11, shape2 = 2*.9), from = 0, to = 1, add = TRUE)
#supported activity
alpha_P <- 1
beta_P <- 0.05
#accumulation rate/gamma noise
alpha_a <- 1
beta_a <- 0.15
#plot(0, 0, xlim = c(0, 50), ylim = c(0, 0.4), type = "n")
#curve(dgamma(x, shape =  3, rate = .9), from = 0, to = 50, add = TRUE)

nonconst_supp_alph_siml_dat <- list(N = N, L = L, x = x, xL = xL, delta = delta, M=M,
                            rho = rho, rhoL = rhoL, sigma = sigma, sigmaL = sigmaL, 
                            p = p, pL = pL, lambda = lambda,  alpha_phi = alpha_phi, 
                            beta_phi = beta_phi, a_omega = a_omega, b_omega = b_omega, alpha_P = alpha_P, 
                            beta_P = beta_P, alpha_a = alpha_a, beta_a = beta_a)

nonconst_supp_norm_alph_initJuly28 <- stan(file = 'Pb210_draft.stan', data = nonconst_supp_alph_siml_dat, 
                                     chains = 4, iter = 200000, control = list(adapt_delta = 0.99))
saveRDS(nonconst_supp_norm_alph_initJuly28, "nonconst_supp_norm_alph_initJuly28.rds")

#save to .csv
nonconst_supp_norm_alph_initJuly28_list <- extract(nonconst_supp_norm_alph_initJuly28)
nonconst_supp_norm_alph_initJuly28_ages <- data.frame(nonconst_supp_norm_alph_initJuly28_list$ages)
colnames(nonconst_supp_norm_alph_initJuly28_ages) <- paste0("Age", 1:ncol(nonconst_supp_norm_alph_initJuly28_ages))
nonconst_supp_norm_alph_initJuly28_accurate <- data.frame(nonconst_supp_norm_alph_initJuly28_list$m)
colnames(nonconst_supp_norm_alph_initJuly28_accurate) <- paste0("m", 1:ncol(nonconst_supp_norm_alph_initJuly28_accurate))
nonconst_supp_norm_alph_initJuly28_param <- data.frame(cbind(nonconst_supp_norm_alph_initJuly28_list$phi, 
                                                       nonconst_supp_norm_alph_initJuly28_list$Ps,
                                                       nonconst_supp_norm_alph_initJuly28_list$omega))
colnames(nonconst_supp_norm_alph_initJuly28_param) <- c("Supply", "SuppAct", "Memory")
write.csv(nonconst_supp_norm_alph_initJuly28_ages, "nonconst_supp_norm_alph_initJuly28_ages.csv")
write.csv(nonconst_supp_norm_alph_initJuly28_accurate, "nonconst_supp_norm_alph_initJuly28_accurate.csv")
write.csv(nonconst_supp_norm_alph_initJuly28_param, "nonconst_supp_norm_alph_initJuly28_param.csv")

#save summaries to .csv
nonconst_supp_norm_alph_initJuly28_ages_summ <- t(apply(X = nonconst_supp_norm_alph_initJuly28_ages[,1:ncol(nonconst_supp_norm_alph_initJuly28_ages)], 
                                                  MARGIN = 2, 
                                                  FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(nonconst_supp_norm_alph_initJuly28_ages_summ)[1:2] <- c("mean", "sd")

nonconst_supp_norm_alph_initJuly28_accurate_summ <- t(apply(X = nonconst_supp_norm_alph_initJuly28_accurate[,1:ncol(nonconst_supp_norm_alph_initJuly28_accurate)], 
                                                      MARGIN = 2, 
                                                      FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(nonconst_supp_norm_alph_initJuly28_accurate_summ)[1:2] <- c("mean", "sd")
write.csv(nonconst_supp_norm_alph_initJuly28_ages_summ, "nonconst_supp_norm_alph_initJuly28_ages_summ.csv", row.names = F)
write.csv(nonconst_supp_norm_alph_initJuly28_accurate_summ, "nonconst_supp_norm_alph_initJuly28_accurate_summ.csv", row.names = F)

plot(nonconst_supp_norm_alph_initJuly28, pars = "ages")
plot(nonconst_supp_norm_alph_initJuly28, pars = "m")
plot(nonconst_supp_norm_alph_initJuly28, pars = "alpha")
plot(nonconst_supp_norm_alph_initJuly28, pars = "omega")
plot(nonconst_supp_norm_alph_initJuly28, plotfun = "hist", pars = "phi")
plot(nonconst_supp_norm_alph_initJuly28, plotfun = "hist", pars = "Ps")
plot(nonconst_supp_norm_alph_initJuly28, plotfun = "hist", pars = "omega")
plot(nonconst_supp_norm_alph_initJuly28, plotfun = "hist", pars = "ages")
plot(nonconst_supp_norm_alph_initJuly28, plotfun = "hist", pars = "ages", binwidth = 0.5)
plot(nonconst_supp_norm_alph_initJuly28, plotfun = "hist", pars = "alpha")
plot(nonconst_supp_norm_alph_initJuly28, plotfun = "hist", pars = "m")
plot(nonconst_supp_norm_alph_initJuly28, plotfun = "hist", pars = "mN")
plot(nonconst_supp_norm_alph_initJuly28, plotfun = "trace", pars = "omega")
plot(nonconst_supp_norm_alph_initJuly28, plotfun = "trace", pars = "phi")
plot(nonconst_supp_norm_alph_initJuly28, plotfun = "trace", pars = "Ps")
plot(nonconst_supp_norm_alph_initJuly28, plotfun = "trace", pars = "ages")
plot(nonconst_supp_norm_alph_initJuly28, plotfun = "trace", pars = "mN")
plot(nonconst_supp_norm_alph_initJuly28, plotfun = "trace", pars = "alpha")

#normal with gamma

N <- 30
M <- 31
L <- 30
delta <- 1
x <- 1:30
xL <- 1:30
rho <- siml_coredens(1:30)
rhoL <- siml_coredens(1:30)
sigma <- sigmas[1:30]
sigmaL <- sigmas[1:30]
p <- siml_conc2[1:30]
pL <- siml_supp2[1:30]
lambda <- 0.03114 
#omega <- 0.5
#priors
#yearly accumulation
alpha_phi <- 1
beta_phi <- 0.007
#memory parameter #try beta(1,5)
a_omega <- 1.1
b_omega <- 1.2
#plot(0, 0, xlim = c(0, 1), ylim = c(0, 1), type = "n")
#curve(dbeta(x, shape1 = 1.1, shape2 = 1.2), from = 0, to = 1, add = TRUE)
#supported activity
alpha_P <- 1
beta_P <- 0.05
#accumulation rate/gamma noise
alpha_a <- 1
beta_a <- 0.15
#plot(0, 0, xlim = c(0, 50), ylim = c(0, 0.4), type = "n")
#curve(dgamma(x, shape = 1, rate = 0.2), from = 0, to = 50, add = TRUE)

nonconst_supp_gam_siml_dat <- list(N = N, L = L, x = x, xL = xL, delta = delta, M=M,
                            rho = rho, rhoL = rhoL, sigma = sigma, sigmaL = sigmaL, 
                            p = p, pL = pL, lambda = lambda,  alpha_phi = alpha_phi, 
                            beta_phi = beta_phi, a_omega = a_omega, b_omega = b_omega, alpha_P = alpha_P, 
                            beta_P = beta_P, alpha_a = alpha_a, beta_a = beta_a)

nonconst_supp_norm_gam_initJuly28 <- stan(file = 'Pb210_draft.stan', data = nonconst_supp_gam_siml_dat, 
                                         chains = 4, iter = 200000, control = list(adapt_delta = 0.99))
saveRDS(nonconst_supp_norm_gam_initJuly28, "nonconst_supp_norm_gam_initJuly28.rds")

#save to .csv
nonconst_supp_norm_gam_initJuly28_list <- extract(nonconst_supp_norm_gam_initJuly28)
nonconst_supp_norm_gam_initJuly28_ages <- data.frame(nonconst_supp_norm_gam_initJuly28_list$ages)
colnames(nonconst_supp_norm_gam_initJuly28_ages) <- paste0("Age", 1:ncol(nonconst_supp_norm_gam_initJuly28_ages))
nonconst_supp_norm_gam_initJuly28_accurate <- data.frame(nonconst_supp_norm_gam_initJuly28_list$m)
colnames(nonconst_supp_norm_gam_initJuly28_accurate) <- paste0("m", 1:ncol(nonconst_supp_norm_gam_initJuly28_accurate))
nonconst_supp_norm_gam_initJuly28_param <- data.frame(cbind(nonconst_supp_norm_gam_initJuly28_list$phi, 
                                                           nonconst_supp_norm_gam_initJuly28_list$Ps,
                                                           nonconst_supp_norm_gam_initJuly28_list$omega))
colnames(nonconst_supp_norm_gam_initJuly28_param) <- c("Supply", "SuppAct", "Memory")
write.csv(nonconst_supp_norm_gam_initJuly28_ages, "nonconst_supp_norm_gam_initJuly28_ages.csv")
write.csv(nonconst_supp_norm_gam_initJuly28_accurate, "nonconst_supp_norm_gam_initJuly28_accurate.csv")
write.csv(nonconst_supp_norm_gam_initJuly28_param, "nonconst_supp_norm_gam_initJuly28_param.csv")

#save summaries to .csv
nonconst_supp_norm_gam_initJuly28_ages_summ <- t(apply(X = nonconst_supp_norm_gam_initJuly28_ages[,1:ncol(nonconst_supp_norm_gam_initJuly28_ages)], 
                                                      MARGIN = 2, 
                                                      FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(nonconst_supp_norm_gam_initJuly28_ages_summ)[1:2] <- c("mean", "sd")

nonconst_supp_norm_gam_initJuly28_accurate_summ <- t(apply(X = nonconst_supp_norm_gam_initJuly28_accurate[,1:ncol(nonconst_supp_norm_gam_initJuly28_accurate)], 
                                                          MARGIN = 2, 
                                                          FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(nonconst_supp_norm_gam_initJuly28_accurate_summ)[1:2] <- c("mean", "sd")
write.csv(nonconst_supp_norm_gam_initJuly28_ages_summ, "nonconst_supp_norm_gam_initJuly28_ages_summ.csv", row.names = F)
write.csv(nonconst_supp_norm_gam_initJuly28_accurate_summ, "nonconst_supp_norm_gam_initJuly28_accurate_summ.csv", row.names = F)

plot(nonconst_supp_norm_gam_initJuly28, pars = "ages")
plot(nonconst_supp_norm_gam_initJuly28, pars = "m")
plot(nonconst_supp_norm_gam_initJuly28, pars = "alpha")
plot(nonconst_supp_norm_gam_initJuly28, pars = "omega")
plot(nonconst_supp_norm_gam_initJuly28, plotfun = "hist", pars = "phi")
plot(nonconst_supp_norm_gam_initJuly28, plotfun = "hist", pars = "Ps")
plot(nonconst_supp_norm_gam_initJuly28, plotfun = "hist", pars = "omega")
plot(nonconst_supp_norm_gam_initJuly28, plotfun = "hist", pars = "ages")
plot(nonconst_supp_norm_gam_initJuly28, plotfun = "hist", pars = "ages", binwidth = 0.5)
plot(nonconst_supp_norm_gam_initJuly28, plotfun = "hist", pars = "alpha")
plot(nonconst_supp_norm_gam_initJuly28, plotfun = "hist", pars = "m")
plot(nonconst_supp_norm_gam_initJuly28, plotfun = "hist", pars = "mN")
plot(nonconst_supp_norm_gam_initJuly28, plotfun = "trace", pars = "omega")
plot(nonconst_supp_norm_gam_initJuly28, plotfun = "trace", pars = "phi")
plot(nonconst_supp_norm_gam_initJuly28, plotfun = "trace", pars = "Ps")
plot(nonconst_supp_norm_gam_initJuly28, plotfun = "trace", pars = "ages")
plot(nonconst_supp_norm_gam_initJuly28, plotfun = "trace", pars = "mN")
plot(nonconst_supp_norm_gam_initJuly28, plotfun = "trace", pars = "alpha")


#normal nonconst with gamma

N <- 30
M <- 31
L <- 30
delta <- 1
x <- 1:30
xL <- 1:30
rho <- siml_coredens(1:30)
rhoL <- siml_coredens(1:30)
sigma <- sigmas[1:30]
sigmaL <- sigmas[1:30]
p <- siml_conc2[1:30]
pL <- siml_supp2[1:30]
lambda <- 0.03114 
#omega <- 0.5
#priors
#yearly accumulation
alpha_phi <- 1
beta_phi <- 0.007
#memory parameter #try beta(1,5)
a_omega <- 1.1
b_omega <- 1.2
#plot(0, 0, xlim = c(0, 1), ylim = c(0, 1), type = "n")
#curve(dbeta(x, shape1 = 1.1, shape2 = 1.2), from = 0, to = 1, add = TRUE)
#supported activity
alpha_P <- 1
beta_P <- 0.05
#accumulation rate/gamma noise
alpha_a <- 1
beta_a <- 0.15
#plot(0, 0, xlim = c(0, 50), ylim = c(0, 0.4), type = "n")
#curve(dgamma(x, shape = 1, rate = 0.2), from = 0, to = 50, add = TRUE)

nonconst_supp_gam_siml_dat <- list(N = N, L = L, x = x, xL = xL, delta = delta, M=M,
                                   rho = rho, rhoL = rhoL, sigma = sigma, sigmaL = sigmaL, 
                                   p = p, pL = pL, lambda = lambda,  alpha_phi = alpha_phi, 
                                   beta_phi = beta_phi, a_omega = a_omega, b_omega = b_omega, alpha_P = alpha_P, 
                                   beta_P = beta_P, alpha_a = alpha_a, beta_a = beta_a)

nonconst_supp_nonconst_norm_gam_initJuly28 <- stan(file = 'Pb210_nonconst_supp.stan', data = nonconst_supp_gam_siml_dat, 
                                          chains = 4, iter = 200000, control = list(adapt_delta = 0.99))
saveRDS(nonconst_supp_nonconst_norm_gam_initJuly28, "nonconst_supp_nonconst_norm_gam_initJuly28.rds")

#save to .csv
nonconst_supp_nonconst_norm_gam_initJuly28_list <- extract(nonconst_supp_nonconst_norm_gam_initJuly28)
nonconst_supp_nonconst_norm_gam_initJuly28_ages <- data.frame(nonconst_supp_nonconst_norm_gam_initJuly28_list$ages)
colnames(nonconst_supp_nonconst_norm_gam_initJuly28_ages) <- paste0("Age", 1:ncol(nonconst_supp_nonconst_norm_gam_initJuly28_ages))
nonconst_supp_nonconst_norm_gam_initJuly28_accurate <- data.frame(nonconst_supp_nonconst_norm_gam_initJuly28_list$m)
colnames(nonconst_supp_nonconst_norm_gam_initJuly28_accurate) <- paste0("m", 1:ncol(nonconst_supp_nonconst_norm_gam_initJuly28_accurate))
nonconst_supp_nonconst_norm_gam_initJuly28_param <- data.frame(cbind(nonconst_supp_nonconst_norm_gam_initJuly28_list$phi, 
                                                            nonconst_supp_nonconst_norm_gam_initJuly28_list$Ps,
                                                            nonconst_supp_nonconst_norm_gam_initJuly28_list$omega))
colnames(nonconst_supp_nonconst_norm_gam_initJuly28_param) <- c("Supply", "SuppAct", "Memory")
write.csv(nonconst_supp_nonconst_norm_gam_initJuly28_ages, "nonconst_supp_nonconst_norm_gam_initJuly28_ages.csv")
write.csv(nonconst_supp_nonconst_norm_gam_initJuly28_accurate, "nonconst_supp_nonconst_norm_gam_initJuly28_accurate.csv")
write.csv(nonconst_supp_nonconst_norm_gam_initJuly28_param, "nonconst_supp_nonconst_norm_gam_initJuly28_param.csv")

#save summaries to .csv
nonconst_supp_nonconst_norm_gam_initJuly28_ages_summ <- t(apply(X = nonconst_supp_nonconst_norm_gam_initJuly28_ages[,1:ncol(nonconst_supp_nonconst_norm_gam_initJuly28_ages)], 
                                                       MARGIN = 2, 
                                                       FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(nonconst_supp_nonconst_norm_gam_initJuly28_ages_summ)[1:2] <- c("mean", "sd")

nonconst_supp_nonconst_norm_gam_initJuly28_accurate_summ <- t(apply(X = nonconst_supp_nonconst_norm_gam_initJuly28_accurate[,1:ncol(nonconst_supp_nonconst_norm_gam_initJuly28_accurate)], 
                                                           MARGIN = 2, 
                                                           FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(nonconst_supp_nonconst_norm_gam_initJuly28_accurate_summ)[1:2] <- c("mean", "sd")
write.csv(nonconst_supp_nonconst_norm_gam_initJuly28_ages_summ, "nonconst_supp_nonconst_norm_gam_initJuly28_ages_summ.csv", row.names = F)
write.csv(nonconst_supp_nonconst_norm_gam_initJuly28_accurate_summ, "nonconst_supp_nonconst_norm_gam_initJuly28_accurate_summ.csv", row.names = F)

plot(nonconst_supp_nonconst_norm_gam_initJuly28, pars = "ages")
plot(nonconst_supp_nonconst_norm_gam_initJuly28, pars = "m")
plot(nonconst_supp_nonconst_norm_gam_initJuly28, pars = "alpha")
plot(nonconst_supp_nonconst_norm_gam_initJuly28, pars = "omega")
plot(nonconst_supp_nonconst_norm_gam_initJuly28, plotfun = "hist", pars = "phi")
plot(nonconst_supp_nonconst_norm_gam_initJuly28, plotfun = "hist", pars = "Ps")
plot(nonconst_supp_nonconst_norm_gam_initJuly28, plotfun = "hist", pars = "omega")
plot(nonconst_supp_nonconst_norm_gam_initJuly28, plotfun = "hist", pars = "ages")
plot(nonconst_supp_nonconst_norm_gam_initJuly28, plotfun = "hist", pars = "ages", binwidth = 0.5)
plot(nonconst_supp_nonconst_norm_gam_initJuly28, plotfun = "hist", pars = "alpha")
plot(nonconst_supp_nonconst_norm_gam_initJuly28, plotfun = "hist", pars = "m")
plot(nonconst_supp_nonconst_norm_gam_initJuly28, plotfun = "hist", pars = "mN")
plot(nonconst_supp_nonconst_norm_gam_initJuly28, plotfun = "trace", pars = "omega")
plot(nonconst_supp_nonconst_norm_gam_initJuly28, plotfun = "trace", pars = "phi")
plot(nonconst_supp_nonconst_norm_gam_initJuly28, plotfun = "trace", pars = "Ps")
plot(nonconst_supp_nonconst_norm_gam_initJuly28, plotfun = "trace", pars = "ages")
plot(nonconst_supp_nonconst_norm_gam_initJuly28, plotfun = "trace", pars = "mN")
plot(nonconst_supp_nonconst_norm_gam_initJuly28, plotfun = "trace", pars = "alpha")

##t distribution
#t alpha
N <- 27
M <- 28
L <- 3
Tot <- N+L
delta <- 1
x <- 1:27
xL <- 28:30
rho <- siml_coredens(1:27)
rhoL <- siml_coredens(28:30)
sigma <- sigmas[1:27]
sigmaL <- sigmas[28:30]
p <- siml_conc2[1:27]
pL <- siml_conc2[28:30]
lambda <- 0.03114 
#omega <- 0.5
#priors
#yearly accumulation
alpha_phi <- 1
beta_phi <- 0.007
#memory parameter #try beta(1,5)
a_omega <- 1.1
b_omega <- 1.2
#plot(0, 0, xlim = c(0, 1), ylim = c(0, 5), type = "n")
#curve(dbeta(x, shape1 = 2*.11, shape2 = 2*.9), from = 0, to = 1, add = TRUE)
#supported activity
alpha_P <- 1
beta_P <- 0.05
#accumulation rate/gamma noise
alpha_a <- 1
beta_a <- 0.15
#plot(0, 0, xlim = c(0, 50), ylim = c(0, 0.4), type = "n")
#curve(dgamma(x, shape =  3, rate = .9), from = 0, to = 50, add = TRUE)

nonconst_supp_alph_siml_dat <- list(N = N, L = L, Tot = Tot, x = x, xL = xL, delta = delta, M=M,
                                    rho = rho, rhoL = rhoL, a = 3, b = 4, sigma = sigma, sigmaL = sigmaL, 
                                    p = p, pL = pL, lambda = lambda,  alpha_phi = alpha_phi, 
                                    beta_phi = beta_phi, a_omega = a_omega, b_omega = b_omega, alpha_P = alpha_P, 
                                    beta_P = beta_P, alpha_a = alpha_a, beta_a = beta_a)

nonconst_supp_t_alph_initJuly28 <- stan(file = 'Pb210_tdistr.stan', data = nonconst_supp_alph_siml_dat, 
                                           chains = 4, iter = 200000, control = list(adapt_delta = 0.99))
saveRDS(nonconst_supp_t_alph_initJuly28, "nonconst_supp_t_alph_initJuly28.rds")

#save to .csv
nonconst_supp_t_alph_initJuly28_list <- extract(nonconst_supp_t_alph_initJuly28)
nonconst_supp_t_alph_initJuly28_ages <- data.frame(nonconst_supp_t_alph_initJuly28_list$ages)
colnames(nonconst_supp_t_alph_initJuly28_ages) <- paste0("Age", 1:ncol(nonconst_supp_t_alph_initJuly28_ages))
nonconst_supp_t_alph_initJuly28_accurate <- data.frame(nonconst_supp_t_alph_initJuly28_list$m)
colnames(nonconst_supp_t_alph_initJuly28_accurate) <- paste0("m", 1:ncol(nonconst_supp_t_alph_initJuly28_accurate))
nonconst_supp_t_alph_initJuly28_param <- data.frame(cbind(nonconst_supp_t_alph_initJuly28_list$phi, 
                                                             nonconst_supp_t_alph_initJuly28_list$Ps,
                                                             nonconst_supp_t_alph_initJuly28_list$omega))
colnames(nonconst_supp_t_alph_initJuly28_param) <- c("Supply", "SuppAct", "Memory")
write.csv(nonconst_supp_t_alph_initJuly28_ages, "nonconst_supp_t_alph_initJuly28_ages.csv")
write.csv(nonconst_supp_t_alph_initJuly28_accurate, "nonconst_supp_t_alph_initJuly28_accurate.csv")
write.csv(nonconst_supp_t_alph_initJuly28_param, "nonconst_supp_t_alph_initJuly28_param.csv")

#save summaries to .csv
nonconst_supp_t_alph_initJuly28_ages_summ <- t(apply(X = nonconst_supp_t_alph_initJuly28_ages[,1:ncol(nonconst_supp_t_alph_initJuly28_ages)], 
                                                        MARGIN = 2, 
                                                        FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(nonconst_supp_t_alph_initJuly28_ages_summ)[1:2] <- c("mean", "sd")

nonconst_supp_t_alph_initJuly28_accurate_summ <- t(apply(X = nonconst_supp_t_alph_initJuly28_accurate[,1:ncol(nonconst_supp_t_alph_initJuly28_accurate)], 
                                                            MARGIN = 2, 
                                                            FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(nonconst_supp_t_alph_initJuly28_accurate_summ)[1:2] <- c("mean", "sd")
write.csv(nonconst_supp_t_alph_initJuly28_ages_summ, "nonconst_supp_t_alph_initJuly28_ages_summ.csv", row.names = F)
write.csv(nonconst_supp_t_alph_initJuly28_accurate_summ, "nonconst_supp_t_alph_initJuly28_accurate_summ.csv", row.names = F)

plot(nonconst_supp_t_alph_initJuly28, pars = "ages")
plot(nonconst_supp_t_alph_initJuly28, pars = "m")
plot(nonconst_supp_t_alph_initJuly28, pars = "alpha")
plot(nonconst_supp_t_alph_initJuly28, pars = "omega")
plot(nonconst_supp_t_alph_initJuly28, plotfun = "hist", pars = "phi")
plot(nonconst_supp_t_alph_initJuly28, plotfun = "hist", pars = "Ps")
plot(nonconst_supp_t_alph_initJuly28, plotfun = "hist", pars = "omega")
plot(nonconst_supp_t_alph_initJuly28, plotfun = "hist", pars = "ages")
plot(nonconst_supp_t_alph_initJuly28, plotfun = "hist", pars = "ages", binwidth = 0.5)
plot(nonconst_supp_t_alph_initJuly28, plotfun = "hist", pars = "alpha")
plot(nonconst_supp_t_alph_initJuly28, plotfun = "hist", pars = "m")
plot(nonconst_supp_t_alph_initJuly28, plotfun = "hist", pars = "mN")
plot(nonconst_supp_t_alph_initJuly28, plotfun = "trace", pars = "omega")
plot(nonconst_supp_t_alph_initJuly28, plotfun = "trace", pars = "phi")
plot(nonconst_supp_t_alph_initJuly28, plotfun = "trace", pars = "Ps")
plot(nonconst_supp_t_alph_initJuly28, plotfun = "trace", pars = "ages")
plot(nonconst_supp_t_alph_initJuly28, plotfun = "trace", pars = "mN")
plot(nonconst_supp_t_alph_initJuly28, plotfun = "trace", pars = "alpha")

#t with gamma

N <- 30
M <- 31
L <- 30
Tot <- N+L
delta <- 1
x <- 1:30
xL <- 1:30
rho <- siml_coredens(1:30)
rhoL <- siml_coredens(1:30)
sigma <- sigmas[1:30]
sigmaL <- sigmas[1:30]
p <- siml_conc2[1:30]
pL <- siml_supp2[1:30]
lambda <- 0.03114 
#omega <- 0.5
#priors
#yearly accumulation
alpha_phi <- 1
beta_phi <- 0.007
#memory parameter #try beta(1,5)
a_omega <- 1.1
b_omega <- 1.2
#plot(0, 0, xlim = c(0, 1), ylim = c(0, 1), type = "n")
#curve(dbeta(x, shape1 = 1.1, shape2 = 1.2), from = 0, to = 1, add = TRUE)
#supported activity
alpha_P <- 1
beta_P <- 0.05
#accumulation rate/gamma noise
alpha_a <- 1
beta_a <- 0.15
#plot(0, 0, xlim = c(0, 50), ylim = c(0, 0.4), type = "n")
#curve(dgamma(x, shape = 1, rate = 0.2), from = 0, to = 50, add = TRUE)

nonconst_supp_gam_siml_dat <- list(N = N, L = L, Tot = Tot, x = x, xL = xL, delta = delta, M=M,
                                   rho = rho, rhoL = rhoL, a = 3, b = 4, sigma = sigma, sigmaL = sigmaL, 
                                   p = p, pL = pL, lambda = lambda,  alpha_phi = alpha_phi, 
                                   beta_phi = beta_phi, a_omega = a_omega, b_omega = b_omega, alpha_P = alpha_P, 
                                   beta_P = beta_P, alpha_a = alpha_a, beta_a = beta_a)

nonconst_supp_t_gam_initJuly28 <- stan(file = 'Pb210_tdistr.stan', data = nonconst_supp_gam_siml_dat, 
                                          chains = 4, iter = 200000, control = list(adapt_delta = 0.99))
saveRDS(nonconst_supp_t_gam_initJuly28, "nonconst_supp_t_gam_initJuly28.rds")

#save to .csv
nonconst_supp_t_gam_initJuly28_list <- extract(nonconst_supp_t_gam_initJuly28)
nonconst_supp_t_gam_initJuly28_ages <- data.frame(nonconst_supp_t_gam_initJuly28_list$ages)
colnames(nonconst_supp_t_gam_initJuly28_ages) <- paste0("Age", 1:ncol(nonconst_supp_t_gam_initJuly28_ages))
nonconst_supp_t_gam_initJuly28_accurate <- data.frame(nonconst_supp_t_gam_initJuly28_list$m)
colnames(nonconst_supp_t_gam_initJuly28_accurate) <- paste0("m", 1:ncol(nonconst_supp_t_gam_initJuly28_accurate))
nonconst_supp_t_gam_initJuly28_param <- data.frame(cbind(nonconst_supp_t_gam_initJuly28_list$phi, 
                                                            nonconst_supp_t_gam_initJuly28_list$Ps,
                                                            nonconst_supp_t_gam_initJuly28_list$omega))
colnames(nonconst_supp_t_gam_initJuly28_param) <- c("Supply", "SuppAct", "Memory")
write.csv(nonconst_supp_t_gam_initJuly28_ages, "nonconst_supp_t_gam_initJuly28_ages.csv")
write.csv(nonconst_supp_t_gam_initJuly28_accurate, "nonconst_supp_t_gam_initJuly28_accurate.csv")
write.csv(nonconst_supp_t_gam_initJuly28_param, "nonconst_supp_t_gam_initJuly28_param.csv")

#save summaries to .csv
nonconst_supp_t_gam_initJuly28_ages_summ <- t(apply(X = nonconst_supp_t_gam_initJuly28_ages[,1:ncol(nonconst_supp_t_gam_initJuly28_ages)], 
                                                       MARGIN = 2, 
                                                       FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(nonconst_supp_t_gam_initJuly28_ages_summ)[1:2] <- c("mean", "sd")

nonconst_supp_t_gam_initJuly28_accurate_summ <- t(apply(X = nonconst_supp_t_gam_initJuly28_accurate[,1:ncol(nonconst_supp_t_gam_initJuly28_accurate)], 
                                                           MARGIN = 2, 
                                                           FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(nonconst_supp_t_gam_initJuly28_accurate_summ)[1:2] <- c("mean", "sd")
write.csv(nonconst_supp_t_gam_initJuly28_ages_summ, "nonconst_supp_t_gam_initJuly28_ages_summ.csv", row.names = F)
write.csv(nonconst_supp_t_gam_initJuly28_accurate_summ, "nonconst_supp_t_gam_initJuly28_accurate_summ.csv", row.names = F)

plot(nonconst_supp_t_gam_initJuly28, pars = "ages")
plot(nonconst_supp_t_gam_initJuly28, pars = "m")
plot(nonconst_supp_t_gam_initJuly28, pars = "alpha")
plot(nonconst_supp_t_gam_initJuly28, pars = "omega")
plot(nonconst_supp_t_gam_initJuly28, plotfun = "hist", pars = "phi")
plot(nonconst_supp_t_gam_initJuly28, plotfun = "hist", pars = "Ps")
plot(nonconst_supp_t_gam_initJuly28, plotfun = "hist", pars = "omega")
plot(nonconst_supp_t_gam_initJuly28, plotfun = "hist", pars = "ages")
plot(nonconst_supp_t_gam_initJuly28, plotfun = "hist", pars = "ages", binwidth = 0.5)
plot(nonconst_supp_t_gam_initJuly28, plotfun = "hist", pars = "alpha")
plot(nonconst_supp_t_gam_initJuly28, plotfun = "hist", pars = "m")
plot(nonconst_supp_t_gam_initJuly28, plotfun = "hist", pars = "mN")
plot(nonconst_supp_t_gam_initJuly28, plotfun = "trace", pars = "omega")
plot(nonconst_supp_t_gam_initJuly28, plotfun = "trace", pars = "phi")
plot(nonconst_supp_t_gam_initJuly28, plotfun = "trace", pars = "Ps")
plot(nonconst_supp_t_gam_initJuly28, plotfun = "trace", pars = "ages")
plot(nonconst_supp_t_gam_initJuly28, plotfun = "trace", pars = "mN")
plot(nonconst_supp_t_gam_initJuly28, plotfun = "trace", pars = "alpha")


#t nonconst with gamma

N <- 30
M <- 31
L <- 30
Tot <- N+L
delta <- 1
x <- 1:30
xL <- 1:30
rho <- siml_coredens(1:30)
rhoL <- siml_coredens(1:30)
sigma <- sigmas[1:30]
sigmaL <- sigmas[1:30]
p <- siml_conc2[1:30]
pL <- siml_supp2[1:30]
lambda <- 0.03114 
#omega <- 0.5
#priors
#yearly accumulation
alpha_phi <- 1
beta_phi <- 0.007
#memory parameter #try beta(1,5)
a_omega <- 1.1
b_omega <- 1.2
#plot(0, 0, xlim = c(0, 1), ylim = c(0, 1), type = "n")
#curve(dbeta(x, shape1 = 1.1, shape2 = 1.2), from = 0, to = 1, add = TRUE)
#supported activity
alpha_P <- 1
beta_P <- 0.05
#accumulation rate/gamma noise
alpha_a <- 1
beta_a <- 0.15
#plot(0, 0, xlim = c(0, 50), ylim = c(0, 0.4), type = "n")
#curve(dgamma(x, shape = 1, rate = 0.2), from = 0, to = 50, add = TRUE)

nonconst_supp_gam_siml_dat <- list(N = N, L = L, Tot = Tot, x = x, xL = xL, delta = delta, M=M,
                                   rho = rho, rhoL = rhoL, a = 3, b = 4, sigma = sigma, sigmaL = sigmaL, 
                                   p = p, pL = pL, lambda = lambda,  alpha_phi = alpha_phi, 
                                   beta_phi = beta_phi, a_omega = a_omega, b_omega = b_omega, alpha_P = alpha_P, 
                                   beta_P = beta_P, alpha_a = alpha_a, beta_a = beta_a)

nonconst_supp_nonconst_t_gam_initJuly28 <- stan(file = 'Pb210_nonconst_tdistr.stan', data = nonconst_supp_gam_siml_dat, 
                                                   chains = 4, iter = 200000, control = list(adapt_delta = 0.99))
saveRDS(nonconst_supp_nonconst_t_gam_initJuly28, "nonconst_supp_nonconst_t_gam_initJuly28.rds")

#save to .csv
nonconst_supp_nonconst_t_gam_initJuly28_list <- extract(nonconst_supp_nonconst_t_gam_initJuly28)
nonconst_supp_nonconst_t_gam_initJuly28_ages <- data.frame(nonconst_supp_nonconst_t_gam_initJuly28_list$ages)
colnames(nonconst_supp_nonconst_t_gam_initJuly28_ages) <- paste0("Age", 1:ncol(nonconst_supp_nonconst_t_gam_initJuly28_ages))
nonconst_supp_nonconst_t_gam_initJuly28_accurate <- data.frame(nonconst_supp_nonconst_t_gam_initJuly28_list$m)
colnames(nonconst_supp_nonconst_t_gam_initJuly28_accurate) <- paste0("m", 1:ncol(nonconst_supp_nonconst_t_gam_initJuly28_accurate))
nonconst_supp_nonconst_t_gam_initJuly28_param <- data.frame(cbind(nonconst_supp_nonconst_t_gam_initJuly28_list$phi, 
                                                                     nonconst_supp_nonconst_t_gam_initJuly28_list$Ps,
                                                                     nonconst_supp_nonconst_t_gam_initJuly28_list$omega))
colnames(nonconst_supp_nonconst_t_gam_initJuly28_param) <- c("Supply", "SuppAct", "Memory")
write.csv(nonconst_supp_nonconst_t_gam_initJuly28_ages, "nonconst_supp_nonconst_t_gam_initJuly28_ages.csv")
write.csv(nonconst_supp_nonconst_t_gam_initJuly28_accurate, "nonconst_supp_nonconst_t_gam_initJuly28_accurate.csv")
write.csv(nonconst_supp_nonconst_t_gam_initJuly28_param, "nonconst_supp_nonconst_t_gam_initJuly28_param.csv")

#save summaries to .csv
nonconst_supp_nonconst_t_gam_initJuly28_ages_summ <- t(apply(X = nonconst_supp_nonconst_t_gam_initJuly28_ages[,1:ncol(nonconst_supp_nonconst_t_gam_initJuly28_ages)], 
                                                                MARGIN = 2, 
                                                                FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(nonconst_supp_nonconst_t_gam_initJuly28_ages_summ)[1:2] <- c("mean", "sd")

nonconst_supp_nonconst_t_gam_initJuly28_accurate_summ <- t(apply(X = nonconst_supp_nonconst_t_gam_initJuly28_accurate[,1:ncol(nonconst_supp_nonconst_t_gam_initJuly28_accurate)], 
                                                                    MARGIN = 2, 
                                                                    FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(nonconst_supp_nonconst_t_gam_initJuly28_accurate_summ)[1:2] <- c("mean", "sd")
write.csv(nonconst_supp_nonconst_t_gam_initJuly28_ages_summ, "nonconst_supp_nonconst_t_gam_initJuly28_ages_summ.csv", row.names = F)
write.csv(nonconst_supp_nonconst_t_gam_initJuly28_accurate_summ, "nonconst_supp_nonconst_t_gam_initJuly28_accurate_summ.csv", row.names = F)

plot(nonconst_supp_nonconst_t_gam_initJuly28, pars = "ages")
plot(nonconst_supp_nonconst_t_gam_initJuly28, pars = "m")
plot(nonconst_supp_nonconst_t_gam_initJuly28, pars = "alpha")
plot(nonconst_supp_nonconst_t_gam_initJuly28, pars = "omega")
plot(nonconst_supp_nonconst_t_gam_initJuly28, plotfun = "hist", pars = "phi")
plot(nonconst_supp_nonconst_t_gam_initJuly28, plotfun = "hist", pars = "Ps")
plot(nonconst_supp_nonconst_t_gam_initJuly28, plotfun = "hist", pars = "omega")
plot(nonconst_supp_nonconst_t_gam_initJuly28, plotfun = "hist", pars = "ages")
plot(nonconst_supp_nonconst_t_gam_initJuly28, plotfun = "hist", pars = "ages", binwidth = 0.5)
plot(nonconst_supp_nonconst_t_gam_initJuly28, plotfun = "hist", pars = "alpha")
plot(nonconst_supp_nonconst_t_gam_initJuly28, plotfun = "hist", pars = "m")
plot(nonconst_supp_nonconst_t_gam_initJuly28, plotfun = "hist", pars = "mN")
plot(nonconst_supp_nonconst_t_gam_initJuly28, plotfun = "trace", pars = "omega")
plot(nonconst_supp_nonconst_t_gam_initJuly28, plotfun = "trace", pars = "phi")
plot(nonconst_supp_nonconst_t_gam_initJuly28, plotfun = "trace", pars = "Ps")
plot(nonconst_supp_nonconst_t_gam_initJuly28, plotfun = "trace", pars = "ages")
plot(nonconst_supp_nonconst_t_gam_initJuly28, plotfun = "trace", pars = "mN")
plot(nonconst_supp_nonconst_t_gam_initJuly28, plotfun = "trace", pars = "alpha")


###constant supply
#normal alpha
N <- 27
M <- 28
L <- 3
delta <- 1
x <- 1:27
xL <- 28:30
rho <- siml_coredens(1:27)
rhoL <- siml_coredens(28:30)
sigma <- sigmas[1:27]
sigmaL <- sigmas[28:30]
p <- siml_conc3[1:27]
pL <- siml_conc3[28:30]
lambda <- 0.03114 
#omega <- 0.5
#priors
#yearly accumulation
alpha_phi <- 1
beta_phi <- 0.007
#memory parameter #try beta(1,5)
a_omega <- 1.1
b_omega <- 1.2
#plot(0, 0, xlim = c(0, 1), ylim = c(0, 5), type = "n")
#curve(dbeta(x, shape1 = 2*.11, shape2 = 2*.9), from = 0, to = 1, add = TRUE)
#supported activity
alpha_P <- 1
beta_P <- 0.05
#accumulation rate/gamma noise
alpha_a <- 1
beta_a <- 0.15
#plot(0, 0, xlim = c(0, 50), ylim = c(0, 0.4), type = "n")
#curve(dgamma(x, shape =  3, rate = .9), from = 0, to = 50, add = TRUE)

const_supp_alph_siml_dat <- list(N = N, L = L, x = x, xL = xL, delta = delta, M=M,
                                    rho = rho, rhoL = rhoL, sigma = sigma, sigmaL = sigmaL, 
                                    p = p, pL = pL, lambda = lambda,  alpha_phi = alpha_phi, 
                                    beta_phi = beta_phi, a_omega = a_omega, b_omega = b_omega, alpha_P = alpha_P, 
                                    beta_P = beta_P, alpha_a = alpha_a, beta_a = beta_a)

const_supp_norm_alph_initJuly28 <- stan(file = 'Pb210_draft.stan', data = const_supp_alph_siml_dat, 
                                           chains = 4, iter = 200000, control = list(adapt_delta = 0.99))
saveRDS(const_supp_norm_alph_initJuly28, "const_supp_norm_alph_initJuly28.rds")

#save to .csv
const_supp_norm_alph_initJuly28_list <- extract(const_supp_norm_alph_initJuly28)
const_supp_norm_alph_initJuly28_ages <- data.frame(const_supp_norm_alph_initJuly28_list$ages)
colnames(const_supp_norm_alph_initJuly28_ages) <- paste0("Age", 1:ncol(const_supp_norm_alph_initJuly28_ages))
const_supp_norm_alph_initJuly28_accurate <- data.frame(const_supp_norm_alph_initJuly28_list$m)
colnames(const_supp_norm_alph_initJuly28_accurate) <- paste0("m", 1:ncol(const_supp_norm_alph_initJuly28_accurate))
const_supp_norm_alph_initJuly28_param <- data.frame(cbind(const_supp_norm_alph_initJuly28_list$phi, 
                                                             const_supp_norm_alph_initJuly28_list$Ps,
                                                             const_supp_norm_alph_initJuly28_list$omega))
colnames(const_supp_norm_alph_initJuly28_param) <- c("Supply", "SuppAct", "Memory")
write.csv(const_supp_norm_alph_initJuly28_ages, "const_supp_norm_alph_initJuly28_ages.csv")
write.csv(const_supp_norm_alph_initJuly28_accurate, "const_supp_norm_alph_initJuly28_accurate.csv")
write.csv(const_supp_norm_alph_initJuly28_param, "const_supp_norm_alph_initJuly28_param.csv")

#save summaries to .csv
const_supp_norm_alph_initJuly28_ages_summ <- t(apply(X = const_supp_norm_alph_initJuly28_ages[,1:ncol(const_supp_norm_alph_initJuly28_ages)], 
                                                        MARGIN = 2, 
                                                        FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(const_supp_norm_alph_initJuly28_ages_summ)[1:2] <- c("mean", "sd")

const_supp_norm_alph_initJuly28_accurate_summ <- t(apply(X = const_supp_norm_alph_initJuly28_accurate[,1:ncol(const_supp_norm_alph_initJuly28_accurate)], 
                                                            MARGIN = 2, 
                                                            FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(const_supp_norm_alph_initJuly28_accurate_summ)[1:2] <- c("mean", "sd")
write.csv(const_supp_norm_alph_initJuly28_ages_summ, "const_supp_norm_alph_initJuly28_ages_summ.csv", row.names = F)
write.csv(const_supp_norm_alph_initJuly28_accurate_summ, "const_supp_norm_alph_initJuly28_accurate_summ.csv", row.names = F)

plot(const_supp_norm_alph_initJuly28, pars = "ages")
plot(const_supp_norm_alph_initJuly28, pars = "m")
plot(const_supp_norm_alph_initJuly28, pars = "alpha")
plot(const_supp_norm_alph_initJuly28, pars = "omega")
plot(const_supp_norm_alph_initJuly28, plotfun = "hist", pars = "phi")
plot(const_supp_norm_alph_initJuly28, plotfun = "hist", pars = "Ps")
plot(const_supp_norm_alph_initJuly28, plotfun = "hist", pars = "omega")
plot(const_supp_norm_alph_initJuly28, plotfun = "hist", pars = "ages")
plot(const_supp_norm_alph_initJuly28, plotfun = "hist", pars = "ages", binwidth = 0.5)
plot(const_supp_norm_alph_initJuly28, plotfun = "hist", pars = "alpha")
plot(const_supp_norm_alph_initJuly28, plotfun = "hist", pars = "m")
plot(const_supp_norm_alph_initJuly28, plotfun = "hist", pars = "mN")
plot(const_supp_norm_alph_initJuly28, plotfun = "trace", pars = "omega")
plot(const_supp_norm_alph_initJuly28, plotfun = "trace", pars = "phi")
plot(const_supp_norm_alph_initJuly28, plotfun = "trace", pars = "Ps")
plot(const_supp_norm_alph_initJuly28, plotfun = "trace", pars = "ages")
plot(const_supp_norm_alph_initJuly28, plotfun = "trace", pars = "mN")
plot(const_supp_norm_alph_initJuly28, plotfun = "trace", pars = "alpha")

#normal with gamma

N <- 30
M <- 31
L <- 30
delta <- 1
x <- 1:30
xL <- 1:30
rho <- siml_coredens(1:30)
rhoL <- siml_coredens(1:30)
sigma <- sigmas[1:30]
sigmaL <- sigmas[1:30]
p <- siml_conc3[1:30]
pL <- siml_supp3[1:30]
lambda <- 0.03114 
#omega <- 0.5
#priors
#yearly accumulation
alpha_phi <- 1
beta_phi <- 0.007
#memory parameter #try beta(1,5)
a_omega <- 1.1
b_omega <- 1.2
#plot(0, 0, xlim = c(0, 1), ylim = c(0, 1), type = "n")
#curve(dbeta(x, shape1 = 1.1, shape2 = 1.2), from = 0, to = 1, add = TRUE)
#supported activity
alpha_P <- 1
beta_P <- 0.05
#accumulation rate/gamma noise
alpha_a <- 1
beta_a <- 0.15
#plot(0, 0, xlim = c(0, 50), ylim = c(0, 0.4), type = "n")
#curve(dgamma(x, shape = 1, rate = 0.2), from = 0, to = 50, add = TRUE)

const_supp_gam_siml_dat <- list(N = N, L = L, x = x, xL = xL, delta = delta, M=M,
                                   rho = rho, rhoL = rhoL, sigma = sigma, sigmaL = sigmaL, 
                                   p = p, pL = pL, lambda = lambda,  alpha_phi = alpha_phi, 
                                   beta_phi = beta_phi, a_omega = a_omega, b_omega = b_omega, alpha_P = alpha_P, 
                                   beta_P = beta_P, alpha_a = alpha_a, beta_a = beta_a)

const_supp_norm_gam_initJuly28 <- stan(file = 'Pb210_draft.stan', data = const_supp_gam_siml_dat, 
                                          chains = 4, iter = 200000, control = list(adapt_delta = 0.99))
saveRDS(const_supp_norm_gam_initJuly28, "const_supp_norm_gam_initJuly28.rds")

#save to .csv
const_supp_norm_gam_initJuly28_list <- extract(const_supp_norm_gam_initJuly28)
const_supp_norm_gam_initJuly28_ages <- data.frame(const_supp_norm_gam_initJuly28_list$ages)
colnames(const_supp_norm_gam_initJuly28_ages) <- paste0("Age", 1:ncol(const_supp_norm_gam_initJuly28_ages))
const_supp_norm_gam_initJuly28_accurate <- data.frame(const_supp_norm_gam_initJuly28_list$m)
colnames(const_supp_norm_gam_initJuly28_accurate) <- paste0("m", 1:ncol(const_supp_norm_gam_initJuly28_accurate))
const_supp_norm_gam_initJuly28_param <- data.frame(cbind(const_supp_norm_gam_initJuly28_list$phi, 
                                                            const_supp_norm_gam_initJuly28_list$Ps,
                                                            const_supp_norm_gam_initJuly28_list$omega))
colnames(const_supp_norm_gam_initJuly28_param) <- c("Supply", "SuppAct", "Memory")
write.csv(const_supp_norm_gam_initJuly28_ages, "const_supp_norm_gam_initJuly28_ages.csv")
write.csv(const_supp_norm_gam_initJuly28_accurate, "const_supp_norm_gam_initJuly28_accurate.csv")
write.csv(const_supp_norm_gam_initJuly28_param, "const_supp_norm_gam_initJuly28_param.csv")

#save summaries to .csv
const_supp_norm_gam_initJuly28_ages_summ <- t(apply(X = const_supp_norm_gam_initJuly28_ages[,1:ncol(const_supp_norm_gam_initJuly28_ages)], 
                                                       MARGIN = 2, 
                                                       FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(const_supp_norm_gam_initJuly28_ages_summ)[1:2] <- c("mean", "sd")

const_supp_norm_gam_initJuly28_accurate_summ <- t(apply(X = const_supp_norm_gam_initJuly28_accurate[,1:ncol(const_supp_norm_gam_initJuly28_accurate)], 
                                                           MARGIN = 2, 
                                                           FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(const_supp_norm_gam_initJuly28_accurate_summ)[1:2] <- c("mean", "sd")
write.csv(const_supp_norm_gam_initJuly28_ages_summ, "const_supp_norm_gam_initJuly28_ages_summ.csv", row.names = F)
write.csv(const_supp_norm_gam_initJuly28_accurate_summ, "const_supp_norm_gam_initJuly28_accurate_summ.csv", row.names = F)

plot(const_supp_norm_gam_initJuly28, pars = "ages")
plot(const_supp_norm_gam_initJuly28, pars = "m")
plot(const_supp_norm_gam_initJuly28, pars = "alpha")
plot(const_supp_norm_gam_initJuly28, pars = "omega")
plot(const_supp_norm_gam_initJuly28, plotfun = "hist", pars = "phi")
plot(const_supp_norm_gam_initJuly28, plotfun = "hist", pars = "Ps")
plot(const_supp_norm_gam_initJuly28, plotfun = "hist", pars = "omega")
plot(const_supp_norm_gam_initJuly28, plotfun = "hist", pars = "ages")
plot(const_supp_norm_gam_initJuly28, plotfun = "hist", pars = "ages", binwidth = 0.5)
plot(const_supp_norm_gam_initJuly28, plotfun = "hist", pars = "alpha")
plot(const_supp_norm_gam_initJuly28, plotfun = "hist", pars = "m")
plot(const_supp_norm_gam_initJuly28, plotfun = "hist", pars = "mN")
plot(const_supp_norm_gam_initJuly28, plotfun = "trace", pars = "omega")
plot(const_supp_norm_gam_initJuly28, plotfun = "trace", pars = "phi")
plot(const_supp_norm_gam_initJuly28, plotfun = "trace", pars = "Ps")
plot(const_supp_norm_gam_initJuly28, plotfun = "trace", pars = "ages")
plot(const_supp_norm_gam_initJuly28, plotfun = "trace", pars = "mN")
plot(const_supp_norm_gam_initJuly28, plotfun = "trace", pars = "alpha")


#normal nonconst with gamma

N <- 30
M <- 31
L <- 30
delta <- 1
x <- 1:30
xL <- 1:30
rho <- siml_coredens(1:30)
rhoL <- siml_coredens(1:30)
sigma <- sigmas[1:30]
sigmaL <- sigmas[1:30]
p <- siml_conc3[1:30]
pL <- siml_supp3[1:30]
lambda <- 0.03114 
#omega <- 0.5
#priors
#yearly accumulation
alpha_phi <- 1
beta_phi <- 0.007
#memory parameter #try beta(1,5)
a_omega <- 1.1
b_omega <- 1.2
#plot(0, 0, xlim = c(0, 1), ylim = c(0, 1), type = "n")
#curve(dbeta(x, shape1 = 1.1, shape2 = 1.2), from = 0, to = 1, add = TRUE)
#supported activity
alpha_P <- 1
beta_P <- 0.05
#accumulation rate/gamma noise
alpha_a <- 1
beta_a <- 0.15
#plot(0, 0, xlim = c(0, 50), ylim = c(0, 0.4), type = "n")
#curve(dgamma(x, shape = 1, rate = 0.2), from = 0, to = 50, add = TRUE)

const_supp_gam_siml_dat <- list(N = N, L = L, x = x, xL = xL, delta = delta, M=M,
                                   rho = rho, rhoL = rhoL, sigma = sigma, sigmaL = sigmaL, 
                                   p = p, pL = pL, lambda = lambda,  alpha_phi = alpha_phi, 
                                   beta_phi = beta_phi, a_omega = a_omega, b_omega = b_omega, alpha_P = alpha_P, 
                                   beta_P = beta_P, alpha_a = alpha_a, beta_a = beta_a)

const_supp_nonconst_norm_gam_initJuly28 <- stan(file = 'Pb210_nonconst_supp.stan', data = const_supp_gam_siml_dat, 
                                                   chains = 4, iter = 200000, control = list(adapt_delta = 0.99))
saveRDS(const_supp_nonconst_norm_gam_initJuly28, "const_supp_nonconst_norm_gam_initJuly28.rds")

#save to .csv
const_supp_nonconst_norm_gam_initJuly28_list <- extract(const_supp_nonconst_norm_gam_initJuly28)
const_supp_nonconst_norm_gam_initJuly28_ages <- data.frame(const_supp_nonconst_norm_gam_initJuly28_list$ages)
colnames(const_supp_nonconst_norm_gam_initJuly28_ages) <- paste0("Age", 1:ncol(const_supp_nonconst_norm_gam_initJuly28_ages))
const_supp_nonconst_norm_gam_initJuly28_accurate <- data.frame(const_supp_nonconst_norm_gam_initJuly28_list$m)
colnames(const_supp_nonconst_norm_gam_initJuly28_accurate) <- paste0("m", 1:ncol(const_supp_nonconst_norm_gam_initJuly28_accurate))
const_supp_nonconst_norm_gam_initJuly28_param <- data.frame(cbind(const_supp_nonconst_norm_gam_initJuly28_list$phi, 
                                                                     const_supp_nonconst_norm_gam_initJuly28_list$Ps,
                                                                     const_supp_nonconst_norm_gam_initJuly28_list$omega))
colnames(const_supp_nonconst_norm_gam_initJuly28_param) <- c("Supply", "SuppAct", "Memory")
write.csv(const_supp_nonconst_norm_gam_initJuly28_ages, "const_supp_nonconst_norm_gam_initJuly28_ages.csv")
write.csv(const_supp_nonconst_norm_gam_initJuly28_accurate, "const_supp_nonconst_norm_gam_initJuly28_accurate.csv")
write.csv(const_supp_nonconst_norm_gam_initJuly28_param, "const_supp_nonconst_norm_gam_initJuly28_param.csv")

#save summaries to .csv
const_supp_nonconst_norm_gam_initJuly28_ages_summ <- t(apply(X = const_supp_nonconst_norm_gam_initJuly28_ages[,1:ncol(const_supp_nonconst_norm_gam_initJuly28_ages)], 
                                                                MARGIN = 2, 
                                                                FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(const_supp_nonconst_norm_gam_initJuly28_ages_summ)[1:2] <- c("mean", "sd")

const_supp_nonconst_norm_gam_initJuly28_accurate_summ <- t(apply(X = const_supp_nonconst_norm_gam_initJuly28_accurate[,1:ncol(const_supp_nonconst_norm_gam_initJuly28_accurate)], 
                                                                    MARGIN = 2, 
                                                                    FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(const_supp_nonconst_norm_gam_initJuly28_accurate_summ)[1:2] <- c("mean", "sd")
write.csv(const_supp_nonconst_norm_gam_initJuly28_ages_summ, "const_supp_nonconst_norm_gam_initJuly28_ages_summ.csv", row.names = F)
write.csv(const_supp_nonconst_norm_gam_initJuly28_accurate_summ, "const_supp_nonconst_norm_gam_initJuly28_accurate_summ.csv", row.names = F)

plot(const_supp_nonconst_norm_gam_initJuly28, pars = "ages")
plot(const_supp_nonconst_norm_gam_initJuly28, pars = "m")
plot(const_supp_nonconst_norm_gam_initJuly28, pars = "alpha")
plot(const_supp_nonconst_norm_gam_initJuly28, pars = "omega")
plot(const_supp_nonconst_norm_gam_initJuly28, plotfun = "hist", pars = "phi")
plot(const_supp_nonconst_norm_gam_initJuly28, plotfun = "hist", pars = "Ps")
plot(const_supp_nonconst_norm_gam_initJuly28, plotfun = "hist", pars = "omega")
plot(const_supp_nonconst_norm_gam_initJuly28, plotfun = "hist", pars = "ages")
plot(const_supp_nonconst_norm_gam_initJuly28, plotfun = "hist", pars = "ages", binwidth = 0.5)
plot(const_supp_nonconst_norm_gam_initJuly28, plotfun = "hist", pars = "alpha")
plot(const_supp_nonconst_norm_gam_initJuly28, plotfun = "hist", pars = "m")
plot(const_supp_nonconst_norm_gam_initJuly28, plotfun = "hist", pars = "mN")
plot(const_supp_nonconst_norm_gam_initJuly28, plotfun = "trace", pars = "omega")
plot(const_supp_nonconst_norm_gam_initJuly28, plotfun = "trace", pars = "phi")
plot(const_supp_nonconst_norm_gam_initJuly28, plotfun = "trace", pars = "Ps")
plot(const_supp_nonconst_norm_gam_initJuly28, plotfun = "trace", pars = "ages")
plot(const_supp_nonconst_norm_gam_initJuly28, plotfun = "trace", pars = "mN")
plot(const_supp_nonconst_norm_gam_initJuly28, plotfun = "trace", pars = "alpha")

##t distribution
#t alpha
N <- 27
M <- 28
L <- 3
Tot <- N+L
delta <- 1
x <- 1:27
xL <- 28:30
rho <- siml_coredens(1:27)
rhoL <- siml_coredens(28:30)
sigma <- sigmas[1:27]
sigmaL <- sigmas[28:30]
p <- siml_conc3[1:27]
pL <- siml_conc3[28:30]
lambda <- 0.03114 
#omega <- 0.5
#priors
#yearly accumulation
alpha_phi <- 1
beta_phi <- 0.007
#memory parameter #try beta(1,5)
a_omega <- 1.1
b_omega <- 1.2
#plot(0, 0, xlim = c(0, 1), ylim = c(0, 5), type = "n")
#curve(dbeta(x, shape1 = 2*.11, shape2 = 2*.9), from = 0, to = 1, add = TRUE)
#supported activity
alpha_P <- 1
beta_P <- 0.05
#accumulation rate/gamma noise
alpha_a <- 1
beta_a <- 0.15
#plot(0, 0, xlim = c(0, 50), ylim = c(0, 0.4), type = "n")
#curve(dgamma(x, shape =  3, rate = .9), from = 0, to = 50, add = TRUE)

const_supp_alph_siml_dat <- list(N = N, L = L, Tot = Tot, x = x, xL = xL, delta = delta, M=M,
                                 rho = rho, rhoL = rhoL, a = 3, b = 4, sigma = sigma, sigmaL = sigmaL, 
                                 p = p, pL = pL, lambda = lambda,  alpha_phi = alpha_phi, 
                                 beta_phi = beta_phi, a_omega = a_omega, b_omega = b_omega, alpha_P = alpha_P, 
                                 beta_P = beta_P, alpha_a = alpha_a, beta_a = beta_a)

const_supp_t_alph_initJuly28 <- stan(file = 'Pb210_tdistr.stan', data = const_supp_alph_siml_dat, 
                                           chains = 4, iter = 200000, control = list(adapt_delta = 0.99))
saveRDS(const_supp_t_alph_initJuly28, "const_supp_t_alph_initJuly28.rds")

#save to .csv
const_supp_t_alph_initJuly28_list <- extract(const_supp_t_alph_initJuly28)
const_supp_t_alph_initJuly28_ages <- data.frame(const_supp_t_alph_initJuly28_list$ages)
colnames(const_supp_t_alph_initJuly28_ages) <- paste0("Age", 1:ncol(const_supp_t_alph_initJuly28_ages))
const_supp_t_alph_initJuly28_accurate <- data.frame(const_supp_t_alph_initJuly28_list$m)
colnames(const_supp_t_alph_initJuly28_accurate) <- paste0("m", 1:ncol(const_supp_t_alph_initJuly28_accurate))
const_supp_t_alph_initJuly28_param <- data.frame(cbind(const_supp_t_alph_initJuly28_list$phi, 
                                                             const_supp_t_alph_initJuly28_list$Ps,
                                                             const_supp_t_alph_initJuly28_list$omega))
colnames(const_supp_t_alph_initJuly28_param) <- c("Supply", "SuppAct", "Memory")
write.csv(const_supp_t_alph_initJuly28_ages, "const_supp_t_alph_initJuly28_ages.csv")
write.csv(const_supp_t_alph_initJuly28_accurate, "const_supp_t_alph_initJuly28_accurate.csv")
write.csv(const_supp_t_alph_initJuly28_param, "const_supp_t_alph_initJuly28_param.csv")

#save summaries to .csv
const_supp_t_alph_initJuly28_ages_summ <- t(apply(X = const_supp_t_alph_initJuly28_ages[,1:ncol(const_supp_t_alph_initJuly28_ages)], 
                                                        MARGIN = 2, 
                                                        FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(const_supp_t_alph_initJuly28_ages_summ)[1:2] <- c("mean", "sd")

const_supp_t_alph_initJuly28_accurate_summ <- t(apply(X = const_supp_t_alph_initJuly28_accurate[,1:ncol(const_supp_t_alph_initJuly28_accurate)], 
                                                            MARGIN = 2, 
                                                            FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(const_supp_t_alph_initJuly28_accurate_summ)[1:2] <- c("mean", "sd")
write.csv(const_supp_t_alph_initJuly28_ages_summ, "const_supp_t_alph_initJuly28_ages_summ.csv", row.names = F)
write.csv(const_supp_t_alph_initJuly28_accurate_summ, "const_supp_t_alph_initJuly28_accurate_summ.csv", row.names = F)

plot(const_supp_t_alph_initJuly28, pars = "ages")
plot(const_supp_t_alph_initJuly28, pars = "m")
plot(const_supp_t_alph_initJuly28, pars = "alpha")
plot(const_supp_t_alph_initJuly28, pars = "omega")
plot(const_supp_t_alph_initJuly28, plotfun = "hist", pars = "phi")
plot(const_supp_t_alph_initJuly28, plotfun = "hist", pars = "Ps")
plot(const_supp_t_alph_initJuly28, plotfun = "hist", pars = "omega")
plot(const_supp_t_alph_initJuly28, plotfun = "hist", pars = "ages")
plot(const_supp_t_alph_initJuly28, plotfun = "hist", pars = "ages", binwidth = 0.5)
plot(const_supp_t_alph_initJuly28, plotfun = "hist", pars = "alpha")
plot(const_supp_t_alph_initJuly28, plotfun = "hist", pars = "m")
plot(const_supp_t_alph_initJuly28, plotfun = "hist", pars = "mN")
plot(const_supp_t_alph_initJuly28, plotfun = "trace", pars = "omega")
plot(const_supp_t_alph_initJuly28, plotfun = "trace", pars = "phi")
plot(const_supp_t_alph_initJuly28, plotfun = "trace", pars = "Ps")
plot(const_supp_t_alph_initJuly28, plotfun = "trace", pars = "ages")
plot(const_supp_t_alph_initJuly28, plotfun = "trace", pars = "mN")
plot(const_supp_t_alph_initJuly28, plotfun = "trace", pars = "alpha")

#t with gamma

N <- 30
M <- 31
L <- 30
Tot <- N+L
delta <- 1
x <- 1:30
xL <- 1:30
rho <- siml_coredens(1:30)
rhoL <- siml_coredens(1:30)
sigma <- sigmas[1:30]
sigmaL <- sigmas[1:30]
p <- siml_conc3[1:30]
pL <- siml_supp3[1:30]
lambda <- 0.03114 
#omega <- 0.5
#priors
#yearly accumulation
alpha_phi <- 1
beta_phi <- 0.007
#memory parameter #try beta(1,5)
a_omega <- 1.1
b_omega <- 1.2
#plot(0, 0, xlim = c(0, 1), ylim = c(0, 1), type = "n")
#curve(dbeta(x, shape1 = 1.1, shape2 = 1.2), from = 0, to = 1, add = TRUE)
#supported activity
alpha_P <- 1
beta_P <- 0.05
#accumulation rate/gamma noise
alpha_a <- 1
beta_a <- 0.15
#plot(0, 0, xlim = c(0, 50), ylim = c(0, 0.4), type = "n")
#curve(dgamma(x, shape = 1, rate = 0.2), from = 0, to = 50, add = TRUE)

const_supp_gam_siml_dat <- list(N = N, L = L, Tot = Tot, x = x, xL = xL, delta = delta, M=M,
                                rho = rho, rhoL = rhoL, a = 3, b = 4, sigma = sigma, sigmaL = sigmaL, 
                                p = p, pL = pL, lambda = lambda,  alpha_phi = alpha_phi, 
                                beta_phi = beta_phi, a_omega = a_omega, b_omega = b_omega, alpha_P = alpha_P, 
                                beta_P = beta_P, alpha_a = alpha_a, beta_a = beta_a)

const_supp_t_gam_initJuly28 <- stan(file = 'Pb210_tdistr.stan', data = const_supp_gam_siml_dat, 
                                          chains = 4, iter = 200000, control = list(adapt_delta = 0.99))
saveRDS(const_supp_t_gam_initJuly28, "const_supp_t_gam_initJuly28.rds")

#save to .csv
const_supp_t_gam_initJuly28_list <- extract(const_supp_t_gam_initJuly28)
const_supp_t_gam_initJuly28_ages <- data.frame(const_supp_t_gam_initJuly28_list$ages)
colnames(const_supp_t_gam_initJuly28_ages) <- paste0("Age", 1:ncol(const_supp_t_gam_initJuly28_ages))
const_supp_t_gam_initJuly28_accurate <- data.frame(const_supp_t_gam_initJuly28_list$m)
colnames(const_supp_t_gam_initJuly28_accurate) <- paste0("m", 1:ncol(const_supp_t_gam_initJuly28_accurate))
const_supp_t_gam_initJuly28_param <- data.frame(cbind(const_supp_t_gam_initJuly28_list$phi, 
                                                            const_supp_t_gam_initJuly28_list$Ps,
                                                            const_supp_t_gam_initJuly28_list$omega))
colnames(const_supp_t_gam_initJuly28_param) <- c("Supply", "SuppAct", "Memory")
write.csv(const_supp_t_gam_initJuly28_ages, "const_supp_t_gam_initJuly28_ages.csv")
write.csv(const_supp_t_gam_initJuly28_accurate, "const_supp_t_gam_initJuly28_accurate.csv")
write.csv(const_supp_t_gam_initJuly28_param, "const_supp_t_gam_initJuly28_param.csv")

#save summaries to .csv
const_supp_t_gam_initJuly28_ages_summ <- t(apply(X = const_supp_t_gam_initJuly28_ages[,1:ncol(const_supp_t_gam_initJuly28_ages)], 
                                                       MARGIN = 2, 
                                                       FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(const_supp_t_gam_initJuly28_ages_summ)[1:2] <- c("mean", "sd")

const_supp_t_gam_initJuly28_accurate_summ <- t(apply(X = const_supp_t_gam_initJuly28_accurate[,1:ncol(const_supp_t_gam_initJuly28_accurate)], 
                                                           MARGIN = 2, 
                                                           FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(const_supp_t_gam_initJuly28_accurate_summ)[1:2] <- c("mean", "sd")
write.csv(const_supp_t_gam_initJuly28_ages_summ, "const_supp_t_gam_initJuly28_ages_summ.csv", row.names = F)
write.csv(const_supp_t_gam_initJuly28_accurate_summ, "const_supp_t_gam_initJuly28_accurate_summ.csv", row.names = F)

plot(const_supp_t_gam_initJuly28, pars = "ages")
plot(const_supp_t_gam_initJuly28, pars = "m")
plot(const_supp_t_gam_initJuly28, pars = "alpha")
plot(const_supp_t_gam_initJuly28, pars = "omega")
plot(const_supp_t_gam_initJuly28, plotfun = "hist", pars = "phi")
plot(const_supp_t_gam_initJuly28, plotfun = "hist", pars = "Ps")
plot(const_supp_t_gam_initJuly28, plotfun = "hist", pars = "omega")
plot(const_supp_t_gam_initJuly28, plotfun = "hist", pars = "ages")
plot(const_supp_t_gam_initJuly28, plotfun = "hist", pars = "ages", binwidth = 0.5)
plot(const_supp_t_gam_initJuly28, plotfun = "hist", pars = "alpha")
plot(const_supp_t_gam_initJuly28, plotfun = "hist", pars = "m")
plot(const_supp_t_gam_initJuly28, plotfun = "hist", pars = "mN")
plot(const_supp_t_gam_initJuly28, plotfun = "trace", pars = "omega")
plot(const_supp_t_gam_initJuly28, plotfun = "trace", pars = "phi")
plot(const_supp_t_gam_initJuly28, plotfun = "trace", pars = "Ps")
plot(const_supp_t_gam_initJuly28, plotfun = "trace", pars = "ages")
plot(const_supp_t_gam_initJuly28, plotfun = "trace", pars = "mN")
plot(const_supp_t_gam_initJuly28, plotfun = "trace", pars = "alpha")


#t nonconst with gamma

N <- 30
M <- 31
L <- 30
Tot <- N+L
delta <- 1
x <- 1:30
xL <- 1:30
rho <- siml_coredens(1:30)
rhoL <- siml_coredens(1:30)
sigma <- sigmas[1:30]
sigmaL <- sigmas[1:30]
p <- siml_conc3[1:30]
pL <- siml_supp3[1:30]
lambda <- 0.03114 
#omega <- 0.5
#priors
#yearly accumulation
alpha_phi <- 1
beta_phi <- 0.007
#memory parameter #try beta(1,5)
a_omega <- 1.1
b_omega <- 1.2
#plot(0, 0, xlim = c(0, 1), ylim = c(0, 1), type = "n")
#curve(dbeta(x, shape1 = 1.1, shape2 = 1.2), from = 0, to = 1, add = TRUE)
#supported activity
alpha_P <- 1
beta_P <- 0.05
#accumulation rate/gamma noise
alpha_a <- 1
beta_a <- 0.15
#plot(0, 0, xlim = c(0, 50), ylim = c(0, 0.4), type = "n")
#curve(dgamma(x, shape = 1, rate = 0.2), from = 0, to = 50, add = TRUE)

const_supp_gam_siml_dat <- list(N = N, L = L, Tot = Tot, x = x, xL = xL, delta = delta, M=M,
                                rho = rho, rhoL = rhoL, a = 3, b = 4, sigma = sigma, sigmaL = sigmaL, 
                                p = p, pL = pL, lambda = lambda,  alpha_phi = alpha_phi, 
                                beta_phi = beta_phi, a_omega = a_omega, b_omega = b_omega, alpha_P = alpha_P, 
                                beta_P = beta_P, alpha_a = alpha_a, beta_a = beta_a)

const_supp_nonconst_t_gam_initJuly28 <- stan(file = 'Pb210_nonconst_tdistr.stan', data = const_supp_gam_siml_dat, 
                                                   chains = 4, iter = 200000, control = list(adapt_delta = 0.99))
saveRDS(const_supp_nonconst_t_gam_initJuly28, "const_supp_nonconst_t_gam_initJuly28.rds")

#save to .csv
const_supp_nonconst_t_gam_initJuly28_list <- extract(const_supp_nonconst_t_gam_initJuly28)
const_supp_nonconst_t_gam_initJuly28_ages <- data.frame(const_supp_nonconst_t_gam_initJuly28_list$ages)
colnames(const_supp_nonconst_t_gam_initJuly28_ages) <- paste0("Age", 1:ncol(const_supp_nonconst_t_gam_initJuly28_ages))
const_supp_nonconst_t_gam_initJuly28_accurate <- data.frame(const_supp_nonconst_t_gam_initJuly28_list$m)
colnames(const_supp_nonconst_t_gam_initJuly28_accurate) <- paste0("m", 1:ncol(const_supp_nonconst_t_gam_initJuly28_accurate))
const_supp_nonconst_t_gam_initJuly28_param <- data.frame(cbind(const_supp_nonconst_t_gam_initJuly28_list$phi, 
                                                                     const_supp_nonconst_t_gam_initJuly28_list$Ps,
                                                                     const_supp_nonconst_t_gam_initJuly28_list$omega))
colnames(const_supp_nonconst_t_gam_initJuly28_param) <- c("Supply", "SuppAct", "Memory")
write.csv(const_supp_nonconst_t_gam_initJuly28_ages, "const_supp_nonconst_t_gam_initJuly28_ages.csv")
write.csv(const_supp_nonconst_t_gam_initJuly28_accurate, "const_supp_nonconst_t_gam_initJuly28_accurate.csv")
write.csv(const_supp_nonconst_t_gam_initJuly28_param, "const_supp_nonconst_t_gam_initJuly28_param.csv")

#save summaries to .csv
const_supp_nonconst_t_gam_initJuly28_ages_summ <- t(apply(X = const_supp_nonconst_t_gam_initJuly28_ages[,1:ncol(const_supp_nonconst_t_gam_initJuly28_ages)], 
                                                                MARGIN = 2, 
                                                                FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(const_supp_nonconst_t_gam_initJuly28_ages_summ)[1:2] <- c("mean", "sd")

const_supp_nonconst_t_gam_initJuly28_accurate_summ <- t(apply(X = const_supp_nonconst_t_gam_initJuly28_accurate[,1:ncol(const_supp_nonconst_t_gam_initJuly28_accurate)], 
                                                                    MARGIN = 2, 
                                                                    FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(const_supp_nonconst_t_gam_initJuly28_accurate_summ)[1:2] <- c("mean", "sd")
write.csv(const_supp_nonconst_t_gam_initJuly28_ages_summ, "const_supp_nonconst_t_gam_initJuly28_ages_summ.csv", row.names = F)
write.csv(const_supp_nonconst_t_gam_initJuly28_accurate_summ, "const_supp_nonconst_t_gam_initJuly28_accurate_summ.csv", row.names = F)

plot(const_supp_nonconst_t_gam_initJuly28, pars = "ages")
plot(const_supp_nonconst_t_gam_initJuly28, pars = "m")
plot(const_supp_nonconst_t_gam_initJuly28, pars = "accu_rates")
plot(const_supp_nonconst_t_gam_initJuly28, pars = "alpha")
plot(const_supp_nonconst_t_gam_initJuly28, pars = "omega")
plot(const_supp_nonconst_t_gam_initJuly28, plotfun = "hist", pars = "phi")
plot(const_supp_nonconst_t_gam_initJuly28, plotfun = "hist", pars = "Ps")
plot(const_supp_nonconst_t_gam_initJuly28, plotfun = "hist", pars = "omega")
plot(const_supp_nonconst_t_gam_initJuly28, plotfun = "hist", pars = "ages")
plot(const_supp_nonconst_t_gam_initJuly28, plotfun = "hist", pars = "ages", binwidth = 0.5)
plot(const_supp_nonconst_t_gam_initJuly28, plotfun = "hist", pars = "alpha")
plot(const_supp_nonconst_t_gam_initJuly28, plotfun = "hist", pars = "m")
plot(const_supp_nonconst_t_gam_initJuly28, plotfun = "hist", pars = "mN")
plot(const_supp_nonconst_t_gam_initJuly28, plotfun = "trace", pars = "omega")
plot(const_supp_nonconst_t_gam_initJuly28, plotfun = "trace", pars = "phi")
plot(const_supp_nonconst_t_gam_initJuly28, plotfun = "trace", pars = "Ps")
plot(const_supp_nonconst_t_gam_initJuly28, plotfun = "trace", pars = "ages")
plot(const_supp_nonconst_t_gam_initJuly28, plotfun = "trace", pars = "mN")
plot(const_supp_nonconst_t_gam_initJuly28, plotfun = "trace", pars = "alpha")

plot(ABOR1309_01_initJuly29nonconst, pars = "ages")
plot(ABOR1309_01_initJuly29nonconst, pars = "m")
plot(ABOR1309_01_initJuly29nonconst, plotfun = "hist", pars = "m")
plot(ABOR1309_01_initJuly29nonconst, plotfun = "hist", pars = "ages")

plot(CMOR1706_01_initJuly29nonconst, pars = "ages")
plot(CMOR1706_01_initJuly29nonconst, pars = "m")
plot(CMOR1706_01_initJuly29nonconst, plotfun = "hist", pars = "m")
plot(CMOR1706_01_initJuly29nonconst, plotfun = "hist", pars = "ages")


#######
depth <- 1:27
true_ages <- siml_age(1:27)
data.frame(cbind(const_supp_siml_1_initJuly9_ages_summ, depth, true_ages)) %>%
  select(X50., depth, true_ages, mean) %>%
  pivot_longer(cols = c("X50.", "true_ages", "mean")) %>%
  ggplot(aes(x = depth, y = value, col = name))+
  geom_point()

data.frame(cbind(const_supp_siml_2_initJuly9_ages_summ, depth, true_ages)) %>%
  select(depth, true_ages, mean) %>%
  pivot_longer(cols = c("true_ages", "mean"), names_to = "type", values_to = "age") %>%
  ggplot(aes(x = depth, y = age, col = type))+
  geom_point()

data.frame(cbind(const_supp_siml_3_initJuly9_ages_summ, depth, true_ages)) %>%
  select(depth, true_ages, mean) %>%
  pivot_longer(cols = c("true_ages", "mean"), names_to = "type", values_to = "age") %>%
  ggplot(aes(x = depth, y = age, col = type))+
  geom_point()

data.frame(cbind(const_supp_siml_4_initJuly9_ages_summ, depth, true_ages)) %>%
  select(depth, true_ages, mean) %>%
  pivot_longer(cols = c("true_ages", "mean"), names_to = "type", values_to = "age") %>%
  ggplot(aes(x = depth, y = age, col = type))+
  geom_point()

data.frame(cbind(const_supp_siml_5_initJuly9_ages_summ, depth, true_ages)) %>%
  select(depth, true_ages, mean, X2.5., X97.5.) %>%
  pivot_longer(cols = c("true_ages", "mean"), names_to = "type", values_to = "age") %>%
  ggplot(aes(x = depth, y = age, col = type))+
  geom_line()+
  geom_errorbar(aes(ymin = X2.5., ymax = X97.5.))

data.frame(cbind(const_supp_siml_4_initJuly9_ages_summ[,1], depth[1:27], true_ages[1:27], const_supp_siml_4_initJuly9_ages_summ[1:27,1])) %>%
  select(X1, X2, X3, X4) %>%
  pivot_longer(cols = c("X1", "X3", "X4"), names_to = "type", values_to = "age") %>%
  ggplot(aes(x = X2, y = age, col = type))+
  geom_point(alpha = 0.4)


check_hmc_diagnostics(const_supp_siml_bgar_1_initJuly28)
check_hmc_diagnostics(const_supp_siml_bgar_tdistr_1_initJuly28)
check_hmc_diagnostics(const_supp_siml_bgar_gam_1_initJuly28)
check_hmc_diagnostics(const_supp_siml_bgar_tdistr_gam_1_initJuly28)

summary(const_supp_siml_bgar_1_initJuly28)
summary(const_supp_siml_bgar_tdistr_1_initJuly28)
summary(const_supp_siml_bgar_gam_1_initJuly28)
summary(const_supp_siml_bgar_tdistr_gam_1_initJuly28)
