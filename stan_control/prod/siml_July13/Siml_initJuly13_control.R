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

#Simulation Set-Up: Use core_section_simulation_setup.R file
#like alpha spec setup v gamma spec set up

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
p <- siml_conc[1:27]
pL <- siml_conc[28:30]
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
alpha_a <- 2
beta_a <- 0.3
#plot(0, 0, xlim = c(0, 50), ylim = c(0, 0.4), type = "n")
#curve(dgamma(x, shape =  3, rate = .9), from = 0, to = 50, add = TRUE)

const_supp_siml_dat <- list(N = N, L = L, x = x, xL = xL, delta = delta, M=M,
                            rho = rho, rhoL = rhoL, sigma = sigma, sigmaL = sigmaL, 
                            p = p, pL = pL, lambda = lambda,  alpha_phi = alpha_phi, 
                            beta_phi = beta_phi, a_omega = a_omega, b_omega = b_omega, alpha_P = alpha_P, 
                            beta_P = beta_P, alpha_a = alpha_a, beta_a = beta_a)

const_supp_siml_2_initJuly13 <- stan(file = 'Pb210_draft.stan', data = const_supp_siml_dat, 
                                    chains = 4, iter = 20000, control = list(adapt_delta = 0.99))
saveRDS(const_supp_siml_2_initJuly13, "const_supp_siml_2_initJuly13.rds")

#save to .csv
const_supp_siml_2_initJuly13_list <- extract(const_supp_siml_2_initJuly13)
const_supp_siml_2_initJuly13_ages <- data.frame(const_supp_siml_2_initJuly13_list$ages)
colnames(const_supp_siml_2_initJuly13_ages) <- paste0("Age", 1:ncol(const_supp_siml_2_initJuly13_ages))
const_supp_siml_2_initJuly13_accurate <- data.frame(const_supp_siml_2_initJuly13_list$m)
colnames(const_supp_siml_2_initJuly13_accurate) <- paste0("m", 1:ncol(const_supp_siml_2_initJuly13_accurate))
const_supp_siml_2_initJuly13_param <- data.frame(cbind(const_supp_siml_2_initJuly13_list$phi, 
                                                      const_supp_siml_2_initJuly13_list$Ps,
                                                      const_supp_siml_2_initJuly13_list$omega))
colnames(const_supp_siml_2_initJuly13_param) <- c("Supply", "SuppAct", "Memory")
write.csv(const_supp_siml_2_initJuly13_ages, "const_supp_siml_2_initJuly13_ages.csv")
write.csv(const_supp_siml_2_initJuly13_accurate, "const_supp_siml_2_initJuly13_accurate.csv")
write.csv(const_supp_siml_2_initJuly13_param, "const_supp_siml_2_initJuly13_param.csv")

#save summaries to .csv
const_supp_siml_2_initJuly13_ages_summ <- t(apply(X = const_supp_siml_2_initJuly13_ages[,1:ncol(const_supp_siml_2_initJuly13_ages)], 
                                                 MARGIN = 2, 
                                                 FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(const_supp_siml_2_initJuly13_ages_summ)[1:2] <- c("mean", "sd")

const_supp_siml_2_initJuly13_accurate_summ <- t(apply(X = const_supp_siml_2_initJuly13_accurate[,1:ncol(const_supp_siml_2_initJuly13_accurate)], 
                                                     MARGIN = 2, 
                                                     FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(const_supp_siml_2_initJuly13_accurate_summ)[1:2] <- c("mean", "sd")
write.csv(const_supp_siml_2_initJuly13_ages_summ, "const_supp_siml_2_initJuly13_ages_summ.csv", row.names = F)
write.csv(const_supp_siml_2_initJuly13_accurate_summ, "const_supp_siml_2_initJuly13_accurate_summ.csv", row.names = F)

plot(const_supp_siml_2_initJuly13, pars = "ages")
plot(const_supp_siml_2_initJuly13, pars = "m")
plot(const_supp_siml_2_initJuly13, pars = "alpha")
plot(const_supp_siml_2_initJuly13, pars = "omega")
plot(const_supp_siml_2_initJuly13, plotfun = "hist", pars = "phi")
plot(const_supp_siml_2_initJuly13, plotfun = "hist", pars = "Ps")
plot(const_supp_siml_2_initJuly13, plotfun = "hist", pars = "omega")
plot(const_supp_siml_2_initJuly13, plotfun = "hist", pars = "ages")
plot(const_supp_siml_2_initJuly13, plotfun = "hist", pars = "ages", binwidth = 0.5)
plot(const_supp_siml_2_initJuly13, plotfun = "hist", pars = "alpha")
plot(const_supp_siml_2_initJuly13, plotfun = "hist", pars = "m")
plot(const_supp_siml_2_initJuly13, plotfun = "hist", pars = "mN")
plot(const_supp_siml_2_initJuly13, plotfun = "trace", pars = "omega")
plot(const_supp_siml_2_initJuly13, plotfun = "trace", pars = "phi")
plot(const_supp_siml_2_initJuly13, plotfun = "trace", pars = "Ps")
plot(const_supp_siml_2_initJuly13, plotfun = "trace", pars = "ages")
plot(const_supp_siml_2_initJuly13, plotfun = "trace", pars = "mN")
plot(const_supp_siml_2_initJuly13, plotfun = "trace", pars = "alpha")

#t distribution

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
p <- siml_conc[1:27]
pL <- siml_conc[28:30]
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
alpha_a <- 2
beta_a <- 0.3
#plot(0, 0, xlim = c(0, 50), ylim = c(0, 0.4), type = "n")
#curve(dgamma(x, shape = 1, rate = 0.2), from = 0, to = 50, add = TRUE)
a <- 3
b <- 4

t_distr_siml_dat <- list(N = N, L = L, Tot = Tot, x = x, xL = xL, delta = delta, M=M,
                          rho = rho, rhoL = rhoL, a = 3, b = 4, sigma = sigma, sigmaL = sigmaL, 
                          p = p, pL = pL, lambda = lambda,  alpha_phi = alpha_phi, 
                          beta_phi = beta_phi, a_omega = a_omega, b_omega = b_omega, alpha_P = alpha_P, 
                          beta_P = beta_P, alpha_a = alpha_a, beta_a = beta_a)


const_supp_siml_tdistr_2_initJuly13 <- stan(file = 'Pb210_tdistr.stan', data = t_distr_siml_dat, 
                                     chains = 4, iter = 20000, control = list(adapt_delta = 0.99))
saveRDS(const_supp_siml_tdistr_2_initJuly13, "const_supp_siml_tdistr_2_initJuly13.rds")

#save to .csv
const_supp_siml_tdistr_2_initJuly13_list <- extract(const_supp_siml_tdistr_2_initJuly13)
const_supp_siml_tdistr_2_initJuly13_ages <- data.frame(const_supp_siml_tdistr_2_initJuly13_list$ages)
colnames(const_supp_siml_tdistr_2_initJuly13_ages) <- paste0("Age", 1:ncol(const_supp_siml_tdistr_2_initJuly13_ages))
const_supp_siml_tdistr_2_initJuly13_accurate <- data.frame(const_supp_siml_tdistr_2_initJuly13_list$m)
colnames(const_supp_siml_tdistr_2_initJuly13_accurate) <- paste0("m", 1:ncol(const_supp_siml_tdistr_2_initJuly13_accurate))
const_supp_siml_tdistr_2_initJuly13_param <- data.frame(cbind(const_supp_siml_tdistr_2_initJuly13_list$phi, 
                                                       const_supp_siml_tdistr_2_initJuly13_list$Ps,
                                                       const_supp_siml_tdistr_2_initJuly13_list$omega))
colnames(const_supp_siml_tdistr_2_initJuly13_param) <- c("Supply", "SuppAct", "Memory")
write.csv(const_supp_siml_tdistr_2_initJuly13_ages, "const_supp_siml_tdistr_2_initJuly13_ages.csv")
write.csv(const_supp_siml_tdistr_2_initJuly13_accurate, "const_supp_siml_tdistr_2_initJuly13_accurate.csv")
write.csv(const_supp_siml_tdistr_2_initJuly13_param, "const_supp_siml_tdistr_2_initJuly13_param.csv")

#save summaries to .csv
const_supp_siml_tdistr_2_initJuly13_ages_summ <- t(apply(X = const_supp_siml_tdistr_2_initJuly13_ages[,1:ncol(const_supp_siml_tdistr_2_initJuly13_ages)], 
                                                  MARGIN = 2, 
                                                  FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(const_supp_siml_tdistr_2_initJuly13_ages_summ)[1:2] <- c("mean", "sd")

const_supp_siml_tdistr_2_initJuly13_accurate_summ <- t(apply(X = const_supp_siml_tdistr_2_initJuly13_accurate[,1:ncol(const_supp_siml_tdistr_2_initJuly13_accurate)], 
                                                      MARGIN = 2, 
                                                      FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(const_supp_siml_tdistr_2_initJuly13_accurate_summ)[1:2] <- c("mean", "sd")
write.csv(const_supp_siml_tdistr_2_initJuly13_ages_summ, "const_supp_siml_tdistr_2_initJuly13_ages_summ.csv", row.names = F)
write.csv(const_supp_siml_tdistr_2_initJuly13_accurate_summ, "const_supp_siml_tdistr_2_initJuly13_accurate_summ.csv", row.names = F)

plot(const_supp_siml_tdistr_2_initJuly13, pars = "ages")
plot(const_supp_siml_tdistr_2_initJuly13, pars = "m")
plot(const_supp_siml_tdistr_2_initJuly13, pars = "alpha")
plot(const_supp_siml_tdistr_2_initJuly13, pars = "omega")
plot(const_supp_siml_tdistr_2_initJuly13, plotfun = "hist", pars = "phi")
plot(const_supp_siml_tdistr_2_initJuly13, plotfun = "hist", pars = "Ps")
plot(const_supp_siml_tdistr_2_initJuly13, plotfun = "hist", pars = "omega")
plot(const_supp_siml_tdistr_2_initJuly13, plotfun = "hist", pars = "ages")
plot(const_supp_siml_tdistr_2_initJuly13, plotfun = "hist", pars = "ages", binwidth = 0.5)
plot(const_supp_siml_tdistr_2_initJuly13, plotfun = "hist", pars = "alpha")
plot(const_supp_siml_tdistr_2_initJuly13, plotfun = "hist", pars = "m")
plot(const_supp_siml_tdistr_2_initJuly13, plotfun = "hist", pars = "mN")
plot(const_supp_siml_tdistr_2_initJuly13, plotfun = "trace", pars = "omega")
plot(const_supp_siml_tdistr_2_initJuly13, plotfun = "trace", pars = "phi")
plot(const_supp_siml_tdistr_2_initJuly13, plotfun = "trace", pars = "Ps")
plot(const_supp_siml_tdistr_2_initJuly13, plotfun = "trace", pars = "ages")
plot(const_supp_siml_tdistr_2_initJuly13, plotfun = "trace", pars = "mN")
plot(const_supp_siml_tdistr_2_initJuly13, plotfun = "trace", pars = "alpha")


#bgar1

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
p <- siml_conc[1:27]
pL <- siml_conc[28:30]
lambda <- 0.03114 
#omega <- 0.5
#priors
#yearly accumulation
alpha_phi <- 1
beta_phi <- 0.007
#correlation parameter #try beta(1,5)
alpha_psi <- 0.1
beta_psi <- 0.9
#plot(0, 0, xlim = c(0, 1), ylim = c(0, 1), type = "n")
#curve(dbeta(x, shape1 = 0.1, shape2 = 0.9), from = 0, to = 1, add = TRUE)
#supported activity
alpha_P <- 1
beta_P <- 0.05
#accumulation rate/gamma noise
alpha_a <- 1
beta_a <- 0.15
#plot(0, 0, xlim = c(0, 50), ylim = c(0, 0.4), type = "n")
#curve(dgamma(x, shape = 1, rate = 0.2), from = 0, to = 50, add = TRUE)

bgar1_siml_dat <- list(N = N, L = L, x = x, xL = xL, delta = delta, M=M,
                          rho = rho, rhoL = rhoL, sigma = sigma, sigmaL = sigmaL, 
                          p = p, pL = pL, lambda = lambda,  alpha_phi = alpha_phi, 
                          beta_phi = beta_phi, alpha_psi = alpha_psi, beta_psi = beta_psi, 
                          alpha_P = alpha_P, beta_P = beta_P, alpha_a = alpha_a, beta_a = beta_a)

const_supp_siml_bgar_1_initJuly13 <- stan(file = 'Pb210_bgar1.stan', data = bgar1_siml_dat, 
                                   chains = 4, iter = 200000, control = list(adapt_delta = 0.99, max_treedepth = 10))

saveRDS(const_supp_siml_bgar_1_initJuly13, "const_supp_siml_bgar_1_initJuly13.rds")

#save to .csv
const_supp_siml_bgar_1_initJuly13_list <- extract(const_supp_siml_bgar_1_initJuly13)
const_supp_siml_bgar_1_initJuly13_ages <- data.frame(const_supp_siml_bgar_1_initJuly13_list$ages)
colnames(const_supp_siml_bgar_1_initJuly13_ages) <- paste0("Age", 1:ncol(const_supp_siml_bgar_1_initJuly13_ages))
const_supp_siml_bgar_1_initJuly13_accurate <- data.frame(const_supp_siml_bgar_1_initJuly13_list$m)
colnames(const_supp_siml_bgar_1_initJuly13_accurate) <- paste0("m", 1:ncol(const_supp_siml_bgar_1_initJuly13_accurate))
const_supp_siml_bgar_1_initJuly13_param <- data.frame(cbind(const_supp_siml_bgar_1_initJuly13_list$phi, 
                                                              const_supp_siml_bgar_1_initJuly13_list$Ps,
                                                              const_supp_siml_bgar_1_initJuly13_list$omega))
colnames(const_supp_siml_bgar_1_initJuly13_param) <- c("Supply", "SuppAct", "Memory")
write.csv(const_supp_siml_bgar_1_initJuly13_ages, "const_supp_siml_bgar_1_initJuly13_ages.csv")
write.csv(const_supp_siml_bgar_1_initJuly13_accurate, "const_supp_siml_bgar_1_initJuly13_accurate.csv")
write.csv(const_supp_siml_bgar_1_initJuly13_param, "const_supp_siml_bgar_1_initJuly13_param.csv")

#save summaries to .csv
const_supp_siml_bgar_1_initJuly13_ages_summ <- t(apply(X = const_supp_siml_bgar_1_initJuly13_ages[,1:ncol(const_supp_siml_bgar_1_initJuly13_ages)], 
                                                         MARGIN = 2, 
                                                         FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(const_supp_siml_bgar_1_initJuly13_ages_summ)[1:2] <- c("mean", "sd")

const_supp_siml_bgar_1_initJuly13_accurate_summ <- t(apply(X = const_supp_siml_bgar_1_initJuly13_accurate[,1:ncol(const_supp_siml_bgar_1_initJuly13_accurate)], 
                                                             MARGIN = 2, 
                                                             FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(const_supp_siml_bgar_1_initJuly13_accurate_summ)[1:2] <- c("mean", "sd")
write.csv(const_supp_siml_bgar_1_initJuly13_ages_summ, "const_supp_siml_bgar_1_initJuly13_ages_summ.csv", row.names = F)
write.csv(const_supp_siml_bgar_1_initJuly13_accurate_summ, "const_supp_siml_bgar_1_initJuly13_accurate_summ.csv", row.names = F)

plot(const_supp_siml_bgar_1_initJuly13, pars = "ages")
plot(const_supp_siml_bgar_1_initJuly13, pars = "m")
plot(const_supp_siml_bgar_1_initJuly13, pars = "alpha")
plot(const_supp_siml_bgar_1_initJuly13, pars = "omega")
plot(const_supp_siml_bgar_1_initJuly13, plotfun = "hist", pars = "phi")
plot(const_supp_siml_bgar_1_initJuly13, plotfun = "hist", pars = "Ps")
plot(const_supp_siml_bgar_1_initJuly13, plotfun = "hist", pars = "omega")
plot(const_supp_siml_bgar_1_initJuly13, plotfun = "hist", pars = "ages")
plot(const_supp_siml_bgar_1_initJuly13, plotfun = "hist", pars = "ages", binwidth = 0.5)
plot(const_supp_siml_bgar_1_initJuly13, plotfun = "hist", pars = "alpha")
plot(const_supp_siml_bgar_1_initJuly13, plotfun = "hist", pars = "m")
plot(const_supp_siml_bgar_1_initJuly13, plotfun = "hist", pars = "mN")
plot(const_supp_siml_bgar_1_initJuly13, plotfun = "trace", pars = "omega")
plot(const_supp_siml_bgar_1_initJuly13, plotfun = "trace", pars = "phi")
plot(const_supp_siml_bgar_1_initJuly13, plotfun = "trace", pars = "Ps")
plot(const_supp_siml_bgar_1_initJuly13, plotfun = "trace", pars = "ages")
plot(const_supp_siml_bgar_1_initJuly13, plotfun = "trace", pars = "mN")
plot(const_supp_siml_bgar_1_initJuly13, plotfun = "trace", pars = "alpha")

#bgar1 w/ t
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
p <- siml_conc[1:27]
pL <- siml_conc[28:30]
lambda <- 0.03114 
#omega <- 0.5
#priors
#yearly accumulation
alpha_phi <- 1
beta_phi <- 0.007
#correlation parameter #try beta(1,5)
alpha_psi <- 0.1
beta_psi <- 0.9
#plot(0, 0, xlim = c(0, 1), ylim = c(0, 1), type = "n")
#curve(dbeta(x, shape1 = 0.1, shape2 = 0.9), from = 0, to = 1, add = TRUE)
#supported activity
alpha_P <- 1
beta_P <- 0.05
#accumulation rate/gamma noise
alpha_a <- 1
beta_a <- 0.15
#plot(0, 0, xlim = c(0, 50), ylim = c(0, 0.4), type = "n")
#curve(dgamma(x, shape = 1, rate = 0.2), from = 0, to = 50, add = TRUE)
a <- 3
b <- 4

bgar1_tdistr_siml_dat <- list(N = N, L = L, Tot = Tot, x = x, xL = xL, delta = delta, M=M,
                       rho = rho, rhoL = rhoL, sigma = sigma, sigmaL = sigmaL, a =3, b = 4, 
                       p = p, pL = pL, lambda = lambda,  alpha_phi = alpha_phi, 
                       beta_phi = beta_phi, alpha_psi = alpha_psi, beta_psi = beta_psi, 
                       alpha_P = alpha_P, beta_P = beta_P, alpha_a = alpha_a, beta_a = beta_a)

const_supp_siml_bgar_tdistr_1_initJuly13 <- stan(file = 'Pb210_bgar1_tdistr.stan', data = bgar1_tdistr_siml_dat, 
                                          chains = 4, iter = 200000, control = list(adapt_delta = 0.99, max_treedepth = 10))

saveRDS(const_supp_siml_bgar_tdistr_1_initJuly13, "const_supp_siml_bgar_tdistr_1_initJuly13.rds")

#save to .csv
const_supp_siml_bgar_tdistr_1_initJuly13_list <- extract(const_supp_siml_bgar_tdistr_1_initJuly13)
const_supp_siml_bgar_tdistr_1_initJuly13_ages <- data.frame(const_supp_siml_bgar_tdistr_1_initJuly13_list$ages)
colnames(const_supp_siml_bgar_tdistr_1_initJuly13_ages) <- paste0("Age", 1:ncol(const_supp_siml_bgar_tdistr_1_initJuly13_ages))
const_supp_siml_bgar_tdistr_1_initJuly13_accurate <- data.frame(const_supp_siml_bgar_tdistr_1_initJuly13_list$m)
colnames(const_supp_siml_bgar_tdistr_1_initJuly13_accurate) <- paste0("m", 1:ncol(const_supp_siml_bgar_tdistr_1_initJuly13_accurate))
const_supp_siml_bgar_tdistr_1_initJuly13_param <- data.frame(cbind(const_supp_siml_bgar_tdistr_1_initJuly13_list$phi, 
                                                            const_supp_siml_bgar_tdistr_1_initJuly13_list$Ps,
                                                            const_supp_siml_bgar_tdistr_1_initJuly13_list$omega))
colnames(const_supp_siml_bgar_tdistr_1_initJuly13_param) <- c("Supply", "SuppAct", "Memory")
write.csv(const_supp_siml_bgar_tdistr_1_initJuly13_ages, "const_supp_siml_bgar_tdistr_1_initJuly13_ages.csv")
write.csv(const_supp_siml_bgar_tdistr_1_initJuly13_accurate, "const_supp_siml_bgar_tdistr_1_initJuly13_accurate.csv")
write.csv(const_supp_siml_bgar_tdistr_1_initJuly13_param, "const_supp_siml_bgar_tdistr_1_initJuly13_param.csv")

#save summaries to .csv
const_supp_siml_bgar_tdistr_1_initJuly13_ages_summ <- t(apply(X = const_supp_siml_bgar_tdistr_1_initJuly13_ages[,1:ncol(const_supp_siml_bgar_tdistr_1_initJuly13_ages)], 
                                                       MARGIN = 2, 
                                                       FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(const_supp_siml_bgar_tdistr_1_initJuly13_ages_summ)[1:2] <- c("mean", "sd")

const_supp_siml_bgar_tdistr_1_initJuly13_accurate_summ <- t(apply(X = const_supp_siml_bgar_tdistr_1_initJuly13_accurate[,1:ncol(const_supp_siml_bgar_tdistr_1_initJuly13_accurate)], 
                                                           MARGIN = 2, 
                                                           FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(const_supp_siml_bgar_tdistr_1_initJuly13_accurate_summ)[1:2] <- c("mean", "sd")
write.csv(const_supp_siml_bgar_tdistr_1_initJuly13_ages_summ, "const_supp_siml_bgar_tdistr_1_initJuly13_ages_summ.csv", row.names = F)
write.csv(const_supp_siml_bgar_tdistr_1_initJuly13_accurate_summ, "const_supp_siml_bgar_tdistr_1_initJuly13_accurate_summ.csv", row.names = F)

plot(const_supp_siml_bgar_tdistr_1_initJuly13, pars = "ages")
plot(const_supp_siml_bgar_tdistr_1_initJuly13, pars = "m")
plot(const_supp_siml_bgar_tdistr_1_initJuly13, pars = "alpha")
plot(const_supp_siml_bgar_tdistr_1_initJuly13, pars = "omega")
plot(const_supp_siml_bgar_tdistr_1_initJuly13, plotfun = "hist", pars = "phi")
plot(const_supp_siml_bgar_tdistr_1_initJuly13, plotfun = "hist", pars = "Ps")
plot(const_supp_siml_bgar_tdistr_1_initJuly13, plotfun = "hist", pars = "omega")
plot(const_supp_siml_bgar_tdistr_1_initJuly13, plotfun = "hist", pars = "ages")
plot(const_supp_siml_bgar_tdistr_1_initJuly13, plotfun = "hist", pars = "ages", binwidth = 0.5)
plot(const_supp_siml_bgar_tdistr_1_initJuly13, plotfun = "hist", pars = "alpha")
plot(const_supp_siml_bgar_tdistr_1_initJuly13, plotfun = "hist", pars = "m")
plot(const_supp_siml_bgar_tdistr_1_initJuly13, plotfun = "hist", pars = "mN")
plot(const_supp_siml_bgar_tdistr_1_initJuly13, plotfun = "trace", pars = "omega")
plot(const_supp_siml_bgar_tdistr_1_initJuly13, plotfun = "trace", pars = "phi")
plot(const_supp_siml_bgar_tdistr_1_initJuly13, plotfun = "trace", pars = "Ps")
plot(const_supp_siml_bgar_tdistr_1_initJuly13, plotfun = "trace", pars = "ages")
plot(const_supp_siml_bgar_tdistr_1_initJuly13, plotfun = "trace", pars = "mN")
plot(const_supp_siml_bgar_tdistr_1_initJuly13, plotfun = "trace", pars = "alpha")




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


check_hmc_diagnostics(const_supp_siml_bgar_1_initJuly13)
check_hmc_diagnostics(const_supp_siml_bgar_tdistr_1_initJuly13)
check_hmc_diagnostics(const_supp_siml_bgar_gam_1_initJuly13)
check_hmc_diagnostics(const_supp_siml_bgar_tdistr_gam_1_initJuly13)

summary(const_supp_siml_bgar_1_initJuly13)
summary(const_supp_siml_bgar_tdistr_1_initJuly13)
summary(const_supp_siml_bgar_gam_1_initJuly13)
summary(const_supp_siml_bgar_tdistr_gam_1_initJuly13)
