library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

#define data from HP1C
N <- 29
M <- 30
L <- 4
x <- 1:29
xL <- c(35,40,45,50)
delta <- 1
rho <- c(0.045, 0.047, 0.051, 0.049, 0.049, 0.051, 0.05, 0.047, 
         0.048, 0.049, 0.052, 0.047, 0.051, 0.05, 0.053, 0.048, 
         0.048, 0.045, 0.045, 0.054, 0.086, 0.089, 0.099, 0.085, 
         0.066, 0.06, 0.062, 0.055, 0.059)
rhoL <- c(0.356, 0.414, 0.347, 0.352)
sigma <- c(11.9, 15.08, 17.11, 14.43, 16.44, 16.75, 16.78, 
           15.2, 14.83, 16.21, 13.03, 15.22, 13.45, 10.02, 11.62, 
           9.76, 12.95, 11.14, 9.94, 9.17, 13.1, 11.38, 12.72, 7.24, 
           4.74, 3.41,2.26, 2.1, 1.76) 
sigmaL <- c(1.27, 1.01, 1.31, 1.6)
p <- c(371.73, 456.39, 454.24, 449.64, 479.04, 490.97, 
       482.12, 486.88, 431.58, 422.75, 315.31, 349.77, 301.74, 
       284.41, 280.58, 250.17, 267.74, 279.32, 243.82, 246.75, 
       351.68, 281.28, 235.3, 192.82, 94.28, 50.55, 36.08, 28.71, 24.68)
pL <- c(11.04, 6.24, 10.15, 7.96)
#pmod <- p*0.001
#pmodL <- pL*0.001
#rhomod <- rho*10
#rhoLmod <- rhoL*10
lambda <- 0.03114 
#omega <- 0.5
#priors
alpha_phi <- 2
beta_phi <- 0.015
a_omega <- 3
b_omega <- 7
alpha_P <- 2 
beta_P <- 0.25
alpha_a <- 0.001
beta_a <- 0.001

HP1c_dat <- list(N = N, L = L, x = x, xL = xL, delta = delta, M=M,
                 rho = rhomod, rhoL = rhoLmod, sigma = sigma, sigmaL = sigmaL, 
                 p = p, pL = pL, lambda = lambda,  alpha_phi = alpha_phi, 
                 beta_phi = beta_phi, a_omega = a_omega, b_omega = b_omega, alpha_P = alpha_P, 
                 beta_P = beta_P, alpha_a = alpha_a, beta_a = beta_a)



#run stan fit
test_run <- stan(file = 'Pb210_draft.stan', data = HP1c_dat, chains = 1, iter = 10)
#stan(file = 'debug_Pb210.stan', data = HP1c_dat, chains = 1, iter = 10)
extract(test_run)
plot(test_run)

test_prodrun <- stan(file = 'Pb210_draft.stan', data = HP1c_dat, chains = 4, iter = 200000)
plot(test_prodrun)
prodrun_fit <- extract(test_prodrun)
plot(test_prodrun, pars = "ages")
plot(test_prodrun, pars = "m")
plot(test_prodrun, plotfun = "hist", pars = "m")
plot(test_prodrun, pars = "alpha")
plot(test_prodrun, pars = "accu_rates")
plot(test_prodrun, pars = "mN", plotfun = "hist")
plot(test_prodrun, plotfun = "hist", pars = "phi")
plot(test_prodrun, plotfun = "hist", pars = "Ps")
plot(test_prodrun, plotfun = "hist", pars = "omega")
plot(test_prodrun, pars = "ages")
plot(test_prodrun, plotfun = "trace", pars = "phi")
plot(test_prodrun, plotfun = "trace", pars = "Ps")

save(prodrun_fit, file="prodrun_fit1.RData")
