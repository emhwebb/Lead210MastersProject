library(tidyverse)
library(knitr)
library(kableExtra)

#age function 
siml_age <- function(x){
  x^2/3+x/2  
}

#density function
siml_coredens <- function(x){
  1.5-0.05*cos((pi*x)/30)
}

#P_0(x)
siml_P_o <- function(x){
  (150*(2*x/3+1/2))/siml_coredens(x)   
}

#Ps supported concentration
Ps <- 20

#P^U unsupp concentration
P_u <- function(x){
  siml_P_o(x)*exp(-0.03114*siml_age(x))
}

#unsupported concentration for each section
unsupp_conc <- function(depths){
  n <- length(depths)-1
  total_conc <- rep(NA, n)
  for(i in 1:n){
    total_conc[i] <- integrate(f = P_u, lower = depths[i], upper = depths[i+1])$value
  }
  total_conc
}

#total concentration 
#Depths will be from 0 to end-depth of choice
tot_conc <- function(depths){
  n <- length(depths)-1
  total_conc <- rep(NA, n)
  for(i in 1:n){
    total_conc[i] <- Ps*(depths[i+1]-depths[i])+integrate(f = P_u, lower = depths[i], upper = depths[i+1])$value
  }
  total_conc
}

mar_aq <- function(x, dens){
  dens/(2*x/3+1/2)
}

sedacc_aq <- function(x){
  1/(2*x/3+1/2) 
}

siml_age(1:30)
sigmas <- c(10, rep(9, 7), rep(8, 7), rep(7, 7), rep(6, 7), 5)
depths <- 0:30
true_tot <- tot_conc(depths)
unsupp_conc(0:30)
P_u(0:30)
rand_err <- rnorm(30, mean = 0, sd = sigmas)
siml_conc <- true_tot+rand_err
siml_supp <- rnorm(30, mean = Ps, sd = sigmas)
siml_supp
siml_conc
#saveRDS(rand_err, "siml_rand_err.rds")
#saveRDS(siml_supp, "siml_rand_supp.rds")
rand_err <- readRDS("siml_rand_err.rds")
siml_supp <- readRDS("siml_rand_supp.rds")

lambda <- 0.03114 

#implements CRS age-depth
CRS_calc <- function(depths = 1:30, siml_supp = siml_supp, siml_conc = siml_conc, dens, lambda = 0.03114){
  #unsupp_act_whol <- siml_coredens(depths)*(siml_conc-siml_supp)
  unsupp_act_whol <- dens*(siml_conc-siml_supp)
  whether <- rep(NA, length(depths))
  
  for(i in 1:length(depths)){
    if(i == 1){
      ifelse(unsupp_act_whol[i] <= 0, whether[i] <- 0, whether[i] <- 1)
    }
    else 
      ifelse(unsupp_act_whol[i] <= 0 | whether[i-1] == 0, whether[i] <- 0, whether[i] <- 1)
     
  }
  
  unsupp_act <- unsupp_act_whol[whether == 1]
  n <- sum(whether)
  ages <- rep(NA, n)
  
  for(i in 1:n){
   ages[i] <- (1/lambda)*log(sum(unsupp_act[1:n])/sum(unsupp_act[i:n]))
  }
  ages
}

CRS_calc(depths = 1:30, siml_supp = siml_supp, siml_conc = siml_conc, dens = siml_coredens(1:30))

c1 <- c("Depth", "cm", 1:15)
c2 <- c("${}^{210}Pb(P^T)$", "Bq/kg", siml_conc[1:15])
c3 <- c("${}^{214}Pb(P^S)$", "Bq/kg", siml_supp[1:15])
c4 <- c("$\\sigma$", "Bq/kg", sigmas[1:15])
c5 <- c("Density $$\\rho$)", "$g/cm^2$", siml_coredens(1:30)[1:15])
c6 <- c("Depth", "cm", 16:30)
c7 <- c("${}^{210}Pb(P^T)$", "Bq/kg", siml_conc[16:30])
c8 <- c("${}^{214}Pb(P^S)$", "Bq/kg", siml_supp[16:30])
c9 <- c("$\\sigma$", "Bq/kg", sigmas[16:30])
c10 <- c("Density $$\\rho$)", "$g/cm^2$", siml_coredens(1:30)[16:30])

data.frame(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10) %>%
  kable("latex")


