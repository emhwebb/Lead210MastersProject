library(tidyverse)
library(knitr)
library(kableExtra)

#age function 
siml_age <- function(x){
  180*(exp(.35*x-5)/(1+exp(.35*x-5))) 
}

#density function
siml_coredens <- function(x){
  1.5-0.05*cos((pi*x)/30)
}

#P_0(x)
siml_P_o <- function(x){
  (150*((9350.03*exp(0.35*x))/(exp(5) + exp(0.35*x))^2))/siml_coredens(x)   
}

#Ps supported concentration
Ps <- 20

#nonconstant supported activity according to a cosine
supp_conc <- function(x, Ps, amp){
  Ps+amp*cos((pi*x)/30) 
}

#mass accumulation rate

mar <- function(x, dens){
  dens/((9350.03*exp(0.35*x))/(exp(5) + exp(0.35*x))^2)
}

#sediment accuumulation rate
sedacc <- function(x){
  1/((9350.03*exp(0.35*x))/(exp(5) + exp(0.35*x))^2)
}

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
#nonconstant support
tot_conc_nonconstsupp <- function(depths, Ps){
  n <- length(depths)-1
  total_conc <- rep(NA, n)
  for(i in 1:n){
    total_conc[i] <- Ps[i]*(depths[i+1]-depths[i])+integrate(f = P_u, lower = depths[i], upper = depths[i+1])$value
  }
  total_conc
}

#constant support
tot_conc <- function(depths, Ps){
  n <- length(depths)-1
  total_conc <- rep(NA, n)
  for(i in 1:n){
    total_conc[i] <- Ps*(depths[i+1]-depths[i])+integrate(f = P_u, lower = depths[i], upper = depths[i+1])$value
  }
  total_conc
}

siml_age(1:30)
sigmas <- c(10, rep(9, 7), rep(8, 7), rep(7, 7), rep(6, 7), 5)
depths <- 0:30
Ps_unsupp <- supp_conc(0:30, Ps = 20, amp = 5)
#with nonconstant support
true_tot <- tot_conc_nonconstsupp(depths, Ps = Ps_unsupp)
#with constant support
true_tot2 <- tot_conc(depths, Ps = Ps)
#unsupp_conc(0:30)
#P_u(0:30)
#true_tot <- tot_conc(depths,Ps = Ps_unsupp)
rand_err2 <- rnorm(30, mean = 0, sd = sigmas)
#with nonconstant support
siml_conc2 <- true_tot+rand_err2
#with constant support
siml_conc3 <- true_tot2+rand_err2
#with nonconstant support
siml_supp2 <- rnorm(30, mean = Ps_unsupp, sd = sigmas)
#with constant support
siml_supp3 <- rnorm(30, mean = Ps, sd = sigmas)
siml_supp2
siml_supp3
siml_conc2
siml_conc3
#saveRDS(rand_err2, "siml_rand_err2.rds")
#saveRDS(siml_supp2, "siml_rand_supp2.rds")
#saveRDS(rand_err3, "siml_rand_err3.rds")
#saveRDS(siml_supp3, "siml_rand_supp3.rds")
rand_err2 <- readRDS("siml_rand_err2.rds")
siml_supp2 <- readRDS("siml_rand_supp2.rds")
#rand_err3 <- readRDS("siml_rand_err3.rds")
siml_supp3 <- readRDS("siml_rand_supp3.rds")

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

CRS_calc(depths = 1:30, siml_supp = siml_supp2, siml_conc = siml_conc2, dens = siml_coredens(1:30))
CRS_calc(depths = 1:30, siml_supp = siml_supp3, siml_conc = siml_conc3, dens = siml_coredens(1:30))


