library(readr)
library(tidyverse)
library(patchwork)

#Estuary
#Core Name
#siml Date
#Sample Year

#ABOR
abor_num <- c(paste0("ABOR1309_0", 1:4), paste0("ABOR1702_0", c(1,2,5:8)))
#abor_name <- paste0("ABOR", abor_num)
abor_year <- c(rep(2013, 4), rep(2017, 6))
abor_conv <- paste0("AB", 
                    c("10", "11", "12", "13", "01", "02", "05", "06", "07", "08"))
#NBOR
nbor_num <- c(paste0("NBOR1606_0", c(3,4,7,8)), paste0("NBOR1609_0", 1:4))
#nbor_name <- paste0("NBOR", nbor_num)
nbor_year <- rep(2016, 8)
nbor_conv <- paste0("NB", c("03", "04", "07", "08", "09", "10", "11", "12"))
#CMOR
cmor_num <- c(paste0("CMOR1706_0", 1:9), "CMOR1706_10")
#cmor_name <- paste0("CMOR", cmor_num)
cmor_year <- rep(2017, 10)
cmor_conv <- paste0("CM", c(paste0(0, 1:9), "10"))
#NTOR
ntor_num <- c(paste0("NTOR1009_0", 1:4), paste0("NTOR1709_0", 1:4), paste0("NTOR1802_0", 1:9), "NTOR1802_10")
ntor_year <- c(rep(2010, 4), rep(2017, 4), rep(2018, 10))
#SROR
sror_num <- c(paste0("SROR1509_0", 1:4), paste0("SROR1809_0", 1:4))
sror_year <- c(rep(2015, 4), rep(2018, 4))
  
  
#idea will be to run mass accumulation versus year plot in lapply to obtain lists for each estuary
#can then save and manipulate plots as we want 
#Estuary = "ABOR", etc
#core_name = abor_name, etc so ABOR1309_01, etc. 
#siml_date is date simulation was ran (to construct file path) in MonthDay (e.g. July29 or August6) format as a character 
#samp_year = year sample was taken
#conv_name is to switch from ABOR to AB
mar_plot <- function(estuary, core_name, siml_date, samp_year, conv_name){
  #define terms/read in data
  PU <- paste0(core_name, "PU")
  peck_cores <- read_csv("peck_et_al_2020_depthseries.csv")
  #string paths to read in the posterior simulation summaries
  accurate_summ_strpath <- paste0("run_results/posterior_simulation/prod/init/", 
                                  siml_date, "/", estuary, "/", PU, "/", 
                                  core_name, "_init", siml_date, "nonconst_accurate_summ.csv")
  age_summ_strpath <- paste0("run_results/posterior_simulation/prod/init/", 
                             siml_date, "/", estuary, "/", PU, "/", 
                             core_name, "_init", siml_date, "nonconst_ages_summ.csv")
  #read in the summary tables
  accurate_summ <- read_csv(accurate_summ_strpath)
  ages_summ <- read_csv(age_summ_strpath)
  
  #max_depth is maximum depth for the total lead-210 and is 2*number rows in accurate_summ or ages_summ
  max_depth <- 2*nrow(accurate_summ)
  #grab the dry bulk density (modeled)
  dens <- peck_cores %>% 
    filter(core_id == PU, depth_max <= max_depth) %>%
    select(dry_bulk_density_modeled)	
  
  MAR <- rbind(dens$dry_bulk_density_modeled*accurate_summ,
        rep(NA, ncol(accurate_summ)))
  Years <- select(rbind(c(samp_year, 0, 0, 0, samp_year, 0, 0), 
                        samp_year-ages_summ), `50%`)
  colnames(Years) <- "Year"
  #change year fitler to be 1880 to 2020
  MAR_p <- MAR %>% 
    select(`25%`, `50%`, `75%`) %>%
    cbind(Years) %>%
    filter(Year >= 1880) %>%
    ggplot(aes(x = Year, y = `50%`))+
    geom_point()+
    geom_line()+
    geom_errorbar(aes(ymin = `25%`, ymax = `75%`))+
    scale_x_continuous(name = "Year", breaks = seq(1880, 2020, by = 15))+
    scale_y_continuous(name = bquote("Mass Accumulation Rate " (~g~cm^-2~yr^-1)), breaks = seq(0, 3, by = 0.1))+
    theme_classic()+
    theme(axis.text.x = element_text(angle = -90), 
          plot.title = element_text(vjust = -5, hjust = 0.75), 
          axis.title.y = element_blank())+
    ggtitle(conv_name)
  MAR_p
}

#mar_plot("ABOR", "ABOR1309_02", "July29", 2013)

#note that ABOR1702_08 is odd b/c b/w 32 and 36, 34 is missing
#alsea
ABOR_MAR_plots <- lapply(1:length(abor_num), 
       FUN = function(i) mar_plot(estuary = "ABOR", 
                                  core_name = abor_num[i], 
                                  siml_date = "July29", 
                                  samp_year = abor_year[i],
                                  conv_name = abor_conv[i]))
names(ABOR_MAR_plots) <- abor_conv
ABOR_MAR_plots$AB02 <- ABOR_MAR_plots$AB02+
  theme(axis.title.y = element_text(angle = 90))+
  scale_y_continuous(name = bquote("MAR " (~g~cm^-2~yr^-1)), breaks = seq(0, 3, by = 0.05))
ABOR_MAR_plots$AB05 <- ABOR_MAR_plots$AB05+
  theme(axis.title.y = element_text(angle = 90))+
  scale_y_continuous(name = bquote("MAR " (~g~cm^-2~yr^-1)), breaks = seq(0, 3, by = 0.05))
ABOR_MAR_plots$AB11 <- ABOR_MAR_plots$AB11+
  theme(axis.title.y = element_text(angle = 90))+
  scale_y_continuous(name = bquote("MAR " (~g~cm^-2~yr^-1)), breaks = seq(0, 3, by = 0.05))  

(ABOR_MAR_plots$AB11+ABOR_MAR_plots$AB13+ABOR_MAR_plots$AB12)/
(ABOR_MAR_plots$AB02+ABOR_MAR_plots$AB01+ABOR_MAR_plots$AB10)/
(ABOR_MAR_plots$AB05+ABOR_MAR_plots$AB06+ABOR_MAR_plots$AB07)

#   11  13 12
#02 01       10
#
#   05  06      07 
#approx layout of sediment core locations

#(ABOR_MAR_plots[[1]]+ABOR_MAR_plots[[2]])/(ABOR_MAR_plots[[3]]+ABOR_MAR_plots[[4]])/(ABOR_MAR_plots[[5]]+ABOR_MAR_plots[[6]])

#nehalem
NBOR_MAR_plots <- lapply(1:length(nbor_num), 
                         FUN = function(i) mar_plot(estuary = "NBOR", 
                                                    core_name = nbor_num[i], 
                                                    siml_date = "July29", 
                                                    samp_year = nbor_year[i],
                                                    conv_name = nbor_conv[i]))
names(NBOR_MAR_plots) <- nbor_conv
NBOR_MAR_plots$NB07 <- NBOR_MAR_plots$NB07+
  theme(axis.title.y = element_text(angle = 90))+
  scale_y_continuous(name = bquote("MAR " (~g~cm^-2~yr^-1)), breaks = seq(0, 1, 0.15))
NBOR_MAR_plots$NB04 <- NBOR_MAR_plots$NB04+
  theme(axis.title.y = element_text(angle = 90))+
  scale_y_continuous(name = bquote("MAR " (~g~cm^-2~yr^-1)), breaks = seq(0, 1, 0.15))
NBOR_MAR_plots$NB12 <- NBOR_MAR_plots$NB12+
  theme(axis.title.y = element_text(angle = 90))+
  scale_y_continuous(name = bquote("MAR " (~g~cm^-2~yr^-1)), breaks = seq(0, 3, by = 0.05))
NBOR_MAR_plots$NB03 <- NBOR_MAR_plots$NB03+
  scale_y_continuous(breaks = seq(0, 0.3, by = 0.02))
NBOR_MAR_plots$NB08 <- NBOR_MAR_plots$NB08+
  scale_y_continuous(breaks = seq(0, 1, 0.05))
NBOR_MAR_plots$NB09 <- NBOR_MAR_plots$NB09+
  scale_y_continuous(breaks = seq(0, 1, 0.05))
NBOR_MAR_plots$NB11 <- NBOR_MAR_plots$NB11+
  scale_y_continuous(breaks = seq(0, 1, 0.05))
NBOR_MAR_plots$NB10 <- NBOR_MAR_plots$NB10+
  scale_y_continuous(breaks = seq(0, 1, 0.05))

(NBOR_MAR_plots$NB07+NBOR_MAR_plots$NB08+NBOR_MAR_plots$NB09)/
(NBOR_MAR_plots$NB12+NBOR_MAR_plots$NB10+NBOR_MAR_plots$NB11)/
  (NBOR_MAR_plots$NB04+NBOR_MAR_plots$NB03)

#   07 08 09 
#   12  10  11
#         04
#         03
#

#coquille
CMOR_MAR_plots <- lapply(1:length(cmor_num), 
                         FUN = function(i) mar_plot(estuary = "CMOR", 
                                                    core_name = cmor_num[i], 
                                                    siml_date = "July29", 
                                                    samp_year = cmor_year[i],
                                                    conv_name = cmor_conv[i]))

names(CMOR_MAR_plots) <- cmor_conv

CMOR_MAR_plots$CM05 <- CMOR_MAR_plots$CM05+
  theme(axis.title.y = element_text(angle = 90))+
  scale_y_continuous(name = bquote("MAR " (~g~cm^-2~yr^-1)), breaks = seq(0, 0.4, by = 0.05))
CMOR_MAR_plots$CM09 <- CMOR_MAR_plots$CM09+
  theme(axis.title.y = element_text(angle = 90))+
  scale_y_continuous(name = bquote("MAR " (~g~cm^-2~yr^-1)), breaks = seq(0, 3, by = 0.05))
CMOR_MAR_plots$CM10 <- CMOR_MAR_plots$CM10+
  scale_y_continuous(breaks = seq(0, 1.5, by = 0.15))
CMOR_MAR_plots$CM02 <- CMOR_MAR_plots$CM02+
  scale_y_continuous(breaks = seq(0, 0.5, by = 0.02))
CMOR_MAR_plots$CM04 <- CMOR_MAR_plots$CM04+
  scale_y_continuous(breaks = seq(0, 0.5, by = 0.05))
CMOR_MAR_plots$CM03 <- CMOR_MAR_plots$CM03+
  scale_y_continuous(breaks = seq(0, 0.4, by = 0.05))
CMOR_MAR_plots$CM07 <- CMOR_MAR_plots$CM07+
  scale_y_continuous(breaks = seq(0, 0.5, by = 0.05))
CMOR_MAR_plots$CM06 <- CMOR_MAR_plots$CM06+
  theme(axis.title.y = element_text(angle = 90))+
  scale_y_continuous(name = bquote("MAR " (~g~cm^-2~yr^-1)), breaks = seq(0, 3, by = 0.1))
CMOR_MAR_plots$CM08 <- CMOR_MAR_plots$CM08+
  theme(axis.title.y = element_text(angle = 90))+
  scale_y_continuous(name = bquote("MAR " (~g~cm^-2~yr^-1)), breaks = seq(0, 3, by = 0.1))

#     04    01
#     03    02
#     06    05
#     09    10
#     08    07

(CMOR_MAR_plots$CM05+CMOR_MAR_plots$CM04+CMOR_MAR_plots$CM01)/
(CMOR_MAR_plots$CM06+CMOR_MAR_plots$CM03+CMOR_MAR_plots$CM02)/
(CMOR_MAR_plots$CM09+CMOR_MAR_plots$CM10)/
(CMOR_MAR_plots$CM08+CMOR_MAR_plots$CM07)


#netarts
NTOR_MAR_plots <- lapply(1:length(ntor_num), 
                         FUN = function(i) mar_plot(estuary = "NTOR", 
                                                    core_name = ntor_num[i], 
                                                    siml_date = "August6", 
                                                    samp_year = ntor_year[i]))
NTOR_MAR_plots
#NTOR1802_05 looks strange b/c of gap between 12 cm and 16 cm 

#salmon
SROR_MAR_plots <- lapply(1:length(sror_num), 
                         FUN = function(i) mar_plot(estuary = "SROR", 
                                                    core_name = sror_num[i], 
                                                    siml_date = "August6", 
                                                    samp_year = sror_year[i]))
SROR_MAR_plots
#SROR1509_03 looks strange likely is because depths start at 5cm and go to 25 cm
