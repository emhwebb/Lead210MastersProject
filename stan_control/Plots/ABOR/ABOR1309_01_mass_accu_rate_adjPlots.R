library(readr)
library(tidyverse)


ABOR1309_01_initJuly29nonconst_accurate_summ <- read_csv("run_results/posterior_simulation/prod/init/July29/ABOR/ABOR1309_01PU/ABOR1309_01_initJuly29nonconst_accurate_summ.csv")
peck_cores <- read_csv("peck_et_al_2020_depthseries.csv")
ABOR1309_01_initJuly29nonconst_ages_summ <- read_csv("run_results/posterior_simulation/prod/init/July29/ABOR/ABOR1309_01PU/ABOR1309_01_initJuly29nonconst_ages_summ.csv")
#2013

#nonconstant supported activity
#ABOR1309_01PU
ABOR1309_01PU <- peck_cores %>% 
  filter(core_id == "ABOR1309_01PU", depth_max <= 38) %>%
  select(depth_max, dry_bulk_density_modeled, total_pb210_activity, total_pb210_activity_se,
         pb214_activity, pb214_activity_se, excess_pb210_activity, excess_pb210_activity_se)	
#year when sample taken
top_year <- 2013
sed_core <- "ABOR1309_01"

ABOR1309_01_MAR <- rbind(ABOR1309_01PU$dry_bulk_density_modeled*ABOR1309_01_initJuly29nonconst_accurate_summ, 
             rep(NA, ncol(ABOR1309_01_initJuly29nonconst_accurate_summ)))
ABOR1309_01_Years <- select(rbind(c(top_year, 0, 0, 0, top_year, 0, 0), 
                                  top_year-ABOR1309_01_initJuly29nonconst_ages_summ), `50%`)
colnames(ABOR1309_01_Years) <- "Year"

ABOR1309_01_MAR_df <- ABOR1309_01_MAR %>% 
  select(`25%`, `50%`, `75%`) %>%
  cbind(ABOR1309_01_Years)
  
ABOR1309_01_MAR_plot <- ABOR1309_01_MAR_df %>%
  ggplot(aes(x = Year, y = `50%`))+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin = `25%`, ymax = `75%`))+
  scale_x_continuous(name = "Year", breaks = seq(1830, 2020, by = 10))+
  scale_y_continuous(name = bquote("Mass Accumulation Rate " (~g~cm^-2~yr^-1)), breaks = seq(0, 3, by = 0.05))+
  theme_classic()+
  ggtitle(sed_core)

ABOR1309_01_MAR_plot
