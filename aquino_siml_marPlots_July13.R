library(tidyverse)
library(patchwork)

Depth <- 1:30
core_dens <- siml_coredens(Depth)
True_mar <- mar_aq(x = Depth, dens = core_dens)

const_supp_siml_gam_1_initJuly13_accurate <- read_csv("run_results/posterior_simulation/prod/siml/July13/const_supp/gam/Normal/const_supp_siml_gam_1_initJuly13_accurate.csv")
const_supp_siml_tdistr_gam_1_initJuly13_accurate <- read_csv("run_results/posterior_simulation/prod/siml/July13/const_supp/gam/tdistr/const_supp_siml_tdistr_gam_1_initJuly13_accurate.csv")

const_supp_siml_gam_1_initJuly13_accurate <- core_dens/const_supp_siml_gam_1_initJuly13_accurate[,2:ncol(const_supp_siml_gam_1_initJuly13_accurate)]
const_supp_siml_tdistr_gam_1_initJuly13_accurate <- core_dens/const_supp_siml_tdistr_gam_1_initJuly13_accurate[,2:ncol(const_supp_siml_tdistr_gam_1_initJuly13_accurate)]


#
const_supp_siml_gam_1_initJuly13_accurate_summ <- t(apply(X = const_supp_siml_gam_1_initJuly13_accurate[,1:ncol(const_supp_siml_gam_1_initJuly13_accurate)], 
                                                           MARGIN = 2, 
                                                           FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(const_supp_siml_gam_1_initJuly13_accurate_summ)[1:2] <- c("mean", "sd")
#
const_supp_siml_tdistr_gam_1_initJuly13_accurate_summ <- t(apply(X = const_supp_siml_tdistr_gam_1_initJuly13_accurate[,1:ncol(const_supp_siml_tdistr_gam_1_initJuly13_accurate)], 
                                                           MARGIN = 2, 
                                                           FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(const_supp_siml_tdistr_gam_1_initJuly13_accurate_summ)[1:2] <- c("mean", "sd")

const_supp_siml_gam_1_initJuly13_accurate_summ <- as.data.frame(const_supp_siml_gam_1_initJuly13_accurate_summ)
const_supp_siml_tdistr_gam_1_initJuly13_accurate_summ <- as.data.frame(const_supp_siml_tdistr_gam_1_initJuly13_accurate_summ)

simlmar_aquino_norm_gam <- select(const_supp_siml_gam_1_initJuly13_accurate_summ, `50%`, `2.5%`, `97.5%`)
simlmar_aquino_norm_gam <- data.frame(cbind(simlmar_aquino_norm_gam, rep("Normal", 30)))
colnames(simlmar_aquino_norm_gam) <- c("Normal", "2.5%", "97.5%", "Method")
simlmar_aquino_tdistr_gam <- select(const_supp_siml_tdistr_gam_1_initJuly13_accurate_summ, `50%`, `2.5%`, `97.5%`)
simlmar_aquino_tdistr_gam <- data.frame(cbind(simlmar_aquino_tdistr_gam, rep("tdistr", 30)))
colnames(simlmar_aquino_tdistr_gam) <- c("tdistr", "2.5%", "97.5%", "Method")


siml_aquino_gam_bci <- as.data.frame(rbind(cbind(Depth, select(simlmar_aquino_norm_gam, `2.5%`, `97.5%`, Method)), 
                                    cbind(Depth, select(simlmar_aquino_tdistr_gam, `2.5%`, `97.5%`, Method))))

siml_gam_normal_mean <- select(simlmar_aquino_norm_gam, Normal)
siml_gam_tdistr_mean <- select(simlmar_aquino_tdistr_gam, tdistr)

siml_mar_gam_df <- as.data.frame(cbind(Depth, True_mar, 
                                        siml_gam_normal_mean, siml_gam_tdistr_mean)) %>% 
  select(Depth, True_mar, Normal, tdistr) %>%
  pivot_longer(cols = c("True_mar", "Normal", "tdistr"), 
               names_to = "Method", values_to = "Age") 

siml_aquino_gam_plot_df <- left_join(siml_mar_gam_df, siml_aquino_gam_bci, by = c("Depth", "Method"))


p3 <- siml_aquino_gam_plot_df %>%
  ggplot(aes(x = Depth, y = Age, col = Method))+
  geom_line(size = 1, alpha = 0.4)+
  geom_errorbar(aes(ymin = `2.5%`, ymax = `97.5%`), size = 1, alpha = 0.4)+
  scale_x_continuous(name="Depth (cm)", breaks = seq(0, 30, 5)) +
  scale_y_continuous(name= bquote("Median Acc Rate " (~g~cm^-2~yr^-1)), breaks = seq(0, 25, 1.5))+
  theme_bw()+
  ggtitle("Normal and t-distr, Whole Core Support") +
  theme(legend.position = c(0.2, 0.8), plot.title = element_text(margin = margin(t = 15, b = -15), hjust = 0.5))

p3
