library(tidyverse)
library(readr)
library(patchwork)

#for 1:27 with alpha-spectrometry est. 
Depth <- 1:27
True_Ages <- siml_age(1:27)
CRS <- c(CRS_calc(depths = 1:30, 
                  siml_supp = rep(mean(siml_conc[28:30]), 30), 
                  siml_conc = siml_conc,
                  dens = siml_coredens(1:30))[1:26], NA)

simlages_norm_alph <- select(const_supp_siml_1_initJuly13_ages_summ, mean, `2.5%`, `97.5%`)
simlages_norm_alph <- data.frame(cbind(simlages_norm_alph, rep("Normal", 27)))
colnames(simlages_norm_alph) <- c("Normal", "2.5%", "97.5%", "Method")
simlages_tdistr_alph <- select(const_supp_siml_tdistr_1_initJuly13_ages_summ, mean, `2.5%`, `97.5%`)
simlages_tdistr_alph <- data.frame(cbind(simlages_tdistr_alph, rep("tdistr", 27)))
colnames(simlages_tdistr_alph) <- c("tdistr", "2.5%", "97.5%", "Method")
simlages_bgar_alph <- select(const_supp_siml_bgar_1_initJuly13_ages_summ, mean, `2.5%`, `97.5%`)
simlages_bgar_alph <- data.frame(cbind(simlages_bgar_alph, rep("bgar1", 27)))
colnames(simlages_bgar_alph) <- c("bgar1", "2.5%", "97.5%", "Method")
simlages_bgartdistr_alph <- select(const_supp_siml_bgar_tdistr_1_initJuly13_ages_summ, mean, `2.5%`, `97.5%`)
simlages_bgartdistr_alph <- data.frame(cbind(simlages_bgartdistr_alph, rep("bgar1_tdistr", 27)))
colnames(simlages_bgartdistr_alph) <- c("bgar1_tdistr", "2.5%", "97.5%", "Method")

siml_alph_bci <- as.data.frame(rbind(cbind(Depth, select(simlages_norm_alph, `2.5%`, `97.5%`, Method)), 
            cbind(Depth, select(simlages_tdistr_alph, `2.5%`, `97.5%`, Method))))
siml_alph_bci_bgar <- as.data.frame(rbind(cbind(Depth, select(simlages_bgar_alph, `2.5%`, `97.5%`, Method)), 
                                          cbind(Depth, select(simlages_bgartdistr_alph, `2.5%`, `97.5%`, Method))))

siml_alph_normal_mean <- select(simlages_norm_alph, Normal)
siml_alph_tdistr_mean <- select(simlages_tdistr_alph, tdistr)
siml_alph_bgar_mean <- select(simlages_bgar_alph, bgar1)
siml_alph_bgartdistr_mean <- select(simlages_bgartdistr_alph, bgar1_tdistr)

siml_ages_alph_df <- as.data.frame(cbind(Depth, True_Ages, CRS, 
                                          siml_alph_normal_mean, siml_alph_tdistr_mean)) %>% 
  select(Depth, True_Ages, CRS, Normal, tdistr) %>%
  pivot_longer(cols = c("True_Ages", "Normal", "tdistr", "CRS"), 
               names_to = "Method", values_to = "Age") 

siml_ages_alph_bgar_df <- as.data.frame(cbind(Depth, True_Ages, CRS, 
                                              siml_alph_bgar_mean, siml_alph_bgartdistr_mean)) %>% 
  select(Depth, True_Ages, CRS, bgar1, bgar1_tdistr) %>%
  pivot_longer(cols = c("True_Ages", "bgar1", "bgar1_tdistr", "CRS"), 
               names_to = "Method", values_to = "Age") 

siml_alph_plot_df <- left_join(siml_ages_alph_df, siml_alph_bci, by = c("Depth", "Method"))
siml_alph_plot_bgar_df <- left_join(siml_ages_alph_bgar_df, siml_alph_bci_bgar, by = c("Depth", "Method"))


p1 <- siml_alph_plot_df %>%
  ggplot(aes(x = Depth, y = Age, col = Method))+
  geom_line(size = 1, alpha = 0.4)+
  geom_errorbar(aes(ymin = `2.5%`, ymax = `97.5%`), size = 1, alpha = 0.4)+
  scale_x_continuous(name="Depth (cm)", breaks = seq(0, 30, 5)) +
  scale_y_continuous(name="Age (Years)", breaks = seq(0, 250, 50))+
  theme_bw()+
  ggtitle("Normal and t-distr, Bottom 3 supported") +
  theme(legend.position = c(0.2, 0.8), plot.title = element_text(margin = margin(t = 15, b = -15), hjust = 0.5))


p1

p2 <- siml_alph_plot_bgar_df %>%
  ggplot(aes(x = Depth, y = Age, col = Method))+
  geom_line(size = 1, alpha = 0.4)+
  geom_errorbar(aes(ymin = `2.5%`, ymax = `97.5%`), size = 1, alpha = 0.4)+
  scale_x_continuous(name="Depth (cm)", breaks = seq(0, 30, 5)) +
  scale_y_continuous(name="Age (Years)", breaks = seq(0, 250, 50))+
  theme_bw()+
  ggtitle("BGAR(1), Bottom 3 Supported") +
  theme(legend.position = c(0.2, 0.8), plot.title = element_text(margin = margin(t = 15, b = -15), hjust = 0.5))

p2

#for 1:30
Depth <- 1:30
True_Ages <- siml_age(1:30)
#CRS <- c(CRS_calc(depths = 1:30, siml_supp = siml_supp, siml_conc = siml_conc)[2:26], rep(NA, 5))
CRS <- c(CRS_calc(depths = 1:30, 
                  siml_supp = siml_supp, 
                  siml_conc = siml_conc,
                  dens = siml_coredens(1:30))[1:26], rep(NA, 4))

simlages_norm_gam <- select(const_supp_siml_gam_1_initJuly13_ages_summ, mean, `2.5%`, `97.5%`)
simlages_norm_gam <- data.frame(cbind(simlages_norm_gam, rep("Normal", 30)))
colnames(simlages_norm_gam) <- c("Normal", "2.5%", "97.5%", "Method")
simlages_tdistr_gam <- select(const_supp_siml_tdistr_gam_1_initJuly13_ages_summ, mean, `2.5%`, `97.5%`)
simlages_tdistr_gam <- data.frame(cbind(simlages_tdistr_gam, rep("tdistr", 30)))
colnames(simlages_tdistr_gam) <- c("tdistr", "2.5%", "97.5%", "Method")
simlages_bgar_gam <- select(const_supp_siml_bgar_gam_1_initJuly13_ages_summ, mean, `2.5%`, `97.5%`)
simlages_bgar_gam <- data.frame(cbind(simlages_bgar_gam, rep("bgar1", 30)))
colnames(simlages_bgar_gam) <- c("bgar1", "2.5%", "97.5%", "Method")
simlages_bgartdistr_gam <- select(const_supp_siml_bgar_tdistr_gam_1_initJuly13_ages_summ, mean, `2.5%`, `97.5%`)
simlages_bgartdistr_gam <- data.frame(cbind(simlages_bgartdistr_gam, rep("bgar1_tdistr", 30)))
colnames(simlages_bgartdistr_gam) <- c("bgar1_tdistr", "2.5%", "97.5%", "Method")

siml_gam_bci <- as.data.frame(rbind(cbind(Depth, select(simlages_norm_gam, `2.5%`, `97.5%`, Method)), 
                                     cbind(Depth, select(simlages_tdistr_gam, `2.5%`, `97.5%`, Method))))
siml_gam_bci_bgar <- as.data.frame(rbind(cbind(Depth, select(simlages_bgar_gam, `2.5%`, `97.5%`, Method)), 
                                          cbind(Depth, select(simlages_bgartdistr_gam, `2.5%`, `97.5%`, Method))))

siml_gam_normal_mean <- select(simlages_norm_gam, Normal)
siml_gam_tdistr_mean <- select(simlages_tdistr_gam, tdistr)
siml_gam_bgar_mean <- select(simlages_bgar_gam, bgar1)
siml_gam_bgartdistr_mean <- select(simlages_bgartdistr_gam, bgar1_tdistr)

siml_ages_gam_df <- as.data.frame(cbind(Depth, True_Ages, CRS, 
                                         siml_gam_normal_mean, siml_gam_tdistr_mean)) %>% 
  select(Depth, True_Ages, CRS, Normal, tdistr) %>%
  pivot_longer(cols = c("True_Ages", "Normal", "tdistr", "CRS"), 
               names_to = "Method", values_to = "Age") 

siml_ages_gam_bgar_df <- as.data.frame(cbind(Depth, True_Ages, CRS, siml_gam_bgar_mean, siml_gam_bgartdistr_mean)) %>% 
  select(Depth, True_Ages, CRS, bgar1, bgar1_tdistr) %>%
  pivot_longer(cols = c("True_Ages", "CRS", "bgar1", "bgar1_tdistr"), 
               names_to = "Method", values_to = "Age") 


siml_gam_plot_df <- left_join(siml_ages_gam_df, siml_gam_bci, by = c("Depth", "Method"))
siml_gam_plot_bgar_df <- left_join(siml_ages_gam_bgar_df, siml_gam_bci_bgar, by = c("Depth", "Method"))


p3 <- siml_gam_plot_df %>%
  ggplot(aes(x = Depth, y = Age, col = Method))+
  geom_line(size = 1, alpha = 0.4)+
  geom_errorbar(aes(ymin = `2.5%`, ymax = `97.5%`), size = 1, alpha = 0.4)+
  scale_x_continuous(name="Depth (cm)", breaks = seq(0, 30, 5)) +
  scale_y_continuous(name="Age (Years)", breaks = seq(0, 250, 50))+
  theme_bw()+
  ggtitle("Normal and t-distr, Whole Core Support") +
  theme(legend.position = c(0.2, 0.8), plot.title = element_text(margin = margin(t = 15, b = -15), hjust = 0.5))


p3
p4 <- siml_gam_plot_bgar_df %>%
  ggplot(aes(x = Depth, y = Age, col = Method))+
  geom_line(size = 1, alpha = 0.4)+
  geom_errorbar(aes(ymin = `2.5%`, ymax = `97.5%`), size = 1, alpha = 0.4)+
  scale_x_continuous(name="Depth (cm)", breaks = seq(0, 30, 5)) +
  scale_y_continuous(name="Age (Years)", breaks = seq(0, 250, 50))+
  theme_bw()+
  ggtitle("BGAR(1), Whole Core Support") +
  theme(legend.position = c(0.2, 0.8), plot.title = element_text(margin = margin(t = 15, b = -15), hjust = 0.5))

p4

(p1+p2)/(p3+p4)


Depth <- seq(2, 24, by = 2)
CRS <- c(CRS_calc(depths = Depth, siml_supp = ABOR1309_02PU$pb214_activity, 
                siml_conc = ABOR1309_02PU$total_pb210_activity, dens = ABOR1309_02PU$dry_bulk_density_modeled), rep(NA, 3))

ABOR1309_02_norm <- select(ABOR1309_02_initJune2_ages_summ, mean, `2.5%`, `97.5%`)
ABOR1309_02_norm <- data.frame(cbind(ABOR1309_02_norm, rep("Normal", 12)))
colnames(ABOR1309_02_norm) <- c("Normal", "2.5%", "97.5%", "Method")
ABOR1309_02_t <- select(ABOR1309_02_test3June16_ages_summ, mean, `2.5%`, `97.5%`)
ABOR1309_02_t <- data.frame(cbind(ABOR1309_02_t, rep("tdistr", 12)))
colnames(ABOR1309_02_t) <- c("tdistr", "2.5%", "97.5%", "Method")

ABOR1309_02_bci <- as.data.frame(rbind(cbind(Depth, select(ABOR1309_02_norm, `2.5%`, `97.5%`, Method)), 
                                    cbind(Depth, select(ABOR1309_02_t, `2.5%`, `97.5%`, Method))))

ABOR1309_02_norm_mean <- select(ABOR1309_02_norm, Normal)
ABOR1309_02_t_mean <- select(ABOR1309_02_t, tdistr)

ABOR1309_02_df <- as.data.frame(cbind(Depth, CRS, 
                                        ABOR1309_02_norm_mean, ABOR1309_02_t_mean)) %>% 
  select(Depth, CRS, Normal, tdistr) %>%
  pivot_longer(cols = c("Normal", "tdistr", "CRS"), 
               names_to = "Method", values_to = "Age") 
ABOR1309_02_plot_df <- left_join(ABOR1309_02_df, ABOR1309_02_bci, by = c("Depth", "Method"))

ABOR1309_02_plot_df %>%
  ggplot(aes(x = Depth, y = Age, col = Method))+
  geom_line(size = 1, alpha = 0.4)+
  geom_errorbar(aes(ymin = `2.5%`, ymax = `97.5%`), size = 1, alpha = 0.4)+
  scale_x_continuous(name="Depth (cm)", breaks = seq(0, 30, 5)) +
  scale_y_continuous(name="Age (Years)", breaks = seq(0, 250, 50))+
  theme_bw()+
  ggtitle("Normal and t-distr") +
  theme(legend.position = c(0.2, 0.8), plot.title = element_text(margin = margin(t = 15, b = -15), hjust = 0.5))

