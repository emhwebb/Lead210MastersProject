library(tidyverse)
library(readr)
library(patchwork)

#nonconst
#for 1:27 with alpha-spectrometry est. 
Depth <- 1:27
True_Ages <- siml_age(1:27)
CRS <- c(CRS_calc(depths = 1:30, 
                  siml_supp = rep(mean(siml_conc2[28:30]), 30), 
                  siml_conc = siml_conc2,
                  dens = siml_coredens(1:30))[1:26], NA)

simlages_norm_alph <- select(const_supp_siml_1_initJuly13_ages_summ, mean, `2.5%`, `97.5%`)
simlages_norm_alph <- data.frame(cbind(simlages_norm_alph, rep("Normal", 27)))
colnames(simlages_norm_alph) <- c("Normal", "2.5%", "97.5%", "Method")
simlages_tdistr_alph <- select(const_supp_siml_tdistr_1_initJuly13_ages_summ, mean, `2.5%`, `97.5%`)
simlages_tdistr_alph <- data.frame(cbind(simlages_tdistr_alph, rep("tdistr", 27)))
colnames(simlages_tdistr_alph) <- c("tdistr", "2.5%", "97.5%", "Method")


siml_alph_bci <- as.data.frame(rbind(cbind(Depth, select(simlages_norm_alph, `2.5%`, `97.5%`, Method)), 
                                     cbind(Depth, select(simlages_tdistr_alph, `2.5%`, `97.5%`, Method))))

siml_alph_normal_mean <- select(simlages_norm_alph, Normal)
siml_alph_tdistr_mean <- select(simlages_tdistr_alph, tdistr)

siml_ages_alph_df <- as.data.frame(cbind(Depth, True_Ages, CRS, 
                                         siml_alph_normal_mean, siml_alph_tdistr_mean)) %>% 
  select(Depth, True_Ages, CRS, Normal, tdistr) %>%
  pivot_longer(cols = c("True_Ages", "Normal", "tdistr", "CRS"), 
               names_to = "Method", values_to = "Age") 

siml_alph_plot_df <- left_join(siml_ages_alph_df, siml_alph_bci, by = c("Depth", "Method"))

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


#nonconst
#for gamma spec
Depth <- 1:30
True_Ages <- siml_age(1:30)
#CRS <- c(CRS_calc(depths = 1:30, siml_supp = siml_supp, siml_conc = siml_conc)[2:26], rep(NA, 5))
CRS <- c(CRS_calc(depths = 1:30, 
                  siml_supp = siml_supp2, 
                  siml_conc = siml_conc2,
                  dens = siml_coredens(1:30))[1:26], rep(NA, 4))

simlages_norm_gam <- select(nonconst_supp_norm_gam_initJuly28_ages_summ, mean, `2.5%`, `97.5%`)
simlages_norm_gam <- data.frame(cbind(simlages_norm_gam, rep("Normal", 30)))
colnames(simlages_norm_gam) <- c("Normal", "2.5%", "97.5%", "Method")
simlages_tdistr_gam <- select(nonconst_supp_t_gam_initJuly28_ages_summ, mean, `2.5%`, `97.5%`)
simlages_tdistr_gam <- data.frame(cbind(simlages_tdistr_gam, rep("tdistr", 30)))
colnames(simlages_tdistr_gam) <- c("tdistr", "2.5%", "97.5%", "Method")

siml_gam_bci <- as.data.frame(rbind(cbind(Depth, select(simlages_norm_gam, `2.5%`, `97.5%`, Method)), 
                                         cbind(Depth, select(simlages_tdistr_gam, `2.5%`, `97.5%`, Method))))

#nonconst normal and t models
simlages_norm_nonconst <- select(nonconst_supp_nonconst_norm_gam_initJuly28_ages_summ, mean, `2.5%`, `97.5%`)
simlages_norm_nonconst <- data.frame(cbind(simlages_norm_nonconst, rep("Normal", 30)))
colnames(simlages_norm_nonconst) <- c("Normal", "2.5%", "97.5%", "Method")
simlages_tdistr_nonconst <- select(nonconst_supp_nonconst_t_gam_initJuly28_ages_summ, mean, `2.5%`, `97.5%`)
simlages_tdistr_nonconst <- data.frame(cbind(simlages_tdistr_nonconst, rep("tdistr", 30)))
colnames(simlages_tdistr_nonconst) <- c("tdistr", "2.5%", "97.5%", "Method")


siml_nonconst_bci <- as.data.frame(rbind(cbind(Depth, select(simlages_norm_nonconst, `2.5%`, `97.5%`, Method)), 
                                    cbind(Depth, select(simlages_tdistr_nonconst, `2.5%`, `97.5%`, Method))))

siml_gam_normal_mean <- select(simlages_norm_gam, Normal)
siml_gam_tdistr_mean <- select(simlages_tdistr_gam, tdistr)
siml_nonconst_normal_mean <- select(simlages_norm_nonconst, Normal)
siml_nonconst_tdistr_mean <- select(simlages_tdistr_nonconst, tdistr)

siml_ages_gam_df <- as.data.frame(cbind(Depth, True_Ages, CRS, 
                                        siml_gam_normal_mean, siml_gam_tdistr_mean)) %>% 
  select(Depth, True_Ages, CRS, Normal, tdistr) %>%
  pivot_longer(cols = c("True_Ages", "Normal", "tdistr", "CRS"), 
               names_to = "Method", values_to = "Age") 

siml_gam_plot_df <- left_join(siml_ages_gam_df, siml_gam_bci, by = c("Depth", "Method"))

siml_ages_nonconst_df <- as.data.frame(cbind(Depth, True_Ages, CRS, 
                                             siml_nonconst_normal_mean, siml_nonconst_tdistr_mean)) %>% 
  select(Depth, True_Ages, CRS, Normal, tdistr) %>%
  pivot_longer(cols = c("True_Ages", "Normal", "tdistr", "CRS"), 
               names_to = "Method", values_to = "Age") 

siml_nonconst_plot_df <- left_join(siml_ages_nonconst_df, siml_nonconst_bci, by = c("Depth", "Method"))

p1 <- siml_nonconst_plot_df %>%
  ggplot(aes(x = Depth, y = Age, col = Method))+
  geom_line(size = 1, alpha = 0.4)+
  geom_errorbar(aes(ymin = `2.5%`, ymax = `97.5%`), size = 1, alpha = 0.4)+
  scale_x_continuous(name="Depth (cm)", breaks = seq(0, 30, 5)) +
  scale_y_continuous(name="Age (Years)", breaks = seq(0, 250, 50))+
  theme_bw()+
  ggtitle("Nonconst Support Treated as Nonconst Support") +
  theme(legend.position = c(0.2, 0.8), plot.title = element_text(margin = margin(t = 15, b = -15), hjust = 0.5))

p1

p2 <- siml_gam_plot_df %>%
  ggplot(aes(x = Depth, y = Age, col = Method))+
  geom_line(size = 1, alpha = 0.4)+
  geom_errorbar(aes(ymin = `2.5%`, ymax = `97.5%`), size = 1, alpha = 0.4)+
  scale_x_continuous(name="Depth (cm)", breaks = seq(0, 30, 5)) +
  scale_y_continuous(name="Age (Years)", breaks = seq(0, 250, 50))+
  theme_bw()+
  ggtitle("Nonconst Support Treated as Constant Support") +
  theme(legend.position = c(0.2, 0.8), plot.title = element_text(margin = margin(t = 15, b = -15), hjust = 0.5))

p2

#const
#for 1:27 with alpha-spectrometry est. 
Depth <- 1:27
True_Ages <- siml_age(1:27)
CRS <- c(CRS_calc(depths = 1:30, 
                  siml_supp = rep(mean(siml_conc2[28:30]), 30), 
                  siml_conc = siml_conc2,
                  dens = siml_coredens(1:30))[1:26], NA)

simlages_norm_alph <- select(const_supp_siml_1_initJuly13_ages_summ, mean, `2.5%`, `97.5%`)
simlages_norm_alph <- data.frame(cbind(simlages_norm_alph, rep("Normal", 27)))
colnames(simlages_norm_alph) <- c("Normal", "2.5%", "97.5%", "Method")
simlages_tdistr_alph <- select(const_supp_siml_tdistr_1_initJuly13_ages_summ, mean, `2.5%`, `97.5%`)
simlages_tdistr_alph <- data.frame(cbind(simlages_tdistr_alph, rep("tdistr", 27)))
colnames(simlages_tdistr_alph) <- c("tdistr", "2.5%", "97.5%", "Method")


siml_alph_bci <- as.data.frame(rbind(cbind(Depth, select(simlages_norm_alph, `2.5%`, `97.5%`, Method)), 
                                     cbind(Depth, select(simlages_tdistr_alph, `2.5%`, `97.5%`, Method))))

siml_alph_normal_mean <- select(simlages_norm_alph, Normal)
siml_alph_tdistr_mean <- select(simlages_tdistr_alph, tdistr)

siml_ages_alph_df <- as.data.frame(cbind(Depth, True_Ages, CRS, 
                                         siml_alph_normal_mean, siml_alph_tdistr_mean)) %>% 
  select(Depth, True_Ages, CRS, Normal, tdistr) %>%
  pivot_longer(cols = c("True_Ages", "Normal", "tdistr", "CRS"), 
               names_to = "Method", values_to = "Age") 

siml_alph_plot_df <- left_join(siml_ages_alph_df, siml_alph_bci, by = c("Depth", "Method"))

p4 <- siml_alph_plot_df %>%
  ggplot(aes(x = Depth, y = Age, col = Method))+
  geom_line(size = 1, alpha = 0.4)+
  geom_errorbar(aes(ymin = `2.5%`, ymax = `97.5%`), size = 1, alpha = 0.4)+
  scale_x_continuous(name="Depth (cm)", breaks = seq(0, 30, 5)) +
  scale_y_continuous(name="Age (Years)", breaks = seq(0, 250, 50))+
  theme_bw()+
  ggtitle("Normal and t-distr, Bottom 3 supported") +
  theme(legend.position = c(0.2, 0.8), plot.title = element_text(margin = margin(t = 15, b = -15), hjust = 0.5))


p4


#const
#for gamma spec
Depth <- 1:30
True_Ages <- siml_age(1:30)
#CRS <- c(CRS_calc(depths = 1:30, siml_supp = siml_supp, siml_conc = siml_conc)[2:26], rep(NA, 5))
CRS <- c(CRS_calc(depths = 1:30, 
                  siml_supp = siml_supp3, 
                  siml_conc = siml_conc3,
                  dens = siml_coredens(1:30))[1:26], rep(NA, 4))

simlages_norm_gam_const <- select(const_supp_norm_gam_initJuly28_ages_summ, mean, `2.5%`, `97.5%`)
simlages_norm_gam_const <- data.frame(cbind(simlages_norm_gam_const, rep("Normal", 30)))
colnames(simlages_norm_gam_const) <- c("Normal", "2.5%", "97.5%", "Method")
simlages_tdistr_gam_const <- select(const_supp_t_gam_initJuly28_ages_summ, mean, `2.5%`, `97.5%`)
simlages_tdistr_gam_const <- data.frame(cbind(simlages_tdistr_gam_const, rep("tdistr", 30)))
colnames(simlages_tdistr_gam_const) <- c("tdistr", "2.5%", "97.5%", "Method")

siml_const_bci <- as.data.frame(rbind(cbind(Depth, select(simlages_norm_gam_const, `2.5%`, `97.5%`, Method)), 
                                               cbind(Depth, select(simlages_tdistr_gam_const, `2.5%`, `97.5%`, Method))))

#nonconst normal and t models
simlages_norm_const_nonconst <- select(const_supp_nonconst_norm_gam_initJuly28_ages_summ, mean, `2.5%`, `97.5%`)
simlages_norm_const_nonconst <- data.frame(cbind(simlages_norm_const_nonconst, rep("Normal", 30)))
colnames(simlages_norm_const_nonconst) <- c("Normal", "2.5%", "97.5%", "Method")
simlages_tdistr_const_nonconst <- select(const_supp_nonconst_t_gam_initJuly28_ages_summ, mean, `2.5%`, `97.5%`)
simlages_tdistr_const_nonconst <- data.frame(cbind(simlages_tdistr_const_nonconst, rep("tdistr", 30)))
colnames(simlages_tdistr_const_nonconst) <- c("tdistr", "2.5%", "97.5%", "Method")


siml_const_nonconst_bci <- as.data.frame(rbind(cbind(Depth, select(simlages_norm_const_nonconst, `2.5%`, `97.5%`, Method)), 
                                    cbind(Depth, select(simlages_tdistr_gam, `2.5%`, `97.5%`, Method))))

siml_gam_const_normal_mean <- select(simlages_norm_gam_const, Normal)
siml_gam_const_tdistr_mean <- select(simlages_tdistr_gam_const, tdistr)
siml_gam_const_nonconst_normal_mean <- select(simlages_norm_const_nonconst, Normal)
siml_gam_const_nonconst_tdistr_mean <- select(simlages_tdistr_const_nonconst, tdistr)



siml_ages_const_gam_df <- as.data.frame(cbind(Depth, True_Ages, CRS, 
                                        siml_gam_const_normal_mean, siml_gam_const_tdistr_mean)) %>% 
  select(Depth, True_Ages, CRS, Normal, tdistr) %>%
  pivot_longer(cols = c("True_Ages", "Normal", "tdistr", "CRS"), 
               names_to = "Method", values_to = "Age") 

siml_gam_const_plot_df <- left_join(siml_ages_const_gam_df, siml_const_bci, by = c("Depth", "Method"))

siml_ages_gam_const_nonconst_df <- as.data.frame(cbind(Depth, True_Ages, CRS, 
                                        siml_gam_const_nonconst_normal_mean, siml_gam_const_nonconst_tdistr_mean)) %>% 
  select(Depth, True_Ages, CRS, Normal, tdistr) %>%
  pivot_longer(cols = c("True_Ages", "Normal", "tdistr", "CRS"), 
               names_to = "Method", values_to = "Age") 

siml_gam_const_nonconst_plot_df <- left_join(siml_ages_gam_const_nonconst_df, siml_const_nonconst_bci, by = c("Depth", "Method"))

p4 <- siml_gam_const_plot_df %>%
  ggplot(aes(x = Depth, y = Age, col = Method))+
  geom_line(size = 1, alpha = 0.4)+
  geom_errorbar(aes(ymin = `2.5%`, ymax = `97.5%`), size = 1, alpha = 0.4)+
  scale_x_continuous(name="Depth (cm)", breaks = seq(0, 30, 5)) +
  scale_y_continuous(name="Age (Years)", breaks = seq(0, 250, 50))+
  theme_bw()+
  ggtitle("Constant Support Treated as Constant Support") +
  theme(legend.position = c(0.2, 0.8), plot.title = element_text(margin = margin(t = 15, b = -15), hjust = 0.5))

p4

p5 <- siml_gam_const_nonconst_plot_df %>%
  ggplot(aes(x = Depth, y = Age, col = Method))+
  geom_line(size = 1, alpha = 0.4)+
  geom_errorbar(aes(ymin = `2.5%`, ymax = `97.5%`), size = 1, alpha = 0.4)+
  scale_x_continuous(name="Depth (cm)", breaks = seq(0, 30, 5)) +
  scale_y_continuous(name="Age (Years)", breaks = seq(0, 250, 50))+
  theme_bw()+
  ggtitle("Constant Support Treated as Nonconst Support") +
  theme(legend.position = c(0.2, 0.8), plot.title = element_text(margin = margin(t = 15, b = -15), hjust = 0.5))

p5

#(p1+p2)/(p3+p4)

(p1+p2)/(p4+p5)
