library(tidyverse)
library(patchwork)

Depth <- 1:30
core_dens <- siml_coredens(Depth)
True_mar <- mar(x = Depth, dens = core_dens)

#nonconst
#for gamma spec

nonconst_supp_norm_gam_initJuly28_accurate <- read_csv("run_results/posterior_simulation/prod/siml/July28/nonconstsupp/normal/gamma/nonconst_supp_norm_gam_initJuly28_accurate.csv")
nonconst_supp_t_gam_initJuly28_accurate <- read_csv("run_results/posterior_simulation/prod/siml/July28/nonconstsupp/t/gamma/nonconst_supp_t_gam_initJuly28_accurate.csv")
nonconst_supp_nonconst_norm_gam_initJuly28_accurate <- read_csv("run_results/posterior_simulation/prod/siml/July28/nonconstsupp/normal/nonconst/nonconst_supp_nonconst_norm_gam_initJuly28_accurate.csv")
nonconst_supp_nonconst_t_gam_initJuly28_accurate <- read_csv("run_results/posterior_simulation/prod/siml/July28/nonconstsupp/t/nonconst/nonconst_supp_nonconst_t_gam_initJuly28_accurate.csv")

nonconst_supp_norm_gam_initJuly28_accurate <- core_dens/nonconst_supp_norm_gam_initJuly28_accurate[,2:ncol(nonconst_supp_norm_gam_initJuly28_accurate)]
nonconst_supp_t_gam_initJuly28_accurate <- core_dens/nonconst_supp_t_gam_initJuly28_accurate[,2:ncol(nonconst_supp_t_gam_initJuly28_accurate)]
nonconst_supp_nonconst_norm_gam_initJuly28_accurate <- core_dens/nonconst_supp_nonconst_norm_gam_initJuly28_accurate[,2:ncol(nonconst_supp_nonconst_norm_gam_initJuly28_accurate)]
nonconst_supp_nonconst_t_gam_initJuly28_accurate <- core_dens/nonconst_supp_nonconst_t_gam_initJuly28_accurate[,2:ncol(nonconst_supp_nonconst_t_gam_initJuly28_accurate)]

#
nonconst_supp_norm_gam_initJuly28_accurate_summ <- t(apply(X = nonconst_supp_norm_gam_initJuly28_accurate[,1:ncol(nonconst_supp_norm_gam_initJuly28_accurate)], 
                                                            MARGIN = 2, 
                                                            FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(nonconst_supp_norm_gam_initJuly28_accurate_summ)[1:2] <- c("mean", "sd")
#
nonconst_supp_t_gam_initJuly28_accurate_summ <- t(apply(X = nonconst_supp_t_gam_initJuly28_accurate[,1:ncol(nonconst_supp_t_gam_initJuly28_accurate)], 
                                                           MARGIN = 2, 
                                                           FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(nonconst_supp_t_gam_initJuly28_accurate_summ)[1:2] <- c("mean", "sd")
#
nonconst_supp_nonconst_norm_gam_initJuly28_accurate_summ <- t(apply(X = nonconst_supp_nonconst_norm_gam_initJuly28_accurate[,1:ncol(nonconst_supp_nonconst_norm_gam_initJuly28_accurate)], 
                                                           MARGIN = 2, 
                                                           FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(nonconst_supp_nonconst_norm_gam_initJuly28_accurate_summ)[1:2] <- c("mean", "sd")
#
nonconst_supp_nonconst_t_gam_initJuly28_accurate_summ <- t(apply(X = nonconst_supp_nonconst_t_gam_initJuly28_accurate[,1:ncol(nonconst_supp_nonconst_t_gam_initJuly28_accurate)], 
                                                           MARGIN = 2, 
                                                           FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(nonconst_supp_nonconst_t_gam_initJuly28_accurate_summ)[1:2] <- c("mean", "sd")

nonconst_supp_norm_gam_initJuly28_accurate_summ <- as.data.frame(nonconst_supp_norm_gam_initJuly28_accurate_summ)
nonconst_supp_t_gam_initJuly28_accurate_summ <- as.data.frame(nonconst_supp_t_gam_initJuly28_accurate_summ)
nonconst_supp_nonconst_norm_gam_initJuly28_accurate_summ <- as.data.frame(nonconst_supp_nonconst_norm_gam_initJuly28_accurate_summ)
nonconst_supp_nonconst_t_gam_initJuly28_accurate_summ <- as.data.frame(nonconst_supp_nonconst_t_gam_initJuly28_accurate_summ)

simlmar_norm_gam <- select(nonconst_supp_norm_gam_initJuly28_accurate_summ, `50%`, `2.5%`, `97.5%`)
simlmar_norm_gam <- data.frame(cbind(simlmar_norm_gam, rep("Normal", 30)))
colnames(simlmar_norm_gam) <- c("Normal", "2.5%", "97.5%", "Method")
simlmar_tdistr_gam <- select(nonconst_supp_t_gam_initJuly28_accurate_summ, `50%`, `2.5%`, `97.5%`)
simlmar_tdistr_gam <- data.frame(cbind(simlmar_tdistr_gam, rep("tdistr", 30)))
colnames(simlmar_tdistr_gam) <- c("tdistr", "2.5%", "97.5%", "Method")

siml_gam_bci <- as.data.frame(rbind(cbind(Depth, select(simlmar_norm_gam, `2.5%`, `97.5%`, Method)), 
                                    cbind(Depth, select(simlmar_tdistr_gam, `2.5%`, `97.5%`, Method))))

#nonconst normal and t models
simlmar_norm_nonconst <- select(nonconst_supp_nonconst_norm_gam_initJuly28_accurate_summ, `50%`, `2.5%`, `97.5%`)
simlmar_norm_nonconst <- data.frame(cbind(simlmar_norm_nonconst, rep("Normal", 30)))
colnames(simlmar_norm_nonconst) <- c("Normal", "2.5%", "97.5%", "Method")
simlmar_tdistr_nonconst <- select(nonconst_supp_nonconst_t_gam_initJuly28_accurate_summ, `50%`, `2.5%`, `97.5%`)
simlmar_tdistr_nonconst <- data.frame(cbind(simlmar_tdistr_nonconst, rep("tdistr", 30)))
colnames(simlmar_tdistr_nonconst) <- c("tdistr", "2.5%", "97.5%", "Method")


siml_nonconst_bci <- as.data.frame(rbind(cbind(Depth, select(simlmar_norm_nonconst, `2.5%`, `97.5%`, Method)), 
                                         cbind(Depth, select(simlmar_tdistr_nonconst, `2.5%`, `97.5%`, Method))))

siml_gam_normal_mean <- select(simlmar_norm_gam, Normal)
siml_gam_tdistr_mean <- select(simlmar_tdistr_gam, tdistr)
siml_nonconst_normal_mean <- select(simlmar_norm_nonconst, Normal)
siml_nonconst_tdistr_mean <- select(simlmar_tdistr_nonconst, tdistr)

siml_mar_gam_df <- as.data.frame(cbind(Depth, True_mar, 
                                        siml_gam_normal_mean, siml_gam_tdistr_mean)) %>% 
  select(Depth, True_mar, Normal, tdistr) %>%
  pivot_longer(cols = c("True_mar", "Normal", "tdistr"), 
               names_to = "Method", values_to = "Age") 

siml_gam_plot_df <- left_join(siml_mar_gam_df, siml_gam_bci, by = c("Depth", "Method"))

siml_mar_nonconst_df <- as.data.frame(cbind(Depth, True_mar, 
                                             siml_nonconst_normal_mean, siml_nonconst_tdistr_mean)) %>% 
  select(Depth, True_mar, Normal, tdistr) %>%
  pivot_longer(cols = c("True_mar", "Normal", "tdistr"), 
               names_to = "Method", values_to = "Age") 

siml_nonconst_plot_df <- left_join(siml_mar_nonconst_df, siml_nonconst_bci, by = c("Depth", "Method"))

p1 <- siml_nonconst_plot_df %>%
  ggplot(aes(x = Depth, y = Age, col = Method))+
  geom_line(size = 1, alpha = 0.4)+
  geom_errorbar(aes(ymin = `2.5%`, ymax = `97.5%`), size = 1, alpha = 0.4)+
  scale_x_continuous(name="Depth (cm)", breaks = seq(0, 30, 5)) +
  scale_y_continuous(name= bquote("Median MAR " (~g~cm^-2~yr^-1)), breaks = seq(0, 250, 1))+
  theme_bw()+
  ggtitle("Nonconst Support Treated as Nonconst Support") +
  theme(legend.position = c(0.2, 0.8), plot.title = element_text(margin = margin(t = 15, b = -15), hjust = 0.5))

p1

p2 <- siml_gam_plot_df %>%
  ggplot(aes(x = Depth, y = Age, col = Method))+
  geom_line(size = 1, alpha = 0.4)+
  geom_errorbar(aes(ymin = `2.5%`, ymax = `97.5%`), size = 1, alpha = 0.4)+
  scale_x_continuous(name="Depth (cm)", breaks = seq(0, 30, 5)) +
  scale_y_continuous(name= bquote("Median MAR " (~g~cm^-2~yr^-1)), breaks = seq(0, 20, 1))+
  theme_bw()+
  ggtitle("Nonconst Support Treated as Constant Support") +
  theme(legend.position = c(0.2, 0.8), plot.title = element_text(margin = margin(t = 15, b = -15), hjust = 0.5))
p2

#const
#for gamma spec

const_supp_norm_gam_initJuly28_accurate <- read_csv("run_results/posterior_simulation/prod/siml/July28/constsupp/normal/gamma/const_supp_norm_gam_initJuly28_accurate.csv")
const_supp_t_gam_initJuly28_accurate <- read_csv("run_results/posterior_simulation/prod/siml/July28/constsupp/t/gamma/const_supp_t_gam_initJuly28_accurate.csv")
const_supp_nonconst_norm_gam_initJuly28_accurate <- read_csv("run_results/posterior_simulation/prod/siml/July28/constsupp/normal/nonconst/const_supp_nonconst_norm_gam_initJuly28_accurate.csv")
const_supp_nonconst_t_gam_initJuly28_accurate <- read_csv("run_results/posterior_simulation/prod/siml/July28/constsupp/t/nonconst/const_supp_nonconst_t_gam_initJuly28_accurate.csv")


const_supp_norm_gam_initJuly28_accurate <- core_dens/const_supp_norm_gam_initJuly28_accurate[,2:ncol(const_supp_norm_gam_initJuly28_accurate)]
const_supp_t_gam_initJuly28_accurate <- core_dens/const_supp_t_gam_initJuly28_accurate[,2:ncol(const_supp_t_gam_initJuly28_accurate)]
const_supp_nonconst_norm_gam_initJuly28_accurate <- core_dens/const_supp_nonconst_norm_gam_initJuly28_accurate[,2:ncol(const_supp_nonconst_norm_gam_initJuly28_accurate)]
const_supp_nonconst_t_gam_initJuly28_accurate <- core_dens/const_supp_nonconst_t_gam_initJuly28_accurate[,2:ncol(const_supp_nonconst_t_gam_initJuly28_accurate)]

#
const_supp_norm_gam_initJuly28_accurate_summ <- t(apply(X = const_supp_norm_gam_initJuly28_accurate[,1:ncol(const_supp_norm_gam_initJuly28_accurate)], 
                                                           MARGIN = 2, 
                                                           FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(const_supp_norm_gam_initJuly28_accurate_summ)[1:2] <- c("mean", "sd")
#
const_supp_t_gam_initJuly28_accurate_summ <- t(apply(X = const_supp_t_gam_initJuly28_accurate[,1:ncol(const_supp_t_gam_initJuly28_accurate)], 
                                                        MARGIN = 2, 
                                                        FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(const_supp_t_gam_initJuly28_accurate_summ)[1:2] <- c("mean", "sd")
#
const_supp_nonconst_norm_gam_initJuly28_accurate_summ <- t(apply(X = const_supp_nonconst_norm_gam_initJuly28_accurate[,1:ncol(const_supp_nonconst_norm_gam_initJuly28_accurate)], 
                                                                    MARGIN = 2, 
                                                                    FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(const_supp_nonconst_norm_gam_initJuly28_accurate_summ)[1:2] <- c("mean", "sd")
#
const_supp_nonconst_t_gam_initJuly28_accurate_summ <- t(apply(X = const_supp_nonconst_t_gam_initJuly28_accurate[,1:ncol(const_supp_nonconst_t_gam_initJuly28_accurate)], 
                                                                 MARGIN = 2, 
                                                                 FUN = function(a) c(mean(a), sd(a), quantile(a, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
colnames(const_supp_nonconst_t_gam_initJuly28_accurate_summ)[1:2] <- c("mean", "sd")

const_supp_norm_gam_initJuly28_accurate_summ <- as.data.frame(const_supp_norm_gam_initJuly28_accurate_summ)
const_supp_t_gam_initJuly28_accurate_summ <- as.data.frame(const_supp_t_gam_initJuly28_accurate_summ)
const_supp_nonconst_norm_gam_initJuly28_accurate_summ <- as.data.frame(const_supp_nonconst_norm_gam_initJuly28_accurate_summ)
const_supp_nonconst_t_gam_initJuly28_accurate_summ <- as.data.frame(const_supp_nonconst_t_gam_initJuly28_accurate_summ)

simlmar_const_norm_gam <- select(const_supp_norm_gam_initJuly28_accurate_summ, `50%`, `2.5%`, `97.5%`)
simlmar_const_norm_gam <- data.frame(cbind(simlmar_const_norm_gam, rep("Normal", 30)))
colnames(simlmar_const_norm_gam) <- c("Normal", "2.5%", "97.5%", "Method")
simlmar_const_tdistr_gam <- select(const_supp_t_gam_initJuly28_accurate_summ, `50%`, `2.5%`, `97.5%`)
simlmar_const_tdistr_gam <- data.frame(cbind(simlmar_const_tdistr_gam, rep("tdistr", 30)))
colnames(simlmar_const_tdistr_gam) <- c("tdistr", "2.5%", "97.5%", "Method")

siml_const_gam_bci <- as.data.frame(rbind(cbind(Depth, select(simlmar_const_norm_gam, `2.5%`, `97.5%`, Method)), 
                                    cbind(Depth, select(simlmar_const_tdistr_gam, `2.5%`, `97.5%`, Method))))

#nonconst normal and t models
simlmar_const_norm_nonconst <- select(const_supp_nonconst_norm_gam_initJuly28_accurate_summ, `50%`, `2.5%`, `97.5%`)
simlmar_const_norm_nonconst <- data.frame(cbind(simlmar_const_norm_nonconst, rep("Normal", 30)))
colnames(simlmar_const_norm_nonconst) <- c("Normal", "2.5%", "97.5%", "Method")
simlmar_const_tdistr_nonconst <- select(const_supp_nonconst_t_gam_initJuly28_accurate_summ, `50%`, `2.5%`, `97.5%`)
simlmar_const_tdistr_nonconst <- data.frame(cbind(simlmar_const_tdistr_nonconst, rep("tdistr", 30)))
colnames(simlmar_const_tdistr_nonconst) <- c("tdistr", "2.5%", "97.5%", "Method")


siml_const_nonconst_bci <- as.data.frame(rbind(cbind(Depth, select(simlmar_const_norm_nonconst, `2.5%`, `97.5%`, Method)), 
                                         cbind(Depth, select(simlmar_const_tdistr_nonconst, `2.5%`, `97.5%`, Method))))

siml_const_gam_normal_mean <- select(simlmar_const_norm_gam, Normal)
siml_const_gam_tdistr_mean <- select(simlmar_const_tdistr_gam, tdistr)
siml_const_nonconst_normal_mean <- select(simlmar_const_norm_nonconst, Normal)
siml_const_nonconst_tdistr_mean <- select(simlmar_const_tdistr_nonconst, tdistr)

siml_mar_const_gam_df <- as.data.frame(cbind(Depth, True_mar, 
                                       siml_const_gam_normal_mean, siml_const_gam_tdistr_mean)) %>% 
  select(Depth, True_mar, Normal, tdistr) %>%
  pivot_longer(cols = c("True_mar", "Normal", "tdistr"), 
               names_to = "Method", values_to = "Age") 

siml_const_gam_plot_df <- left_join(siml_mar_const_gam_df, siml_const_gam_bci, by = c("Depth", "Method"))

siml_mar_const_nonconst_df <- as.data.frame(cbind(Depth, True_mar, 
                                            siml_const_nonconst_normal_mean, siml_const_nonconst_tdistr_mean)) %>% 
  select(Depth, True_mar, Normal, tdistr) %>%
  pivot_longer(cols = c("True_mar", "Normal", "tdistr"), 
               names_to = "Method", values_to = "Age") 

siml_const_nonconst_plot_df <- left_join(siml_mar_const_nonconst_df, siml_const_nonconst_bci, by = c("Depth", "Method"))

p4 <- siml_const_gam_plot_df %>%
  ggplot(aes(x = Depth, y = Age, col = Method))+
  geom_line(size = 1, alpha = 0.4)+
  geom_errorbar(aes(ymin = `2.5%`, ymax = `97.5%`), size = 1, alpha = 0.4)+
  scale_x_continuous(name="Depth (cm)", breaks = seq(0, 30, 5)) +
  scale_y_continuous(name= bquote("Median MAR " (~g~cm^-2~yr^-1)), breaks = seq(0, 30, 1.5))+
  theme_bw()+
  ggtitle("Constant Support Treated as Constant Support") +
  theme(legend.position = c(0.2, 0.8), plot.title = element_text(margin = margin(t = 15, b = -15), hjust = 0.5))

p4

p5 <- siml_const_nonconst_plot_df %>%
  ggplot(aes(x = Depth, y = Age, col = Method))+
  geom_line(size = 1, alpha = 0.4)+
  geom_errorbar(aes(ymin = `2.5%`, ymax = `97.5%`), size = 1, alpha = 0.4)+
  scale_x_continuous(name="Depth (cm)", breaks = seq(0, 30, 5)) +
  scale_y_continuous(name= bquote("Median MAR " (~g~cm^-2~yr^-1)), breaks = seq(0, 20, 1.5))+
  theme_bw()+
  ggtitle("Constant Support Treated as Nonconst Support") +
  theme(legend.position = c(0.2, 0.8), plot.title = element_text(margin = margin(t = 15, b = -15), hjust = 0.5))

p5

(p1+p2)/(p4+p5)
