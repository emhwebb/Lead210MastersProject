


#scratch: filter(perc_vuln_siml_df, `Geographic Area Name` == "Census Tract 9501, Clatsop County, Oregon") %>%
#  select(!`Geographic Area Name`) %>%
#  colMeans()

#histogram test
#perc_vuln_subset %>%
#filter(`Geographic Area Name` == "Census Tract 9, Coos County, Oregon") %>%
#  filter(`Geographic Area Name` == "Coos, 9") %>%
#  select(E_EMPLOYED) %>%
#  ggplot(aes(x = E_EMPLOYED))+
#  geom_histogram()+
#  geom_vline(data = select(filter(tdpr_subset, `Geographic Area Name` == "Coos, 9"), E_EMPLOYED), 
#             aes(xintercept = E_EMPLOYED), col = "magenta")+
#  theme_bw()

#violin plot example
#census tract 9
#add in mean also
#perc_vuln_subset %>%
#  filter(`Geographic Area Name` == "Coos, 9") %>%
#  pivot_longer(cols = E_EMPLOYED:E_65PLUS, names_to = "Variable", values_to = "Percentile") %>%
#  ggplot(aes(x = Variable, y = Percentile))+ #pivot does not include sumRank
#  geom_violin()+
#  geom_point(data = filter(tdpr_subset_long, `Geographic Area Name` == "Coos, 9"), 
#             aes(x = Variable, y = Percentile), col = "magenta")+
#  theme_bw()+
#  theme(axis.text.x = element_text(angle = 90))+
#  geom_boxplot(width=0.1)

#comparing specific variables across different locales
perc_vuln_subset %>%
  select(c(`Geographic Area Name`, "E_DISABILITY")) %>%
  ggplot(aes(x = `Geographic Area Name`, y = get("E_DISABILITY")))+
  geom_boxplot(width=0.1)+
  geom_point(data = filter(tdpr_subset_long, Variable == "E_DISABILITY"), 
             aes(x = `Geographic Area Name`, y = Percentile), col = "magenta")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))

perc_vuln_subset %>%
  select(c(`Geographic Area Name`, colnames(perc_vuln_subset)[3])) %>%
  ggplot(aes(x = `Geographic Area Name`, y = get(colnames(perc_vuln_subset)[3])))+
  geom_boxplot(width=0.1)+
  geom_point(data = filter(tdpr_subset_long, Variable == colnames(perc_vuln_subset)[3]), 
             aes(x = `Geographic Area Name`, y = Percentile), col = "magenta")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  ylab(colnames(perc_vuln_subset)[3])+
  xlab("County, Census Tract")

#perc_vuln_subset %>%
#  pivot_longer(cols = E_EMPLOYED:E_65PLUS, names_to = "Variable", values_to = "Percentile") %>%
#  filter(Variable == "E_EMPLOYED") 
