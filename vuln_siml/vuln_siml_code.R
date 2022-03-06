####For the vulnerability index simulation
#load packages
library(readxl)
library(readr)
library(tidyverse)

##now to do final dataset with all the census tracts
#load and skip first line, make sure to have correct file path
#tdvuln_df <- read_csv("vulnerabilitydata_final.csv", skip = 1)
tdvuln_df <- read_csv("D:/Downloads/vulnerabilitydata_final.csv", skip = 1)
tdvuln_df2 <- tdvuln_df[1:169,]

##This next code chunk is to rank all the included counties and census tracts together
#Names of counties + census tracts
#first filter out the 9900 and 9901 census tracts (which are bodies of water)
tdvuln_df2_filt <- tdvuln_df2 %>%
  filter(!grepl("9900|9901", tdvuln_df2$`Geographic Area Name`)) 
`Geographic Area Name` <- tdvuln_df2_filt$`Geographic Area Name`

#vars that are reversed
#employed, citizen, insured, and income.

#for adjusting to population per 100000
totalpop_adj <- tdvuln_df2 %>%
  filter(!grepl("9900|9901", tdvuln_df2$`Geographic Area Name`)) %>%
  select(E_TOTALPOP) %>%
  mutate(E_TOTALPOP = 100000/E_TOTALPOP) %>%
  as.matrix()
  
#compute the percentile ranks
tdpr_df <- tdvuln_df2 %>%
  filter(!grepl("9900|9901", tdvuln_df2$`Geographic Area Name`)) %>% #take out the 9900 and 9901 census tract rows
  select(-c(E_TOTALPOP, M_TOTALPOP)) %>% #deselect the E_TOTALPOP and M_TOTALPOP
  select(grep("+M_|Name|+E_PCT", colnames(.), invert = T)) %>% #Take out Name, MOE (M_) and E_PCT columns 
  mutate(E_EMPLOYED = -E_EMPLOYED, E_CITIZEN = -E_CITIZEN, E_INSURED = -E_INSURED, E_INCOME = -as.numeric(E_INCOME)) %>% #reversing EMPLOYED, CITIZEN, INSURED, AND INCOME
  mutate_at(vars(-E_INCOME), .funs = ~.*totalpop_adj) %>% #adjust so that counts are per 100000
  apply(., MARGIN = 2, FUN = function(x) rank(x, na.last = T, ties.method = c("first"))) %>%  #calculate ranks for remaining columns
  apply(., MARGIN = 2, FUN = function(x) (x-1)/(length(x)-1)) %>% #calculate percentiles for each of the columns
  as.data.frame() %>% #turn into data frame to use mutate
  mutate(sum_rank = round(rowSums(.), 2)) %>% #calculate the sum rank and round
  round(., 2) %>%  #round the entire data frame
  cbind(`Geographic Area Name`, .) #add back in the geographic names
  
#Can also filter to see the rankings of the list of 8 census tracts interested in:
tdpr_subset <- tdpr_df %>%
  filter(grepl("9, Coos|10, Coos|11, Coos|9515, Lincoln|9516, Lincoln|9517, Lincoln|9601, Tillamook|9602, Tillamook", 
               tdpr_df$`Geographic Area Name`)) 
#shorter names for geographic area (tract #, county)
tract_county <- str_remove_all(tdpr_subset$`Geographic Area Name`, "Census Tract | County, Oregon")
tract_county <- gsub("([0-9]+), ([A-Za-z]+)", "\\2, \\1", tract_county)
tdpr_subset <- tdpr_subset %>%
  mutate(`Geographic Area Name` = tract_county)
#convert to longer format
tdpr_subset_long <- tdpr_subset %>%
  pivot_longer(cols = E_EMPLOYED:E_65PLUS, names_to = "Variable", values_to = "Percentile")

tdct_df <- tdvuln_df2 %>%
  filter(!grepl("9900|9901", tdvuln_df2$`Geographic Area Name`)) %>% #take out the 9900 and 9901 census tract rows
  select(-c(E_TOTALPOP, M_TOTALPOP)) %>% #deselect the E_TOTALPOP and M_TOTALPOP
  select(grep("+M_|Name|+E_PCT", colnames(.), invert = T)) %>% #Take out Name, MOE (M_) and E_PCT columns 
  mutate(E_EMPLOYED = -E_EMPLOYED, E_CITIZEN = -E_CITIZEN, E_INSURED = -E_INSURED, E_INCOME = -as.numeric(E_INCOME)) %>% #reversing EMPLOYED, CITIZEN, INSURED, AND INCOME
  mutate_at(vars(-E_INCOME), .funs = ~.*totalpop_adj) %>% #adjust so that counts are per 100000
  cbind(`Geographic Area Name`, .) #add back in the geographic names
  
tdct_subset <- tdct_df %>%


tdct_subset <- tdct_subset %>%
  mutate(`Geographic Area Name` = tract_county)
#convert to longer format
tdct_subset_long <- tdct_subset %>%
  pivot_longer(cols = E_EMPLOYED:E_65PLUS, names_to = "Variable", values_to = "Count")

#num siml
#nsim = 1000 takes ~0.25 sec to sample from rnorm() and ~1.7 sec to run percentile rankings around 27 Mb
#nsim = 10000 takes ~2.6 sec to sample from rnorm() and ~16 sec to run percentile rankings around 272 Mb
#expect: nsim = 100000 may take ~26 sec to sample from rnorm() and ~150-160 sec to run percentile rankings around 2700 Mb
#easy way to speed up: parallelize esp want to do siml size larger than 100000 replicates
nsim <- 10000

#filter out for mean and se
estvuln_df <- tdvuln_df2 %>%
  filter(!grepl("9900|9901", tdvuln_df2$`Geographic Area Name`)) %>% #take out the 9900 and 9901 census tract rows
  select(-c(E_TOTALPOP, M_TOTALPOP)) %>% #deselect the E_TOTALPOP and M_TOTALPOP
  select(grep("+M_|Name|+E_PCT", colnames(.), invert = T)) %>%
  mutate(E_EMPLOYED = -E_EMPLOYED, E_CITIZEN = -E_CITIZEN, E_INSURED = -E_INSURED, E_INCOME = -as.numeric(E_INCOME)) %>%
  mutate_at(vars(-E_INCOME), .funs = ~.*totalpop_adj) #adjust so that counts are per 100000
#convert to matrix to help with computation time
estvuln_mat <- as.matrix(estvuln_df)

#divid MOE by 1.645 to obtain approx. SE
#source: https://www.census.gov/content/dam/Census/programs-surveys/acs/guidance/training-presentations/20170419_MOE_Transcript.pdf
sevuln_df <- tdvuln_df2 %>%
  filter(!grepl("9900|9901", tdvuln_df2$`Geographic Area Name`)) %>% #take out the 9900 and 9901 census tract rows
  select(-c(E_TOTALPOP, M_TOTALPOP)) %>% #deselect the E_TOTALPOP and M_TOTALPOP
  select(grep("+E_|Name|+E_PCT|+M_PCT", colnames(.), invert = T)) %>%
  mutate(M_INCOME = as.numeric(M_INCOME)) %>%
  mutate_at(vars(-M_INCOME), ~as.numeric(.)*totalpop_adj/1.645) #recover standard errors
#convert to matrix to help with computation time
sevuln_mat <- as.matrix(sevuln_df)

#filter out for geo names and col names
#col names
var_names <- colnames(estvuln_df)
#geo names
`Geographic Area Name` <- rep(filter(tdvuln_df2, 
                                     !grepl("9900|9901", tdvuln_df2$`Geographic Area Name`))$`Geographic Area Name`, 
                              nsim)

#allocate dim, vectors, matrices (note: should be from tdvuln_df2_filt bc the 9900 and 9901 tracts have been filtered out)
row_num <- nrow(estvuln_df)
col_num <- ncol(estvuln_df)+1 #includes the names so this is one smaller than final size bc add sum rank in operation

#two matrices for mean and se that is nsim*row_num x (col_num-1) [used for drawing from rnorm()]
est_mat <- matrix(NA, nrow = nsim*row_num, ncol = col_num-1)
se_mat <- matrix(NA, nrow = nsim*row_num, ncol = col_num-1)
#a matrix that is nsim*row_num x (col_num-1) for rnorm() draws
rnorm_mat <- matrix(NA, nrow = nsim*row_num, ncol = col_num-1)
#a matrix that is nsim*row_num x col_num for the percentile calculation
perc_vuln_siml_mat <- matrix(NA, nrow = nsim*row_num, ncol = col_num)

#iterate so do i in 1:nsim then have row nums be from (note row_num = 162)
#162*(i-1)+1 to 162*i when subsetting or assigning for matrices
# vectorize where possible

#simulate entries in siml matrix from rnorm() draws
#(i) est_mat and se_mat are entries from estvuln_df and sevuln_df stacked nsim times

for(i in 1:nsim){
  est_mat[(row_num*(i-1)+1):(row_num*i),] <- estvuln_mat
  se_mat[(row_num*(i-1)+1):(row_num*i),] <- sevuln_mat
}

#(ii) use est_mat and se_mat to make draws from normal distribution
#where mean is est_entry from est_mat and sd is the se_entry from se_mat

#for benchmarking
#start_time1 <- Sys.time()

for(i in 1:(col_num-1)){
  rnorm_mat[,i] <- rnorm(n = nsim*row_num, mean = est_mat[,i], sd = se_mat[,i])
}

#the csv for the rnorm_mat
vuln_siml_rnorm_df <- as.data.frame(rnorm_mat)
write.csv(vuln_siml_rnorm_df, "vuln_siml_rnorm_siml2.csv")
#siml2 is with adjusting to population per 100000

###--- for when reading in simulation 
#vuln_siml_rnorm_df <- read_csv("vuln_siml/vuln_siml_rnorm.csv")
#rnorm_mat <- as.matrix(vuln_siml_rnorm_df)[,2:23] 
#vuln_siml_rnorm_df <- select(vuln_siml_rnorm_df, 2:23)
###---

#for benchmarking
#end_time1 <- Sys.time()
#end_time1-start_time1

#apply rank and percentile calculations to obtain percentile matrices
#for benchmarking
#start_time2 <- Sys.time()
for(i in 1:nsim){
  #apply rank then percentile 
  perc_vuln_siml_mat[(row_num*(i-1)+1):(row_num*i), 1:(col_num-1)] <- apply(rnorm_mat[(row_num*(i-1)+1):(row_num*i),], 
                                                                  MARGIN = 2,
                                                                  FUN = function(x) (rank(x, na.last = T, ties.method = c("first"))-1)/(length(x)-1))
  #calculate the sumRank for each row
  perc_vuln_siml_mat[(row_num*(i-1)+1):(row_num*i), col_num] <- rowSums(perc_vuln_siml_mat[(row_num*(i-1)+1):(row_num*i), 1:(col_num-1)])
}
#for benchmarking
#end_time2 <- Sys.time()
#end_time2-start_time2

#assign column names
colnames(perc_vuln_siml_mat) <- c(var_names, "sumRank")
colnames(vuln_siml_rnorm_df) <- var_names #for looking at distribution of simulated counts

#convert matrix to dataframe to use dplyr functions on 
#note that these values are not yet rounded 
#should only be rounded when presenting summary statistics or displaying portions of table.
perc_vuln_siml_df <- cbind(`Geographic Area Name`, as.data.frame(perc_vuln_siml_mat))
vuln_siml_rnorm_df <- cbind(`Geographic Area Name`, vuln_siml_rnorm_df)

#write functions to grab summary statistics on subsets of columns or rows, individual entries 

#computes the mean and variance of each of the columns for each of the census tracts
perc_vuln_siml_summary <- perc_vuln_siml_df %>%
  group_by(`Geographic Area Name`) %>%
  summarize_all(.funs = list(mean = mean, var = var)) %>% #can also do list(mean = mean, var = var, median = median)
  ungroup()

vuln_siml_rnorm_summary <- vuln_siml_rnorm_df %>%
  group_by(`Geographic Area Name`) %>%
  summarize_all(.funs = list(mean = mean, var = var)) %>% #can also do list(mean = mean, var = var, median = median)
  ungroup()

#can also filter simulation by the 8 specific locations we're interested in  
perc_vuln_subset <- perc_vuln_siml_df %>%
  filter(grepl("9, Coos|10, Coos|11, Coos|9515, Lincoln|9516, Lincoln|9517, Lincoln|9601, Tillamook|9602, Tillamook", 
               perc_vuln_siml_df$`Geographic Area Name`)) 
tract_county <- str_remove_all(perc_vuln_subset$`Geographic Area Name`, "Census Tract | County, Oregon")
tract_county <- gsub("([0-9]+), ([A-Za-z]+)", "\\2, \\1", tract_county)

vuln_rnorm_subset <- vuln_siml_rnorm_df %>%
  filter(grepl("9, Coos|10, Coos|11, Coos|9515, Lincoln|9516, Lincoln|9517, Lincoln|9601, Tillamook|9602, Tillamook", 
               vuln_siml_rnorm_df$`Geographic Area Name`))
tract_county <- str_remove_all(vuln_rnorm_subset$`Geographic Area Name`, "Census Tract | County, Oregon")
tract_county <- gsub("([0-9]+), ([A-Za-z]+)", "\\2, \\1", tract_county)

perc_vuln_subset <- perc_vuln_subset %>%
  mutate(`Geographic Area Name` = tract_county)

vuln_rnorm_subset <- vuln_rnorm_subset %>%
  mutate(`Geographic Area Name` = tract_county)
    
#subset summary
perc_vuln_subset_summary <- perc_vuln_subset %>% 
  group_by(`Geographic Area Name`) %>%
  summarize_all(.funs = list(mean = mean, var = var)) %>%
  ungroup()
write.csv(perc_vuln_subset_summary, "perc_vuln_subset_summary_siml2.csv")

vuln_siml_subset_rnorm_summary <- vuln_rnorm_subset %>%
  group_by(`Geographic Area Name`) %>%
  summarize_all(.funs = list(mean = mean, var = var)) %>%
  ungroup()
write.csv(vuln_siml_subset_rnorm_summary, "vuln_siml_subset_rnorm_summary_siml2.csv")
#again siml2 is with adjusting to per 100000

#Code for examining joint relationships, plotting histograms and boxplots

#For plotting the 

var_by_tract_plotfunc <- function(data, tractnum){
  data %>%
    filter(`Geographic Area Name` == unique(data$`Geographic Area Name`)[tractnum]) %>%
    pivot_longer(cols = E_EMPLOYED:E_65PLUS, names_to = "Variable", values_to = "Percentile") %>%
    ggplot(aes(x = Variable, y = Percentile))+ #pivot does not include sumRank
    geom_boxplot()+
    geom_point(data = filter(tdpr_subset_long, `Geographic Area Name` == unique(data$`Geographic Area Name`)[tractnum]), 
               aes(x = Variable, y = Percentile), col = "magenta")+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90))+
    ggtitle(paste0("Simulated Per 100000 Percentile Ranks for ", unique(data$`Geographic Area Name`)[tractnum]),
            subtitle = paste("Magenta Point is Percentile Ranking Estimated from Actual Data", "(nsim = 10000)"))
}

var_by_tract_plots <- lapply(1:8, var_by_tract_plotfunc, data = perc_vuln_subset)
var_by_tract_plots

#sumRank of different locales should be plotted together
perc_vuln_subset %>%
  select(c(`Geographic Area Name`, sumRank)) %>%
  ggplot(aes(x = `Geographic Area Name`, y = sumRank))+
  geom_violin()+
  geom_boxplot(width=0.1)+
  geom_point(data = tdpr_subset, 
             aes(x = `Geographic Area Name`, y = sum_rank), col = "magenta", size = 2)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  ylab("Sum Rank")+
  xlab("County, Census Tract")+
  ggtitle("Per 100000 Sum Rank Compared Across Census Tracts",
          subtitle = paste("Magenta Point is Percentile Ranking Estimated from Actual Data", "(nsim = 10000)"))


#to save each plot of variable compared across tract, county

var_cross_locale_plotfunc <- function(column, data){
  data %>%
    select(c(`Geographic Area Name`, colnames(data)[column])) %>%
    ggplot(aes(x = `Geographic Area Name`, y = get(colnames(data)[column])))+
    geom_boxplot(width=0.1)+
    geom_point(data = filter(tdpr_subset_long, Variable == colnames(data)[column]), 
               aes(x = `Geographic Area Name`, y = Percentile), col = "magenta")+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90))+
    ylab(paste(colnames(data)[column], "(Percentile)"))+
    xlab("County, Census Tract")+
    ggtitle(paste("Per 100000 Simulated Percentile Ranking of", colnames(data)[column], "Across Census Tracts"),
            subtitle = paste("Magenta Point is Percentile Ranking Estimated from Actual Data", "(nsim = 10000)"))
}

var_cross_tract_county <- lapply(2:23, var_cross_locale_plotfunc, data = perc_vuln_subset)
var_cross_tract_county

#to save plot of counts of simulation across variables
var_cross_locale_ct_plotfunc <- function(column, data){
  data %>%
    select(c(`Geographic Area Name`, colnames(data)[column])) %>%
    ggplot(aes(x = `Geographic Area Name`, y = get(colnames(data)[column])))+
    geom_boxplot(width=0.1)+
    geom_point(data = filter(tdct_subset_long, Variable == colnames(data)[column]), 
               aes(x = `Geographic Area Name`, y = Count), col = "magenta")+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90))+
    ylab(paste(colnames(data)[column], "(Count)"))+
    xlab("County, Census Tract")+
    ggtitle(paste("Per 100000 Simulated Count of", colnames(data)[column], "Across Census Tracts"),
            subtitle = paste("Magenta Point is Count from Actual Data", "(nsim = 10000)"))
}

var_cross_tract_county_rnorm <- lapply(2:23, var_cross_locale_ct_plotfunc, data = vuln_rnorm_subset)
var_cross_tract_county_rnorm

