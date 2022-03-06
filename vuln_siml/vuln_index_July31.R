#adapted from Jasmine's code
#load packages
library(readxl)
library(readr)
library(tidyverse)

#-----------------------------------------------------
#------------- PT  1 --------------------------------
#-----------------------------------------------------
#This is with the data from Jasmine's ST599 Final Project
#read in data
#best to first save data as .csv in Excel, then load as a .csv
#This is the dataset from Jasmine's ST599 Final Project
#Change filepath to whatever it is for you
fvul_df <- read_csv("ST599_FinalProjectDataset.csv")
#grabbing non-empty rows and columns
fvul_df2 <- fvul_df[1:12,1:15]

#geographic names
`Geographic Names` <- fvul_df2$`Geographic Area Name`

#rank data frame with total rank
rank_df <- fvul_df2 %>%
  select(grep("+MOE|Name", colnames(.), invert = T)) %>% #takes out the name and MOE columns
  apply(., MARGIN = 2, FUN = function(x) rank(x, na.last = T, ties.method = c("first"))) %>% #ranks remaining columns
  as.data.frame() %>% #turn from matrix to dataframe to use mutate
  mutate(total_rank = rowSums(.)) %>% #calculate the total rank for each location
  cbind(`Geographic Names`, .) #add back in the geographic names

#percentile data frame with sum rank
pr_df <- rank_df %>% #start off with the rank data frame
  select(-c(`Geographic Names`, total_rank)) %>% #take out the geographic names and the total rank
  apply(., MARGIN = 2, FUN = function(x) (x-1)/(length(x)-1)) %>% #calculate the percentiles for each of the columns
  as.data.frame() %>% #turn into data frame to use mutate
  mutate(sum_rank = round(rowSums(.), 2)) %>% #calculate sum rank and round it
  round(., 2) %>% #round the entire data frame 
  cbind(`Geographic Names`, .) #add back in the geographic names

rank_df
pr_df

#--------------------------------------------
#------ PT 2 --------------------------------
#-------------------------------------------
##now to do final dataset with all the census tracts
#load and skip first line, make sure to have correct file path
tdvuln_df <- read_csv("vulnerabilitydata_final.csv", skip = 1)
tdvuln_df2 <- tdvuln_df[1:169,]

##This next code chunk is to rank all the included counties and census tracts together
#Names of counties + census tracts
#first filter out the 9900 and 9901 census tracts (which are bodies of water)
tdvuln_df2_filt <- tdvuln_df2 %>%
  filter(!grepl("9900|9901", tdvuln_df2$`Geographic Area Name`)) 
`Geographic Area Name` <- tdvuln_df2_filt$`Geographic Area Name`

#employed, citizen, insured, and income.

#rank data with total rank
tdrank_df <- tdvuln_df2 %>%
  filter(!grepl("9900|9901", tdvuln_df2$`Geographic Area Name`)) %>% #take out the 9900 and 9901 census tract rows
  select(-c(E_TOTALPOP, M_TOTALPOP)) %>% #deselect the E_TOTALPOP and M_TOTALPOP
  select(grep("+M_|Name|+E_PCT", colnames(.), invert = T)) %>% #Take out Name, MOE (M_) and E_PCT columns 
  mutate(E_EMPLOYED = -E_EMPLOYED, E_CITIZEN = -E_CITIZEN, E_INSURED = -E_INSURED, E_INCOME = -as.numeric(E_INCOME)) %>% #reversing EMPLOYED, CITIZEN, INSURED, AND INCOME
  apply(., MARGIN = 2, FUN = function(x) rank(x, na.last = T, ties.method = c("first"))) %>%  #calculate ranks for remaining columns
  as.data.frame() %>% #turn into dataframe
  mutate(total_rank = rowSums(.)) %>% #calculate total rank for each census tract
  cbind(`Geographic Area Name`, .) #add back in the geographic names

#percentile data frame with sum rank
tdpr_df <- tdrank_df %>% #start off with rank data frame
  select(-c(`Geographic Area Name`, total_rank)) %>% #take out geographic names and total rank
  apply(., MARGIN = 2, FUN = function(x) (x-1)/(length(x)-1)) %>% #calculate percentiles for each of the columns
  as.data.frame() %>% #turn into data frame to use mutate
  mutate(sum_rank = round(rowSums(.), 2)) %>% #calculate the sum rank and round
  round(., 2) %>%  #round the entire data frame
  cbind(`Geographic Area Name`, .) #add back in the geographic names

#view the total rank and percentile sum rank data frames
View(tdrank_df)
View(tdpr_df)

#Can also filter to see the rankings of the list of 8 census tracts interested in:

tdrank_df %>%
  filter(grepl("9, Coos|10, Coos|11, Coos|9515, Lincoln|9516, Lincoln|9517, Lincoln|9601, Tillamook|9602, Tillamook", 
               tdrank_df$`Geographic Area Name`)) 

tdpr_df %>%
  filter(grepl("9, Coos|10, Coos|11, Coos|9515, Lincoln|9516, Lincoln|9517, Lincoln|9601, Tillamook|9602, Tillamook", 
               tdpr_df$`Geographic Area Name`)) 


#--------------------------------------------------------------------
#------------------- PT 3 -------------------------------------------
#--------------------------------------------------------------------

#Finally, we rank each of the census tracts from list Jasmine gave
# we filter for the specified list Jasmine gave
#Coos: 9 (Bandon), 10 (Coquille), and 11 (Myrtle Point)
#Lincoln: 9516 (Waldport), 9517 (Yachats), and 9515 (Alsea Bay)
#Tillamook: 9601 (Nehalem & Wheeler), 9602 (Rockaway Beach)* 

#first filter for the 8 particular census tracts
tdvuln_df2_short <- tdvuln_df2 %>%
  filter(grepl("9, Coos|10, Coos|11, Coos|9515, Lincoln|9516, Lincoln|9517, Lincoln|9601, Tillamook|9602, Tillamook", 
               tdvuln_df2$`Geographic Area Name`)) 
#Save names of shortened list of census tracts
`Geographic Name` <- tdvuln_df2_short$`Geographic Area Name`

#rank data with total rank
tdrank_df_filt <- tdvuln_df2_short %>%
  select(-c(E_TOTALPOP, M_TOTALPOP)) %>% #deselect the E_TOTALPOP and M_TOTALPOP
  select(grep("+M_|Name|+E_PCT", colnames(.), invert = T)) %>% #Take out Name, MOE (M_) and E_PCT columns
  mutate(E_EMPLOYED = -E_EMPLOYED, E_CITIZEN = -E_CITIZEN, E_INSURED = -E_INSURED, E_INCOME = -as.numeric(E_INCOME)) %>% #reversing EMPLOYED, CITIZEN, INSURED, AND INCOME
  apply(., MARGIN = 2, FUN = function(x) rank(x, na.last = T, ties.method = c("first"))) %>%  #calculate ranks for remaining columns
  as.data.frame() %>% #turn into dataframe
  mutate(total_rank = rowSums(.)) %>% #calculate total rank for each census tract
  cbind(`Geographic Name`, .) #add back in the geographic names

#percentile data frame with sum rank
tdpr_df_filt <- tdrank_df_filt %>% #start off with rank data frame
  select(-c(`Geographic Name`, total_rank)) %>% #take out geographic names and total rank
  apply(., MARGIN = 2, FUN = function(x) (x-1)/(length(x)-1)) %>% #calculate percentiles for each of the columns
  as.data.frame() %>% #turn into data frame to use mutate
  mutate(sum_rank = round(rowSums(.), 2)) %>% #calculate the sum rank and round
  round(., 2) %>%  #round the entire data frame
  cbind(`Geographic Name`, .) #add back in the geographic names

#view the total rank and percentile sum rank data frames
View(tdrank_df_filt)
View(tdpr_df_filt)
