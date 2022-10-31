
# 0_Deer File Replacement ####

library(tidyverse); library(fs)

"Data/Behaviour" %>% list.files
"Data/Phenotypes" %>% list.files

# Parasites ####
# All straight from the file

# Behaviour ####
# Censuses straight from the file

# Phenotypes ####
# Main page queries ##
# HindStatus <- PhenotypeList$sys_HindStatusByYear
# LBS <- PhenotypeList$LBS.xlsx

# Auxiliary queries ##
# PopSize <- PhenotypeList$PopSize.xlsx
# BirthWts <- PhenotypeList$sys_BirthWt.xlsx