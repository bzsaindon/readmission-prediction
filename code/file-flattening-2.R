################################################################################################################
################################################################################################################
#   Code: file-flattening-2.R
#   Purpose: create binary flags to indicate diagnosis groups
#   DS: Brian Saindon
#   Date: 10/1/2017
#   Input: diabetic_data.csv, hccxwalk.csv
#   Output: hcc.rds, dx_groups_biomed.rds
################################################################################################################
################################################################################################################
library(stringr)
library(dplyr)
library(reshape)

################################################################################################################
# Data Read In
#   HCC X walk
xwalk<-read.csv("../lookup/cms_hcc_icd9_abbrev.csv", sep=",", header=TRUE, stringsAsFactors = FALSE)
xwalk$icd<- as.factor(xwalk$icd9_abbrev)

#   Diabetic Data
df <- readRDS("../derived_variables/df_flattened_1.rds")

# Reshape ICD codes
df_tmp <- df[, c("patient_nbr", "diag_1", "diag_2", "diag_3")]
df_tmp1 <- melt(df_tmp, id=c("patient_nbr"))

rownames(df_tmp1) <- c()
df_tmp1$icd<-as.factor(df_tmp1$value)
df_tmp1$icd<-gsub("(.*)\\..*", "\\1", df_tmp1$icd) # Remove Child Codes

# Clean ICD column
df_tmp1$icd<-str_trim(df_tmp1$icd, side = c("both"))
xwalk$icd<-str_trim(xwalk$icd, side = c("both"))
# Remove Missing
df_tmp1 <- df_tmp1 %>%
  filter(icd != "?")
# 209970 - 208445 = 1525 records dropped
# Merge to xwalk
df_tmp2<- merge(x = df_tmp1, y = xwalk, by = "icd")
df_tmp2 <- df_tmp2[!is.na(df_tmp2$icd),]
# Many ICDs don't map to mapping - need to improve!

library(dplyr)
df_tmp3<- df_tmp2[c("patient_nbr", "condition")]
df_tmp3<- df_tmp3[!(is.na(df_tmp3$condition)),]

# Only keep unique conditions per individual
deduped.data <- unique(df_tmp3[ , c("patient_nbr", "condition") ] )

wide.data <-deduped.data %>%
  reshape(.,
          idvar = "patient_nbr",
          v.names = c("condition"),
          timevar = "condition",
          direction = "wide")

# Code NA as O
wide.data[ , !names(wide.data) %in% c("patient_nbr")]<- ifelse(is.na(wide.data[ , !names(wide.data) %in% c("patient_nbr")]), 0, 1)

# Save Flattened File
saveRDS(wide.data_2008, file = "../derived_variables/hcc.rds")


#######################
#   DX Groups  
#   Create DX group variables according to https://www.hindawi.com/journals/bmri/2014/781670/tab2/ 
#######################

df_tmp2$dx_group_circulatory<- ifelse(df_tmp2$icd <=459 & df_tmp2$icd >=390 || df_tmp2$icd == 785 , 1, 0)
df_tmp2$dx_group_digestive<- ifelse(df_tmp2$icd <=570 & df_tmp2$icd >=520 || df_tmp2$icd == 787 , 1, 0)
df_tmp2$dx_group_genitourinary<- ifelse(df_tmp2$icd <=629 & df_tmp2$icd >=580 || df_tmp2$icd == 788 , 1, 0)
df_tmp2$dx_group_diabetes<- ifelse(df_tmp2$icd ==250, 1, 0)
df_tmp2$dx_group_injury<- ifelse(df_tmp2$icd <=999 & df_tmp2$icd >=800 , 1, 0)
df_tmp2$dx_group_musculoskeletal<- ifelse(df_tmp2$icd <=739 & df_tmp2$icd >=710 , 1, 0)
df_tmp2$dx_group_neoplasms<- ifelse(df_tmp2$icd <=239 & df_tmp2$icd >=140 , 1, 0)
df_tmp2$dx_group_respiratory<- ifelse(df_tmp2$icd <=519 & df_tmp2$icd >=460 || df_tmp2$icd == 786 , 1, 0)
df_tmp2_out <-df_tmp2[ -c(1, 3:8) ]

df_dx_group_out <-df_tmp2_out %>%
  group_by(patient_nbr) %>%
  summarise(dx_group_circulatory = max(dx_group_circulatory), 
            dx_group_digestive = max(dx_group_digestive), 
            dx_group_genitourinary = max(dx_group_genitourinary), 
            dx_group_diabetes = max(dx_group_diabetes), 
            dx_group_injury = max(dx_group_injury), 
            dx_group_musculoskeletal = max(dx_group_musculoskeletal), 
            dx_group_neoplasms = max(dx_group_neoplasms),
            dx_group_respiratory = max(dx_group_respiratory))

# Save dx groups file
saveRDS(df_dx_group_out, file = "../derived_variables/dx_groups_biomed.rds")


# To Do - get better mapping from ICD to DX groups