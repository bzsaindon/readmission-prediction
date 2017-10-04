################################################################################################################
################################################################################################################
#   Code: file-flattening-1.R
#   Purpose: remove variables with high missing, remove duplicate records, derive variables
#   DS: Brian Saindon
#   Date: 10/1/2017
#   Input: diabetic_data.csv
#   Output: df_flattening.RDS
################################################################################################################
################################################################################################################
setwd("github_repos/readmission-predictions/")
getwd()
################################################################################################################
#   Data Read In

df <-read.csv("../data/diabetic_data.csv",
              sep=",",quote = ",", header=TRUE, stringsAsFactors = FALSE)
################################################################################################################

################################################################################################################
# Exclusion Criteria
#   Remove Variables with high percent missing: Weight and Payer 
#   Source: https://www.hindawi.com/journals/bmri/2014/781670/tab1/ 
df <- df[ -c(6, 11) ]

#   Remove Individuals with a discharge to hospice or patient death
#   Source: https://www.hindawi.com/journals/bmri/2014/781670/
df <- df %>%
  filter(discharge_disposition_id != 11  & discharge_disposition_id != 19 & discharge_disposition_id != 20 & discharge_disposition_id != 21 & discharge_disposition_id != 13 & discharge_disposition_id != 14)

#   Remove duplicate Records

df$age_group<- ifelse(df$age == "[0-10)", 1,
                      ifelse(df$age =="[10-20)", 2, 
                             ifelse(df$age =="[20-30)", 3,
                                    ifelse(df$age =="[30-40)", 4, 
                                           ifelse(df$age =="[40-50)", 5,
                                                  ifelse(df$age =="[50-60)", 6,
                                                         ifelse(df$age =="[60-70)", 7, 
                                                                ifelse(df$age=="[70-80)", 8, 
                                                                       ifelse(df$age=="[80-90)", 9,
                                                                              ifelse(df$age=="[90-100)", 10, 0))))))))))

attach(df)
ordered_data <- df[order(patient_nbr, -age_group), ]
detach(df)

# Select first row per id
df <- ordered_data[!duplicated(ordered_data$patient_nbr),]

########################################################
# Variable Derivation / Modification
########################################################

# Dependent Variable: READMISSION
# Definition: readmission if readmission is < 30 day, no readmission if readamission is > 30 days or blank
df$readmitted_dv <- ifelse(df$readmitted =="<30", 1, 0)

# medical_specialty: group ? as missing category

df$medical_specialty<- ifelse(df$medical_specialty =="?", "missing", df$medical_specialty)

# medication values: convert to binary (0 = no, 1 = Steady/Up/Down)

colnames(df)
varnames<- colnames(df[c(23:45)])
varnames
for(i in varnames) {
  df[i]<- ifelse(df[i] == "No", 0,1)
}

# hba1c - convert value to a binary indicator that identifies whether a test had been performed
# source: https://www.hindawi.com/journals/bmri/2014/781670/

# hba1c_performed
df$hba1c_performed <- ifelse(df$A1Cresult =="None", 0, 1)

# convert several variables to 1, 0  
df$diabetesMed<- ifelse(df$diabetesMed =="Yes", 1, 0)
df$gender<- ifelse(df$gender == "Female", 1, 0)

# race: code ? as missing
df$race <- ifelse(df$race =="?", "missing", df$race)

# change: convert to 1, 0
df$change<-ifelse(df$change == "Ch", 1, 0)


colnames(df)
final <- df[-c(1, 5, 48) ]
# Remove encounter_id, age (recoded), 48(recoded), 49(counter)
saveRDS(final, file = "../derived_variables/df_flattened_1.rds")


#Pending:
# Group:
# admission_type_id, discharge_disposition_id, admission_source_id
# time_in_hospital, num_lab_procedures, num_procedures, num_medications, 
# number_outpatient, num_emergency, number_inpatient, number_diagnoses



