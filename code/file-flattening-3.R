################################################################################################################
################################################################################################################
#   Code: file-flattening-3.R
#   Purpose: combine all flattened files into one file for model build
#   DS: Brian Saindon
#   Date: 10/1/2017
#   Input: df_flattened_1.rds, hcc.rds, dx_groups_biomed.rds
#   Output: model.data1.RDS
################################################################################################################
################################################################################################################

################################################################################################################
#   Data Read In

df_flattened<-readRDS("../derived_variables/df_flattened_1.rds")
hcc_flattened<-readRDS(file = "../derived_variables/hcc.rds")
biomed_groups_flattened<-readRDS(file = "../derived_variables/dx_groups_biomed.rds")

length(unique(biomed_groups_flattened$patient_nbr))
length(unique(df_flattened$patient_nbr))
length(unique(hcc_flattened$patient_nbr))

###Merge
# Left join
model.data <- merge.data.frame(x = df_flattened, y = hcc_flattened, by = "patient_nbr", all.x =TRUE)
model.data <- merge.data.frame(x = model.data, y = biomed_groups_flattened, by = "patient_nbr", all.x =TRUE)
# Replace NA as 0
model.data[is.na(model.data)] <- 0
colnames(model.data)
#rename condition variables
names(model.data)[49:117] <- paste0('x', 1:(ncol(model.data[49:117])))
# remove diag columns
model.data<- model.data[,-c(15, 16, 17)]
saveRDS(model.data, file = "../derived_variables/model.data.rds")
# NEED TO DOCUMENT condition variables
