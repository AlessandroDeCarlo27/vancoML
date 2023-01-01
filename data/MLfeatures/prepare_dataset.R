###### PREAMBLE ########################################################
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list=ls())

library(dplyr)

##################### BASAL FEATURES #########################################

basal <- read.csv("raw/BASAL_FEATURES.csv")

features <- unique(basal$label)

#integrity of UoM
for(i in 1:length(features)){
    d <- basal %>% filter(label==features[i]) %>% arrange(valueuom)
    ums <- d$valueuom
    print(unique(ums))
}

#if missing uom--> default uom used

basal_essentials <- basal %>% select(-c("subject_id","hadm_id","charttime","category","valueuom"))
basal_rh <-reshape(basal_essentials,idvar = "stay_id",timevar = "label",direction = "wide")
basal_rh <- basal_rh %>% arrange(stay_id)

colnames(basal_rh) <- c("ID",'AnionGap','BUN','Ca','Chloride','Creatinine','Glucose','HCO3',
                        'HCT','Hb','P','PLT','K','Na','WBC','LacticAcid','HR','Temperature')

########################### DEMOGRAPHIC ##############################################

demo <- read.csv("raw/DEMO_SUBJ.csv") %>% select(c("stay_id","anchor_age","gender"))
colnames(demo)  <- c("ID",'AGE','SEX')

########################### BASAL WT ################################################

wt_first_adm <- read.csv("raw/WT_SUBJ.csv") %>% select(c("stay_id","patientweight"))
colnames(wt_first_adm)  <- c("ID",'WT')


######################### merging ################################################

MLfeat1 <- basal_rh %>% inner_join(demo,by="ID") %>% inner_join(wt_first_adm,by="ID")

write.table(MLfeat1,"MLfeat_p1.csv",quote=F,sep=",",row.names = F)
