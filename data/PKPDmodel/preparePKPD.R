###### PREAMBLE ########################################################
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list=ls())

library(dplyr)
library(bigrquery)
library(glue)
library(data.table)
projectid <- "deepqlearning-342122"
################## raw data #############################################
adm <- read.csv("adm.csv")
meas <- read.csv("meas.csv")
feat <- read.csv("../MLfeatures/MLfeat_p1.csv")

subjs <- unique(adm$stay_id)

flag_load <- TRUE #load results of step2
############## step 1:  get true amount  and valid stay_IDs ###################
if(flag_load){
    load("C:/Users/AlessandroDeCarlo/Desktop/DT/projs/vanco/ws.RData")
}else{
    
    adm_list <- list()
    
    
    meas$charttime  <- as.POSIXct(meas$charttime)
    keep_list <- list()
    
    #filtro soggetti che hanno la prima misurazione prima della prima dose riportata
    
    for(i in 1:length(subjs)){
        
        
        
        adm_i <- adm %>% filter(stay_id==subjs[i]) %>% arrange(starttime)
        adm_i$starttime <- as.POSIXct(adm_i$starttime)
        first_adm <- adm_i %>% slice(1:1)
        meas_i <- meas %>% filter(stay_id==subjs[i]) %>% arrange(charttime) %>% slice(1:1)
        
        if(meas_i$charttime>=adm_i$starttime){
            keep_list <- append(keep_list,subjs[i])
        }
        
    }
    
    keep_list <- as.array(keep_list)
    
    
    for (i in 1:length(keep_list)) {
        subj_list <- list()
        adm_i <- adm %>% filter(stay_id==keep_list[i]) %>% arrange(starttime)
        print(i)
        
        for (j in 1:nrow(adm_i)) {
            adm_ij <- adm_i[j,]
            uom <- adm_ij$amountuom
            
            if(uom=="grams"){
                adm_ij$amount <- as.numeric(adm_ij$amount)*1000
                adm_ij$amountuom <- "mg"
            }else{
                sbj_id <- adm_ij$subject_id
                hd_id <- adm_ij$hadm_id
                time_th <- adm_ij$starttime
                
                query_body <- "SELECT * FROM `deepqlearning-342122.MIMIC4_v2.prescriptions` 
                where
                subject_id = {sbj_id} and 
                hadm_id = {hd_id} and starttime <='{time_th}'
                and lower(drug) like 'vancom%'
                and route = 'IV'"
                
                # Set your query
                sql <- glue(query_body)
                # Run the query; this returns a bq_table object that you can query further
                tb <- bq_project_query(projectid, sql)
                # Store  the data in a tibble
                sample <-bq_table_download(tb)
                
                if(nrow(sample)>0){
                    sample <- sample %>% filter(starttime==max(sample$starttime))
                    if(nrow(sample) > 1){
                        sample <- sample %>% slice(1:1)
                    }
                    
                    
                    if(sample$dose_unit_rx == "mg"){
                        adm_ij$amount <- as.numeric(sample$dose_val_rx)
                        adm_ij$amountuom <- sample$dose_unit_rx
                    }else if(sample$dose_unit_rx == "grams"){
                        adm_ij$amount <- as.numeric(sample$dose_val_rx)*1000
                        adm_ij$amountuom <- "mg"
                    }else{
                        adm_ij$amount <- 10000
                        adm_ij$amountuom <- "mg"
                    }
                    
                    
                }else{
                    adm_ij$amount <- 1000
                    adm_ij$amountuom <- "mg"
                }
                
            }
            subj_list[[j]]<-adm_ij
        }
        
        adm_list[[i]] <- rbindlist(subj_list)
    }
    
    
}

############ NONMEM DATASET #################################################




adm_df <- rbindlist(adm_list)
adm_df$starttime <- as.POSIXct(adm_df$starttime)

map_IDs <- data.frame(oldID=unlist(keep_list),newID=1:length(keep_list))
write.table(map_IDs,"subj_list_def_remapped_ids.csv",quote=F,sep=",",
            row.names = F)

feat_filtered <- feat[feat$ID %in% as.array(keep_list),]

nm_list <- list()

for (k in 1:length(keep_list)) {
    id <- keep_list[[k]]
    
    adm_k <- adm_df %>% filter(stay_id==id) %>% select(-c(endtime,amountuom,
                                                          ordercategoryname,
                                                          subject_id,hadm_id,
                                                          totalamount,
                                                          totalamountuom))
    t0 <- adm_k$starttime[1]
    deltaTadm <- adm_k$starttime-t0
    units(deltaTadm) <- "hours"
    adm_k$starttime <- as.numeric(deltaTadm)
    
    
    meas_k <- meas %>% filter(stay_id==id) %>% select(-c(subject_id,hadm_id,
                                                     label,valueuom))
    deltaTmeas <- meas_k$charttime - t0
    units(deltaTmeas) <- "hours"
    meas_k$charttime <- as.numeric(deltaTmeas)
    
    tmax <- min(meas_k$charttime[1]+48,max(meas_k$charttime))
    
    adm_k <- adm_k %>% filter(starttime<tmax)
    meas_k <- meas_k %>% filter(charttime<=tmax)
    
    nRowAdm <- nrow(adm_k)
    nm_adm <- data.frame(C=rep('.',nRowAdm),ID=rep(map_IDs$newID[k],nRowAdm),
                         TIME = adm_k$starttime,
                         DV=rep('.',nRowAdm),MDV=rep(1,nRowAdm),
                         EVID=rep(1,nRowAdm),AMT=as.numeric(adm_k$amount),
                         RATE=rep(600,nRowAdm))
    
    nRowMeas <- nrow(meas_k)
    nm_meas <- data.frame(C=rep('.',nRowMeas),ID=rep(map_IDs$newID[k],nRowMeas),
                           TIME = meas_k$charttime,
                           DV=meas_k$value,MDV=rep(0,nRowMeas),
                           EVID=rep(0,nRowMeas),AMT=rep('.',nRowMeas),
                           RATE=rep('.',nRowMeas))
    
    nm_subj <- rbind(nm_adm,nm_meas) %>% arrange(TIME)
    
    covs <- feat_filtered %>% filter(ID==id) %>% select(-c(ID)) %>% 
        slice(rep(1:n(), each = nrow(nm_subj)))
    
    nm_subj <- cbind(nm_subj,covs)
    
    
    nm_list[[k]] <- nm_subj
}





nm_df <- rbindlist(nm_list)

nm_df$SEX[nm_df$SEX=="M"] <- 1
nm_df$SEX[nm_df$SEX=="F"] <- 0

nm_df$AMT[nm_df$AMT==10000] <- 1000 #fixing some wrong dosages

nm_df <- nm_df[nm_df$DV!=150.4,] #dumb way to remove one outlier meas

write.table(nm_df,"dataset_no_covs.csv",quote=F,sep=",",
            row.names = F)

mx_df <- nm_df %>% rename(Y=DV) %>% filter(WT>1)



write.table(mx_df,"dataset_mx.csv",quote=F,sep=",",
            row.names = F)

