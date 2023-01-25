###### PREAMBLE ########################################################
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list=ls())

library(dplyr)
library(ggplot2)
library(data.table)
library(RColorBrewer)
library(corrplot)#plot of matrices
library(ggpubr)
library(grid)
library(ramify)
library(pracma)
######################### functions #######################################

ind_plot <- function(id_i,name_mod,data.fit,obs,ll.df,params.df,auc){
    
    colori = c("Dati"="blue","Ipred" = "purple")
    datas <- data.fit %>% filter(ID==id_i) # replace pred.model2 with input name of the fcn
    datas.obs <- obs%>% filter(ID==id_i)
    ll <- (ll.df %>% filter(ID==id_i))[,name_mod] # replace model2 with the input name of the fcn
    auc_i <- (auc %>% filter(ID==id_i))$auc
    
    cl <-(params.df %>% filter(id==id_i))$Cl_mode
    v <-(params.df %>% filter(id==id_i))$V_mode
    
    p <- ggplot(data=datas,aes(x=time)) + geom_line(aes(y=indivPredMode,color="Ipred"),size=2) +
        geom_point(data=datas.obs,aes(x=time, y=Y,color="Dati"),size=3)+scale_color_manual(values=colori)+
        theme(text= element_text(size=13),
              plot.title =  element_text(hjust = 0.5),
              panel.background = element_rect(fill = "#f9f9f9"
                                              ,colour = "#f9f9f9",
                                              size = 0.5, 
                                              linetype = "solid"),
              panel.grid.major = element_line(size = 0.5, 
                                              linetype = 'solid',
                                              colour = "gray"), 
              panel.grid.minor = element_line(size = 0.25, 
                                              linetype = 'solid',
                                              colour = "gray"),
              legend.position = "none") +
        labs(title = paste("ID",as.character(id_i),"-",name_mod,"- -2LL:",as.character(round(ll,3)),
                           "CL:",as.character(round(cl,3)),
                           "-", "V:",as.character(round(v,3)),
                           "-", "AUC", as.character(round(auc_i,2)),
                           sep=" "), x="",y="",color="Legend")
    
    return(p)
    
}

######### LOAD DATA SOURCES #############################################

model2 <- read.csv("1cmt_V_wt_CL_age_cr_sex_prop/vanco_1cmt_V_wt_CL_age_cr_sex_prop/LogLikelihood/individualLL.txt")

ffc <- read.csv("1cmt_fullCovs_NO_LAcl_HCTv_SEXv_AGcl_prop/vanco_1cmt_fullCovs_NO_LAcl_HCTv_SEXv_AGcl_prop/LogLikelihood/individualLL.txt")

roberts <- read.csv("0_Literature_Models/Roberts/Roberts_add_originalCV/roberts_add_originalCV/LogLikelihood/individualLL.txt")

base <- read.csv("1cmt_noCovs_prop/vanco_1cmt_noCovs_prop/LogLikelihood/individualLL.txt")

revilla <- read.csv("0_Literature_Models/Revilla/Revilla/revilla/LogLikelihood/individualLL.txt")

############# Graphical Constant ########################################
fontsz.grid <- theme(text= element_text(size=22),
                     plot.title =  element_text(hjust = 0.5),
                     panel.background = element_rect(fill = "#f9f9f9"
                                                     ,colour = "#f9f9f9",
                                                     size = 0.5, 
                                                     linetype = "solid"),
                     panel.grid.major = element_line(size = 0.5, 
                                                     linetype = 'solid',
                                                     colour = "gray"), 
                     panel.grid.minor = element_line(size = 0.25, 
                                                     linetype = 'solid',
                                                     colour = "gray"))
#################### best model 4 each subj #################################

mod_names <-  c("Model2","FFC","Roberts","Base","Revilla")


m_ll <- as.matrix(cbind(model2$importanceSampling,ffc$importanceSampling,
                        roberts$importanceSampling,
                        base$importanceSampling,
                        revilla$importanceSampling))

w_mll <- exp(-0.5*m_ll) / rowSums(exp(-0.5*m_ll))


colnames(w_mll) <- mod_names
amins <- argmax(w_mll,rows = T)

amins_names <- data.frame(BestModel = as.factor(mod_names[amins]))


p<-ggplot(data=amins_names, aes(x=BestModel,fill=BestModel)) +
    geom_bar(stat="count") + fontsz.grid
plot(p)


################## likelihood of the mixture model ###########################
scores <- c()
labs_best <- c()
for (i in 1:nrow(w_mll)) {
    scores <- c(scores,w_mll[i,amins[i]])
    labs_best <- c(labs_best,mod_names[amins[i]])
}

######################### load data for plots ################################



obs <- read.csv("1cmt_fullCovs_NO_LAcl_HCTv_SEXv_AGcl_prop/vanco_1cmt_fullCovs_NO_LAcl_HCTv_SEXv_AGcl_prop/ChartsData/IndividualFits/Y_observations.txt")
pred.model2 <- read.table("1cmt_V_wt_CL_age_cr_sex_prop/vanco_1cmt_V_wt_CL_age_cr_sex_prop/ChartsData/IndividualFits/Y_fits.txt",sep = ",", comment.char = "", header = T)
pred.ffc  <- read.csv("1cmt_fullCovs_NO_LAcl_HCTv_SEXv_AGcl_prop/vanco_1cmt_fullCovs_NO_LAcl_HCTv_SEXv_AGcl_prop/ChartsData/IndividualFits/Y_fits.txt")
pred.roberts <- read.csv("0_Literature_Models/Roberts/Roberts_add_originalCV/roberts_add_originalCV/ChartsData/IndividualFits/Y_fits.txt")
pred.base <- read.csv("1cmt_noCovs_prop/vanco_1cmt_noCovs_prop/ChartsData/IndividualFits/Y_fits.txt")
pred.revilla <- read.csv("0_Literature_Models/Revilla/Revilla/revilla/ChartsData/IndividualFits/Y_fits.txt")



auc.model2 <- as.data.frame(pred.model2 %>% group_by(ID) %>% summarise(auc=trapz(time,indivPredMode)))
auc.ffc <- as.data.frame(pred.ffc %>% group_by(ID) %>% summarise(auc=trapz(time,indivPredMode)))
auc.roberts <- as.data.frame(pred.roberts %>% group_by(ID) %>% summarise(auc=trapz(time,indivPredMode)))
auc.base <- as.data.frame(pred.base %>% group_by(ID) %>% summarise(auc=trapz(time,indivPredMode)))
auc.revilla <-as.data.frame(pred.revilla %>% group_by(ID) %>% summarise(auc=trapz(time,indivPredMode)))






model2.params <- read.csv("1cmt_V_wt_CL_age_cr_sex_prop/vanco_1cmt_V_wt_CL_age_cr_sex_prop/IndividualParameters/estimatedIndividualParameters.txt")
ffc.params <- read.csv("1cmt_fullCovs_NO_LAcl_HCTv_SEXv_AGcl_prop/vanco_1cmt_fullCovs_NO_LAcl_HCTv_SEXv_AGcl_prop/IndividualParameters/estimatedIndividualParameters.txt")
roberts.params <- read.csv("0_Literature_Models/Roberts/Roberts_add_originalCV/roberts_add_originalCV/IndividualParameters/estimatedIndividualParameters.txt")
base.params <- read.csv("1cmt_noCovs_prop/vanco_1cmt_noCovs_prop/IndividualParameters/estimatedIndividualParameters.txt")
revilla.params.raw <- read.csv("0_Literature_Models/Revilla/Revilla/revilla/IndividualParameters/estimatedIndividualParameters.txt") 
revilla.dataset <- read.csv("../data/PKPDmodel/dataset_mx_crclWT_min_doseWT.csv") %>% filter(WT>1)


covs_i <- revilla.dataset %>% group_by(ID) %>% slice(1:1)
cli <- (((revilla.params.raw$Clpop_SAEM*covs_i$CrCl)+covs_i$AGE^(revilla.params.raw$thetaAge_SAEM))*exp(revilla.params.raw$Cl_mode))*60*covs_i$WT/1000
vi <- ((revilla.params.raw$Vpop_SAEM*revilla.params.raw$thetaCr_SAEM^(covs_i$CrBin))*exp(revilla.params.raw$V_mode))*covs_i$WT





revilla.params <- data.frame(id=base.params$id,
                             V_mode=vi,
                             Cl_mode = cli
                             )



################################ make individual plots #######################

IDs <- unique(model2.params$id)


ll_df <- cbind(data.frame(ID=IDs),as.data.frame(w_mll))

auc_mat  <- cbind(auc.model2$auc,auc.ffc$auc,auc.roberts$auc,auc.base$auc,auc.revilla$auc)

delta_auc_ffc <- c()

for (i in 1:length(IDs)) {
    id_i <- IDs[i]
    subj_plots <- list(
                ind_plot(id_i,"Model2",pred.model2,obs,ll_df,model2.params,auc.model2),
                ind_plot(id_i,"FFC",pred.ffc,obs,ll_df,ffc.params,auc.ffc),
                ind_plot(id_i,"Roberts",pred.roberts,obs,ll_df,roberts.params,auc.roberts),
                ind_plot(id_i,"Base",pred.base,obs,ll_df,base.params,auc.base),
                ind_plot(id_i,"Revilla",pred.revilla,obs,ll_df,revilla.params,auc.revilla)
    )
    
    subj_plots[[amins[i]]]$theme$text$face <- "bold"
    
    sb <- ggarrange(plotlist = subj_plots,ncol = 2,nrow = 3)
    
    delta_auc_ffc <- c(delta_auc_ffc,(auc_mat[i,2]-auc_mat[i,amins[i]])/auc_mat[i,2])
    
    png(filename = paste0("Individual_fittings/ID_",as.numeric(id_i),".png"),width =1024 ,height =1400 )
    print(sb)
    dev.off()
    
}


####################  delta aucs with respect to FFC ########################


delta_auc_df <- data.frame(ID=base.params$id,
                           DeltaAucRelFFC = delta_auc_ffc,
                           BestModel = amins_names$BestModel) %>% filter(BestModel!="FFC")


p<-ggplot(data=delta_auc_df) + 
    geom_boxplot(aes(x=DeltaAucRelFFC,fill=BestModel),alpha=.5,colour="black",
                 size=.8,outlier.size = 3) + fontsz.grid



print(p)

delta_auc_df %>% group_by(BestModel) %>% summarise(median=median(DeltaAucRelFFC),
                                                   Q1=quantile(DeltaAucRelFFC,0.25),
                                                   Q2=quantile(DeltaAucRelFFC,0.75),
                                                   low=quantile(DeltaAucRelFFC,0.025),
                                                   up= quantile(DeltaAucRelFFC,0.975),
                                                   .groups="drop"
                                                   )
                                                
########### statistical tests ############################################                                                
delta_auc_uf_df <- data.frame(ID=base.params$id,
                           DeltaAucRelFFC = delta_auc_ffc,
                           BestModel = amins_names$BestModel)    

auc.base_best <- auc.base[delta_auc_uf_df$BestModel=="Base",c("ID","auc")]
auc.ffc_base <- auc.ffc[delta_auc_uf_df$BestModel=="Base",c("ID","auc")]


wilcox.test(auc.base_best$auc,auc.ffc_base$auc,paired = TRUE,alternative = "two.sided")


auc.model2_best <- auc.model2[delta_auc_uf_df$BestModel=="Model2",c("ID","auc")]
auc.ffc_model2 <- auc.ffc[delta_auc_uf_df$BestModel=="Model2",c("ID","auc")]


wilcox.test(auc.model2_best$auc,auc.ffc_model2$auc,paired = TRUE,alternative = "two.sided")

auc.revilla_best <- auc.revilla[delta_auc_uf_df$BestModel=="Revilla",c("ID","auc")]
auc.ffc_revilla<- auc.ffc[delta_auc_uf_df$BestModel=="Revilla",c("ID","auc")]


wilcox.test(auc.revilla_best$auc,auc.ffc_revilla$auc,paired = TRUE,alternative = "two.sided")


auc.roberts_best <- auc.roberts[delta_auc_uf_df$BestModel=="Roberts",c("ID","auc")]
auc.ffc_roberts<- auc.ffc[delta_auc_uf_df$BestModel=="Roberts",c("ID","auc")]


wilcox.test(auc.roberts_best$auc,auc.ffc_roberts$auc,paired = TRUE,alternative = "two.sided")