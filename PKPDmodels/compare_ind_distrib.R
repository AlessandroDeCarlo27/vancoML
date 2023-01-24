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
library(pracma)

############### functions ###############################################

summarize_diffs <- function(df,pkpar){
    
    if (pkpar=="CL") {
        
        sumo <- df %>% group_by(labs) %>% 
            summarise(median=median(diffCL),
                      Q1=quantile(diffCL,0.25),
                      Q2=quantile(diffCL,0.75),
                      low=quantile(diffCL,0.025),
                      up= quantile(diffCL,0.975),
                      .groups="drop",
            )
        
    }else if(pkpar=="V"){
        
        sumo <- df %>% group_by(labs) %>% 
            summarise(median=median(diffV),
                      Q1=quantile(diffV,0.25),
                      Q2=quantile(diffV,0.75),
                      low=quantile(diffV,0.025),
                      up= quantile(diffV,0.975),
                      .groups="drop",
            )
        
        
    }else if(pkpar=="K"){
        
        sumo <- df %>% group_by(labs) %>% 
            summarise(median=median(diffK),
                      Q1=quantile(diffK,0.25),
                      Q2=quantile(diffK,0.75),
                      low=quantile(diffK,0.025),
                      up= quantile(diffK,0.975),
                      .groups="drop",)
    }else{
       
        sumo <- df %>% group_by(labs) %>% 
            summarise(median=median(diffAUC),
                      Q1=quantile(diffAUC,0.25),
                      Q2=quantile(diffAUC,0.75),
                      low=quantile(diffAUC,0.025),
                      up= quantile(diffAUC,0.975),
                      .groups="drop",) 
        
        
    }
    
    return(as.data.frame(sumo))
}

######### LOAD DATA SOURCES #############################################

model2 <- read.csv("1cmt_V_wt_CL_age_cr_sex_prop/vanco_1cmt_V_wt_CL_age_cr_sex_prop/IndividualParameters/estimatedIndividualParameters.txt") %>%
    select(c(id,V_mode,Cl_mode)) %>% mutate(ke = Cl_mode/V_mode)

final_fullcovs <- read.csv("1cmt_fullCovs_NO_LAcl_HCTv_SEXv_AGcl_prop/vanco_1cmt_fullCovs_NO_LAcl_HCTv_SEXv_AGcl_prop/IndividualParameters/estimatedIndividualParameters.txt") %>%
                    select(c(id,V_mode,Cl_mode)) %>% mutate(ke = Cl_mode/V_mode)

roberts <- read.csv("0_Literature_Models/Roberts/Roberts_add_originalCV/roberts_add_originalCV/IndividualParameters/estimatedIndividualParameters.txt") %>% 
            select(c(id,V_mode,Cl_mode)) %>% mutate(ke = Cl_mode/V_mode)


fact_labs <- as.factor(c(rep("Model2",nrow(model2)),rep("FinalFullCovs",nrow(final_fullcovs)),
                         rep("Roberts",nrow(roberts))))

plot_dataset <- cbind(rbind(model2,final_fullcovs,roberts),fact_labs)

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



###########  Paired differences plot m2 #######################################


diffCL.m2  <- data.frame(
                        ID = rep(model2$id,2),
                        diffCL = 
                            c((model2$Cl_mode-final_fullcovs$Cl_mode)/model2$Cl_mode,
                            (model2$Cl_mode-roberts$Cl_mode)/model2$Cl_mode),
                        labs = as.factor(c(rep("Model2-FFC",nrow(model2)),rep("Model2-Roberts",nrow(final_fullcovs))))
                        )


diffV.m2  <- data.frame(
                        ID = rep(model2$id,2),
                        diffV = 
                             c((model2$V_mode-final_fullcovs$V_mode)/model2$V_mode,
                               (model2$V_mode-roberts$V_mode)/model2$V_mode),
                        labs = as.factor(c(rep("Model2-FFC",nrow(model2)),rep("Model2-Roberts",nrow(final_fullcovs))))
                        )



p<-ggplot(data=diffCL.m2) + 
    geom_boxplot(aes(x=diffCL,fill=labs),alpha=.5,colour="black",
                 size=.8,outlier.size = 3) + fontsz.grid

print(p)


p<-ggplot(data=diffV.m2) + 
    geom_boxplot(aes(x=diffV,fill=labs),alpha=.5,colour="black",
                 size=.8,outlier.size = 3) + fontsz.grid

print(p)



###########  Paired differences plot FFC #######################################


diffCL.ffc <- data.frame(
    ID = rep(model2$id,2),
    diffCL = 
        c((final_fullcovs$Cl_mode-model2$Cl_mode)/final_fullcovs$Cl_mode,
          (final_fullcovs$Cl_mode-roberts$Cl_mode)/final_fullcovs$Cl_mode),
    labs = as.factor(c(rep("FFC-Model2",nrow(model2)),rep("FFC-Roberts",nrow(final_fullcovs))))
)


diffV.ffc  <- data.frame(
    ID = rep(model2$id,2),
    diffV = 
        c((final_fullcovs$V_mode-model2$V_mode)/final_fullcovs$V_mode,
          (final_fullcovs$V_mode-roberts$V_mode)/final_fullcovs$V_mode),
    labs = as.factor(c(rep("FFC-Model2",nrow(model2)),rep("FFC-Roberts",nrow(final_fullcovs))))
)



p<-ggplot(data=diffCL.ffc) + 
    geom_boxplot(aes(x=diffCL,fill=labs),alpha=.5,colour="black",
                 size=.8,outlier.size = 3) + fontsz.grid

print(p)


p<-ggplot(data=diffV.ffc) + 
    geom_boxplot(aes(x=diffV,fill=labs),alpha=.5,colour="black",
                 size=.8,outlier.size = 3) + fontsz.grid

print(p)



###########  Paired differences plot Roberts #######################################


diffCL.rob <- data.frame(
    ID = rep(model2$id,2),
    diffCL = 
        c((roberts$Cl_mode-model2$Cl_mode)/roberts$Cl_mode,
          (roberts$Cl_mode-final_fullcovs$Cl_mode)/roberts$Cl_mode),
    labs = as.factor(c(rep("Roberts-Model2",nrow(model2)),rep("Roberts-FFC",nrow(final_fullcovs))))
)


diffV.rob  <- data.frame(
    ID = rep(model2$id,2),
    diffV = 
        c((roberts$V_mode-model2$V_mode)/roberts$V_mode,
          (roberts$V_mode-final_fullcovs$V_mode)/roberts$V_mode),
    labs = as.factor(c(rep("Roberts-Model2",nrow(model2)),rep("Roberts-FFC",nrow(final_fullcovs))))
)



p<-ggplot(data=diffCL.rob) + 
    geom_boxplot(aes(x=diffCL,fill=labs),alpha=.5,colour="black",
                 size=.8,outlier.size = 3) + fontsz.grid

print(p)


p<-ggplot(data=diffV.rob) + 
    geom_boxplot(aes(x=diffV,fill=labs),alpha=.5,colour="black",
                 size=.8,outlier.size = 3) + fontsz.grid

print(p)


 ##################################
##### Whole distribution ###########################################

p <- ggplot(data=plot_dataset) + geom_density(aes(x=Cl_mode,color=fact_labs),alpha=0.3,size=1.2) + fontsz.grid
print(p)

p <- ggplot(data=plot_dataset) + geom_density(aes(x=V_mode,color=fact_labs),alpha=0.3,size=1.2) + fontsz.grid
print(p)

p<-ggplot(data=plot_dataset) + 
    geom_boxplot(aes(x=Cl_mode,fill=fact_labs),alpha=.5,colour="black",
                 size=.8,outlier.size = 3) + fontsz.grid

print(p)

p<-ggplot(data=plot_dataset) + 
    geom_boxplot(aes(x=V_mode,fill=fact_labs),alpha=.5,colour="black",
                 size=.8,outlier.size = 3) + fontsz.grid

print(p)

 ####################################################
################ statistical tests #################################################

friedman.test(as.matrix(cbind(model2$Cl_mode,final_fullcovs$Cl_mode,roberts$Cl_mode)))

wilcox.test(model2$Cl_mode,final_fullcovs$Cl_mode,paired = TRUE,alternative = "two.sided")
wilcox.test(model2$Cl_mode,roberts$Cl_mode,paired = TRUE,alternative = "two.sided")
wilcox.test(roberts$Cl_mode,final_fullcovs$Cl_mode,paired = TRUE,alternative = "two.sided")







friedman.test(as.matrix(cbind(model2$V_mode,final_fullcovs$V_mode,roberts$V_mode)))

wilcox.test(model2$V_mode,final_fullcovs$V_mode,paired = TRUE,alternative = "two.sided")
wilcox.test(model2$V_mode,roberts$V_mode,paired = TRUE,alternative = "two.sided")
wilcox.test(final_fullcovs$V_mode,roberts$V_mode,paired = TRUE,alternative = "two.sided")

##################################### summarize diffs #########################

summarize_diffs(diffCL.m2,"CL")
summarize_diffs(diffV.m2,"V")

summarize_diffs(diffCL.ffc,"CL")
summarize_diffs(diffV.ffc,"V")

summarize_diffs(diffCL.rob,"CL")
summarize_diffs(diffV.rob,"V")

############## keliminazione ##################################################



diffK.m2  <- data.frame(
    ID = rep(model2$id,2),
    diffK = 
        c((model2$ke-final_fullcovs$ke)/model2$ke,
          (model2$ke-roberts$ke)/model2$ke),
    labs = as.factor(c(rep("Model2-FFC",nrow(model2)),rep("Model2-Roberts",nrow(final_fullcovs))))
)


p<-ggplot(data=diffK.m2) + 
    geom_boxplot(aes(x=diffK,fill=labs),alpha=.5,colour="black",
                 size=.8,outlier.size = 3) + fontsz.grid

print(p)


diffK.ffc <- data.frame(
    ID = rep(model2$id,2),
    diffK = 
        c((final_fullcovs$ke-model2$ke)/final_fullcovs$ke,
          (final_fullcovs$ke-roberts$ke)/final_fullcovs$ke),
    labs = as.factor(c(rep("FFC-Model2",nrow(model2)),rep("FFC-Roberts",nrow(final_fullcovs))))
)


p<-ggplot(data=diffK.ffc) + 
    geom_boxplot(aes(x=diffK,fill=labs),alpha=.5,colour="black",
                 size=.8,outlier.size = 3) + fontsz.grid

print(p)


diffK.rob <- data.frame(
    ID = rep(model2$id,2),
    diffK = 
        c((roberts$ke-model2$ke)/roberts$ke,
          (roberts$ke-final_fullcovs$ke)/roberts$ke),
    labs = as.factor(c(rep("Roberts-Model2",nrow(model2)),rep("Roberts-FFC",nrow(final_fullcovs))))
)

p<-ggplot(data=diffK.rob) + 
    geom_boxplot(aes(x=diffK,fill=labs),alpha=.5,colour="black",
                 size=.8,outlier.size = 3) + fontsz.grid

print(p)



summarize_diffs(diffK.m2,"K")
summarize_diffs(diffK.ffc,"K")
summarize_diffs(diffK.rob,"K")


friedman.test(as.matrix(cbind(model2$ke,final_fullcovs$ke,roberts$ke)))

wilcox.test(model2$ke,final_fullcovs$ke,paired = TRUE,alternative = "two.sided")
wilcox.test(model2$ke,roberts$ke,paired = TRUE,alternative = "two.sided")
wilcox.test(final_fullcovs$ke,roberts$ke,paired = TRUE,alternative = "two.sided")

 

#### AUCs - load datas ##############################################

pred.model2 <- read.table("1cmt_V_wt_CL_age_cr_sex_prop/vanco_1cmt_V_wt_CL_age_cr_sex_prop/ChartsData/IndividualFits/Y_fits.txt",sep = ",", comment.char = "", header = T)
pred.ffc  <- read.csv("1cmt_fullCovs_NO_LAcl_HCTv_SEXv_AGcl_prop/vanco_1cmt_fullCovs_NO_LAcl_HCTv_SEXv_AGcl_prop/ChartsData/IndividualFits/Y_fits.txt")
pred.roberts <- read.csv("0_Literature_Models/Roberts/Roberts_add_originalCV/roberts_add_originalCV/ChartsData/IndividualFits/Y_fits.txt")

auc.model2 <- as.data.frame(pred.model2 %>% group_by(ID) %>% summarise(auc=trapz(time,indivPredMode)))
auc.ffc <- as.data.frame(pred.ffc %>% group_by(ID) %>% summarise(auc=trapz(time,indivPredMode)))
auc.roberts <- as.data.frame(pred.roberts %>% group_by(ID) %>% summarise(auc=trapz(time,indivPredMode)))


############## diff AUCs ####################################################

diffAUC.m2  <- data.frame(
    ID = rep(model2$id,2),
    diffAUC = 
        c((auc.model2$auc-auc.ffc$auc)/auc.model2$auc,
          (auc.model2$auc-auc.roberts$auc)/auc.model2$auc),
    labs = as.factor(c(rep("Model2-FFC",nrow(model2)),rep("Model2-Roberts",nrow(final_fullcovs))))
)




p<-ggplot(data=diffAUC.m2) + 
    geom_boxplot(aes(x=diffAUC,fill=labs),alpha=.5,colour="black",
                 size=.8,outlier.size = 3) + fontsz.grid

print(p)




diffAUC.ffc  <- data.frame(
    ID = rep(model2$id,2),
    diffAUC = 
        c((auc.ffc$auc-auc.model2$auc)/auc.ffc$auc,
          (auc.ffc$auc-auc.roberts$auc)/auc.ffc$auc),
    labs = as.factor(c(rep("FFC-Model2",nrow(model2)),rep("FFC-Roberts",nrow(final_fullcovs))))
)

p<-ggplot(data=diffAUC.ffc) + 
    geom_boxplot(aes(x=diffAUC,fill=labs),alpha=.5,colour="black",
                 size=.8,outlier.size = 3) + fontsz.grid

print(p)


diffAUC.roberts  <- data.frame(
    ID = rep(model2$id,2),
    diffAUC = 
        c((auc.roberts$auc-auc.model2$auc)/auc.roberts$auc,
          (auc.roberts$auc-auc.ffc$auc)/auc.roberts$auc),
    labs = as.factor(c(rep("Roberts-Model2",nrow(model2)),rep("Roberts-FFC",nrow(final_fullcovs))))
)

p<-ggplot(data=diffAUC.roberts) + 
    geom_boxplot(aes(x=diffAUC,fill=labs),alpha=.5,colour="black",
                 size=.8,outlier.size = 3) + fontsz.grid

print(p)

summarize_diffs(diffAUC.m2,"AUC")
summarize_diffs(diffAUC.ffc,"AUC")
summarize_diffs(diffAUC.roberts,"AUC")



friedman.test(as.matrix(cbind(auc.model2$auc,auc.ffc$auc,auc.roberts$auc)))

wilcox.test(auc.model2$auc,auc.ffc$auc,paired = TRUE,alternative = "two.sided")
wilcox.test(auc.model2$auc,auc.roberts$auc,paired = TRUE,alternative = "two.sided")
wilcox.test(auc.ffc$auc,auc.roberts$auc,paired = TRUE,alternative = "two.sided")
