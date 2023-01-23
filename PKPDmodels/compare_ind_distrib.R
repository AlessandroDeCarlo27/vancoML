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

######### LOAD DATA SOURCES #############################################

model2 <- read.csv("1cmt_V_wt_CL_age_cr_sex_prop/vanco_1cmt_V_wt_CL_age_cr_sex_prop/IndividualParameters/estimatedIndividualParameters.txt") %>%
    select(c(id,V_mode,Cl_mode))

final_fullcovs <- read.csv("1cmt_fullCovs_NO_LAcl_HCTv_SEXv_AGcl_prop/vanco_1cmt_fullCovs_NO_LAcl_HCTv_SEXv_AGcl_prop/IndividualParameters/estimatedIndividualParameters.txt") %>%
                    select(c(id,V_mode,Cl_mode))

roberts <- read.csv("0_Literature_Models/Roberts/Roberts_add_originalCV/roberts_add_originalCV/IndividualParameters/estimatedIndividualParameters.txt") %>% 
            select(c(id,V_mode,Cl_mode))


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


################ statistical tests

friedman.test(as.matrix(cbind(model2$Cl_mode,final_fullcovs$Cl_mode,roberts$Cl_mode)))

wilcox.test(model2$Cl_mode,final_fullcovs$Cl_mode,paired = TRUE,alternative = "two.sided")
wilcox.test(model2$Cl_mode,roberts$Cl_mode,paired = TRUE,alternative = "two.sided")
wilcox.test(roberts$Cl_mode,final_fullcovs$Cl_mode,paired = TRUE,alternative = "two.sided")







friedman.test(as.matrix(cbind(model2$V_mode,final_fullcovs$V_mode,roberts$V_mode)))

wilcox.test(model2$V_mode,final_fullcovs$V_mode,paired = TRUE,alternative = "two.sided")
wilcox.test(model2$V_mode,roberts$V_mode,paired = TRUE,alternative = "two.sided")
wilcox.test(final_fullcovs$V_mode,roberts$V_mode,paired = TRUE,alternative = "two.sided")


