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

m_ll <- as.matrix(cbind(model2$importanceSampling,ffc$importanceSampling,roberts$importanceSampling,base$importanceSampling,
                        revilla$importanceSampling))

colnames(m_ll) <- mod_names
amins <- argmin(m_ll,rows = T)

amins_names <- data.frame(BestModel = as.factor(mod_names[amins]))


p<-ggplot(data=amins_names, aes(x=BestModel,fill=BestModel)) +
    geom_bar(stat="count") + fontsz.grid
plot(p)


################## likelihood of the mixture model ###########################
scores <- c()
labs_best <- c()
for (i in 1:nrow(m_ll)) {
    scores <- c(scores,m_ll[i,amins[i]])
    labs_best <- c(labs_best,mod_names[amins[i]])
}
