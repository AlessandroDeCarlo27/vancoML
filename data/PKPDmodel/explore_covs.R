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
######################## LOAD DATA #####################################

mx_dataset <- read.csv("dataset_mx.csv") %>% filter(TIME==0)
# graphical constants for CorrMat
colori <- brewer.pal(7, "Set3")
pal <- colorRampPalette(colori)
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


# selecting only covs
covs <- mx_dataset[-c(279),] %>% select(-c(C,TIME,Y,AMT,MDV,EVID,ID,RATE))

######## VOLUME ##############################################################
volume_covs <- covs %>% select(c(WT,Hb,HCT))
corr_volume <- cor(volume_covs)

corrplot(corr_volume, method="color",number.digits = 2,  
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # hide correlation coefficient on the principal diagonal
         diag=T,col.lim = c(20, 30),
         col = pal(7))#COL1('YlGn'))
########### CL ########################################################

cl_covs <- covs %>% select(c(SEX,AGE,Creatinine,AnionGap,BUN,HCO3,HCT,Hb,K,LacticAcid,P))

corr_cl <- cor(cl_covs%>% select(-c(SEX)))

corrplot(corr_cl, method="color",number.digits = 2,  
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # hide correlation coefficient on the principal diagonal
         diag=T,col.lim = c(20, 30),
         col = pal(7))#COL1('YlGn'))


continous <- c("AGE","Creatinine","AnionGap","BUN","HCO3","HCT","Hb","K","LacticAcid","P")


p_male_logic <- list()
p_fem_logic <- list()

hist_list <- list()

for (i in 1:length(continous)) {
    c_i <- cbind(cl_covs %>% select(c(continous[i])),cl_covs %>% select(SEX))
    c_i$SEX <- as.factor(c_i$SEX)
    
    hist_list[[i]] <- ggplot(data=c_i,aes_string(x=continous[i])) + 
        geom_density(aes(fill=SEX),alpha=0.3,colour="black",size=0.8) + fontsz.grid

    p_male_logic[[i]] <- (shapiro.test(c_i[c_i$SEX==1,continous[i]]))$p.value >0.05
    
    p_fem_logic[[i]] <- (shapiro.test(c_i[c_i$SEX==0,continous[i]]))$p.value >0.05
    
}

sb <- ggarrange(plotlist = hist_list,ncol = 3,nrow = 4,
                common.legend = T,legend = "top")

fig <- annotate_figure(sb)
print(fig)


box_list <- list()

uman <- list()

for (i in 1:length(continous)) {
    c_i <- cbind(cl_covs %>% select(c(continous[i])),cl_covs %>% select(SEX))
    c_i$SEX <- as.factor(c_i$SEX)
    
    
    uman[[i]] <- (wilcox.test(eval(parse(text=paste(continous[i],"~ SEX",sep=" "))), data=c_i))$p.value
    text_print <- paste("pUMW:",format(uman[[i]], digits=3),sep=" ")
    grob <- grobTree(textGrob(text_print, x=0.1,  y=0.95, hjust=0,
                              gp=gpar(col="red", fontsize=15, fontface="bold")))
    
    
    
    
    box_list[[i]] <- ggplot(data=c_i,aes_string(y=continous[i])) + 
        geom_boxplot(aes(x=SEX,fill=SEX),alpha=.5,colour="black",
                     size=.8,outlier.size = 3) + fontsz.grid + annotation_custom(grob)
    
}


sb <- ggarrange(plotlist = box_list,ncol = 3,nrow = 4,
                common.legend = T,legend = "top")

fig <- annotate_figure(sb)
print(fig)
