#Loading packages
install.packages("nlme")                   
install.packages("lmerTest")              
install.packages("lme4")                 
install.packages("ggeffects")               
install.packages("stargazer")               
install.packages("texreg") 

library(datos)
library(ggplot2)
library(reshape2)
library(ggsci)
library(readr)
library(scales)
library(stringr)
library(utf8)
library(devtools)
library(ggpubr)
library(nlme)                   
library(lmerTest)              
library(lme4)                 
library(ggeffects)               
library(stargazer)               
library(texreg)  
library(dplyr)

#Loading data base
DBT=read.csv("Data_set.csv",sep = ";")
DBT=as.data.frame(DBT)
attach(DBT)

#******************************* Analysis *******************************************************************************************************

m7<-lme(STUDENTSCORE~SAR_SCORE+HMG_SCORE+TEACHERSCORE, random = ~1|ID_TEACHER, method = "ML", data = DBT)
summary(m7)


#************************* Examples of multilevel regression model ************************************
#Model 1 (10.3.2 Unconditional Means Model (Model 1: Null Model)
m1 <- lme(mathach ~ 1, random = ~1|SCH_ID, na.action = "na.omit", method = "ML", data = chp10)
#Model 2 




