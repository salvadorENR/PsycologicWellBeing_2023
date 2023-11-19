#***************************** Loading the required packages *************************************************************************************************************************************************************************************************************************************
library(hrbrthemes)
library(stargazer)               
library(plyr)
library(ggridges)
library(lavaan)
library(semPlot)
library(pastecs)
library(tidyverse)
library(insight)
library(shiny)
library(car)
library(fmsb)
library(gapminder)
library(ltm)
library(Hmisc)
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
#*************************************** Reading the data base *******************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************
data1=read.csv("DBHE.csv",sep=",")
data1=as.data.frame(data1)
attach(data1)
#************************************** Hypothesis tests ************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************
#Wilcoxon test by school zone
#Hypothesis test1 and 2 
Tapply(SAR_SCORE ~ SCHOOL_ZONE,median,data=data1)
test <- wilcox.test(SAR_SCORE ~ SCHOOL_ZONE,data=data1,alternative = "greater",paired = FALSE)
test
Tapply(HMG_SCORE ~ SCHOOL_ZONE,median,data=data1)
test <- wilcox.test(HMG_SCORE ~ SCHOOL_ZONE,data=data1,alternative = "greater",paired = FALSE)
test
#Wilcoxon test by gender
#Hypothesis test 3 and 4 
Tapply(SAR_SCORE ~ GENDER,median,data=data1)
test <- wilcox.test(SAR_SCORE ~ GENDER,data=data1,alternative = "two.sided",paired = FALSE)
test
Tapply(HMG_SCORE ~ GENDER,median,data=data1)
test <- wilcox.test(HMG_SCORE ~ GENDER,data=data1,alternative = "greater",paired = FALSE)
test
#Wilcoxon test by school zone
#Hypothesis test 5
Tapply(STUDENTSCORE ~ SCHOOL_ZONE,median,data=data1)
test <- wilcox.test(STUDENTSCORE ~ SCHOOL_ZONE,data=data1,alternative = "less",paired = FALSE)
test

#Wilcoxon test by teacher PCK performance
#Hypothesis test 7

teacherPCK=function(var1){
  group=numeric()
  m=0
  for (i in 1:length(var1)) {
    if(var1[i]<=9){
      m=m+1
      group[m]=1
    }
    else{
      m=m+1
      group[m]=2
    }
  }
  group
}
groupV1=teacherPCK(SCORE_PCK)
Tapply(STUDENTSCORE ~ groupV1,median,data=data1)
test <- wilcox.test(STUDENTSCORE ~groupV1,data=data1,alternative = "less",paired = FALSE)
test
#Wilcoxon test by teacher experience
#Hypothesis test 8
teacherEXP=function(var1){
  group=numeric()
  m=0
  for (i in 1:length(var1)) {
    if(var1[i]<=15){
      m=m+1
      group[m]=1
    }
    else{
      m=m+1
      group[m]=2
    }
    
  }
  group
}
groupV2=teacherEXP(TEACHINGEXPERIENCE)
Tapply(STUDENTSCORE ~ groupV2,median,data=data1)
Tapply(SCORE_SMK ~ groupV2,median,data=data1)
Tapply(SCORE_PCK ~ groupV2,median,data=data1)


