#***************************** Loading the required packages *************************************************************************************************************************************************************************************************************************************
install.packages("dunn.test")
install.packages("PMCMRplus")


# Packages ----------------------------------------------------------------
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
library(dunn.test)
library(PMCMRplus)



#Load Data Base ----------------------------------------------------------
dataG=read.csv("DBHE.csv",sep=",")
dataG=as.data.frame(dataG)
attach(dataG)
#If your data do not follow a normal distribution and you want to compare means between three groups, you can use 
#a non-parametric test. One common non-parametric test for this scenario is the Kruskal-Wallis test. This test is
#an extension of the Mann-Whitney U test to three or more groups. Here's how you can perform the Kruskal-Wallis 
#test in R:
#In this example, group1, group2, and group3 represent the quantitative data for each group. You need to replace 
#these with your actual data.

#The kruskal.test function will perform the Kruskal-Wallis test, and the result will include the test statistic, 
#degrees of freedom, and the p-value.

#If the p-value is below your chosen significance level (e.g., 0.05), you can reject the null hypothesis, suggesting
#that there is a significant difference in medians between at least two of the groups. If the Kruskal-Wallis test indicates 
#significant differences, you might consider post-hoc tests (e.g., Dunn's test) to identify which specific group pairs differ.

#Remember that non-parametric tests like Kruskal-Wallis are less sensitive to normality assumptions but might have 
#less power than parametric tests when normality is met. Always consider the characteristics of your data and the 
#assumptions of the statistical test when making interpretations.



#Functions for making factors for Kruskal Wallis -------------------------
TExp=function(var1){
  rango=numeric()
  m=0
  for (i in 1:length(var1)) {
    if(var1[i]<=2.5){
      m=m+1
      rango[m]=1
    }
    if(var1[i]>2.5&var1[i]<=5){
      m=m+1
      rango[m]=2
    }
    if(var1[i]>5&var1[i]<=10){
      m=m+1
      rango[m]=3
    }
    if(var1[i]>10){
      m=m+1
      rango[m]=4
    }
  }
  rango
}
Edad=function(var1){
  rango=numeric()
  m=0
  for (i in 1:length(var1)) {
    if(var1[i]>20&var1[i]<=30){
      m=m+1
      rango[m]=1
    }
    if(var1[i]>30&var1[i]<=40){
      m=m+1
      rango[m]=2
    }
    if(var1[i]>40){
      m=m+1
      rango[m]=3
    }
  }
  rango
}
RESNiveles=function(var1){
  rango=numeric()
  m=0
  for (i in 1:length(var1)) {
    if(var1[i]<=5.7){
      m=m+1
      rango[m]=1
    }
    if(var1[i]>5.7&var1[i]<=7.5){
      m=m+1
      rango[m]=2
    }
    if(var1[i]>7.5&var1[i]<=8.3){
      m=m+1
      rango[m]=3
    }
    if(var1[i]>8.3&var1[i]<=9.2){
      m=m+1
      rango[m]=4
    }
    if(var1[i]>9.2){
      m=m+1
      rango[m]=5
    }
  }
  rango
}
PWBNiveles=function(var1){
  rango=numeric()
  m=0
  for (i in 1:length(var1)) {
    if(var1[i]<=168){
      m=m+1
      rango[m]=1
    }
    if(var1[i]>168&var1[i]<=336){
      m=m+1
      rango[m]=2
    }
    if(var1[i]>336&var1[i]<=504){
      m=m+1
      rango[m]=3
    }
  
  }
  rango
}
PWBNiveles2=function(var1){
  rango=numeric()
  m=0
  for (i in 1:length(var1)) {
    if(var1[i]<=quantile(var1, 0.25)){
      m=m+1
      rango[m]=1
    }
    if(var1[i]>quantile(var1, 0.25)&var1[i]<=quantile(var1, 0.75)){
      m=m+1
      rango[m]=2
    }
    if(var1[i]>quantile(var1, 0.75)){
      m=m+1
      rango[m]=3
    }
    
  }
  rango
}
MBI_SCALE_EE_STD=function(var1){
  rango=numeric()
  m=0
  for (i in 1:length(var1)) {
    if(var1[i]<=3.5){#18
      m=m+1
      rango[m]=1
    }
    if(var1[i]>3.5&var1[i]<=5){ #19 and 26
      m=m+1
      rango[m]=2
    }
    if(var1[i]>5){ #27
      m=m+1
      rango[m]=3 
    }
  }
  rango
}
MBI_SCALE_DP_STD=function(var1){
  rango=numeric()
  m=0
  for (i in 1:length(var1)) {
    if(var1[i]<=1.7){#5
      m=m+1
      rango[m]=1
    }
    if(var1[i]>1.7&var1[i]<=3.0){#6 a 9
      m=m+1
      rango[m]=2
    }
    if(var1[i]>3){#10
      m=m+1
      rango[m]=3
    }
  }
  rango
}
MBI_SCALE_PA_STD=function(var1){
  rango=numeric()
  m=0
  for (i in 1:length(var1)) {
    if(var1[i]>8.3){#40
      m=m+1
      rango[m]=1
    }
    if(var1[i]>6.9&var1[i]<=8.3){#34 and 39
      m=m+1
      rango[m]=2
    }
    if(var1[i]<=6.9){#33 
      m=m+1
      rango[m]=3
    }
  }
  rango
}
MBI_SCALE_EE=function(var1){
  rango=numeric()
  m=0
  for (i in 1:length(var1)) {
    if(var1[i]<=18){#18
      m=m+1
      rango[m]=1
    }
    if(var1[i]>=19&var1[i]<=26){ #19 and 26
      m=m+1
      rango[m]=2
    }
    if(var1[i]>=27){ #27
      m=m+1
      rango[m]=3 
    }
  }
  rango
}
MBI_SCALE_DP=function(var1){
  rango=numeric()
  m=0
  for (i in 1:length(var1)) {
    if(var1[i]<=5){#5
      m=m+1
      rango[m]=1
    }
    if(var1[i]>=6&var1[i]<=9){#6 a 9
      m=m+1
      rango[m]=2
    }
    if(var1[i]>=10){#10
      m=m+1
      rango[m]=3
    }
  }
  rango
}
MBI_SCALE_PA=function(var1){
  rango=numeric()
  m=0
  for (i in 1:length(var1)) {
    if(var1[i]>=40){#40
      m=m+1
      rango[m]=1
    }
    if(var1[i]>=34&var1[i]<=39){#34 and 39
      m=m+1
      rango[m]=2
    }
    if(var1[i]<=33){#33 
      m=m+1
      rango[m]=3
    }
  }
  rango
}

TEACHING_EXPERIENCE=TExp(TEACHING_EXPERIENCE)
EdadProfesores=Edad(AGE)

PWB_I=PWBNiveles(TOTALSCORE_PWB)
PWB_I=PWBNiveles2(TOTALSCORE_PWB_STD)

RES_I=RESNiveles(TOTALSCORE_RES_STD)

MBI_EE=MBI_SCALE_EE(SCORE_MBI_S1)
MBI_DP=MBI_SCALE_DP(SCORE_MBI_S2)
MBI_PA=MBI_SCALE_PA(SCORE_MBI_S3)
MBI_EE=MBI_SCALE_EE_STD(SCORE_MBI_S1_STD)
MBI_DP=MBI_SCALE_DP_STD(SCORE_MBI_S2_STD)
MBI_PA=MBI_SCALE_PA_STD(SCORE_MBI_S3_STD_2)

sort(SCORE_MBI_S1)
sort(SCORE_MBI_S2)
sort(SCORE_MBI_S3)

table(MBI_EE)
table(MBI_DP)
table(MBI_PA)



#****************************Kruskal Wallis*******************************************
#++++++++++++++++++++++++++++TESTS++++++++++++++++++++++++++++++++++++++++++++++

#External Exhaustation_and_TeachingExp ---------------------------------------------------------
data1 <- data.frame(var1=SCORE_MBI_S1_STD,var2=TEACHING_EXPERIENCE) 
kruskal_result1 <- kruskal.test(var1 ~ var2, data = data1)
print(kruskal_result1)
dunn_result1 <- dunn.test(data1$var1, g = data1$var2, method = "bonferroni")
print(dunn_result1)
Tapply(data1$var1 ~ data1$var2,median,data=data1)
#Depersonalization_and_TeachingExp ---------------------------------------------------------
data1 <- data.frame(var1=SCORE_MBI_S2_STD,var2=TEACHING_EXPERIENCE) 
kruskal_result1 <- kruskal.test(var1 ~ var2, data = data1)
print(kruskal_result1)
dunn_result1 <- dunn.test(data1$var1, g = data1$var2, method = "bonferroni")
print(dunn_result1)
Tapply(data1$var1 ~ data1$var2,median,data=data1)
#Personal Accomplishment_and_TeachingExp ---------------------------------------------------------
data1 <- data.frame(var1=SCORE_MBI_S3_STD_2,var2=TEACHING_EXPERIENCE) 
kruskal_result1 <- kruskal.test(var1 ~ var2, data = data1)
print(kruskal_result1)
dunn_result1 <- dunn.test(data1$var1, g = data1$var2, method = "bonferroni")
print(dunn_result1)
Tapply(data1$var1 ~ data1$var2,median,data=data1)
#External Exhaustation_and_Age ---------------------------------------------------------
data1 <- data.frame(var1=SCORE_MBI_S1_STD,var2=EdadProfesores) 
kruskal_result1 <- kruskal.test(var1 ~ var2, data = data1)
print(kruskal_result1)
dunn_result1 <- dunn.test(data1$var1, g = data1$var2, method = "bonferroni")
print(dunn_result1)
Tapply(data1$var1 ~ data1$var2,median,data=data1)

#Depersonalization_and_Age ---------------------------------------------------------
data1 <- data.frame(var1=SCORE_MBI_S2_STD,var2=EdadProfesores) 
kruskal_result1 <- kruskal.test(var1 ~ var2, data = data1)
print(kruskal_result1)
dunn_result1 <- dunn.test(data1$var1, g = data1$var2, method = "bonferroni")
print(dunn_result1)
Tapply(data1$var1 ~ data1$var2,median,data=data1)

#Personal Accomplishment_and_Age ---------------------------------------------------------
data1 <- data.frame(var1=SCORE_MBI_S3_STD_2,var2=EdadProfesores) 
kruskal_result1 <- kruskal.test(var1 ~ var2, data = data1)
print(kruskal_result1)
dunn_result1 <- dunn.test(data1$var1, g = data1$var2, method = "bonferroni")
print(dunn_result1)
Tapply(data1$var1 ~ data1$var2,median,data=data1)



#Burnout_and_Psycological_Well Being -------------------------------------
data2 <- data.frame(var1=TOTALSCORE_MBI_STD,var2=PWB_I) 
kruskal_result2 <- kruskal.test(var1 ~ var2, data =data2)
print(kruskal_result2)
dunn_result2 <- dunn.test(data2$var1, g = data2$var2, method = "bonferroni")
print(dunn_result2)
Tapply(data1$var1 ~ data1$var2,median,data=data2)



#Burnout_and_Resilience -------------------------------------------------
data3 <- data.frame(var1=TOTALSCORE_MBI_STD,var2=RES_I) 
kruskal_result3 <- kruskal.test(var1 ~ var2, data = data3)
print(kruskal_result3)
dunn_result3 <- dunn.test(data3$var1, g = data3$var2, method = "bonferroni")
print(dunn_result3)
Tapply(data3$var1 ~ data3$var2,median,data=data3)





#External Exhaustation and pwb -------------------------------------------
data1 <- data.frame(var1=SCORE_MBI_S1_STD,var2=PWB_I) 
kruskal_result1 <- kruskal.test(var1 ~ var2, data = data1)
print(kruskal_result1)
dunn_result1 <- dunn.test(data1$var1, g = data1$var2, method = "bonferroni")
print(dunn_result1)
Tapply(data1$var1 ~ data1$var2,median,data=data1)

#Depersonalization_and_pwb ---------------------------------------------------------
data1 <- data.frame(var1=SCORE_MBI_S2_STD,var2=PWB_I) 
kruskal_result1 <- kruskal.test(var1 ~ var2, data = data1)
print(kruskal_result1)
dunn_result1 <- dunn.test(data1$var1, g = data1$var2, method = "bonferroni")
print(dunn_result1)
Tapply(data1$var1 ~ data1$var2,median,data=data1)

#Personal Accomplishment_and_pwb ---------------------------------------------------------
data1 <- data.frame(var1=SCORE_MBI_S3_STD_2,var2=PWB_I) 
kruskal_result1 <- kruskal.test(var1 ~ var2, data = data1)
print(kruskal_result1)
dunn_result1 <- dunn.test(data1$var1, g = data1$var2, method = "bonferroni")
print(dunn_result1)
Tapply(data1$var1 ~ data1$var2,median,data=data1)




#External Exhaustation_and_res ---------------------------------------------------------
data1 <- data.frame(var1=SCORE_MBI_S1_STD,var2=RES_I) 
kruskal_result1 <- kruskal.test(var1 ~ var2, data = data1)
print(kruskal_result1)
dunn_result1 <- dunn.test(data1$var1, g = data1$var2, method = "bonferroni")
print(dunn_result1)
Tapply(data1$var1 ~ data1$var2,median,data=data1)

#Depersonalization_and_res ---------------------------------------------------------
data1 <- data.frame(var1=SCORE_MBI_S2_STD,var2=RES_I) 
kruskal_result1 <- kruskal.test(var1 ~ var2, data = data1)
print(kruskal_result1)
dunn_result1 <- dunn.test(data1$var1, g = data1$var2, method = "bonferroni")
print(dunn_result1)
Tapply(data1$var1 ~ data1$var2,median,data=data1)

#Personal Accomplishment_and_res ---------------------------------------------------------
data1 <- data.frame(var1=SCORE_MBI_S3_STD_2,var2=RES_I) 
kruskal_result1 <- kruskal.test(var1 ~ var2, data = data1)
print(kruskal_result1)
dunn_result1 <- dunn.test(data1$var1, g = data1$var2, method = "bonferroni")
print(dunn_result1)
Tapply(data1$var1 ~ data1$var2,median,data=data1)



