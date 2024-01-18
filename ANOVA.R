#***************************** Loading the required packages *************************************************************************************************************************************************************************************************************************************
install.packages("dunn.test")
install.packages("PMCMRplus")

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
#*************************************** Reading the data base *******************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************
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

#++++++++++++++++++++++++++ Functions' creation ++++++++++++++++++++++++++++++++
TXClass=function(var1){
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
    if(var1[i]>10&var1[i]<=15){
      m=m+1
      rango[m]=4
    }
    if(var1[i]>15){
      m=m+1
      rango[m]=5
    }
  }
  rango
}
Edad=function(var1){
  rango=numeric()
  m=0
  for (i in 1:length(var1)) {
    if(var1[i]<=20){
      m=m+1
      rango[m]=1
    }
    if(var1[i]>20&var1[i]<=30){
      m=m+1
      rango[m]=2
    }
    if(var1[i]>30&var1[i]<=40){
      m=m+1
      rango[m]=3
    }
    if(var1[i]>40&var1[i]<=50){
      m=m+1
      rango[m]=4
    }
    if(var1[i]>50){
      m=m+1
      rango[m]=5
    }
  }
  rango
}
Edad2=function(var1){
  rango=numeric()
  m=0
  for (i in 1:length(var1)) {
    if(var1[i]<=20){
      m=m+1
      rango[m]=1
    }
    if(var1[i]>20&var1[i]<=30){
      m=m+1
      rango[m]=2
    }
    if(var1[i]>30&var1[i]<=40){
      m=m+1
      rango[m]=3
    }
    if(var1[i]>40){
      m=m+1
      rango[m]=5
    }
  }
  rango
}
RESNiveles=function(var1){
  rango=numeric()
  m=0
  for (i in 1:length(var1)) {
    if(var1[i]<=100){
      m=m+1
      rango[m]=1
    }
    if(var1[i]>100&var1[i]<=131){
      m=m+1
      rango[m]=2
    }
    if(var1[i]>131&var1[i]<=145){
      m=m+1
      rango[m]=3
    }
    if(var1[i]>145&var1[i]<=161){
      m=m+1
      rango[m]=4
    }
    if(var1[i]>161){
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

MBI_SCALE_EE=function(var1){
  rango=numeric()
  m=0
  for (i in 1:length(var1)) {
    if(var1[i]<=18){
      m=m+1
      rango[m]=1
    }
    if(var1[i]>=19&var1[i]<=26){
      m=m+1
      rango[m]=2
    }
    if(var1[i]>=27){
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
    if(var1[i]<=5){
      m=m+1
      rango[m]=1
    }
    if(var1[i]>=6&var1[i]<=9){
      m=m+1
      rango[m]=2
    }
    if(var1[i]>=10){
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
    if(var1[i]<=33){
      m=m+1
      rango[m]=1
    }
    if(var1[i]>=34&var1[i]<=39){
      m=m+1
      rango[m]=2
    }
    if(var1[i]>=40){
      m=m+1
      rango[m]=3
    }
  }
  rango
}
TEACHING_EXPERIENCE1=TXClass(TEACHING_EXPERIENCE)
EdadProfesores=Edad(AGE)
EdadProfesores2=Edad2(AGE)
PWB_I=PWBNiveles(TOTALSCORE_PWB)
PWB_I=PWBNiveles2(TOTALSCORE_PWB)
RES_I=RESNiveles(TOTALSCORE_RES)
MBI_EE=MBI_SCALE_EE(SCORE_MBI_S1)
MBI_DP=MBI_SCALE_DP(SCORE_MBI_S2)
MBI_PA=MBI_SCALE_PA(SCORE_MBI_S3)
#****************************ANOVA ONE FACTOR*******************************************
#++++++++++++++++++++++++++++TESTS++++++++++++++++++++++++++++++++++++++++++++++
data1 <- data.frame(var1=TOTALSCORE_MBI_STD,var2=MBI_SCALE_EE(SCORE_MBI_S1)) 
kruskal_result1 <- kruskal.test(var1 ~ var2, data = data1)
print(kruskal_result1)
dunn_result1 <- dunn.test(data1$var1, g = data1$var2, method = "bonferroni")
print(dunn_result1)
Tapply(data1$var1 ~ data1$var2,median,data=data1)
#The interaction is not significant

data2 <- data.frame(var1=TOTALSCORE_PWB_STD,var2=MBI_SCALE_DP(SCORE_MBI_S2)) 
kruskal_result2 <- kruskal.test(var1 ~ var2, data = data2)
print(kruskal_result2)
dunn_result2 <- dunn.test(data2$var1, g = data2$var2, method = "bonferroni")
print(dunn_result2)

data3 <- data.frame(var1=TOTALSCORE_RES_STD,var2=TXClass(TEACHING_EXPERIENCE)) 
kruskal_result3 <- kruskal.test(var1 ~ var2, data = data3)
print(kruskal_result3)
dunn_result3 <- dunn.test(data3$var1, g = data3$var2, method = "bonferroni")
print(dunn_result3)

#************************* ANOVA TWO FACTORS ***********************************
# Create a data frame
data=data.frame(TEACHING_EXPERIENCE1,MBI_EE,TOTALSCORE_PWB)
# Step 2: Conduct Two-Way ANOVA with Interaction
model=lm(TOTALSCORE_PWB~MBI_EE*TEACHING_EXPERIENCE1,data=data)
anova_result=anova(model)
# Step 3: Interpret Results
print(anova_result)

data2=data.frame(TEACHING_EXPERIENCE1,MBI_DP,TOTALSCORE_PWB)
model=lm(TOTALSCORE_PWB_STD~MBI_DP*TEACHING_EXPERIENCE1,data=data2)
anova_result=anova(model)
print(anova_result)

data3=data.frame(TEACHING_EXPERIENCE1,MBI_PA,TOTALSCORE_PWB)
model=lm(TOTALSCORE_PWB_STD~MBI_PA*TEACHING_EXPERIENCE1,data=data3)
anova_result=anova(model)
print(anova_result)
#*******************************************************************************
# Create a data frame
data=data.frame(EdadProfesores,MBI_EE,TOTALSCORE_PWB)
# Step 2: Conduct Two-Way ANOVA with Interaction
model=lm(TOTALSCORE_PWB~MBI_EE*EdadProfesores,data=data)
anova_result=anova(model)
# Step 3: Interpret Results
print(anova_result)

data2=data.frame(EdadProfesores,MBI_DP,TOTALSCORE_PWB)
model=lm(TOTALSCORE_PWB_STD~MBI_DP*EdadProfesores,data=data2)
anova_result=anova(model)
print(anova_result)

data3=data.frame(EdadProfesores,MBI_PA,TOTALSCORE_PWB)
model=lm(TOTALSCORE_PWB_STD~MBI_PA*EdadProfesores,data=data3)
anova_result=anova(model)
print(anova_result)
#*******************************************************************************
data1=data.frame(RES_I,MBI_EE,TOTALSCORE_PWB)
model=lm(TOTALSCORE_PWB~MBI_EE*RES_I,data=data1)
anova_result=anova(model)
print(anova_result)

data2=data.frame(RES_I,MBI_DP,TOTALSCORE_PWB)
model=lm(TOTALSCORE_PWB~MBI_DP*RES_I,data=data2)
anova_result=anova(model)
print(anova_result)

data3=data.frame(RES_I,MBI_PA,TOTALSCORE_PWB)
model=lm(TOTALSCORE_PWB~MBI_PA*RES_I,data=data3)
anova_result=anova(model)
print(anova_result)
#*******************************************************************************
data1=data.frame(TEACHING_EXPERIENCE1,TOTALSCORE_MBI,EdadProfesores)
model=lm(TOTALSCORE_MBI~TEACHING_EXPERIENCE1*EdadProfesores,data=data1)
anova_result=anova(model)
print(anova_result)

data2=data.frame(TEACHING_EXPERIENCE1,TOTALSCORE_MBI,EdadProfesores2)
model=lm(TOTALSCORE_MBI~TEACHING_EXPERIENCE1*EdadProfesores2,data=data2)
anova_result=anova(model)
print(anova_result)

data3=data.frame(PWB_I,TOTALSCORE_MBI,RES_I)
model=lm(TOTALSCORE_MBI~PWB_I*RES_I,data=data3)
anova_result=anova(model)
print(anova_result)
