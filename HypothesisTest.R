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
median(TOTALSCORE_PWB_STD)
PWBClass=function(var1){
  group=numeric()
  m=0
  for (i in 1:length(var1)) {
    if(var1[i]<=7.4){
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
groupV2=PWBClass(TOTALSCORE_PWB_STD)
Tapply(TOTALSCORE_MBI_STD ~ groupV2,median,data=data1)
test <- wilcox.test(TOTALSCORE_MBI_STD ~ groupV2,data=data1,alternative = "less",paired = FALSE)
test

median(TOTALSCORE_RES_STD)
RESClass=function(var1){
  group=numeric()
  m=0
  for (i in 1:length(var1)) {
    if(var1[i]<=7.1){
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
groupV2=RESClass(TOTALSCORE_RES_STD)
Tapply(TOTALSCORE_MBI_STD ~ groupV2,median,data=data1)
test <- wilcox.test(TOTALSCORE_MBI_STD ~ groupV2,data=data1,alternative = "two.sided",paired = FALSE)
test

median(TEACHING_EXPERIENCE)
TXClass=function(var1){
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
groupV2=TXClass(TEACHING_EXPERIENCE)
Tapply(TOTALSCORE_MBI_STD ~ groupV2,median,data=data1)
test <- wilcox.test(TOTALSCORE_MBI_STD ~ groupV2,data=data1,alternative = "two.sided",paired = FALSE)
test
median(TEACHING_EXPERIENCE)

# Example data (replace this with your actual data)
group1 <- c(15, 18, 20)
group2 <- c(12, 14, 16, 18, 21)
group3 <- c(10, 13, 15, 17)

# Combine the data into a list
data <- list(Group1 = group1, Group2 = group2, Group3 = group3)

# Perform Kruskal-Wallis test
result <- kruskal.test(data)

# Display the result
print(result) 



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


#****************************Emotion.exhaustion scale*******************************************
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

data1 <- data.frame(var1=TOTALSCORE_MBI_STD,var2=MBI_SCALE_EE(SCORE_MBI_S1)) 
kruskal_result1 <- kruskal.test(var1 ~ var2, data = data1)
print(kruskal_result1)
dunn_result1 <- dunn.test(data1$var1, g = data1$var2, method = "bonferroni")
print(dunn_result1)
Tapply(data1$var1 ~ data1$var2,median,data=data1)

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

#****************************Depersonalization*******************************************
TXClass=function(var1){
  group=numeric()
  m=0
  for (i in 1:length(var1)) {
    if(var1[i]>=0&var1[i]<=5){
      m=m+1
      group[m]=1
    }
    if(var1[i]>=6&var1[i]<=10){
      m=m+1
      group[m]=2
    }
    if(var1[i]>=11){
      m=m+1
      group[m]=3
    }
    
  }
  group
}

data1 <- data.frame(var1=TOTALSCORE_MBI_STD,var2=TXClass(TEACHING_EXPERIENCE)) 
kruskal_result1 <- kruskal.test(var1 ~ var2, data = data1)
print(kruskal_result1)
dunn_result1 <- dunn.test(data1$var1, g = data1$var2, method = "bonferroni")
print(dunn_result1)


data2 <- data.frame(var1=TOTALSCORE_PWB_STD,var2=TXClass(TEACHING_EXPERIENCE)) 
kruskal_result2 <- kruskal.test(var1 ~ var2, data = data2)
print(kruskal_result2)
dunn_result2 <- dunn.test(data2$var1, g = data2$var2, method = "bonferroni")
print(dunn_result2)

data3 <- data.frame(var1=TOTALSCORE_RES_STD,var2=TXClass(TEACHING_EXPERIENCE)) 
kruskal_result3 <- kruskal.test(var1 ~ var2, data = data3)
print(kruskal_result3)
dunn_result3 <- dunn.test(data3$var1, g = data3$var2, method = "bonferroni")
print(dunn_result3)

#****************************Red.Pers.Accompl*******************************************
TXClass=function(var1){
  group=numeric()
  m=0
  for (i in 1:length(var1)) {
    if(var1[i]>=0&var1[i]<=30){
      m=m+1
      group[m]=1
    }
    if(var1[i]>=31&var1[i]<=36){
      m=m+1
      group[m]=2
    }
    if(var1[i]>=37){
      m=m+1
      group[m]=3
    }
    
  }
  group
}

data1 <- data.frame(var1=TOTALSCORE_MBI_STD,var2=TXClass(TEACHING_EXPERIENCE)) 
kruskal_result1 <- kruskal.test(var1 ~ var2, data = data1)
print(kruskal_result1)
dunn_result1 <- dunn.test(data1$var1, g = data1$var2, method = "bonferroni")
print(dunn_result1)


data2 <- data.frame(var1=TOTALSCORE_PWB_STD,var2=TXClass(TEACHING_EXPERIENCE)) 
kruskal_result2 <- kruskal.test(var1 ~ var2, data = data2)
print(kruskal_result2)
dunn_result2 <- dunn.test(data2$var1, g = data2$var2, method = "bonferroni")
print(dunn_result2)

data3 <- data.frame(var1=TOTALSCORE_RES_STD,var2=TXClass(TEACHING_EXPERIENCE)) 
kruskal_result3 <- kruskal.test(var1 ~ var2, data = data3)
print(kruskal_result3)
dunn_result3 <- dunn.test(data3$var1, g = data3$var2, method = "bonferroni")
print(dunn_result3)
#****************************Percentile 33, 66 and 99 *******************************************
TXClass=function(var1){
  group=numeric()
  m=0
  for (i in 1:length(var1)) {
    if(var1[i]>=0&var1[i]<=11){
      m=m+1
      group[m]=1
    }
    if(var1[i]>=12&var1[i]<=21){
      m=m+1
      group[m]=2
    }
    if(var1[i]>=22){
      m=m+1
      group[m]=3
    }
    
  }
  group
}

data1 <- data.frame(var1=TOTALSCORE_MBI_STD,var2=TXClass(TEACHING_EXPERIENCE)) 
kruskal_result1 <- kruskal.test(var1 ~ var2, data = data1)
print(kruskal_result1)
dunn_result1 <- dunn.test(data1$var1, g = data1$var2, method = "bonferroni")
print(dunn_result1)


data2 <- data.frame(var1=TOTALSCORE_PWB_STD,var2=TXClass(TEACHING_EXPERIENCE)) 
kruskal_result2 <- kruskal.test(var1 ~ var2, data = data2)
print(kruskal_result2)
dunn_result2 <- dunn.test(data2$var1, g = data2$var2, method = "bonferroni")
print(dunn_result2)

data3 <- data.frame(var1=TOTALSCORE_RES_STD,var2=TXClass(TEACHING_EXPERIENCE)) 
kruskal_result3 <- kruskal.test(var1 ~ var2, data = data3)
print(kruskal_result3)
dunn_result3 <- dunn.test(data3$var1, g = data3$var2, method = "bonferroni")
print(dunn_result3)











data1


q=quantile(TEACHING_EXPERIENCE,probs=c(0.33,0.66,0.99))
q


contains_decimal <- any(grepl("\\.", as.character(TEACHING_EXPERIENCE)))
print(contains_decimal)
