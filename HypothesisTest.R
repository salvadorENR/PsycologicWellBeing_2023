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
group1 <- c(15, 18, 20, 22, 25)
group2 <- c(12, 14, 16, 18, 21)
group3 <- c(10, 13, 15, 17, 19)

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




















