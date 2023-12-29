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
DBT=read.csv("DBHE.csv",sep = ",")
DBT=as.data.frame(DBT)
attach(DBT)
TEACHING_EXPERIENCE

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

RangoYTE=TXClass(TEACHING_EXPERIENCE)
DBTM=data.frame(TOTALSCORE_MBI_STD,TOTALSCORE_PWB_STD,TOTALSCORE_RES_STD,RangoYTE)

modelo<-lme(TOTALSCORE_MBI_STD~TOTALSCORE_PWB_STD+TOTALSCORE_RES_STD, random = list(TOTALSCORE_PWB_STD~1,TOTALSCORE_RES_STD~1)|RangoYTE, data = DBTM)
modelo <-lme(TOTALSCORE_MBI_STD~TOTALSCORE_PWB_STD+TOTALSCORE_RES_STD, random = list(RangoYTE = pdIdent(~ TOTALSCORE_PWB_STD+TOTALSCORE_RES_STD)), data = DBTM)
summary(modelo)

#******************************* Analysis *******************************************************************************************************

m7<-lme(STUDENTSCORE~SAR_SCORE+HMG_SCORE+TEACHERSCORE, random = ~1|ID_TEACHER, method = "ML", data = DBT)
summary(m7)

# Generate a hypothetical dataset
set.seed(123)
n <- 100  # Number of observations
groups <- rep(1:10, each = 10)  # 10 groups with 10 individuals each
id <- rep(1:10, each = 10)  # Individuals within each group
x1 <- rnorm(n)  # Predictor 1
x2 <- rnorm(n)  # Predictor 2
y <- 2 + 0.5 * x1 + 1.5 * x2 + rnorm(n)  # Response variable with random noise

# Create a data frame
data <- data.frame(id, groups, x1, x2, y)

# Fit a multilevel model with random slopes for both x1 and x2
model <- lme(y ~ x1 + x2, random = list(groups = pdIdent(~ x1 + x2)), data = data)

# Print the summary of the model
summary(model)

#************************* Examples of multilevel regression model ************************************
#Model 1 (10.3.2 Unconditional Means Model (Model 1: Null Model)
m1 <- lme(mathach ~ 1, random = ~1|SCH_ID, na.action = "na.omit", method = "ML", data = chp10)
#Model 2 (Random-intercept model)
m2 <- lme(mathach ~ gceffic, random = ~1|SCH_ID, na.action="na.omit", method="ML", data=chp10)
#Model 3 (Random-coefficient model)
m3 <- lme(mathach ~ gceffic, random = ~gceffic|SCH_ID, na.action="na.omit", method="ML", data=chp10)
#Model 4
m4 <- lme(mathach ~ gceffic + public + csclimat, random = ~gceffic|SCH_ID, na.action = "na.omit", method = "ML", data = chp10)
#Model 5 (Contextual model with cross-level interactions)
m5 <- lme(mathach ~ gceffic + public + csclimat + public*gceffic + csclimat*gceffic, random = ~gceffic|SCH_ID, method="ML", data=chp10)

#To divide the group of teachers in teaching experience time, 2.5, 5, 10, 15, 20, ...
# Multilevel regression model with random slopes for both predictors
model <- lme(response ~ predictor1 + predictor2, random = list(predictor1 ~ 1, predictor2 ~ 1) | group, data = your_data)

# Display the model summary
summary(model)




