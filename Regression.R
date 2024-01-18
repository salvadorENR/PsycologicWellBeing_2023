#Loading packages
install.packages("nlme")                   
install.packages("lmerTest")              
install.packages("lme4")                 
install.packages("ggeffects")               
install.packages("stargazer")               
install.packages("texreg") 
install.packages(c("lme4", "MuMIn"))
install.packages("caTools")

library(lme4)
library(MuMIn)
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
library(caTools)

#***********************************************Loading data base *******************************************************
DBT=read.csv("DBHE.csv",sep = ",")
DBT=as.data.frame(DBT)
attach(DBT)
#********************Creating the function to classify the teachers according to their time of teaching experience ******************************************************************************************************************
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
#************************************Application of the function to classify the teachers******************************************************************************************************************
RangoYTE=TXClass(TEACHING_EXPERIENCE)
#****************************************** Creating the data frame to apply the modelling ******************************************************************************************************************
DBTM=data.frame(TOTALSCORE_MBI_STD,TOTALSCORE_PWB_STD,TOTALSCORE_RES_STD,RangoYTE)
#*************** First model, TOTALSCORE_MBI_STD=TOTALSCORE_PWB_STD+TOTALSCORE_RES_STD (Multilevel) **************************************************************************************************************************************
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ Adjusting the model +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Model1<-lme(TOTALSCORE_MBI_STD~TOTALSCORE_PWB_STD+TOTALSCORE_RES_STD, random = list(RangoYTE = pdIdent(~ TOTALSCORE_PWB_STD+TOTALSCORE_RES_STD)), data = DBTM)
summary(Model1)
#*************** Second model, TOTALSCORE_MBI_STD=TOTALSCORE_PWB_STD (Multilevel)**************************************************************************************************************************************
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ Adjusting the model +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Model2=lme(TOTALSCORE_MBI_STD~TOTALSCORE_PWB_STD, random = ~TOTALSCORE_PWB_STD|RangoYTE, na.action="na.omit", method="ML", data=DBTM)
summary(Model2)
#*************** Third model, TOTALSCORE_MBI_STD=TOTALSCORE_PWB_STD (Simple model) **************************************************************************************************************************************
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ Adjusting the model +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Model3=lm(TOTALSCORE_MBI_STD~TOTALSCORE_PWB_STD,data=DBTM)
summary(Model3)
#*************** Fourth model, TOTALSCORE_PWB_STD=TOTALSCORE_RES_STD+TOTALSCORE_MBI_STD (Multilevel) **************************************************************************************************************************************
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ Adjusting the model +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Model4<-lme(TOTALSCORE_PWB_STD~TOTALSCORE_MBI_STD+TOTALSCORE_RES_STD, random = list(RangoYTE = pdIdent(~ TOTALSCORE_MBI_STD+TOTALSCORE_RES_STD)), data = DBTM)
Model42<-lm(TOTALSCORE_PWB_STD~TOTALSCORE_MBI_STD+TOTALSCORE_RES_STD, data = DBTM)
summary(Model4)
summary(Model42)
#*************** Fifth model, TOTALSCORE_PWB_STD=TOTALSCORE_RES_STD (Multilevel model) **************************************************************************************************************************************
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ Adjusting the model +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Model5=lme(TOTALSCORE_PWB_STD~TOTALSCORE_RES_STD, random = ~TOTALSCORE_RES_STD|RangoYTE, na.action="na.omit", method="ML", data=DBTM)
summary(Model5)
#*************** Sixth model, TOTALSCORE_PWB_STD=TOTALSCORE_RES_STD (Simple model) **************************************************************************************************************************************
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ Adjusting the model +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Model6=lm(TOTALSCORE_PWB_STD~TOTALSCORE_RES_STD,data=DBTM)
summary(Model6)

#**************************** validition of the model ************************** 
#1.Split the Data
split_ratio <- 0.8
data_split <- sample.split(DBT$TOTALSCORE_PWB_STD, SplitRatio = split_ratio)
train_data <- subset(DBT, data_split == TRUE)
test_data <- subset(DBT, data_split == FALSE)

#2.Train the Model
model <- lm(TOTALSCORE_PWB_STD ~TOTALSCORE_MBI_STD+TOTALSCORE_RES_STD, data = train_data)
summary(model)

# Step 3: Make Predictions
predictions <- predict(model, newdata = test_data)

# Step 4: Evaluate Performance
mae <- mean(abs(test_data$TOTALSCORE_PWB_STD - predictions))
mse <- mean((test_data$TOTALSCORE_PWB_STD - predictions)^2)
rmse <- sqrt(mse)

# Step 5: Visualize Residuals
residuals <- test_data$TOTALSCORE_PWB_STD - predictions
plot(test_data$TOTALSCORE_PWB_STD, 
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     , 
     main = "Residual Plot", 
     xlab = "Actual Values", 
     ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)

# Step 6: Cross-Validation (Optional)
library(caret)
cv_results <- train(model, data = dataset, method = "lm", trControl = trainControl(method = "cv", number = 5))

# Display cross-validation results
print(cv_results)

# Step 7: Adjust Model if Necessary
# You can try adjusting hyperparameters or adding polynomial features here.

# Step 8: Check Assumptions
# Use diagnostic plots to check assumptions
par(mfrow = c(2, 2))
plot(model)




#+++++++++++++++++++++++++++++++++++++++++++++++++++Validading and comparing models+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
rsquaredLM <- summary(LinearModel)$r.squared
rsquaredLM
adj_rsquaredLM <- summary(LinearModel)$adj.r.squared
adj_rsquaredLM

rsquared_conditional <- r.squaredGLMM(MultilevelModel)
rsquared_marginal <- r.squaredGLMM(MultilevelModel, type = "marginal")
adj_rsquaredML <- r.squaredGLMM(MultilevelModel, type = "conditional") - 
  (r.squaredGLMM(MultilevelModel, type = "conditional") - rsquared_conditional) * 
  (nobs(MultilevelModel) - 1) / (nobs(MultilevelModel) - ncol(model.matrix(MultilevelModel)))




par(mfrow = c(1,2))
plot(modelo3$residuals, main="Linear Regression Residuals")
plot(resid(modelo2), main="Multilevel Regression Residuals")

AIC(modelo3)
AIC(modelo2)

BIC(modelo3)
BIC(modelo2)


library(caret)
set.seed(123)
lm_cv <- train(response_variable ~ predictor1 + predictor2, data = your_data, method = "lm", trControl = trainControl(method = "cv"))
multilevel_cv <- train(response_variable ~ predictor1 + predictor2, data = your_data, method = "lmer", trControl = trainControl(method = "cv"))

summary(lm_cv)
summary(multilevel_cv)







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


datos1=rnorm(50,2,0.1)
hist(datos1)
datos1=c(datos1,7)
datos1=round(datos1,2)
nuevodatos1=(datos1-mean(datos1))/sd(datos1)
nuevodatos1=round(nuevodatos1,2)
nuevodatos1
est_nuevodatos1=nuevodatos1*100+500
est_nuevodatos1
sort(est_nuevodatos1)
