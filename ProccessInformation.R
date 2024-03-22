#***********************************************Loading data base *******************************************************
DBT=read.csv("DBHE.csv",sep = ",")
DBT=as.data.frame(DBT)
attach(DBT)
#*********************************************************************************************
#Functions to make classifications
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
#------------------------------------------------------------------------------------
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
#------------------------------------------------------------------------------------
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
#------------------------------------------------------------------------------------
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
MBI_EE=MBI_SCALE_EE(SCORE_MBI_S1)
MBI_DP=MBI_SCALE_DP(SCORE_MBI_S2)
MBI_PA=MBI_SCALE_PA(SCORE_MBI_S3)
table(MBI_PA)
#------------------------------------------------------------------------------------
#******************************************************************************************
# Create a data frame
data=data.frame(TEACHING_EXPERIENCE1,MBI_EE,TOTALSCORE_PWB)
# Step 2: Conduct Two-Way ANOVA with Interaction
model=lm(TOTALSCORE_PWB~MBI_EE*TEACHING_EXPERIENCE1,data=data)
anova_result=anova(model)
# Step 3: Interpret Results
print(anova_result)


# Create a data frame
data2=data.frame(TEACHING_EXPERIENCE1,MBI_DP,TOTALSCORE_PWB_STD)
# Step 2: Conduct Two-Way ANOVA with Interaction
model=lm(TOTALSCORE_PWB_STD~MBI_DP*TEACHING_EXPERIENCE1,data=data2)
anova_result=anova(model)
# Step 3: Interpret Results
print(anova_result)

# Create a data frame
data3=data.frame(TEACHING_EXPERIENCE1,MBI_PA,TOTALSCORE_PWB_STD)
# Step 2: Conduct Two-Way ANOVA with Interaction
model=lm(TOTALSCORE_PWB_STD~MBI_PA*TEACHING_EXPERIENCE1,data=data3)
anova_result=anova(model)
# Step 3: Interpret Results
print(anova_result)




# Step 4: Post-hoc Tests (if needed)
# If the interaction is significant, you may want to conduct post-hoc tests.
# For example, using Tukey's test:
library(emmeans)
posthoc_result <- emmeans(model, ~ MBI * TeachingExperience, adjust = "tukey")
print(posthoc_result)

# Step 5: Effect Size
# You can compute eta-squared for effect size
library(effsize)
eta_squared(model)

# Additional: Check Assumptions
# Check assumptions of ANOVA (homogeneity of variances, normality of residuals)
# Use diagnostic plots:
par(mfrow = c(2, 2))
plot(model)

# Additional: Visualize Interaction (if needed)
interaction_plot(data$MBI, data$TeachingExperience, response = data$PWB, type = "means", legend = TRUE)