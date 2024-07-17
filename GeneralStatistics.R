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
#********************************** Descriptive statistics ************************************************************************************************************************************************************************************************************************************************************************************************************************************************************
#---------------------------------- Representative measures ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Getting descriptive statistics
estadistics=data.frame(TOTALSCORE_PWB_STD,TOTALSCORE_RES_STD,TOTALSCORE_MBI_STD)
stat.desc(estadistics)
#---------------------------------- Correlations ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
res2 <- rcorr(cbind(SCORE_PWB_S1_STD,SCORE_PWB_S2_STD, SCORE_PWB_S3_STD, SCORE_PWB_S4_STD,SCORE_PWB_S5_STD,SCORE_PWB_S6_STD, TOTALSCORE_PWB_STD,+
                      SCORE_RES_S1_STD,SCORE_RES_S2_STD,SCORE_RES_S3_STD,SCORE_RES_S4_STD,SCORE_RES_S5_STD,TOTALSCORE_RES_STD, SCORE_MBI_S1_STD,SCORE_MBI_S2_STD,SCORE_MBI_S3_STD,+   
                      TOTALSCORE_MBI_STD ), type = c("pearson","spearman"))
res2 
res3 <- rcorr(cbind( TOTALSCORE_PWB_STD, TOTALSCORE_RES_STD, TOTALSCORE_MBI_STD,SCORE_MBI_S1_STD,SCORE_MBI_S2_STD,SCORE_MBI_S3_STD,TEACHING_EXPERIENCE,AGE), type = c("pearson","spearman"))
res3
#--------------------------------- Cronbach's alpha ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
RESBASE=data.frame(RES1,RES2,RES3,RES4,RES5,RES6,RES7,RES8,RES9,RES10,RES11,RES12,RES13,+             
                     RES14,RES15,RES16,RES17,RES18,RES19,RES20,RES21,RES22,RES23,RES24,RES25)
cronbach.alpha(RESBASE, CI=TRUE, standardized=TRUE)

MBIBASE=data.frame(MBI1,MBI2,MBI3,MBI4,MBI5,MBI6,MBI7,MBI8,+
                     MBI9,MBI10,MBI11,MBI12,MBI13,MBI14,MBI15,MBI16,MBI17,MBI18,MBI19,MBI20,MBI21,MBI22)
cronbach.alpha(MBIBASE, CI=TRUE, standardized=TRUE)

MBI_EE=data.frame(MBI1,MBI2,MBI3,MBI6,MBI8,+
                    MBI13,MBI14,MBI16,MBI20)
cronbach.alpha(MBI_EE, CI=TRUE, standardized=TRUE)

MBI_DP=data.frame(MBI5,MBI10,MBI11,MBI15,+
                    MBI22)
cronbach.alpha(MBI_DP, CI=TRUE, standardized=TRUE)

MBI_PA=data.frame(MBI4,MBI7,MBI9,MBI12,MBI17,MBI18,MBI19,MBI21)
cronbach.alpha(MBI_PA, CI=TRUE, standardized=TRUE)

PWBBASE=data.frame(PWB1,PWB2,PWB3,PWB4,PWB5,PWB6,PWB7,PWB8,PWB9,PWB10,PWB11,+              
                     PWB12,PWB13,PWB14,PWB15,PWB16,PWB17,PWB18,PWB19,PWB20,PWB21,PWB22,PWB23,PWB24,PWB25,PWB26,PWB27,+              
                     PWB28,PWB29,PWB30,PWB31,PWB32,PWB33,PWB34,PWB35,PWB36,PWB37,PWB38,PWB39,PWB40,+
                     PWB41,PWB42,PWB43,PWB44,PWB45,PWB46,PWB47,PWB48,PWB49,PWB50,PWB51,PWB52,PWB53,PWB54,+
                     PWB55,PWB56,PWB57,PWB58,PWB59,PWB60,PWB61,PWB62,PWB63,PWB64,PWB65,PWB66,PWB67,+    
                     PWB68,PWB69,PWB70,PWB71,PWB72,PWB73,PWB74,PWB75,PWB76,PWB77,PWB78,PWB79,PWB80,PWB81,+
                     PWB82,PWB83,PWB84)
cronbach.alpha(PWBBASE, CI=TRUE, standardized=TRUE)
#********************************** Graphs of the distributions **********************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************
#Making the graphs
theme_set(theme_ridges())
theme_set(theme_bw())
chart1 <- data.frame(Scores = c(TOTALSCORE_PWB_STD,TOTALSCORE_RES_STD,TOTALSCORE_MBI_STD),Frequency= c(rep("PWB", length(TOTALSCORE_PWB_STD)),rep("RES", length(TOTALSCORE_RES_STD)),rep("MBI", length(TOTALSCORE_MBI_STD))))
ggplot(chart1, aes(x = Scores, y = Frequency)) + geom_density_ridges(quantile_lines = TRUE, quantiles = 2,fill ="#008AA6",alpha = .5,)+coord_cartesian(clip = "off") + labs(title = "Distribution of teacher's scores")+theme_ridges(font_size = 30,grid =TRUE, line_size = 0.75, center_axis_labels = TRUE)+scale_x_continuous(breaks = c(0:10), limits = c(-.5, 13),expand = c(0, 0), name = "Scores")
#******************************* Radar chart **********************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************
#------------------------------ PWB Questionnaire ------------------------------------------------------------------------------------------*******************************************************************************************************************************************************************************************************************************************
pwbs1=median(SCORE_PWB_S1_STD)
pwbs2=median(SCORE_PWB_S2_STD)
pwbs3=median(SCORE_PWB_S3_STD)
pwbs4=median(SCORE_PWB_S4_STD)
pwbs5=median(SCORE_PWB_S5_STD)
pwbs6=median(SCORE_PWB_S6_STD)

subscales1=data.frame(pwbs1,pwbs2,pwbs3,pwbs4,pwbs5,pwbs6)
colnames(subscales1) <- c("Positive Relations with Others","Autonomy","Environmental Mastery","Personal Growth","Purpose in Life","Self-Acceptance")
subscales1=rbind(rep(10,6),rep(0,6),subscales1)
subscales1=as.data.frame(subscales1)

radarchart(subscales1, axistype=1 , 
           #custom polygon
           pcol="#00758C",pfcol = scales::alpha("#008AA6", 0.5),plwd=4 , plty=1,
           #custom the grid
           cglcol="black", cglty=4, axislabcol="black", caxislabels=seq(0,10,2.5), cglwd=0.8,
           #custom labels
           vlcex=1)
#------------------------------ RES Questionnaire ------------------------------------------------------------------------------------------------------------------------***********************************************************************************************************************************************************************************************************************
ress1=median(SCORE_RES_S1_STD)
ress2=median(SCORE_RES_S2_STD)
ress3=median(SCORE_RES_S3_STD)
ress4=median(SCORE_RES_S4_STD)
ress5=median(SCORE_RES_S5_STD)

subscales2=data.frame(ress1,ress2,ress3,ress4,ress5)
colnames(subscales2) <- c("Meaningfullness","Equanimity","Existential Aloneness","Self-reliance ","Perseverance")
subscales2=rbind(rep(10,5),rep(0,5),subscales2)
subscales2=as.data.frame(subscales2)

radarchart(subscales2, axistype=1 , 
           #custom polygon
           pcol="#00758C",pfcol = scales::alpha("#008AA6", 0.5),plwd=4 , plty=1,
           #custom the grid
           cglcol="black", cglty=4, axislabcol="black", caxislabels=seq(0,10,2.5), cglwd=0.8,
           #custom labels
           vlcex=1)
#------------------------------ MBI Questionnaire ------------------------------------------------------------------------------------------------------------------------*******************************************************************************************************************************************************************************************************************************************
mbis1=median(SCORE_MBI_S1_STD)
mbis2=median(SCORE_MBI_S2_STD)
mbis3=median(SCORE_MBI_S3_STD)

subscales3=data.frame(mbis1,mbis2,mbis3)
colnames(subscales3) <- c("Emotional Exhaustion","Depersonalization","Personal Accomplishment")
subscales3=rbind(rep(10,3),rep(0,3),subscales3)
subscales3=as.data.frame(subscales3)

radarchart(subscales3, axistype=1 , 
           #custom polygon
           pcol="#00758C",pfcol = scales::alpha("#008AA6", 0.5),plwd=4 , plty=1,
           #custom the grid
           cglcol="black", cglty=4, axislabcol="black", caxislabels=seq(0,10,2.5), cglwd=0.8,
           #custom labels
           vlcex=1)
#******************************************** Scatter plot ******************************************************************************************************************************************************************************************************************************************************************************************************************************************************************
#--------------------------------------- PWB VS RES---------------------------------------------------------------------------------------------------------------------------------------------------
#Getting the parameters of the regression model for the scatter plot
model <- lm( TOTALSCORE_PWB_STD~ TOTALSCORE_RES_STD, data = data1)
model
#Making the graph
d=data.frame(TOTALSCORE_RES_STD,TOTALSCORE_PWB_STD)
d$pc <- predict(prcomp(~TOTALSCORE_RES_STD+TOTALSCORE_PWB_STD,d))[,1]
ggplot(d, aes(TOTALSCORE_RES_STD,TOTALSCORE_PWB_STD, color = pc)) +
  geom_point(shape = 16,size = 5, show.legend = FALSE) + ggtitle("Wagnild & Young’s Resilience Scale VS PWRyff’s PWB Inventory ") +
  xlab("RES SCORES") + ylab("PWB SCORES")+geom_abline(intercept = 4.11 , slope =  0.47 , color="red", linetype="dashed", size=1.5)+
  theme_minimal() +scale_color_gradient(low = "#008AA6", high = "#00758C")+theme(plot.title = element_text(hjust = 0.5))
#---------------------------------------- MBI VS RES -------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Getting the parameters of the regression model for the scatter plot
model <- lm( TOTALSCORE_RES_STD~ TOTALSCORE_MBI_STD, data = data1)
model
#Making the graph
d=data.frame(TOTALSCORE_MBI_STD,TOTALSCORE_RES_STD)
d$pc <- predict(prcomp(~TOTALSCORE_MBI_STD+TOTALSCORE_RES_STD, d))[,1]
ggplot(d, aes(TOTALSCORE_MBI_STD,TOTALSCORE_RES_STD, color = pc)) +
  geom_point(shape = 16,size = 5, show.legend = FALSE) + ggtitle("Maslach’s Burnout Inventory  VS Wagnild & Young’s Resilience Scale") +
  xlab("MBI SCORES") + ylab("RES SCORES")+geom_abline(intercept = 6.0179 , slope =0.1683 , color="red", linetype="dashed", size=1.5)+
  theme_minimal() +scale_color_gradient(low = "#008AA6", high = "#00758C")+theme(plot.title = element_text(hjust = 0.5))
#---------------------------------------- MBI VS PWB ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- 
#Getting the parameters of the regression model for the scatter plot
model <- lm( TOTALSCORE_PWB_STD~ TOTALSCORE_MBI_STD, data = data1)
model
#Making the graph
d=data.frame(TOTALSCORE_MBI_STD,TOTALSCORE_PWB_STD)
d$pc <- predict(prcomp(~TOTALSCORE_MBI_STD+TOTALSCORE_PWB_STD, d))[,1]
ggplot(d, aes(TOTALSCORE_MBI_STD,TOTALSCORE_PWB_STD, color = pc)) +
  geom_point(shape = 16,size = 5, show.legend = FALSE) + ggtitle("Maslach’s Burnout Inventory  VS Ryff’s PWB Inventory") +
  xlab("MBI SCORES") + ylab("PWB SCORES")+geom_abline(intercept = 7.05 , slope =  0.06 , color="red", linetype="dashed", size=1.5)+
  theme_minimal() +scale_color_gradient(low = "#008AA6", high = "#00758C")+theme(plot.title = element_text(hjust = 0.5))
#****************************************** Teacher experience histogram *************************************************************************************************************************************************************
ggplot(data = data1, aes(x = TEACHING_EXPERIENCE)) +
  geom_histogram(colour="#1F3552", fill="#008AA6",binwidth=5,boundary=0)+scale_x_continuous(breaks = seq(0,45,5),name="Number of years")+
  ggtitle("Years of teaching experience",)+scale_y_continuous(name="Number of teachers", breaks = seq(0,60, by =10))+
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))+geom_text(aes(label =..count..), vjust = -0.2,stat="bin", size=4,vjust=-1,breaks = seq(0, 45, 5))
#******************************************* Age of teachers *********************************************************************************************************************************************************************************************
ggplot(data = data1, aes(x =AGE)) +
  geom_histogram(colour="#1F3552", fill="#008AA6",binwidth =5,boundary=20)+scale_x_continuous(breaks = seq(20,70,5),name="Age")+
  ggtitle("Age of the teachers",)+scale_y_continuous(name="Number of teachers", breaks = seq(0,30, by =10))+
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))+geom_text(aes(label =..count..), vjust = -0.2,stat="bin", size=4,vjust=-1,breaks = seq(20, 70, 5))
#---------------------------------------- External Exhaustation VS PWB ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- 
#Getting the parameters of the regression model for the scatter plot
model <- lm( TOTALSCORE_PWB_STD~ SCORE_MBI_S1_STD, data = data1)
model
#Making the graph
d=data.frame(SCORE_MBI_S1_STD,TOTALSCORE_PWB_STD)
d$pc <- predict(prcomp(~SCORE_MBI_S1_STD+TOTALSCORE_PWB_STD, d))[,1]
ggplot(d, aes(SCORE_MBI_S1_STD,TOTALSCORE_PWB_STD, color = pc)) +
  geom_point(shape = 16,size = 5, show.legend = FALSE) + ggtitle("External Exhaustation  VS Ryff’s PWB Inventory") +
  xlab("External Exhaustation") + ylab("PWB SCORES")+geom_abline(intercept = 8.18 , slope =  -0.1661 , color="red", linetype="dashed", size=1.5)+
  theme_minimal() +scale_color_gradient(low = "#008AA6", high = "#00758C")+theme(plot.title = element_text(hjust = 0.5))

#---------------------------------------- Depersonalization VS PWB ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- 
#Getting the parameters of the regression model for the scatter plot
model <- lm( TOTALSCORE_PWB_STD~ SCORE_MBI_S2_STD, data = data1)
model
#Making the graph
d=data.frame(SCORE_MBI_S2_STD,TOTALSCORE_PWB_STD)
d$pc <- predict(prcomp(~SCORE_MBI_S2_STD+TOTALSCORE_PWB_STD, d))[,1]
ggplot(d, aes(SCORE_MBI_S2_STD,TOTALSCORE_PWB_STD, color = pc)) +
  geom_point(shape = 16,size = 5, show.legend = FALSE) + ggtitle("Depersonalization  VS Ryff’s PWB Inventory") +
  xlab("Depersonalization") + ylab("PWB SCORES")+geom_abline(intercept = 8.0067 , slope =  -0.1698 , color="red", linetype="dashed", size=1.5)+
  theme_minimal() +scale_color_gradient(low = "#008AA6", high = "#00758C")+theme(plot.title = element_text(hjust = 0.5))
#---------------------------------------- Personal Accomplishment VS PWB ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- 
#Getting the parameters of the regression model for the scatter plot
model <- lm( TOTALSCORE_PWB_STD~ SCORE_MBI_S3_STD_2, data = data1)
model
#Making the graph
d=data.frame(SCORE_MBI_S3_STD_2,TOTALSCORE_PWB_STD)
d$pc <- predict(prcomp(~SCORE_MBI_S3_STD_2+TOTALSCORE_PWB_STD, d))[,1]
ggplot(d, aes(SCORE_MBI_S3_STD_2,TOTALSCORE_PWB_STD, color = pc)) +
  geom_point(shape = 16,size = 5, show.legend = FALSE) + ggtitle("Personal Accomplishment VS Ryff’s PWB Inventory") +
  xlab("Personal Accomplishment") + ylab("PWB SCORES")+geom_abline(intercept = 4.2621 , slope =  0.4038 , color="red", linetype="dashed", size=1.5)+
  theme_minimal() +scale_color_gradient(low = "#008AA6", high = "#00758C")+theme(plot.title = element_text(hjust = 0.5))

#---------------------------------------- External Exhaustation VS RES ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- 
#Getting the parameters of the regression model for the scatter plot
model <- lm( TOTALSCORE_RES_STD~ SCORE_MBI_S1_STD, data = data1)
model
#Making the graph
d=data.frame(SCORE_MBI_S1_STD,TOTALSCORE_RES_STD)
d$pc <- predict(prcomp(~SCORE_MBI_S1_STD+TOTALSCORE_RES_STD, d))[,1]
ggplot(d, aes(SCORE_MBI_S1_STD,TOTALSCORE_RES_STD, color = pc)) +
  geom_point(shape = 16,size = 5, show.legend = FALSE) + ggtitle("External Exhaustation  VS Wagnild & Young’s Resilience Scale 
") +
  xlab("External Exhaustation") + ylab("RES SCORES")+geom_abline(intercept = 7.4025 , slope =    -0.0926  
                                                                 , color="red", linetype="dashed", size=1.5)+
  theme_minimal() +scale_color_gradient(low = "#008AA6", high = "#00758C")+theme(plot.title = element_text(hjust = 0.5))

#---------------------------------------- Depersonalization VS RES ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- 
#Getting the parameters of the regression model for the scatter plot
model <- lm( TOTALSCORE_RES_STD~ SCORE_MBI_S2_STD, data = data1)
model
#Making the graph
d=data.frame(SCORE_MBI_S2_STD,TOTALSCORE_RES_STD)
d$pc <- predict(prcomp(~SCORE_MBI_S2_STD+TOTALSCORE_RES_STD, d))[,1]
ggplot(d, aes(SCORE_MBI_S2_STD,TOTALSCORE_RES_STD, color = pc)) +
  geom_point(shape = 16,size = 5, show.legend = FALSE) + ggtitle("Despersonalization  VS Wagnild & Young’s Resilience Scale 
") +
  xlab("Despersonalization") + ylab("RES SCORES")+geom_abline(intercept = 7.24521 , slope =    -0.07803  
                                                              , color="red", linetype="dashed", size=1.5)+
  theme_minimal() +scale_color_gradient(low = "#008AA6", high = "#00758C")+theme(plot.title = element_text(hjust = 0.5))

#---------------------------------------- Personal Accomplishment VS RES ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- 
#Getting the parameters of the regression model for the scatter plot
model <- lm( TOTALSCORE_RES_STD~ SCORE_MBI_S3_STD_2, data = data1)
model
#Making the graph
d=data.frame(SCORE_MBI_S3_STD_2,TOTALSCORE_RES_STD)
d$pc <- predict(prcomp(~SCORE_MBI_S3_STD_2+TOTALSCORE_RES_STD, d))[,1]
ggplot(d, aes(SCORE_MBI_S3_STD_2,TOTALSCORE_RES_STD, color = pc)) +
  geom_point(shape = 16,size = 5, show.legend = FALSE) + ggtitle("Personal Accomplishment  VS Wagnild & Young’s Resilience Scale 
") +
  xlab("Personal Accomplishment") + ylab("RES SCORES")+geom_abline(intercept =  3.7784  , slope =   0.4111  
                                                                   , color="red", linetype="dashed", size=1.5)+
  theme_minimal() +scale_color_gradient(low = "#008AA6", high = "#00758C")+theme(plot.title = element_text(hjust = 0.5))








