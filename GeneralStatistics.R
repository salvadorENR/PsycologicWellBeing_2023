#***************************** Loading the requiered packages *************************************************************************************************************************************************************************************************************************************
library(readr)
library(stringr)
library(utf8)
library(devtools)
library(nlme)                   
library(lme4)                 
library(stargazer)               
library(texreg)  
library(dplyr)
library("Hmisc")
library(plyr)
library(datos)
library(ggplot2)
library(reshape2)
library(ggsci)
library(scales)
library(ggpubr)
library(lmerTest)              
library(ggeffects)               
library(stargazer)               
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
#*************************************** Reaing the data base *******************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************
data1=read.csv("DBHE.csv",sep=",")
data1=as.data.frame(data1)
attach(data1)
colnames(data1)
#********************************** Graphs of the distributions **********************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************
#Making the graphs
theme_set(theme_ridges())
theme_set(theme_bw())
chart1 <- data.frame(Scores = c(TOTALSCORE_PWB_STD,TOTALSCORE_RES_STD,TOTALSCORE_MBI_STD),Frequency= c(rep("PWD", length(TOTALSCORE_PWB_STD)),rep("RES", length(TOTALSCORE_RES_STD)),rep("MBI", length(TOTALSCORE_MBI_STD))))
ggplot(chart1, aes(x = Scores, y = Frequency)) + geom_density_ridges(quantile_lines = TRUE, quantiles = 2,fill ="#2C64DD",alpha = .5,)+coord_cartesian(clip = "off") + labs(title = "Distribution of teacher's scores")+theme_ridges(font_size = 30,grid =TRUE, line_size = 0.75, center_axis_labels = TRUE)+scale_x_continuous(breaks = c(0:10), limits = c(-.5, 13),expand = c(0, 0), name = "Scores")
#Getting descriptive statistics
estadistics=data.frame(TOTALSCORE_PWB_STD,TOTALSCORE_RES_STD,TOTALSCORE_MBI_STD)
stat.desc(estadistics)
ggplot(chart1, aes(x = Scores, y = Frequency)) + geom_density_ridges(quantile_lines = TRUE, quantiles = 2,alpha = .5,)+coord_cartesian(clip = "off") + labs(title = 'Distribution of students scores')+theme_ridges(font_size = 30,grid =TRUE, line_size = 0.75, center_axis_labels = TRUE)+scale_x_continuous(breaks = c(0:10), limits = c(-.5, 13),expand = c(0, 0), name = "Scores")
#******************************* Radar chart **********************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************
#******************************* PWB Questionnaire *******************************************************************************************************************************************************************************************************************************************
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
pcol="#2C64DD",pfcol = scales::alpha("#2C64DD", 0.5),plwd=4 , plty=1,
#custom the grid
cglcol="black", cglty=4, axislabcol="black", caxislabels=seq(0,10,2.5), cglwd=0.8,
#custom labels
vlcex=1)
#******************************* RES Questionnaire *******************************************************************************************************************************************************************************************************************************************
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
pcol="#2C64DD",pfcol = scales::alpha("#2C64DD", 0.5),plwd=4 , plty=1,
#custom the grid
cglcol="black", cglty=4, axislabcol="black", caxislabels=seq(0,10,2.5), cglwd=0.8,
#custom labels
vlcex=1)
#******************************* MBI Questionnaire *******************************************************************************************************************************************************************************************************************************************
mbis1=median(SCORE_MBI_S1_STD)
mbis2=median(SCORE_MBI_S2_STD)
mbis3=median(SCORE_MBI_S3_STD)

subscales3=data.frame(mbis1,mbis2,mbis3)
colnames(subscales) <- c("Emotional Exhaustion","Depersonalization","Reduced Personal Accomplishment")
subscales3=rbind(rep(10,3),rep(0,3),subscales3)
subscales3=as.data.frame(subscales3)

radarchart(subscales3, axistype=1 , 
            #custom polygon
            pcol="#2C64DD",pfcol = scales::alpha("#2C64DD", 0.5),plwd=4 , plty=1,
            #custom the grid
            cglcol="black", cglty=4, axislabcol="black", caxislabels=seq(0,10,2.5), cglwd=0.8,
            #custom labels
            vlcex=1
)
#******************************************** Scatter plot ******************************************************************************************************************************************************************************************************************************************************************************************************************************************************************
#--------------------------------------- PWB VS RES---------------------------------------------------------------------------------------------------------------------------------------------------
#Getting the parameters of the regression model for the scatter plot
model <- lm( TOTALSCORE_PWB_STD~ TOTALSCORE_RES_STD, data = data1)
model
#Making the graph
d=data.frame(TOTALSCORE_RES_STD,TOTALSCORE_PWB_STD)
d$pc <- predict(prcomp(~TOTALSCORE_RES_STD+TOTALSCORE_PWB_STD,d))[,1]
ggplot(d, aes(TOTALSCORE_RES_STD,TOTALSCORE_PWB_STD, color = pc)) +
  geom_point(shape = 16,size = 5, show.legend = FALSE) + ggtitle("RES VS PWB") +
  xlab("RES_STD SCORES") + ylab("PWB_STD SCORES")+geom_abline(intercept = 4.9131 , slope =  0.5369 , color="red", linetype="dashed", size=1.5)+
  theme_minimal() +scale_color_gradient(low = "#D7E3FB", high = "#092C71")
#---------------------------------------- MBI VS RES -------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Getting the parameters of the regression model for the scatter plot
model <- lm( TOTALSCORE_RES_STD~ TOTALSCORE_MBI_STD, data = data1)
model
#Making the graph
d=data.frame(TOTALSCORE_MBI_STD,TOTALSCORE_RES_STD)
d$pc <- predict(prcomp(~TOTALSCORE_MBI_STD+TOTALSCORE_RES_STD, d))[,1]
ggplot(d, aes(TOTALSCORE_MBI_STD,TOTALSCORE_RES_STD, color = pc)) +
  geom_point(shape = 16,size = 5, show.legend = FALSE) + ggtitle("MBI VS RES") +
  xlab("MBI_STD SCORES") + ylab("RES_STD SCORES")+geom_abline(intercept = 4.9131 , slope =  0.5369 , color="red", linetype="dashed", size=1.5)+
  theme_minimal() +scale_color_gradient(low = "#D7E3FB", high = "#092C71")
#---------------------------------------- MBI VS PWB ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- 
#Getting the parameters of the regression model for the scatter plot
model <- lm( TOTALSCORE_PWB_STD~ TOTALSCORE_MBI_STD, data = data1)
model
#Making the graph
d=data.frame(TOTALSCORE_MBI_STD,TOTALSCORE_PWB_STD)
d$pc <- predict(prcomp(~TOTALSCORE_MBI_STD+TOTALSCORE_PWB_STD, d))[,1]
ggplot(d, aes(TOTALSCORE_MBI_STD,TOTALSCORE_PWB_STD, color = pc)) +
  geom_point(shape = 16,size = 5, show.legend = FALSE) + ggtitle("MBI VS PWB") +
  xlab("MBI_STD SCORES") + ylab("PWB_STD SCORES")+geom_abline(intercept = 4.9131 , slope =  0.5369 , color="red", linetype="dashed", size=1.5)+
  theme_minimal() +scale_color_gradient(low = "#D7E3FB", high = "#092C71")






