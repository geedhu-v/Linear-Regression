##################################################
### PROG8430                                    ##
### Simple Linear Regression - Demo             ## 
##################################################
#                                               ##
##################################################
# Written by Peiyuan
# ID: 123456
#
##################################################
### Basic Set Up                                ##
##################################################
# Clear plots
if(!is.null(dev.list())) dev.off()
# Clear console
cat("\014") 
# Clean workspace
rm(list=ls())
options(scipen=9)

##################################################
### Install Libraries                           ##
##################################################
#If the library is not already downloaded, download it
if(!require(lattice)){install.packages("lattice")}
library("lattice")
if(!require(pastecs)){install.packages("pastecs")}
library("pastecs")
if(!require(cowplot)){install.packages("cowplot")}
library("cowplot")

##################################################
### Read in Data                                ##
##################################################
#Read Data
Systolic <- read.csv("C:/Users/Geedhu/Documents/Systolic.csv")
Thunder <- read.table("C:/Users/Geedhu/Documents/ThunderBasin.csv", header = TRUE, sep = ",")


##################################################
### Rename and Clean Variables                  ##
##################################################

#Rename Variables to something meaningful
str(Systolic)
names(Systolic) <- c("BP", "Age", "Wgt") 
str(Systolic)

str(Thunder)
names(Thunder) <- c("Fwn", "Adt", "Prc", "Sev") 
str(Thunder)

##################################################
### Description of Data                         ##
##################################################
#Systolic
summary(Systolic)
SysSum <-stat.desc(Systolic)
format(SysSum,digits=2)

den1=densityplot( ~BP, data=Systolic, main="Distribution of BP")
den2=densityplot( ~Age, data=Systolic, main="Distribution of Age")
den3=densityplot( ~Wgt, data=Systolic, main="Distribution of Weight")
plot_grid(den1, den2, den3, ncol=2, nrow=2)

#Thunder Basin
summary(Thunder)
TdrSum <-stat.desc(Thunder)
format(TdrSum,digits=2)

den1=densityplot( ~Fwn, dat=Thunder, main="Dist of Fawns")
den2=densityplot( ~Adt, dat=Thunder,  main="Dist of Adults")
den3=densityplot( ~Prc, dat=Thunder, main="Dist of precipitation")
den4=densityplot( ~Sev, dat=Thunder, main="Dist of Winter Sev.")
plot_grid(den1, den2, den3, den4, ncol=2, nrow=2)

#Scatter plots of Systolic and Thunder (and correlation)
plot(BP ~ Age, data=Systolic,main="Comparing BP to Age")
cor(Systolic$BP, Systolic$Age, method="spearman") # By default is pearson
cor.test(Systolic$BP, Systolic$Age) 


plot(Fwn ~ Adt, data=Thunder,main="Comparing Spring Births")
cor(Thunder$Fwn, Thunder$Adt, method="spearman")
cor.test(Thunder$Fwn, Thunder$Adt)
##################################################
### Create a Model                              ##
##################################################
#Create a Simple Linear Model for each dataset and print specifications
Sysmodel <- lm(BP ~ Age, data=Systolic) ### To include many features use + operator along with other independent variables
Sysmodel
plot(BP ~ Age, data=Systolic,
     main="BP by Age (with Regression Line)")
abline(Sysmodel) #add the regression line to the plot

Sysmodel <- lm(BP ~ Age, data=Systolic) ### To include many features use + operator along with other independent variables
Sysmodel
plot(BP ~ Age, data=Systolic,
     main="BP by Age (with Regression Line)")
abline(Sysmodel)


#Now the same for Thunder Basin data
Tdrmodel <- lm(Fwn ~ Adt, data=Thunder)
Tdrmodel
plot(Fwn ~ Adt, data=Thunder,
     main="Spring Fawns by Adult Population (with Regression Line)")
abline(Tdrmodel)

#Check details of the models 
summary(Sysmodel)
summary(Tdrmodel)

#############################
############RMSE#############
#############################
### Read in test data to compare to training 
Sys_tst <- read.table("C:/Users/Geedhu/Documents/Systolic_tst.csv", header = TRUE, sep = ",")
names(Sys_tst) <- c("BP", "Age", "Wgt")

### Comparing the RMSE
pred <- predict(Sysmodel, newdata=Systolic)
RMSE_trn <- sqrt(mean((Systolic$BP - pred)^2))
round(RMSE_trn,3)

pred <- predict(Sysmodel, newdata=Sys_tst)
RMSE_tst <- sqrt(mean((Sys_tst$BP - pred)^2))
round(RMSE_tst,3)

##############################
###How Good are these Predictions?#########
#############################
summary(Sysmodel)
###Output###
#Median value should be closer to zero, which means the model is good.
#Coefficients : B0 - 58.7055 B1-1.462,Pr: Pvalue for t test
# pvalue<0.05 means it is against null hypothesis.

#R squared - how good the model fits the data ->equal to zero not good otherwise if it is closer 1 it good.
#multiple and adjusted rsquared values will  be more or less same value.
summary(Tdrmodel)








