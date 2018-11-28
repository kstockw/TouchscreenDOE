setwd ("/Users/kaydenstockwell/Desktop/Prelim Analysis/Touchscreen/TouchscreenDOE")
library(reshape2)
library(gtools) 
library(lme4)
library(plyr)
library(lattice)
library(car)
library(HH)
library(agricolae)
library(multcomp) 
library(MASS)
library(heplots) 
library (coin) 
library(ggplot2)
library(zoo)
library(Hmisc)
library(lmerTest)
##library(LMERConvenienceFunctions)
library(languageR)
library(doBy)
rm(list=ls())
ls()  ##check

##############################################
###########Code DOE EPrime Analysis###########
########By Sarah Olsen edited by Kayden#######
############Stockwell for DOE 2018############
##############################################
##############################################
getwd()

##Coding note: Group: 1 = DOP, 2 = NOP; Block = Trial Number

##Read in the csv file(s) of Eprime data, skip first row to ensure correct column names
TDOE_11_26_18ORIGINAL <- read.csv(file = "TDOE_11_26_18.csv", header = TRUE, skip = 1)