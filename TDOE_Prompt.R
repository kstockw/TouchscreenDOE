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

##Remove participants 18999025 and 18999040 (side perseveration and survey indicated no effort, respectively)
TDOE_11_26_18minus <- TDOE_11_26_18ORIGINAL[-c(1801:1850), ]
TDOE_11_26_18 <- TDOE_11_26_18minus[-c(1951:2000), ]

##Make Dataframe of only participants who were run in the Prompt condition
TDOE_Prompt <- TDOE_11_26_18[TDOE_11_26_18$Prompt == 1, ]

##Displays subject numbers in numerical order and how many times they occur (trial #)
count(TDOE_Prompt$Subject)

###Print the column names
colnames(TDOE_Prompt)

#############################################
#############################################
########## Means and SDs ####################
#############################################
#############################################

##Calculate and print mean accuracy by subject per group (1 = DOP, 2 = NOP) and accuracy standard error per group
ACC_Mean_BySubject <- ddply(TDOE_Prompt, c("Group", "Subject"), summarise, Mean.ACC=mean(ComparisonStimuli.ACC), SE.ACC=sd(ComparisonStimuli.ACC)/sqrt(length(ComparisonStimuli.ACC)))
ACC_Mean_BySubject

##Calculate and print mean accuracy per group (1 = DOP, 2 = NOP) and accuracy standard error per group
ACC_Mean_ByGroup <- ddply(TDOE_Prompt, c("Group"), summarise, Mean.ACC=mean(ComparisonStimuli.ACC), SE.ACC=sd(ComparisonStimuli.ACC)/sqrt(length(ComparisonStimuli.ACC)))
ACC_Mean_ByGroup

#############################################
#############################################
################Response Types###############
#############################################
#############################################

##Number of left, right, Image1 (touched non target area of screen), and TimedOut (i.e., no responses) across subject; Group: 1 = DOP, 2 = NOP
ResponseTypeSubLevel <- ddply(TDOE_Prompt, c("Subject", "Group", "ComparisonStimuli.RESP"), summarise, Resp.Count=length(ComparisonStimuli.ACC))
ResponseTypeSubLevel

##Number of left, right, Image1 (touched non target area of screen), and TimedOut (i.e., no responses) across group; Group: 1 = DOP, 2 = NOP
ResponseTypeNumGroupLevel <- ddply(TDOE_Prompt, c("Group", "ComparisonStimuli.RESP"), summarise, Resp.Count=length(ComparisonStimuli.ACC))
ResponseTypeNumGroupLevel

##Divide data into each first and second half block (trial number) 
First25_Trial_Data <- TDOE_Prompt[TDOE_Prompt$Block <= 25, ]
head(First25_Trial_Data$Block, 50)
Last25_Trial_Data <- TDOE_Prompt[TDOE_Prompt$Block > 25, ]
head(Last25_Trial_Data$Block, 50)

###First half of data; number of left, right, Image1 (touch non target area of screen) and TimedOut (i.e., no responses) across subject; Group: 1 = DOP, 2 = NOP
First25_ResponseTypeNum <- ddply(First25_Trial_Data, c("Subject", "Group", "ComparisonStimuli.RESP"), summarise, Resp.Count=length(ComparisonStimuli.ACC))
First25_ResponseTypeNum

###Second half of data; number of left, right, Image1 (touch non target area of screen) and TimedOut (i.e., no responses) across subject; Group: 1 = DOP, 2 = NOP
Last25_ResponseTypeNum <- ddply(Last25_Trial_Data, c("Subject", "Group", "ComparisonStimuli.RESP"), summarise, Resp.Count=length(ComparisonStimuli.ACC))
Last25_ResponseTypeNum

##Exclude trials where response was "TimedOut"
TDOE_Prompt_NoTimedOut <- TDOE_Prompt[TDOE_Prompt$ComparisonStimuli.RESP != "TimedOut", ]

##Number of left, right, Image1 (touch non target area of screen) across subject; Group: 1 = DOP, 2 = NOP
ResponseTypeSubLevel_NoTimedOut <- ddply(TDOE_Prompt_NoTimedOut, c("Subject", "Group", "ComparisonStimuli.RESP"), summarise, Resp.Count=length(ComparisonStimuli.ACC))
ResponseTypeSubLevel_NoTimedOut

##Number of left, right, Image1 (touch non target area of screen) by Group; Group: 1 = DOP, 2 = NOP
ResponseTypeNumGroupLevel_NoTimedOut <- ddply(TDOE_Prompt_NoTimedOut, c("Group", "ComparisonStimuli.RESP"), summarise, Resp.Count=length(ComparisonStimuli.ACC))
ResponseTypeNumGroupLevel_NoTimedOut

#############################################
##############Response Congruency############
###################Check#####################
#############################################

##Compares each subject's response (all trials) to the correct answer and how many times it occured (e.g. how many times was the correct response
##left and they looked left, correct right and they looked left, etc; Image 1 included)
ResponseTypeNum_Congruent <- ddply(TDOE_Prompt, c("Subject", "ComparisonStimuli.RESP", "CorrectAnswer"), summarise, Resp.Count=length(CorrectAnswer))
ResponseTypeNum_Congruent

##Compares each subject's response (no TimedOut trials) to the correct answer and how many times it occured (e.g. how many times was the correct response
##left and they looked left, correct right and they looked left, etc; Image1 included)
ResponseTypeNum_Congruent_NoTimedOut <- ddply(TDOE_Prompt_NoTimedOut, c("Subject", "ComparisonStimuli.RESP", "CorrectAnswer"), summarise, Resp.Count=length(CorrectAnswer))
ResponseTypeNum_Congruent_NoTimedOut

#############################################
#############################################
########### Means w/o TimedOut ##############
#############################################
#############################################

##Mean accuracy for each subject without TimedOut trials, noting the subject's group (1 = DOP, 2 = NOP) 
ACC_Mean_BySubject_NoTimedOut <- ddply(TDOE_Prompt_NoTimedOut, c("Group", "Subject"), summarise, Mean.ACC=mean(ComparisonStimuli.ACC))
ACC_Mean_BySubject_NoTimedOut

##Mean accuracy for each DOP subject without TimedOut trials
ACC_Mean_DOP_NoTimedOut <- ACC_Mean_BySubject_NoTimedOut[ACC_Mean_BySubject_NoTimedOut$Group == "1", ]
colnames(ACC_Mean_DOP_NoTimedOut) <- c("Group", "Subject", "DOP_ACC")
ACC_Mean_DOP_NoTimedOut

##Mean accuracy for each NOP subject without TimedOut trials
ACC_Mean_NOP_NoTimedOut <- ACC_Mean_BySubject_NoTimedOut[ACC_Mean_BySubject_NoTimedOut$Group == "2", ]
colnames(ACC_Mean_NOP_NoTimedOut) <- c("Group", "Subject", "NOP_ACC")
ACC_Mean_NOP_NoTimedOut

##Mean accuracy for each trial in both groups (1 = DOP, 2 = NOP) without TimedOut trials
ACC_Mean_ByTrial_NoTimedOut <- ddply(TDOE_Prompt_NoTimedOut, c("Group", "Block"), summarise, Mean.ACC=mean(ComparisonStimuli.ACC))
ACC_Mean_ByTrial_NoTimedOut

##Mean accuracy and standard error from each group (1 = DOP, 2 = NOP) without TimedOut trials
ACC_Mean_ByGroup_NoTimedOut <- ddply(TDOE_Prompt_NoTimedOut, c("Group"), summarise, Mean.ACC=mean(ComparisonStimuli.ACC), SE.ACC=sd(ComparisonStimuli.ACC)/sqrt(length(ComparisonStimuli.ACC)))
ACC_Mean_ByGroup_NoTimedOut

#############################################
#############################################
###############Side Preference###############
#############################################
#############################################

##Side preference by subject
SidePreference <- ddply(TDOE_Prompt, c("Subject", "Group", "ComparisonStimuli.RESP"), summarise, SidePreference.count=length(ComparisonStimuli.RESP), mean=mean(ComparisonStimuli.ACC))
SidePreference

##Side preference by subject without TimedOut trials
SidePreference_NoTimedOut <- ddply(TDOE_Prompt_NoTimedOut, c("Subject", "Group", "ComparisonStimuli.RESP"), summarise, SidePreference.count=length(ComparisonStimuli.RESP), mean=mean(ComparisonStimuli.ACC))
SidePreference_NoTimedOut

#############################################
#############################################
################Moving Window################
#############################################
#############################################

##Merges mean accuracy of each subject by group without NFIB trials into a data frame (includes NAs)
ACC_DataFrame <- merge(ACC_Mean_DOP_NoTimedOut, ACC_Mean_NOP_NoTimedOut, by=c("Subject"), all=TRUE)

#############################################
#############################################
#############Rolling Mean####################
#############################################
#############################################

##Working rolling mean function
rollmeanfun <- function(x) {
  rollapply(x, 5, (mean), na.rm = TRUE)
}

##Creates new column with accuracy (0, 1) including NoTimedOut and Image1-caused 0s 
TDOE_Prompt$ACCwNA <- TDOE_Prompt$ComparisonStimuli.ACC

##Changes the ACCwNA column 0s caused by TimedOut to NA, leaves 0 caused by actual error
for (i in 1:nrow (TDOE_Prompt)) {
  if (TDOE_Prompt[i, c('ACCwNA')] == 0) {
    if (TDOE_Prompt[i, c('ComparisonStimuli.RESP')] == 'TimedOut') {
      TDOE_Prompt$ACCwNA[i] <- NA
    }
  }
}

Sub_ACC_window <- ddply(TDOE_Prompt, c("Group", "Subject"), summarise, ACCMovWin=rollmeanfun(ACCwNA))
Sub_ACC_window

##Creates a .csv file containing rolling mean calculations from above lines of script
write.csv(Sub_ACC_window, "Windowed_Data.csv")

Sub_ACC_window <- ddply(Sub_ACC_window, c("Subject"), transform, WindowNum=seq(1, (length(ACCMovWin)), by=1))
Sub_ACC_window

ACC_Window_Mean <- ddply(Sub_ACC_window, c("Group", "WindowNum"), summarise, Mean.ACC=mean(ACCMovWin, na.rm = TRUE), SE.ACC=sd(ACCMovWin, na.rm = TRUE)/sqrt(length(ACCMovWin)))
ACC_Window_Mean

##Creates the graph of the rolling mean; Condition 1 = DOP, 2 = NOP
ggplot(data=ACC_Window_Mean, aes(x=WindowNum, y=Mean.ACC, colour=factor(Group))) + 
  geom_line(stat = "identity", position = "dodge")+
  xlab("Averaged Trial") + ylab("Mean Accuracy (Prompt)") + ggtitle("DOE Learning Progression by Condition") +
  coord_cartesian(xlim = c(0, 50), ylim = c(0.0, 1.0)) +        
  labs(colour="Condition") +
  theme(plot.title = element_text(colour="Black", size=20, face="bold"), 
        legend.title = element_text(colour="Black", size=18, face="bold"), 
        legend.text = element_text(colour="Black", size=16, face="bold"),
        axis.title.x = element_text(colour="Black", size=16, face="bold"), 
        axis.title.y = element_text(colour="Black", size=16, face="bold"),
        axis.text.x = element_text(colour="Gray48", size=16, face="bold"),
        axis.text.y = element_text(colour="Gray48", size=16, face="bold")) 

#############################################
#############################################
########Averaged Trials in Bins##############
#############################################
#############################################

###Create bins of 5 trials each for all subjects excluding TimedOut trials
for (i in 1:nrow(TDOE_Prompt_NoTimedOut)) {
  if ((TDOE_Prompt_NoTimedOut[i, c("Block")] >= 1) & (TDOE_Prompt_NoTimedOut[i, c("Block")] <= 5)){
    TDOE_Prompt_NoTimedOut[i, c("BinNum")] <- 1
  } else if ((TDOE_Prompt_NoTimedOut[i, c("Block")] >= 6) & (TDOE_Prompt_NoTimedOut[i, c("Block")] <= 10)){
    TDOE_Prompt_NoTimedOut[i, c("BinNum")] <- 2
  } else if ((TDOE_Prompt_NoTimedOut[i, c("Block")] >= 11) & (TDOE_Prompt_NoTimedOut[i, c("Block")] <= 15)){
    TDOE_Prompt_NoTimedOut[i, c("BinNum")] <- 3
  } else if ((TDOE_Prompt_NoTimedOut[i, c("Block")] >= 16) & (TDOE_Prompt_NoTimedOut[i, c("Block")] <= 20)){
    TDOE_Prompt_NoTimedOut[i, c("BinNum")] <- 4
  } else if ((TDOE_Prompt_NoTimedOut[i, c("Block")] >= 21) & (TDOE_Prompt_NoTimedOut[i, c("Block")] <= 25)){
    TDOE_Prompt_NoTimedOut[i, c("BinNum")] <- 5
  } else if ((TDOE_Prompt_NoTimedOut[i, c("Block")] >= 26) & (TDOE_Prompt_NoTimedOut[i, c("Block")] <= 30)){
    TDOE_Prompt_NoTimedOut[i, c("BinNum")] <- 6
  } else if ((TDOE_Prompt_NoTimedOut[i, c("Block")] >= 31) & (TDOE_Prompt_NoTimedOut[i, c("Block")] <= 35)){
    TDOE_Prompt_NoTimedOut[i, c("BinNum")] <- 7
  } else if ((TDOE_Prompt_NoTimedOut[i, c("Block")] >= 36) & (TDOE_Prompt_NoTimedOut[i, c("Block")] <= 40)){
    TDOE_Prompt_NoTimedOut[i, c("BinNum")] <- 8
  } else if ((TDOE_Prompt_NoTimedOut[i, c("Block")] >= 41) & (TDOE_Prompt_NoTimedOut[i, c("Block")] <= 45)){
    TDOE_Prompt_NoTimedOut[i, c("BinNum")] <- 9
  } else if ((TDOE_Prompt_NoTimedOut[i, c("Block")] >= 46) & (TDOE_Prompt_NoTimedOut[i, c("Block")] <= 50)){
    TDOE_Prompt_NoTimedOut[i, c("BinNum")] <- 10
  } else {
    TDOE_Prompt_NoTimedOut[i, c("BinNum")] <- 11
  }
  
  if ((i %% 100) == 0) {
    print(i)
  }
}

Sub_ACC_Bin <- ddply(TDOE_Prompt_NoTimedOut, c("Group", "Subject", "BinNum"), summarise, Bin_ACC=mean(ComparisonStimuli.ACC), Bin_SE.ACC=sd(ComparisonStimuli.ACC)/sqrt(length(ComparisonStimuli.ACC)))
Sub_ACC_Bin

ACC_Bin <- ddply(TDOE_Prompt_NoTimedOut, c("Group", "BinNum"), summarise, Bin_ACC=mean(ComparisonStimuli.ACC), Bin_SE.ACC=sd(ComparisonStimuli.ACC)/sqrt(length(ComparisonStimuli.ACC)))
ACC_Bin

##Creates the graph of the averaged binned trials; Condition 1 = DOP, 2 = NOP
ggplot(data=ACC_Bin, aes(x=BinNum, y=Bin_ACC, colour=factor(Group))) + 
  geom_line(stat="identity", position= "identity")+
  xlab("Averaged Trials in Bins of 5") + ylab("Mean Accuracy (Prompt)") + ggtitle("DOE Learning Progression Per Condition") +
  coord_cartesian(xlim = c(0, 11), ylim = c(0.0, 1.0)) +        
  scale_fill_brewer(palette="Dark2", name = "Orientation") +
  labs(colour="Condition") +
  theme(plot.title = element_text(colour="Black", size=20, face="bold"), 
        legend.title = element_text(colour="Black", size=18, face="bold"), 
        legend.text = element_text(colour="Black", size=16, face="bold"),
        axis.title.x = element_text(colour="Black", size=16, face="bold"), 
        axis.title.y = element_text(colour="Black", size=16, face="bold"),
        axis.text.x = element_text(colour="Gray48", size=16, face="bold"),
        axis.text.y = element_text(colour="Gray48", size=16, face="bold"))

#############################################
#############################################
####################Stats####################
#############################################
#############################################
ACC_Bin <- glmer(ComparisonStimuli.ACC~ Group*BinNum + (1|Subject), data=TDOE_Prompt_NoTimedOut, family = "binomial")
summary(ACC_Bin)

ACC_Bin_2 <- lmer(Bin_ACC~ Group*BinNum + (1|Subject), data=Sub_ACC_Bin)
summary(ACC_Bin_2)

TDOE_Prompt_NoTimedOut_1_5 <- TDOE_Prompt_NoTimedOut[TDOE_Prompt_NoTimedOut$BinNum <= 5, ]
TDOE_Prompt_NoTimedOut_6_11 <- TDOE_Prompt_NoTimedOut[TDOE_Prompt_NoTimedOut$BinNum > 5, ]

##Analysis of bins 1-5
ACC_Bin_1_5 <- glmer(ComparisonStimuli.ACC~ Group*BinNum + (1|Subject), data=TDOE_Prompt_NoTimedOut_1_5, family = "binomial")
summary(ACC_Bin_1_5)

##Analysis of bins 6-11
ACC_Bin_6_11 <- glmer(ComparisonStimuli.ACC~ Group*BinNum + (1|Subject), data=TDOE_Prompt_NoTimedOut_6_11, family = "binomial")
summary(ACC_Bin_6_11)
