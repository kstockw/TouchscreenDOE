setwd ("/Users/kaydenstockwell/Desktop/Prelim Analysis/Touchscreen/TouchscreenDOE")
library(reshape2)
library(gtools) 
library(lme4)
library(plyr)
library(dplyr)
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
#########Stockwell for Kids DOE 2019##########
##############################################
##############################################

getwd()

#Coding note: Group: 1 = DOP, 2 = NOP; Use Trial for trial number

#Read in the csv file(s) of Eprime data, skip first row to ensure correct column names
TDOE_Kids_2_25_19_ORIGINAL <- read.csv(file = "TDOE_Kids_2_25_19.csv", header = TRUE, skip = 1)

#Make Dataframe of only experimental trials, remove practice trials
TDOE_Kids_TestOnly <- TDOE_Kids_2_25_19_ORIGINAL[TDOE_Kids_2_25_19_ORIGINAL$Running == 1, ]

#Displays subject numbers in numerical order and how many times they occur (trial #)
plyr::count(TDOE_Kids_TestOnly$Subject)

#Print column names
colnames(TDOE_Kids_TestOnly)

#Creates new column (Age) populated with subject numbers
TDOE_Kids_TestOnly$Age <- TDOE_Kids_TestOnly$Subject

#Replace subject numbers in age column with ages
TDOE_Kids_TestOnly$Age <- with(TDOE_Kids_TestOnly, ifelse(Age %in% c("19990001", "19990003", "19990006", "19990010",
                                                   "19990011", "19990012", "19990013", "19990014", "19990015"), 6,
                                        ifelse(Age %in% c("19990004", "19990007", "19990008",
                                                          "19990016", "19990017"), 4, 5)))

#Remove participants 19990009, 19990012, 19990013, 19990014 (side perseveration), possibly 19990015
TDOE_Kids_NoPersev <- TDOE_Kids_TestOnly %>% filter(!Subject %in% c("19990009", "19990012",
                                                                    "19990013", "19990014"))

#(old) Remove participants 19990009, 19990012, 19990013, 19990014 (side perseveration), possibly 19990015
#TDOE_Kids_No09 <- TDOE_Kids_TestOnly[TDOE_Kids_TestOnly$Subject != "19990009", ]
#TDOE_Kids_No12 <- TDOE_Kids_No09[TDOE_Kids_No09$Subject != "19990012", ]
#TDOE_Kids_No13 <- TDOE_Kids_No12[TDOE_Kids_No12$Subject != "19990013", ]
#TDOE_Kids_NoPersev <- TDOE_Kids_No13[TDOE_Kids_No13$Subject != "19990014", ]

#Remove excess columns from related to practice trial stimuli
TDOE_Kids <- TDOE_Kids_NoPersev[, -c(7, 13:14, 17:19)]

#Print column names
colnames(TDOE_Kids)

#Subset data to look at only 6 year olds
TDOE_Kids_Six <- TDOE_Kids[TDOE_Kids$Age == "6", ]

#Displays subject numbers in numerical order and how many times they occur (trial #)
plyr::count(TDOE_Kids_Six$Subject)

#############################################
#############################################
########## Means and SDs ####################
#############################################
#############################################

#Calculate and print mean accuracy by subject per group (1 = DOP, 2 = NOP) and accuracy standard error per group
ACC_Mean_BySubject <- ddply(TDOE_Kids_Six, c("Group", "Subject"), summarise, Mean.ACC=mean(ComparisonStimuli.ACC),
                            SE.ACC=sd(ComparisonStimuli.ACC)/sqrt(length(ComparisonStimuli.ACC)))
ACC_Mean_BySubject

#Calculate and print mean accuracy per group (1 = DOP, 2 = NOP) and accuracy standard error per group
ACC_Mean_ByGroup <- ddply(TDOE_Kids_Six, c("Group"), summarise, Mean.ACC=mean(ComparisonStimuli.ACC),
                          SE.ACC=sd(ComparisonStimuli.ACC)/sqrt(length(ComparisonStimuli.ACC)))
ACC_Mean_ByGroup

#############################################
#############################################
################Response Types###############
#############################################
#############################################

#Number of left, right, Image1 (touched non target area of screen), and TimedOut (i.e., no responses) across subject; Group: 1 = DOP, 2 = NOP
ResponseTypeSubLevel <- ddply(TDOE_Kids_Six, c("Subject", "Group", "ComparisonStimuli.RESP"), summarise, Resp.Count=length(ComparisonStimuli.ACC))
ResponseTypeSubLevel

#Number of left, right, Image1 (touched non target area of screen), and TimedOut (i.e., no responses) across group; Group: 1 = DOP, 2 = NOP
ResponseTypeNumGroupLevel <- ddply(TDOE_Kids_Six, c("Group", "ComparisonStimuli.RESP"), summarise, Resp.Count=length(ComparisonStimuli.ACC))
ResponseTypeNumGroupLevel

#Divide data into each first and second half block (trial number) 
First25_Trial_Data <- TDOE_Kids_Six[TDOE_Kids_Six$Trial <= 25, ]
head(First25_Trial_Data$Trial, 50)
Last25_Trial_Data <- TDOE_Kids_Six[TDOE_Kids_Six$Trial > 25, ]
head(Last25_Trial_Data$Trial, 50)

#First half of data; number of left, right, Image1 (touch non target area of screen) and TimedOut (i.e., no responses) across subject; Group: 1 = DOP, 2 = NOP
First25_ResponseTypeNum <- ddply(First25_Trial_Data, c("Subject", "Group", "ComparisonStimuli.RESP"),
                                 summarise, Resp.Count=length(ComparisonStimuli.ACC))
First25_ResponseTypeNum

#Second half of data; number of left, right, Image1 (touch non target area of screen) and TimedOut (i.e., no responses) across subject; Group: 1 = DOP, 2 = NOP
Last25_ResponseTypeNum <- ddply(Last25_Trial_Data, c("Subject", "Group", "ComparisonStimuli.RESP"),
                                summarise, Resp.Count=length(ComparisonStimuli.ACC))
Last25_ResponseTypeNum

#Exclude trials where response was "TimedOut"
TDOE_Kids_NoTimedOut <- TDOE_Kids_Six[TDOE_Kids_Six$ComparisonStimuli.RESP != "TimedOut", ]

#Number of left, right, Image1 (touch non target area of screen) across subject; Group: 1 = DOP, 2 = NOP
ResponseTypeSubLevel_NoTimedOut <- ddply(TDOE_Kids_NoTimedOut, c("Subject", "Group", "ComparisonStimuli.RESP"),
                                         summarise, Resp.Count=length(ComparisonStimuli.ACC))
ResponseTypeSubLevel_NoTimedOut

#Number of left, right, Image1 (touch non target area of screen) by Group; Group: 1 = DOP, 2 = NOP
ResponseTypeNumGroupLevel_NoTimedOut <- ddply(TDOE_Kids_NoTimedOut, c("Group", "ComparisonStimuli.RESP"),
                                              summarise, Resp.Count=length(ComparisonStimuli.ACC))
ResponseTypeNumGroupLevel_NoTimedOut

#############################################
##############Response Congruency############
###################Check#####################
#############################################

#Compares each subject's response (all trials) to the correct answer and how many times it occured (e.g. how many times was the correct response
#left and they looked left, correct right and they looked left, etc; Image 1 included)
ResponseTypeNum_Congruent <- ddply(TDOE_Kids_Six, c("Subject", "ComparisonStimuli.RESP", "CorrectAnswer"),
                                   summarise, Resp.Count=length(CorrectAnswer))
ResponseTypeNum_Congruent

#Compares each subject's response (no TimedOut trials) to the correct answer and how many times it occured (e.g. how many times was the correct response
#left and they looked left, correct right and they looked left, etc; Image1 included)
ResponseTypeNum_Congruent_NoTimedOut <- ddply(TDOE_Kids_NoTimedOut, c("Subject", "ComparisonStimuli.RESP",
                                              "CorrectAnswer"), summarise, Resp.Count=length(CorrectAnswer))
ResponseTypeNum_Congruent_NoTimedOut

#############################################
#############################################
########### Means w/o TimedOut ##############
#############################################
#############################################

#Mean accuracy for each subject without TimedOut trials, noting the subject's group (1 = DOP, 2 = NOP) 
ACC_Mean_BySubject_NoTimedOut <- ddply(TDOE_Kids_NoTimedOut, c("Group", "Subject"),
                                       summarise, Mean.ACC=mean(ComparisonStimuli.ACC))
ACC_Mean_BySubject_NoTimedOut

#Mean accuracy for each DOP subject without TimedOut trials
ACC_Mean_DOP_NoTimedOut <- ACC_Mean_BySubject_NoTimedOut[ACC_Mean_BySubject_NoTimedOut$Group == "1", ]
colnames(ACC_Mean_DOP_NoTimedOut) <- c("Group", "Subject", "DOP_ACC")
ACC_Mean_DOP_NoTimedOut

#Mean accuracy for each NOP subject without TimedOut trials
ACC_Mean_NOP_NoTimedOut <- ACC_Mean_BySubject_NoTimedOut[ACC_Mean_BySubject_NoTimedOut$Group == "2", ]
colnames(ACC_Mean_NOP_NoTimedOut) <- c("Group", "Subject", "NOP_ACC")
ACC_Mean_NOP_NoTimedOut

#Mean accuracy for each trial in both groups (1 = DOP, 2 = NOP) without TimedOut trials
ACC_Mean_ByTrial_NoTimedOut <- ddply(TDOE_Kids_NoTimedOut, c("Group", "Trial"), summarise,
                                     Mean.ACC=mean(ComparisonStimuli.ACC))
ACC_Mean_ByTrial_NoTimedOut

#Mean accuracy and standard error from each group (1 = DOP, 2 = NOP) without TimedOut trials
ACC_Mean_ByGroup_NoTimedOut <- ddply(TDOE_Kids_NoTimedOut, c("Group"), summarise, Mean.ACC=mean(ComparisonStimuli.ACC),
                                     SE.ACC=sd(ComparisonStimuli.ACC)/sqrt(length(ComparisonStimuli.ACC)))
ACC_Mean_ByGroup_NoTimedOut

#############################################
#############################################
###############Side Preference###############
#############################################
#############################################

#Side preference by subject
SidePreference <- ddply(TDOE_Kids_Six, c("Subject", "Group", "ComparisonStimuli.RESP"), summarise,
                        SidePreference.count=length(ComparisonStimuli.RESP), mean=mean(ComparisonStimuli.ACC))
SidePreference

#Side preference by subject without TimedOut trials
SidePreference_NoTimedOut <- ddply(TDOE_Kids_NoTimedOut, c("Subject", "Group", "ComparisonStimuli.RESP"), summarise,
                                   SidePreference.count=length(ComparisonStimuli.RESP), mean=mean(ComparisonStimuli.ACC))
SidePreference_NoTimedOut

#Mean accuracy and standard error from each group (1 = DOP, 2 = NOP) without TimedOut trials
ACC_Mean_ByGroup_NoPersev <- ddply(TDOE_Kids_NoTimedOut, c("Group"), summarise, Mean.ACC=mean(ComparisonStimuli.ACC),
                                   SE.ACC=sd(ComparisonStimuli.ACC)/sqrt(length(ComparisonStimuli.ACC)))
ACC_Mean_ByGroup_NoTimedOut

#############################################
#############################################
################Moving Window################
#############################################
#############################################

#Merges mean accuracy of each subject by group without NFIB trials into a data frame (includes NAs)
ACC_DataFrame <- merge(ACC_Mean_DOP_NoTimedOut, ACC_Mean_NOP_NoTimedOut, by=c("Subject"), all=TRUE)

#############################################
#############################################
#############Rolling Mean####################
#############################################
#############################################

#Working rolling mean function
rollmeanfun <- function(x) {
  rollapply(x, 5, (mean), na.rm = TRUE)
}

#Creates new column with accuracy (0, 1) including NoTimedOut and Image1-caused 0s 
TDOE_Kids_Six$ACCwNA <- TDOE_Kids_Six$ComparisonStimuli.ACC

#Changes the ACCwNA column 0s caused by TimedOut to NA, leaves 0 caused by actual error
for (i in 1:nrow (TDOE_Kids_Six)) {
  if (TDOE_Kids_Six[i, c('ACCwNA')] == 0) {
    if (TDOE_Kids_Six[i, c('ComparisonStimuli.RESP')] == 'TimedOut') {
      TDOE_Kids_Six$ACCwNA[i] <- NA
    }
  }
}

Sub_ACC_window <- ddply(TDOE_Kids_Six, c("Group", "Subject"), plyr::summarise, ACCMovWin=rollmeanfun(ACCwNA))
Sub_ACC_window

#Creates a .csv file containing rolling mean calculations from above lines of script
write.csv(Sub_ACC_window, "Windowed_Kids_Six_Data.csv")

Sub_ACC_window <- ddply(Sub_ACC_window, c("Subject"), transform, WindowNum=seq(1, (length(ACCMovWin)), by=1))
Sub_ACC_window

ACC_Window_Mean <- ddply(Sub_ACC_window, c("Group", "WindowNum"), summarise, Mean.ACC=mean(ACCMovWin,
                        na.rm = TRUE), SE.ACC=sd(ACCMovWin, na.rm = TRUE)/sqrt(length(ACCMovWin)))
ACC_Window_Mean

#Creates the graph of the rolling mean; Condition 1 = DOP, 2 = NOP
ggplot(data=ACC_Window_Mean, aes(x=WindowNum, y=Mean.ACC, colour=factor(Group))) + 
  geom_line(stat = "identity", position = "dodge")+
  xlab("Averaged Trial") + ylab("Mean Accuracy (Kids)") + ggtitle("DOE Learning Progression by Condition") +
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

#Create bins of 5 trials each for all subjects excluding TimedOut trials
for (i in 1:nrow(TDOE_Kids_NoTimedOut)) {
  if ((TDOE_Kids_NoTimedOut[i, c("Trial")] >= 1) & (TDOE_Kids_NoTimedOut[i, c("Trial")] <= 5)){
    TDOE_Kids_NoTimedOut[i, c("BinNum")] <- 1
  } else if ((TDOE_Kids_NoTimedOut[i, c("Trial")] >= 6) & (TDOE_Kids_NoTimedOut[i, c("Trial")] <= 10)){
    TDOE_Kids_NoTimedOut[i, c("BinNum")] <- 2
  } else if ((TDOE_Kids_NoTimedOut[i, c("Trial")] >= 11) & (TDOE_Kids_NoTimedOut[i, c("Trial")] <= 15)){
    TDOE_Kids_NoTimedOut[i, c("BinNum")] <- 3
  } else if ((TDOE_Kids_NoTimedOut[i, c("Trial")] >= 16) & (TDOE_Kids_NoTimedOut[i, c("Trial")] <= 20)){
    TDOE_Kids_NoTimedOut[i, c("BinNum")] <- 4
  } else if ((TDOE_Kids_NoTimedOut[i, c("Trial")] >= 21) & (TDOE_Kids_NoTimedOut[i, c("Trial")] <= 25)){
    TDOE_Kids_NoTimedOut[i, c("BinNum")] <- 5
  } else if ((TDOE_Kids_NoTimedOut[i, c("Trial")] >= 26) & (TDOE_Kids_NoTimedOut[i, c("Trial")] <= 30)){
    TDOE_Kids_NoTimedOut[i, c("BinNum")] <- 6
  } else if ((TDOE_Kids_NoTimedOut[i, c("Trial")] >= 31) & (TDOE_Kids_NoTimedOut[i, c("Trial")] <= 35)){
    TDOE_Kids_NoTimedOut[i, c("BinNum")] <- 7
  } else if ((TDOE_Kids_NoTimedOut[i, c("Trial")] >= 36) & (TDOE_Kids_NoTimedOut[i, c("Trial")] <= 40)){
    TDOE_Kids_NoTimedOut[i, c("BinNum")] <- 8
  } else if ((TDOE_Kids_NoTimedOut[i, c("Trial")] >= 41) & (TDOE_Kids_NoTimedOut[i, c("Trial")] <= 45)){
    TDOE_Kids_NoTimedOut[i, c("BinNum")] <- 9
  } else if ((TDOE_Kids_NoTimedOut[i, c("Trial")] >= 46) & (TDOE_Kids_NoTimedOut[i, c("Trial")] <= 50)){
    TDOE_Kids_NoTimedOut[i, c("BinNum")] <- 10
  } else {
    TDOE_Kids_NoTimedOut[i, c("BinNum")] <- 11
  }
  
  if ((i %% 100) == 0) {
    print(i)
  }
}

Sub_ACC_Bin <- ddply(TDOE_Kids_NoTimedOut, c("Group", "Subject", "BinNum"), summarise, Bin_ACC=mean(ComparisonStimuli.ACC), Bin_SE.ACC=sd(ComparisonStimuli.ACC)/sqrt(length(ComparisonStimuli.ACC)))
Sub_ACC_Bin

ACC_Bin <- ddply(TDOE_Kids_NoTimedOut, c("Group", "BinNum"), summarise, Bin_ACC=mean(ComparisonStimuli.ACC), Bin_SE.ACC=sd(ComparisonStimuli.ACC)/sqrt(length(ComparisonStimuli.ACC)))
ACC_Bin

#Creates the graph of the averaged binned trials; Condition 1 = DOP, 2 = NOP
ggplot(data=ACC_Bin, aes(x=BinNum, y=Bin_ACC, colour=factor(Group))) + 
  geom_line(stat="identity", position= "identity")+
  xlab("Averaged Trials in Bins of 5") + ylab("Mean Accuracy (Kids)") + ggtitle("DOE Learning Progression By Condition") +
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
ACC_Bin <- glmer(ComparisonStimuli.ACC~ Group*BinNum + (1|Subject), data=TDOE_Kids_NoTimedOut, family = "binomial")
summary(ACC_Bin)

ACC_Bin_2 <- lmer(Bin_ACC~ Group*BinNum + (1|Subject), data=Sub_ACC_Bin)
summary(ACC_Bin_2)

TDOE_Kids_NoTimedOut_1_5 <- TDOE_Kids_NoTimedOut[TDOE_Kids_NoTimedOut$BinNum <= 5, ]
TDOE_Kids_NoTimedOut_6_11 <- TDOE_Kids_NoTimedOut[TDOE_Kids_NoTimedOut$BinNum > 5, ]

#Analysis of bins 1-5
ACC_Bin_1_5 <- glmer(ComparisonStimuli.ACC~ Group*BinNum + (1|Subject), data=TDOE_Kids_NoTimedOut_1_5, family = "binomial")
summary(ACC_Bin_1_5)

#Analysis of bins 6-11
ACC_Bin_6_11 <- glmer(ComparisonStimuli.ACC~ Group*BinNum + (1|Subject), data=TDOE_Kids_NoTimedOut_6_11, family = "binomial")
summary(ACC_Bin_6_11)
