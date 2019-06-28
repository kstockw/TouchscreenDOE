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


##############################################
###########Code DOE EPrime Analysis###########
########By Sarah Olsen edited by Kayden#######
#########Stockwell for Kids DOE 2019##########
##############################################
##############################################

getwd()
source("Extra_Scripts.R")

#Coding note: Group: 1 = DOP, 2 = NOP; Use DOP.Sample for trial number

#Read in the csv file(s) of Eprime data, skip first row to ensure correct column names
TDOE_Kids_6_26_19_ORIGINAL <- read.csv(file = "ORIGINAL6_26_19SpaceCow.csv", header = TRUE, skip = 3)

#Make Dataframe of only experimental trials, remove practice trials
TDOE_Kids_TestOnly <- TDOE_Kids_6_26_19_ORIGINAL %>% filter(grepl('DOP', Running))
                             
#Displays subject numbers in numerical order and how many times they occur (# of trials)
plyr::count(TDOE_Kids_TestOnly$Subject)
#Fixes incorrectly named subject
TDOE_Kids_TestOnly$Subject[TDOE_Kids_TestOnly$Subject == 990048] <- 19990048

#Print column names
colnames(TDOE_Kids_TestOnly)

#Retain only relevant columns
TDOE_Kids <- TDOE_Kids_TestOnly[, c("Subject", "Session", "Block", "ComparisonStimuli.ACC", "ComparisonStimuli.RESP",
                                    "ComparisonStimuli1.ACC", "ComparisonStimuli1.RESP", "ComparisonStimulus",
                                    "CorrectAnswer", "DOP", "DOP.Sample", "SampleStimulus")]

TDOE_Kids <- plyr::rename(TDOE_Kids, c("Block" = "Trial"))
TDOE_Kids <- plyr::rename(TDOE_Kids, c("Session" = "Group"))

#Requires source script loaded at the beginning of code
TDOE_Kids <- TDOE_Kids[moveme(names(TDOE_Kids), "Trial after Group")]

#Creates new column (Age) populated with subject numbers (change this to importing csv and binding)
#TDOE_Kids_TestOnly$Age <- TDOE_Kids_TestOnly$Subject

#Replace subject numbers in age column with ages
#TDOE_Kids_TestOnly$Age <- with(TDOE_Kids_TestOnly, ifelse(test = Age %in% c("19990001", "19990003", "19990006", "19990010",
#                                                  "1999011", "19990012", "19990013", "19990014", "19990015"), yes = 6,
#                                       no = ifelse(test = Age %in% c("19990004", "19990007", "19990008", "19990016", "19990017", "19990023"), yes = 4,
#                                        no = ifelse(test = Age %in% c("19990019", "19990020", "19990029", "19990032", "19990036"), yes = 9,
#                                        no = ifelse(test = Age %in% c("19990022", "19990024", "19990030", "19990035", "19990037"), yes = 8,
#                                        no = ifelse(test = Age %in% c("19990026", "19990028", "19990031", "19990033", "19990034"), yes = 7,
#                                        no = 5))))))

#Confirms that the subject numbers of 5 year olds are correct, could probably be written better
#TDOE_Kids_Five <- TDOE_Kids_TestOnly[TDOE_Kids_TestOnly$Age == "5", ]
#plyr::count(TDOE_Kids_Five$Subject)

#Remove participants 19990009, 19990012, 19990013, 19990014 (side perseveration), possibly 19990015
#try to write an equation to auto remove based on persev proportion
#TDOE_Kids_Younger <- TDOE_Kids_Younger %>% filter(!Subject %in% c("19990009", "19990012",
#                                                                    "19990013", "19990014"))

#Print column names
colnames(TDOE_Kids)

#Subset data to look at only 6 year olds
#TDOE_Kids_Six <- TDOE_Kids[TDOE_Kids$Age == "6", ]

#Subset data to look at only 4 - 6 year olds
#TDOE_Kids_Younger <- TDOE_Kids %>% filter(Age %in% c("4", "5", "6"))
#plyr::count(TDOE_Kids_Younger$Subject)

#Subset data to look at only 7 - 9 year olds
#TDOE_Kids_Older <- TDOE_Kids %>% filter(Age %in% c("7", "8", "9"))
#plyr::count(TDOE_Kids_Older$Subject)

#Merge columns to remove NULL data due to EPrime design (should work with mutate/coalesce but isn't)
#Transform factor columns to characters
TDOE_Kids$ComparisonStimuli.ACC <- as.character(TDOE_Kids$ComparisonStimuli.ACC)
TDOE_Kids$ComparisonStimuli1.ACC <- as.character(TDOE_Kids$ComparisonStimuli1.ACC)
TDOE_Kids_Merged <- within(TDOE_Kids, ComparisonStimuli.ACC <- ifelse(ComparisonStimuli.ACC == "NULL",
                                                                      ComparisonStimuli1.ACC,
                                                                      ComparisonStimuli.ACC)) 

TDOE_Kids_Merged$ComparisonStimuli.RESP <- as.character(TDOE_Kids_Merged$ComparisonStimuli.RESP)
TDOE_Kids_Merged$ComparisonStimuli1.RESP <- as.character(TDOE_Kids_Merged$ComparisonStimuli1.RESP)
TDOE_Kids_Merged <- within(TDOE_Kids_Merged, ComparisonStimuli.RESP <- ifelse(ComparisonStimuli.RESP == "NULL",
                                                                      ComparisonStimuli1.RESP,
                                                                      ComparisonStimuli.RESP))

#Convert to numeric for analyses (Using DOP.Sample for trial number)
TDOE_Kids_Merged$ComparisonStimuli.ACC <- as.numeric(TDOE_Kids_Merged$ComparisonStimuli.ACC)
TDOE_Kids_Merged$DOP.Sample <- as.character(TDOE_Kids_Merged$DOP.Sample)
TDOE_Kids_Merged$DOP.Sample <- as.numeric(TDOE_Kids_Merged$DOP.Sample)
#############################################
#############################################
########## Means and SDs ####################
#############################################
#############################################

#Calculate and print mean accuracy by subject per group (1 = DOP, 2 = NOP) and accuracy standard error per group
ACC_Mean_BySubject <- ddply(TDOE_Kids_Merged, c("Group", "Subject"), summarise, Mean.ACC=mean(ComparisonStimuli.ACC),
                            SE.ACC=sd(ComparisonStimuli.ACC)/sqrt(length(ComparisonStimuli.ACC)))
ACC_Mean_BySubject

#Calculate and print mean accuracy per group (1 = DOP, 2 = NOP) and accuracy standard error per group
ACC_Mean_ByGroup <- ddply(TDOE_Kids_Merged, c("Group"), summarise, Mean.ACC=mean(ComparisonStimuli.ACC),
                          SE.ACC=sd(ComparisonStimuli.ACC)/sqrt(length(ComparisonStimuli.ACC)))
ACC_Mean_ByGroup

#############################################
#############################################
################Response Types###############
#############################################
#############################################

#Number of left, right, Image1 (touched non target area of screen), and TimedOut (i.e., no responses) across subject; Group: 1 = DOP, 2 = NOP
ResponseTypeSubLevel <- ddply(TDOE_Kids_Merged, c("Subject", "Group", "ComparisonStimuli.RESP"), summarise, Resp.Count=length(ComparisonStimuli.ACC))
ResponseTypeSubLevel

#Number of left, right, Image1 (touched non target area of screen), and TimedOut (i.e., no responses) across group; Group: 1 = DOP, 2 = NOP
ResponseTypeNumGroupLevel <- ddply(TDOE_Kids_Merged, c("Group", "ComparisonStimuli.RESP"), summarise, Resp.Count=length(ComparisonStimuli.ACC))
ResponseTypeNumGroupLevel

#Divide data into each first and second half block (trial number) (use DOP.Sample for trial)
First25_Trial_Data <- TDOE_Kids_Merged[TDOE_Kids_Merged$DOP.Sample <= 24, ]
head(First25_Trial_Data$DOP.Sample, 48)
Last25_Trial_Data <- TDOE_Kids_Merged[TDOE_Kids_Merged$DOP.Sample > 24, ]
head(Last25_Trial_Data$DOP.Sample, 48)

#First half of data; number of left, right, Image1 (touch non target area of screen) and TimedOut (i.e., no responses) across subject; Group: 1 = DOP, 2 = NOP
First25_ResponseTypeNum <- ddply(First25_Trial_Data, c("Subject", "Group", "ComparisonStimuli.RESP"),
                                 summarise, Resp.Count=length(ComparisonStimuli.ACC))
First25_ResponseTypeNum

#Second half of data; number of left, right, Image1 (touch non target area of screen) and TimedOut (i.e., no responses) across subject; Group: 1 = DOP, 2 = NOP
Last25_ResponseTypeNum <- ddply(Last25_Trial_Data, c("Subject", "Group", "ComparisonStimuli.RESP"),
                                summarise, Resp.Count=length(ComparisonStimuli.ACC))
Last25_ResponseTypeNum

#Exclude trials where response was "TimedOut"
TDOE_Kids_NoTimedOut <- TDOE_Kids_Merged[TDOE_Kids_Merged$ComparisonStimuli.RESP != "TimedOut", ]

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
ResponseTypeNum_Congruent <- ddply(TDOE_Kids_Merged, c("Subject", "ComparisonStimuli.RESP", "CorrectAnswer"),
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

#Mean accuracy for each trial in both groups (1 = DOP, 2 = NOP) without TimedOut trials (use DOP.Sample for trial)
ACC_Mean_ByTrial_NoTimedOut <- ddply(TDOE_Kids_NoTimedOut, c("Group", "DOP.Sample"), summarise,
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
SidePreference <- ddply(TDOE_Kids_Merged, c("Subject", "Group", "ComparisonStimuli.RESP"), summarise,
                        SidePreference.count=length(ComparisonStimuli.RESP), mean=mean(ComparisonStimuli.ACC))
SidePreference

#Side preference by subject without TimedOut trials
SidePreference_NoTimedOut <- ddply(TDOE_Kids_NoTimedOut, c("Subject", "Group", "ComparisonStimuli.RESP"), summarise,
                                   SidePreference.count=length(ComparisonStimuli.RESP), mean=mean(ComparisonStimuli.ACC))
SidePreference_NoTimedOut

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

#Working rolling mean function (uses 5 for 50 trials, 4 for 48
rollmeanfun <- function(x) {
  rollapply(x, 4, (mean), na.rm = TRUE)
}

#Creates new column with accuracy (0, 1) including NoTimedOut and Image1-caused 0s 
TDOE_Kids_Merged$ACCwNA <- TDOE_Kids_Merged$ComparisonStimuli.ACC

#Changes the ACCwNA column 0s caused by TimedOut to NA, leaves 0 caused by actual error
for (i in 1:nrow (TDOE_Kids_Merged)) {
  if (TDOE_Kids_Merged[i, c('ACCwNA')] == 0) {
    if (TDOE_Kids_Merged[i, c('ComparisonStimuli.RESP')] == 'TimedOut') {
      TDOE_Kids_Merged$ACCwNA[i] <- NA
    }
  }
}

Sub_ACC_window <- ddply(TDOE_Kids_Merged, c("Group", "Subject"), plyr::summarise, ACCMovWin=rollmeanfun(ACCwNA))
Sub_ACC_window

#Creates a .csv file containing rolling mean calculations from above lines of script
write.csv(Sub_ACC_window, "Windowed_Kids_CowSpace_Data.csv")

Sub_ACC_window <- ddply(Sub_ACC_window, c("Subject"), transform, WindowNum=seq(1, (length(ACCMovWin)), by=1))
Sub_ACC_window

ACC_Window_Mean <- ddply(Sub_ACC_window, c("Group", "WindowNum"), summarise, Mean.ACC=mean(ACCMovWin,
                        na.rm = TRUE), SE.ACC=sd(ACCMovWin, na.rm = TRUE)/sqrt(length(ACCMovWin)))
ACC_Window_Mean

#Creates the graph of the rolling mean; Condition 1 = DOP, 2 = NOP
ggplot(data=ACC_Window_Mean, aes(x=WindowNum, y=Mean.ACC, colour=factor(Group))) + 
  geom_line(stat = "identity", position = "dodge")+
  xlab("Averaged Trial") + ylab("Mean Accuracy (5 yrs)") + ggtitle("DOE Learning Progression by Condition") +
  coord_cartesian(xlim = c(0, 48), ylim = c(0.0, 1.0)) +        
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

#Create bins of 4 trials each for all subjects excluding TimedOut trials (use DOP.Sample for trial)
for (i in 1:nrow(TDOE_Kids_NoTimedOut)) {
  if ((TDOE_Kids_NoTimedOut[i, c("DOP.Sample")] >= 1) & (TDOE_Kids_NoTimedOut[i, c("DOP.Sample")] <= 4)){
    TDOE_Kids_NoTimedOut[i, c("BinNum")] <- 1
  } else if ((TDOE_Kids_NoTimedOut[i, c("DOP.Sample")] >= 5) & (TDOE_Kids_NoTimedOut[i, c("DOP.Sample")] <= 8)){
    TDOE_Kids_NoTimedOut[i, c("BinNum")] <- 2
  } else if ((TDOE_Kids_NoTimedOut[i, c("DOP.Sample")] >= 9) & (TDOE_Kids_NoTimedOut[i, c("DOP.Sample")] <= 12)){
    TDOE_Kids_NoTimedOut[i, c("BinNum")] <- 3
  } else if ((TDOE_Kids_NoTimedOut[i, c("DOP.Sample")] >= 13) & (TDOE_Kids_NoTimedOut[i, c("DOP.Sample")] <= 16)){
    TDOE_Kids_NoTimedOut[i, c("BinNum")] <- 4
  } else if ((TDOE_Kids_NoTimedOut[i, c("DOP.Sample")] >= 17) & (TDOE_Kids_NoTimedOut[i, c("DOP.Sample")] <= 20)){
    TDOE_Kids_NoTimedOut[i, c("BinNum")] <- 5
  } else if ((TDOE_Kids_NoTimedOut[i, c("DOP.Sample")] >= 21) & (TDOE_Kids_NoTimedOut[i, c("DOP.Sample")] <= 24)){
    TDOE_Kids_NoTimedOut[i, c("BinNum")] <- 6
  } else if ((TDOE_Kids_NoTimedOut[i, c("DOP.Sample")] >= 25) & (TDOE_Kids_NoTimedOut[i, c("DOP.Sample")] <= 28)){
    TDOE_Kids_NoTimedOut[i, c("BinNum")] <- 7
  } else if ((TDOE_Kids_NoTimedOut[i, c("DOP.Sample")] >= 29) & (TDOE_Kids_NoTimedOut[i, c("DOP.Sample")] <= 32)){
    TDOE_Kids_NoTimedOut[i, c("BinNum")] <- 8
  } else if ((TDOE_Kids_NoTimedOut[i, c("DOP.Sample")] >= 33) & (TDOE_Kids_NoTimedOut[i, c("DOP.Sample")] <= 36)){
    TDOE_Kids_NoTimedOut[i, c("BinNum")] <- 9
  } else if ((TDOE_Kids_NoTimedOut[i, c("DOP.Sample")] >= 37) & (TDOE_Kids_NoTimedOut[i, c("DOP.Sample")] <= 40)){
    TDOE_Kids_NoTimedOut[i, c("BinNum")] <- 10
  } else if ((TDOE_Kids_NoTimedOut[i, c("DOP.Sample")] >= 41) & (TDOE_Kids_NoTimedOut[i, c("DOP.Sample")] <= 44)){
    TDOE_Kids_NoTimedOut[i, c("BinNum")] <- 11
  } else if ((TDOE_Kids_NoTimedOut[i, c("DOP.Sample")] >= 45) & (TDOE_Kids_NoTimedOut[i, c("DOP.Sample")] <= 48)){
    TDOE_Kids_NoTimedOut[i, c("BinNum")] <- 12
  } else {
    TDOE_Kids_NoTimedOut[i, c("BinNum")] <- 13
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
  xlab("Averaged Trials in Bins of 5") + ylab("Mean Accuracy (5 yrs)") + ggtitle("DOE Learning Progression By Condition") +
  coord_cartesian(xlim = c(0, 14), ylim = c(0.0, 1.0)) +        
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

TDOE_Kids_NoTimedOut_1_6 <- TDOE_Kids_NoTimedOut[TDOE_Kids_NoTimedOut$BinNum <= 6, ]
TDOE_Kids_NoTimedOut_7_12 <- TDOE_Kids_NoTimedOut[TDOE_Kids_NoTimedOut$BinNum > 6, ]

#Analysis of bins 1-6
ACC_Bin_1_6 <- glmer(ComparisonStimuli.ACC~ Group*BinNum + (1|Subject), data=TDOE_Kids_NoTimedOut_1_6, family = "binomial")
summary(ACC_Bin_1_6)

#Analysis of bins 7-12
ACC_Bin_7_12 <- glmer(ComparisonStimuli.ACC~ Group*BinNum + (1|Subject), data=TDOE_Kids_NoTimedOut_7_12, family = "binomial")
summary(ACC_Bin_7_12)
