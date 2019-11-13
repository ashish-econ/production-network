
################################################################ ASSIGNMENT# 1 POLITICAL ECONOMY #################################################################################################################
###############################################################       ANALYSIS STARTS            ##############################################################################################################################
##############################################################         READIND DATA ##############################################################################################################################################################

library(data.table)

file1 = fread('F:/Ashoka University/Semester 3/Political Economy/Assignments/Assignment_1/Data_CSV/AE 2010.csv',skip = 1,header = T)
file2 = fread('F:/Ashoka University/Semester 3/Political Economy/Assignments/Assignment_1/Data_CSV/AE_2008.csv',skip=4,header =T)
file3 = fread('F:/Ashoka University/Semester 3/Political Economy/Assignments/Assignment_1/Data_CSV/AE2009_8913.csv',skip=1,header=T)
file4 = fread('F:/Ashoka University/Semester 3/Political Economy/Assignments/Assignment_1/Data_CSV/AE2011.csv',skip=3,header=T)
file5 = fread('F:/Ashoka University/Semester 3/Political Economy/Assignments/Assignment_1/Data_CSV/AE2012_8913.csv',skip=1,header=T)
file6 = fread('F:/Ashoka University/Semester 3/Political Economy/Assignments/Assignment_1/Data_CSV/December 2013 LA election.csv',skip=2,header = T)
file7  = fread('F:/Ashoka University/Semester 3/Political Economy/Assignments/Assignment_1/Data_CSV/DECEMBER 2014 LA election.csv',skip =2,header=T)
file8 = fread('F:/Ashoka University/Semester 3/Political Economy/Assignments/Assignment_1/Data_CSV/FEBRUARY- 2015 LA election.csv',skip = 3,header=T)
file9 = fread('F:/Ashoka University/Semester 3/Political Economy/Assignments/Assignment_1/Data_CSV/LA (Nov) 2015.csv',skip=2,header = T)
file10 = fread('F:/Ashoka University/Semester 3/Political Economy/Assignments/Assignment_1/Data_CSV/LA 2013.csv',skip=3,header = T)
file11 = fread('F:/Ashoka University/Semester 3/Political Economy/Assignments/Assignment_1/Data_CSV/LA 2016.csv',skip=2,header = T)
file12 = fread('F:/Ashoka University/Semester 3/Political Economy/Assignments/Assignment_1/Data_CSV/LA 2017.csv')
file13 = fread('F:/Ashoka University/Semester 3/Political Economy/Assignments/Assignment_1/Data_CSV/MAY 2014 LA election.csv',skip=2,header = T)
file14 = fread('F:/Ashoka University/Semester 3/Political Economy/Assignments/Assignment_1/Data_CSV/OCTOBER 2014 LA election.csv',skip=2,header = T)

############################################################# READING DATA ENDS ##############################################################################################################################
######################################### MAKING THE COLUMNS UNIFORM, CLEANING THE DATA AND MERGING THE DATA ###################################################################################
file2 <- select(file2,-c(V16))
file2 <- na.omit(file2)
file8 <- na.omit(file8)
file11 <- na.omit(file11)
file12 <- na.omit(file12)
file13 <- na.omit(file13)
file8 <- select(file8,-c(V16:V23))
file6$DIST_NAME <- NA

colnames(file6)[12] <- "PARTYABBRE"
colnames(file6)[13] <- "TOTVOTPOLL"
colnames(file7)[14] <- "TOTVOTPOLL"
colnames(file8)[14] <- "TOTVOTPOLL"
colnames(file9)[14] <- "TOTVOTPOLL"
colnames(file10)[13] <- "PARTYABBRE"
colnames(file10)[14] <- "TOTVOTPOLL"
colnames(file11)[14] <- "TOTVOTPOLL"
colnames(file12)[14] <- "TOTVOTPOLL"
colnames(file13)[14] <- "TOTVOTPOLL"
colnames(file14)[14] <- "TOTVOTPOLL"



remove(Election_Data_1)
Election_Data_1 <- rbind(file1,file2,file3,file4,file5,file6,file7,file8,file9,file10,file11,file12,file13,file14)
Election_Data <- Election_Data_1
############################################################ DATASET CREATED ########################################################################################################################################

########################################################## ANALYSIS PART STARTED ######################################################################################################################################################3


#####################################################   CALCULATING VOTE SHARE OF THE CANDIDATES ###################################################################################################################################
Election_Data$TOTVOTPOLL = as.numeric(Election_Data$TOTVOTPOLL)

total_votes = Election_Data[,.(total_votes = sum(TOTVOTPOLL)),by=.(YEAR, ST_NAME,AC_NO)]

merged_dataset = merge(Election_Data, total_votes, by=c("YEAR", "ST_NAME", "AC_NO"))
merged_dataset$vote_share = (merged_dataset$TOTVOTPOLL / merged_dataset$total_votes)

# Election_Data_2 <- Election_Data_1[complete.cases(Election_Data_1)]
# total_votes_2 <- Election_Data_2[,.(total_votes_2=sum(TOTVOTPOLL)),by=.(YEAR,ST_NAME,AC_NO)]
# merged_dataset_2 <- merge(Election_Data_2,total_votes_2,by=c("YEAR","ST_NAME","AC_NO"))
# merged_dataset_2$vote_share <- (merged_dataset_2$TOTVOTPOLL/merged_dataset_2$total_votes_2)

################################################## CALCULATING WIN MARGIN OF THE CANDIDATES ###########################################################################################################################################

Election_Data_Win <- merged_dataset
winner_vote_share <- Election_Data_Win[,.(Winner_Vote= max(vote_share)),by=.(YEAR,ST_NAME,AC_NO)]
runner_up_vote_share <- Election_Data_Win[,.(Runner_Up_Vote =max(vote_share[vote_share != max(vote_share)])),by = .(YEAR,ST_NAME,AC_NO)]
merges_dataset_3 <- merge(Election_Data_Win,runner_up_vote_share,c("YEAR","ST_NAME","AC_NO"))
merges_dataset_3 <- merge(merges_dataset_3,winner_vote_share,c("YEAR","ST_NAME","AC_NO"))
merges_dataset_3$win_margin <- (merges_dataset_3$Winner_Vote - merges_dataset_3$Runner_Up_Vote)

################################################ CALCULATING CANDIDATE NUMBER FOR ELECTIONS IN A CONSTITUENCY ######################################################################################################################################
Election_Data_Win <- merges_dataset_3
num_candidates <- Election_Data_Win[,.(num_candidates = length(CAND_NAME)),by=.(YEAR,ST_NAME,AC_NO)]
merged_dataset_4 <- merge(Election_Data_Win,num_candidates,c("YEAR","ST_NAME","AC_NO"))
Final_Election_Data <- merged_dataset_4
###################################################### FINAL  DATASET CREATED #####################################################################################################################################################################################

######################################################### PLOTTING FIGURES ###############################################################################################################################################################################################
winner_vote_share <- na.omit(winner_vote_share)
runner_up_vote_share <- na.omit(runner_up_vote_share)

Winner_Voting_Margin <- cbind(winner_vote_share,runner_up_vote_share)
Winner_Voting_Margin$win_margin <- Winner_Voting_Margin$Winner_Vote - Winner_Voting_Margin$Runner_Up_Vote


Final_Election_Data <- Winner_Voting_Margin
Final_Election_Data <-   Final_Election_Data[-c(1414,1415,1416,4819,4820,4827,4828,4831,4832,4834,4835,4838,4839,4855,4823),]
Average_Win_Margin <- Final_Election_Data[,.(Average_Win_Margin = mean(win_margin)),by=.(YEAR)]

ggplot(Average_Win_Margin, aes(x = YEAR, y =Average_Win_Margin)) +
  geom_line( color = 'darkorange1',lwd = 1.5)+  
  labs(
    x = "Years",
    y = "Average Win Margin",
    title = "Comparison of Win Margin in State Assembly Elections Over Years"
    
  ) +theme_bw()
############################################ PLOT_1 COMPLETED ######################################################################################################################################################################################
############################################ PLOT_2 STARTS #########################################################################################################################################################################################
num_candidates <- na.omit(num_candidates)
Average_Num_Candidates <- num_candidates[,.(Average_Num_Candidates = mean(num_candidates)),by=.(YEAR)]



ggplot(Average_Num_Candidates, aes(x = YEAR, y =Average_Num_Candidates)) +
  geom_line( color = 'darkorange1',lwd = 1.5)+  
  labs(
    x = "Years",
    y = "Average Number of Candidates",
    title = "Comparison of Average Number of Candidates in State Assembly Elections Over Years in India"
    ) +theme_bw()

################################################ PLOT:2 COMPLETED #################################################################################################################################

################################################ DO-FILE ENDS #####################################################################################################################################
