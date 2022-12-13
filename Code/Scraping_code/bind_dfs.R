## Code for combining data frames

#================*********==============================
# QS_ranks_2020.csv file is downloaded from project repo
#================*********==============================
main.df.20 <- read.csv("QS_ranks_2020.csv")
main.df.21 <- read.csv("QS_ranks_2021.csv")
main.df.22 <- read.csv("QS_ranks_2022.csv")
main.df.23 <- read.csv("QS_ranks_2023.csv")

#===============*********=================
# df_20 types data frames are outputs of
# universities_other_variable.R file
#===============*********=================
QS_Rank_2020 <- cbind(main.df.20,df_20)
QS_Rank_2021 <- cbind(main.df.21,df_21)
QS_Rank_2022 <- cbind(main.df.22,df_22)
QS_Rank_2023 <- cbind(main.df.23,df_23)

#==============********==================
# Save resultant data frames as Rdata files
#==============********==================
save(QS_Rank_2020,"QS_Ranks_2020.Rdata")
save(QS_Rank_2021,"QS_Ranks_2021.Rdata")
save(QS_Rank_2022,"QS_Ranks_2022.Rdata")
save(QS_Rank_2023,"QS_Ranks_2023.Rdata")
