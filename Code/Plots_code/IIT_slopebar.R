#########################
# Make sure all datasets 
# are loaded in R session 
#########################

library(dplyr)

IIT <- c(
  "Indian Institute of Technology Bombay (IITB)" ,
  "Indian Institute of Technology Delhi (IITD)"  ,
  "Indian Institute of Technology Madras (IITM)" ,
  "Indian Institute of Technology Kharagpur (IIT-KGP)" ,
  "Indian Institute of Technology Kanpur (IITK)" ,
  "Indian Institute of Technology Roorkee (IITR)" ,
  "Indian Institute of Technology Guwahati (IITG)"
)

IIT_df <- data.frame(University = IIT)

res.my.data <- merge(IIT_df, QS_Rank_2020,by = "University")
names(res.my.data)[names(res.my.data) == 'Rank'] <- '2020'
res.my.data <- res.my.data[ ,-c(3:12)]

res.my.data <- merge(res.my.data, QS_Rank_2021,by = "University")
names(res.my.data)[names(res.my.data) == 'Rank'] <- '2021'
res.my.data <- res.my.data[ ,-c(4:13)]

res.my.data <- merge(res.my.data, QS_Rank_2022,by = "University")
names(res.my.data)[names(res.my.data) == 'Rank'] <- '2022'
res.my.data <- res.my.data[ ,-c(5:14)]

res.my.data <- merge(res.my.data, QS_Rank_2023,by = "University")
names(res.my.data)[names(res.my.data) == 'Rank'] <- '2023'
res.my.data <- res.my.data[ ,-c(6:15)]

res.my.data <-  res.my.data %>% arrange(2020)

#================================
# PLOT SLOPE BAR GRAPH FOR TOP 7
# IIT of India
#================================

### Required library
#install.packages("CGPfunctions")
#install.packages("ggthemes")
library(CGPfunctions)
library(ggthemes)

df <- res.my.data
df[1,1] <- "IITB"
df[2,1] <- "IITD"
df[3,1] <- "IITG"
df[4,1] <- "IITK"
df[5,1] <- "IIT-KGP"
df[6,1] <- "IITM"
df[7,1] <- "IITR"
Year <- as.character(c(rep(2020, 7), rep(2021, 7),rep(2022, 7), rep(2023, 7)))
University <- rep(df$University, 4)
Rank <- c(df$`2020`, df$`2021`, df$`2022`, df$`2023`)

sg_df <- data.frame(Year, University, Rank)

sg_df$Rank <- as.numeric(sg_df$Rank)

newggslopegraph(dataframe = sg_df, 
                Times = Year,
                Measurement = Rank,
                Title = "QS ranking of the top 7 IIT over last 4 years",
                SubTitle = "2020 - 2023",
                Grouping = University, 
                ReverseYAxis = T, 
                YTextSize = 5,
                WiderLabels = T,ThemeChoice = "bw",
                DataTextSize = 5,
                Caption = "")
