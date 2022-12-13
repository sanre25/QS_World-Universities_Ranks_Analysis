#########################
# Make sure all datasets 
# are loaded in R session 
#########################


library(dplyr)

##   ***Top 10 universities of Year 2020***
my.data <- data.frame(
    Rank20 = QS_Rank_2020$Rank[1:10],
    University    = QS_Rank_2020$University[1:10]
)


## ***res.my.data is result DF after merging with sunsequent DF's***
## *** Merge with 2021 ***
res.my.data <- merge(my.data, QS_Rank_2021,by = "University")
names(res.my.data)[names(res.my.data) == 'Rank'] <- 'Rank21'
res.my.data <- res.my.data[ ,-c(4:13)]


## ***College ETH zurich have different name in 2022 or 2023 Ranking***
QS_Rank_2022[8,2] <- "ETH Zurich - Swiss Federal Institute of Technology"
QS_Rank_2023[9,2] <- "ETH Zurich - Swiss Federal Institute of Technology"


## *** Merge with 2022 ***
res.my.data <- merge(res.my.data, QS_Rank_2022,by = "University")
names(res.my.data)[names(res.my.data) == 'Rank'] <- 'Rank22'
res.my.data <- res.my.data[ ,-c(5:14)]


## *** Merge with 2023 ***
res.my.data <- merge(res.my.data, QS_Rank_2023,by = "University")
names(res.my.data)[names(res.my.data) == 'Rank'] <- 'Rank23'
res.my.data <- res.my.data[ ,-c(6:15)]


## *** Converting Ranks into numeric ***
res.my.data$Rank20 = as.numeric(res.my.data$Rank20)
res.my.data$Rank21 = as.numeric(res.my.data$Rank21)
res.my.data$Rank22 = as.numeric(res.my.data$Rank22)
res.my.data$Rank23 = as.numeric(res.my.data$Rank23)


res.my.data <-  res.my.data %>% arrange(Rank20)

#================================
# PLOT SLOPE BAR GRAPH FOR TOP 10
# UNIVERSITIES 
#================================

### Required library
#install.packages("CGPfunctions")
#install.packages("ggthemes")
library(CGPfunctions)
library(ggthemes)

df <- res.my.data

colnames(df) <- c("University","2020", "2021", "2022" ,"2023")

df[1,1] <- "MIT"
df[5,1] <- "CalTech"
df[6,1] <- "ETH Zurich"

Year <- as.character(c(rep(2020, 10), rep(2021, 10),rep(2022, 10), rep(2023, 10)))
University <- rep(df$University, 4)
Rank <- c(df$`2020`, df$`2021`, df$`2022`, df$`2023`)

## sg_df is required DF for ploting graph
sg_df <- data.frame(Year, University, Rank)


head(sg_df)

### ** code 
newggslopegraph(dataframe = sg_df, 
                Times = Year,
                Measurement = Rank,
                Title = "QS ranking of the top 10 universities over last 4 years",
                SubTitle = "2020 - 2023",
                Grouping = University, 
                ReverseYAxis = T, 
                YTextSize = 5,
                WiderLabels = T,ThemeChoice = "bw",
                DataTextSize = 5,
                Caption = "")










