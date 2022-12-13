rm(list = ls())

library(httr)
library(jsonlite)
library(dplyr)
library(stringr)

links <- c("https://www.topuniversities.com/sites/default/files/qs-rankings-data/en/914824.txt?rk1fb0", 
           "https://www.topuniversities.com/sites/default/files/qs-rankings-data/en/2057712.txt?rk1fb0",
           "https://www.topuniversities.com/sites/default/files/qs-rankings-data/en/3740566.txt?rk1fb0",
           "https://www.topuniversities.com/sites/default/files/qs-rankings-data/en/3816281.txt?rk1fb0"           )


### Function extracting the Rank, University name, score, city, country and the continent.
cleaning <- function(x){
  response <- GET(x)
  data_json <- content(response,encoding="UTF-8")
  data <- jsonlite::fromJSON(data_json)
  df <- data.frame(data)
  
  ##---->>> extracting university names
  University <- array(NA, dim = nrow(df))
  for(i in 1:nrow(df)){
    str<- df$data.title[i]
    bstr <- str_split_fixed(str, pattern = "uni-link", n = 2)
    University[i] <- gsub("\">||</a></div>||", "", bstr[1, 2])
  }
  
  
  df <- df[,c(2, 3, 8, 9, 10)]
  colnames(df) <- c("Country", "City", "Score", "Rank", "Continent")
  df <- data.frame(df, University)
  df <- df[, c(4, 6, 3, 2, 1, 5)]
  return(df)
}

cleaning_2022 <- function(x){
  response <- GET(x)
  data_json <- content(response,encoding="UTF-8")
  data <- jsonlite::fromJSON(data_json)
  df <- data.frame(data)
  
  ##---->>> extracting university names
  University <- array(NA, dim = nrow(df))
  for(i in 1:nrow(df)){
    str<- df$data.title[i]
    bstr <- str_split_fixed(str, pattern = "uni-link", n = 2)
    University[i] <- gsub("\">||</a></div>||", "", bstr[1, 2])
  }
  
  
  df <- df[,c(2, 3, 8, 9, 11)]
  colnames(df) <- c("Country", "City", "Score", "Rank", "Continent")
  df <- data.frame(df, University)
  df <- df[, c(4, 6, 3, 2, 1, 5)]
  return(df)
}

uni_rank_2020 <- cleaning(links[1])
uni_rank_2021 <- cleaning(links[2])
uni_rank_2022 <- cleaning_2022(links[3])
uni_rank_2023 <- cleaning(links[4])


