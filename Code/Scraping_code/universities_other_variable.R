
library(httr)
library(jsonlite)
library(dplyr)

## Assuming httr,dplyr,jsonlite library in working
get_data <- function(link)
{
  response <- GET(link)
  data_json <- content(response,encoding="UTF-8")
  
  data <- fromJSON(data_json)
  
  df2 <- data.frame(data[2])
  
  #1 Academic Reputation
  Acad_repu <- array(NA,dim=nrow(df2))
  for(i in 1:nrow(df2)){
    str <- df2$data.ind_76[i]
    bstr <- str_split_fixed(str, pattern="td-wrap-in", n=2)
    Acad_repu[i]=gsub("\">||</div></div>||", "",bstr[1,2])
  }
  Acad_repu <- as.numeric(str_trim(Acad_repu))
  
  #2 Employer Reputation
  Emply_repu <- array(NA,dim=nrow(df2))
  for(i in 1:nrow(df2)){
    str <- df2$data.ind_77[i]
    bstr <- str_split_fixed(str, pattern="td-wrap-in", n=2)
    Emply_repu[i]=gsub("\">||</div></div>||", "",bstr[1,2])
  }
  Emply_repu<-as.numeric(str_trim(Emply_repu))
  
  #3 Faculty student ratio
  fac_stu.ratio <- array(NA,dim=nrow(df2))
  for(i in 1:nrow(df2)){
    str <- df2$data.ind_36[i]
    bstr <- str_split_fixed(str, pattern="td-wrap-in", n=2)
    fac_stu.ratio[i]=gsub("\">||</div></div>||", "",bstr[1,2])
  }
  fac_stu.ratio<-as.numeric(str_trim(fac_stu.ratio))
  
  #4 Citation per faculty
  citation <- array(NA,dim=nrow(df2))
  for(i in 1:nrow(df2)){
    str <- df2$data.ind_73[i]
    bstr <- str_split_fixed(str, pattern="td-wrap-in", n=2)
    citation [i]=gsub("\">||</div></div>||", "",bstr[1,2])
  }
  citation <-as.numeric(str_trim(citation))
  
  #5 International faculty ratio
  int.fac.ratio <- array(NA,dim=nrow(df2))
  for(i in 1:nrow(df2)){
    str <- df2$data.ind_18[i]
    bstr <- str_split_fixed(str, pattern="td-wrap-in", n=2)
    int.fac.ratio[i]=gsub("\">||</div></div>||", "",bstr[1,2])
  }
  int.fac.ratio <-as.numeric(str_trim(int.fac.ratio))
  
  #6 International student ratio
  int.stu.ratio <- array(NA,dim=nrow(df2))
  for(i in 1:nrow(df2)){
    str <- df2$data.ind_14[i]
    bstr <- str_split_fixed(str, pattern="td-wrap-in", n=2)
    int.stu.ratio[i]=gsub("\">||</div></div>||", "",bstr[1,2])
  }
  int.stu.ratio <-as.numeric(str_trim(int.stu.ratio))
  
  res_df <- data.frame(
    Acad_repu=Acad_repu,
    Emply_repu=Emply_repu,
    fac_stu.ratio=fac_stu.ratio,
    citation=citation,
    int.fac.ratio=int.fac.ratio,
    int.stu.ratio=int.stu.ratio
  )
  
  colnames(res_df) <- c("Academic.Reputation","Employer.Reputation",
                       "Faculty.Student.Ratio","Citations.per.Faculty",
                       "International.Faculty.Ratio","International.Students.Ratio")
  return(res_df)
}

link20 <-"https://www.topuniversities.com/sites/default/files/qs-rankings-data/en/914824_indicators.txt?rk1fb0"
link21 <-"https://www.topuniversities.com/sites/default/files/qs-rankings-data/en/2057712_indicators.txt?rk1fb0"
link22 <-"https://www.topuniversities.com/sites/default/files/qs-rankings-data/en/3740566_indicators.txt?rk1fb0"
link23 <-"https://www.topuniversities.com/sites/default/files/qs-rankings-data/en/3816281_indicators.txt?rk1fb0"

df_20 <- get_data(link20)
df_21 <- get_data(link21)
df_22 <- get_data(link22)
df_23 <- get_data(link23)
