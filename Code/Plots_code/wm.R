library(ggmap)
library(maptools)
library(maps)


City_Qs = City
Country_Qs = Country

Country_Qs = replace(Country_Qs , which(Country_Qs == "China (Mainland)") , "China")
City_Qs = replace(City_Qs , which(City_Qs == "") , NA)

d_Qs = data.frame(City1 = City_Qs ,Country1 =  Country_Qs)

d_Qs<- d_Qs[-which(is.na(City_Qs)), ]


#=========================================

loc = read.csv("C:/Users/Rishiraj/Downloads/worldcities.csv")
attach(loc)

loc_d = data.frame(City1 = city_ascii , Country1 = country , latitute = lat , lngt = lng)


data_duplicated <- loc_d[!duplicated(loc_d[ , c("City1", "Country1")]), ]



#===========================================

d = merge(data_duplicated,d_Qs,by=c("City1" , "Country1"),all.y=T)
dim(d)
d = d[-which(is.na(d$latitute)), ]


#===========================================

#===========================================

map("world", fill=TRUE, col="white", bg="lightblue", ylim=c(-60, 90), mar=c(0,0,0,0))
points(d$lngt,d$latitute, col="red", pch=16)

#===========================================


mp <- NULL
mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
mp <- ggplot() +   mapWorld

#Now Layer the cities on top
mp <- mp+ geom_point(aes(x=d$lngt, y=d$latitute) ,color="blue", size=1) 
mp
View(Univ_rank)
attach(Univ_rank)

#=======================================

