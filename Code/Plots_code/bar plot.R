
#########################
# Make sure all datasets 
# are loaded in R session 
#########################


####===============================================
## BAR diagram for the each year of Top 10 counrtry 
####===============================================
library(ggplot2)
library(dplyr)

###++++++++++++++++++++++++++++++++++++++++++++++++
##2020
feq <- table(QS_Rank_2020$Country)
country.data <- data.frame(feq)
country.data = country.data%>%arrange(desc(Freq))

# Draw bar plot
theme_set(theme_bw())
ggplot(country.data[1:10,], aes(x=reorder(Var1, -Freq), y=Freq), xlab = "Countries", ylab = "No of Universities") + 
  geom_bar(stat="identity", width=.5, fill="red") +  xlab("Universities") + ylab("No. of universities") +
  labs(title="Ordered Bar Chart", 
       subtitle="Country vs No. of university ", 
       caption="") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))
###+++++++++++++++++++++++++++++++++++++++++++++++++


###+++++++++++++++++++++++++++++++++++++++++++++++++
##2021
feq <- table(QS_Rank_2021$Country)
country.data <- data.frame(feq)
country.data = country.data%>%arrange(desc(Freq))

# Draw bar plot
theme_set(theme_bw())
ggplot(country.data[1:10,], aes(x=reorder(Var1, -Freq), y=Freq), xlab = "Countries", ylab = "No of Universities") + 
  geom_bar(stat="identity", width=.5, fill="green") +  xlab("Universities") + ylab("No. of universities") +
  labs(title="Ordered Bar Chart", 
       subtitle="Country vs No. of university ", 
       caption="") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))
###+++++++++++++++++++++++++++++++++++++++++++++++++


###+++++++++++++++++++++++++++++++++++++++++++++++++
##2022
feq <- table(QS_Rank_2022$Country)
country.data <- data.frame(feq)
country.data = country.data%>%arrange(desc(Freq))

# Draw bar plot
theme_set(theme_bw())
ggplot(country.data[1:10,], aes(x=reorder(Var1, -Freq), y=Freq), xlab = "Countries", ylab = "No of Universities") + 
  geom_bar(stat="identity", width=.5, fill="blue") +  xlab("Universities") + ylab("No. of universities") +
  labs(title="Ordered Bar Chart", 
       subtitle="Country vs No. of university ", 
       caption="") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))
###+++++++++++++++++++++++++++++++++++++++++++++++++


###+++++++++++++++++++++++++++++++++++++++++++++++++
##2023
feq <- table(QS_Rank_2021$Country)
country.data <- data.frame(feq)
country.data = country.data%>%arrange(desc(Freq))

# Draw bar plot
theme_set(theme_bw())
ggplot(country.data[1:10,], aes(x=reorder(Var1, -Freq), y=Freq), xlab = "Countries", ylab = "No of Universities") + 
  geom_bar(stat="identity", width=.5, fill="orange") + xlab("Universities") + ylab("No. of universities") +
  labs(title="Ordered Bar Chart", 
       subtitle="Country vs No. of university ", 
       caption="") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))
###+++++++++++++++++++++++++++++++++++++++++++++++++
