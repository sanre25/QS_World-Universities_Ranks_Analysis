

#########################
# Make sure all datasets 
# are loaded in R session 
#########################

library(ggplot2)
library(dplyr)

## Donut for year 2020

Continent_2020 <- QS_Rank_2020$Continent
cdf <- sort(table(Continent_2020), F) %>% as.data.frame()

cdf <- cdf %>%
  arrange(desc(Continent_2020)) %>%
  mutate(lab.ypos = cumsum(Freq) - 0.5*Freq)

ggplot(cdf, aes(x = 2, y = Freq, fill = Continent_2020)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  geom_text(aes(y = lab.ypos, label = Freq), color = "white") +
  guides(fill = guide_legend("Continents")) +
  ggtitle("Donut chart showing the respective number of universities \n for different continents in 2020") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlim(0.5, 2.5)

## Donut for year 2021

Continent_2021 <- QS_Rank_2021$Continent
cdf <- sort(table(Continent_2021), F) %>% as.data.frame()

cdf <- cdf %>%
  arrange(desc(Continent_2021)) %>%
  mutate(lab.ypos = cumsum(Freq) - 0.5*Freq)

ggplot(cdf, aes(x = 2, y = Freq, fill = Continent_2021)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  geom_text(aes(y = lab.ypos, label = Freq), color = "white") +
  guides(fill = guide_legend("Continents")) +
  ggtitle("Donut chart showing the respective number \n of universities for different continents in 2021") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlim(0.5, 2.5)


## Donut for year 2022

Continent_2022 <- QS_Rank_2022$Continent
cdf <- sort(table(Continent_2022), F) %>% as.data.frame()

cdf <- cdf %>%
  arrange(desc(Continent_2022)) %>%
  mutate(lab.ypos = cumsum(Freq) - 0.5*Freq)

ggplot(cdf, aes(x = 2, y = Freq, fill = Continent_2022)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  geom_text(aes(y = lab.ypos, label = Freq), color = "white") +
  guides(fill = guide_legend("Continents")) +
  ggtitle("Donut chart showing the respective number \n of universities for different continents in 2022") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlim(0.5, 2.5)



## Donut for year 2023

Continent_2023 <- QS_Rank_2023$Continent
cdf <- sort(table(Continent_2023), F) %>% as.data.frame()

cdf <- cdf %>%
  arrange(desc(Continent_2023)) %>%
  mutate(lab.ypos = cumsum(Freq) - 0.5*Freq)

ggplot(cdf, aes(x = 2, y = Freq, fill = Continent_2023)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  geom_text(aes(y = lab.ypos, label = Freq), color = "white") +
  guides(fill = guide_legend("Continents")) +
  ggtitle("Donut chart showing the respective number \n of universities for different continents in 2023") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlim(0.5, 2.5)

