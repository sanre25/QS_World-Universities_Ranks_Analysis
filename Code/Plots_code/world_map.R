
#########################
# Make sure all datasets 
# are loaded in R session 
#########################

library(shiny)
library(fmsb)
library(dplyr)
library(ggplot2)
library(shinythemes)
library(rworldmap)
library(geosphere)
library(gpclib)
library(countrycode)
library(RColorBrewer)
library(CGPfunctions)
library(ggthemes)
library(viridis)
library(animation)



#########################
# World map for 2020
#########################
worldMap <- getMap()
world.points <- fortify(worldMap)
world.points$region <- world.points$id

world.df <- world.points[,c("long","lat","group", "region")]

new_df_1 <- world.df %>% 
  mutate(Iso3_1 = countrycode::countrycode(
    sourcevar = region,
    origin = "country.name",
    destination = "iso3c"
  )) 

new_df_2 <- lat_long_cont_gdp_2020 %>% 
  mutate(Iso3_2 = countrycode::countrycode(
    sourcevar = Country,
    origin = "country.name",
    destination = "iso3c"
  )) 


new_map_df <- new_df_1 %>% 
  select(long, lat, group,Iso3_1) %>% 
  left_join(new_df_2, by = c("Iso3_1" = "Iso3_2")) 


p1 <- ggplot(new_map_df, mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(aes(fill = GDPPC)) +
  scale_fill_viridis_c(direction = -1, limits = c(0, 100000), oob = scales::squish) +
  geom_path(aes(x = long, y = lat, group = group)) +
  scale_color_brewer(palette="Set1") +
  geom_point(aes(x = Longitude, y = Latitude, color = Continent)) +
  #coord_map("ortho", orientation = c(10, 75, 0)) +
  xlab("Longitude")  +
  ylab("Latitude") +
  labs(title = "GDP per capita of countries overlayed with location of \nthe Universties for year 2020") +
  theme_minimal();p1



#########################
# World map for 2021
#########################
worldMap <- getMap()
world.points <- fortify(worldMap)
world.points$region <- world.points$id

world.df <- world.points[,c("long","lat","group", "region")]

new_df_1 <- world.df %>% 
  mutate(Iso3_1 = countrycode::countrycode(
    sourcevar = region,
    origin = "country.name",
    destination = "iso3c"
  )) 

new_df_2 <- lat_long_cont_gdp_2021 %>% 
  mutate(Iso3_2 = countrycode::countrycode(
    sourcevar = Country,
    origin = "country.name",
    destination = "iso3c"
  )) 


new_map_df <- new_df_1 %>% 
  select(long, lat, group,Iso3_1) %>% 
  left_join(new_df_2, by = c("Iso3_1" = "Iso3_2")) 


p1 <- ggplot(new_map_df, mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(aes(fill = GDPPC)) +
  scale_fill_viridis_c(direction = -1, limits = c(0, 100000), oob = scales::squish) +
  geom_path(aes(x = long, y = lat, group = group)) +
  scale_color_brewer(palette="Set1") +
  geom_point(aes(x = Longitude, y = Latitude, color = Continent)) +
  #coord_map("ortho", orientation = c(10, 75, 0)) +
  xlab("Longitude")  +
  ylab("Latitude") +
  labs(title = "GDP per capita of countries overlayed with location of \nthe Universties for year 2021") +
  theme_minimal();p1



#########################
# World map for 2022
#########################
worldMap <- getMap()
world.points <- fortify(worldMap)
world.points$region <- world.points$id

world.df <- world.points[,c("long","lat","group", "region")]

new_df_1 <- world.df %>% 
  mutate(Iso3_1 = countrycode::countrycode(
    sourcevar = region,
    origin = "country.name",
    destination = "iso3c"
  )) 

new_df_2 <- lat_long_cont_gdp_2022 %>% 
  mutate(Iso3_2 = countrycode::countrycode(
    sourcevar = Country,
    origin = "country.name",
    destination = "iso3c"
  )) 


new_map_df <- new_df_1 %>% 
  select(long, lat, group,Iso3_1) %>% 
  left_join(new_df_2, by = c("Iso3_1" = "Iso3_2")) 


p1 <- ggplot(new_map_df, mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(aes(fill = GDPPC)) +
  scale_fill_viridis_c(direction = -1, limits = c(0, 100000), oob = scales::squish) +
  geom_path(aes(x = long, y = lat, group = group)) +
  scale_color_brewer(palette="Set1") +
  geom_point(aes(x = Longitude, y = Latitude, color = Continent)) +
  #coord_map("ortho", orientation = c(10, 75, 0)) +
  xlab("Longitude")  +
  ylab("Latitude") +
  labs(title = "GDP per capita of countries overlaid with location of \nthe Universties for year 2022") +
  theme_minimal();p1


#########################
# Globe GIF for 2022
#########################

rotate_globe <- function(angle){
 ggplot(new_map_df, mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(aes(fill = GDPPC)) +
  scale_fill_viridis_c(direction = -1, limits = c(0, 100000), oob = scales::squish) +
  geom_path(aes(x = long, y = lat, group = group)) +
  scale_color_brewer(palette="Set1") +
  geom_point(aes(x = Longitude, y = Latitude, color = Continent)) +
  coord_map("ortho", orientation = c(10, angle, 0)) +
  xlab("Longitude")  +
  ylab("Latitude") +
  labs(title = "GDP per capita of countries overlaid with location of \nthe Universties for year 2022") +
  theme_minimal() 
}


saveGIF({
  ani.options(nmax = 360)
  for(i in seq(0,360)){
    print(rotate_globe(i))
  }
}, interval = 0.1, outdir="D:/R_IITK/SEM_1/R_IITK_SEM_1", movie.name = "globe.gif") ## use your target folder location in outdir.
