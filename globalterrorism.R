install.packages("gridExtra")
install.packages("ggmap")
install.packages("rworldmap")
install.packages("ggpubr")
install.packages("caret")
install.packages("forecast")
install.packages("ggfortify")
install.packages("arules")
install.packages("arulesViz")

library(ggplot2)
library(corrplot)
library(tidymodels)
library(tidyverse)
library(cluster) 
library(fpc)
library(gridExtra)
library(readr)
library(ggmap)
library(rworldmap)
library(ggpubr)
library(car)
library(caret)
library(forecast)
library(zoo)
library(ggfortify)
library(arules)
library(arulesViz)

## Terror attacks over the years

globalterror <- read_csv('globalterrorismdb_0718dist.csv')

gterror <- globalterror %>%
  select(iyear, imonth, iday, country_txt, region_txt, city, latitude, longitude, summary, multiple, attacktype1_txt, targtype1_txt, targsubtype1_txt, gname, weaptype1_txt, nkill, nwound, nkillter)

gterror <- gterror %>%
  rename(year = iyear, month = imonth, day = iday, country = country_txt, region = region_txt, multiple_attack = multiple, attacktype = attacktype1_txt, target_type = targtype1_txt, target_sub_type = targsubtype1_txt, group_name = gname, weapon_type = weaptype1_txt)

gterror <- gterror %>%
  mutate(decade = 
           ifelse(year<1980, '70s', 
                  ifelse(year < 1990, '80s', 
                         ifelse(year < 2000, '90s', 
                                ifelse( year < 2010, '2000s', '2010s')))))
gterror$decade <- factor(gterror$decade, levels = c("70s", "80s", "90s", "2000s", "2010s"))

ggplot(data = gterror, aes(year))+
  geom_histogram(stat = 'count')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title = 'Terrorism over the decades')

## Terrorism hotspots

world1 <- borders("world", colour = "gray50", fill = "gray50")
worldmap1<- ggplot() + world1 + scale_y_continuous(limits = c(-55,90))

gterror2000<- gterror %>%
  filter(year>2007)

worldmap1 +
  geom_point(aes(x=gterror2000$longitude[gterror2000$nkill<51], y=gterror2000$latitude[gterror2000$nkill<51]), col='blue', alpha= 0.2)+
  geom_point(aes(x=gterror2000$longitude[gterror2000$nkill>50], y=gterror2000$latitude[gterror2000$nkill>50]), col='red', size=2)+
  labs(title = 'Terrorism hotspots in the last 10 years')

## Most number of terror attacks by region, country and city

gterror %>%
  group_by(region) %>%
  summarise( nr_of_attacks = n()) %>%
  arrange(desc(nr_of_attacks)) %>%
  head(n=10)

gterror %>%
  group_by(country) %>%
  summarise( nr_of_attacks = n()) %>%
  arrange(desc(nr_of_attacks)) %>%
  head(n=10)

gterror %>%
  filter(city != 'Unknown') %>%
  group_by(city) %>%
  summarise( nr_of_attacks = n()) %>%
  arrange(desc(nr_of_attacks)) %>%
  head(n=10)

## Security and insights

## Type of terrorism and increase over the years
## Terrorism targets over the years

weaponl <- gterror %>%
  filter(weapon_type != "Unknown") %>%
  select(decade, weapon_type, nkill)%>%
  group_by(decade,weapon_type)%>%
  summarise(nr_of_deaths = n())%>%
  top_n(n=5, wt=nr_of_deaths) %>%
  mutate(percent_deaths = (nr_of_deaths/sum(nr_of_deaths)*100))

#Visual by decade / weapon type

ggplot(data=weaponl, aes(x=decade, y=nr_of_deaths, col=weapon_type, group= weapon_type)) +
  geom_line(size=1.5, alpha=0.5) + 
  labs(title='Terrorism lethality by weapon over time')

#Target type over the decades

gterrortargets <- gterror %>%
  select(year, target_type, target_sub_type, decade) %>%
  filter(target_type != "Unknown")

targetstop <- gterrortargets %>%
  group_by(decade, target_type) %>%
  summarise(nr_of_attacks = n()) %>%
  top_n(n=5, wt=nr_of_attacks) %>%
  arrange(decade, desc(nr_of_attacks))

ggplot()+
  geom_line(data = targetstop, aes(decade,nr_of_attacks, color = target_type, group= target_type), size= 1.5)+
  labs(title = 'Terror')
