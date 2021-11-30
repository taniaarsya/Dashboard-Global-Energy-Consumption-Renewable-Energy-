#library setup

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(tidyverse) 
library(plotly) 
library(glue) 
library(scales) 
library(ggpubr)
library(gt)
library(DT)
library(shinythemes)
library(googleVis)

# read data
clean <-read.csv("clean11.csv")
cont_consump<-read.csv("Continent_Consumption_TWH.csv")
country_consump<-read.csv("Country_Consumption_TWH.csv")
nonre_eng<-read.csv("nonRenewablesTotalPowerGeneration.csv")
re_eng_gen_90_17<-read.csv("renewablePowerGeneration97-17.csv")
re_eng_tot_gen<-read.csv("renewablesTotalPowerGeneration.csv")
re_eng_countries<-read.csv("top20CountriesPowerGeneration.csv")

# preprocessing data

clean$X.1=NULL

str(country_consump)
unique(country_consump$Year)
sum(is.na(country_consump))
which(is.na(country_consump),arr.ind = TRUE)
country_consump<- na.omit(country_consump)
country_consump<-rename(country_consump,"Saudi Arabia" = "Saudi.Arabia")
country_consump<-rename(country_consump,"United Kingdom" = "United.Kingdom")
country_consump<-rename(country_consump,"United States" = "United.States")
country_consump<-rename(country_consump,"New Zealand" = "New.Zealand")
country_consump<-rename(country_consump,"South Africa" = "South.Africa")
country_consump<-rename(country_consump,"South Korea" = "South.Korea")
country_consump<-rename(country_consump,"United Arab Emirates" = "United.Arab.Emirates")

str(nonre_eng)
unique(nonre_eng$Mode.of.Generation)
sum(is.na(nonre_eng))

str(re_eng_gen_90_17)
unique(re_eng_gen_90_17$Year)
sum(is.na(re_eng_gen_90_17))

str(re_eng_tot_gen)
sum(is.na(re_eng_tot_gen))
unique(re_eng_tot_gen$Mode.of.Generation)

str(re_eng_countries)
unique(re_eng_countries$Country)
sum(is.na(country_consump))


nonre_eng$energy_type <- "non renewable"
re_eng_tot_gen$energy_type <- "renewable"

country<-c("Australia","Austria","Belgium","Canada","Colombia","Chile","Czech Republic","Denmark","Estonia","Finland","France","Germany","Greece","Hungary","Iceland","Ireland","Israel",
           "Italy","Japan","Korea","Luxembourg","Latvia","Lithuania","Mexico","Netherlands","New Zealand","Norway","Poland","Portugal","Slovak Republic","Slovenia","Spain","Sweden","Switzerland","Turkey","United Kingdom","United States")
oecd_countries_90_20 <-data.frame(country)

oecd_countries_90_20 <- merge(gather(country_consump,country,Twh,China:"United Arab Emirates"),oecd_countries_90_20) %>% 
  arrange(country,Year)

fit <- prcomp(x = country_consump, 
              center = TRUE, 
              scale = TRUE)
