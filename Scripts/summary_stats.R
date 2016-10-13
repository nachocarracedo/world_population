#Describe each variable, including the units of the numeric varables. Describe the meaning of the observations of the dataset.
#Include summaries of individual variables using only the group_by and summarize functions
#from the dplyr library. You must use the pipe operator from the magrittr library.
#Each summary should be interpreted in a way that informs the reader of important
#information about that variable or those variables in the context of the data set.


library(WDI)
library(readr)
library(dplyr)
library(tidyr)

# Read dataframe of wdi.df and drop index column
setwd("C:/Users/carrai1/Desktop/Master/MA402_Data_Science/Assigment_1/")
wdi.df = read_csv("./data/wdi.df.csv")
drops='X1'
wdi.df = wdi.df[ , !(names(wdi.df) %in% drops)]
# check data frame
str(wdi.df)
head(wdi.df)

#check missing values
# check missing values again
wdi.df %>%  group_by(year) %>%  summarize(count=n(), cnt.missing=sum(is.na(value)), pct.missing= 100*cnt.missing/count) %>% 
  arrange(year) %>%  select(year, pct.missing) %>%   print(n=100)
# same for countries
wdi.df %>%  group_by(country.code) %>%  summarize(count=n(), cnt.missing=sum(is.na(value)), pct.missing= 100*cnt.missing/count) %>% 
  arrange(country.code) %>%  select(country.code, pct.missing) %>%  arrange(desc(pct.missing)) %>% print(n=250)


# check unique years, countries, and indicators
wdi.df %>% summarise(number_of_unique_years = n_distinct(year))
wdi.df %>% summarise(number_of_unique_ind = n_distinct(indicator.code))
wdi.df %>% summarise(number_of_unique_countries = n_distinct(country.code))

options(scipen=999) # disable scientific notation. Easiear to read
# get values summary for every indicator (mean, median, min, max, sd)
wdi.df %>% group_by(indicator.name, indicator.code) %>% summarise(mean=round(mean(value), 2), median=round(median(value), 2), sd=round(sd(value), 2),
                                                                  max=round(max(value), 2),min=round(min(value), 2))  

# get values summary for every year/indicator (mean, median, min, max, sd)
wdi.df %>% group_by(year,indicator.code ) %>% summarise(mean=round(mean(value), 2), median=round(median(value), 2), sd=round(sd(value), 2),
                                                                  max=round(max(value), 2),min=round(min(value), 2))  -> summary.year.code.df
for (y in unique(summary.year.code.df$year)){summary.year.code.df %>% filter(year == y) %>% print()}

# get values summary for every country/indicator (mean, median, min, max, sd)
wdi.df %>% group_by(country.name,indicator.code) %>% summarise(mean=round(mean(value), 2), median=round(median(value), 2), sd=round(sd(value), 2),
                                                                  max=round(max(value), 2),min=round(min(value), 2)) -> summary.country.code.df 
for (c in unique(summary.country.code.df$country.name)){summary.country.code.df %>% filter(country.name == c) %>% print()}

