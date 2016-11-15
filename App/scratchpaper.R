library(shinydashboard)
library(wbstats)
library(magrittr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(rworldmap)

intro = "Write intro here"
topics="This is a test Write topics here 
and here and here, etc"
objective = "This is a test Write objective here 
and here and here, etc"


tab1explanation = "Select a country and an indicator to see a summary of the indicator for that country. Also, select an indicator to check its histogram, you can also select the number of bins of the histogram."
tab2explanation = "Select a pair of indicators to see a scatter plot of both, this will show how they relate. A correlation heatmap of all indicators it's also shown"
#tab3explanation

setwd("C:/Users/carrai1/Desktop/Master/MA402_Data_Science/assigment_1/")
wb.df = read.csv("./data/wdi.df.csv")
indicator.codes = c("pop.density", "energy.use", "co2.emission", "GDP", "GDP.growth","pop.0.14", "pop.15.64", "pop.growth", "pop.total" , "pop.rural" ,"pop.urban")
dots <- lapply(indicator.codes, as.symbol)
#indicator.codes2 = c(pop.density, energy.use, co2.emission, GDP, GDP.growth,pop.0.14, pop.15.64, pop.growth, pop.total, pop.rural ,pop.urban)

correlation <- round(cor(select(wb.df, one_of(indicator.codes))), 3)
# Store the sorted list of country country codes
wb.df %>% rename(. , country = country.code) -> wb.df
#wb.df$country <- wb.df$country
country.codes = sort(unique(wb.df$country))

# Create column country.names with countries complete names
mapdf <- data.frame(old=c("BRA", "CHN", "IND", "RUS", "ZAF"),new=c("Brazil", "China", "India", "Russia", "South Africa"))
wb.df$country.names <- mapdf$new[match(wb.df$country,mapdf$old)]


country.names = sort(unique(wb.df$country.names))

wb.df %>%
  group_by(.,country) %>%
  mutate_(chosen_ic="co2.emission") %>%
  summarize(avg=mean(chosen_ic, na.rm=TRUE)) %>% 
  plot_ly(.,
          type         ="choropleth", 
          locationmode ='ISO-3',
          locations    =~country.names,
          text         =~country.names, 
          z            =~avg) 


wb.df %>%
  group_by(.,country) %>%
  mutate_(chosen_ic="co2.emission") %>%
  summarize(avg=mean(chosen_ic, na.rm=TRUE)) -> aa

#aa$country <- sapply(aa$country, as.character)
#l <- list(color = toRGB("grey"), width = 0.5)

plot_ly(aa,
        type="choropleth",
        locationmode ='ISO-3',
        locations=~country,
        text =~country,
        z=~avg,
        marker = list(line = l)) %>%  layout(geo = list(scope="world"))
        

# light grey boundaries
l <- list(color = toRGB("grey"), width = 0.5)

g <- list(
  showframe = FALSE,
  showcoastlines = TRUE,
  projection = list(type = 'Mercator')
)

plot_geo(aa, locationmode ='ISO-3') %>%
  add_trace(
    z = ~avg, color = ~country, colors = 'Blues',
    text = ~country, locations = ~country, marker = list(line = l)
  )  %>%
  layout(
    title = 'World map - Indicator',
    geo = g
  )




















