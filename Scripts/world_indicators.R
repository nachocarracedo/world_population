# World Development Indicators
#
# This script loads csv downloaded from ://data.worldbank.org/topic
# makes years as a feature and concatenates them together into one data.frame
# then selects a good combination of years/countries without missing values

# Load the WDI, readr and dplyr libraries
library(WDI)
library(readr)
library(dplyr)
library(tidyr)
# disable scientific notation. Easiear to read
options(scipen=999)
# Read each one fo the indicators into a data frame from the downloaded CSV files (remove manuall first 3 lines of cvs)
setwd("C:/Users/carrai1/Desktop/Master/MA402_Data_Science/Assigment_1/")
pop0.df = read_csv("./data/%_population_0_14_60_15.csv")
pop15.df = read_csv("./data/%_population_15_64_60_15.csv")
poprur.df= read_csv("./data/%_rural_population_60_15.csv")
popur.df= read_csv("./data/%_urban_population_60_15.csv")
co2.df= read_csv("./data/CO2_emisions_60_11.csv")
kmsqr.df= read_csv("./data/km_square_61_15.csv")
popgrowth.df= read_csv("./data/population_growth_60_15.csv")
poptotal.df= read_csv("./data/population_total_60_15.csv")
energy.df= read_csv("./data/energy_use_kg_oil.csv")
gdp_growth.df= read_csv("./data/GDP_growth_annual%.csv")
gdp.df= read_csv("./data/GDP.csv")



# put all data.frames into a list
all.list <- list(pop0.df,pop15.df,poprur.df,popur.df,co2.df,kmsqr.df,
                 popgrowth.df,poptotal.df,energy.df,gdp_growth.df,gdp.df)


# Join all data.frames into a single data.frame
wdi.df = data.frame()
# loop the list to set year and value as a column and concatenate
for (aux.df in all.list){
  # Rename a few variable
  aux.df <- rename(aux.df, country.name = `Country Name`, country.code = `Country Code`,indicator.name = `Indicator Name`,indicator.code = `Indicator Code`)
  # "Gather" the year variables and values into a pair of variables: `year` and `value`
  aux.df <- gather(aux.df, year, value, -country.name, -country.code, -indicator.name, -indicator.code) 
  #concatenates
  wdi.df <- rbind(aux.df,wdi.df)
}

#remove column year=x61 and country.code=INX with 100% missing values
wdi.df <- filter(wdi.df, year!="X61")
wdi.df <- filter(wdi.df, country.code!="INX")

# Make the `indicator.code` variable into a factor
wdi.df$indicator.code = factor(wdi.df$indicator.code)
# Make the `country.code` variable into a factor
wdi.df$country.code = factor(wdi.df$country.code)
# Make the `year` variable into an ordered factor
wdi.df$year = factor(wdi.df$year, ordered=TRUE)



#select 5 countries of interest (https://en.wikipedia.org/wiki/BRICSg) and years 1992/2011 (2 decades/20 years) 
wdi.df %>% filter(. , country.code %in% c("BRA","CHN","IND","RUS","ZAF")) -> wdi.df
wdi.df <- filter(wdi.df, year>1991 & year<2012) 


# are there missing values for any of our countries?
wdi.df %>%  group_by(country.code) %>%  summarize(count=n(), cnt.missing=sum(is.na(value)), pct.missing= 100*cnt.missing/count) %>% 
  arrange(country.code) %>%  select(country.code, pct.missing) %>%  arrange(desc(pct.missing)) %>% print(n=250)


# Let's save our final data.frame wdi.dfv2 as a csv file
write.csv(file=("./data/wdi.df.csv"), x=wdi.df)

#Describe each variable, including the units of the numeric varables. Describe the meaning of the observations of the dataset.
#Include summaries of individual variables using only the group_by and summarize functions
#from the dplyr library. You must use the pipe operator from the magrittr library.
#Each summary should be interpreted in a way that informs the reader of important
#information about that variable or those variables in the context of the data set.

# drom extra columns
drops=c('country.name','indicator.name')
wdi.df = wdi.df[ , !(names(wdi.df) %in% drops)]
wdi.df$country.code = factor(wdi.df$country.code) #refactor
wdi.df$year = factor(wdi.df$year, ordered=TRUE) #refactor
wdi.df$indicator.code = factor(wdi.df$indicator.code, ordered=TRUE) #refactor

#column value to numberic
wdi.df$value <- as.numeric(wdi.df$value)

#transform data frame to have a column for each indicator
#aux.df <- gather(aux.df, year, value, -country.name, -country.code, -indicator.name, -indicator.code)
wdi.df %>% spread(., indicator.code, value) -> wdi.df

#rename indicator columns for better understanding
wdi.df %>% rename(. , surface.km2 =AG.SRF.TOTL.K2,
                  energy.use = EG.USE.PCAP.KG.OE,
                  co2.emission = EN.ATM.CO2E.PC,
                  GDP = NY.GDP.MKTP.CD,
                  GDP.growth = NY.GDP.MKTP.KD.ZG,
                  pop.0.14 = SP.POP.0014.TO.ZS,
                  pop.15.64 = SP.POP.1564.TO.ZS,
                  pop.growth = SP.POP.GROW,
                  pop.total =  SP.POP.TOTL,
                  pop.rural=  SP.RUR.TOTL.ZS,
                  pop.urban =SP.URB.TOTL.IN.ZS 
) -> wdi.df

#create new indicator POPULATION DENSITY = POPULATION TOTAL/TOTAL COUNTRY SURFACE
wdi.df %>% mutate(., pop.density = pop.total / surface.km2) -> wdi.df
wdi.df$surface.km2 <- NULL

#get quartiles. Not sure how we are going to use them but professor wanted us to have it.
#we do it for all indicators and usen the name of the column + .q
make.ntiles = function (inputvar, n) { 
  inputvar %>%
    quantile(., 
             (1/n) * 1:(n-1),
             na.rm=TRUE
    ) %>%
    c(-Inf, ., Inf) %>%
    cut(inputvar, 
        breaks=., 
        paste("Q", 1:n, sep="")
    ) 
}
wdi.df %>%  mutate(energy.use.q = make.ntiles(energy.use, 4 ),
                   co2.emission.q = make.ntiles(co2.emission, 4 ),
                   GDP.q = make.ntiles(GDP, 4 ),
                   GDP.growth.q = make.ntiles(GDP.growth, 4 ),
                   pop.0.14.q = make.ntiles(pop.0.14, 4 ),
                   pop.15.64.q = make.ntiles(pop.15.64, 4 ),
                   pop.growth.q = make.ntiles(pop.growth, 4 ),
                   pop.total.q =  make.ntiles(pop.total, 4 ),
                   pop.rural.q =  make.ntiles(pop.rural, 4 ),
                   pop.urban.q = make.ntiles(pop.urban, 4 ), 
                   pop.density.q = make.ntiles(pop.density, 4)
                   ) -> wdi.df



# now we can work on the summaries...
indicators <- c("energy.use", "co2.emission","GDP", "GDP.growth", "pop.0.14", "pop.15.64",
                "pop.growth", "pop.total", "pop.rural", "pop.urban", "pop.density")

#summaries of all the indicators during the 2 decades
summary(wdi.df[indicators])

#summaries per countries
wdi.df %>% group_by(country.code) %>% summarise(avg=mean(energy.use), median=median(energy.use), sd=sd(energy.use), max=max(energy.use),min=min(energy.use))
wdi.df %>% group_by(country.code) %>% summarise(avg=mean(co2.emission), median=median(co2.emission), sd=sd(co2.emission), max=max(co2.emission),min=min(co2.emission))
wdi.df %>% group_by(country.code) %>% summarise(avg=mean(GDP.growth), median=median(GDP.growth), sd=sd(GDP.growth), max=max(GDP.growth),min=min(GDP.growth))
wdi.df %>% group_by(country.code) %>% summarise(avg=mean(GDP), median=median(GDP), sd=sd(GDP), max=max(GDP),min=min(GDP))
wdi.df %>% group_by(country.code) %>% summarise(avg=mean(pop.0.14), median=median(pop.0.14), sd=sd(pop.0.14), max=max(pop.0.14),min=min(pop.0.14))
wdi.df %>% group_by(country.code) %>% summarise(avg=mean(pop.15.64), median=median(pop.15.64), sd=sd(pop.15.64), max=max(pop.15.64),min=min(pop.15.64))
wdi.df %>% group_by(country.code) %>% summarise(avg=mean(pop.growth), median=median(pop.growth), sd=sd(pop.growth), max=max(pop.growth),min=min(pop.growth))
wdi.df %>% group_by(country.code) %>% summarise(avg=mean(pop.total), median=median(pop.total), sd=sd(pop.total), max=max(pop.total),min=min(pop.total))
wdi.df %>% group_by(country.code) %>% summarise(avg=mean(pop.rural), median=median(pop.rural), sd=sd(pop.rural), max=max(pop.rural),min=min(pop.rural))
wdi.df %>% group_by(country.code) %>% summarise(avg=mean(pop.urban), median=median(pop.urban), sd=sd(pop.urban), max=max(pop.urban),min=min(pop.urban))
wdi.df %>% group_by(country.code) %>% summarise(avg=mean(pop.density), median=median(pop.density), sd=sd(pop.density), max=max(pop.density),min=min(pop.density))

# summaries of indicators per years
wdi.df %>% group_by(year) %>% summarise(avg=mean(energy.use), median=median(energy.use), sd=sd(energy.use), max=max(energy.use),min=min(energy.use))
wdi.df %>% group_by(year) %>% summarise(avg=mean(co2.emission), median=median(co2.emission), sd=sd(co2.emission), max=max(co2.emission),min=min(co2.emission))
wdi.df %>% group_by(year) %>% summarise(avg=mean(GDP.growth), median=median(GDP.growth), sd=sd(GDP.growth), max=max(GDP.growth),min=min(GDP.growth))
wdi.df %>% group_by(year) %>% summarise(avg=mean(GDP), median=median(GDP), sd=sd(GDP), max=max(GDP),min=min(GDP))
wdi.df %>% group_by(year) %>% summarise(avg=mean(pop.0.14), median=median(pop.0.14), sd=sd(pop.0.14), max=max(pop.0.14),min=min(pop.0.14))
wdi.df %>% group_by(year) %>% summarise(avg=mean(pop.15.64), median=median(pop.15.64), sd=sd(pop.15.64), max=max(pop.15.64),min=min(pop.15.64))
wdi.df %>% group_by(year) %>% summarise(avg=mean(pop.growth), median=median(pop.growth), sd=sd(pop.growth), max=max(pop.growth),min=min(pop.growth))
wdi.df %>% group_by(year) %>% summarise(avg=mean(pop.total), median=median(pop.total), sd=sd(pop.total), max=max(pop.total),min=min(pop.total))
wdi.df %>% group_by(year) %>% summarise(avg=mean(pop.rural), median=median(pop.rural), sd=sd(pop.rural), max=max(pop.rural),min=min(pop.rural))
wdi.df %>% group_by(year) %>% summarise(avg=mean(pop.urban), median=median(pop.urban), sd=sd(pop.urban), max=max(pop.urban),min=min(pop.urban))
wdi.df %>% group_by(year) %>% summarise(avg=mean(pop.density), median=median(pop.density), sd=sd(pop.density), max=max(pop.density),min=min(pop.density))

