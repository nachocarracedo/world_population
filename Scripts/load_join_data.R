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

# Read each one fo the indicators into a data frame from the downloaded CSV files (remove manuall first 3 lines of cvs)
setwd("C:/Users/carrai1/Desktop/Master/MA402_Data_Science/Assigment_1/")

san.df = read_csv("./data/%_access_sanitation_facilities_90_15.csv")
ag.df = read_csv("./data/%_agricultural_land_61_13.csv")
pop0.df = read_csv("./data/%_population_0_14_60_15.csv")
pop15.df = read_csv("./data/%_population_15_64_60_15.csv")
poprur.df= read_csv("./data/%_rural_population_60_15.csv")
popur.df= read_csv("./data/%_urban_population_60_15.csv")
co2.df= read_csv("./data/CO2_emisions_60_11.csv")
kmsqr.df= read_csv("./data/km_square_61_15.csv")
popgrowth.df= read_csv("./data/population_growth_60_15.csv")
poptotal.df= read_csv("./data/population_total_60_15.csv")
elecoutput.df= read_csv("./data/renew_elec_output_90_12.csv")
mort5.df= read_csv("./data/mortality_less5_rate.csv")
energy.df= read_csv("./data/energy_use_kg_oil.csv")
gdp_growth.df= read_csv("./data/GDP_growth_annual%.csv")
gdp.df= read_csv("./data/GDP.csv")

# put all data.frames into a list
all.list <- list(san.df,ag.df,pop0.df,pop15.df,poprur.df,popur.df,co2.df,kmsqr.df,
                 popgrowth.df,poptotal.df,elecoutput.df,mort5.df,energy.df,
                 gdp_growth.df,gdp.df)

# chech: loop the list to print first observatinos
for (i in all.list){print(head(i))}

# Join all data.frames into a single data.frame
wdi.df = data.frame()
# loop the list to set year and value as a column and concatenate
for (aux.df in all.list){
  # Rename a few variable
  aux.df <- rename(aux.df, country.name = `Country Name`, country.code = `Country Code`,indicator.name = `Indicator Name`,indicator.code = `Indicator Code`)
  # "Gather" the year variables and values into a pair of variables: `year` and `value`
  aux.df <- gather(aux.df, year, value, -country.name, -country.code, -indicator.name, -indicator.code) 
  #print number of rows to check
  print(nrow(aux.df))
  #concatenates
  wdi.df <- rbind(aux.df,wdi.df)
}

# check structure of final dataframe
str(wdi.df)

# Make the `indicator.code` variable into a factor
wdi.df$indicator.code = factor(wdi.df$indicator.code)
str(wdi.df$indicator.code)
length(unique(wdi.df$indicator.code))
summary(wdi.df$indicator.code)

# Make the `country.code` variable into a factor
wdi.df$country.code = factor(wdi.df$country.code)
str(wdi.df$country.code)
length(unique(wdi.df$country.code))
levels(wdi.df$country.code)
summary(wdi.df$country.code)

# Make the `year` variable into an ordered factor
wdi.df$year = factor(wdi.df$year, ordered=TRUE)
str(wdi.df$year)
levels(wdi.df$year)
summary(wdi.df$year)

# Some final check on our data.frame:
str(wdi.df)
head(wdi.df,4)
sample_n(wdi.df, size=10)
options(dplyr.width = Inf) # forces dplyr to show all columns
slice(wdi.df, 1000:1020)
filter(wdi.df, year==1995)

# Now let's check missing values to select a subset of years (and countries?) 

# Display the years sorted by the percent of missing values for all measurements for that year
wdi.df %>%  group_by(year) %>%  summarize(count=n(), cnt.missing=sum(is.na(value)), pct.missing= 100*cnt.missing/count) %>% 
  arrange(year) %>%  select(year, pct.missing) %>%   print(n=100)

# same for countries
wdi.df %>%  group_by(country.code) %>%  summarize(count=n(), cnt.missing=sum(is.na(value)), pct.missing= 100*cnt.missing/count) %>% 
  arrange(country.code) %>%  select(country.code, pct.missing) %>%  arrange(desc(pct.missing)) %>% print(n=250)

# We'll also remove column year=x61 and country.code=INX with 100% missing values
wdi.df <- filter(wdi.df, year!="X61")
wdi.df <- filter(wdi.df, country.code!="INX")


# Let's create wdi.dfv2 20 years and 134 countries WITHOUT missing values!!!!!!!!!!!!!!!! :)
wdi.dfv2 <- filter(wdi.df, year>1991& year<2012) 
wdi.dfv2 %>%  group_by(country.code) %>%  summarize(count=n(), cnt.missing=sum(is.na(value)), pct.missing= 100*cnt.missing/count) %>% 
  arrange(country.code) %>%  select(country.code, pct.missing) %>%  arrange(desc(pct.missing)) %>% print(n=270) -> countries.mv
# Keep countries with 0% missing values
country_keep = (filter(countries.mv, pct.missing == 0))$country.code
wdi.dfv2 <- filter(wdi.dfv2, country.code %in% country_keep)
# check missing values again
wdi.dfv2 %>%  group_by(year) %>%  summarize(count=n(), cnt.missing=sum(is.na(value)), pct.missing= 100*cnt.missing/count) %>% 
  arrange(year) %>%  select(year, pct.missing) %>%   print(n=100)
# same for countries
wdi.dfv2 %>%  group_by(country.code) %>%  summarize(count=n(), cnt.missing=sum(is.na(value)), pct.missing= 100*cnt.missing/count) %>% 
  arrange(country.code) %>%  select(country.code, pct.missing) %>%  arrange(desc(pct.missing)) %>% print(n=250)

# there are other combinations which have more years and countries but with some missing values. 
# Let's save our final data.frame wdi.dfv2 as a csv file
str(wdi.dfv2)
wdi.dfv2$country.code = factor(wdi.dfv2$country.code) #refactor
wdi.dfv2$year = factor(wdi.dfv2$year, ordered=TRUE) #refactor
str(wdi.dfv2)
write.csv(file=("./data/wdi.df.csv"), x=wdi.dfv2)