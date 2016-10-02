# World Development Indicators
#
# 1) Go to http://data.worldbank.org/topic
# 2) Choose a topic (click on it)
# 3) Click on "DOWNLOAD DATA" (red box) and then on EXCEL
# 4) Open the spreadsheet and look through the list of 
#    series codes and descriptions to find codes
#    that interest you. 
# 5) Add these codes to the `indicator.codes` variable below.
# 6) Read and summarize the data. (code below)
# 7) Keep the series code if there is enough data.
# 8) Repeat (add new series codes)


# Load the WDI, readr and dplyr libraries
library(WDI)
library(readr)
library(dplyr)
library(tidyr)


# Read the WDI data frame from the downloaded CSV file
setwd("C:/Users/carrai1/Desktop/Master/MA402_Data_Science/Assigment_1/")
wdi.df = read_csv("./API_EG.ELC.ACCS.ZS_DS2_en_csv_v2.csv")

# Display the structure of the dataframe
str(wdi.df)
names <- names(wdi.df)
names[1] <- "Country Name"
names(wdi.df) <- names
names(wdi.df)


# Rename a few variables
wdi.df %>%  rename(Country Name   = Country.Name
                   ) %>%  {.} -> wdi.df

# Make the `indicator.code` variable into a factor
wdi.df$indicator.code = factor(wdi.df$indicator.code)
str(wdi.df$indicator.code)
length(unique(wdi.df$indicator.code))
summary(wdi.df$indicator.code)

# Make the `country.code` variable into a factor
wdi.df$country.code = factor(wdi.df$country.code)
str(wdi.df$country.code)
length(unique(wdi.df$country.code))
summary(wdi.df$country.code)

# Display the structure of the dataframe to check that 
# the variables were renamed and retyped correctly 
str(wdi.df)

# Take a look at a few cases with non-missing values
complete.cases(wdi.df) %>%
  {wdi.df[.,]} %>%
  select(country.code, indicator.name, starts_with("1"), starts_with("2")) %>%
  sample_n(10) %>%
  print()

# "Gather" the year variables and values 
# into a pair of variables: `year` and `value`
wdi.df %>%
  gather(., year, value, -country.name, -country.code, -indicator.name, -indicator.code) %>%
  {.} -> wdi.df

# Take a look at a few cases with non-missing values
complete.cases(wdi.df) %>%
  {wdi.df[.,]} %>%
  select(country.code, indicator.name, year, value) %>%
  sample_n(10) %>%
  print()

# Check the structure of the data set
str(wdi.df)

# Make the `year` variable into an ordered factor
wdi.df$year = factor(wdi.df$year, ordered=TRUE)
str(wdi.df$year)
levels(wdi.df$year)
summary(wdi.df$year)

# Check the structure of the dataset
str(wdi.df)

# Display the up to 100 indicator codes with fewer than 25% missing values
# Consider all measurements after 1980 (all three numbers can be changed)
wdi.df %>%
  group_by(indicator.code) %>%
  filter(year>1980) %>%
  summarize(count          = n(),
            cnt.missing    = sum(is.na(value)),
            pct.missing    = 100*cnt.missing/count,
            indicator.name = first(indicator.name)) %>%
  filter(pct.missing < 25) %>%
  arrange(pct.missing) %>%
  select(indicator.name, indicator.code, pct.missing) %>%
  print(n=100) 

# Display the years sorted by the percent of missing values
# for all measurements for that year
wdi.df %>%
  group_by(year) %>%
  summarize(count          = n(),
            cnt.missing    = sum(is.na(value)),
            pct.missing    = 100*cnt.missing/count) %>%
  arrange(year) %>%
  select(year, pct.missing) %>%
  print(n=100)

# Now determine a range of years and a collection of indicator codes
# to use in creating your data set. Use two criteria: 
# 1) There shouldn't be too many missing values for any indicator code
# 2) You are telling a story by investigating the data set. 
#    Your data should be interesting so you have something 
#    to analyze, summarize, investigate and write about. 

wdi.df %>%
  filter(indicator.code %in% c("SP.RUR.TOTL","SP.URB.TOTL")) %>%
  filter(year > 2000) %>%
  {.} -> wdi.df

str(wdi.df)

summary(wdi.df$indicator.code)
summary(wdi.df$year)
wdi.df %>%
  mutate(indicator.code=factor(indicator.code),
         year          =factor(year)) %>%
  {.} -> wdi.df
summary(wdi.df$indicator.code)
summary(wdi.df$year)

str(wdi.df)
wdi.df %>%
  print(n=6)

wdi.df %>%
  select(-indicator.name) %>%
  spread(indicator.code,value) %>%
  {.} -> wdi.df

wdi.df %>%
  print(n=3)

# Function to create a factor vector 
# containing `n` quantiles from `inputvar`
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

wdi.df %>%
  mutate(SP.RUR.TOTL.factor = make.ntiles(SP.RUR.TOTL, 4 ),
         SP.URB.TOTL.factor = make.ntiles(SP.URB.TOTL, 4 )) %>%
  {.} -> wdi.df

summary(wdi.df)



