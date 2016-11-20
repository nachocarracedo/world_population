## app.R ##
library(shinydashboard)
library(wbstats)
library(magrittr)
library(dplyr)
library(tidyr)
library(plotly)
library(ggplot2)

intro = "The World Bank is an international organization that aims to defeat poverty in the world (specifically in countries with low to medium income) through means of training, development and loan offerings to countries so that they may "help countries help themselves".
The organization focuses on delivering measurable results and in order to do that it leads a powerful initiative for open development and access to information. This consists of access to tools and data that spans over more than 50 years for macroeconomic and microeconomic factors for countries from all over the globe."

topics=" This application shows how Brazil, Russia, India, China and South Africa (BRICS countries) have changed along the years based on information from The World Bank. BRICS countries are the top five emerging economies in the world.  
It focuses mainly on BRICS countries' population. In doing this, it attemps to answer the following questions:
(i) What are the countries that present consistent positive GDP growth in the past years? Which countries have not been as succesful?; 

(ii) what are the countries with the highest CO2 Emissions? Which ones are on their way to efficient use of energy?; 

(iii) how does the population of 0 to 14 year olds vary against the population aged between 15 and 64? How do populations metrics shift?; 

(iv) how do the two age groups affect the country's economic well-being?; 

(v) how does population density look in these countries?"

objective = "The goal of this application is to provide an easy way to show information regarding population growth in BRICS contries and analyze how this growth changes along with variables such as CO2 emissions and energy use as well as taking a glimpse into the evolution of such factors through time. 
We have chosen to study the BRICS countries because in an effort to reveal possible connections in the data, these countries present highly dynamic changes and a shifting landscape that can help shed a light on the questions we look to answer. If any set of countries can make connections clearer, it would have to be the BRICS."


tab1explanation = "Select a country and an indicator to see a summary of the indicator for that country. Also, select an indicator to check its histogram, you can also select the number of bins of the histogram."
tab2explanation = "Select a pair of indicators to see a scatter plot of both, this will show how they relate. A correlation heatmap of all indicators it's also shown"
#tab3explanation

#setwd("C:/Users/carrai1/Desktop/Master/MA402_Data_Science/assigment_1/")
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


# Create the UI element
ui <- 
  dashboardPage(
    skin="blue",
    title="change this_",
    header=dashboardHeader(title="Assignment 2"),
    ## Sidebar content
    sidebar=dashboardSidebar(
      sidebarMenu(
        menuItem("Home",        tabName="home_tab",   icon=icon(name="home",       lib="glyphicon")),
        menuItem("Single World indicator", tabName="first_tab",  icon=icon(name="menu-right", lib="glyphicon")),
        menuItem("Multi World indicator", tabName="second_tab", icon=icon(name="menu-right", lib="glyphicon"))
      )
    ),
    ## Body content
    body=dashboardBody(
      tabItems(
        # Tab content for "home_tab"
        tabItem(tabName="home_tab",
                fluidRow(
                  box(width=12, background="blue", title="Introduction",intro),
                  box(width=12, background="blue", title="Topics"      ,topics),
                  box(width=12, background="blue", title="Objectives"  ,objective)
                )
        ),
        
        # Tab content for "first_tab"
        tabItem(tabName="first_tab",
                fluidRow(
                  box(width=12, background="blue", title="Exploring indivudual Indicators by BRICS country",tab1explanation),
                  box(width=12, selectInput("first_country", "Choose one BRICS country: ", 
                                            multiple=FALSE, selected="Brazil",
                                            choices=country.names)), 
                  box(width=12, selectInput("first_indicator", "Choose one indicator: ", 
                                            multiple=FALSE, selected="co2.emission",
                                            choices=indicator.codes)),
                  box(width=12, sliderInput("obs", "Size of bin (histogram):",  
                                            min = 1, max = 500, value = 2)),
                  box(width=12, title="Indicator average for the selected BRICS country", tableOutput("avg_by_country")),
                  box(width=12, title="Histogram", plotlyOutput("ic_histogram")),
                  box(width=12, title="This map shows information for all BRICS countries", plotlyOutput("world_map"))
                )
        ),
        
        # tab content for "second_tab" 
        tabItem(tabName="second_tab",
                fluidRow(
                  box(width=12, background="blue", title="Exploring interaction between Indicators",tab2explanation),
                  box(width=12, selectInput("first_indicator_scatter", "Choose one indicator:", 
                                            multiple=FALSE, selected="energy.use",
                                            choices=indicator.codes)),
                  box(width=12, selectInput("second_indicator_scatter", "Choose a second indicator:", 
                                            multiple=FALSE, selected="co2.emission",
                                            choices=indicator.codes)),
                  box(width=12, plotlyOutput("scatter_plot")),
                  box(width=12, title="Correlation between indicators", plotlyOutput("cor_heat"))
                  
                )
        )
        # You may want to create additional tabs to 
        # organize your input and output elements
      )
    )
  )

server <- function(input, output) {
  
  
  output$avg_by_country <- renderTable({ 
    wb.df %>%
      filter(country.names==(input$first_country)) %>% 
      group_by(country) %>%
      summarise(avg=mean(quote(indicator.codes %in% input$first_indicator)))#, sd=sd(indicator.codes %in% input$first_indicator), max=max(input$first_indicator))
  }, align='c', bordered=TRUE)
  
  
  output$ic_histogram <- renderPlotly({
    wb.df %>%
      ggplot() + 
      aes_string(x=input$first_indicator) +
      geom_histogram(binwidth = input$obs) %>%
      {.} -> p
    ggplotly(p,color = I('yellow'))
  })
  
  
  
  
  output$world_map <- renderPlotly({
    wb.df %>%
      group_by(.,country) %>%
      mutate_(chosen_ic=input$first_indicator) %>%
      summarize(avg=mean(chosen_ic, na.rm=TRUE)) -> aa
    
    l <- list(color = toRGB("grey"), width = 0.5)
  
    g <- list(
      showframe = FALSE,
      showcoastlines = TRUE,
      projection = list(type = 'Mercator')
    )
    
    plot_geo(aa, locationmode ='ISO-3') %>%
      add_trace(
        z = ~avg, color = ~avg, colors = 'Blues',
        text = ~country, locations = ~country, marker = list(line = l)
      )  %>%
      colorbar(title = input$first_indicator, tickprefix = '') %>%
      layout(
        title = paste('World map -',input$first_indicator),
        geo = g
      )
  })
  
  output$scatter_plot <- renderPlotly({
    x.vec = wb.df[,input$first_indicator_scatter]
    y.vec = wb.df[,input$second_indicator_scatter]
    plot_ly(x = x.vec, y = y.vec, mode='markers', color = I('black'), marker = list(size = 7)) %>%
      layout(title="Indicators Scatter Plot",
             xaxis=list(title=input$first_indicator_scatter), 
             yaxis=list(title=input$second_indicator_scatter)) 
  })
  
  
  output$cor_heat <- renderPlotly({
    plot_ly(x = indicator.codes, y = indicator.codes, z = correlation, 
            key = correlation, type = "heatmap") %>%
      layout(xaxis = list(title = ""), 
             yaxis = list(title = ""))
  })
  
}

shinyApp(ui, server)
