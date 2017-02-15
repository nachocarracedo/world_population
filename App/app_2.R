## app.R ##
library(shiny)
library(shinydashboard)
library(shinythemes)
library(wbstats)
library(magrittr)
library(dplyr)
library(tidyr)
library(plotly)
library(ggplot2)

intro = "The World Bank is an international organization that aims to defeat poverty in the world (specifically in countries with low to medium income) through means of training, development and loan offerings to countries so that they may <<help countries help themselves>>.
The organization focuses on delivering measurable results and in order to do that it leads a powerful initiative for open development and access to information. This consists of access to tools and data that spans over more than 50 years for macroeconomic and microeconomic factors for countries from all over the globe.
We have gathered a few of these indicators, selected a few countries and built a dashboard for data visualization and analysis around it."

topics = HTML("This application shows how Brazil, Russia, India, China and South Africa (BRICS countries) have changed along the years based on information from The World Bank. 
It focuses mainly on population metrics. In doing this, it attemps to answer the following questions:
<ul>
<li> What are the countries that present consistent positive GDP growth in the past years? Which countries have not been as succesful? </li>

<li> What are the countries with the highest CO2 Emissions? Which ones are on their way to efficient use of energy? </li>

<li> How does the population of 0 to 14 year olds vary against the population aged between 15 and 64? How do populations metrics shift? </li>

<li> How do the two age groups affect the country's economic well-being? </li>

<li>How does population density look in these countries?</li>
</ul>")

objective = "The goal of this application is to provide an easy way to show information regarding population growth in BRICS contries and analyze how this growth changes along with variables such as CO2 emissions and energy use as well as taking a glimpse into the evolution of such factors through time.
We have chosen to study the BRICS countries because in an effort to reveal possible connections in the data, these countries present highly dynamic changes and a shifting landscape that can help shed a light on the questions we look to answer. If any group of countries can make such connections clearer, it would have to be the BRICS."

tab1explanation = HTML("Select a country and an indicator to see a summary for that indicator for the chosen country. <br><br>The indicator codes are described as follows: <br>
                       <ol>
                        <li>pop.density = Population Density (%)</li>
                        <li>energy.use = Energy Use (kg of oil equivalent per capita)</li>
                        <li>co2.emission = CO2 Emissions (metric tons per capita)</li>
                        <li>GDP = Gross Domestic Product (USD)</li>
                        <li>GDP.growth = Gross Domestic Product Growth (annual %)</li>
                        <li>pop.0.14 = Population ages 0-14 (% of total)</li>
                        <li>pop.15.64 = Population ages 15-64 (% of total)</li>
                        <li>pop.growth = Population growth (annual %)</li>
                        <li>pop.total = Total population (numeric)</li>
                        <li>pop.rural = Rural population (% of total population)</li>
                        <li>pop.urban = Urban population (% of total population)</li>
                      </ol>")
tab2explanation = HTML("In this section you are able to mix and match two indicators. A scatterplot of the data for the selected indicators is automatically generated to facilitate visualization of possible relationships. <br><br>The indicator codes are described as follows: <br>
                       <ol>
                        <li>pop.density = Population Density (%)</li>
                        <li>energy.use = Energy Use (kg of oil equivalent per capita)</li>
                        <li>co2.emission = CO2 Emissions (metric tons per capita)</li>
                        <li>GDP = Gross Domestic Product (USD)</li>
                        <li>GDP.growth = Gross Domestic Product Growth (annual %)</li>
                        <li>pop.0.14 = Population ages 0-14 (% of total)</li>
                        <li>pop.15.64 = Population ages 15-64 (% of total)</li>
                        <li>pop.growth = Population growth (annual %)</li>
                        <li>pop.total = Total population (numeric)</li>
                        <li>pop.rural = Rural population (% of total population)</li>
                        <li>pop.urban = Urban population (% of total population)</li>
                      </ol>")
tab3explanation = HTML("Finally, this section offers the possbility of comparing an indicator between two countries.<br><br>
                The indicator codes are described as follows: <br>
                       <ol>
                        <li>pop.density = Population Density (%)</li>
                        <li>energy.use = Energy Use (kg of oil equivalent per capita)</li>
                        <li>co2.emission = CO2 Emissions (metric tons per capita)</li>
                        <li>GDP = Gross Domestic Product (USD)</li>
                        <li>GDP.growth = Gross Domestic Product Growth (annual %)</li>
                        <li>pop.0.14 = Population ages 0-14 (% of total)</li>
                        <li>pop.15.64 = Population ages 15-64 (% of total)</li>
                        <li>pop.growth = Population growth (annual %)</li>
                        <li>pop.total = Total population (numeric)</li>
                        <li>pop.rural = Rural population (% of total population)</li>
                        <li>pop.urban = Urban population (% of total population)</li>
                      </ol>")

#setwd("C:/Users/carrai1/Desktop/Master/MA402_Data_Science/assigment_1/")
#setwd("C:/Users/Ross DeWitt/Documents/GitHub/MA799_Project1/world_population")
wb.df = read.csv("wdi.df.csv")
indicator.codes = c("pop.density", "energy.use", "co2.emission", "GDP", "GDP.growth","pop.0.14", "pop.15.64", "pop.growth", "pop.total" , "pop.rural" ,"pop.urban")
years = c(1992:2011)
dots <- lapply(indicator.codes, as.symbol)
#TODO: change indicator codes to readable names
#indicator.codes2 = c(pop.density, energy.use, co2.emission, GDP, GDP.growth,pop.0.14, pop.15.64, pop.growth, pop.total, pop.rural ,pop.urban)

correlation <- round(cor(select(wb.df, one_of(indicator.codes))), 3)
# Store the sorted list of country country codes
wb.df %>% rename(. , country = country.code) -> wb.df
#wb.df$country <- wb.df$country
country.codes = sort(unique(wb.df$country))

# Create column country.names with countries complete names
mapdf <- data.frame(old=c("BRA", "CHN", "IND", "RUS", "ZAF"),new=c("Brazil", "China", "India", "Russia", "South Africa"))
wb.df$country.names <- mapdf$new[match(wb.df$country,mapdf$old)]

# Create column indicator.full with indicator full names
#c("pop.density", "energy.use", "co2.emission", "GDP", "GDP.growth", "pop.0.14", "pop.15.64", "pop.growth", "pop.total", "pop.rural", "pop.urban"
#indicatordf <- data.frame(old=indicator.codes, new=c("Population Density", "Energy Use", "CO2 Emissions", "Gross Domestic Product", "GDP Growth", "Population of 0 to 14 year-olds", "Population of 15 to 64 year-olds", "Population Growth", "Population Total", "Rural Population (%)", "Urban Population (%)"))
#wb.df$indicator.names <- indicatordf$new[match(wb.df$indicator.codes,indicatordf$old)]

country.names = sort(unique(wb.df$country.names))
#indicator.full = sort(unique(wb.df$indicator.full))

# Create the UI element
ui <- 
  dashboardPage("black",
    #theme = shinytheme("United"),
    #skin="blue",
    #theme=shinytheme("Cerulean"),
    title="WDI Indicators Dashboard",
    header=dashboardHeader(title="BRICS"),
    ## Sidebar content
    sidebar=dashboardSidebar(
      sidebarMenu(
        menuItem("Home",        tabName="home_tab",   icon=icon(name="home",       lib="glyphicon")),
        menuItem("Explore", tabName="first_tab",  icon=icon(name="menu-right", lib="glyphicon")),
        menuItem("Compare Indicators", tabName="second_tab", icon=icon(name="menu-right", lib="glyphicon")),
        menuItem("Compare Countries", tabName="third_tab", icon=icon(name="menu-right", lib="glyphicon"))
      )
    ),
    ## Body content
    body=dashboardBody(
      tabItems(
        # Tab content for "home_tab"
        tabItem(tabName="home_tab",
                fluidRow(
                  box(width=12, background="green", title="Introduction",intro),
                  box(width=12, background="yellow", title="Topics"      ,topics),
                  box(width=12, background="red", title="Objectives"  ,objective)
                )
        ),
        
        # Tab content for "first_tab"
        tabItem(tabName="first_tab",
                fluidRow(
                  box(width=12, background="green", title="Exploring individual Indicators for each BRICS country",tab1explanation),
                  box(width=6, background="green", selectInput("first_country", "Choose one BRICS country: ", 
                                            choices=country.names, multiple=FALSE, selected="Brazil")), 
                  box(width=6, background="green", selectInput("first_indicator", "Choose one indicator: ", 
                                            multiple=FALSE, selected="co2.emission",
                                            choices=indicator.codes)),
                  box(width=6, title="Across Time:", plotlyOutput("line_chart")),
                  box(width=6, title="Around the world: ", plotlyOutput("world_map")),
                  box(width=4, background="green", sliderInput("obs", "Histogram bin size (consider the selected indicator):",  
                                          min = 0.01, max =2, value = 0.5)),
                  box(width=8, title="Histogram", plotlyOutput("ic_histogram"))
                )
        ),
        
        # tab content for "second_tab" 
        tabItem(tabName="second_tab",
                fluidRow(
                  box(width=12, background="yellow", title="Exploring interaction between Indicators",tab2explanation),
                  box(width=4, background="yellow", selectInput("first_indicator_scatter", "Choose one indicator:", 
                                            multiple=FALSE, selected="energy.use",
                                            choices=indicator.codes)),
                  box(width=4, background="yellow", selectInput("second_indicator_scatter", "Choose a second indicator:", 
                                            multiple=FALSE, selected="co2.emission",
                                            choices=indicator.codes)),
                  box(width=4, background="yellow", selectInput("first_country_scatter", "Choose a country: ", 
                                           choices=country.names, multiple=FALSE, selected="Brazil")),
                  box(width=12, plotlyOutput("scatter_plot")),
                  box(width=12, title="General correlation heatmap for all indicators:", plotlyOutput("cor_heat"))
                  
                )
        ),
       
        # tab content for "third_tab" 
        tabItem(tabName="third_tab",
                fluidRow(
                  box(width=12, background="red", title="Exploring indicators across countries",tab3explanation),
                  box(width=4, background="red",selectInput("first_country_compare", "Choose a country: ", 
                                           choices=country.names, multiple=FALSE, selected="Brazil")),
                  box(width=4, background="red",selectInput("second_country_compare", "Choose a country: ", 
                                           choices=country.names, multiple=FALSE, selected="China")),
                  box(width=4, background="red",selectInput("first_indicator_compare", "Choose one indicator:", 
                                           multiple=FALSE, selected="energy.use",
                                           choices=indicator.codes)),
                  box(width=12,plotlyOutput("linez_chart")),
                  box(width=12, background="red", title="BRICS Overview", "In the section below, use the selectors to generate a bar chart that displays the recorded value for each country for a selected indicator at a selected point in time."),
                  box(width=6, background="red",selectInput("year_selected", "Choose a year: ", 
                                           choices=years, multiple=FALSE, selected="2000")),
                  box(width=6, background="red",selectInput("indicator_pie", "Choose an indicator:", 
                                           multiple=FALSE, selected="energy.use",
                                           choices = indicator.codes)),
                  box(width=12,plotlyOutput("pie_chart"))
                  
                )
        )
        
        # You may want to create additional tabs to 
        # organize your input and output elements
      )
    )
  )

server <- function(input, output) {
  
  
#  output$avg_by_country <- renderTable({ 
#    wb.df %>%
#      filter(indicator.codes %in% (input$first_indicator)) %>% 
#      filter(country.names %in% (input$first_country))
#      group_by(year) %>%
#      summarise(Year = mean(input$first_indicator))
# #, sd=sd(indicator.codes %in% input$first_indicator), max=max(input$first_indicator))
# }, align='c', bordered=TRUE)

  output$ic_histogram <- renderPlotly({
    wb.df %>%
      filter(country.names %in% (input$first_country)) %>% 
      ggplot() + 
      aes_string(x=input$first_indicator) +
      geom_histogram(binwidth = input$obs) %>%
      {.} -> p
    ggplotly(p,color = I('yellow'))
  })
  
  #added line chart for view across the years
  output$line_chart <- renderPlotly({
      wb.fil = filter(wb.df, country.names %in% input$first_country) 
      x.vertical = wb.fil[,input$first_indicator]
      y.horizontal = wb.fil[, 'year']
      plot_ly(x=y.horizontal, y=x.vertical, type='scatter', mode='lines') %>%
      layout(title=paste("Evolution of",input$first_indicator," between 1992 and 2011"),
             xaxis=list(title='YEAR'), 
             yaxis=list(title=input$first_indicator)) 
  })
  
  output$linez_chart <- renderPlotly({
    wb.fil1 = filter(wb.df, country.names %in% input$first_country_compare)
    wb.fil2 = filter(wb.df, country.names %in% input$second_country_compare)
    x.vertical = wb.fil1[,input$first_indicator_compare]
    x2.vertical = wb.fil2[,input$first_indicator_compare]
    y1.horizontal = wb.fil1[, 'year']
    y2.horizontal = wb.fil2[, 'year']
    plot_ly(x=y1.horizontal, y=x.vertical, name = paste(input$first_country_compare,input$first_indicator_compare),type='scatter', mode='lines') %>%
      add_trace(y=x2.vertical, name=paste(input$second_country_compare,input$first_indicator_compare), mode='lines') %>%
      layout(title=paste("Comparative evolution of",input$first_indicator_compare," between 1992 and 2011:"),
             xaxis=list(title='YEAR'), 
             yaxis=list(title=input$first_indicator_compare)) 
  })
  
  output$pie_chart <- renderPlotly({
    #wb.pie = wb.df[,c('country', input$indicator_pie, 'year')]
    wb.pie2 = filter(wb.df, year %in% input$year_selected)
    #input$indicator_pie %>%
    newindicator = wb.pie2[,input$indicator_pie]
    ggplot(data=wb.pie2, aes(y=newindicator, x=country, fill=country)) +
      geom_bar(stat="identity") + 
      labs(x='Country', y = input$indicator_pie)
    
    #ggplotly()
    #wb.pie3 = wb.pie2[, c('country', input$indicator_pie)]
    #val = input$indicator_pie
    #newind = wb.pie2[,input$indicator_pie]
    #plot_ly(wb.pie2, x=~country, y=~newind, type='bar') %>%
    #  layout(title = paste('Distribution of',input$indicator_pie,'in', input$year_selected,'among BRICS countries'))
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
        title = paste('Average',input$first_indicator, "between 1992 and 2011 "),
        geo = g
      )
  })
  
  output$scatter_plot <- renderPlotly({
    wb.scatfil = filter(wb.df, country.names %in% input$first_country_scatter)
    x.vec = wb.scatfil[,input$first_indicator_scatter]
    y.vec = wb.scatfil[,input$second_indicator_scatter]
    plot_ly(x = x.vec, y = y.vec, mode='markers', color = I('black'), marker = list(size = 7)) %>%
      layout(title="Indicators Scatter Plot",
             xaxis=list(title=input$first_indicator_scatter), 
             yaxis=list(title=input$second_indicator_scatter)) 
  })
  
  
  output$cor_heat <- renderPlotly({
    plot_ly(x = indicator.codes, y = indicator.codes, z = correlation, 
            key = correlation, colorscale = "Grey", type = "heatmap") %>%
      layout(xaxis = list(title = ""), 
             yaxis = list(title = ""))
  })
  
}

shinyApp(ui, server)
