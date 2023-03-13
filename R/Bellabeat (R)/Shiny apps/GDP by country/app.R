library(tidyverse)
library(shiny)
library(shinydashboard)
library(DT)
library(ggrepel)
library(rsconnect)


cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#9999CC", "#66CC99")

df_gdp <- as.data.frame(read.csv2("countries_gdp_hist.csv", sep = ';')) 
df_gdp$total_gdp <- as.numeric(df_gdp$total_gdp)
df_gdp$total_gdp_million <- round(as.numeric(df_gdp$total_gdp_million), 2)
df_gdp$country_name <- as.factor(df_gdp$country_name)
df_gdp$country_code <- as.factor(df_gdp$country_code)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(
    title = "GDP by countries"
  ),
  
  dashboardSidebar(
    sidebarMenu(
      
      selectInput("selectCountry", "Select a country", choices = unique(df_gdp$country_name), selected = "Spain"),
      
      menuItem(text = "Data per Country/Year",
               startExpanded = TRUE,
               menuSubItem(text = "Data", tabName = "countryData", icon = icon("table")),
               menuSubItem(text = "Plots", tabName = "countryPlots", icon = icon("chart-bar"))
      ),
      
      selectInput("selectYear", "Select which year to view", choices = unique(df_gdp$year), selected = 2021),
      
      menuItem(text = "Aggregated countries",
               startExpanded = TRUE,
               menuSubItem(text = "All data", tabName = "allData", icon = icon("table")),
               menuSubItem(text = "Top GDP countries", tabName = "topGdp", icon = icon("chart-bar"))
              )
      
            )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "countryData",
              DTOutput("countryDataAggreg"),
              DTOutput("yearlyCountryData")
              ),
      tabItem(tabName = "countryPlots",
              plotOutput("yearlyCountryPlot")
              ),
      tabItem(tabName = "topGdp",
              plotOutput("histTopGdp"),
              plotOutput("lineTopGdp")
              ),
      tabItem(tabName = "allData",
              DTOutput("allDataTable")
              )
    )
  )

)

# Define server logic required to draw a histogram
server <- function(input, output) {

  countryData <- reactive({
    filteredData <- df_gdp[df_gdp$country_name == input$selectCountry,] %>% arrange(desc(year))
    return(filteredData)
  })
  
  data_year_max_gdp <- reactive({
    filteredData3 <- df_gdp[df_gdp$country_name == input$selectCountry, ] %>%
      filter(total_gdp_million == max(total_gdp_million)) %>%
      select(year)
    return(filteredData3)
  })
      
  aggregatedData <- reactive({
    filteredData2 <- df_gdp[df_gdp$country_name == input$selectCountry, ] %>% 
      group_by(country_name) %>% 
      drop_na() %>% 
      reframe(country_code = unique(country_code), 
              country_name = unique(country_name), 
              region_name = unique(region_name),
              income_group = unique(income_group),
              max_total_gdp_million = max(total_gdp_million), 
              year_max_gdp = data_year_max_gdp(),
              average_gdp = mean(total_gdp)
                )
    return(filteredData2)
  })
  
  yearData <- reactive({
    filteredData <- df_gdp[df_gdp$year == input$selectYear,] %>% arrange(desc(total_gdp_million))
    final <- filteredData[1:20,]
    return(final)
  })
  
  topGdpData <- df_gdp[, c("country_code", "country_name", "year", "total_gdp_million")] %>% 
    filter(total_gdp_million != 0.00) %>% 
    filter(country_code == ("USA") | country_code == ("CHN") | country_code == ("JPN") | country_code == ("DEU") | country_code == ("IND") | country_code == ("GBR") | country_code == ("FRA") | country_code == ("ITA") | country_code == ("CAN") | country_code == ("KOR"))

  
  ##########   OUTPUTS   ##############
  
  output$countryDataAggreg <- renderDT({
    datatable(aggregatedData())
  })
  
  output$yearlyCountryData <- renderDT({
    datatable(countryData())
  })

  output$yearlyCountryPlot <- renderPlot({
    plotTitle <- paste0("GDP evolution of ", input$selectCountry)
    return(ggplot(data = countryData(), aes(x = year, y = total_gdp_million, color = country_name)) + geom_line() + labs(title = plotTitle, x = "Year", y = "GDP (million $)")
    )
  })
  
  output$allDataTable <- renderDT({
    datatable(df_gdp)
  })
  
  
  output$histTopGdp <- renderPlot({
    plotTop <- ggplot(data = yearData(), mapping = aes(x = fct_rev(fct_reorder(country_name, total_gdp_million)), y = total_gdp_million, fill = fct_rev(fct_reorder(country_name, total_gdp_million))))
    plotTitle <- paste0("Top countries with highest GDP in year ", input$selectYear)
    return(plotTop + geom_col() + labs(title = plotTitle, y = "GDP (million $)") + 
             theme(axis.text.x = element_text(angle = 45, hjust = 1), 
                   axis.title.x=element_blank(), 
                   legend.position = "none") 
          )
  })
  
  output$lineTopGdp <- renderPlot({
    #countryLabels <- unique(topGdpData$country_name)
    legend_ord <- levels(with(topGdpData, reorder(country_name, total_gdp_million)))
    plotTitle <- paste0("Evolution of Top countries with highest GDP in year ", input$selectYear)
    return(ggplot(topGdpData, aes(x = year, y = total_gdp_million, color = country_name)) + geom_line() + labs(title = plotTitle, x = "Year", y = "GDP (million $)", color = "Countries") + scale_colour_manual(values=cbPalette) 
             #ggrepel::geom_text_repel(data = countryLabels, aes(label = class))
          )
  })

}


# Run the application 
shinyApp(ui = ui, server = server)
