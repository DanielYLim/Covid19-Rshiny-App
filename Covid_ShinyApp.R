#### Libraries ----
library(shiny)
library(shinydashboard)
library(tidyverse)
library(scales)
library(ggplot2)

#### Data ----
dat <- readRDS(file = "covid19_data.rds")

metric_choices = colnames(dat)[3:ncol(dat)]
metric_names = gsub("_", " ", metric_choices)
metric_names = paste0(toupper(substr(metric_names,1,1)), substr(metric_names, 2, nchar(metric_names)))
metric_list = as.list(metric_choices)
names(metric_list) = metric_names


#### UI ----
ui <- dashboardPage(
  #_______________________----
  # Header ----
  dashboardHeader(    
    title = "COVID19 Plots",
    titleWidth = 350
  ),
  #_______________________----
  # Sidebar ----
  dashboardSidebar(
    width = 350,
    br(),
    h4("Select Your Inputs Here", style = "padding-left:20px"),
    # metric input ----
    selectInput(
      inputId = "metric",
      label = strong("Select Metric", style = "font-family: 'arial'; font-size: 12px"),
      choices = metric_list,
      selected = metric_list[1]
    ),
    
    
    # country input ----
    selectInput(
      inputId = "country",
      multiple = TRUE,
      label = strong("Select Counrtries to Compare", style = "font-family: 'arial'; font-size: 14px"),
      choices = unique(dat$Country),
      selected = c("South Korea", "France", "Canada")
    ),
    
    # date_range_country input ----
    dateRangeInput(
      inputId = "date_range_country",
      label = "Select Date Range",
      start = "2019-12-31",
      end = "2022-12-30"
      
    )
    
    
    
    ),
  #_______________________----
  # Body ----
  dashboardBody(
    tabsetPanel(
      type = "tabs",
      id = "tab_selected",
      tabPanel(
        title = "Country View",
        plotOutput("plot_data_country")
      )
    )
  )
    
  
  
  
)



#### server ----
server <- function(input, output){
  observe(print(input$metric))
  observe(print(input$country))
  observe(print(input$date_range_country))
  
  #________________________________________ ----
  # 1- Data Cleaning Functions ----
  # 01.A clean_dat_country() ----
  
  clean_dat_country <- reactive({
    clean_dat <- dat %>%
      filter(Country == input$country & date >= input$date_range_country[1] & date<=input$date_range_country[2] ) %>%
      group_by(Country, date) %>%
      summarise_all(sum) %>%
      select(Country, date, input$metric) %>%
      set_names(c("Country", "date", "metric")) %>%
      arrange(date)
  })
  #________________________________________ ----
  # 2- Plotting data ----
  # 02 A plot_data_country ----
  
  output$plot_data_country <- renderPlot({
    ggplot(data = clean_dat_country(), aes(y=metric, x= date, color = Country )  ) +
      geom_line(size =1)+
      ylab( metric_names[which( metric_choices == input$metric   )]   )+
      xlab("Date")+
      labs(color = "Country Name")+
      scale_y_continuous(label = comma)+
      scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")+
      ggtitle(metric_names[which( metric_choices == input$metric   )]  )    
  })
  
}




#### shiny app ----
shinyApp(ui, server)

