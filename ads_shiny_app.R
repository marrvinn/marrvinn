#
library("googledrive") 
library("httpuv")
library("dplyr")
library("tidyverse")
library("RSQLite")
library("quantmod")
library("TTR")
# library("")
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(plotly)
library(tidyverse)
library(tidyquant)

# tagesaktuelle daten
tickers <- c("RKT.L", "FRES.L", "PFC.L", "GSK.L", "NWG.L")
benchmarks <- c("^NDX","^GSPC")

prices <- tq_get(tickers, 
                 get  = "stock.prices",
                 from = today()-months(12),
                 to   = today(),
                 complete_cases = F) %>%
  select(symbol,date,close)

bench <- tq_get(benchmarks,
                get  = "stock.prices",
                from = today()-months(12),
                to   = today()) %>%
  select(symbol,date,close)

# -----------------------------------------------------
# UI
#------------------------------------------------------
ui <- fluidPage(
  
  # app theme and title
  theme = shinytheme("slate"),
  # titlePanel(),
  
  # navbar
  navbarPage(
    
    # navbar options
    "ADS Financial Dashboard Group 4",
    theme = shinytheme("slate"),
    
    # General View - theme and title
    tabPanel("General View",
             
             # sidebar with a slider input for number of bins 
             sidebarLayout(
               sidebarPanel(
                 
                 ## conditionalPanel() functions for selected tab
                 conditionalPanel(condition="input.tabselected==1", 
                                  sliderInput("bins",
                                              "Number of bars:",
                                              min = 0,
                                              max = 50,
                                              value = 30,
                                              step = 5)),
                 
                 conditionalPanel(condition="input.tabselected==2",
                                  # width = 3,
                                  # Let user pick stocks
                                  pickerInput(inputId = "stocks",
                                              label = h4("Stocks"),
                                              choices = c("Reckitt"          = tickers[1], 
                                                          "Fresnillo"        = tickers[2],
                                                          "Petrofac"         = tickers[3],
                                                          "GlaxoSmithKline"  = tickers[4],
                                                          "NatWest"          = tickers[5]),
                                              selected = tickers,   
                                              options = list(`actions-box` = TRUE), 
                                              multiple = T),
                                  
                                  # Pick time period
                                  radioButtons("period",
                                               label = h4("Period"),
                                               choices = list("1 month" = 1,
                                                              "3 months" = 2,
                                                              "6 months" = 3,
                                                              "12 months" = 4,
                                                              "YTD" = 5), 
                                               selected = 4),
                                  
                                  # Pick benchmark
                                  radioButtons("benchmark", 
                                               label = h4("Benchmark"),
                                               choices = list("SP500" = 1,
                                                              "Nasdaq100" = 2, 
                                                              "None" = 3),
                                               selected = 3)),
                 
                 conditionalPanel(condition="input.tabselected==3",
                                  h2("uiOutput(\"varx\"), uiOutput(\"vary\")"))
                 
               ),
               
               # mainPanel with tabPanels of TABSET
               mainPanel(
                 tabsetPanel(
                   tabPanel("Example Histogram", value = 1, plotOutput("distPlot")), 
                   tabPanel("Stock Analysis", value = 2, plotlyOutput("stockPlot", height = "200%", width = "100%")), 
                   tabPanel("Return on Investment", value = 3, "plotOutput(\"roiPlot\")"),
                   id = "tabselected"
                 )
               )
             )
    ),
    
    ## further tabPanels of TABSET
    # Detailed View - theme and title
    tabPanel("Detailed View", "platzhalter"),
    
    # Impressum
    tabPanel("Impressum", "xoxo Kevin, Nils und Marvin")
  )
)

# -----------------------------------------------------
# SERVER
#------------------------------------------------------
server <- function(input, output, session) {
  
  ## examplePlot
  # output histogram
  output$distPlot <- renderPlot({
    
    # hand over data and generate bins based on input$bins
    x    <- demo_stocks$close
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white',
         xlab = 'Price',
         main = 'Histogram of closing prices')
  })
  
  ## stockPlot
  # server logic based on user input
  observeEvent(c(input$period,input$stocks,input$benchmark), {
    
    prices <- prices %>%
      filter(symbol %in% input$stocks)
    
    if (input$period == 1) {
      prices <- prices %>%
        filter(
          date >= today()-months(1)) }
    
    if (input$period == 2) {
      prices <- prices %>%
        filter(date >= today()-months(3)) }
    
    if (input$period == 3) {
      prices <- prices %>%
        filter(date >= today()-months(6)) }
    
    if (input$period == 5) {
      prices <- prices %>%
        filter(year(date) == year(today())) }
    
    if (input$benchmark == 1) {
      bench <- bench %>%
        filter(symbol=="^GSPC",
               date >= min(prices$date))
      prices <- rbind(prices,bench) }
    
    if (input$benchmark == 2) {
      bench <- bench %>%
        filter(symbol=="^NDX",
               date >= min(prices$date))
      prices <- rbind(prices,bench) }
    
    # output histogram
    output$stockPlot <- renderPlotly({
      ggplotly(prices %>%
                 group_by(symbol) %>%
                 mutate(init_close = if_else(date == min(date),close,NA_real_)) %>%
                 mutate(value = round(100 * close / sum(init_close,na.rm=T),1)) %>%
                 ungroup() %>%
                 ggplot(aes(date, value,colour = symbol)) +
                 geom_line(size = 1, alpha = .9) +
                 # uncomment the line below to show area under curves
                 #geom_area(aes(fill=symbol),position="identity",alpha=.2) +
                 theme_minimal(base_size=16) +
                 theme(axis.title=element_blank(),
                       plot.background = element_rect(fill = "#1C1E22"),
                       panel.background = element_rect(fill = "#1C1E22"),
                       panel.grid = element_blank(),
                       legend.title = element_text(colour = "#1C1E22"),
                       legend.text = element_text(colour = "white"),
                       axis.line = element_line(colour = "white"),
                       axis.text = element_text(colour = "white"))
      )
    })
  })
  
  ## roiPlot
  # output histogram
  output$roiPlot <- renderPlot({
    
  })
  
}

# run the app
shinyApp(ui = ui, server = server)