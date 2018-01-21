library(shiny)
library(shinythemes)

ui <- fluidPage(
  theme = shinytheme("cyborg"),
  
  fluidRow(
    column(4,offset=2,
  textInput(inputId = "lastprice",
            label=h4("Input the latest price"),
            value="0")
  )),
  
  fluidRow(
    column(4,offset = 2,
  actionButton(inputId = "refresh1",
               label = "XAU/USD"),
  
  #sliderInput(inputId = "num", 
  #            label = "Choose a number", 
  #            value = 25, min = 1, max = 100),
  #plotOutput("goldplot"),
  
  actionButton(inputId = "refresh4",
               label = "XAG/USD"),
  actionButton(inputId = "refresh2",
               label = "GBP/USD"),
  actionButton(inputId = "refresh3",
               label = "USD/JPY")
  )),
  
  fluidRow(),
  fluidRow(),
  fluidRow(),
  fluidRow(
  #plotOutput("goldplot"),
  plotOutput("outplot")
))

server <- function(input, output) {
  require(quantmod)
  require(curl)
  require(xts)
  
  #latest <- eventReactive(input$refresh1, input$lastprice)
  #latestprice <- isolate({latest()})
  
  
  
  observeEvent(input$refresh1, {
    getSymbols("XAU/USD",src="oanda")
    latestprice <- isolate(input$lastprice)
    if(latestprice>0) {
      newline <- xts(data.frame(XAU.USD=latestprice),order.by = Sys.Date())
      XAUUSD <- rbind(XAUUSD, newline)
    }
    
    output$outplot <- renderPlot({
      lineChart(XAUUSD,TA=c(addEMA(20),addBBands()))
    #addSMA(20)
    
    #addBBands() 
    #addEMA(20)
    #addCCI()
    })
  })
  
  observeEvent(input$refresh2, {
    getSymbols("GBP/USD",src="oanda")
    latestprice <- isolate(input$lastprice)
    if(latestprice>0) {
      newline <- xts(data.frame(GBP.USD=latestprice),order.by = Sys.Date())
      GBPUSD <- rbind(GBPUSD, newline)
    }
    output$outplot <- renderPlot({
      lineChart(GBPUSD,TA=c(addEMA(20),addBBands()))
      #addEMA(20)
      #addBBands()
    })
  })  
    
    observeEvent(input$refresh3, {
      getSymbols("USD/JPY",src="oanda")
      latestprice <- isolate(input$lastprice)
      if(latestprice>0) {
        newline <- xts(data.frame(USD.JPY=latestprice),order.by = Sys.Date())
        USDJPY <- rbind(USDJPY, newline)
      }
      output$outplot <- renderPlot({
        lineChart(USDJPY,TA=c(addEMA(20),addBBands()))
        #addEMA(20)
        #addBBands()
      })  
  })
  
    observeEvent(input$refresh4, {
      getSymbols("XAG/USD",src="oanda")
      latestprice <- isolate(input$lastprice)
      if(latestprice>0) {
        newline <- xts(data.frame(XAG.USD=latestprice),order.by = Sys.Date())
        XAGUSD <- rbind(XAGUSD, newline)
      }
      output$outplot <- renderPlot({
        lineChart(XAGUSD,TA=c(addEMA(20),addBBands()))
        #addEMA(20)
        #addBBands()
      })  
    })
    
  #output$goldplot <- renderPlot({
  #  getSymbols("XAU/USD",src="oanda")
    
    #tail(XAUUSD)
  #  lineChart(XAUUSD)
    #addSMA(20)
  #  addEMA(20)
  #  addBBands()
  #})
  #output$scatter <- renderPlot({
  #  plot(rnorm(input$num), rnorm(input$num))
  #})
}

shinyApp(ui = ui, server = server)