#this is required to load rjava in mac
#dyn.load('/Library/Java/JavaVirtualMachines/jdk-9.0.1.jdk/Contents/Home/lib/server/libjvm.dylib')

library(shiny)
require(shinythemes)
require(dygraphs)

consolidate <- function(df,locfile,fakeline) {
  require(xlsx)
  require(xts)
  locxls <- read.xlsx2(locfile,1,stringsAsFactors=F,colIndex = c(1,2),colClasses = c("character","numeric"))
  colnames(locxls)[1] <- "Date"
  locxts <- xts(locxls,order.by = as.Date(locxls$Date))[,-1]
  if(!is.xts(df)) {
    print("Error: df for function consolidate is not an xts object.")
    break
  }
  apdrange <- paste0((max(index(locxts))+1),"/")
  outxts <- rbind(locxts,df[apdrange,])
  outxts <- cbind(outxts,BBands(outxts)[,-4])
  if(fakeline) savexts <- outxts[-nrow(outxts),]
   else savexts <- outxts
  write.xlsx2(savexts,locfile)
  return(outxts)
}

#addsig <- function(xts1) {
#  outdata <- cbind(xts1,BBands(xts1)[,-4])
#  return(outdata)
#}

draw <- function(xts1, ttlname) {
  require(quantmod)
  require(dygraphs)
  
  dygraph(xts1,main=paste(ttlname,"Trends")) %>% 
    dySeries(ttlname,color="black") %>% 
    dySeries("dn",color="brown") %>% 
    dySeries("mavg",color="green") %>% 
    dySeries("up",color="blue") %>% 
    dyLegend(show="always",width=600,hideOnMouseOut=F) %>% 
    dyRangeSelector()

}

ui <- fluidPage(
  theme = shinytheme("simplex"),
  
  titlePanel("Market Turbulance"),
  
  tabsetPanel(
    tabPanel("XAU/USD",
             textInput(inputId = "pricexau",
                       label=h4("Input the latest price"),
                       value="0"),
             actionButton(inputId = "refresh1",
                          label = "XAU/USD"),
             hr(),
             tableOutput("xautable"),
             dygraphOutput("outplot.xau")
             #plotOutput("outplot.xau")
             ),
    tabPanel("XAG/USD",
             textInput(inputId = "pricexag",
                       label=h4("Input the latest price"),
                       value="0"),
             actionButton(inputId = "refresh2",
                          label = "XAG/USD"),
             hr(),
             tableOutput("xagtable"),
             dygraphOutput("outplot.xag")
    ),
    tabPanel("GBP/USD",
             textInput(inputId = "pricegbp",
                       label=h4("Input the latest price"),
                       value="0"),
             actionButton(inputId = "refresh3",
                          label = "GBP/USD"),
             hr(),
             tableOutput("gbptable"),
             dygraphOutput("outplot.gbp")
    ),
    tabPanel("USD/JPY",
             textInput(inputId = "pricejpy",
                       label=h4("Input the latest price"),
                       value="0"),
             actionButton(inputId = "refresh4",
                          label = "USD/JPY"),
             hr(),
             tableOutput("jpytable"),
             dygraphOutput("outplot.jpy")
    )
  )
 
)

server <- function(input, output) {
  require(quantmod)
  require(curl)
  require(xts)
  require(dygraphs)
  
  #latest <- eventReactive(input$refresh1, input$lastprice)
  #latestprice <- isolate({latest()})
  
  
  
  observeEvent(input$refresh1, {
    xauprice <- F
    getSymbols("XAU/USD",src="oanda")
    latestprice <- isolate(input$pricexau)
    if(latestprice>0) {
      xauprice <- T
      newline <- xts(data.frame(XAU.USD=latestprice),order.by = Sys.Date())
      XAUUSD <- rbind(XAUUSD, newline)
    }
    
    XAUUSD <- consolidate(XAUUSD, "XAUUSD.xls",xauprice)
    
    #XAUUSD <- addsig(XAUUSD)
    output$xautable <- renderTable(tail(XAUUSD,2),striped=T,hover=T,bordered = T,rownames = T)
    output$outplot.xau <- renderDygraph({
      draw(XAUUSD,"XAU.USD")
      #lineChart(XAUUSD,TA=c(addEMA(20),addBBands()))
    #addSMA(20)
    
    #addBBands() 
    #addEMA(20)
    #addCCI()
    })
  })
  
  observeEvent(input$refresh2, {
    xagprice <- F
    getSymbols("XAG/USD",src="oanda")
    latestprice <- isolate(input$pricexag)
    if(latestprice>0) {
      xagprice <- T
      newline <- xts(data.frame(XAG.USD=latestprice),order.by = Sys.Date())
      XAGUSD <- rbind(XAGUSD, newline)
    }
    
    XAGUSD <- consolidate(XAGUSD, "XAGUSD.xls",xagprice)
    
    #XAGUSD <- addsig(XAGUSD)
    output$xagtable <- renderTable(tail(XAGUSD,2),striped=T,hover=T,bordered = T,rownames = T)
    output$outplot.xag <- renderDygraph({
      draw(XAGUSD,"XAG.USD")
      #lineChart(XAGUSD,TA=c(addEMA(20),addBBands()))
      #addEMA(20)
      #addBBands()
    })  
  })
  
  observeEvent(input$refresh3, {
    gbpprice <- F
    getSymbols("GBP/USD",src="oanda")
    latestprice <- isolate(input$pricegbp)
    if(latestprice>0) {
      gbpprice <- T
      newline <- xts(data.frame(GBP.USD=latestprice),order.by = Sys.Date())
      GBPUSD <- rbind(GBPUSD, newline)
    }
    GBPUSD <- consolidate(GBPUSD, "GBPUSD.xls",gbpprice)
    #GBPUSD <- addsig(GBPUSD)
    output$gbptable <- renderTable(tail(GBPUSD,2),striped=T,hover=T,bordered = T,rownames = T,digits = 4)
    output$outplot.gbp <- renderDygraph({
      draw(GBPUSD,"GBP.USD")
      #lineChart(GBPUSD,TA=c(addEMA(20),addBBands()))
      #addEMA(20)
      #addBBands()
    })
  })  
    
    observeEvent(input$refresh4, {
      jpyprice <- F
      getSymbols("USD/JPY",src="oanda")
      latestprice <- isolate(input$pricejpy)
      if(latestprice>0) {
        jpyprice <- T
        newline <- xts(data.frame(USD.JPY=latestprice),order.by = Sys.Date())
        USDJPY <- rbind(USDJPY, newline)
      }
      USDJPY <- consolidate(USDJPY, "USDJPY.xls",jpyprice)
      #USDJPY <- addsig(USDJPY)
      output$jpytable <- renderTable(tail(USDJPY,2),striped=T,hover=T,bordered = T,rownames = T)
      output$outplot.jpy <- renderDygraph({
        draw(USDJPY,"USD.JPY")
        #lineChart(USDJPY,TA=c(addEMA(20),addBBands()))
        #addEMA(20)
        #addBBands()
      })  
  })
  

}

shinyApp(ui = ui, server = server)