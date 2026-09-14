library(shiny)
library(forecast)
function(input, output) {
  data_l<-reactive({
    inFile <- input$TextFile
    if (is.null(inFile))
      return(NULL)
    data<-ts(scan(inFile$datapath,skip=input$skipper),start=c(input$year,input$month),
           frequency = input$frequency)
    return(data)
    })

  output$contents1 <- renderPlot({
    data<-data_l()
    M2=auto.arima(data)
    M2F=forecast(M2,h=12)
    Acc <- accuracy(M2F)
    write.csv(Acc,"Accuracy Forecast.csv")
    write.csv(M2F,"Hasil Forecast.csv")
    plot(M2F,main="ARIMA Forecast")

  })
  output$contents2 <- renderPlot({
    data<-data_l()
    tsdisplay(diff(data,lag=1,differences = 1),lag.max=12,main="Is Data Stationary?")  })
}
