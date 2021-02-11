server <- function (input, output, session) {
    font_add_google("Fira Sans", family = "fira")
    result <- reactiveValues(arima = NULL)
    
    observeEvent(input$reset, {
        updateTextInput(session, "ticker", value = "")
        updateDateInput(session, "date", value = dateDefault())
        updateSelectInput(session, "train", selected = "")
        updateSelectInput(session, "horizon", selected = "")
        result$arima <- NULL
    })
    
    output$boxTrain <- renderValueBox({
        req(input$train != "")
        valueBox(
            format(input$date - as.numeric(input$train), "%b %e, %Y"),
            "Start of training",
            icon("cogs"),
            "orange"
        )
    })
    
    output$boxHorizon <- renderValueBox({
        req(input$horizon != "")
        valueBox(
            format(input$date + as.numeric(input$horizon), "%b %e, %Y"),
            "End of forecast",
            icon("low-vision"),
            "purple"
        )
    })
    
    observeEvent(input$arima, {
        if (!detectMissing(input, session)) {
            price <- tickerData(input)
            training <-
                (input$date - as.numeric(input$train)):input$date
            fit <- log(price[date %in% training]$close) %>%
                auto.arima(stepwise = F, approximation = F)
            cast <- forecast(fit, h = input$horizon, level = 95)
            castPlot <- arimaPlot(input, price, cast) %>%
                ggplotly(dynamicTicks = T) %>% layout(font = plotFont)
            castCalc <- arimaCalc(cast)
            result$arima <- tabBox(
                title = "Forecast",
                tabPanel("Plot", castPlot),
                tabPanel("P/L Calculator", castCalc),
                width = 12
            )
        }
    })
    
    output$boxResult <- renderUI({
        result$arima
    })
}
