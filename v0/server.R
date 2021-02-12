server <- function (input, output, session) {
    result <- reactiveValues(arima = NULL)
    
    observeEvent(input$reset, {
        updateTextInput(session, "ticker", value = "")
        updateDateInput(session, "date", value = dateSet$default)
        updateSelectInput(session, "train", selected = "")
        updateSelectInput(session, "horizon", selected = "")
        result$arima <- NULL
    })
    
    output$tabBrowse <- renderTable({
        browseTicker(input)
    }, spacing = "xs", striped = T, width = "100%")
    
    output$boxBrowse <- renderUI({
        req(input$ticker != "")
        box(
            title = "Similar tickers",
            status = "primary",
            collapsible = T,
            width = 12,
            tableOutput("tabBrowse")
        )
    })
    
    output$boxTrain <- renderValueBox({
        req(input$train != "")
        valueBox(
            format(input$date - as.numeric(input$train), "%b %e, %Y"),
            "Start of training",
            shiny::icon("cogs"),
            "olive"
        )
    })
    
    output$boxHorizon <- renderValueBox({
        req(input$horizon != "")
        valueBox(
            format(input$date + as.numeric(input$horizon), "%b %e, %Y"),
            "End of forecast",
            shiny::icon("low-vision"),
            "purple"
        )
    })
    
    observeEvent(input$arima, {
        if (checkInput(input)) {
            price <- tickerData(input)
            train <-
                (input$date - as.numeric(input$train)):input$date
            if (checkData(price, train)) {
                showModal(modalDialog(
                    "Calculating...",
                    size = "l",
                    footer = NULL
                ))
                fit <- log(price[date %in% train]$close) %>%
                    auto.arima(stepwise = F,
                               approximation = F)
                cast <-
                    forecast(fit, h = input$horizon, level = 95)
                castPlot <- arimaPlot(price, train, cast)
                castMod <- arimaMod(input, fit)
                result$arima <- tabBox(
                    title = toupper(input$ticker),
                    tabPanel("Forecast", castPlot),
                    tabPanel("Details", castMod),
                    width = 12
                )
                removeModal()
            }
        }
    })
    
    output$boxResult <- renderUI({
        result$arima
    })
}
