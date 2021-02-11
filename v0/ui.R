header <- dashboardHeader(title = site$title)

menuArima <- menuItem(
    "AutoARIMA",
    icon = icon("line-chart"),
    textInput("ticker", "Ticker", placeholder = "e.g. GME"),
    dateInput(
        "date",
        "Forecast from",
        dateDefault(),
        dateMin(),
        dateMax(),
        "MM d, yyyy",
        weekstart = 1,
        daysofweekdisabled = c(0, 6)
    ),
    selectizeInput("train", "Training period", trainMenu()),
    selectizeInput("horizon", "Time horizon", horizonMenu()),
    div(
        class = "action",
        actionButton("arima", "Forecast"),
        actionButton("reset", "Reset")
    ),
    startExpanded = T
)

sidebar <-
    dashboardSidebar(sidebarMenu(menuArima), sidebarSitePanel())

body <-
    dashboardBody(
        fluidRow(
            valueBoxOutput("boxTrain", width = 6),
            valueBoxOutput("boxHorizon", width = 6)
        ),
        uiOutput("boxResult"),
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = googleFont()),
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
            tags$script(src = "message-handler.js")
        )
    )

ui <- dashboardPage(header, sidebar, body)
