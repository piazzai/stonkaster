header <- dashboardHeader(title = site$title, authorInfo)

menuArima <- menuItem(
    "AutoARIMA",
    icon = shiny::icon("line-chart"),
    textInput("ticker", "Ticker", placeholder = "Start typing..."),
    dateInput(
        "date",
        "Forecast from",
        dateSet$default,
        dateSet$min,
        dateSet$max,
        "MM d, yyyy",
        weekstart = 1,
        daysofweekdisabled = c(0, 6)
    ),
    selectizeInput("train", "Training period", trainMenu()),
    selectizeInput("horizon", "Time horizon", horizonMenu()),
    div(
        class = "action",
        actionButton("arima", "Forecast"),
        actionButton("reset", "Clear")
    ),
    startExpanded = T
)

sidebar <-
    dashboardSidebar(sidebarMenu(menuArima), sidebarSitePanel)

body <-
    dashboardBody(
        fluidRow(
            valueBoxOutput("boxTrain", width = 6),
            valueBoxOutput("boxHorizon", width = 6)
        ),
        fluidRow(uiOutput("boxResult")),
        fluidRow(uiOutput("boxBrowse")),
        tags$head(
            tags$link(
                rel = "stylesheet",
                type = "text/css",
                href = googleApi(font)
            ),
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.min.css")
        )
    )

ui <- dashboardPage(header, sidebar, body)
