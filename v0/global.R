library(shiny)
library(shinydashboard)
library(shinyjs)
library(dplyr)
library(data.table)
library(showtext)
library(quantmod)
library(forecast)
library(ggplot2)
library(plotly)

# Configuration

site <-
  list(
    title = "Stonkaster",
    version = "v0.3.0",
    repo = "https://github.com/piazzai/stonkaster",
    url = "https://piazzai.shinyapps.io/stonkaster",
    license = "MIT License"
  )

author <-
  list(
    name = "Michele Piazzai",
    email = "michele.piazzai@uc3m.es",
    website = "https://piazzai.github.io",
    github = "https://github.com/piazzai"
  )

font <- list(name = "Fira Sans", weights = c(400, 700, 900))

plotFont <- list(family = "Fira Sans")

# Functions

googleFont <- function () {
  paste0(
    "https://fonts.googleapis.com/css2?family=",
    gsub(" ", "+", font$name),
    ":wght@",
    paste(font$weights, collapse = ";"),
    "&display=swap"
  )
}

sidebarSitePanel <- function () {
  div(class = "site-panel",
      p(site$title, site$version),
      a(href = site$repo, shiny::icon("github"), "GitHub"))
}

dateDefault <- function () {
  x <- Sys.Date() - 1:7
  x[wday(x) %in% 2:6] %>% head(1)
}

dateMin <- function () {
  as.Date("1970-01-01")
}

dateMax <- function () {
  Sys.Date()
}

trainMenu <- function () {
  c(
    "Choose" = "",
    "One month" = 30,
    "Six months" = 180,
    "One year" = 360,
    "Five years" = 1800,
    "Ten years" = 3600
  )
}

horizonMenu <- function () {
  c(
    "Choose" = "",
    "One day" = 1,
    "One week" = 7,
    "Two weeks" = 14,
    "One month" = 30,
    "Six months" = 180
  )
}

detectMissing <- function (x, y) {
  if (x$ticker == "") {
    y$sendCustomMessage(type = "testmessage",
                        message = "You must provide a ticker")
    return(T)
  } else {
    if (x$train == "") {
      y$sendCustomMessage(type = "testmessage",
                          message = "You must provide a training period")
      return(T)
    } else {
      if (x$horizon == "") {
        y$sendCustomMessage(type = "testmessage",
                            message = "You must provide a time horizon")
        return(T)
      } else return(F)
    }
  }
}

tickerData <- function (x) {
  df <-
    getSymbols(
      x$ticker,
      from = as.Date("1970-01-01"),
      to = Sys.Date(),
      auto.assign = F,
      return.class = "data.frame"
    )
  colnames(df) <- c("open", "high", "low", "close", "vol", "adj")
  df$date <- rownames(df) %>% as.Date()
  data.table(date = as.Date(min(df$date):max(df$date))) %>%
    left_join(df, by = "date") %>% na.locf()
}

arimaPlot <- function (x, dt, f) {
  training <- (input$date - as.numeric(input$train)):input$date
  ts1 <-
    data.table(Date = x$date, rep(price[date == x$date]$close, 3) %>% t()) %>%
    rbind(
      data.table(
        Date = x$date + 1:x$horizon,
        V1 = as.numeric(f$mean) %>% exp(),
        V2 = as.numeric(f$lower) %>% exp(),
        V3 = as.numeric(f$upper) %>% exp()
      )
    )
  colnames(ts1)[-1] <-
    c("Price", "Lower 95% CI", "Upper 95% CI")
  ts2 <- tail(dt[date <= x$date], nrow(ts1)) %>%
    rbind(dt[date %in% ts1$Date]) %>%
    distinct() %>% select(Date = date, Price = close)
  ggplot() +
    geom_ribbon(
      aes(Date, ymin = `Lower 95% CI`, ymax = `Upper 95% CI`),
      fill = "blue",
      alpha = .2,
      ts1
    ) +
    geom_line(aes(Date, Price), lty = 3, col = "#800080", ts1) +
    geom_line(aes(Date, Price), ts2)
}

arimaCalc <- function (f) {
  div(
    p("Average price"),
    p("Quantity"),
    p("Crunch")
  )
}

# TO DO: info arima, info author, disclaimer, model details tab
