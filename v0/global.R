library(shiny)
library(shinydashboard)
library(shinyjs)
library(dplyr)
library(data.table)
library(showtext)
library(rvest)
library(stringr)
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
    y$sendCustomMessage(type = "alert",
                        message = "You must provide a ticker")
    return(T)
  } else {
    if (x$train == "") {
      y$sendCustomMessage(type = "alert",
                          message = "You must provide a training period")
      return(T)
    } else {
      if (x$horizon == "") {
        y$sendCustomMessage(type = "alert",
                            message = "You must provide a time horizon")
        return(T)
      } else
        return(F)
    }
  }
}

cleanCell <- function (x) {
  as.character(x) %>% str_extract(">.+<") %>%
    str_remove_all(">|<") %>% str_squish() %>%
    str_replace_all("&amp;", "&")
}

matchTicker <- function (x) {
  tr <- paste0("https://finance.yahoo.com/lookup?s=", x$ticker) %>%
    read_html() %>% html_nodes("tr")
  td <- html_nodes(tr[-1], "td")
  c1 <- td[grepl("col0", td)] %>% html_nodes("a") %>%
    html_attr("data-symbol")
  c2 <- td[grepl("col1", td)] %>% cleanCell()
  c3 <- td[grepl("col4", td)] %>% cleanCell()
  c4 <- td[grepl("col5", td)] %>% cleanCell()
  data.table(
    Ticker = c1,
    Name = c2,
    Type = c3,
    Exchange = c4
  )
}

checkTicker <- function (x, y) {
  t <- try(getSymbols(
    x$ticker,
    from = as.Date("1970-01-01"),
    to = Sys.Date(),
    auto.assign = F
  ))
  if ("try-error" %in% class(t)) {
    y$sendCustomMessage(type = "alert",
                        message = "No data exists for this ticker")
    return(F)
  } else
    return(T)
}

footYahoo <- function (x) {
  p("If you cannot find a ticker, try",
    a(
      "Yahoo Search",
      href = paste0("https://search.yahoo.com/search?p=", x$ticker)
    ))
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

checkData <- function (x, y, z) {
  if (nrow(x[date %in% y]) == 0) {
    z$sendCustomMessage(type = "alert",
                        message = "No data exists for this ticker during the training period")
    return(F)
  } else
    return(T)
}

arimaPlot <- function (x, y, z) {
  training <- (x$date - as.numeric(x$train)):x$date
  ts1 <-
    data.table(Date = x$date, rep(y[date == x$date]$close, 3) %>% t()) %>%
    rbind(
      data.table(
        Date = x$date + 1:x$horizon,
        V1 = as.numeric(z$mean) %>% exp(),
        V2 = as.numeric(z$lower) %>% exp(),
        V3 = as.numeric(z$upper) %>% exp()
      )
    )
  colnames(ts1)[-1] <-
    c("Price", "Lower 95% CI", "Upper 95% CI")
  ts2 <- tail(y[date <= x$date], nrow(ts1)) %>%
    rbind(y[date %in% ts1$Date]) %>%
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


arimaMod <- function(x, y) {
  div(class = "about-tab",
      div(
        class = "model-info",
        p(
          "AutoARIMA is an algorithm written by",
          a("Rob J. Hyndman", href = "https://robjhyndman.com"),
          "that automatically fits an",
          em("autoregressive integrated moving average"),
          "(ARIMA) model to a time series. To explain the price evolution of",
          x$ticker,
          "between",
          format(x$date - as.numeric(x$train), "%B %e, %Y,"),
          "and",
          format(x$date, "%B %e, %Y,"),
          "the algorithm fitted a model",
          paste0(as.character(y) %>% str_squish(), ".")
        ),
        a(href = "https://en.wikipedia.org/wiki/Autoregressive_integrated_moving_average",
          shiny::icon("question-circle"),
          "What does this mean?")
      ),
      div(class = "model-info"))
}

# TO DO: pl calculator, info author, disclaimer, zoomable plot
