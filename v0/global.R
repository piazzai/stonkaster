library(shiny)
library(shinydashboard)
library(shinyjs)
library(dplyr)
library(data.table)
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
    version = "v0.3.3",
    url = "https://piazzai.shinyapps.io/stonkaster",
    repo = "https://github.com/piazzai/stonkaster",
    license = "https://github.com/piazzai/stonkaster/blob/master/LICENSE"
  )

author <-
  list(name = "Michele Piazzai",
       url = "https://piazzai.github.io")

font <- list(name = "Fira Sans", weights = c(400, 900))

textSet <-
  list(
    infobox = c("Stonkaster is a Shiny app developed by", "Code is available at:"),
    disclaimer = "This is an educational tool. Forecasts do not represent financial advice.",
    msgBlankTicker = "You must provide a ticker.",
    msgBlankTrain = "You must provide a training period.",
    msgBlankHorizon = "You must provide a time horizon.",
    msgNoTicker = "This ticker cannot be found.",
    msgNoTrain = "No data exists during the training period.",
    arima = c(
      "AutoARIMA is an algorithm written by",
      "Rob Hyndman",
      "that automatically fits an",
      "autoregressive integrated moving average",
      "(ARIMA) model to a time series. To explain the price evolution of",
      "between",
      "and",
      "the algorithm selected a model"
    )
  )

dateSet <- list(min = "1970-01-01",
                max = Sys.Date(),
                default = {
                  w <- Sys.Date() - 1:7
                  w[wday(w) %in% 2:6] %>% head(1)
                })

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

# UI elements

authorInfo <-
  dropdownMenu(
    type = "messages",
    badgeStatus = NULL,
    icon = shiny::icon("info"),
    headerText = p(h5(
      textSet$infobox[1],
      a(href = author$url, paste0(author$name, ".")),
      textSet$infobox[2]
    ), h5(
      a(
        href = site$repo,
        shiny::icon("github"),
        str_replace(site$repo, "https://", "&nbsp;") %>% HTML()
      )
    ))
  )

sidebarSitePanel <-
  div(class = "site-panel",
      p(a(
        href = site$url, paste(site$title, site$version)
      ),
      br(),
      h5(em(textSet$disclaimer))),
      h5(a(
        href = site$license,
        shiny::icon("balance-scale"),
        HTML("&nbsp;License")
      )))

# Queries

googleApi <- function (x) {
  paste0(
    "https://fonts.googleapis.com/css2?family=",
    gsub(" ", "+", x$name),
    ":wght@",
    paste(x$weights, collapse = ";"),
    "&display=swap"
  )
}

# Warnings

checkInput <- function (x) {
  if (x$ticker == "") {
    showModal(
      modalDialog(
        textSet$msgBlankTicker,
        title = "Error",
        size = "l",
        footer = NULL,
        easyClose = T
      )
    )
    return(F)
  } else {
    t <- try(getSymbols(
      x$ticker,
      from = dateSet$min,
      to = dateSet$max,
      auto.assign = F
    ))
    if ("try-error" %in% class(t)) {
      showModal(
        modalDialog(
          textSet$msgNoTicker,
          title = "Error",
          size = "l",
          footer = NULL,
          easyClose = T
        )
      )
      return(F)
    } else {
      if (x$train == "") {
        showModal(
          modalDialog(
            textSet$msgBlankTrain,
            title = "Error",
            size = "l",
            footer = NULL,
            easyClose = T
          )
        )
        return(F)
      } else {
        if (x$horizon == "") {
          showModal(
            modalDialog(
              textSet$msgBlankHorizon,
              title = "Error",
              size = "l",
              footer = NULL,
              easyClose = T
            )
          )
          return(F)
        } else
          return(T)
      }
    }
  }
}

checkData <- function (x, y) {
  if (nrow(x[date %in% y]) < 2) {
    showModal(modalDialog(
      textSet$msgNoTrain,
      title = "Error",
      size = "l",
      footer = NULL,
      easyClose = T
    ))
    return(F)
  } else
    return(T)
}

# Ticker search

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
  c3 <- ifelse(c3 == "CRYPTOCURRENCY", "Cryptocurrency", c3)
  c4 <- td[grepl("col5", td)] %>% cleanCell()
  data.table(
    Ticker = c1,
    Name = c2,
    Type = c3,
    Exchange = c4
  )
}

yahooLink <- function (x) {
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
      from = dateSet$min,
      to = dateSet$max,
      auto.assign = F,
      return.class = "data.frame"
    )
  colnames(df) <- c("open", "high", "low", "close", "vol", "adj")
  df$date <- rownames(df) %>% as.Date()
  data.table(date = as.Date(min(df$date):max(df$date))) %>%
    left_join(df, by = "date") %>% na.locf()
}

# AutoARIMA

arimaPlot <- function (x, y, z) {
  ts1 <-
    data.table(Date = as.Date(tail(y, 1)),
               rep(x[date == tail(y, 1)]$close, 3) %>% t()) %>% rbind(
                 data.table(
                   Date = (tail(y, 1) + 1:length(z$mean)) %>% as.Date(),
                   V1 = as.numeric(z$mean) %>% exp() %>% round(3),
                   V2 = as.numeric(z$lower) %>% exp() %>% round(3),
                   V3 = as.numeric(z$upper) %>% exp() %>% round(3)
                 )
               )
  colnames(ts1)[-1] <-
    c("Price", "Lower 95% CI", "Upper 95% CI")
  ts2 <- tail(x[date <= tail(y, 1)], nrow(ts1)) %>%
    rbind(x[date %in% ts1$Date]) %>%
    distinct() %>% select(Date = date, Price = close)
  p <- ggplot() +
    geom_ribbon(
      aes(Date, ymin = `Lower 95% CI`, ymax = `Upper 95% CI`),
      fill = "blue",
      alpha = .2,
      ts1
    ) +
    geom_line(aes(Date, Price), lty = 3, col = "#800080", ts1) +
    geom_line(aes(Date, Price), ts2)
  ggplotly(p, dynamicTicks = T)
}

arimaMod <- function(x, y) {
  div(
    class = "model-info",
    p(
      textSet$arima[1],
      a(textSet$arima[2], href = "https://robjhyndman.com"),
      textSet$arima[3],
      em(textSet$arima[4]),
      textSet$arima[5],
      x$ticker,
      textSet$arima[6],
      format(x$date - as.numeric(x$train), "%B %e, %Y,"),
      textSet$arima[7],
      format(x$date, "%B %e, %Y,"),
      textSet$arima[8],
      paste0(as.character(y) %>% str_squish(), ".")
    ),
    a(
      href = "https://en.wikipedia.org/wiki/Autoregressive_integrated_moving_average",
      shiny::icon("question-circle"),
      HTML("What does this mean?")
    )
  )
}
