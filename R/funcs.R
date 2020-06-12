#' Hitting the Binance API to get the most recent price of Bitcoin in USD
#'
#' This is a wrapper around \code{binancer} package...
#' @export
#' @param retried Number of retries in the previous step
#' @importFrom binancer binance_coins_prices
#' @import data.table
get_bitcoin_price <- function(retried = 0) {
  tryCatch(binance_coins_prices()[symbol == 'BTC']$usd,
           error = function (e) {
             # exponential backoff retries
             Sys.sleep(1 + retried ^ 2)
             get_bitcoin_price(retried = retried + 1)
           })
}

#' Formatting numbers as Hungarian Forints
#'
#' Wrapper around \code{scales} package
#' @export
#' @param x number
#' @importFrom scales dollar
forint <- function(x) {dollar(x, suffix = "Ft", prefix = NULL)}

#' Get exchange rates between two currencies in a given time frame
#'
#' Hitting the exchangerates.io API to get historical exchange rate from today n days backwards
#' @export
#' @param to Currency to convert TO
#' @param from Currency to convert FROM
#' @param n_days Number of days from current date
#' @param retried Number of retries in the previous step
#' @import data.table
convert_currency <- function(to, from, n_days, retried = 0) {

  date_from <- Sys.Date() - n_days
  date_to <- Sys.Date()

  x <- seq(date_from, date_to, by = 1) # create days vector within period
  day_of_week <- weekdays(x) # return what days are in the period

  # check if there is ANY weekday
  currency_week_days <- day_of_week[day_of_week %in% c("Monday", "Tuesday", "Wednesady", "Thursday", "Friday")]

  # second condition checks if the only workday is today's Monday and the rest of the period falls
  # within the weekend: in this case there is nor closing rate for today nor for the weekend
  if (length(currency_week_days) > 0 & !(weekdays(Sys.Date()) == "Monday" & n_days < 3)) {

    tryCatch(
      query_rates(to=to, from=from, n_days=n_days, retried = retried),
      error = function(e) {
        ## exponential backoff retries
        Sys.sleep(1 + retried^2)
        query_rates(to=to, from=from, n_days=n_days, retried = retried + 1)
      })

  } else {
    print("Only weekend days were queried or the only weekday is today's Monday for which there is
          no closing exchange rate")
  }
}

#' Hitting the exchangerates.io API to get historical exchange rates between two currencies
#' in a given time period
#'
#' @export
#' @param to Currency to convert TO
#' @param from Currency to convert FROM
#' @param n_days Number of days from current date
#' @param retried Number of retries in the previous step
#' @importFrom httr GET
#' @importFrom httr content
#' @import data.table
query_rates <- function(to, from, n_days, retried=0) {
  currency <- GET(
    "https://api.exchangeratesapi.io/history",
    query = list(
      start_at = Sys.Date() - n_days,
      end_at   = Sys.Date(),
      base     = to,
      symbols  = from ))
  currency <- content(currency)$rates # extract data from json

  currency <- data.table(date = as.Date(names(currency)),
                         rate = unlist(currency))
  setorder(currency, date)
  return(currency)
}
