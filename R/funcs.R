#' Hitting the Binance API to get the most recent price of Bitcoin in USD
#'
#' This is a wrapper around \code{binancer} package...
#' @export
#' @param retried Number of retries in the previous step
#' @importFrom binancer binance_coins_prices
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
