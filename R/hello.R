#' Hitting the Binance API to get the most recent price of a Bitcoin in USD
#'
#' This is a wrapper arond the \code{binancer} package
#' @export
#' @param retried the number of retries previously done before the exponential backoff sleep
#' @importFrom binancer binance_coins_prices
get_bitcoin_price <- function(retried = 0){
  tryCatch(
    binance_coins_prices()[symbol == 'BTC', usd],
    error = function(e) {
      ## exponential backoff retries
      Sys.sleep(1 + retried^2)
      get_bitcoin_price(retried = retried + 1)
    })
}

#' Formatter function for Hungarian Forint
#' @param x number
#' @export
#' @importFrom scales dollar
forint <- function(amt){
  dollar(amt, prefix = '', suffix = ' HUF')
}

#TODO
