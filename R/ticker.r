# [FUNCTIONS] --------------------------------------------------------------
# - standardize tickers ---------------------------------------------------
fun_b3_ticker <- function(chr_ticker){

  # arguments validation
  stopifnot(
    "'chr_ticker' must be a character string." =
      is.character(chr_ticker)
  )

  # standardize ticker
  str_replace(
    chr_ticker
    , '(\\d)[^0-9]+$'
    , '\\1'
  ) -> chr_ticker

  # output
  return(chr_ticker)

}

# - ticker type (int code) --------------------------------------------
fun_b3_ticker_type <- function(chr_ticker){

  # arguments validation
  stopifnot(
    "'chr_ticker' must be a character string." =
      is.character(chr_ticker)
  )

  # parse ticker number
  as.integer(
    str_remove_all(
      chr_ticker
      , '\\D'
    )
  ) -> int_ticker

  # valid b3 ticker type
  int_ticker[
    str_length(
      int_ticker
    ) > 2
  ] <- NA

  # output
  return(int_ticker)

}


