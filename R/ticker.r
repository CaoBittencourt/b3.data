# [FUNCTIONS] --------------------------------------------------------------
# - Standardize tickers ---------------------------------------------------
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

fun_b3_clean <- function(
    list_chr_path_transactions,
    list_chr_path_events,
    list_chr_path_events_remove = NULL,
    list_chr_path_events_add = NULL
){

  # arguments validation
  stopifnot(
    "'list_chr_path_transactions' must be a list of paths to B3 financial transactions .xlsx files." =
      all(
        is.list(list_chr_path_transactions),
        sapply(
          list_chr_path_transactions
          , is.character
        )
      )
  )

  stopifnot(
    "'list_chr_path_events' must be a list of paths to B3 financial events .xlsx files." =
      all(
        is.list(list_chr_path_events),
        sapply(
          list_chr_path_events
          , is.character
        )
      )
  )

  # apply data cleaning functions
  Map(
    function(b3_files, b3_fun){
      tryCatch(
        expr = {do.call(b3_fun, b3_files)}
        , error = function(e){NULL}
      )
    }
    , b3_files = list(
      'transactions' = list(
        list_chr_path_transactions
      )
      , 'events' = list(
        list_chr_path_events,
        list_chr_path_events_remove,
        list_chr_path_events_add
      )
    )
    , b3_fun = list(
      fun_b3_clean_transactions,
      fun_b3_clean_events
    )
  ) -> list_b3_data

  # output
  return(list_b3_data)

}