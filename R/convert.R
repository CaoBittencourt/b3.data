# [FUNCTIONS] -------------------------------------------------------------
# - conversion date -------------------------------------------------------
fun_b3_convert_date <- function(
    df_events_transfers
    , df_convert
){

  # helper function for events cleaning function
  # arguments validated in the cleaning function

  # conversion dates
  df_convert %>%
    inner_join(
      df_events_transfers
      , by = c(
        'new_ticker' =
          'ticker'
      )
      , multiple = 'all'
      , relationship =
        'many-to-many'
    ) %>%
    select(
      convert = date,
      ticker,
      new_ticker,
      prop
    ) %>%
    group_by(
      new_ticker,
      ticker
    ) %>%
    slice(1) %>%
    ungroup() %>%
    group_by(
      ticker
    ) %>%
    mutate(
      prop = 1 / n()
    ) %>%
    ungroup() ->
    df_convert

  # output
  return(df_convert)

}

# - conversion data -------------------------------------------------------
fun_b3_convert_data <- function(
    df_events_transfers
    , df_convert
){

  # helper function for events cleaning function
  # arguments validated in the cleaning function

  # converted ticker data
  df_events_transfers %>%
    inner_join(
      df_convert
    ) %>%
    replicate(
      n = 2
      , simplify = F
    ) %>%
    set_names(c(
      'ticker',
      'new_ticker'
    )) %>%
    bind_rows(
      .id = 'ticker_convert'
    ) %>%
    mutate(
      ticker = if_else(
        ticker_convert ==
          'ticker'
        , ticker
        , new_ticker
      )
    ) %>%
    select(
      -new_ticker
    ) %>%
    bind_rows(
      df_events_transfers %>%
        filter(!(
          ticker %in%
            df_convert$
            ticker
        ))
    ) %>%
    arrange(
      date
    ) %>%
    mutate(
      cycle = if_else(
        is.na(ticker_convert)
        | ticker_convert !=
          'new_ticker'
        , cycle + 1
        , cycle
      )
    ) -> df_events_transfers

  # output
  return(df_events_transfers)

}