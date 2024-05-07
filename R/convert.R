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
      new_ticker
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
  df_convert %>%
    inner_join(
      df_events_transfers
      , multiple = 'all'
      , relationship =
        'many-to-many'
    ) %>%
    mutate(
      ticker = new_ticker
    ) %>%
    select(
      -new_ticker
    ) %>%
    bind_rows(
      df_events_transfers
    ) %>%
    bind_rows(
      df_convert %>%
        select(
          ticker
          , obsolete =
            convert
          , date =
            convert
        ) %>%
        unique() %>%
        mutate(
          qtd = 0,
          price = 0,
          active = F
        )
    ) %>%
    arrange(
      ticker,
      date
    ) %>%
    group_by(
      ticker
    ) %>%
    fill(
      c(
        ticker_type,
        cycle,
        stock,
        prop
      )
    ) %>%
    ungroup() %>%
    mutate(
      cycle = if_else(
        is.na(convert)
        , cycle + 1
        , cycle
      )
    ) -> df_events_transfers

  # output
  return(df_events_transfers)

}

# - conversion stages -----------------------------------------------------
fun_b3_convert_stages <- function(
    df_events_transfers,
    df_convert
){

  # helper function for events cleaning function
  # arguments validated in the cleaning function

  # stage of ticker conversion
  df_convert %>%
    # mutate(
    #   ticker_stage =
    #     as.numeric(
    #       ticker %in%
    #         new_ticker
    #     )
    #   , new_ticker_stage =
    #     ticker_stage +
    #     as.numeric(
    #       new_ticker %in%
    #         ticker
  #     )
  #   # , stage =
  # ) %>%
  mutate(
    stage =
      ticker %in%
      new_ticker
  ) %>%
    split(.$stage) ->
    list_convert

  # iterative ticker conversion
  for (df_stage in list_convert) {

    # apply the conversion function
    # updating transfers data frame
    # for subsequent stages
    fun_b3_convert_data(
      df_events_transfers,
      df_stage
    ) -> df_events_transfers

  }

  # output
  return(df_events_transfers)

}
