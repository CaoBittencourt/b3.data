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

# - Standardize numeric variables -----------------------------------------
fun_b3_numeric <- function(chr_var){

  # standardize numeric variable
  if(any(is.character(chr_var))){

    chr_var %>%
      as.character() %>%
      str_remove_all('R$|r$|-') %>%
      str_replace_all(
        ',', '.'
      ) %>%
      as.numeric() ->
      dbl_var

  } else {

    chr_var -> dbl_var

  }

  rm(chr_var)

  # output
  return(dbl_var)
}

# - Identify stocks ----------------------------------------------------
fun_b3_is_stock <- function(chr_ticker){

  # arguments validation
  stopifnot(
    "'chr_ticker' must be a character string." =
      is.character(chr_ticker)
  )

  # pattern matching
  str_detect(
    chr_ticker
    , '^.{4}[0-9]+$'
  ) -> lgc_stock

  # output
  return(lgc_stock)

}

# - Clean data (transactions) ---------------------------------------------------------
fun_b3_clean_transactions <- function(list_chr_path_transactions){

  # read b3 financial transactions files
  lapply(
    list_chr_path_transactions,
    read_excel
  ) %>%
    bind_rows() ->
    df_transactions

  # clean b3 financial transactions files
  # rename necessary columns
  df_transactions %>%
    rename(
      date = `Data do Negócio`,
      type = `Tipo de Movimentação`,
      ticker = `Código de Negociação`,
      qtd = Quantidade,
      price = Preço
    ) -> df_transactions

  # select only necessary columns
  df_transactions %>%
    select(
      date,
      type,
      ticker,
      qtd,
      price
    ) -> df_transactions

  # date type
  df_transactions %>%
    mutate(
      date = as_date(
        date,
        format = '%d/%m/%Y'
      )
    ) -> df_transactions

  # lowercase chr
  df_transactions %>%
    mutate(
      type = str_to_lower(type)
    ) -> df_transactions

  # operation sign
  df_transactions %>%
    mutate(
      qtd = case_when(
        type == 'compra' ~ qtd,
        type == 'venda' ~ -qtd,
        T ~ NA
      )
    ) -> df_transactions

  # standardize tickers
  df_transactions %>%
    mutate(
      ticker =
        fun_b3_ticker(
          ticker
        )
    ) -> df_transactions

  # standardize numeric variables
  df_transactions %>%
    mutate(across(
      .cols = c(qtd, price)
      ,.fns = fun_b3_numeric
    )) -> df_transactions

  # arrange by date
  df_transactions %>%
    arrange(date) ->
    df_transactions

  # stock indicator
  df_transactions %>%
    mutate(
      .after = ticker
      , stock = fun_b3_is_stock(
        ticker
      )
      , stock = as.logical(stock)
    ) -> df_transactions

  # add subclass
  new_data_frame(
    df_transactions
    , class = c(
      class(df_transactions),
      'df_transactions'
    )
  ) -> df_transactions

  # output
  return(df_transactions)

}

# - Clean data (events) ---------------------------------------------------------
fun_b3_clean_events <- function(list_chr_path_events){

  # read b3 financial events files
  lapply(
    list_chr_path_events,
    read_excel
  ) %>%
    bind_rows() ->
    df_events

  # clean b3 financial events files
  # rename necessary columns
  df_events %>%
    rename(
      date = Data,
      type = `Entrada/Saída`,
      event = Movimentação,
      ticker = Produto,
      qtd = Quantidade,
      price = `Preço unitário`
    ) -> df_events

  # select only necessary columns
  df_events %>%
    select(
      date,
      type,
      event,
      ticker,
      qtd,
      price
    ) -> df_events

  # date type
  df_events %>%
    mutate(
      date = as_date(
        date,
        format = '%d/%m/%Y'
      )
    ) -> df_events

  # lowercase chr
  df_events %>%
    mutate(across(
      .cols = c(type, event)
      ,.fns = str_to_lower
    )) -> df_events

  # operation sign
  df_events %>%
    mutate(
      qtd = case_match(
        type
        , c('crédito', 'credito') ~ qtd
        , c('débito', 'debito') ~ -qtd
      )
    ) -> df_events

  # standardize tickers
  df_events %>%
    mutate(
      ticker =
        str_split(
          ticker,
          pattern = ' - ',
          n = 2,
          simplify = T
        )[,1]
      , ticker =
        fun_b3_ticker(
          ticker
        )
    ) -> df_events

  # standardize numeric variables
  df_events %>%
    mutate(across(
      .cols = c(qtd, price)
      ,.fns = fun_b3_numeric
    )) -> df_events

  # set NA prices to 0
  df_events %>%
    mutate(
      price = if_else(
        !is.na(price)
        , price
        , 0
      )
    ) -> df_events

  # arrange by date
  df_events %>%
    arrange(date) ->
    df_events

  # grouping indicator
  df_events %>%
    group_by(ticker) %>%
    mutate(
      .after = event
      , cycle = cumsum(event == 'grupamento')
      # , cycle = factor(cycle)
    ) %>%
    ungroup() ->
    df_events

  # stock indicator
  df_events %>%
    mutate(
      .after = ticker
      , stock = fun_b3_is_stock(
        ticker
      )
      , stock = as.logical(stock)
    ) -> df_events

  # separate financial transfers,
  # dividends, and other events
  df_events %>%
    filter(
      if_all(
        .cols = event
        ,.fns = list(
          ~ str_detect(.x, 'divid|juros|rend'),
          ~ !str_detect(.x, 'transf')
        )
      )
    ) -> df_dividends

  df_events %>%
    filter(
      str_detect(
        event,
        'fraç|leil'
      )
    ) -> df_other

  df_events %>%
    filter(
      event != 'transferência'
      # , event != 'atualização'
      , !str_detect(
        event,
        'divid|juros|rend|fraç|leil'
      )
    ) -> df_transfers

  rm(df_events)

  # round down decimal stocks
  df_transfers %>%
    mutate(
      qtd = if_else(
        stock
        , floor(qtd)
        , qtd
      )
    ) -> df_transfers

  df_dividends %>%
    mutate(
      qtd = if_else(
        stock
        , floor(qtd)
        , qtd
      )
    ) -> df_dividends

  # add subclasses
  new_data_frame(
    df_transfers
    , class = c(
      class(df_transfers),
      'df_transfers'
    )
  ) -> df_transfers

  new_data_frame(
    df_dividends
    , class = c(
      class(df_dividends),
      'df_dividends'
    )
  ) -> df_dividends

  new_data_frame(
    df_other
    , class = c(
      class(df_other),
      'df_other'
    )
  ) -> df_other

  # output
  return(list(
    'transfers' = df_transfers,
    'dividends' = df_dividends,
    'other' = df_other
  ))

}

# - Clean data (main) ---------------------------------------------------------
fun_b3_clean <- function(
    list_chr_path_transactions,
    list_chr_path_events
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
        expr = {do.call(b3_fun, list(b3_files))}
        , error = function(e){NULL}
      )
    }
    , b3_files = list(
      'transactions' = list_chr_path_transactions,
      'events' = list_chr_path_events
    )
    , b3_fun = list(
      fun_b3_clean_transactions,
      fun_b3_clean_events
    )
  ) -> list_b3_data


  # output
  return(list_b3_data)

}
