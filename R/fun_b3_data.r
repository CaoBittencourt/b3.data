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
      date = Data,
      type = `Entrada/Saída`,
      event = Movimentação,
      ticker = Produto,
      qtd = Quantidade,
      price = `Preço unitário`
    ) -> df_transactions

  # select only necessary columns
  df_transactions %>%
    select(
      date,
      type,
      event,
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
    mutate(across(
      .cols = c(type, event)
      ,.fns = str_to_lower
    )) -> df_transactions

  # operation sign
  df_transactions %>%
    mutate(
      qtd = case_match(
        type
        , c('crédito', 'credito') ~ qtd
        , c('débito', 'debito') ~ -qtd
      )
    ) -> df_transactions

  # standardize tickers
  df_transactions %>%
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
    ) -> df_transactions

  # standardize numeric variables
  df_transactions %>%
    mutate(across(
      .cols = c(qtd, price)
      ,.fns = fun_b3_numeric
    )) -> df_transactions

  # set NA prices to 0
  df_transactions %>%
    mutate(
      price = if_else(
        !is.na(price)
        , price
        , 0
      )
    ) -> df_transactions

  # arrange by date
  df_transactions %>%
    arrange(date) ->
    df_transactions

  # grouping indicator
  df_transactions %>%
    group_by(ticker) %>%
    mutate(
      .after = event
      , cycle = cumsum(event == 'grupamento')
      , cycle = factor(cycle)
    ) %>%
    ungroup() ->
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

  # separate regular transactions,
  # dividends, and other events
  df_transactions %>%
    filter(
      if_all(
        .cols = event
        ,.fns = list(
          ~ str_detect(.x, 'divid|juros|rend'),
          ~ !str_detect(.x, 'transf')
        )
      )
    ) -> df_dividends

  df_transactions %>%
    filter(
      str_detect(
        event,
        'fraç|leil'
      )
    ) -> df_other

  df_transactions %>%
    filter(
      !str_detect(
        event,
        'divid|juros|rend|fraç|leil'
      )
    ) -> df_transactions

  # round down decimal stocks
  df_transactions %>%
    mutate(
      qtd = if_else(
        stock
        , floor(qtd)
        , qtd
      )
    ) -> df_transactions

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
    df_transactions
    , class = c(
      class(df_transactions),
      'df_transactions'
    )
  ) -> df_transactions

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
      class(df_transactions),
      'df_other'
    )
  ) -> df_other

  # output
  return(list(
    'transactions' = df_transactions,
    'dividends' = df_dividends,
    'other' = df_other
  ))

}

# - Clean data (main) ---------------------------------------------------------
fun_b3_clean <- function(list_chr_path_transactions){

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

  # apply data cleaning function
  tryCatch(
    expr = {fun_b3_clean_transactions(
      list_chr_path_transactions
    )}
    , error = function(e){NULL}
  ) -> list_b3_data

  # output
  return(list_b3_data)

}
