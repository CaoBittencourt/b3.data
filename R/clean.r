# [FUNCTIONS] --------------------------------------------------------------
# [ ] remove alt-tickers with nrow == 1 ---------------------------------------
# - clean data (transactions) -----------------------------------------------
fun_b3_clean_transactions <- function(list_chr_path_transactions) {
  # read b3 financial transactions files
  lapply(
    list_chr_path_transactions,
    read_excel
  ) %>%
    bind_rows() ->
  df_transactions

  # clean b3 financial transactions files
  # rename necessary columns
  # select only necessary columns
  df_transactions %>%
    select(
      date = `Data do Negócio`,
      type = `Tipo de Movimentação`,
      ticker = `Código de Negociação`,
      qtd = Quantidade,
      price = Preço
    ) -> df_transactions

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
        format = "%d/%m/%Y"
      )
    ) -> df_transactions

  # lowercase chr
  df_transactions %>%
    mutate(
      type = str_to_lower(type)
    ) -> df_transactions

  # standardize tickers
  df_transactions %>%
    mutate(
      ticker =
        fun_b3_ticker(
          ticker
        )
    ) -> df_transactions

  # ticker type
  df_transactions %>%
    mutate(
      .after = ticker,
      ticker_type =
        fun_b3_ticker_type(
          ticker
        )
    ) -> df_transactions

  # stock indicator
  df_transactions %>%
    mutate(
      .after = ticker_type,
      stock = fun_b3_is_stock(
        ticker
      ),
      stock = as.logical(stock)
    ) -> df_transactions

  # standardize numeric variables
  df_transactions %>%
    mutate(across(
      .cols = c(qtd, price),
      .fns = fun_b3_numeric
    )) -> df_transactions

  # operation sign
  df_transactions %>%
    mutate(
      qtd = case_when(
        type == "compra" ~ qtd,
        type == "venda" ~ -qtd,
        .default = NA
      )
    ) -> df_transactions

  # arrange by date
  df_transactions %>%
    arrange(date) ->
  df_transactions

  # add subclass
  new_data_frame(
    df_transactions,
    class = c(
      class(df_transactions),
      "df_transactions"
    )
  ) -> df_transactions

  # output
  return(df_transactions)
}

# # - clean data (events) ---------------------------------------------------
# fun_b3_clean_events <- function(
#     list_chr_path_events,
#     list_chr_path_events_remove = NULL,
#     list_chr_path_events_add = NULL,
#     df_convert = NULL
# ){
#
#   # read b3 financial events files
#   lapply(
#     list_chr_path_events,
#     read_excel
#   ) %>%
#     bind_rows() ->
#     df_events
#
#   # anti-join to remove rows
#   if(length(
#     list_chr_path_events_remove
#   )){
#
#     df_events %>%
#       anti_join(
#         lapply(
#           list_chr_path_events_remove,
#           read_excel
#         ) %>%
#           bind_rows()
#       ) -> df_events
#
#   }
#
#   # bind rows to add rows
#   if(length(
#     list_chr_path_events_add
#   )){
#
#     lapply(
#       list_chr_path_events_add
#       , read_excel
#     ) %>%
#       bind_rows(
#         df_events
#       ) -> df_events
#
#   }
#
#   # clean b3 financial events files
#   # rename necessary columns
#   df_events %>%
#     rename(
#       date = Data,
#       type = `Entrada/Saída`,
#       event = Movimentação,
#       ticker = Produto,
#       qtd = Quantidade,
#       price = `Preço unitário`
#     ) -> df_events
#
#   # select only necessary columns
#   df_events %>%
#     select(
#       date,
#       type,
#       event,
#       ticker,
#       qtd,
#       price
#     ) -> df_events
#
#   # date type
#   df_events %>%
#     mutate(
#       date = as_date(
#         date,
#         format = '%d/%m/%Y'
#       )
#     ) -> df_events
#
#   # lowercase chr
#   df_events %>%
#     mutate(across(
#       .cols = c(type, event)
#       ,.fns = str_to_lower
#     )) -> df_events
#
#   # standardize tickers
#   df_events %>%
#     mutate(
#       ticker =
#         str_split(
#           ticker,
#           pattern = ' - ',
#           n = 2,
#           simplify = T
#         )[,1]
#       , ticker =
#         fun_b3_ticker(
#           ticker
#         )
#     ) -> df_events
#
#   # ticker type
#   df_events %>%
#     mutate(
#       .after = ticker
#       , ticker_type =
#         fun_b3_ticker_type(
#           ticker
#         )
#     ) -> df_events
#
#   # stock indicator
#   df_events %>%
#     mutate(
#       .after = ticker_type
#       , stock = fun_b3_is_stock(
#         ticker
#       )
#       , stock = as.logical(stock)
#     ) -> df_events
#
#   # standardize numeric variables
#   df_events %>%
#     mutate(across(
#       .cols = c(qtd, price)
#       ,.fns = fun_b3_numeric
#     )) -> df_events
#
#   # operation sign
#   df_events %>%
#     mutate(
#       qtd = case_match(
#         type
#         , c('crédito', 'credito') ~ qtd
#         , c('débito', 'debito') ~ -qtd
#       )
#     ) -> df_events
#
#   # set NA prices to 0
#   df_events %>%
#     mutate(
#       price = if_else(
#         !is.na(price)
#         , price
#         , 0
#       )
#     ) -> df_events
#
#   # arrange by date
#   df_events %>%
#     arrange(date) ->
#     df_events
#
#   # grouping indicator
#   df_events %>%
#     group_by(ticker) %>%
#     mutate(
#       .after = event
#       , cycle = cumsum(
#         event == 'grupamento'
#       )
#     ) %>%
#     ungroup() ->
#     df_events
#
#   # indicator for whether the asset is currently active
#   # i.e. available on the market
#   df_events$active <- T
#
#   # separate financial transfers,
#   # dividends, and other events
#   df_events %>%
#     filter(
#       if_all(
#         .cols = event
#         ,.fns = list(
#           ~ str_detect(.x, 'divid|juros|rend'),
#           ~ !str_detect(.x, 'transf')
#         )
#       )
#     ) -> df_events_dividends
#
#   df_events %>%
#     filter(
#       str_detect(
#         event,
#         'fraç|leil'
#       )
#     ) -> df_events_other
#
#   df_events %>%
#     filter(
#       event != 'transferência'
#       , !str_detect(
#         event,
#         'divid|juros|rend|fraç|leil'
#       )
#     ) -> df_events_transfers
#
#   rm(df_events)
#
#   # remove problematic transfer events
#   df_events_transfers %>%
#     group_by(ticker) %>%
#     filter(!(
#       str_detect(event, 'atualiza|cis')
#       & qtd == lag(cumsum(
#         qtd * (1 - str_detect(event, 'atualiza|cis'))
#       ), default = 0)
#     )) %>%
#     ungroup() ->
#     df_events_transfers
#
#   # default proportion
#   df_events_transfers$prop <- 1
#
#   # converted tickers
#   if(!is.null(df_convert)){
#
#     # conversion dates
#     fun_b3_convert_date(
#       df_events_transfers,
#       df_convert
#     ) -> df_convert
#
#     # converted ticker data
#     fun_b3_convert_data(
#       df_events_transfers,
#       df_convert
#     ) -> df_events_transfers
#
#   }
#
#   # round down decimal stocks
#   df_events_transfers %>%
#     mutate(
#       qtd = if_else(
#         stock
#         , floor(qtd)
#         , qtd
#       )
#     ) -> df_events_transfers
#
#   df_events_dividends %>%
#     mutate(
#       qtd = if_else(
#         stock
#         , floor(qtd)
#         , qtd
#       )
#     ) -> df_events_dividends
#
#   # remove events that do not affect neither qtd nor price
#   df_events_transfers %>%
#     filter(!if_all(
#       .cols = c(qtd, price)
#       ,.fns = ~ .x == 0
#     )) -> df_events_transfers
#
#   df_events_dividends %>%
#     filter(!if_all(
#       .cols = c(qtd, price)
#       ,.fns = ~ .x == 0
#     )) -> df_events_dividends
#
#   # add subclasses
#   new_data_frame(
#     df_events_transfers
#     , class = c(
#       class(df_events_transfers),
#       'df_events_transfers'
#     )
#   ) -> df_events_transfers
#
#   new_data_frame(
#     df_events_dividends
#     , class = c(
#       class(df_events_dividends),
#       'df_events_dividends'
#     )
#   ) -> df_events_dividends
#
#   new_data_frame(
#     df_events_other
#     , class = c(
#       class(df_events_other),
#       'df_events_other'
#     )
#   ) -> df_events_other
#
#   # output
#   return(list(
#     'transfers' = df_events_transfers,
#     'dividends' = df_events_dividends,
#     'other' = df_events_other
#   ))
#
# }

# - clean data (events) ---------------------------------------------------
fun_b3_clean_events <- function(
    list_chr_path_events,
    list_chr_path_events_remove = NULL,
    list_chr_path_events_add = NULL,
    df_convert = NULL) {
  # read b3 financial events files
  lapply(
    list_chr_path_events,
    read_excel
  ) %>%
    bind_rows() ->
  df_events

  # anti-join to remove rows
  if (length(
    list_chr_path_events_remove
  )) {
    df_events %>%
      anti_join(
        lapply(
          list_chr_path_events_remove,
          read_excel
        ) %>%
          bind_rows()
      ) -> df_events
  }

  # bind rows to add rows
  if (length(
    list_chr_path_events_add
  )) {
    lapply(
      list_chr_path_events_add,
      read_excel
    ) %>%
      bind_rows(
        df_events
      ) -> df_events
  }

  # clean b3 financial events files
  # rename necessary columns
  # select only necessary columns
  df_events %>%
    select(
      date = Data,
      type = `Entrada/Saída`,
      event = Movimentação,
      ticker = Produto,
      qtd = Quantidade,
      price = `Preço unitário`
    ) -> df_events

  # date type
  df_events %>%
    mutate(
      date = as_date(
        date,
        format = "%d/%m/%Y"
      )
    ) -> df_events

  # lowercase chr
  df_events %>%
    mutate(across(
      .cols = c(type, event),
      .fns = str_to_lower
    )) -> df_events

  # standardize tickers
  df_events %>%
    mutate(
      ticker =
        str_split(
          ticker,
          pattern = " - ",
          n = 2,
          simplify = T
        )[, 1],
      ticker =
        fun_b3_ticker(
          ticker
        )
    ) -> df_events

  # ticker type
  df_events %>%
    mutate(
      .after = ticker,
      ticker_type =
        fun_b3_ticker_type(
          ticker
        )
    ) -> df_events

  # stock indicator
  df_events %>%
    mutate(
      .after = ticker_type,
      stock = fun_b3_is_stock(
        ticker
      ),
      stock = as.logical(stock)
    ) -> df_events

  # standardize numeric variables
  df_events %>%
    mutate(across(
      .cols = c(qtd, price),
      .fns = fun_b3_numeric
    )) -> df_events

  # stock buyback
  df_events %>%
    mutate(
      type = if_else(
        event |> str_like("restitui%"),
        "debito",
        type
      ),
      price = if_else(
        event |> str_like("restitui%"),
        0,
        price
      )
    ) -> df_events

  # operation sign
  df_events %>%
    mutate(
      qtd = case_match(
        type,
        c("crédito", "credito") ~ qtd,
        c("débito", "debito") ~ -qtd
      )
    ) -> df_events

  # set NA prices to 0
  df_events %>%
    mutate(
      price = if_else(
        !is.na(price),
        price,
        0
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
      .after = event,
      cycle = cumsum(
        event == "grupamento"
      )
    ) %>%
    ungroup() ->
  df_events

  # separate financial transfers,
  # dividends, and other events
  df_events %>%
    filter(
      if_all(
        .cols = event,
        .fns = list(
          ~ str_detect(.x, "divid|juros|rend"),
          ~ !str_detect(.x, "transf")
        )
      )
    ) -> df_events_dividends

  df_events %>%
    filter(
      str_detect(
        event,
        "fraç|leil"
      )
    ) -> df_events_other

  df_events %>%
    filter(
      event != "transferência",
      !str_detect(
        event,
        "divid|juros|rend|fraç|leil"
      )
    ) -> df_events_transfers

  # remove problematic transfer events
  df_events_transfers %>%
    group_by(ticker) %>%
    filter(!(
      str_detect(event, "atualiza|cis") &
        qtd == lag(cumsum(
          qtd * (1 - str_detect(event, "atualiza|cis"))
        ), default = 0)
    )) %>%
    ungroup() ->
  df_events_transfers

  # default proportion
  df_events_transfers$prop <- 1

  # converted tickers
  if (!is.null(df_convert)) {
    # conversion dates
    df_convert %>%
      inner_join(
        df_events_transfers,
        by = c(
          "new_ticker" =
            "ticker"
        ),
        multiple = "all",
        relationship =
          "many-to-many"
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

    # converted ticker data
    df_convert %>%
      inner_join(
        df_events_transfers,
        multiple = "all",
        relationship =
          "many-to-many"
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
            ticker,
            obsolete =
              convert,
            date =
              convert
          ) %>%
          unique() %>%
          mutate(
            qtd = NA,
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
          is.na(convert),
          cycle + 1,
          cycle
        ),
        prop = if_else(
          is.na(prop),
          1,
          prop
        ),
        active = if_else(
          is.na(active),
          T,
          active
        )
      ) ->
    df_events_transfers
  }

  # round down decimal stocks
  df_events_transfers %>%
    mutate(
      qtd = if_else(
        stock,
        floor(qtd),
        qtd
      )
    ) -> df_events_transfers

  df_events_dividends %>%
    mutate(
      qtd = if_else(
        stock,
        floor(qtd),
        qtd
      )
    ) -> df_events_dividends

  # remove events that do not affect neither qtd nor price
  df_events_transfers %>%
    filter(!if_all(
      .cols = c(qtd, price),
      .fns = ~ .x == 0
    )) -> df_events_transfers

  df_events_dividends %>%
    filter(!if_all(
      .cols = c(qtd, price),
      .fns = ~ .x == 0
    )) -> df_events_dividends

  # add subclasses
  new_data_frame(
    df_events_transfers,
    class = c(
      class(df_events_transfers),
      "df_events_transfers"
    )
  ) -> df_events_transfers

  new_data_frame(
    df_events_dividends,
    class = c(
      class(df_events_dividends),
      "df_events_dividends"
    )
  ) -> df_events_dividends

  new_data_frame(
    df_events_other,
    class = c(
      class(df_events_other),
      "df_events_other"
    )
  ) -> df_events_other

  # output
  return(list(
    "transfers" = df_events_transfers,
    "dividends" = df_events_dividends,
    "other" = df_events_other
  ))
}

# - clean data (main) -----------------------------------------------------
# clean data (main)
fun_b3_clean <- function(
    list_chr_path_transactions,
    list_chr_path_events,
    list_chr_path_events_remove = NULL,
    list_chr_path_events_add = NULL,
    df_convert = NULL) {
  # arguments validation
  stopifnot(
    "'list_chr_path_transactions' must be a list of paths to B3 financial transactions .xlsx files." =
      all(
        is.list(list_chr_path_transactions),
        sapply(
          list_chr_path_transactions,
          is.character
        )
      )
  )

  stopifnot(
    "'list_chr_path_events' must be a list of paths to B3 financial events .xlsx files." =
      all(
        is.list(list_chr_path_events),
        sapply(
          list_chr_path_events,
          is.character
        )
      )
  )

  # apply data cleaning functions
  Map(
    function(b3_files, b3_fun) {
      tryCatch(
        expr = {
          do.call(b3_fun, b3_files)
        },
        error = function(e) {
          NULL
        }
      )
    },
    b3_files = list(
      "transactions" = list(
        list_chr_path_transactions
      ),
      "events" = list(
        list_chr_path_events,
        list_chr_path_events_remove,
        list_chr_path_events_add,
        df_convert
      )
    ),
    b3_fun = list(
      fun_b3_clean_transactions,
      fun_b3_clean_events
    )
  ) -> list_b3_data

  # output
  return(list_b3_data)
}
