# [FUNCTIONS] --------------------------------------------------------------
# - identify stocks ----------------------------------------------------
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

