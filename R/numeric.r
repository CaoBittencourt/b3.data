# [FUNCTIONS] --------------------------------------------------------------
# - standardize numeric variables -----------------------------------------
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

