#' A different as.character
#'
#' @description A function to coerce everything to type character.
#'
#' @param x An object to coerce to type character.
#' @param only.on Selects a subset of the data to which to apply as_character.
#' @note Works with data.frames, lists, data.tables, without exhibiting 'weird' behaviour.
#' @return An object of same structure as x, with elements coerced to character.
#' @importFrom data.table data.table
#' @export
#' @examples
#'
#'
#'  # lets start by creating a factor
#'  A <- factor(c(1,2,7,5))
#'  as.character(A) # returns what anyone would expect
#'
#'  # so lets try to see what happens when you convert A to a matrix
#'  A <- matrix( data = A, nrow = 2)
#'  # A gets converted to character automatically, so far so good
#'  # lets make it into a data.frame now
#'  A <- data.frame(A)
#'  as.character(A)
#'  # the above returns nonsense. "1:2" "2:1" is probably not what anyone expects
#'
#'  # so we try the old trick
#'  apply(A, MARGIN = 2, FUN = as.character)
#'
#'  # and this makes sense.
#'  # so what does as_character return?
#'  as_character(A)
#'  # the sensible answer
#'


as_character <- function( x,
                          only.on = NULL )
{
  data.table <- NULL
  make_character <- function(x_dat)
  {
    infer_type <- class(x_dat)

    get_res <- match( x = infer_type, table = c( "integer",    "numeric", "factor",
                                                 "character",  "matrix",  "data.frame",
                                                 "data.table", "list"))

  as_char_num <- function(char_vec)
  { return(as.character(char_vec))}
  as_char_fac <- function(fac_vec)
  { return(as.character(fac_vec))}
  as_char_mat <- function(fac_vec)
  { return(apply( fac_vec, MARGIN = 2, FUN = as_character))}
  as_char_df <- function(df_vec)
  { return(data.frame(lapply(df_vec, as_character)))}
  as_char_dt <-function(dt_vec)
  { return(data.table(lapply(dt_vec, as_character)))}
  as_char_lst <- function(lst_vec)
  { return(lapply(lst_vec, as_character))}


  fun_list <- list( as.character, as_char_num, as_char_fac,
                    as.character, as_char_mat, as_char_df,
                    as_char_dt, as_char_lst )

  return(do.call(fun_list[[get_res]], list(x_dat)))
}
if( typeof(x) == "list" && !is.null(only.on)){
  x_sub <- x[,only.on]
  x_res <- suppressWarnings(make_character(x_dat = x_sub ))
  final <- cbind(x,x_res)
}
else
{
  final <- suppressWarnings(make_character(x_dat = x ))
}
return(final)
}


