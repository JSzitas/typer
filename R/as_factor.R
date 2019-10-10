#' A different as.factor
#'
#' @description A function to coerce everything to type factor.
#'
#' @param x An object to coerce to type factor.
#' @param only.on Selects a subset of the data to which to apply as_factor.
#' @note Works with data.frames, lists, data.tables, without exhibiting 'weird' behaviour.
#' @return An object of same structure as x, with elements coerced to factor.
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
#'  apply(A, MARGIN = 2, FUN = as.factor)
#'  # which decidedly does not return a factor anywhere in sight.
#'
#'  # so what does as_factor return?
#'  as_factor(A)
#'  # the most sensible answer
#'


as_factor <- function( x,
                       only.on = NULL )
{
  data.table <- NULL
  make_factor <- function(x_dat)
  {
    infer_type <- class(x_dat)

    get_res <- match( x = infer_type, table = c( "integer",    "numeric", "factor",
                                                 "character",  "matrix",  "data.frame",
                                                 "data.table", "list"))


    as_fac_mat <- function(fac_mat)
    { return(apply( fac_mat, MARGIN = 2, FUN = as_factor))}
    as_fac_df <- function(df_vec)
    { return(data.frame(lapply(df_vec, as_factor)))}
    as_fac_dt <-function(dt_vec)
    { return(data.table(lapply(dt_vec, as_factor)))}
    as_fac_lst <- function(lst_vec)
    { return(lapply(lst_vec, as_factor))}


    fun_list <- list( as.factor, as.factor, as.factor,
                      as.factor, as_fac_mat, as_fac_df,
                      as_fac_dt, as_fac_lst )

    return(do.call(fun_list[[get_res]], list(x_dat)))
  }
  if( typeof(x) == "list" && !is.null(only.on)){
    x_sub <- x[,only.on]
    x_res <- suppressWarnings(make_factor(x_dat = x_sub ))
    final <- cbind(x,x_res)
  }
  else
  {
    final <- suppressWarnings(make_factor(x_dat = x ))
  }
  return(final)
}


