#' A different as.numeric
#'
#' @description A function to coerce everything to type numeric, without giving an NA result.
#'
#' @param x An object to coerce to type numeric.
#' @param only.on Selects a subset of the data to which to apply as_numeric.
#' @param coerce.warning Whether to warn the user if coercion happened, defaults to TRUE
#' @note Works with character vectors like "0,5145" without giving a warning or returning an NA.
#' @return An object of same structure as x, with elements coerced to numeric.
#' @export
#' @examples
#'
#'  # observe what as.numeric does
#'  A <- c("2,4","0.36","0.45","9,7")
#'  as.numeric(A)
#'  # and what as_numeric does
#'  as_numeric(A)
#'  # further if A is a factor
#'  A <- as.factor(c(10,2.7,5.5))
#'  as.numeric(A) # this is basically terrible and not what you want (most likely)
#'  as_numeric(A) # this works
#'  # further this function works happily with lists and matrices (and everything else)
#'  # and it will throw a warning and omit where the coercion would produce an NA (unless force is set to TRUE)
#'


as_numeric <- function( x,
                        only.on = NULL,
                        coerce.warning = TRUE)
{
  data.table <- NULL
  na_cont <- sum(is.na(x))

    make_numeric <- function(x_dat)
    {
      infer_type <- class(x_dat)

        get_res <- match( x = infer_type, table = c( "integer","numeric","factor","character",
                                                     "matrix", "data.frame",
                                                     "data.table","list"))

      char_conversion <- function(x_char)
      {
        x_contains_comma <- grep(pattern = "[[:digit:]]+,{1}[[:digit:]]+", x = x)
        if( sum(x_contains_comma) != 0 ){
          x <- gsub(pattern = ",", replacement = ".", x = x)
        }
        return(x)
      }
                as_num_char <- function(char_vec)
                             { return(as.numeric(char_conversion(char_vec)))}
                as_num_fac <- function(fac_vec)
                            { return(as.numeric(as.character(char_conversion(fac_vec))))}
                as_num_mat <- function(fac_vec)
                { return(apply( x, MARGIN = 2, FUN = as_numeric))}
                as_num_df <- function(df_vec)
                { return(data.frame(lapply(x, as_numeric)))}
                as_num_dt <-function(dt_vec)
                { return(data.table(lapply(x, as_numeric)))}
                as_num_lst <- function(lst_vec)
                { return(lapply(x, as_numeric))}


      fun_list <- list( as.numeric, as.numeric, as_num_fac,
                        as_num_char,as_num_mat, as_num_df,
                        as_num_dt, as_num_lst )
   #   return(list(fun_list[[get_res]], list(x_dat)))

      return(do.call(fun_list[[get_res]], list(x_dat)))
    }
  if( typeof(x) == "list" && !is.null(only.on)){
    x_sub <- x[,only.on]
    x_res <- suppressWarnings(make_numeric(x_dat = x_sub ))
   final <- cbind(x,x_res)
  }
  else
  {
   final <- suppressWarnings(make_numeric(x_dat = x ))
  }
  if(coerce.warning == TRUE){
    coercion_message <- sum(is.na(final)) - na_cont
    if(coercion_message > 0)
    {
      message(paste(coercion_message," elements have been coerced to NA.",sep = ""))
    }
  }
return(final)
}


