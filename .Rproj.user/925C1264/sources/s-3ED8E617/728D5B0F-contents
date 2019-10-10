#' A type coercion operator
#'
#' @description Allows the quick changing of types, using the new type coercions introduced in the package.
#'
#' @param obj The object to coerce into a new type.
#' @param new_type The character string (or string shorthand) giving the new type required.
#' @note Accepts both full argument names: "numeric","character","logical",
#'                                         "integer","double","complex",
#'                                         "list","unlist","NULL","environment",
#'                                         "symbol","expression","data.frame",
#'                                         "vector","factor", "data.table"
#'
#'                  as well as shorthands: "num","char","logi",
#'                                         "int","dbl","cmplx",
#'                                         "lst","unlst","NUL","env",
#'                                         "sym","expr","dfm",
#'                                         "vec","fac","dtb"
#'
#' @details Behaves a bit more smartly than base coercion (ie numbers containing a "," as a separator get coerced to type numeric)
#' @return The coerced object.
#' @export
#' @examples
#'
#'
#' A <- c( "0,7",0.9,"15","1235","0.66","plane" )
#'
#' # there are shorthands
#' A %t% "num"
#' A %t% "character"
#' A %t% "factor"
#'
#' # NOTE that you can create your own shorthands using global environment objects
#' # for interactive use if you want to
#' # (Please never do this for anything that is not throwaway code, meaning code that
#' # gets reused or run multiple times;
#' # it WILL cause confusion and IT WILL make people angry. )
#' n <- "numeric"
#' A %t% n
#'
#'



'%t%' <- function( obj, new_type ){
as.data.table <- NULL

match_string_short <-   match(new_type, table = c( "num","char","logi",
                                      "int","dbl","cmplx",
                                      "lst","unlst","NUL","env",
                                      "sym","expr","dfm",
                                      "vec","fac","dtb" ))
match_string_full <-    match(new_type, table = c( "numeric","character","logical",
                                                   "integer","double","complex",
                                                   "list","unlist","NULL","environment",
                                                   "symbol","expression","data.frame",
                                                   "vector","factor", "data.table" ))

make_call <- which( c(!is.na(match_string_short), !is.na(match_string_full)))


  fun_list <- list( as_numeric,      as_character,   as.logical,
                    as.integer,      as.double,      as.complex,
                    as.list,         unlist,         as.null,
                    as.environment,  as.symbol,      as.expression,
                    as.data.frame,   c,              as_factor,
                    as.data.table )

  if(make_call == 1 ){
    return( do.call(fun_list[[match_string_short]], list(obj)))
  }
  else if(make_call == 2 ){
    return( do.call(fun_list[[match_string_full]], list(obj)))
  }
  else
  {
    stop("Please provide a valid type name/shorthand for coercion.")
  }
}




