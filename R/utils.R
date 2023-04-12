#' Set your primary Comtrade API key in the environment variable
#'
#' If you would like your Comtrade API key to persist in between sessions, use `usethis::edit_r_environ()` to add the env variable COMTRADE_PRIMARY to your environment file.
#'
#' @param key Provide your primary comtrade key
#'
#' @return Saves your comtrade primary key in the environment.
#' @export
set_primary_comtrade_key <- function(key = NULL) {
  if (is.null(key)) {
    key <- askpass::askpass("Please enter your API key")
  }
  Sys.setenv("COMTRADE_PRIMARY" = key)
}

#' get_primary_comtrade_key
#'
#' If you would like your Comtrade API key to persist in between sessions, use `usethis::edit_r_environ()` to add the env variable COMTRADE_PRIMARY to your environment file.
#'
#' @return Gets your primary comtrade key from the environment var COMTRADE_PRIMARY
#' @export
get_primary_comtrade_key <- function() {
  key <- Sys.getenv("COMTRADE_PRIMARY")
  if (!identical(key, "")) {
    return(key)
  } else {
    rlang::abort("No API key found, please supply with `set_primary_comtrade_key` function or set COMTRADE_PRIMARY env var")
  }
}
