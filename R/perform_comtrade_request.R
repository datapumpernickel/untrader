#' perform_comtrade_request
#' Performs the request and returns an error body with the respective error returned by the Comtrade API. Also throttles all requests to 1 request per 6 seconds, or 10 requests per minute.
#'
#' @param req a valid comtrade request built by the build_comtrade_request function
#' @param requests_per_second rate at which throttling is done
#'
#' @return json data from comtrade and possible error codes
perform_comtrade_request <-
  function(req, requests_per_second = 10 / 60) {
    resp <- req |>
      httr2::req_error(body = comtrade_error_body) |>
      httr2::req_throttle(rate = requests_per_second) |>
      httr2::req_perform()

    return(resp)
  }

comtrade_error_body <- function(resp) {
  body <- httr2::resp_body_json(resp, simplifyVector = T)

  message <- body$errorObject$errorMessage
  if (!is.null(message)) {
    message <- c(message)
  }
  message
}

