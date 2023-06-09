#' Processes the response object
#'
#' The function processes the httr2response object, parses the json and adds the respective iso codes for the reporter and partner countries, as well as the commodity code description.
#'
#' @param resp a valid httr2 response object created from the function `perform_comtrade_request`
#' @param verbose whether the function sends status updates to the console
#'
#' @return a data.frame object with the results
process_comtrade_response <- function(resp, verbose = F) {
  result <- resp |>
    httr2::resp_body_json(simplifyVector = T)

  if (length(result$data) > 0) {
    if(nrow(result$data)>249999){
        rlang::abort('Your request returns more than 250k rows. This means that most likely not all the data you queried has been returned, as the upper limit is 250k. Please partition your API call, e.g. by using only half the period in the first call.')
    } else if(nrow(result$data)>200000){
        cli::cli_inform(c("i" = "Your request has passed 200k rows. If you exceed 250k rows Comtrade will not return all data. You will have to slice your request in smaller parts."))
    }
    return(result$data)
  } else {
    return(data.frame(count = 0))
  }
}
