#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

#' get_comtrade_data
#'
#' @param freq The frequency of returned trade data, default is 'A' for annual. Alternative is 'M' for monthly or "Q" for monthly.
#' @param clCode The used classification scheme for the commodity code. As of now, only HS codes are supported, so default is 'HS'.
#' @param cmdCode The commodity code that you would like to investigate. The default value is NULL. Multiple values can be supplied as a comma separated string.
#' @param flowCode The direction of flows, e.g. whether you would like to get data on reported imports or exports. Possible values are "M" for imports, "X" for exports. Multiple values can be supplied as a comma separated string.
#' @param reporterCode This has to be a character vector specifying the reporter in the iso3c format. The reporter is the country that supplied the data to the UN. Multiple values can be supplied as a comma separated string. The string 'all' can be supplied to return values for all reporter countries that are not labelled as 'group' by the UN (e.g. ASEAN countries)
#' @param partnerCode This has to be a character vector specifying the partner country in the iso3c format. The partner area is the country with whom the reporter has reported trade relations. Multiple values can be supplied as a comma separated string. The string 'all' can be supplied to return values for all partner countries that are not labelled as 'group' by the UN (e.g. ASEAN countries or the entire World). The value 'world' can be supplied, to include trade with all partner countries aggregated globally.
#' @param period This has to be a character vector specifying the year of interest. Multiple values can be supplied as a comma separated string.
#' @param process Whether to return the raw httr2 request or a dataframe with the results.
#' @param ... For future extension
#'
#' @examplesIf interactive()
#' get_comtrade_data(freq = 'A',
#' clCode = 'HS',
#' cmdCode = c('2204','2203'),
#' flowCode = 'export',
#' reporterCode = c("ARG","GBR"),
#' partnerCode = 'world',
#' period = "2018:2021",
#' process = T)
#'
#' @export
#' @return returns a list of named parameters for building a request
get_comtrade_data <- function(freq = 'A',
                              clCode = 'HS',
                              cmdCode = NULL,
                              flowCode = NULL,
                              reporterCode = NULL,
                              partnerCode = NULL,
                              period = NULL,
                              process = T,
                              ...) {
  ## compile codes
  params <- check_params(
    freq = freq,
    clCode = clCode,
    cmdCode = cmdCode,
    flowCode = flowCode,
    partnerCode = partnerCode,
    reporterCode = reporterCode,
    period = period,
    ...
  )

  req <- build_comtrade_request(params)


  resp <- perform_comtrade_request(req)

  if (process) {
    result <- process_comtrade_response(resp)
    return(result)
  } else{
    return(resp)
  }


}


#' compile_params
#'
#' @param freq The frequency of returned trade data, default is 'A' for annual. Alternative is 'M' for monthly or "Q" for monthly.
#' @param clCode The used classification scheme for the commodity code. As of now, only HS codes are supported, so default is 'HS'.
#' @param cmdCode The commodity code that you would like to investigate. The default value is 'TOTAL' to get trade across all commodities.
#' @param flowCode The direction of flows, e.g. whether you would like to get data on reported imports or exports.
#' @param reporterCode This has to be a character vector specifying the reporter, so the country whose data you would like to query.
#' @param partnerCode This has to be a character vector specifying the partner country, so the country with whom trade is being reported by the reporter country.
#' @param period This has to be a character vector specifying the year of interest.
#'
#' @param ... For future extension
#'
#' @return returns a list of named parameters for building a request
check_params <- function(freq = 'A',
                         clCode = 'HS',
                         cmdCode = NULL,
                         flowCode = NULL,
                         reporterCode = NULL,
                         partnerCode = NULL,
                         period = NULL,
                         ...) {

  freq <- check_freq(freq)
  clCode <- check_clCode(clCode)
  flowCode <- check_flowCode(flowCode)
  cmdCode <- check_cmdCode(cmdCode)
  reporterCode <- check_reporterCode(reporterCode)
  partnerCode <- check_partnerCode(partnerCode)
  period <- check_period(period)




  params <- list(
    query_params = list(
      cmdCode = cmdCode,
      flowCode = flowCode,
      partnerCode = partnerCode,
      reporterCode = reporterCode,
      period = period,
      motCode = '0',
      partner2Code = '0',
      ...
    ),
    url_params = list(freq = freq,
                      clCode = clCode)
  )

  return(params)
}


#' build_comtrade_request
#'
#' This function takes the necessary parameters and creates a httr2 request to be performed
#' this request can then be used in a second function, to actually return the data
#'
#'@param params a named vector of parameters for the comtrade request
#'
#' @param primary_token Your primary token. Default is to check in environment for stored token, if not passed through the `set_primary_comtrade_key` function
#' @return a httr2 request object
build_comtrade_request <- function(params,
                                   primary_token = get_primary_comtrade_key()) {
  query_params <- params$query_params

  freq <- params$url_params$freq

  clCode <- params$url_params$clCode

  res <-
    httr2::request("https://comtradeapi.un.org/data/v1/get/C") |>
    httr2::req_url_path_append(freq) |>
    httr2::req_url_path_append(clCode) |>
    httr2::req_headers(`Ocp-Apim-Subscription-Key` = primary_token) |>
    httr2::req_url_query(!!!query_params)

  return(res)
}

comtrade_error_body <- function(resp) {
  body <- httr2::resp_body_json(resp, simplifyVector = T)

  message <- body$errorObject$errorMessage
  if (!is.null(message)) {
    message <- c(message)
  }
  message
}


#' make_comtrade_request
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

#' process_comtrade_response
#' The function adds the respective iso codes for the reporter and partner countries, as well as the commodity code description.
#' @param resp a valid httr2 response object created from the function `perform_comtrade_request`
#'
#' @return a data.frame object with the results
process_comtrade_response <- function(resp) {
  result <- resp |>
    httr2::resp_body_json(simplifyVector = T)

  if (length(result$data) > 0) {
    result <- result$data |>
      poorman::left_join(
        untrader::HS |>
          poorman::rename(cmd_description = text) |>
          poorman::select(cmd_description, id),
        by = c("cmdCode" = 'id')
      ) |>
      poorman::left_join(
        untrader::PARTNER |>
          poorman::select(
            partner_iso3c = PartnerCodeIsoAlpha3,
            partner_description = PartnerDesc,
            id
          ),
        by = c("partnerCode" = 'id')
      ) |>
      poorman::left_join(
        untrader::REPORTER |>
          poorman::select(
            reporter_iso3c = reporterCodeIsoAlpha3,
            reporter_description = reporterDesc,
            id
          ),
        by = c("reporterCode" = 'id')
      )
    return(result)
  } else {
    return(data.frame(count = 0))
  }
}
