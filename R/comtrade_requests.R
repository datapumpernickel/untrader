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
#'
#' @param process whether the httr2 request should be returned, which still includes the headers and possible error codes, or whether a data.frame with just the results should be returned
#' @param ... For future extension
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

  if(process){
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
  # make sure arguments exist and are not null ------------------------------

  # check that frequency is a valid input
  rlang::arg_match(freq, values = c('A', "Q", "M"))

  # check that clCode is a valid input
  rlang::arg_match(clCode, values = c('HS'))

  # check that cmdCode code is not null
  if (!is.null(cmdCode)) {
    cmdCode <- as.character(cmdCode)
  } else{
    stop("You need to provide at least one cmdCode reference.")
  }

  # check that flowCode code is not null
  if (!is.null(flowCode)) {
    flowCode <- as.character(flowCode)
  } else{
    stop("You need to provide at least one flowCode reference.")
  }

  # check that reporter code is valid
  if (!is.null(reporterCode)) {
    reporterCode <- as.character(reporterCode)
  } else{
    stop("You need to provide at least one reporterCode")
  }

  # check that partner code is valid
  if (!is.null(partnerCode)) {
    partnerCode <- as.character(partnerCode)
  } else{
    stop("You need to provide at least one partnerCode")
  }

  # check that period code is not null
  if (!is.null(period)) {
    period <- as.character(period)
  } else{
    stop("You need to provide at least one period reference.")
  }


  # check validity of arguments ---------------------------------------------


  #check if cmdCode is comma separated
  if (stringr::str_detect(cmdCode, ",")) {
    # separating provided hs codes
    values <-
      stringr::str_split_1(cmdCode, pattern = ',') |> stringr::str_squish()
    # if one of the HS codes is not in the list of valid HS codes send stop signal and list problems
    if (!all(values %in% untrader::HS$id)) {
      stop(paste0(
        "The following HS codes you provided are invalid: ",
        paste0(values[!values %in% untrader::HS$id], collapse = ", ")
      ))
    }
    cmdCode <- paste0(values, collapse = ',')
  } else {
    # if not a csv, check if value in list of valid HS codes
    if (!cmdCode %in% untrader::HS$id) {
      stop("The HS code you provided is invalid.")
    }
  }

  #check if reporterCode is comma separated
  if (stringr::str_detect(reporterCode, ",")) {
    # separating provided hs codes
    values <-
      stringr::str_split_1(reporterCode, pattern = ',') |> stringr::str_squish()
    # if one of the HS codes is not in the list of valid reporter codes send stop signal and list problems
    if (!all(values %in% untrader::REPORTER$reporterCodeIsoAlpha3)) {
      stop(paste0(
        "The following reporterCodes you provided are invalid: ",
        paste0(values[!values %in% untrader::REPORTER$reporterCodeIsoAlpha3], collapse = ", ")
      ))
    }
    reporterCode <- paste0(values, collapse = ',')
  } else {
    # if not a csv, check if value in list of valid reporter codes
    if (!reporterCode %in% c(untrader::REPORTER$reporterCodeIsoAlpha3, 'all')) {
      stop("The reporterCode you provided is invalid.")
    }
  }



  #check if partnerCode is comma separated
  if (stringr::str_detect(partnerCode, ",")) {
    # separating provided hs codes
    values <-
      stringr::str_split_1(partnerCode, pattern = ',') |> stringr::str_squish()
    # if one of the HS codes is not in the list of valid HS codes send stop signal and list problems
    if (!all(values %in% c(untrader::PARTNER$PartnerCodeIsoAlpha3, 'world'))) {
      stop(paste0(
        "The following partnerCode you provided are invalid: ",
        paste0(values[!values %in% c(untrader::PARTNER$PartnerCodeIsoAlpha3, 'world')], collapse = ", ")
      ))
    }
    partnerCode <- paste0(values, collapse = ',')
  } else {
    # if not a csv, check if value in list of valid HS codes
    if (!partnerCode %in% c(untrader::PARTNER$PartnerCodeIsoAlpha3,
                            'all',
                            'world')) {
      stop("The partnerCode you provided is invalid.")
    }
  }


  # replace codes with proper values ----------------------------------------

  # create proper ids for reporter Code
  if (stringr::str_detect(reporterCode, ",")) {
    values <-
      stringr::str_split_1(reporterCode, pattern = ',') |> stringr::str_squish()
    reporterCode <-
      untrader::REPORTER$id[untrader::REPORTER$reporterCodeIsoAlpha3 %in% values &
                                 untrader::REPORTER$isGroup ==
                                 F] |>
      paste0(collapse = ',')

  } else if (!reporterCode == 'all') {
    reporterCode <-
      untrader::REPORTER$id[untrader::REPORTER$reporterCodeIsoAlpha3 %in% reporterCode &
                                 untrader::REPORTER$isGroup ==
                                 F] |>
      paste0(collapse = ',')
  } else {
    reporterCode <-
      untrader::REPORTER$id[untrader::REPORTER$isGroup == F] |>
      paste0(collapse = ',')
  }


  # create proper ids for reporter Code
  if (stringr::str_detect(partnerCode, ",")) {
    values <-
      stringr::str_split_1(partnerCode, pattern = ',') |> stringr::str_squish()
    partnerCode <-
      untrader::PARTNER$id[untrader::PARTNER$PartnerCodeIsoAlpha3 %in% values &
                                untrader::PARTNER$isGroup == F] |>
      paste0(collapse = ',')

    if (any(stringr::str_detect(values, 'world'))) {
      partnerCode <- paste0(partnerCode, ',0')
    }

  } else if (partnerCode == 'all') {
    partnerCode <-
      untrader::PARTNER$id[untrader::PARTNER$isGroup == F] |>
      paste0(collapse = ',')
  } else if (partnerCode == 'world') {
    partnerCode <- '0'
  } else {
    partnerCode <-
      untrader::PARTNER$id[untrader::PARTNER$PartnerCodeIsoAlpha3 %in% partnerCode &
                                untrader::PARTNER$isGroup == F] |>
      paste0(collapse = ',')
  }



  params <- list(
    cmdCode = cmdCode,
    flowCode = flowCode,
    partnerCode = partnerCode,
    reporterCode = reporterCode,
    period = period,
    ...
  )

  return(list(params, freq = freq, clCode = clCode))
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
  query_params <- params[[1]]

  freq <- params$freq

  clCode <- params$clCode

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
perform_comtrade_request <- function(req, requests_per_second = 10/60) {
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

  result <- result$data |>
    poorman::left_join(untrader::HS |>
                         poorman::rename(cmd_description = text) |>
                         poorman::select(cmd_description, id),
                       by = c("cmdCode"='id')) |>
    poorman::left_join(untrader::PARTNER |>
                         poorman::select(partner_iso3c = PartnerCodeIsoAlpha3,
                                         partner_description = PartnerDesc, id),
                       by = c("partnerCode"='id'))|>
    poorman::left_join(untrader::REPORTER |>
                         poorman::select(reporter_iso3c = reporterCodeIsoAlpha3,
                                         reporter_description = reporterDesc, id),
                       by = c("reporterCode"='id'))

  return(result)
}
