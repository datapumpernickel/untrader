#' Get trade data from the Comtrade API
#'
#' As the package is under development, please note, that the following parameters will be supplied internally in a fixed manner:
#' * customsCode is set to C00, which is the most general
#' * motCode is set to 0, which is all modes of transportation
#' * partner2Code is set to 0, which is the most general settings returning all data
#'
#' @param frequency The frequency of returned trade data, default is 'A' for annual. Alternative is 'M' for monthly. The default value is 'A'.
#' @param commodity_classification The used classification scheme for the commodity code. As of now, only HS codes are supported, so default is 'HS'.
#' @param commodity_code The commodity code that you would like to investigate. The default value is TOTAL, implying the sum of all commodities. Multiple values can be supplied as a character vector.
#' @param flow_direction The direction of flows, e.g. whether you would like to get data on reported imports or exports. Possible values are "import" for imports, "export" for exports. Multiple values can be supplied as a character vector. The default value is 'all' for imports, exports, re-imports and re-exports.
#' @param reporter This has to be a vector of character values specifying one or multiple reporter countries in the iso3c format. The reporter is the country that supplied the data to the UN. The string 'all' can be supplied to return values for all reporter countries that are not labelled as 'group' by the UN (e.g. ASEAN countries)
#' @param partner This has to be a vector of character values specifying the partner country in the iso3c format. The partner area is the country with whom the reporter has reported trade relations. The string 'all' can be supplied to return values for all partner countries that are not labelled as 'group' by the UN (e.g. ASEAN countries or the entire World). The value 'world' can be supplied, to include trade with all partner countries aggregated globally.
#' @param start_date Start date of a time period.
#' @param end_date End date of a time period.
#' @param primary_token Your primary token. Default is to check in environment for stored token, if not passed through the `set_primary_comtrade_key` function
#' @param process Whether to return the raw httr2 request or a data.frame with the results.
#' @param verbose whether the function sends status updates to the console
#' @param ... You can pass in further parameters to the API that will not be checked and passed on as query parameters as is.
#' @param mode_of_transport The Mode of Transport is set to `0`, which is the default for TOTAL across all modes of transportation. This parameter is so far not being validated.
#' @param partner_2 This value is set as a default to `0`, which is most likely the most general value and also the default on the Comtrade website.
#' @param customs_code The customs code is set to the default of `C00` which is the default for TOTAL across all customs procedures.
#'
#' @examplesIf interactive()
#' get_comtrade_data(frequency = 'A',
#' commodity_classification = 'HS',
#' commodity_code = c('2204','2203'),
#' flow_direction = 'export',
#' reporter = c("ARG","GBR"),
#' partner = 'world',
#' start_date = "2018",
#' end_date = "2019",
#' process = T)
#'
#' @export
#' @return returns a data.frame with trade data or if `process = F` returns a httr2response object.
get_comtrade_data <- function(frequency = 'A',
                              commodity_classification = 'HS',
                              commodity_code = 'TOTAL',
                              flow_direction = 'all',
                              reporter = NULL,
                              partner = NULL,
                              start_date = NULL,
                              end_date = NULL,
                              process = T,
                              verbose = F,
                              primary_token = get_primary_comtrade_key(),
                              mode_of_transport = '0',
                              partner_2 = '0',
                              customs_code ='C00',
                              ...) {
  ## compile codes
  params <- check_params(
    frequency = frequency,
    commodity_classification = commodity_classification,
    commodity_code = commodity_code,
    flow_direction = flow_direction,
    partner = partner,
    reporter = reporter,
    start_date = start_date,
    end_date = end_date,
    verbose = verbose,
    includeDesc = "TRUE",
    ...
  )

  req <- build_comtrade_request(params, verbose = verbose,primary_token = primary_token)

  resp <- perform_comtrade_request(req, verbose = verbose)

  if (process) {
    result <- process_comtrade_response(resp, verbose = verbose)
    return(result)
  } else{
    return(resp)
  }
}


