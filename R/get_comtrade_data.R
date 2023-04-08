#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

#' get_comtrade_data
#'
#' @param frequency The frequency of returned trade data, default is 'A' for annual. Alternative is 'M' for monthly or "Q" for monthly.
#' @param commodity_classification The used classification scheme for the commodity code. As of now, only HS codes are supported, so default is 'HS'.
#' @param commodity_code The commodity code that you would like to investigate. The default value is NULL. Multiple values can be supplied as a comma separated string.
#' @param flow_direction The direction of flows, e.g. whether you would like to get data on reported imports or exports. Possible values are "M" for imports, "X" for exports. Multiple values can be supplied as a comma separated string.
#' @param reporter This has to be a character vector specifying the reporter in the iso3c format. The reporter is the country that supplied the data to the UN. Multiple values can be supplied as a comma separated string. The string 'all' can be supplied to return values for all reporter countries that are not labelled as 'group' by the UN (e.g. ASEAN countries)
#' @param partner This has to be a character vector specifying the partner country in the iso3c format. The partner area is the country with whom the reporter has reported trade relations. Multiple values can be supplied as a comma separated string. The string 'all' can be supplied to return values for all partner countries that are not labelled as 'group' by the UN (e.g. ASEAN countries or the entire World). The value 'world' can be supplied, to include trade with all partner countries aggregated globally.
#' @param period This has to be a character vector specifying the year of interest. Multiple values can be supplied as a comma separated string.
#' @param process Whether to return the raw httr2 request or a dataframe with the results.
#' @param verbose whether the function sends status updates to the console
#'
#' @param ... For future extension
#'
#' @examplesIf interactive()
#' get_comtrade_data(frequency = 'A',
#' commodity_classification = 'HS',
#' commodity_code = c('2204','2203'),
#' flow_direction = 'export',
#' reporter = c("ARG","GBR"),
#' partner = 'world',
#' period = "2018:2021",
#' process = T)
#'
#' @export
#' @return returns a list of named parameters for building a request
get_comtrade_data <- function(frequency = 'A',
                              commodity_classification = 'HS',
                              commodity_code = NULL,
                              flow_direction = NULL,
                              reporter = NULL,
                              partner = NULL,
                              period = NULL,
                              process = T,
                              verbose = F,
                              ...) {
  ## compile codes
  params <- check_params(
    frequency = frequency,
    commodity_classification = commodity_classification,
    commodity_code = commodity_code,
    flow_direction = flow_direction,
    partner = partner,
    reporter = reporter,
    period = period,
    verbose = verbose,
    ...
  )

  req <- build_comtrade_request(params, verbose = verbose)

  resp <- perform_comtrade_request(req, verbose = verbose)

  if (process) {
    result <- process_comtrade_response(resp, verbose = verbose)
    return(result)
  } else{
    return(resp)
  }
}


