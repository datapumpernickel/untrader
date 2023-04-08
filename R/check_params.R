#' check_params
#'
#' @param frequency The frequency of returned trade data, default is 'A' for annual. Alternative is 'M' for monthly or "Q" for monthly.
#' @param commodity_classification The used classification scheme for the commodity code. As of now, only HS codes are supported, so default is 'HS'.
#' @param commodity_code The commodity code that you would like to investigate. The default value is 'TOTAL' to get trade across all commodities.
#' @param flow_direction The direction of flows, e.g. whether you would like to get data on reported imports or exports.
#' @param reporter This has to be a character vector specifying the reporter, so the country whose data you would like to query.
#' @param partner This has to be a character vector specifying the partner country, so the country with whom trade is being reported by the reporter country.
#' @param period This has to be a character vector specifying the year of interest.
#'
#' @param ... For future extension
#'
#' @return returns a list of named parameters for building a request
check_params <- function(frequency = 'A',
                         commodity_classification = 'HS',
                         commodity_code = NULL,
                         flow_direction = NULL,
                         reporter = NULL,
                         partner = NULL,
                         period = NULL,
                         verbose = F,
                         ...) {

  frequency <- check_freq(frequency)
  if (verbose) {
    cli::cli_inform(c("v" = "Checked validity of frequency!"))
  }

  commodity_classification <- check_clCode(commodity_classification)
  if (verbose) {
    cli::cli_inform(c("v" = "Checked validity of commodity_classification."))
  }

  flow_direction <- check_flowCode(flow_direction)
  if (verbose) {
    cli::cli_inform(c("v" = "Checked validity of flow_direction."))
  }

  commodity_code <- check_cmdCode(commodity_code)
  if (verbose) {
    cli::cli_inform(c("v" = "Checked validity of commodity_code."))
  }

  reporter <- check_reporterCode(reporter)
  if (verbose) {
    cli::cli_inform(c("v" = "Checked validity of reporter."))
  }

  partner <- check_partnerCode(partner)
  if (verbose) {
    cli::cli_inform(c("v" = "Checked validity of partner."))
  }

  period <- check_period(period)
  if (verbose) {
    cli::cli_inform(c("v" = "Checked validity of period."))
  }



  params <- list(
    query_params = list(
      cmdCode = commodity_code,
      flowCode = flow_direction,
      partnerCode = partner,
      reporterCode = reporter,
      period = period,
      motCode = '0',
      partner2Code = '0',
      ...
    ),
    url_params = list(freq = frequency,
                      clCode = commodity_classification)
  )

  return(params)
}

#' Check frequency code
#'
#' @param frequency A character string specifying the frequency of the data. Must be one of "A", "Q", or "M".
#'
#' @return A character string specifying the frequency of the data.
#'
#' @examplesIf interactive()
#' check_freq("A") # returns "A"
#' check_freq("Q") # returns "Q"
#' check_freq("M") # returns "M"
#' check_freq("D") # throws an error because "D" is not a valid frequency code
check_freq <- function(frequency) {
  rlang::arg_match(frequency, values = c('A', "Q", "M"))
  return(frequency)
}


#' Check HS classification code
#'
#' @param commodity_classification A character string specifying the HS classification code. Must be "HS".
#'
#' @return A character string specifying the HS classification code.
#'
#'
#' @examplesIf interactive()
#' untrader:::check_clCode("HS") # returns "HS"
#' untrader:::check_clCode("ISIC") # throws an error because "ISIC" is not a valid classification code
check_clCode <- function(commodity_classification) {
  rlang::arg_match(commodity_classification, values = c('HS'))
  return(commodity_classification)
}


#' Check flow code
#'
#' @param flow_direction A character string or vector specifying the type of trade flow. Must be one or more of "import", "export", "re-export", "re-import", or "all".
#'
#' @return A character vector specifying the trade flow codes.
#'
#' @examplesIf interactive()
#' check_flowCode("import") # returns "M"
#' check_flowCode(c("export", "re-export")) # returns "X,RX"
#' check_flowCode("trade") # throws an error because "trade" is not a valid flow code
#' check_flowCode(NULL) # throws an error because at least one flow code must be provided
#'
check_flowCode <- function(flow_direction) {
  rlang::arg_match(
    flow_direction,
    values = c('import', 'export', 're-export', 're-import', 'all'),
    multiple = T
  )
  # check that flow_direction code is not null
  if (!is.null(flow_direction)) {
    flow_direction <- as.character(flow_direction)
  } else{
    rlang::abort("You need to provide at least one flow_direction reference.")
  }

  if(length(flow_direction)>1 & any(flow_direction=='all')){
    rlang::abort("You can only provide 'all' as a single argument")
  }

  if(length(flow_direction)>1|!any(flow_direction=='all')){
    flow_direction <- stringr::str_replace_all(flow_direction,'^import$',"M")
    flow_direction <- stringr::str_replace_all(flow_direction,'^export$',"X")
    flow_direction <- stringr::str_replace_all(flow_direction,'^re-import$',"RM")
    flow_direction <- stringr::str_replace_all(flow_direction,'^re-export$',"RX")
    flow_direction <- flow_direction |> paste0(collapse = ',')
  } else if( flow_direction=='all') {
    flow_direction <- 'M,X,RM,RX'
  }
  return(flow_direction)
}


#' Check HS code
#'
#' @param commodity_code A character string or vector specifying the HS codes.
#'
#' @return A character vector specifying the HS codes.
#'
#' @examplesIf interactive()
#' check_cmdCode("01") # returns "01"
#' check_cmdCode(c("01", "02")) # returns "01,02"
#' check_cmdCode("ABC") # throws an error because "ABC" is not a valid HS code
#' check_cmdCode(NULL) # throws an error because at least one HS code must be provided
check_cmdCode <- function(commodity_code) {
  # check that commodity_code code is not null
  if (!is.null(commodity_code)) {
    commodity_code <- as.character(commodity_code)
  } else{
    rlang::abort("You need to provide at least one commodity_code reference.")
  }

  # check validity of arguments ---------------------------------------------
  # separating provided hs codes
  commodity_code <- stringr::str_squish(commodity_code)
  # if one of the HS codes is not in the list of valid HS codes send stop signal and list problems
  if (!all(commodity_code %in% untrader::HS$id)) {
    rlang::abort(paste0(
      "The following HS codes you provided are invalid: ",
      paste0(commodity_code[!commodity_code %in% untrader::HS$id], collapse = ", ")
    ))
  } else {
    commodity_code <- paste0(commodity_code, collapse = ',')
  }

  return(commodity_code)
}

#' Check the validity of reporter
#'
#' This function checks that the given reporter code is valid. If the code is not
#' valid, the function throws an error message indicating which codes are invalid.
#' It also converts the input to a proper format if necessary.
#'
#' @param reporter A character vector or string of comma-separated codes that
#'   represent the reporters in the trade data. The default value is NULL.
#'
#' @return A character vector of valid reporter IDs.
#'
#' @examplesIf interactive()
#' check_reporterCode("USA")
#' check_reporterCode(c("USA", "FRA"))
#' check_reporterCode("all")
check_reporterCode <- function(reporter) {
  # check that reporter code is valid
  if (!is.null(reporter)) {
    reporter <- as.character(reporter)
  } else{
    rlang::abort("You need to provide at least one reporter")
  }


  ## check if valid reporter code length and type
  reporter <- stringr::str_squish(reporter)
  ## get multiple values or single values that are not 'all'
  if (length(reporter) > 1 | !any(reporter == 'all')) {
    if (any(reporter == 'all')) {
      rlang::abort('"all" can only be provided as a single argument')
    }
    # if one of the reporter codes is not in the list of valid reporter codes send stop signal and list problems
    if (!all(reporter %in% untrader::REPORTER$reporterCodeIsoAlpha3)) {
      rlang::abort(paste0(
        "The following reporterCodes you provided are invalid: ",
        paste0(reporter[!reporter %in% untrader::REPORTER$reporterCodeIsoAlpha3], collapse = ", ")
      ))
    }
  }

  # create proper ids for reporter Code
  if (length(reporter) > 1 | !any(reporter == 'all')) {
    reporter <-
      untrader::REPORTER$id[untrader::REPORTER$reporterCodeIsoAlpha3 %in% reporter &
                              untrader::REPORTER$isGroup ==
                              F] |>
      paste0(collapse = ',')
  } else if (reporter == 'all') {
    reporter <-
      untrader::REPORTER$id[untrader::REPORTER$isGroup == F] |>
      paste0(collapse = ',')
  }

  return(reporter)
}


#' Check the validity of partner
#'
#' This function checks that the given partner code is valid. If the code is not
#' valid, the function throws an error message indicating which codes are invalid.
#' It also converts the input to a proper format if necessary.
#'
#' @param partner A character vector or string of comma-separated codes that
#'   represent the trade partners in the trade data. The default value is NULL.
#'
#' @return A character vector of valid partner IDs.
#'
#' @examplesIf interactive()
#' check_partnerCode("CAN")
#' check_partnerCode(c("CAN", "MEX"))
#' check_partnerCode("all")
check_partnerCode <- function(partner) {
  # check that partner code is valid
  if (!is.null(partner)) {
    partner <- as.character(partner)
  } else{
    rlang::abort("You need to provide at least one partner")
  }

  if (length(partner) > 1 | !any(partner == 'all')) {
    partner <- stringr::str_squish(partner)
    if (any(partner == 'all')) {
      rlang::abort('"all" can only be provided as a single argument')
    }
    # if one of the partnerCodes is not in the list of valid partnerCodes send stop signal and list problems
    if (!all(partner %in% c(untrader::PARTNER$PartnerCodeIsoAlpha3, 'world'))) {
      rlang::abort(paste0(
        "The following partner you provided are invalid: ",
        paste0(partner[!partner %in% c(untrader::PARTNER$PartnerCodeIsoAlpha3, 'world')], collapse = ", ")
      ))
    }
  }

  # create proper ids for partner
  if (length(partner) > 1 | !any(partner == 'all')) {
    values <-
      untrader::PARTNER$id[untrader::PARTNER$PartnerCodeIsoAlpha3 %in% partner &
                             untrader::PARTNER$isGroup == F] |>
      paste0(collapse = ',')

    if (any(stringr::str_detect(partner, 'world'))) {
      partner <- paste0(values, '0', collapse = "")
    } else {
      partner <- values
    }

  } else if (partner == 'world') {
    partner <- '0'
  } else if (partner == 'all') {
    partner <-
      untrader::PARTNER$id[untrader::PARTNER$isGroup == F] |>
      paste0(collapse = ',')
  }
  return(partner)
}



#' Check if period is valid and return a comma-separated string of periods
#'
#' @param period A period reference as an integer or character vector or a range as "start:end".
#'
#' @return A comma-separated string of periods.
#'
#' @examplesIf interactive()
#' check_period(1999:2002)
#' check_period(c('1999', '2020'))
#' check_period('2021')
#'
#' @export
check_period <- function(period) {
  # check that period code is not null
  if (is.null(period)) {
    rlang::abort("You need to provide at least one period reference.")
  }

  # check if input is a range (e.g. "1999:2002")
  if (length(period) == 1 && stringr::str_detect(period, ":")) {
   range <- stringr::str_split_1(period, ":") |> as.numeric()

  if (length(range) != 2 || !is.numeric(range)) {
      rlang::abort("Invalid period range.")
    }
   if(any(is.na(range))){
     rlang::abort("Must provide numbers as input")
   }
    period <- as.character(seq(from = as.numeric(range[1]), to = as.numeric(range[2])))
  } else {
    period <- as.character(period)
  }


  # remove any whitespace from period values
  period <- stringr::str_squish(period)

  # check if valid period and type
  if (!all(stringr::str_detect(period, "^[0-9]+$"))) {
    rlang::abort("Invalid period value(s).")
  }

  # create proper format for periods
  period <- paste(period, collapse = ",")
  return(period)
}
