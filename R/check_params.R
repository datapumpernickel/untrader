#' check_params
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

#' Check frequency code
#'
#' @param freq A character string specifying the frequency of the data. Must be one of "A", "Q", or "M".
#'
#' @return A character string specifying the frequency of the data.
#'
#' @examplesIf interactive()
#' check_freq("A") # returns "A"
#' check_freq("Q") # returns "Q"
#' check_freq("M") # returns "M"
#' check_freq("D") # throws an error because "D" is not a valid frequency code
check_freq <- function(freq) {
  rlang::arg_match(freq, values = c('A', "Q", "M"))
  return(freq)
}


#' Check HS classification code
#'
#' @param clCode A character string specifying the HS classification code. Must be "HS".
#'
#' @return A character string specifying the HS classification code.
#'
#'
#' @examplesIf interactive()
#' untrader:::check_clCode("HS") # returns "HS"
#' untrader:::check_clCode("ISIC") # throws an error because "ISIC" is not a valid classification code
check_clCode <- function(clCode) {
  rlang::arg_match(clCode, values = c('HS'))
  return(clCode)
}


#' Check flow code
#'
#' @param flowCode A character string or vector specifying the type of trade flow. Must be one or more of "import", "export", "re-export", "re-import", or "all".
#'
#' @return A character vector specifying the trade flow codes.
#'
#' @examplesIf interactive()
#' check_flowCode("import") # returns "M"
#' check_flowCode(c("export", "re-export")) # returns "X,RX"
#' check_flowCode("trade") # throws an error because "trade" is not a valid flow code
#' check_flowCode(NULL) # throws an error because at least one flow code must be provided
#'
check_flowCode <- function(flowCode) {
  rlang::arg_match(
    flowCode,
    values = c('import', 'export', 're-export', 're-import', 'all'),
    multiple = T
  )
  # check that flowCode code is not null
  if (!is.null(flowCode)) {
    flowCode <- as.character(flowCode)
  } else{
    rlang::abort("You need to provide at least one flowCode reference.")
  }

  if(length(flowCode)>1 & any(flowCode=='all')){
    rlang::abort("You can only provide 'all' as a single argument")
  }

  if(length(flowCode)>1|!any(flowCode=='all')){
    flowCode <- stringr::str_replace_all(flowCode,'^import$',"M")
    flowCode <- stringr::str_replace_all(flowCode,'^export$',"X")
    flowCode <- stringr::str_replace_all(flowCode,'^re-import$',"RM")
    flowCode <- stringr::str_replace_all(flowCode,'^re-export$',"RX")
    flowCode <- flowCode |> paste0(collapse = ',')
  } else if( flowCode=='all') {
    flowCode <- 'M,X,RM,RX'
  }
  return(flowCode)
}


#' Check HS code
#'
#' @param cmdCode A character string or vector specifying the HS codes.
#'
#' @return A character vector specifying the HS codes.
#'
#' @examplesIf interactive()
#' check_cmdCode("01") # returns "01"
#' check_cmdCode(c("01", "02")) # returns "01,02"
#' check_cmdCode("ABC") # throws an error because "ABC" is not a valid HS code
#' check_cmdCode(NULL) # throws an error because at least one HS code must be provided
check_cmdCode <- function(cmdCode) {
  # check that cmdCode code is not null
  if (!is.null(cmdCode)) {
    cmdCode <- as.character(cmdCode)
  } else{
    rlang::abort("You need to provide at least one cmdCode reference.")
  }

  # check validity of arguments ---------------------------------------------
  # separating provided hs codes
  cmdCode <- stringr::str_squish(cmdCode)
  # if one of the HS codes is not in the list of valid HS codes send stop signal and list problems
  if (!all(cmdCode %in% untrader::HS$id)) {
    rlang::abort(paste0(
      "The following HS codes you provided are invalid: ",
      paste0(cmdCode[!cmdCode %in% untrader::HS$id], collapse = ", ")
    ))
  } else {
    cmdCode <- paste0(cmdCode, collapse = ',')
  }

  return(cmdCode)
}

#' Check the validity of reporterCode
#'
#' This function checks that the given reporter code is valid. If the code is not
#' valid, the function throws an error message indicating which codes are invalid.
#' It also converts the input to a proper format if necessary.
#'
#' @param reporterCode A character vector or string of comma-separated codes that
#'   represent the reporters in the trade data. The default value is NULL.
#'
#' @return A character vector of valid reporter IDs.
#'
#' @examplesIf interactive()
#' check_reporterCode("USA")
#' check_reporterCode(c("USA", "FRA"))
#' check_reporterCode("all")
check_reporterCode <- function(reporterCode) {
  # check that reporter code is valid
  if (!is.null(reporterCode)) {
    reporterCode <- as.character(reporterCode)
  } else{
    rlang::abort("You need to provide at least one reporterCode")
  }


  ## check if valid reporter code length and type
  reporterCode <- stringr::str_squish(reporterCode)
  ## get multiple values or single values that are not 'all'
  if (length(reporterCode) > 1 | !any(reporterCode == 'all')) {
    if (any(reporterCode == 'all')) {
      rlang::abort('"all" can only be provided as a single argument')
    }
    # if one of the reporter codes is not in the list of valid reporter codes send stop signal and list problems
    if (!all(reporterCode %in% untrader::REPORTER$reporterCodeIsoAlpha3)) {
      rlang::abort(paste0(
        "The following reporterCodes you provided are invalid: ",
        paste0(reporterCode[!reporterCode %in% untrader::REPORTER$reporterCodeIsoAlpha3], collapse = ", ")
      ))
    }
  }

  # create proper ids for reporter Code
  if (length(reporterCode) > 1 | !any(reporterCode == 'all')) {
    reporterCode <-
      untrader::REPORTER$id[untrader::REPORTER$reporterCodeIsoAlpha3 %in% reporterCode &
                              untrader::REPORTER$isGroup ==
                              F] |>
      paste0(collapse = ',')
  } else if (reporterCode == 'all') {
    reporterCode <-
      untrader::REPORTER$id[untrader::REPORTER$isGroup == F] |>
      paste0(collapse = ',')
  }

  return(reporterCode)
}


#' Check the validity of partnerCode
#'
#' This function checks that the given partner code is valid. If the code is not
#' valid, the function throws an error message indicating which codes are invalid.
#' It also converts the input to a proper format if necessary.
#'
#' @param partnerCode A character vector or string of comma-separated codes that
#'   represent the trade partners in the trade data. The default value is NULL.
#'
#' @return A character vector of valid partner IDs.
#'
#' @examplesIf interactive()
#' check_partnerCode("CAN")
#' check_partnerCode(c("CAN", "MEX"))
#' check_partnerCode("all")
check_partnerCode <- function(partnerCode) {
  # check that partner code is valid
  if (!is.null(partnerCode)) {
    partnerCode <- as.character(partnerCode)
  } else{
    rlang::abort("You need to provide at least one partnerCode")
  }

  if (length(partnerCode) > 1 | !any(partnerCode == 'all')) {
    partnerCode <- stringr::str_squish(partnerCode)
    if (any(partnerCode == 'all')) {
      rlang::abort('"all" can only be provided as a single argument')
    }
    # if one of the partnerCodes is not in the list of valid partnerCodes send stop signal and list problems
    if (!all(partnerCode %in% c(untrader::PARTNER$PartnerCodeIsoAlpha3, 'world'))) {
      rlang::abort(paste0(
        "The following partnerCode you provided are invalid: ",
        paste0(partnerCode[!partnerCode %in% c(untrader::PARTNER$PartnerCodeIsoAlpha3, 'world')], collapse = ", ")
      ))
    }
  }

  # create proper ids for partnerCode
  if (length(partnerCode) > 1 | !any(partnerCode == 'all')) {
    values <-
      untrader::PARTNER$id[untrader::PARTNER$PartnerCodeIsoAlpha3 %in% partnerCode &
                             untrader::PARTNER$isGroup == F] |>
      paste0(collapse = ',')

    if (any(stringr::str_detect(partnerCode, 'world'))) {
      partnerCode <- paste0(values, '0', collapse = "")
    } else {
      partnerCode <- values
    }

  } else if (partnerCode == 'world') {
    partnerCode <- '0'
  } else if (partnerCode == 'all') {
    partnerCode <-
      untrader::PARTNER$id[untrader::PARTNER$isGroup == F] |>
      paste0(collapse = ',')
  }
  return(partnerCode)
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
