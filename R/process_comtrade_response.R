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
        by = c("commodity_code" = 'id')
      ) |>
      poorman::left_join(
        untrader::PARTNER |>
          poorman::select(
            partner_iso3c = PartnerCodeIsoAlpha3,
            partner_description = PartnerDesc,
            id
          ),
        by = c("partner" = 'id')
      ) |>
      poorman::left_join(
        untrader::REPORTER |>
          poorman::select(
            reporter_iso3c = reporterCodeIsoAlpha3,
            reporter_description = reporterDesc,
            id
          ),
        by = c("reporter" = 'id')
      )
    return(result)
  } else {
    return(data.frame(count = 0))
  }
}
