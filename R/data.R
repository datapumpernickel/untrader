#' HS Codes for that data
#'
#' From the UN Comtrade API
#'
#' @format ## `HS`
#' A data frame with 8,262 rows and 3 columns:
#' \describe{
#'   \item{id}{ID used for querying}
#'   \item{text}{Description of Code}
#'   \item{parent}{parent id node}
#'   ...
#' }
#' @source <https://comtradeapi.un.org/files/v1/app/reference/HS.json>
"HS"


#' REPORTER Codes for that data
#'
#' From the UN Comtrade API
#'
#' @format ## `REPORTER`
#' A data frame with 8,262 rows and 3 columns:
#' \describe{
#'   \item{id}{ID used for querying}
#'   \item{text}{Description of Code}
#'   \item{isGroup}{whether it is not a country, but a group reference}
#'   ...
#' }
#' @source <https://comtradeapi.un.org/files/v1/app/reference/Reporters.json>
"REPORTER"

#' PARTNER Codes for that data
#'
#' From the UN Comtrade API
#'
#' @format ## `PARTNER`
#' A data frame with 8,262 rows and 3 columns:
#' \describe{
#'   \item{id}{ID used for querying}
#'   \item{text}{Description of Code}
#'   \item{isGroup}{whether it is not a country, but a group reference}
#'   ...
#' }
#' @source <https://comtradeapi.un.org/files/v1/app/reference/partnerAreas.json>
"PARTNER"
