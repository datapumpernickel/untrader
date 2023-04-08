
# HS data -----------------------------------------------------------------


HS <- httr2::request("https://comtradeapi.un.org/files/v1/app/reference/HS.json") |>
  httr2::req_perform() |>
  httr2::resp_body_json(simplifyVector = T)
HS <- HS$results[c('id','text','parent')]

usethis::use_data(HS, overwrite = TRUE)

# reporter data -----------------------------------------------------------------


REPORTER <-
  httr2::request("https://comtradeapi.un.org/files/v1/app/reference/Reporters.json") |>
  httr2::req_perform() |>
  httr2::resp_body_json(simplifyVector = T)
REPORTER <-
  REPORTER$results[c(
    'id',
    'text',
    'reporter',
    'reporterDesc',
    'reporterCodeIsoAlpha2',
    'reporterCodeIsoAlpha3',
    'isGroup',
    'entryEffectiveDate'
  )]

usethis::use_data(REPORTER, overwrite = TRUE)


# partner data -----------------------------------------------------------------


PARTNER <-
  httr2::request("https://comtradeapi.un.org/files/v1/app/reference/partnerAreas.json") |>
  httr2::req_perform() |>
  httr2::resp_body_json(simplifyVector = T)
PARTNER <-
  PARTNER$results[c(
    'id',
    'text',
    'PartnerCode',
    'PartnerDesc',
    'PartnerCodeIsoAlpha2',
    'PartnerCodeIsoAlpha3',
    'isGroup',
    'entryEffectiveDate'
  )]

usethis::use_data(PARTNER, overwrite = TRUE)
