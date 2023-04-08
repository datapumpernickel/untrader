test_that("test that invalid commodity_code is detected", {
  testthat::expect_error(check_params(trade_direction = 'A',
                                      commodity_classification = 'HS',
                                      commodity_code = "123",
                                      flow_direction = 'import',
                                      partner = "276",
                                      reporter = "32",
                                      period = "1999"),regexp = ': 123')
})

test_that("test that invalid commodity_code in csv is detected and returned", {
  testthat::expect_error(check_params(trade_direction = 'A',
                                      commodity_classification = 'HS',
                                      commodity_code = c("blabla","01"),
                                      flow_direction = 'import',
                                      partner = "276",
                                      reporter = "32",
                                      period = "1999"),regexp = ': blabla')
})

test_that("test that at least one cmdcode is provided", {
  testthat::expect_error(check_params(trade_direction = 'A',
                                      commodity_classification = 'HS',
                                      commodity_code = NULL,
                                      flow_direction = 'import',
                                      partner = "276",
                                      reporter = "32",
                                      period = "1999"),regexp = 'You need to provide at least one')
})

test_that("test that invalid reporter is detected", {
  testthat::expect_error(check_params(trade_direction = 'A',
                                      commodity_classification = 'HS',
                                      commodity_code = "01",
                                      flow_direction = 'import',
                                      partner = "DEU",
                                      reporter = "blabla",
                                      period = "1999"),regexp = ': blabla')
})

test_that("test that invalid reporter in csv is detected and returned", {
  testthat::expect_error(check_params(trade_direction = 'A',
                                      commodity_classification = 'HS',
                                      commodity_code = "01",
                                      flow_direction = 'import',
                                      partner = "276",
                                      reporter = c("DEU","blabla"),
                                      period = "1999"),regexp = ': blabla')
})

test_that("test that at least one reporter is provided", {
  testthat::expect_error(check_params(trade_direction = 'A',
                                      commodity_classification = 'HS',
                                      commodity_code = '01',
                                      flow_direction = 'import',
                                      partner = '4',
                                      reporter = NULL,
                                      period = "1999"),regexp = 'You need to provide at least one')
})

test_that("test that invalid partner is detected", {
  testthat::expect_error(check_params(trade_direction = 'A',
                                      commodity_classification = 'HS',
                                      commodity_code = "01",
                                      flow_direction = 'import',
                                      partner = "276",
                                      reporter = "DEU",
                                      period = "1999"),regexp = ': 276')
})

test_that("test that invalid partner in csv is detected and returned", {
  testthat::expect_error(check_params(trade_direction = 'A',
                                      commodity_classification = 'HS',
                                      commodity_code = "01",
                                      flow_direction = 'import',
                                      partner = c('world','test'),
                                      reporter = "DEU",
                                      period = "1999"),regexp = ': test')
})

test_that("test that at least one partner is provided", {
  testthat::expect_error(check_params(trade_direction = 'A',
                                      commodity_classification = 'HS',
                                      commodity_code = '01',
                                      flow_direction = 'import',
                                      partner = NULL,
                                      reporter = 'DEU',
                                      period = "1999"),regexp = 'You need to provide at least one')
})

test_that("test that wrong flowcode is found", {
  testthat::expect_error(check_params(trade_direction = 'A',
                                      commodity_classification = 'HS',
                                      commodity_code = '01',
                                      flow_direction = c('M','import'),
                                      partner = 'AUS',
                                      reporter = 'DEU',
                                      period = "1999"),regexp = 'not "M"')
})

test_that("test that wrong flowcode is found", {
  testthat::expect_error(check_params(trade_direction = 'A',
                                      commodity_classification = 'HS',
                                      commodity_code = '01',
                                      flow_direction = c('M','import'),
                                      partner = 'AUS',
                                      reporter = 'DEU',
                                      period = "1999"),regexp = 'not "M"')
})

test_that("test that flowcode cannot be null", {
  testthat::expect_error(check_params(trade_direction = 'A',
                                      commodity_classification = 'HS',
                                      commodity_code = '01',
                                      flow_direction = NULL,
                                      partner = 'AUS',
                                      reporter = 'DEU',
                                      period = "1999"),regexp = ', not `NULL`')
})

test_that("check_params returns a list of parameters if all inputs are valid", {
  params <- check_params(commodity_code = "01", flow_direction = "import", reporter = "USA", partner = "CAN", period = "2022")
  expect_named(params, c("query_params", "url_params"))
  expect_named(params[[2]], c("trade_direction", "commodity_classification"))
  expect_named(params[[1]], c("commodity_code", "flow_direction", "partner", "reporter",'period','motCode','partner2Code'))
  expect_equal(params[[1]]$commodity_code, "01")
  expect_equal(params[[1]]$flow_direction, "M")
  expect_equal(params[[1]]$reporter, "842,841")
  expect_equal(params[[1]]$partner, "124")
  expect_equal(params[[1]]$period, "2022")
})

test_that("check_params() creates proper ids for reporter 'all'", {
  params <- check_params(commodity_code = "01", flow_direction = "import", partner = "CAN", period = "2022",reporter = 'all')
  expect_equal(params[[1]]$reporter, paste0(untrader::REPORTER$id[!untrader::REPORTER$isGroup], collapse = ','))
})

test_that("check_params() creates proper ids for reporter 'USA'", {
  params <- check_params(commodity_code = "01", flow_direction = "import", partner = "CAN", period = "2022",reporter = 'USA')
  expect_equal(params[[1]]$reporter, untrader::REPORTER$id[untrader::REPORTER$reporterCodeIsoAlpha3 == 'USA' & !untrader::REPORTER$isGroup] |> paste0(collapse=','))
})

test_that("check_params() creates proper ids for partner 'all'", {
  params <- check_params(commodity_code = "01", flow_direction = "import", partner = "all", period = "2022",reporter = 'USA')
  expect_equal(params[[1]]$partner, paste0(untrader::PARTNER$id[!untrader::PARTNER$isGroup], collapse = ','))
})

test_that("check_params() creates proper ids for partner 'world'", {
  params <- check_params(commodity_code = "01", flow_direction = "import", partner = "world", period = "2022",reporter = 'USA')
  expect_equal(params[[1]]$partner, '0')
})

test_that("check_params() creates proper ids for partner 'USA'", {
  params <- check_params(commodity_code = "01", flow_direction = "import", partner = "USA", period = "2022",reporter = 'USA')
  expect_equal(params[[1]]$partner, untrader::PARTNER$id[untrader::PARTNER$PartnerCodeIsoAlpha3 == 'USA' & !untrader::PARTNER$isGroup] |> paste0(collapse=','))
})

test_that("check_params() creates proper ids for multiple partnerCodes", {
  params <- check_params(commodity_code = "01",
                         flow_direction = "import",
                         partner = c("USA","CAN"),
                         period = "2022",
                         reporter = 'USA')
  expect_equal(params[[1]]$partner, paste0(untrader::PARTNER$id[untrader::PARTNER$PartnerCodeIsoAlpha3 %in% c('USA', 'CAN') & !untrader::PARTNER$isGroup], collapse = ','))
})


test_that("check_params() all can only be provided as single argument partnerCodes", {
  expect_error(check_params(commodity_code = "01", flow_direction = "import", partner = c("all","CAN"), period = "2022",reporter = 'USA'),regexp = '"all" can only be provided as a single argument')
})

test_that("check_params() all can only be provided as single argument reporterCodes", {
  expect_error(check_params(commodity_code = "01", flow_direction = "import", reporter = c("all","CAN"), period = "2022",partner = 'USA'),regexp = '"all" can only be provided as a single argument')
})


test_that("check_params() creates proper ids for multiple flowCodes", {
  params <- check_params(commodity_code = "01", flow_direction = c('re-import','import'), partner = "USA", period = "2022",reporter = 'USA')
  expect_equal(params[[1]]$flow_direction, 'RM,M')
})

test_that("check_params() creates error for multiple values with 'all'", {
  expect_error(check_params(commodity_code = "01", flow_direction = c('all','import'), partner = "USA", period = "2022",reporter = 'USA'))
})

test_that("check_params() creates proper ids for flow_direction 'all'", {
  params <- check_params(commodity_code = "01", flow_direction = c('all'), partner = "USA", period = "2022",reporter = 'USA')
  expect_equal(params[[1]]$flow_direction, 'M,X,RM,RX')
})

test_that("check_period returns comma-separated string when passed a vector of integers", {
  expect_equal(check_period(1999:2002), "1999,2000,2001,2002")
})

test_that("check_period returns comma-separated string when passed a vector of characters", {
  expect_equal(check_period(c('1999','2020')), "1999,2020")
})

test_that("check_period throws an error when no period is provided", {
  expect_error(check_period(NULL), "You need to provide at least one period reference.")
})

test_that("check_period throws an error when no period is provided", {
  expect_error(check_period('2019:2020:'), "Invalid period range")
})

test_that("check_period throws an error when invalid number or character letters are provided", {
  expect_warning(expect_error(check_period('2019:xxxx'), "Must provide numbers as input"))
})
