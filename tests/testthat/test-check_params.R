test_that("test that invalid commodity_code is detected", {
  testthat::expect_error(
    check_params(
      frequency = 'A',
      commodity_classification = 'HS',
      commodity_code = "123",
      flow_direction = 'import',
      partner = "276",
      reporter = "32",
      start_date = '2020',
      end_date = '2020'
    ),
    regexp = ': 123'
  )
})

test_that("test that invalid commodity_code in csv is detected and returned",
          {
            testthat::expect_error(
              check_params(
                frequency = 'A',
                commodity_classification = 'HS',
                commodity_code = c("blabla", "01"),
                flow_direction = 'import',
                partner = "276",
                reporter = "32",

                start_date = '2020',
                end_date = '2020'
              ),
              regexp = ': blabla'
            )
          })

test_that("test that at least one cmdcode is provided", {
  testthat::expect_error(
    check_params(
      frequency = 'A',
      commodity_classification = 'HS',
      commodity_code = NULL,
      flow_direction = 'import',
      partner = "276",
      reporter = "32",
      start_date = '2020',
      end_date = '2020'
    ),
    regexp = 'You need to provide at least one'
  )
})

test_that("test that invalid reporter is detected", {
  testthat::expect_error(
    check_params(
      frequency = 'A',
      commodity_classification = 'HS',
      commodity_code = "01",
      flow_direction = 'import',
      partner = "DEU",
      reporter = "blabla",
      start_date = '2020',
      end_date = '2020'
      ),
    regexp = ': blabla'
  )
})

test_that("test that invalid reporter in csv is detected and returned", {
  testthat::expect_error(
    check_params(
      frequency = 'A',
      commodity_classification = 'HS',
      commodity_code = "01",
      flow_direction = 'import',
      partner = "276",
      reporter = c("DEU", "blabla"),
      start_date = '2020',
      end_date = '2020'    ),
    regexp = ': blabla'
  )
})

test_that("test that at least one reporter is provided", {
  testthat::expect_error(
    check_params(
      frequency = 'A',
      commodity_classification = 'HS',
      commodity_code = '01',
      flow_direction = 'import',
      partner = '4',
      reporter = NULL,
      start_date = '2020',
      end_date = '2020'    ),
    regexp = 'You need to provide at least one'
  )
})

test_that("test that invalid partner is detected", {
  testthat::expect_error(
    check_params(
      frequency = 'A',
      commodity_classification = 'HS',
      commodity_code = "01",
      flow_direction = 'import',
      partner = "276",
      reporter = "DEU",
      start_date = '2020',
      end_date = '2020'    ),
    regexp = ': 276'
  )
})

test_that("test that invalid partner in csv is detected and returned", {
  testthat::expect_error(
    check_params(
      frequency = 'A',
      commodity_classification = 'HS',
      commodity_code = "01",
      flow_direction = 'import',
      partner = c('world', 'test'),
      reporter = "DEU",
      start_date = '2020',
      end_date = '2020'    ),
    regexp = ': test'
  )
})

test_that("test that at least one partner is provided", {
  testthat::expect_error(
    check_params(
      frequency = 'A',
      commodity_classification = 'HS',
      commodity_code = '01',
      flow_direction = 'import',
      partner = NULL,
      reporter = 'DEU',
      start_date = '2020',
      end_date = '2020'    ),
    regexp = 'You need to provide at least one'
  )
})

test_that("test that wrong flowcode is found", {
  testthat::expect_error(
    check_params(
      frequency = 'A',
      commodity_classification = 'HS',
      commodity_code = '01',
      flow_direction = c('M', 'import'),
      partner = 'AUS',
      reporter = 'DEU',
      start_date = '2020',
      end_date = '2020'    ),
    regexp = 'not "M"'
  )
})

test_that("test that wrong flowcode is found", {
  testthat::expect_error(
    check_params(
      frequency = 'A',
      commodity_classification = 'HS',
      commodity_code = '01',
      flow_direction = c('M', 'import'),
      partner = 'AUS',
      reporter = 'DEU',
      start_date = '2020',
      end_date = '2020'    ),
    regexp = 'not "M"'
  )
})

test_that("test that flowcode cannot be null", {
  testthat::expect_error(
    check_params(
      frequency = 'A',
      commodity_classification = 'HS',
      commodity_code = '01',
      flow_direction = NULL,
      partner = 'AUS',
      reporter = 'DEU',
      start_date = '2020',
      end_date = '2020'    ),
    regexp = ', not `NULL`'
  )
})

test_that("check_params returns a list of parameters if all inputs are valid",
          {
            params <-
              check_params(
                commodity_code = "01",
                flow_direction = "import",
                reporter = "USA",
                partner = "CAN",
                start_date = '2020',
                end_date = '2020'              )
            expect_named(params, c("query_params", "url_params"))
            expect_named(params[[2]], c("freq", "clCode"))
            expect_named(
              params[[1]],
              c(
                "cmdCode",
                "flowCode",
                "partnerCode",
                "reporterCode",
                'period',
                'motCode',
                'partner2Code',
                'customsCode'
              )
            )
            expect_equal(params[[1]]$cmdCode, "01")
            expect_equal(params[[1]]$flowCode, "M")
            expect_equal(params[[1]]$reporterCode, "842,841")
            expect_equal(params[[1]]$partnerCode, "124")
            expect_equal(params[[1]]$period, "2020")
          })

test_that("check_params() creates proper ids for reporter 'all'", {
  params <-
    check_params(
      commodity_code = "01",
      flow_direction = "import",
      partner = "CAN",
      start_date = '2020',
      end_date = '2020',
      reporter = 'all'
    )
  expect_equal(params[[1]]$reporter,
               paste0(untrader::REPORTER$id[!untrader::REPORTER$isGroup], collapse = ','))
})

test_that("check_params() creates proper ids for reporter 'USA'", {
  params <-
    check_params(
      commodity_code = "01",
      flow_direction = "import",
      partner = "CAN",
      start_date = '2020',
      end_date = '2020',
      reporter = 'USA'
    )
  expect_equal(params[[1]]$reporter,
               untrader::REPORTER$id[untrader::REPORTER$reporterCodeIsoAlpha3 == 'USA' &
                                       !untrader::REPORTER$isGroup] |> paste0(collapse = ','))
})

test_that("check_params() creates proper ids for partner 'all'", {
  params <-
    check_params(
      commodity_code = "01",
      flow_direction = "import",
      partner = "all",
      start_date = '2020',
      end_date = '2020',
      reporter = 'USA'
    )
  expect_equal(params[[1]]$partnerCode,
               paste0(untrader::PARTNER$id[!untrader::PARTNER$isGroup], collapse = ','))
})

test_that("check_params() creates proper ids for partner 'world'", {
  params <-
    check_params(
      commodity_code = "01",
      flow_direction = "import",
      partner = "world",
      start_date = '2020',
      end_date = '2020',
      reporter = 'USA'
    )
  expect_equal(params[[1]]$partnerCode, '0')
})

test_that("check_params() creates proper ids for partner 'USA'", {
  params <-
    check_params(
      commodity_code = "01",
      flow_direction = "import",
      partner = "USA",
      start_date = '2020',
      end_date = '2020',
      reporter = 'USA'
    )
  expect_equal(params[[1]]$partnerCode,
               untrader::PARTNER$id[untrader::PARTNER$PartnerCodeIsoAlpha3 == 'USA' &
                                      !untrader::PARTNER$isGroup] |> paste0(collapse = ','))
})

test_that("check_params() creates proper ids for multiple partnerCodes", {
  params <- check_params(
    commodity_code = "01",
    flow_direction = "import",
    partner = c("USA", "CAN"),
    start_date = '2020',
    end_date = '2020',
    reporter = 'USA'
  )
  expect_equal(params[[1]]$partnerCode,
               paste0(untrader::PARTNER$id[untrader::PARTNER$PartnerCodeIsoAlpha3 %in% c('USA', 'CAN') &
                                             !untrader::PARTNER$isGroup], collapse = ','))
})


test_that("check_params() all can only be provided as single argument partnerCodes",
          {
            expect_error(
              check_params(
                commodity_code = "01",
                flow_direction = "import",
                partner = c("all", "CAN"),
                start_date = '2020',
                end_date = '2020',
                reporter = 'USA'
              ),
              regexp = '"all" can only be provided as a single argument'
            )
          })

test_that("check_params() all can only be provided as single argument reporterCodes",
          {
            expect_error(
              check_params(
                commodity_code = "01",
                flow_direction = "import",
                reporter = c("all", "CAN"),
                start_date = '2020',
                end_date = '2020',
                partner = 'USA'
              ),
              regexp = '"all" can only be provided as a single argument'
            )
          })


test_that("check_params() creates proper ids for multiple flowCodes", {
  params <-
    check_params(
      commodity_code = "01",
      flow_direction = c('re-import', 'import'),
      partner = "USA",
      start_date = '2020',
      end_date = '2020',
      reporter = 'USA'
    )
  expect_equal(params[[1]]$flowCode, 'RM,M')
})

test_that("check_params() creates error for multiple values with 'all'", {
  expect_error(
    check_params(
      commodity_code = "01",
      flow_direction = c('all', 'import'),
      partner = "USA",
      start_date = '2020',
      end_date = '2020',
      reporter = 'USA'
    )
  )
})

test_that("check_params() creates proper ids for flow_direction 'all'", {
  params <-
    check_params(
      commodity_code = "01",
      flow_direction = c('all'),
      partner = "USA",
      start_date = '2020',
      end_date = '2020',
      reporter = 'USA'
    )
  expect_equal(params[[1]]$flowCode, 'M,X,RM,RX')
})
#
# test_that("check_period returns comma-separated string when passed a vector of integers",
#           {
#             expect_equal(check_period(1999:2002), "1999,2000,2001,2002")
#           })
#
# test_that("check_period returns comma-separated string when passed a vector of characters",
#           {
#             expect_equal(check_period(c('1999', '2020')), "1999,2020")
#           })
#
# test_that("check_period throws an error when no period is provided", {
#   expect_error(check_period(NULL),
#                "You need to provide at least one period reference.")
# })
#
# test_that("check_period throws an error when no period is provided", {
#   expect_error(check_period('2019:2020:'), "Invalid period range")
# })
#
# test_that("check_period throws an error when invalid number or character letters are provided",
#           {
#             expect_warning(expect_error(check_period('2019:xxxx'), "Must provide numbers as input"))
#           })



### these tests were taken from https://github.com/ropensci/comtradr/blob/master/tests/testthat/test-ct_search.R

test_that("throw error with invalid input to arg 'start_date' & 'end_date'", {
  expect_error(
    get_comtrade_data(
      reporter = "CAN",
      partner = "DEU",
      commodity_code = "TOTAL",
      flow_direction = "import",
      trade_frequency = "monthly",
      start_date = "1/1/2011",
      end_date = "5/1/2011"
    ),
    regexp = "arg must be a date with one of these formats:"
  )
})


test_that("different date inputs produce correct date ranges", {
  # Tests with "freq" is "annual".
  expect_equal(check_date("2016", "2016", "A"), "2016")
  expect_equal(check_date("2016-01-01", 2016, "A"), "2016")
  expect_equal(check_date("2013", "2016", "A"),
               "2013,2014,2015,2016")
  expect_equal(check_date(2010, 2012, "A"), "2010,2011,2012")

  # Tests with "freq" as "monthly".
  expect_equal(
    check_date("2016-01-01", "2016-05", "M"),
    "201601,201602,201603,201604,201605"
  )
  expect_equal(check_date(2016, 2016, "M"), "2016")
  expect_error(check_date("2013", "2016", "M"),
               regexp = "Cannot get more than a single year's worth")
  # expect_error(get_date_range(2015, "2015-03", "M"),
  #              rexexp = "'start_date' and 'end_date' must have the same format")
})
