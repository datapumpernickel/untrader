test_that("test that invalid cmdCode is detected", {
testthat::expect_error(check_params(freq = 'A',
           clCode = 'HS',
           cmdCode = "123",
           flowCode = 'import',
           partnerCode = "276",
           reporterCode = "32",
           period = "1999"),regexp = ': 123')
})

test_that("test that invalid cmdCode in csv is detected and returned", {
testthat::expect_error(check_params(freq = 'A',
           clCode = 'HS',
           cmdCode = c("blabla","01"),
           flowCode = 'import',
           partnerCode = "276",
           reporterCode = "32",
           period = "1999"),regexp = ': blabla')
})

test_that("test that at least one cmdcode is provided", {
testthat::expect_error(check_params(freq = 'A',
           clCode = 'HS',
           cmdCode = NULL,
           flowCode = 'import',
           partnerCode = "276",
           reporterCode = "32",
           period = "1999"),regexp = 'You need to provide at least one')
})

test_that("test that invalid reporterCode is detected", {
testthat::expect_error(check_params(freq = 'A',
           clCode = 'HS',
           cmdCode = "01",
           flowCode = 'import',
           partnerCode = "DEU",
           reporterCode = "blabla",
           period = "1999"),regexp = ': blabla')
})

test_that("test that invalid reporterCode in csv is detected and returned", {
testthat::expect_error(check_params(freq = 'A',
           clCode = 'HS',
           cmdCode = "01",
           flowCode = 'import',
           partnerCode = "276",
           reporterCode = c("DEU","blabla"),
           period = "1999"),regexp = ': blabla')
})

test_that("test that at least one reporterCode is provided", {
testthat::expect_error(check_params(freq = 'A',
           clCode = 'HS',
           cmdCode = '01',
           flowCode = 'import',
           partnerCode = '4',
           reporterCode = NULL,
           period = "1999"),regexp = 'You need to provide at least one')
})

test_that("test that invalid partnerCode is detected", {
testthat::expect_error(check_params(freq = 'A',
           clCode = 'HS',
           cmdCode = "01",
           flowCode = 'import',
           partnerCode = "276",
           reporterCode = "DEU",
           period = "1999"),regexp = ': 276')
})

test_that("test that invalid partnerCode in csv is detected and returned", {
testthat::expect_error(check_params(freq = 'A',
           clCode = 'HS',
           cmdCode = "01",
           flowCode = 'import',
           partnerCode = c('world','test'),
           reporterCode = "DEU",
           period = "1999"),regexp = ': test')
})

test_that("test that at least one partnerCode is provided", {
testthat::expect_error(check_params(freq = 'A',
           clCode = 'HS',
           cmdCode = '01',
           flowCode = 'import',
           partnerCode = NULL,
           reporterCode = 'DEU',
           period = "1999"),regexp = 'You need to provide at least one')
})

test_that("test that wrong flowcode is found", {
testthat::expect_error(check_params(freq = 'A',
           clCode = 'HS',
           cmdCode = '01',
           flowCode = c('M','import'),
           partnerCode = 'AUS',
           reporterCode = 'DEU',
           period = "1999"),regexp = 'not "M"')
})

test_that("test that wrong flowcode is found", {
testthat::expect_error(check_params(freq = 'A',
           clCode = 'HS',
           cmdCode = '01',
           flowCode = c('M','import'),
           partnerCode = 'AUS',
           reporterCode = 'DEU',
           period = "1999"),regexp = 'not "M"')
})

test_that("check_params returns a list of parameters if all inputs are valid", {
  params <- check_params(cmdCode = "01", flowCode = "import", reporterCode = "USA", partnerCode = "CAN", period = "2022")
  expect_named(params, c("query_params", "url_params"))
  expect_named(params[[2]], c("freq", "clCode"))
  expect_named(params[[1]], c("cmdCode", "flowCode", "partnerCode", "reporterCode",'period','motCode','partner2Code'))
  expect_equal(params[[1]]$cmdCode, "01")
  expect_equal(params[[1]]$flowCode, "M")
  expect_equal(params[[1]]$reporterCode, "842,841")
  expect_equal(params[[1]]$partnerCode, "124")
  expect_equal(params[[1]]$period, "2022")
})

test_that("check_params() creates proper ids for reporterCode 'all'", {
  params <- check_params(cmdCode = "01", flowCode = "import", partnerCode = "CAN", period = "2022",reporterCode = 'all')
  expect_equal(params[[1]]$reporterCode, paste0(untrader::REPORTER$id[!untrader::REPORTER$isGroup], collapse = ','))
})

test_that("check_params() creates proper ids for reporterCode 'USA'", {
  params <- check_params(cmdCode = "01", flowCode = "import", partnerCode = "CAN", period = "2022",reporterCode = 'USA')
  expect_equal(params[[1]]$reporterCode, untrader::REPORTER$id[untrader::REPORTER$reporterCodeIsoAlpha3 == 'USA' & !untrader::REPORTER$isGroup] |> paste0(collapse=','))
})

test_that("check_params() creates proper ids for partnerCode 'all'", {
  params <- check_params(cmdCode = "01", flowCode = "import", partnerCode = "all", period = "2022",reporterCode = 'USA')
  expect_equal(params[[1]]$partnerCode, paste0(untrader::PARTNER$id[!untrader::PARTNER$isGroup], collapse = ','))
})

test_that("check_params() creates proper ids for partnerCode 'world'", {
  params <- check_params(cmdCode = "01", flowCode = "import", partnerCode = "world", period = "2022",reporterCode = 'USA')
  expect_equal(params[[1]]$partnerCode, '0')
})

test_that("check_params() creates proper ids for partnerCode 'USA'", {
  params <- check_params(cmdCode = "01", flowCode = "import", partnerCode = "USA", period = "2022",reporterCode = 'USA')
  expect_equal(params[[1]]$partnerCode, untrader::PARTNER$id[untrader::PARTNER$PartnerCodeIsoAlpha3 == 'USA' & !untrader::PARTNER$isGroup] |> paste0(collapse=','))
})

test_that("check_params() creates proper ids for multiple partnerCodes", {
  params <- check_params(cmdCode = "01",
                         flowCode = "import",
                         partnerCode = c("USA","CAN"),
                         period = "2022",
                         reporterCode = 'USA')
  expect_equal(params[[1]]$partnerCode, paste0(untrader::PARTNER$id[untrader::PARTNER$PartnerCodeIsoAlpha3 %in% c('USA', 'CAN') & !untrader::PARTNER$isGroup], collapse = ','))
})


test_that("check_params() all can only be provided as single argument partnerCodes", {
  expect_error(check_params(cmdCode = "01", flowCode = "import", partnerCode = c("all","CAN"), period = "2022",reporterCode = 'USA'),regexp = '"all" can only be provided as a single argument')
})

test_that("check_params() all can only be provided as single argument reporterCodes", {
  expect_error(check_params(cmdCode = "01", flowCode = "import", reporterCode = c("all","CAN"), period = "2022",partnerCode = 'USA'),regexp = '"all" can only be provided as a single argument')
})


test_that("check_params() creates proper ids for multiple flowCodes", {
  params <- check_params(cmdCode = "01", flowCode = c('re-import','import'), partnerCode = "USA", period = "2022",reporterCode = 'USA')
  expect_equal(params[[1]]$flowCode, 'RM,M')
})

test_that("check_params() creates error for multiple values with 'all'", {
  expect_error(check_params(cmdCode = "01", flowCode = c('all','import'), partnerCode = "USA", period = "2022",reporterCode = 'USA'))
})

test_that("check_params() creates proper ids for flowCode 'all'", {
  params <- check_params(cmdCode = "01", flowCode = c('all'), partnerCode = "USA", period = "2022",reporterCode = 'USA')
  expect_equal(params[[1]]$flowCode, 'M,X,RM,RX')
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
