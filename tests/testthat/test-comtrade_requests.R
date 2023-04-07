test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})




test_that("test that invalid cmdCode is detected", {
testthat::expect_error(check_params(freq = 'A',
           clCode = 'HS',
           cmdCode = "123",
           flowCode = 'M',
           partnerCode = "276",
           reporterCode = "32",
           period = "1999"),regexp = 'is invalid')
})

test_that("test that invalid cmdCode in csv is detected and returned", {
testthat::expect_error(check_params(freq = 'A',
           clCode = 'HS',
           cmdCode = "blabla, 01",
           flowCode = 'M',
           partnerCode = "276",
           reporterCode = "32",
           period = "1999"),regexp = ': blabla')
})

test_that("test that at least one cmdcode is provided", {
testthat::expect_error(check_params(freq = 'A',
           clCode = 'HS',
           cmdCode = NULL,
           flowCode = 'M',
           partnerCode = "276",
           reporterCode = "32",
           period = "1999"),regexp = 'You need to provide at least one')
})

test_that("test that invalid reporterCode is detected", {
testthat::expect_error(check_params(freq = 'A',
           clCode = 'HS',
           cmdCode = "01",
           flowCode = 'M',
           partnerCode = "276",
           reporterCode = "blabla",
           period = "1999"),regexp = 'is invalid')
})

test_that("test that invalid reporterCode in csv is detected and returned", {
testthat::expect_error(check_params(freq = 'A',
           clCode = 'HS',
           cmdCode = "01",
           flowCode = 'M',
           partnerCode = "276",
           reporterCode = "blabla,DEU",
           period = "1999"),regexp = ': blabla')
})

test_that("test that at least one reporterCode is provided", {
testthat::expect_error(check_params(freq = 'A',
           clCode = 'HS',
           cmdCode = '01',
           flowCode = 'M',
           partnerCode = '4',
           reporterCode = NULL,
           period = "1999"),regexp = 'You need to provide at least one')
})

test_that("test that invalid partnerCode is detected", {
testthat::expect_error(check_params(freq = 'A',
           clCode = 'HS',
           cmdCode = "01",
           flowCode = 'M',
           partnerCode = "276",
           reporterCode = "test",
           period = "1999"),regexp = 'is invalid')
})

test_that("test that invalid partnerCode in csv is detected and returned", {
testthat::expect_error(check_params(freq = 'A',
           clCode = 'HS',
           cmdCode = "01",
           flowCode = 'M',
           partnerCode = "world,test",
           reporterCode = "DEU",
           period = "1999"),regexp = ': test')
})

test_that("test that at least one partnerCode is provided", {
testthat::expect_error(check_params(freq = 'A',
           clCode = 'HS',
           cmdCode = '01',
           flowCode = 'M',
           partnerCode = NULL,
           reporterCode = 'DEU',
           period = "1999"),regexp = 'You need to provide at least one')
})
