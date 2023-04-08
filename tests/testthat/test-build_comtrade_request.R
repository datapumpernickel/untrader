testthat::test_that('test that build returns a httr2 request',{
  req <- check_params(commodity_code = "01", flow_direction = "import", reporter = "USA", partner = "CAN", start_date = '2020', end_date = '2021') |>
    build_comtrade_request(primary_token = 'test_token')
  expect_equal(class(req),'httr2_request')

  expect_true(stringr::str_detect(req$url,'&partnerCode=124'))
  expect_true(stringr::str_detect(req$url,'https://comtradeapi.un.org/data/v1/get/C/A/HS?'))
})

