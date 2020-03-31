library(testthat)
library(httr)

baseUrl <- "https://test.immunespace.org/"
test_that("Default Params", {
  url <- paste0(baseUrl, "_rapi/log_data")
  print(url)
  res <- GET(url)
  status = http_status(res)
  expect_equal(status$category, "Success")
  expect_equal(http_type(res), "application/json")
  
  json <- content(res, flatten = TRUE)
  expect_equal(names(json), c("byStudy", "byMonth"))
  expect_equal(json$byMonth$`1`$Month[[1]], "2016-01")
  expect_equal(json$byMonth$`1`$ISR[[1]], 0)
  expect_equal(json$byMonth$`1`$UI[[1]], 200)
  expect_equal(json$byMonth$`1`$total[[1]], 200)
  
  expect_equal(json$byMonth[[length(json$byMonth)]]$Month[[1]], strftime(Sys.time(), "%Y-%m"))
  
  expect_gte(length(json$byStudy)[], 93)
  expect_equal(names(json$byStudy[[1]]), c("studyId", "ISR", "UI", "total"))
})

