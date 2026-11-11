test_that("RL_fetch returns wide Red List data", {
  skip_if_no_iucn_api_key()

  result <- RL_fetch(
    x = "Panthera leo",
    query = "name",
    parallel = FALSE,
    num.cores = 1,
    sleep_dur = 1,
    verbose = 0
  )

  expect_s3_class(result, "data.frame")
  expect_true(all(c("iucn_id", "season", "binomial") %in% names(result)))
  expect_true(nrow(result) >= 1L)
})

test_that("RL_threats returns long Red List threat data", {
  skip_if_no_iucn_api_key()

  result <- RL_threats(
    x = "Panthera leo",
    query = "name",
    parallel = FALSE,
    num.cores = 1,
    sleep_dur = 1,
    verbose = 0
  )

  expect_s3_class(result, "data.frame")
  expect_true(all(c("iucn_id", "binomial", "code") %in% names(result)))
  expect_true(nrow(result) >= 1L)
})
