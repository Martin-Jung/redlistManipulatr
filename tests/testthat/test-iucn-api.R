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
  expect_true(any(!is.na(result$iucn_id)))
  expect_true(any(result$binomial == "Panthera leo", na.rm = TRUE))
})

test_that("RL_fetch defaults to assessment IDs for data frames", {
  skip_if_no_iucn_api_key()

  sp <- RL_sp_list(limit = 1)
  result <- RL_fetch(
    x = sp,
    parallel = TRUE,
    num.cores = 1,
    sleep_dur = 1,
    verbose = 0
  )

  expect_true(any(result$binomial == sp$binomial[[1L]], na.rm = TRUE))
  expect_true(any(result$iucn_id == sp$taxonid[[1L]], na.rm = TRUE))
})

test_that("RL_threats returns long Red List threat data", {
  skip_if_no_iucn_api_key()

  sp <- RL_sp_list(limit = 1)
  result <- RL_threats(
    x = sp,
    parallel = TRUE,
    num.cores = 1,
    sleep_dur = 1,
    verbose = 0
  )

  expect_s3_class(result, "data.frame")
  expect_true(all(c("iucn_id", "binomial", "code") %in% names(result)))
  expect_true(nrow(result) >= 1L)
  expect_true(any(result$iucn_id == sp$taxonid[[1L]], na.rm = TRUE))
  expect_true(any(result$binomial == sp$binomial[[1L]], na.rm = TRUE))
  expect_true(any(!is.na(result$code)))
})
