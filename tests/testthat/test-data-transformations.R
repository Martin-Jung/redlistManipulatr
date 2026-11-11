test_that("elevation check replaces invalid ranges", {
  x <- data.frame(
    iucn_id = c(1, 2),
    binomial = c("Species A", "Species B"),
    min_alt = c(100, 50),
    max_alt = c(100, 200)
  )

  result <- RL_elevation_check(x)

  expect_equal(result$min_alt, c(-99999, 50))
  expect_equal(result$max_alt, c(99999, 200))
})

test_that("missing habitat suitability can be recoded", {
  x <- wide_test_data()
  x$iucn_1_1 <- "9991"

  result <- RL_recode_suitabilities(x)

  expect_equal(as.character(result$iucn_1_1), "31")
})

test_that("missing seasons can be recoded and combined", {
  x <- wide_test_data(2)
  x$iucn_id <- c(1, 1)
  x$season <- c(1, 999)
  x$binomial <- "Test species"
  x$iucn_1_1 <- c("21", "31")

  result <- RL_recode_seasons(x, combine.rows = TRUE)

  expect_equal(nrow(result), 1L)
  expect_equal(result$season, 1)
  expect_equal(as.character(result$iucn_1_1), "31")
})

test_that("code fill propagates Level 2 data to its Level 1 habitat", {
  x <- wide_test_data()
  x$iucn_2_1 <- "31"

  result <- RL_code_fill(
    x,
    level1.recode = TRUE,
    level2.recode = FALSE,
    parallel = FALSE,
    num.cores = 1,
    verbose = 0
  )

  expect_equal(as.character(result$iucn_2), "31")
})

test_that("acceptable habitat values can be retained and filtered", {
  x <- wide_test_data(2)
  x$iucn_1_1 <- c("31", "21")

  result <- RL_subset_acceptable(
    x,
    acceptable_suitabilities = 3,
    na.rm = TRUE
  )

  expect_equal(nrow(result), 1L)
  expect_equal(as.character(result$iucn_1_1), "31")
})

test_that("season subsetting selects requested species-season rows", {
  x <- wide_test_data(2)
  x$season <- c(1, 2)
  x$binomial <- "Test species"
  x$iucn_1_1 <- c("31", "21")
  seasons <- data.frame(species = "Test species", season = 2)

  result <- RL_subset_seasons(
    x,
    season_df = seasons,
    query = "name",
    retain.missing.sp = FALSE
  )

  expect_equal(nrow(result), 1L)
  expect_equal(result$season, 2)
  expect_equal(as.character(result$iucn_1_1), "21")
})

test_that("wide data can be reformatted to long data and back", {
  x <- wide_test_data()
  x$iucn_1_1 <- "31"

  long <- RL_reformat_long(x)
  wide <- RL_reformat_wide(long)

  expect_equal(nrow(long), 1L)
  expect_equal(long$level_2, "1.1")
  expect_equal(long$suitability, "3")
  expect_equal(long$major_importance, "1")
  expect_equal(as.character(wide$iucn_1_1), "31")
})

test_that("crosswalk can combine habitat values into one recoded column", {
  x <- wide_test_data()
  x$iucn_1_1 <- "31"
  x$iucn_1_2 <- "21"
  lookup <- data.frame(
    target = "forest",
    habitat = c("1.1", "1.2")
  )

  result <- RL_crosswalk(
    x,
    lut = lookup,
    new.code.colname = "target",
    old.code.colname = "habitat",
    recode = "one"
  )

  expect_true("forest" %in% names(result))
  expect_equal(as.character(result$forest), "1")
})
