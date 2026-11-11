test_that("RL_population returns only the requested population fields", {
  assessments <- list(
    list(
      assessment_id = 101L,
      population_trend = list(
        code = "decreasing",
        description = list(en = "Decreasing")
      ),
      supplementary_info = list(
        population_size = "1,200-2,400",
        population_size_qualifier = "estimated",
        population_severely_fragmented = "No",
        continuing_decline_of_mature_individuals = "Yes",
        extreme_fluctuations = "No",
        no_of_subpopulations = "2-4",
        past = list(
          percent_change = "20-30",
          reduction_or_increase = "reduction",
          population_reduction_basis = "observed",
          causes_of_reduction = "habitat loss"
        ),
        irrelevant_entry = "omit this field"
      ),
      documentation = list(population = "The population is estimated at 1,200 to 2,400 mature individuals.")
    ),
    list(
      assessment_id = 202L,
      population_trend = list(code = "stable", description = list(en = "Stable")),
      supplementary_info = list(population_size = 125),
      documentation = list(population = "About 125 mature individuals remain.")
    )
  )
  testthat::local_mocked_bindings(
    rl_assessment_list = function(ids, key, wait_time, quiet) assessments,
    .package = "rredlist"
  )

  result <- RL_population(
    ids = c(101, 202),
    key = "test-key",
    include.documentation = TRUE,
    quiet = TRUE
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2L)
  population_fields <- c(
    "current_population_trend",
    "number_of_mature_individuals",
    "population_severely_fragmented",
    "continuing_decline_of_mature_individuals",
    "extreme_fluctuations",
    "no_of_subpopulations",
    "continuing_decline_in_subpopulations",
    "extreme_fluctuations_in_subpopulations",
    "all_individuals_in_one_subpopulation",
    "no_of_individuals_in_largest_subpopulation",
    "percent_change_in_past",
    "reduction_or_increase_in_past",
    "past_population_reduction_basis",
    "past_causes_of_reduction",
    "percent_change_in_future",
    "reduction_or_increase_in_future",
    "future_population_reduction_basis",
    "future_causes_of_reduction",
    "percent_change_in_ongoing",
    "reduction_or_increase_in_ongoing",
    "ongoing_population_reduction_basis",
    "ongoing_causes_of_reduction"
  )
  range_fields <- c(
    "number_of_mature_individuals",
    "no_of_subpopulations",
    "no_of_individuals_in_largest_subpopulation",
    "percent_change_in_past",
    "percent_change_in_future",
    "percent_change_in_ongoing"
  )
  expected_names <- c(
    "assessment_id",
    unlist(lapply(population_fields, function(field) {
      c(field, if (field %in% range_fields) c(paste0(field, "_min"), paste0(field, "_max")))
    }), use.names = FALSE),
    "documentation_population"
  )
  expect_named(result, expected_names)
  expect_false(any(c("population_field", "population_size", "irrelevant_entry") %in% names(result)))
  expect_equal(result$current_population_trend, c("Decreasing", "Stable"))
  expect_equal(result$number_of_mature_individuals, c("1,200-2,400", "125"))
  expect_equal(result$number_of_mature_individuals_min, c(1200, 125))
  expect_equal(result$number_of_mature_individuals_max, c(2400, 125))
  expect_equal(result$no_of_subpopulations[1], "2-4")
  expect_equal(result$no_of_subpopulations_min[1], 2)
  expect_equal(result$no_of_subpopulations_max[1], 4)
  expect_equal(result$percent_change_in_past[1], "20-30")
  expect_equal(result$percent_change_in_past_min[1], 20)
  expect_equal(result$percent_change_in_past_max[1], 30)
  expect_equal(result$past_causes_of_reduction[1], "habitat loss")
  expect_true(all(!is.na(result$documentation_population)))
})

test_that("RL_population preserves assessments without supplementary information", {
  assessments <- list(list(
    assessment_id = 303L,
    supplementary_info = NULL,
    documentation = list(population = "No population size estimate is available.")
  ))
  testthat::local_mocked_bindings(
    rl_assessment_list = function(ids, key, wait_time, quiet) assessments,
    .package = "rredlist"
  )

  result <- RL_population(
    ids = 303,
    key = "test-key",
    include.documentation = TRUE,
    quiet = TRUE
  )

  expect_equal(nrow(result), 1L)
  expect_equal(result$assessment_id, 303)
  expect_true(is.na(result$number_of_mature_individuals))
  expect_true(is.na(result$current_population_trend))
  expect_equal(result$documentation_population, "No population size estimate is available.")
})

test_that("RL_population omits documentation unless requested", {
  assessments <- list(list(
    assessment_id = 404L,
    supplementary_info = list(population_size = "30-50"),
    documentation = list(population = "Thirty to fifty individuals.")
  ))
  testthat::local_mocked_bindings(
    rl_assessment_list = function(ids, key, wait_time, quiet) assessments,
    .package = "rredlist"
  )

  result <- RL_population(ids = 404, key = "test-key", quiet = TRUE)

  expect_false("documentation_population" %in% names(result))
  expect_equal(result$number_of_mature_individuals_min, 30)
  expect_equal(result$number_of_mature_individuals_max, 50)
})
