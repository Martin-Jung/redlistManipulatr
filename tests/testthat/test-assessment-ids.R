test_that("RL_sp_list retains latest assessment IDs", {
  skip_if_no_iucn_api_key()

  testthat::local_mocked_bindings(
    rl_version = function(key) "test-version",
    rl_kingdom = function(kingdom = NULL, key = NULL, all = TRUE, ...) {
      if (is.null(kingdom)) {
        return(list(kingdom_names = "ANIMALIA"))
      }
      list(assessments = data.frame(
        sis_taxon_id = 123L,
        assessment_id = 456L,
        taxon_scientific_name = "Panthera leo",
        red_list_category_code = "LC",
        stringsAsFactors = FALSE
      ))
    },
    .package = "rredlist"
  )

  result <- RL_sp_list(key = "test-key", retain.taxonomic.info = "none")

  expect_equal(result$taxonid, 123L)
  expect_equal(result$assessment_id, 456L)
  expect_equal(result$binomial, "Panthera leo")
})

test_that("assessment ID queries fetch and format threats from that assessment", {
  assessment <- list(
    assessment_id = 456L,
    sis_taxon_id = 123L,
    taxon = list(scientific_name = "Panthera leo"),
    threats = data.frame(
      code = "5_1_1",
      description.en = "Intentional use",
      timing = "Ongoing",
      scope = "Minority",
      severity = "Slow, significant declines",
      score = "High Impact: 8",
      ias = NA_character_,
      stringsAsFactors = FALSE
    )
  )
  testthat::local_mocked_bindings(
    rl_assessment = function(id, key) {
      expect_equal(id, 456)
      assessment
    },
    .package = "rredlist"
  )

  result <- threats_nopar(
    x = 456,
    key = "test-key",
    query = "ID",
    subset = 1,
    sleep_dur = 1,
    verbose = 0
  )

  expect_equal(result$iucn_id, 123)
  expect_equal(result$binomial, "Panthera leo")
  expect_equal(result$code, "5_1_1")
  expect_equal(result$title, "Intentional use")
  expect_equal(result$level1_code, "5")
  expect_equal(result$level2_code, "1")
  expect_equal(result$level3_code, "1")
})

test_that("name queries use the latest species assessment lookup", {
  testthat::local_mocked_bindings(
    rl_species_latest = function(genus, species, infra = NULL, key) {
      expect_equal(genus, "Panthera")
      expect_equal(species, "leo")
      expect_null(infra)
      expect_equal(key, "test-key")
      list(assessment_id = 456L)
    },
    .package = "rredlist"
  )

  result <- .rl_assessment_for_query("Panthera leo", "name", "test-key")

  expect_equal(result$assessment_id, 456L)
})