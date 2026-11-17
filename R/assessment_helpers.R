#' Resolve an input to an IUCN assessment
#' @description Retrieve an assessment by ID or resolve a scientific name to its latest assessment.
#' @param value Assessment ID when `query = "ID"`; species scientific name when `query = "name"`.
#' @param query Lookup mode: `"ID"` or `"name"`.
#' @param key IUCN Red List API key.
#' @return A parsed assessment list returned by `rredlist`.
#' @author Martin Jung
#' @name rl_assessment_for_query
#' @aliases .rl_assessment_for_query
#' @keywords internal
.rl_assessment_for_query <- function(value, query, key) {
  if (query == "ID") {
    return(rredlist::rl_assessment(
      id = as.numeric(as.character(value)),
      key = key
    ))
  }

  parts <- strsplit(trimws(as.character(value)), "\\s+")[[1L]]
  if (length(parts) < 2L) {
    stop("Species names must include at least a genus and species epithet.")
  }

  rredlist::rl_species_latest(
    genus = parts[[1L]],
    species = parts[[2L]],
    infra = if (length(parts) > 2L) paste(parts[-c(1L, 2L)], collapse = " ") else NULL,
    key = key
  )
}

#' Extract the taxon ID from an assessment
#' @description Return the assessment's taxon ID, falling back to the nested taxon SIS ID.
#' @param assessment Parsed IUCN assessment.
#' @return Numeric taxon ID, or `NA_real_` if unavailable.
#' @author Martin Jung
#' @name rl_assessment_taxon_id
#' @aliases .rl_assessment_taxon_id
#' @keywords internal
.rl_assessment_taxon_id <- function(assessment) {
  id <- assessment$sis_taxon_id
  if (is.null(id) || length(id) == 0L) {
    id <- assessment$taxon$sis_id
  }
  if (is.null(id) || length(id) == 0L) {
    return(NA_real_)
  }
  as.numeric(id[[1L]])
}

#' Extract the scientific name from an assessment
#' @description Return the nested taxon's scientific name.
#' @param assessment Parsed IUCN assessment.
#' @return Scientific name as a character string, or `NA_character_` if unavailable.
#' @author Martin Jung
#' @name rl_assessment_taxon_name
#' @aliases .rl_assessment_taxon_name
#' @keywords internal
.rl_assessment_taxon_name <- function(assessment) {
  name <- assessment$taxon$scientific_name
  if (is.null(name) || length(name) == 0L) {
    return(NA_character_)
  }
  as.character(name[[1L]])
}

#' Select the first value from an assessment field
#' @description Return the first field value, or a default when the value is empty.
#' @param value Assessment field value.
#' @param default Value to return when `value` is `NULL` or empty.
#' @return The first value, or `default` when no value is available.
#' @author Martin Jung
#' @name rl_assessment_field_value
#' @aliases .rl_assessment_field_value
#' @keywords internal
.rl_assessment_field_value <- function(value, default = NA_character_) {
  if (is.null(value) || length(value) == 0L) {
    return(default)
  }
  value[[1L]]
}

#' Get a column from assessment data
#' @description Return the requested column or missing values when it is absent.
#' @param x Assessment data frame.
#' @param field Column name to retrieve.
#' @return The requested column, or a character vector of missing values.
#' @author Martin Jung
#' @name rl_assessment_column
#' @aliases .rl_assessment_column
#' @keywords internal
.rl_assessment_column <- function(x, field) {
  if (field %in% names(x)) {
    return(x[[field]])
  }
  rep(NA_character_, nrow(x))
}

#' Extract an English description from assessment data
#' @description Support both flattened and nested English description fields.
#' @param x Assessment data frame.
#' @param field Name of the description field.
#' @return English descriptions as a character vector, with missing values where unavailable.
#' @author Martin Jung
#' @name rl_assessment_description
#' @aliases .rl_assessment_description
#' @keywords internal
.rl_assessment_description <- function(x, field) {
  flat_field <- paste0(field, ".en")
  if (flat_field %in% names(x)) {
    return(as.character(x[[flat_field]]))
  }

  description <- x[[field]]
  if (is.data.frame(description) && "en" %in% names(description)) {
    return(as.character(description$en))
  }
  if (is.list(description)) {
    return(vapply(description, function(value) {
      if (is.list(value) && !is.null(value$en)) {
        as.character(value$en[[1L]])
      } else {
        NA_character_
      }
    }, character(1)))
  }
  if (is.null(description)) {
    return(rep(NA_character_, nrow(x)))
  }
  as.character(description)
}

#' Convert assessment threats to package output
#' @description Format an assessment's threats as the long data frame used by `RL_threats()`.
#' @param assessment Parsed IUCN assessment.
#' @return Long-format threat data frame; assessments without threat data receive code `999`.
#' @author Martin Jung
#' @name rl_threats_from_assessment
#' @aliases .rl_threats_from_assessment
#' @keywords internal
.rl_threats_from_assessment <- function(assessment) {
  threats <- assessment$threats
  if (is.null(threats)) {
    threats <- data.frame()
  } else if (!is.data.frame(threats)) {
    threats <- as.data.frame(threats, stringsAsFactors = FALSE)
  }

  if (nrow(threats) == 0L) {
    result <- threatform()[1L, , drop = FALSE]
    result$code <- "999"
  } else {
    result <- threatform()[rep(1L, nrow(threats)), , drop = FALSE]
    codes <- as.character(.rl_assessment_column(threats, "code"))
    result$code <- codes
    result$title <- .rl_assessment_description(threats, "description")

    code_parts <- strsplit(gsub("_", ".", codes, fixed = TRUE), ".", fixed = TRUE)
    for (row_index in seq_along(code_parts)) {
      parts <- code_parts[[row_index]]
      if (length(parts) >= 1L) result$level1_code[[row_index]] <- parts[[1L]]
      if (length(parts) >= 2L) result$level2_code[[row_index]] <- parts[[2L]]
      if (length(parts) >= 3L) result$level3_code[[row_index]] <- parts[[3L]]
    }

    result$timing <- .rl_assessment_column(threats, "timing")
    result$scope <- .rl_assessment_column(threats, "scope")
    result$severity <- .rl_assessment_column(threats, "severity")
    result$score <- .rl_assessment_column(threats, "score")
    result$invasive <- .rl_assessment_column(threats, "ias")
  }

  result$iucn_id <- .rl_assessment_taxon_id(assessment)
  result$binomial <- .rl_assessment_taxon_name(assessment)
  result
}