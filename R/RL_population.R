#' Fetch Red List population information
#' @description Retrieve structured population entries from full Red List assessment records.
#' @param ids Numeric vector of IUCN assessment IDs, as used by `rredlist::rl_assessment_list()`.
#' @param key Character string. IUCN Red List API key. Defaults to `Sys.getenv("IUCN_REDLIST_KEY")`; set `IUCN_REDLIST_KEY` in your `.Renviron` file or pass a key explicitly.
#' @param include.documentation Logical. Include the population narrative from each assessment's documentation. Defaults to `FALSE`.
#' @param wait_time Numeric. Seconds to wait between assessment requests. Defaults to 0.5, as recommended by `rredlist`.
#' @param quiet Logical. Suppress the progress bar from `rredlist::rl_assessment_list()`. Defaults to `FALSE`.
#' @return A dataframe with one row per assessment and columns for the requested population fields. Fields absent from an assessment are `NA`. Numeric population fields retain their original value and also have `_min` and `_max` columns; ranges such as `"100-200"` are split into those columns. If `include.documentation = TRUE`, a `documentation_population` column contains the population narrative.
#' @details Population fields returned are current population trend, number of mature individuals, population severely fragmented, continuing decline of mature individuals, extreme fluctuations, number of subpopulations, continuing decline in subpopulations, extreme fluctuations in subpopulations, all individuals in one subpopulation, number of individuals in the largest subpopulation, and the percent change, reduction or increase, basis, and causes of reduction for past, future, and ongoing periods.
#' @export
RL_population <- function(ids,
                          key = Sys.getenv("IUCN_REDLIST_KEY"),
                          include.documentation = FALSE,
                          wait_time = 0.5,
                          quiet = FALSE) {
  packTest("rredlist")

  if (!is.character(key) || length(key) != 1L || is.na(key) || !nzchar(key)) {
    stop("Please supply a valid API key or set IUCN_REDLIST_KEY in your .Renviron file.")
  }
  if (!is.numeric(ids) || length(ids) == 0L || anyNA(ids) ||
      any(!is.finite(ids)) || any(ids <= 0) || any(ids %% 1 != 0) ||
      any(ids > .Machine$integer.max)) {
    stop("Please supply one or more positive integer assessment IDs in 'ids'.")
  }
  if (!is.logical(include.documentation) || length(include.documentation) != 1L ||
      is.na(include.documentation)) {
    stop("Please supply a single TRUE or FALSE value for 'include.documentation'.")
  }
  if (!is.numeric(wait_time) || length(wait_time) != 1L || is.na(wait_time) ||
      !is.finite(wait_time) || wait_time < 0) {
    stop("Please supply a non-negative numeric value for 'wait_time'.")
  }
  if (!is.logical(quiet) || length(quiet) != 1L || is.na(quiet)) {
    stop("Please supply a single TRUE or FALSE value for 'quiet'.")
  }

  ids <- as.integer(ids)
  assessments <- rredlist::rl_assessment_list(
    ids = ids,
    key = key,
    wait_time = wait_time,
    quiet = quiet
  )

  .population_assessments_to_df(
    assessments = assessments,
    ids = ids,
    include.documentation = include.documentation
  )
}

.population_assessments_to_df <- function(assessments, ids, include.documentation) {
  if (is.null(assessments)) {
    assessments <- list()
  }
  if (!is.list(assessments)) {
    stop("The Red List API returned assessments in an unexpected format.")
  }

  if (length(assessments) == 0L) {
    assessments <- rep(list(NULL), length(ids))
  }

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

  rows <- lapply(seq_along(ids), function(i) {
    assessment <- if (i <= length(assessments)) assessments[[i]] else NULL
    row <- as.list(rep(NA_character_, length(population_fields)))
    names(row) <- population_fields
    row$assessment_id <- as.numeric(ids[[i]])
    for (field in range_fields) {
      row[[paste0(field, "_min")]] <- NA_real_
      row[[paste0(field, "_max")]] <- NA_real_
    }

    if (is.list(assessment)) {
      if (!is.null(assessment$assessment_id) && length(assessment$assessment_id) > 0L) {
        row$assessment_id <- as.numeric(assessment$assessment_id[[1L]])
      }
      entries <- .population_flatten_entries(assessment$supplementary_info)
      for (entry in entries) {
        field <- .population_target_field(entry$field)
        if (!is.null(field) && is.na(row[[field]]) && !is.na(entry$value)) {
          row[[field]] <- entry$value
        }
      }

      trend <- .population_trend_value(assessment$population_trend)
      if (!is.null(trend) && length(trend) == 1L &&
          !is.na(trend) && nzchar(trend)) {
        row$current_population_trend <- trend
      }

      for (field in range_fields) {
        if (!is.na(row[[field]])) {
          value_range <- .population_parse_range(row[[field]])
          row[[paste0(field, "_min")]] <- value_range[[1L]]
          row[[paste0(field, "_max")]] <- value_range[[2L]]
        }
      }

      if (include.documentation && !is.null(assessment$documentation$population)) {
        narrative <- as.character(unlist(
          assessment$documentation$population,
          use.names = FALSE
        ))
        narrative <- narrative[!is.na(narrative)]
        if (length(narrative) > 0L) {
          row$documentation_population <- paste(narrative, collapse = "\n")
        } else {
          row$documentation_population <- NA_character_
        }
      }
    } else if (include.documentation) {
      row$documentation_population <- NA_character_
    }

    row
  })

  result <- as.data.frame(do.call(rbind, lapply(rows, as.data.frame)),
                          stringsAsFactors = FALSE, check.names = FALSE)
  result <- result[, c(
    "assessment_id",
    unlist(lapply(population_fields, function(field) {
      c(field, if (field %in% range_fields) {
        c(paste0(field, "_min"), paste0(field, "_max"))
      })
    }), use.names = FALSE),
    if (include.documentation) "documentation_population"
  ), drop = FALSE]
  rownames(result) <- NULL
  result
}

.population_target_field <- function(field) {
  aliases <- list(
    current_population_trend = c("current_population_trend", "population_trend"),
    number_of_mature_individuals = c(
      "number_of_mature_individuals", "number_mature_individuals",
      "mature_individuals", "population_size"
    ),
    population_severely_fragmented = c("population_severely_fragmented"),
    continuing_decline_of_mature_individuals = c("continuing_decline_of_mature_individuals"),
    extreme_fluctuations = c("extreme_fluctuations"),
    no_of_subpopulations = c("no_of_subpopulations", "number_of_subpopulations"),
    continuing_decline_in_subpopulations = c("continuing_decline_in_subpopulations"),
    extreme_fluctuations_in_subpopulations = c("extreme_fluctuations_in_subpopulations"),
    all_individuals_in_one_subpopulation = c("all_individuals_in_one_subpopulation"),
    no_of_individuals_in_largest_subpopulation = c(
      "no_of_individuals_in_largest_subpopulation",
      "number_of_individuals_in_largest_subpopulation"
    ),
    percent_change_in_past = c("percent_change_in_past", "past_percent_change"),
    reduction_or_increase_in_past = c("reduction_or_increase_in_past", "past_reduction_or_increase"),
    past_population_reduction_basis = c("past_population_reduction_basis"),
    past_causes_of_reduction = c("past_causes_of_reduction", "causes_of_reduction_in_past"),
    percent_change_in_future = c("percent_change_in_future", "future_percent_change"),
    reduction_or_increase_in_future = c("reduction_or_increase_in_future", "future_reduction_or_increase"),
    future_population_reduction_basis = c("future_population_reduction_basis"),
    future_causes_of_reduction = c("future_causes_of_reduction", "causes_of_reduction_in_future"),
    percent_change_in_ongoing = c("percent_change_in_ongoing", "ongoing_percent_change"),
    reduction_or_increase_in_ongoing = c("reduction_or_increase_in_ongoing", "ongoing_reduction_or_increase"),
    ongoing_population_reduction_basis = c("ongoing_population_reduction_basis"),
    ongoing_causes_of_reduction = c("ongoing_causes_of_reduction", "causes_of_reduction_ongoing")
  )

  normalize <- function(x) tolower(gsub("[^a-z0-9]+", "_", x))
  path_parts <- normalize(strsplit(gsub("\\[[0-9]+\\]", "", field), "\\.")[[1L]])
  match_part <- function(part) {
    for (target in names(aliases)) {
      if (part %in% normalize(aliases[[target]])) {
        return(target)
      }
    }
    NULL
  }

  target <- match_part(utils::tail(path_parts, 1L))
  if (!is.null(target)) {
    return(target)
  }
  if (length(path_parts) > 1L &&
      utils::tail(path_parts, 1L) %in% c("value", "amount", "estimate", "count", "number", "en")) {
    for (part in rev(utils::head(path_parts, -1L))) {
      target <- match_part(part)
      if (!is.null(target)) {
        return(target)
      }
    }
  }

  period <- intersect(path_parts, c("past", "future", "ongoing"))
  leaf <- utils::tail(path_parts, 1L)
  if (length(period) > 0L) {
    period <- period[[1L]]
    period_field <- switch(
      leaf,
      percent_change = paste0("percent_change_in_", period),
      reduction_or_increase = paste0("reduction_or_increase_in_", period),
      population_reduction_basis = paste0(period, "_population_reduction_basis"),
      causes_of_reduction = paste0(period, "_causes_of_reduction"),
      NULL
    )
    if (!is.null(period_field)) {
      return(period_field)
    }
  }
  NULL
}

.population_trend_value <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  if (!is.list(x)) {
    return(as.character(x[[1L]]))
  }

  description <- x$description
  if (is.list(description) && !is.null(description$en)) {
    return(as.character(description$en[[1L]]))
  }
  if (!is.null(description) && !is.list(description)) {
    return(as.character(description[[1L]]))
  }
  if (!is.null(x$code)) {
    return(as.character(x$code[[1L]]))
  }
  NULL
}

.population_flatten_entries <- function(x) {
  flatten <- function(value, prefix = "") {
    if (is.null(value)) {
      return(list())
    }

    if (is.data.frame(value)) {
      out <- list()
      if (nrow(value) == 0L || ncol(value) == 0L) {
        return(out)
      }
      for (i in seq_len(nrow(value))) {
        for (column in names(value)) {
          row_prefix <- paste0(prefix, "[", i, "]")
          field <- if (nzchar(row_prefix)) paste(row_prefix, column, sep = ".") else column
          out <- c(out, flatten(value[[column]][i], field))
        }
      }
      return(out)
    }

    if (is.list(value)) {
      out <- list()
      if (length(value) == 0L) {
        return(out)
      }
      value_names <- names(value)
      for (i in seq_along(value)) {
        child_name <- if (!is.null(value_names) && !is.na(value_names[[i]]) &&
                          nzchar(value_names[[i]])) {
          value_names[[i]]
        } else {
          paste0("entry_", i)
        }
        field <- if (nzchar(prefix)) paste(prefix, child_name, sep = ".") else child_name
        out <- c(out, flatten(value[[i]], field))
      }
      return(out)
    }

    if (length(value) == 0L) {
      return(list())
    }

    out <- vector("list", length(value))
    value_names <- names(value)
    for (i in seq_along(value)) {
      if (!is.null(value_names) && !is.na(value_names[[i]]) && nzchar(value_names[[i]])) {
        field <- if (nzchar(prefix)) paste(prefix, value_names[[i]], sep = ".") else value_names[[i]]
      } else if (length(value) > 1L) {
        field <- if (nzchar(prefix)) paste0(prefix, "[", i, "]") else paste0("entry_", i)
      } else {
        field <- if (nzchar(prefix)) prefix else "value"
      }
      item <- value[[i]]
      out[[i]] <- list(
        field = field,
        value = if (length(item) == 0L || is.na(item)) NA_character_ else as.character(item)
      )
    }
    out
  }

  flatten(x)
}

.population_parse_range <- function(value) {
  missing <- c(NA_real_, NA_real_)
  if (length(value) != 1L || is.na(value) || !nzchar(trimws(value))) {
    return(missing)
  }

  value <- trimws(value)
  number <- "(?:[0-9]+|[0-9]{1,3}(?:,[0-9]{3})+)(?:\\.[0-9]+)?"
  range_pattern <- paste0("^(", number, ")\\s*[-\u2013\u2014]\\s*(", number, ")$")
  range_match <- regexec(range_pattern, value, perl = TRUE)
  range_parts <- regmatches(value, range_match)[[1L]]
  if (length(range_parts) == 3L) {
    endpoints <- gsub(",", "", range_parts[2:3], fixed = TRUE)
    return(as.numeric(endpoints))
  }

  if (grepl(paste0("^", number, "$"), value, perl = TRUE)) {
    scalar <- as.numeric(gsub(",", "", value, fixed = TRUE))
    return(c(scalar, scalar))
  }

  missing
}
