#' Obtain list of all species on Red List
#' @description Obtain a list and basic info about all species on the Red List. This function combines the current rredlist taxonomic summary endpoints.
#' @author Matt Lewis, \email{matthewlewis896@@gmail.com}

#' @param key Character string. IUCN Red List API key. Defaults to `Sys.getenv("IUCN_REDLIST_KEY")`; set `IUCN_REDLIST_KEY` in your `.Renviron` file or pass a key explicitly. Keys are available from the account page linked from https://api.iucnredlist.org/.
#' @param retain.taxonomic.info (optional) Character. Options to retain all taxonomic information available from the API ("all"), only Kingdom and Class when supplied ("some"), or none ("none"). The current API summary provides Kingdom but not the other taxonomic ranks. The species name is always retained. Defaults to "some".
#' @param retain.population (optional) Logical. Retain population information when supplied by the API. The current taxonomic summary endpoint does not provide this field. Defaults to FALSE.
#' @param retain.category (optional) Logical. Retain the assessment category? Defaults to TRUE.
#' @return A dataframe with one row per species containing the SIS taxon ID (column taxonid), scientific name (column binomial), Kingdom when available, and Red List category.
#' @export

RL_sp_list <-
  function(
    key = Sys.getenv("IUCN_REDLIST_KEY"),
    retain.taxonomic.info = "some",
    retain.population = FALSE,
    retain.category = TRUE
  ){
    packTest("rredlist")

    if(!is.character(key) || length(key) != 1L || is.na(key) || !nzchar(key)){
      stop("Please supply a valid API key or set IUCN_REDLIST_KEY in your .Renviron file.")
    }

    if(!is.character(retain.taxonomic.info)){
      stop("Please supply a valid character input for 'retain.taxonomic.info'.")
    }else if(!(retain.taxonomic.info) %in% c("all", "some", "none")){
      stop("Please supply a valid character input for 'retain.taxonomic.info'.")
    }
    if(!is.logical(retain.population)){
      stop("Please supply a valid logical input for 'retain.population'.")
    }
    if(!is.logical(retain.category)){
      stop("Please supply a valid logical input for 'retain.category'.")
    }

    message("Using Red List version ", rredlist::rl_version(key = key))

    kingdoms <- rredlist::rl_kingdom(
      key = key,
      all = FALSE
    )$kingdom_names
    kingdoms <- unlist(kingdoms, use.names = FALSE)

    kingdom_results <- lapply(kingdoms, function(kingdom) {
      result <- rredlist::rl_kingdom(
        kingdom = kingdom,
        key = key,
        all = TRUE,
        quiet = TRUE,
        latest = TRUE,
        scope_code = 1L
      )$assessments

      if (is.null(result) || nrow(result) == 0) {
        return(NULL)
      }

      result$kingdom <- kingdom
      result
    })

    kingdom_results <- Filter(Negate(is.null), kingdom_results)
    if (length(kingdom_results) == 0) {
      stop("The Red List API returned no kingdom assessments.")
    }

    assessments <- do.call(rbind, kingdom_results)
    required_columns <- c(
      "sis_taxon_id",
      "taxon_scientific_name",
      "red_list_category_code",
      "kingdom"
    )
    if (!all(required_columns %in% colnames(assessments))) {
      stop("The Red List API returned assessments in an unexpected format.")
    }

    species_df <- data.frame(
      taxonid = assessments$sis_taxon_id,
      kingdom = assessments$kingdom,
      binomial = assessments$taxon_scientific_name,
      category = assessments$red_list_category_code,
      stringsAsFactors = FALSE
    )

    if (retain.taxonomic.info == "some") {
      taxonomic.columns <- c("taxonid", "kingdom", "class", "binomial")
    } else if (retain.taxonomic.info == "none") {
      taxonomic.columns <- c("taxonid", "binomial")
    } else {
      taxonomic.columns <- c(
        "taxonid", "kingdom", "phylum", "class", "order", "family",
        "genus", "binomial", "infra_rank", "infra_name"
      )
    }

    retained.columns <- intersect(taxonomic.columns, colnames(species_df))
    if (retain.population) {
      if ("population" %in% colnames(assessments)) {
        species_df$population <- assessments$population
        retained.columns <- c(retained.columns, "population")
      } else {
        warning("Population information is not available in the Red List taxonomic summary.")
      }
    }
    if (retain.category) {
      retained.columns <- c(retained.columns, "category")
    }
    species_df <- species_df[, retained.columns, drop = FALSE]

    return(species_df)
  }
