#' Fetch Red List threat data
#' @description Retrieve threat information for species from the IUCN Red List API.
#' @author Matt Lewis, \email{matthewlewis896@@gmail.com} and Martin Jung
#'
#' @param x A vector, list, or \code{data.frame} of species binomials or IUCN assessment IDs. For \code{query = "ID"}, use the \code{assessment_id} column returned by \code{RL_sp_list()}.
#' @param col.name Column name containing species names or IDs when \code{x} is a \code{data.frame}. Defaults to \code{"assessment_id"}. Not used for vectors or lists.
#' @param query Lookup type: \code{"name"} to search by scientific name or \code{"ID"} to retrieve a specific assessment by its assessment ID. Defaults to \code{"ID"} when \code{x} is a data frame and \code{col.name} is \code{"assessment_id"}; otherwise required.
#' @param key IUCN Red List API key. Defaults to \code{Sys.getenv("IUCN_REDLIST_KEY")}. Set \code{IUCN_REDLIST_KEY} in your \code{.Renviron} file or pass a key explicitly. API keys are available through the account page at \url{https://api.iucnredlist.org/}.
#' @param sleep_dur Number of seconds to wait between API calls. Defaults to 2.
#' @param subset Proportion of \code{x} to process, from 0 to 1. Defaults to 1 (all entries).
#' @param parallel Logical. If \code{TRUE}, use multiple cores. Defaults to \code{TRUE}.
#' @param num.cores Number of cores to use when \code{parallel = TRUE}. Defaults to the number of available cores minus one.
#' @param verbose Progress reporting level: \code{0} for none, \code{1} for a progress bar, or \code{2} for one line per completed record. Defaults to \code{1}.
#' @details A threat code of \code{999} indicates that no threat information was available for the species.
#' @return A long-format \code{data.frame} with one row per species-threat combination.
#' @export

RL_threats <-
  function(x,
           col.name = "assessment_id",
           key = Sys.getenv("IUCN_REDLIST_KEY"),
           query = NULL,
           subset = 1,
           sleep_dur = 2,
           parallel = TRUE,
           num.cores = parallel::detectCores() - 1,
           verbose = 1) {

    # Checking needed packages are installed.

    packTest("rredlist")

    # Testing if API key is valid.
    if(!is.character(key) || length(key) != 1L || is.na(key) || !nzchar(key)){
      stop("Please supply a valid API key or set IUCN_REDLIST_KEY in your .Renviron file.")
    }
    # Testing if sleep duration is valid
    if(is.numeric(sleep_dur) == F){
      stop("Please supply a valid sleep duration. This should be a numeric input.")
    }
    # Handles verbose option
    if(is.numeric(verbose) == FALSE){
      stop("Please supply a valid value for 'verbose'.")
    }else if(!(verbose %in% c(0:2))){
      stop("Please supply a valid value for 'verbose'.")
    }

    if(is.data.frame(x) == TRUE){
      if(!is.character(col.name) || length(col.name) != 1L || is.na(col.name) || !nzchar(col.name)){
        stop("Please supply a valid value for 'col.name'. 'col.name' should be a character input.")
      }
      if(!(col.name %in% names(x))){
        stop(sprintf("Column '%s' was not found in 'x'.", col.name))
      }
      if(is.null(query) && identical(col.name, "assessment_id")){
        query <- "ID"
      }
      x <- x[[col.name]]
    }
    if(is.list(x) == TRUE){
      x <- unlist(x)
    }
    #handles subset
    if(!is.numeric(subset)){
      stop("Please supply a valid value for 'subset'. 'subset' should be between 0 and 1.")
    }else if(subset <0 | subset > 1){
      stop("Please supply a valid value for 'subset'. 'subset' should be between 0 and 1.")
    }else{
      subset <- round(subset * length(x))
      message("\nFetching data for a subset of ", subset, "/", length(x), " (", round((100*subset/length(x)),2), "%) of the data.")
    }
    #handles query
    if(!is.character(query)){
      stop("Please supply a valid value for 'query'. 'query' should be between a character input.")
    }else if(!(query %in% c("name", "ID"))){
      stop("Please supply a valid value for 'query'. 'query' should be between a character input of 'name' or 'ID'.")
    }

    if(is.numeric(num.cores) == FALSE){
      stop("Please supply a valid number of cores as an integer number.")
    }else if(num.cores %% 1 != 0){
      stop("Please supply a valid number of cores as an integer number.")
    }else if(num.cores > parallel::detectCores()){
      stop("You have supplied a number of CPU cores greater than the number available.")
    }

    message("Using Red List version ", rredlist::rl_version(key = key))
    # Getting data (parallel or not)

    df <- list()
    batches <- c(seq(0, subset, 99), subset)
    for(a in 1:(length(batches) - 1)) {
      batch <- (batches[a] + 1):batches[a + 1]
      batch_to_run <- batch
      batch_to_run <- batch_to_run - diff(c(1, batch_to_run[1]))
      message("\nFetching ",length(batch),  " data records for batch number ",   a,  " of ", (length(batches) - 1))
      if (parallel == TRUE) {
        df[[a]] <-
          threats_par(
            x = x[batch],
            key = key,
            query = query,
            subset = batch_to_run,#subset,
            sleep_dur = sleep_dur,
            num.cores = num.cores,
            verbose = verbose
          )
      } else{
        df[[a]] <-
          threats_nopar(
            x = x[batch],
            key = key,
            query = query,
            subset = batch_to_run,#subset,
            sleep_dur = sleep_dur,
            verbose = verbose
          )
      }
    }
    df <-
      df |>
      do.call(what = "rbind", args = _) |>
      as.data.frame()

    return(df)
  }
