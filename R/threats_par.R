#' Fetch Red List threat data with parallel processing
#' @description Internal helper for the parallel path in \code{RL_threats()}.
#' @author Matt Lewis, \email{matthewlewis896@@gmail.com} and Martin Jung
#' @keywords internal
#'
#' @param x Vector of species names or IUCN assessment IDs.
#' @param query Lookup type: \code{"name"} for species names or \code{"ID"} for IUCN assessment IDs.
#' @param key Character string. IUCN Red List API key. Defaults to `Sys.getenv("IUCN_REDLIST_KEY")`; set `IUCN_REDLIST_KEY` in your `.Renviron` file or pass a key explicitly.
#' @param sleep_dur (optional) Numeric. Duration of sleep between API calls in seconds. Defaults to 2 seconds.
#' @param subset Proportion of \code{x} to process, from 0 to 1.
#' @param num.cores Number of cores to use.
#' @param verbose Progress reporting level: \code{0} for none, \code{1} for a progress bar, or \code{2} for one line per completed record.
#'
#' @return A long-format \code{data.frame} with one row per species-threat combination.

threats_par <-
  function(x,
           key = Sys.getenv("IUCN_REDLIST_KEY"),
           query,
           subset,
           sleep_dur,
           num.cores,
           verbose) {

    if (verbose == 2){
      progress <-
        function(n)
          cat(sprintf("task %d is complete\n", n))
      opts <- list(progress = progress)
    } else if (verbose == 1){
      pb <- utils::txtProgressBar(style = 3, max = length(subset))
      progress <- function(n) utils::setTxtProgressBar(pb, n)
      opts <- list(progress = progress)
    } else {
      opts <- NULL
    }

    invisible(
      lapply(
        c("parallel", "snow", "doSNOW"),
        packTest
      )
    )

    cl <- snow::makeCluster(num.cores, outfile = "")
    doSNOW::registerDoSNOW(cl)
    on.exit(snow::stopCluster(cl))

    temp_df <- threatform()

    ret <- foreach::foreach (
      i = subset,
      .combine = rbind,
      .options.snow = opts,
      .packages = c("rredlist", "redlistManipulatr")
    ) %dopar% {
      species <- as.character(x[i])
      df <- tryCatch({
        assessment <- redlistManipulatr:::.rl_assessment_for_query(species, query, key)
        redlistManipulatr:::.rl_threats_from_assessment(assessment)
      }, error = function(err) {
        df <- temp_df[1L, , drop = FALSE]
        df$code <- "999"
        if (query == "name") {
          df$binomial <- species
        } else {
          df$iucn_id <- NA_real_
        }
        df
      })
      Sys.sleep(sample(stats::rnorm(100, sleep_dur, 0.1), 1))
      df
    }
    df <- do.call("cbind", ret)
    return(df)
  }
