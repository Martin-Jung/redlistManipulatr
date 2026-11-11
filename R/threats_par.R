#' Fetch Red List threat data with parallel processing
#' @description Internal helper for the parallel path in \code{RL_threats()}.
#' @author Matt Lewis, \email{matthewlewis896@@gmail.com}
#' @keywords internal
#'
#' @param x Vector of species names or IUCN Red List IDs.
#' @param query Lookup type: \code{"name"} for species names or \code{"ID"} for IUCN Red List IDs.
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
          cat(sprintf("task %d is complete\n", n))#print when each row is complete
      opts <- list(progress = progress)
    } else if (verbose == 1){
      pb <- utils::txtProgressBar(style = 3, max = length(subset))
      progress <- function(n) utils::setTxtProgressBar(pb, n)
      opts <- list(progress = progress)
    }else{
      opts <- NULL
    }

    invisible(
      lapply(
        c("parallel", "snow", "doSNOW"),
        packTest
      )
    )

    cl <-
      snow::makeCluster(
        num.cores,
        outfile = ""
      )
    doSNOW::registerDoSNOW(cl)
    on.exit(snow::stopCluster(cl))

    temp_df <- threatform()

    ret <- foreach::foreach (
      i = subset,
      .combine = rbind,
      .options.snow = opts,
      .packages = c("rredlist")#packages needing maintaining in this loop
    ) %dopar% {

      df <- temp_df
      #what species we after
      species <- as.character(x[i])
      #trying with an error phase if needed
      df <- tryCatch({
        if (query == "name") {
          habs <- rredlist::rl_threats(name = species, key = key)

        } else if (query == "ID") {
          habs <-
            rredlist::rl_threats(id = as.numeric(as.character(species)), key = key)
        }
        if(length(habs$result)>0){
          df<-df[1:nrow(habs$result),]
          df[,3:4] <- habs$result[,1:2]
          df <- df[!is.na(df[,3]),]
          for(j in 1:nrow(df)){
            code <-
              df[j,3] %>%
              strsplit("[.]") %>%
              unlist()
            for(k in 1:length(code)){
              df[j,k+4] <- paste(code[1:k], collapse = ".")
            }
          }
          df[,8:length(df)] <- habs$result[,3:length(habs$result)]
        }else{
          df<-df[1,]
          df$code <- "999"
        }
        if(length(habs$name) >0L){
          df[,2] <- unlist(habs$name)
        }

        if(query == "ID"){
          df[,1] <- species
        }

        df<- df
      },
      error = function(err) {
        df<-df[1,]
        df$code <- "999"

        if (query == "name") {
          df$binomial = species
        } else if (query == "ID") {
          df$iucn_id = species
        }
        df <- df
      })
      Sys.sleep(sample(stats::rnorm(100, sleep_dur, 0.1), 1))
      df
    }
    df <- do.call("cbind", ret)
    return(df)
  }
