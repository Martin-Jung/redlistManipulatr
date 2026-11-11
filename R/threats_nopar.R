#' Fetch Red List threat data without parallel processing
#' @description Internal helper for the non-parallel path in \code{RL_threats()}.
#' @author Matt Lewis, \email{matthewlewis896@@gmail.com}
#' @keywords internal
#'
#' @param x Vector of species names or IUCN Red List IDs.
#' @param query Lookup type: \code{"name"} for species names or \code{"ID"} for IUCN Red List IDs.
#' @param key Character string. IUCN Red List API key. Defaults to `Sys.getenv("IUCN_REDLIST_KEY")`; set `IUCN_REDLIST_KEY` in your `.Renviron` file or pass a key explicitly.
#' @param sleep_dur (optional) Numeric. Duration of sleep between API calls in seconds. Defaults to 2 seconds.
#' @param subset Proportion of \code{x} to process, from 0 to 1.
#' @param verbose Progress reporting level: \code{0} for none, \code{1} for a progress bar, or \code{2} for one line per completed record.
#'
#' @return A long-format \code{data.frame} with one row per species-threat combination.

threats_nopar <-
  function(x,
           key = Sys.getenv("IUCN_REDLIST_KEY"),
           query,
           subset,
           sleep_dur,
           verbose) {


    if(verbose == 1){
      pb = utils::txtProgressBar(min = 0, max = length(subset), initial = 0, style = 3)
    }

    temp_df <- threatform()

    ret <- foreach::foreach (
      i = subset,
      .combine = rbind,
      .packages = c("rredlist")#packages needing maintaining in this loop
    ) %do% {
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

      if(verbose == 1){
        utils::setTxtProgressBar(pb,i)
      }else if(verbose ==2){
        print(paste("task", i, "is complete"))
      }
      df
    }
    df <- do.call("cbind", ret)
    return(df)
  }
