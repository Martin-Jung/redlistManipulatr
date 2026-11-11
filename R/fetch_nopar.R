#' Fetch Red List habitat and elevation data without parallel processing
#' @description Internal helper for the non-parallel path in \code{RL_fetch()}.
#' @author Matt Lewis, \email{matthewlewis896@@gmail.com}
#' @keywords internal
#'
#' @param x Vector of species names or IUCN Red List IDs.
#' @param query Lookup type: \code{"name"} for species names or \code{"ID"} for IUCN Red List IDs.
#' @param key IUCN Red List API key, normally supplied by \code{RL_fetch()}.
#' @param sleep_dur Number of seconds to wait between API calls.
#' @param subset Proportion of \code{x} to process, from 0 to 1.
#' @param verbose Progress reporting level: \code{0} for none, \code{1} for a progress bar, or \code{2} for one line per completed record.
#'
#' @return A wide-format \code{data.frame} with one row per species-season combination.

fetch_nopar <-
  function(x,
           key = Sys.getenv("IUCN_REDLIST_KEY"),
           query,
           subset,
           sleep_dur,
           verbose) {



    temp_df <- wideform()
    seasons <- redlistManipulatr::seasons
    suitability <- redlistManipulatr::suitability
    major_importance <- redlistManipulatr::major_importance
    cols_to_check <-
      hab_col_positions() %>%
      unlist() %>%
      sort()

    if(verbose == 1){
      pb = utils::txtProgressBar(min = 0, max = length(subset), initial = 0, style = 3)
    }

    ret <- foreach::foreach (
      i = subset,
      .combine = rbind,
      .packages = c("rredlist")#packages needing maintaining in this loop
    ) %do% {
      #Let's see how many of 5 seasons we can fill - output df in the form we want
      df <- temp_df
      #what species we after
      species <- as.character(x[i])
      #trying with an error phase if needed
      df <- tryCatch({
        if (query == "name") {
          habs <- rredlist::rl_habitats(name = species, key = key)

        } else if (query == "ID") {
          habs <-
            rredlist::rl_habitats(id = as.numeric(as.character(species)), key = key)
        }
        if(length(habs$result)>0){

          habs$result$season <- tolower(as.character(habs$result$season))
          habs$result$season[habs$result$season %in% c("unknown", "seasonal occurrence unknown")] <- "seasonal occurrence uncertain"

          habs$result$suitability <- tolower(as.character(habs$result$suitability))

          habs$result$season[is.na(habs$result$season)] = 999
          habs$result$suitability[is.na(habs$result$suitability)] = 999

          #reformatting season
          for(j in 1:length(habs$result$season)){
            season_code <-
              seasons$Code[tolower(seasons$Seasonality) == tolower(habs$result$season[j])]

            if(any(is.na(season_code))){
              next()
            }else if(length(season_code) == 0L){
              next()
            }else{
              habs$result$season[j] <- season_code
            }

          }

          #reformatting suitability
          for(j in 1:length(habs$result$suitability)){
            suit_code <-
              suitability$Code[tolower(suitability$Name) == tolower(habs$result$suitability[j])]

            if(any(is.na(suit_code))){
              next()
            }else if(length(suit_code) == 0L){
              next()
            }else{
              habs$result$suitability[j] <- suit_code
            }

          }

          # Add codes for major importance
          for (j in 1:nrow(habs$result)){
            mi_code <-
              major_importance$Code[tolower(major_importance$Major_Importance) == tolower(habs$result$majorimportance[j])][1] # [1] deals with NA
            if(is.na(mi_code)){
              habs$result$majorimportance[j] <- major_importance$Code[is.na(major_importance$Major_Importance)]
            }else{
              habs$result$majorimportance[j] <- mi_code
            }
            habs$result$suitability[j] <-
              paste0(
                habs$result$suitability[j],
                habs$result$majorimportance[j]
                )
          }

          habs$result <-
            habs$result[order(habs$result$season), ]

          #adding habitat suitability values to relevant columns
          for (j in 1:length(unique(habs$result$season))) {
            this.season <- as.numeric(as.character(unique(habs$result$season)[j]))
            df[j, 2] = this.season
            temp <-
              habs$result[which(as.numeric(as.character(habs$result$season)) ==
                                  this.season), ]
            for (k in 1:nrow(temp)) {
              hab_column <-
                paste("iucn", paste(unlist(
                  strsplit(temp$code[k], split = "[.]")
                ), collapse = "_"), sep = "_")
              df[j, hab_column] = temp$suitability[k]
            }
          }
        }else{
          df<-df[1,]
          df[,2]=999
          df[,cols_to_check] <-
            66
        }

        #and now the rest
        #brief sleep
        Sys.sleep(sample(stats::rnorm(100, sleep_dur, 0.1), 1))
        if (query == "name") {
          elevation <- rredlist::rl_search(name = species, key = key)
        } else if (query == "ID") {
          elevation <-
            rredlist::rl_search(id = as.numeric(as.character(species)), key = key)
        }
        if(length(elevation$result)>0){
          df$iucn_id = as.numeric(as.character(elevation$result$taxonid[1]))
          df$kingdom = as.character(elevation$result$kingdom[1])
          df$class = as.character(elevation$result$class[1])
          df$binomial = as.character(elevation$result$scientific_name[1])
          df$min_alt = as.numeric(as.character(elevation$result$elevation_lower[1]))
          df$max_alt = as.numeric(as.character(elevation$result$elevation_upper[1]))
          df$iucn_category=as.character(elevation$result$category[1])
        }else{
          if (query == "name") {
            df$binomial = species
          } else if (query == "ID") {
            df$iucn_id = species
          }
        }
        #if NA lets make it clear that's because of a lack of data
        if (is.na(df$min_alt[1])) {
          df$min_alt = -99999
        }
        if (is.na(df$max_alt[1])) {
          df$max_alt = 99999
        }
        #to stop the redlist api getting annoyed
        df <- df[which(!is.na(df$season)),, drop = F]
        df <- df
      },
      error = function(err) {
        df<-df[1,]
        #but if this fails then just fill all the hab columns with 6s
        df[,which(colnames(df) == "iucn_1_1"):length(df)] <-
          66
        if (query == "name") {
          df$binomial = species
        } else if (query == "ID") {
          df$iucn_id = species
        }
        df$min_alt=-99999
        df$max_alt=99999
        df[,2]=999
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
