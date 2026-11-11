#' Subset data to selected species-season combinations
#' @description Keep the requested species-season combinations and optionally fill a requested season from data for other seasons.
#' @author Matt Lewis, \email{matthewlewis896@@gmail.com}
#'
#' @param x A wide-format \code{data.frame} with one column per habitat category, such as the output of \code{RL_fetch()}.
#' @param season_df A \code{data.frame} with a species name or ID column and a season-code column. Include one row for each desired species-season combination.
#' @param query Species identifier type: \code{"name"} for scientific names or \code{"ID"} for IUCN Red List IDs.
#' @param species.col.name Name of the species column in \code{season_df}. Defaults to its first column.
#' @param season.col.name Name of the season-code column in \code{season_df}. Defaults to its second column.
#' @param fill.missing.seasons Season codes from which to copy data when a requested season is absent. Defaults to \code{NA}, which disables copying. See \code{seasons} for valid codes.
#' @param retain.na.seasons Logical. If \code{TRUE}, retain rows with missing season information, coded as \code{999} by \code{RL_fetch()}. Defaults to \code{FALSE}.
#' @param retain.missing.sp Logical. If \code{TRUE}, retain species in \code{x} that are absent from \code{season_df}. Defaults to \code{TRUE}.
#' @details The season column in \code{season_df} should use the numeric codes in \code{redlistManipulatr::seasons$Code}.
#'
#' If a requested season is absent from the Red List data, \code{fill.missing.seasons} specifies which available seasons may supply its habitat values. For each habitat, the value with the highest rank in \code{suitability_ordered} is copied. For example, \code{fill.missing.seasons = c(1, 2, 5)} allows data to be copied from Resident, Breeding Season, or Seasonal Occurrence Uncertain records.
#' @return A wide-format \code{data.frame} containing the requested species-season combinations, plus any species retained by \code{retain.missing.sp}.
#' @export

RL_subset_seasons <-
  function(
    x,
    season_df,
    query,
    species.col.name = NA,
    season.col.name = NA,
    fill.missing.seasons = NA,
    retain.na.seasons = FALSE,
    retain.missing.sp = TRUE
  ){
    if(!is.character(query)){
      stop("Please supply a valid value for 'query'. 'query' should be between a character input.")
    }else if(!(query %in% c("name", "ID"))){
      stop("Please supply a valid value for 'query'. 'query' should be between a character input of 'name' or 'ID'.")
    }else if(query =="name"){
      query = "binomial"
    }else{
      query = "iucn_id"
    }

    if(is.na(species.col.name)){
      species.col = 1
    }else{
      if(!is.character(species.col.name)){
        stop("Please supply a valid character input for 'species.col.name'.")
      }
      species.col = which(colnames(season_df) == species.col.name)
    }
    if(is.na(season.col.name)){
      season.col = 2
    }else{
      if(!is.character(season.col.name)){
        stop("Please supply a valid character input for 'season.col.name'.")
      }
      season.col = which(colnames(season_df) == season.col.name)
    }
    if(!is.logical(retain.missing.sp)){
      stop("Please supply a valid logical input for 'retain.missing.sp'.")
    }
    if(!is.logical(retain.na.seasons)){
      stop("Please supply a valid logical input for 'retain.na.seasons'.")
    }

    cols_to_check <-
      hab_col_positions() %>%
      unlist() %>%
      sort()

    pref_order <- redlistManipulatr::suitability_ordered

    output <-
      x[0,]

    for(i in 1:nrow(season_df)){
      species <- season_df[i,species.col]
      seas <- season_df[i,season.col]

      temp <- x[x[,query] == species &
                x$season == seas,]
      #copy from other seasons if no data for desired season
      if(all(nrow(temp) == 0L &
         all(!is.na(fill.missing.seasons)))){
        other_seasons <-
          x[x[,query] == species &
            x$season %in% fill.missing.seasons,]

        if(nrow(other_seasons) >0L){
          temp <- other_seasons[1,]
          temp$season <- seas
          if(nrow(other_seasons) > 1L){
            for(j in cols_to_check){
              vals <- other_seasons[,j]
              vals <- vals[!is.na(vals)]
              if(length(vals) > 0L){
                best_val <-
                  vals %>%
                  lapply(.,
                         function(z){
                           which(pref_order == z)
                         }) %>%
                  unlist() %>%
                  which.min()
                temp[1, j] <- vals[best_val]
              }
            }
          }
        }else{
          temp <- x[1,]
          temp[1,] <- NA
          temp[1, query] <- species
          temp[1,"season"] <- seas
        }
      }
      if(retain.na.seasons == TRUE){
        temp <-
          rbind(
            temp,
            x[x[,query] == species &
                x$season == 999,]
          )
      }
      output <- rbind(output, temp)
    }
    if(retain.missing.sp == TRUE){
      missing_sp <- x[!(x[,query] %in% unique(season_df[,species.col])),]
      output <- rbind(output, missing_sp)
    }
    return(output)
  }
