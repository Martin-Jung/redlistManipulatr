#' Recode missing seasons as resident
#' @description Replace season codes of \code{999}, which represent missing seasonal information, with the resident season code \code{1}.
#' @author Matt Lewis, \email{matthewlewis896@@gmail.com}
#'
#' @param x A wide-format \code{data.frame}, such as the output of \code{RL_fetch()}.
#' @param combine.rows Logical. If recoding creates multiple rows for the same species and season, combine them by retaining the highest-ranked suitability-major importance value for each habitat. See \code{suitability_ordered}. Defaults to \code{FALSE}.
#' @return A wide-format \code{data.frame}. If \code{combine.rows = TRUE}, duplicate Resident rows are combined.
#' @export

RL_recode_seasons <-
  function(
    x,
    combine.rows = FALSE
  ){
    sp <-
      x$iucn_id %>%
      unique()

    sp_999 <-
      x$iucn_id[x$season == 999] %>%
      unique()

    x$season[x$season == 999] <- 1

    if(combine.rows == TRUE){
      output <- x[0,]
      for(i in 1:length(sp)){
        species <- sp[i]
        temp <- x[x$iucn_id == species,]

        if(species %in% sp_999){
          if(nrow(temp[temp$season == 1,]) >1){
            t1 <- temp[temp$season != 1,]
            t2 <- temp[temp$season == 1,]

            t3 <- t2[1,]
            hab_cols <-
              hab_col_positions() %>%
              unlist() %>%
              sort() %>%
              as.vector()

            pref_order = redlistManipulatr::suitability_ordered

            for(j in hab_cols){
              vals <- t2[,j]
              vals <- vals[!is.na(vals)]
              if(length(vals) >0L){
                best_val <-
                  vals %>%
                  lapply(.,
                         function(z){
                           which(pref_order == z)
                         }) %>%
                  unlist() %>%
                  which.min()
                t3[1, j] <- vals[best_val]
              }

            }
            temp <-
              rbind(
                t1,
                t3
              )
            temp <- temp[order(temp$season),]


          }
        }

        output <-
          rbind(
            output,
            temp
          )
      }
    }else{
      output <- x
    }

    row.names(output) <- 1:nrow(output)

    return(output)
  }
