#' Check and recode elevation ranges
#' @description Find rows where minimum elevation is greater than (or, by default, equal to) maximum elevation and replace the invalid range.
#' @author Matt Lewis, \email{matthewlewis896@@gmail.com}

#' @param x A wide- or long-format \code{data.frame}, such as the output of \code{RL_fetch()} or \code{RL_reformat_long()}.
#' @param recode_val Numeric vector of length one or two giving replacement minimum and maximum elevations. Defaults to \code{c(-99999, 99999)}. A single value is used for both columns; \code{NA} is also accepted.
#' @param equal_to Logical. If \code{TRUE}, recode rows where minimum elevation is greater than or equal to maximum elevation. If \code{FALSE}, recode only when minimum is greater. Defaults to \code{TRUE}.
#' @param verbose Logical. If \code{TRUE}, print the species whose elevations were recoded. Defaults to \code{FALSE}.
#' @return A \code{data.frame} in the same format as \code{x}.
#' @export

RL_elevation_check <-
  function(
    x,
    recode_val,
    equal_to = TRUE,
    verbose = FALSE
  ){
    # check inputs
    assertthat::assert_that(
      assertthat::is.flag(verbose),
      assertthat::is.flag(equal_to)
    )
    assertthat::assert_that(
      missing(recode_val) ||
      ((is.numeric(recode_val) || is.na(recode_val)) &&
       length(recode_val) %in% c(1,2)),
      msg = "`recode_val` should either be a numeric vector of length 1 or 2, or NA"
    )
    if(missing(recode_val)){
      recode_val <- c(-99999, 99999)
    }else if(length(recode_val) ==1L){
      recode_val <- rep(recode_val, 2)
    }

    # choose comparator
    if(equal_to){
      comparator <- `<=`
    }else{
      comparator <- `<`
    }

    # get vals from data
    min_alt <-
      x$min_alt %>%
      as.character() %>%
      as.numeric()
    max_alt <-
      x$max_alt %>%
      as.character() %>%
      as.numeric()

    # get delta
    delta_alt <-
      max_alt - min_alt


    alts_to_change <-
      which(comparator(delta_alt, 0))

    if(length(alts_to_change) >0L){
      if(verbose == TRUE){
        species_to_change <-
          x[alts_to_change,c("iucn_id", "binomial")]
        message("The following species have had elevations changed:\n")
        print(species_to_change)
      }

      x$min_alt[alts_to_change] <- recode_val[1]
      x$max_alt[alts_to_change] <- recode_val[2]
    }else{
      if(verbose == TRUE){
        message("No species have had elevations changed.")
      }
    }
    return(x)
  }
