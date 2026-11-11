#' Retain selected habitat suitability and major-importance codes
#' @description Keep only habitat values that meet the selected suitability and major-importance criteria. Other values are replaced with \code{NA}; rows with no remaining habitat values can also be removed.
#' @author Matt Lewis, \email{matthewlewis896@@gmail.com}
#'
#' @param x A wide-format \code{data.frame} with one column per habitat category, such as the output of \code{RL_fetch()}.
#' @param acceptable_suitabilities Numeric vector of suitability codes to retain, such as \code{c(3, 2)}. Defaults to \code{NA}, which retains all suitability values.
#' @param acceptable_importances Numeric vector of major-importance codes to retain, such as \code{c(1, 3)}. Defaults to \code{NA}, which retains all major-importance values.
#' @param na.rm Logical. If \code{TRUE}, remove rows whose habitat values are all \code{NA} after filtering. Defaults to \code{TRUE}.
#' @details See \code{suitability} and \code{major_importance} for the code values and their meanings. A habitat value is retained only when it meets both filters; a filter set to \code{NA} accepts all values for that component.
#' @return A wide-format \code{data.frame} with the same columns as \code{x}.
#' @export

RL_subset_acceptable <-
  function(
    x,
    acceptable_suitabilities = NA,
    acceptable_importances = NA,
    na.rm = TRUE
  ){
    if(!is.logical(na.rm)){
      stop("Please provide a valid logical input for 'na.rm'.")
    }

    cols_to_check <-
      hab_col_positions() %>%
      unlist() %>%
      sort()

    if(all(!is.na(acceptable_suitabilities)) |
       all(!is.na(acceptable_importances))
       ){
      rows_to_remove <- c()
      for(i in 1:nrow(x)){
        vals <- x[i, cols_to_check]
        vals <-
          vals %>%
          lapply(.,
                 function(y){
                   if(!is.na(y)){
                     y <-
                       y %>%
                       strsplit("") %>%
                       unlist()
                     maj_imp <- y[length(y)]
                     suit <-
                       y[1:(length(y)-1)] %>%
                       paste(collapse = "")

                     allow = 0
                     if(all(!is.na(acceptable_suitabilities))){
                       if(suit %in% acceptable_suitabilities){
                         allow = allow + 1
                       }
                     }else{
                       allow = allow + 1
                     }
                     if(all(!is.na(acceptable_importances))){
                       if(maj_imp %in% acceptable_importances){
                         allow = allow + 1
                       }
                     }else{
                       allow = allow + 1
                     }

                     if(allow == 2){
                       y <- paste0(suit, maj_imp)
                     }else{
                       y <- NA
                     }
                   }
                   return(y)
                 }) %>%
          unlist()
        if(all(is.na(vals))){
          rows_to_remove <- c(rows_to_remove, i)
        }
        x[i, cols_to_check] <- vals
      }
      if(na.rm == TRUE &
         length(rows_to_remove) >0L){
        x <- x[-rows_to_remove,]
      }
    }
    return(x)
  }
