#' Recode missing habitat suitability values as suitable
#' @description Replace a missing suitability component (encoded as \code{999} in a habitat cell) with the suitable code \code{3}.
#' @author Matt Lewis, \email{matthewlewis896@@gmail.com}
#'
#' @param x A wide-format \code{data.frame}, such as the output of \code{RL_fetch()}.
#' @return A wide-format \code{data.frame} with the same rows and columns as \code{x}.
#' @export

RL_recode_suitabilities <-
  function(
    x
  ){
    cols_to_check <-
      hab_col_positions() %>%
      unlist() %>%
      sort()
    for(i in 1:nrow(x)){
      for(j in cols_to_check){
        val <-
          x[i,j]
        if(!is.na(val)){
          val <-
            val %>%
            strsplit("") %>%
            unlist()
          suit <-
            paste0(
              val[1:(length(val)-1)],
              collapse=""
            )
          if(suit==999){
            x[i,j] <- paste0("3", val[length(val)])
          }
        }
      }
    }
    return(x)
  }
