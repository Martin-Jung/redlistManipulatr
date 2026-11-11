#' Find Level 1 and Level 2 habitat columns
#' @description Identify the positions of Level 1 and Level 2 habitat columns in a wide-format dataset.
#' @keywords internal
#'
#' @param x \code{data.frame} with column names in the form \code{iucn_n} or \code{iucn_n_n}, where \code{n} is a numeric habitat code.
#' @return A list containing the column positions for Level 1 and Level 2 habitat categories.

hab_col_positions <-
  function(
    x = wideform()
  ){
    cols_to_check <- which(colnames(x) == "iucn_1"):length(x)
    level1_cols <-
      colnames(x)[cols_to_check] %>%
      lapply(.,
             function(x){
               x <-
                 x %>%
                 strsplit("_") %>%
                 unlist()
               if(length(x) == 2){
                 return(1)
               }else{
                 return(NA)
               }
             }) %>%
      unlist()
    level1_cols <-
      cols_to_check[!is.na(level1_cols)] %>%
      as.list()

    level2_cols <- list()
    for(i in 1:length(level1_cols)){
      if(i < length(level1_cols)){
        level2_cols[[i]] <- cols_to_check[cols_to_check > level1_cols[i] &
                                            cols_to_check < level1_cols[i+1]]
      }else{
        level2_cols[[i]] <- integer(0)
      }
    }

    all_hab_cols <-
      list(
        Level1 = level1_cols,
        Level2 = level2_cols
      )
  }
