#' Crosswalk habitat categories to another classification
#' @description Rename habitat columns using a look-up table and optionally recode their values. This is commonly used to map Red List habitat categories to land-cover classes.
#' @author Matt Lewis, \email{matthewlewis896@@gmail.com}
#'
#' @param x A wide-format \code{data.frame}, such as the output of \code{RL_fetch()}, with one column per habitat category.
#' @param lut A look-up \code{data.frame} with columns for the new class names and Red List habitat codes. Mappings may be one-to-one, one-to-many, or many-to-many. Defaults to \code{hab_conversion_lut}.
#' @param new.code.colname Name of the \code{lut} column containing new class names. Defaults to the first column.
#' @param old.code.colname Name of the \code{lut} column containing Red List habitat codes. Defaults to the second column.
#' @param recode How to recode habitat values: \code{"no"} retains the most favourable suitability-major importance code, \code{"one"} assigns \code{1}, and \code{"colname"} assigns the new column name. Defaults to \code{"no"}.
#' @details If only selected suitability or major-importance values should be retained, apply \code{RL_subset_acceptable()} before this function.
#'
#' The default \code{lut}, \code{hab_conversion_lut}, maps habitat codes to integer column names; for example, habitat \code{1.1} maps to \code{110} and \code{9.8.1} maps to \code{981}. When multiple habitat codes map to one new class and \code{recode = "no"}, the most favourable value is chosen according to \code{suitability_ordered}.
#' @return A wide-format \code{data.frame} with habitat columns renamed to the new class names.
#' @export

RL_crosswalk <-
  function(
    x,
    lut = redlistManipulatr::hab_conversion_lut,
    new.code.colname = NULL,
    old.code.colname = NULL,
    recode = "no"
  ){


    if(is.null(new.code.colname)){
      new.col <- 1
    }else if(!is.character(new.code.colname)){
      stop("Please supply a valid character input for 'new.code.colname'.")
    }else if(!(new.code.colname) %in% colnames(lut)){
      stop("Your input for 'new.code.colname' is not a column name of lut.")
    }else{
      new.col <- which(colnames(lut) == new.code.colname)
    }

    if(is.null(old.code.colname)){
      old.col <- 2
    }else if(!is.character(old.code.colname)){
      stop("Please supply a valid character input for 'old.code.colname'.")
    }else if(!(old.code.colname) %in% colnames(lut)){
      stop("Your input for 'old.code.colname' is not a column name of lut.")
    }else{
      old.col <- which(colnames(lut) == old.code.colname)
    }

    if(!is.character(recode)){
      stop("Please supply a valid character input for 'recode'.")
    }else if(!(recode %in% c("no", "one", "colname"))){
      stop("Please supply a valid character input for 'recode'. 'recode' should be one of 'no', 'one' or 'colname'.")
    }

    lut[,old.col] <-
      lut[,old.col] %>%
      gsub("\\.", "_", .) %>%
      lapply(.,
             function(x){
               paste0("iucn_", x)
               })%>%
      unlist()

    # Change column names
    cols_to_check <-
      hab_col_positions() %>%
      unlist() %>%
      sort()

    old_colnames <-
      colnames(x)[cols_to_check] %>%
      gsub("iucn_", "", .) %>%
      gsub("_", ".", .)

    new_colnames <-
      lut[,new.col] %>%
      unique()

    output <-
      matrix(
        nrow = nrow(x),
        ncol = ((length(x) - length(old_colnames)) + length(new_colnames))
      ) %>%
      as.data.frame()

    colnames(output) <-
      c(
        colnames(x)[1:(cols_to_check[1]-1)],
        new_colnames
        )

    new_cols <-
      c(
        cols_to_check[1]:(cols_to_check[1] + (length(new_colnames)-1))
      )

    pref_order <- redlistManipulatr::suitability_ordered

    output[,1:(new_cols[1]-1)] <- x[,1:(new_cols[1]-1)]
    if(recode == "no"){
      for(i in new_cols){
        levels(output[,i]) <- c(levels(output[,i]), pref_order[which(!(pref_order %in% levels(output[,i])))])
      }
    }else if(recode == "one"){
      for(i in new_cols){
        levels(output[,i]) <- c(levels(output[,i]), "1")
      }
    }else if(recode == "colname"){
      for(i in new_cols){
        levels(output[,i]) <- c(levels(output[,i]), colnames(output)[i][which(!(colnames(output)[i] %in% levels(output[,i])))])
      }
    }

    for(i in 1:nrow(output)){
      for(j in new_cols){
        new_value <-
          colnames(output)[j]
        competing_cols <-
          lut[lut[,new.col] == new_value, old.col]
        vals <- x[i, which(colnames(x) %in% competing_cols)]
        vals <- vals[!is.na(vals)]
        if(length(vals) == 0L){
          output[i,j] <- NA
        }else{
          if(recode == "one"){
          output[i,j] <- 1
          } else if (recode == "colname") {
            output[i, j] <- new_value
          } else if (recode == "no") {
            if (length(competing_cols) == 1L) {
              output[i, j] <- x[i, which(colnames(x) == competing_cols)]
            } else{
              best_val <-
                vals %>%
                lapply(.,
                       function(z) {
                         which(pref_order == z)
                       }) %>%
                unlist() %>%
                which.min()
              output[i, j] <- vals[best_val]
            }
          }
        }
      }
    }

    return(output)
  }
