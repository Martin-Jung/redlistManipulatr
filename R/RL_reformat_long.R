#' Reformat wide habitat data to long format
#' @description Pivot habitat columns from wide format, as returned by \code{RL_fetch()}, to long format.
#' @author Matt Lewis, \email{matthewlewis896@@gmail.com}
#'
#' @param x A wide-format \code{data.frame} with one column per habitat category, such as the output of \code{RL_fetch()} or \code{RL_code_fill()}.
#' @param na.rm Logical. If \code{TRUE}, remove rows with missing or no habitat information. If \code{FALSE}, retain these rows. Defaults to \code{TRUE}.
#' @details The pivot includes every habitat column, including categories with no information. With the default \code{na.rm = TRUE}, rows with \code{NA} or the no-data code \code{66} are removed. Set \code{na.rm = FALSE} to retain them.
#' @return A long-format \code{data.frame} with one row per species-season-habitat combination and separate columns for habitat levels, suitability, and major importance.
#' @export

RL_reformat_long <-
  function(
    x,
    na.rm = TRUE
    ){
    if(!is.logical(na.rm)){
      stop("Please supply a valid value for 'na.rm'.")
    }

    packTest("tidyr")

    habitats <- redlistManipulatr::habitats
    seasons <- redlistManipulatr::seasons
    suitability <- redlistManipulatr::suitability
    major_importance <- redlistManipulatr::major_importance

    hab_cols <-
      hab_col_positions() %>%
      unlist() %>%
      sort() %>%
      as.vector()

    df <-
      x %>%
      tidyr::pivot_longer(
        cols = tidyselect::all_of(hab_cols),
        names_to = "level_3",
        values_to = "suitability"
      )

    if(na.rm == TRUE){
      df <- df[!is.na(df$suitability) & df$suitability != 66,]
    }

    df$level_3 <-
      df$level_3 %>%
      gsub("iucn_", "", .) %>%
      gsub("_", ".", .)

    vals <-
      df$suitability %>%
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
                   paste(
                     collapse = ""
                   )

                 y <-
                   c(
                     suit,
                     maj_imp
                   )
               }
              return(y)
             })

    df$season_text <- df$suitability_text <- df$major_importance <- df$major_importance_text <- df$level_1 <- df$level_2 <- NA
    for(i in 1:nrow(df)){
      # level1, 2, 3
      if(df$level_3[i] %in% habitats$Level1){
        df$level_1[i] <- df$level_3[i]
        df$level_2[i] <- df$level_3[i] <- NA
      }else if(df$level_3[i] %in% habitats$Level2){
        df$level_1[i] <- habitats$Level1[which(habitats$Level2 == df$level_3[i])][1]
        df$level_2[i] <- df$level_3[i]
        df$level_3[i] <- NA
      }else{
        df$level_1[i] <- habitats$Level1[which(habitats$Level3 == df$level_3[i])][1]
        df$level_2[i] <- habitats$Level2[which(habitats$Level3 == df$level_3[i])][1]
      }


      # season text
      if(df$season[i] == 999){
        df$season_text[i] <- NA
      }else{
        df$season_text[i] <-
          seasons$Seasonality[seasons$Code == df$season[i]]
      }
      #major importance & suitability
      if(all(!is.na(vals[[i]]))){
        df$suitability[i] <- vals[[i]][1]
        df$major_importance[i] <- vals[[i]][2]
      }

      #suitability text
      if(is.na(df$suitability[i])){
        df$suitability_text[i] <- NA
      }else if(df$suitability[i] %in% c(999, 6, 4)){
        df$suitability_text[i] <- NA
      }else{
        df$suitability_text[i] <-
          suitability$Name[suitability$Code == df$suitability[i]]
      }

      #major importance text
      if(is.na(df$major_importance[i])){
        df$major_importance_text[i] <- NA
      }else if(df$major_importance[i] %in% c(3,6)){
        df$major_importance_text[i] <- NA
      }else{
        df$major_importance_text[i] <-
          major_importance$Major_Importance[major_importance$Code == df$major_importance[i] &
                                !is.na(major_importance$Code)]
      }

    }

    df <-
      df[,c(colnames(df)[1:which(colnames(df) == "max_alt")],
            "season_text","level_1","level_2", "level_3","suitability","suitability_text","major_importance","major_importance_text")]

    df <- as.data.frame(df)
    return(df)
  }
