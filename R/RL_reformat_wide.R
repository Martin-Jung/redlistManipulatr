#' Reformat long habitat data to wide format
#' @description Pivot long-format habitat data, such as the output of \code{RL_reformat_long()}, to wide format for functions such as \code{RL_code_fill()}.
#' @author Matt Lewis, \email{matthewlewis896@@gmail.com}
#'
#' @param x A long-format \code{data.frame} with one row per species-season-habitat combination, such as the output of \code{RL_reformat_long()}.
#' @return A wide-format \code{data.frame} with one column per habitat category.
#' @export

RL_reformat_wide <-
  function(
    x
  ){
    base_df <- wideform()

    x<- x[!is.na(x$suitability),]

    wide_df<- base_df[0,]
    for(i in 1:length(unique(x$iucn_id))){
      sub <- x[x$iucn_id == unique(x$iucn_id)[i],]
      n.seasons <- length(unique(sub$season))
      temp <- base_df[1:n.seasons,]

      temp[,c(1:which(colnames(temp) == "max_alt"))] <-
        sub[1,c(1:which(colnames(temp) == "max_alt"))]
      for(j in 1:n.seasons){
        temp[j, "season"] <- unique(sub$season)[j]
        sub2 <- sub[sub$season == unique(sub$season)[j],]
        for(k in 1:nrow(sub2)){
          val <- paste0(sub2$suitability[k], sub2$major_importance[k])

          if(!is.na(sub2$level_3[k])){
            cat <-
              sub2$level_3[k] %>%
              gsub("\\.", "_",.)
          }else if(!is.na(sub2$level_2[k])){
            cat <-
              sub2$level_2[k] %>%
              gsub("\\.", "_",.)
          }else if(!is.na(sub2$level_1[k])){
            cat <-
              sub2$level_1[k] %>%
              gsub("\\.", "_",.)
          }

          col_name <-
            paste(
              "iucn",
              cat,
              sep = "_"
            )
          temp[j,col_name] <- val
        }
      }
      wide_df <- rbind(wide_df, temp)
    }


    return(wide_df)
  }
