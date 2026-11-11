#' Build the long-format threat output
#' @description Internal helper that creates the threat columns used in the output of \code{RL_threats()}.
#' @author Matt Lewis, \email{matthewlewis896@@gmail.com}
#' @keywords internal
#' @param threats \code{data.frame} of Red List threat categories, such as \code{redlistManipulatr::threats}.
#'
#' @return A long-format \code{data.frame} with one row per species-threat combination.


threatform <-
  function(
    threats = redlistManipulatr::threats
  ){
    threats$Level2[is.na(threats$Level2)] <- threats$Level1[is.na(threats$Level2)]
    threats$Level3[is.na(threats$Level3)] <- threats$Level2[is.na(threats$Level3)]
    threats <- threats[, "Level3"]
    threats <- unique(threats)

    df <-
      as.data.frame(matrix(nrow = length(threats),
                           ncol = 12))

    colnames(df) <-
      c(
        "iucn_id",
        "binomial",
        "code",
        "title",
        "level1_code",
        "level2_code",
        "level3_code",
        "timing",
        "scope",
        "severity",
        "score",
        "invasive"
      )
    return(df)
  }
