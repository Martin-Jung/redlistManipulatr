#' Create the wide-format habitat template
#' @description Internal helper that creates the wide-format structure used by \code{RL_fetch()}, with habitat columns and one row for each Red List season code.
#' @author Matt Lewis, \email{matthewlewis896@@gmail.com}
#' @keywords internal
#' @param habs \code{data.frame} of Red List habitat categories, such as \code{redlistManipulatr::habitats}.
#' @param seas \code{data.frame} of Red List season codes, such as \code{redlistManipulatr::seasons}.
#'
#' @return A wide-format \code{data.frame} with one column per habitat category and one row per season.


wideform <-
  function(
    habs = redlistManipulatr::habitats,
    seas = redlistManipulatr::seasons
  ){
    habs$Level2[is.na(habs$Level2)] <- habs$Level1[is.na(habs$Level2)]
    habs$Level3[is.na(habs$Level3)] <- habs$Level2[is.na(habs$Level3)]
    habs <- habs[, "Level3"]
    habs <- unique(habs)

    df <-
      as.data.frame(matrix(ncol = length(habs) + 8, nrow = nrow(seas)))

    hab_names <- c()
    for (i in 1:length(habs)) {
      name <-
        paste(unlist(strsplit(habs[i], split = "[.]")), collapse = "_")
      hab_names <- c(hab_names, paste("iucn", name, sep = "_"))
    }
    colnames(df) <-
      c(
        "iucn_id",
        "season",
        "kingdom",
        "class",
        "binomial",
        "iucn_category",
        "min_alt",
        "max_alt",
        hab_names
      )
    return(df)
  }
