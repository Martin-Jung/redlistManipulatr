#' Fill missing Level 1 or Level 2 habitat codes
#' @description Fill parent Level 1 habitat codes from their Level 2 categories, or fill Level 2 categories from their Level 1 parent.
#' @author Matt Lewis, \email{matthewlewis896@@gmail.com}
#'
#' @param x A wide-format \code{data.frame}, such as the output of \code{RL_fetch()}, with one column per habitat category.
#' @param level1.recode Logical. If \code{TRUE}, fill a Level 1 category when one or more of its Level 2 categories has data. Defaults to \code{TRUE}.
#' @param level2.recode Logical. If \code{TRUE}, fill Level 2 categories from a Level 1 category that has data. Defaults to \code{TRUE}.
#' @param subset Numeric proportion of rows to process, from 0 to 1. Defaults to 1 (all rows).
#' @param parallel Logical. If \code{TRUE}, use multiple cores. Defaults to \code{TRUE}.
#' @param num.cores Number of cores to use when \code{parallel = TRUE}. Defaults to the number of available cores minus one.
#' @param verbose Progress reporting level: \code{0} for none, \code{1} for a progress bar, or \code{2} for one line per completed row. Defaults to \code{1}.
#'
#' @details With \code{level1.recode = TRUE}, data in a Level 2 category are copied to its Level 1 parent. For example, a value of \code{31} in habitat 2.1 (Dry Savanna) is copied to habitat 2 (Savanna).
#'
#' With \code{level2.recode = TRUE}, data in a Level 1 category are copied to its Level 2 categories. For example, a value of \code{31} in habitat 2 (Savanna) is copied to habitats 2.1 (Dry Savanna) and 2.2 (Moist Savanna).
#'
#' Run this function once on a dataset. Repeated calls can propagate values added by earlier calls.
#' @return A wide-format \code{data.frame}, with one row per species-season combination.
#' @export

RL_code_fill <-
  function(
    x,
    level1.recode = T,
    level2.recode = T,
    subset = 1,
    parallel = T,
    num.cores = parallel::detectCores()-1,
    verbose = 1
    ) {
  #x is the input df
  #y is the final condition - how long is this going for?
  #z is parent_columns we have IUCN hab data in
  #aa is all columns we have IUCN hab data in

    # Handles verbose option
    if(is.numeric(verbose) == FALSE){
      stop("Please supply a valid value for 'verbose'.")
    }else if(!(verbose %in% c(0:2))){
      stop("Please supply a valid value for 'verbose'.")
    }

    #level1 recode
    if(is.logical(level1.recode) == FALSE){
      stop("Please supply a valid value for 'level1.recode'.")
    }
    #level2 recode
    if(is.logical(level2.recode) == FALSE){
      stop("Please supply a valid value for 'level2.recode'.")
    }

    #handles subset
    if(!is.numeric(subset)){
      stop("Please supply a valid value for 'subset'. 'subset' should be between 0 and 1.")
    }else if(subset <0 | subset > 1){
      stop("Please supply a valid value for 'subset'. 'subset' should be between 0 and 1.")
    }else{
      subset <- round(subset * nrow(x))
      message("\nManipulating data for a subset of ", subset, "/", nrow(x), " (", round((100*subset/nrow(x)),2), "%) of the data.")
    }


    if(is.numeric(num.cores) == FALSE){
      stop("Please supply a valid number of cores as an integer number.")
    }else if(num.cores %% 1 != 0){
      stop("Please supply a valid number of cores as an integer number.")
    }else if(num.cores > parallel::detectCores()){
      stop("You have supplied a number of CPU cores greater than the number available.")
    }

    # Getting data (parallel or not)

    if(parallel == TRUE){
      df <- code_fill_par(
        x = x,
        level1.recode = level1.recode,
        level2.recode = level2.recode,
        subset = subset,
        num.cores = num.cores,
        verbose = verbose
      )
    }else{
      df <- code_fill_nopar(
        x = x,
        level1.recode = level1.recode,
        level2.recode = level2.recode,
        subset = subset,
        verbose = verbose
      )
    }
}
