#' Fill missing Level 1 or Level 2 habitat codes in parallel
#' @description Internal parallel helper for \code{RL_code_fill()}.
#' @author Matt Lewis, \email{matthewlewis896@@gmail.com}
#' @keywords internal
#'
#' @param x A wide-format \code{data.frame} with one column per habitat category.
#' @param level1.recode Logical. If \code{TRUE}, fill Level 1 categories from their Level 2 categories.
#' @param level2.recode Logical. If \code{TRUE}, fill Level 2 categories from their Level 1 parent.
#' @param subset Proportion of rows to process, from 0 to 1.
#' @param num.cores Number of cores to use.
#' @param verbose Progress reporting level: \code{0} for none, \code{1} for a progress bar, or \code{2} for one line per completed row.
#'
#' @details A child category can supply its value to its Level 1 parent; a Level 1 value can also be copied to its Level 2 categories.
#' @return A wide-format \code{data.frame} with one row per species-season combination.

code_fill_par <-
  function(x,
           level1.recode,
           level2.recode,
           subset,
           num.cores,
           verbose) {
    if (verbose == 2) {
      progress <-
        function(n)
          cat(sprintf("task %d is complete\n", n))#print when each row is complete
      opts <- list(progress = progress)
    } else if (verbose == 1) {
      pb <- utils::txtProgressBar(style = 3)
      progress <- function(n)
        utils::setTxtProgressBar(pb, n)
      opts <- list(progress = progress)
    } else{
      opts <- NULL
    }

    invisible(lapply(c("parallel", "snow", "doSNOW"),
                     packTest
                     )
    )

    cl <-
      snow::makeCluster(num.cores,
                        outfile = "")
    doSNOW::registerDoSNOW(cl)
    on.exit(snow::stopCluster(cl))

    hab_cols <- hab_col_positions()
    cols_to_check <- as.vector(sort(unlist(hab_cols)))

    pref_order <- redlistManipulatr::suitability_ordered


    ret <- foreach::foreach(i = 1:subset,
                   .combine = "rbind",
                   .options.snow = opts
                   ) %dopar% {
                     df <- x[i,]
                     #which columns are occupied?
                     col_occupied <- c()
                     for (j in cols_to_check) {
                       levels(df[1, j]) <- c(levels(df[1, j]), as.character(pref_order))
                       if (!is.na(df[1, j])) {
                         col_occupied <- c(col_occupied, j)
                       }
                     }
                     #if there is more than 1
                     if (length(col_occupied) > 0L) {
                       for (j in 1:length(col_occupied)) {
                         if (level2.recode == TRUE) {
                           #if level1 codes occupied fill out level 2
                           if (col_occupied[j] %in% unlist(hab_cols$Level1)) {
                             lvl1_col <- which(unlist(hab_cols$Level1) == col_occupied[j])
                             #if it is 18 we don't need to do anything
                             if (lvl1_col != 18) {
                               for (k in unlist(hab_cols$Level2[lvl1_col])) {
                                 #if it's unoccupied
                                 if (is.na(df[1, k])) {
                                   df[1, k] <- as.character(df[1, col_occupied[j]])
                                 }
                               }
                             }
                           }
                         }
                       }
                       if (level1.recode == TRUE) {
                         for (j in 1:length(hab_cols$Level2)) {
                           if (is.na(df[1, unlist(hab_cols$Level1[j])])) {
                             if (any(unlist(hab_cols$Level2[j]) %in% col_occupied)) {
                               lvl2s <-
                                 col_occupied[which(col_occupied %in% unlist(hab_cols$Level2[j]))]
                               if (length(lvl2s) == 1L) {
                                 df[1, unlist(hab_cols$Level1[j])] <- df[1, lvl2s]
                               } else{
                                 vals <- df[1, lvl2s]
                                 vals_max_priority <-
                                   which.min(unlist(lapply(vals,
                                                           function(x) {
                                                             which(pref_order == x)
                                                           })))

                                 best_val <- vals[vals_max_priority]
                                 df[1, unlist(hab_cols$Level1[j])] <-
                                   best_val
                               }
                             }
                           }
                         }
                       }
                     }
                     df
                   }
    return(ret)
  }
