#' Create EGSS-Table with calculated totals
#'
#' Function caltulates all totals of EGSS data by summing up base-level data.
#' Function is applied directly after gap-filling.
#' No iterative fitting has to be applied after this step.
#'
#' @param x EGSS-data table with missing cells completed
#' @return Completed EGSS data table with totals (total nace, total ceparemas
#' and totals) calculated. Observed totals
#' are replaced
#'
#' @examples
#' datEgss <- loadEGSS(x = dat_egssBas, y = currency)
#' datAll <- loadNA(x = natAccN, y = datEgss, z = currency, toEst = 2016,
#'                  t1 = "TOT_EGSS")
#' datComp <- gapFill(x = datAll)
#' tabCalc <- aggTotals(datComp)
#'
#' @import data.table
#' @importFrom utils tail
#' @importFrom formattable formattable
#' @export
#'
#'
aggTotals <- function(x) {
  nace_r2 <- ceparema <- code <- val1 <- obs_value <- NULL
  y <- x[nace_r2 == "TOTAL" | substr(ceparema, 1, 3) == "TOT", ":="
         (obs_value = NA, orig = FALSE)]
  y[, ":=" (cepId = substr(code, 15, 15))]

  # calculation of totals per nace
  y_row <- y[, val1 := lapply(.SD, sum, na.rm = TRUE),
                       by = c("geo", "yyyy", "indic_pi", "nace_r2"),
             .SDcols = "obs_value"]
  y_row[, ":=" (obs_value = ifelse(ceparema == "TOTAL" & !(nace_r2 == "TOTAL"),
                                   val1, obs_value),
                     val1 = NULL), ]
  # calculation of tot_cepa and tot_crema per nace
  y_row1 <- y_row[, val1 := lapply(.SD, sum, na.rm = TRUE),
                         by = c("geo", "yyyy", "indic_pi", "nace_r2", "cepId"),
                  .SDcols = "obs_value"]
  y_row1[, ":=" (obs_value = ifelse(substr(ceparema, 1, 4) == "TOT_" &
                                      !(nace_r2 == "TOTAL"), val1, obs_value),
                   val1 = NULL), ]
  # calculation of total ceparemas (including tot_cepa, tot_crema and total)
  y_col <- y_row1[, val1 := lapply(.SD, sum, na.rm = TRUE),
                         by = c("geo", "yyyy", "indic_pi", "ceparema"),
                  .SDcols = "obs_value"]
  y_col[, ":=" (obs_value = ifelse(nace_r2 == "TOTAL", val1, obs_value),
                     val1 = NULL)]
  return(y_col[])
}
