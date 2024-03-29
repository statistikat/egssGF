#' Tables of Nace times Ceparema for given countries, years and variables
#'
#' This function creates Tables with the same built as the original EGSS-data tables from Excel. Further, the tables
#' consider, if a cell entry was reported by the member-country or if it was estimated with the gap-filling procedure.
#' Finally, row-sums and column-sums are compared with respective row- and column-totals. These tables can be created
#' with every level of EGSS-data set, i.e. with original reported data or with some intermediate or final EGSS-data set.
#'
#' @param x EGSS-data table
#' @param per Period demanded for the table
#' @param g Country to be displayed
#' @param v Variable to be displayed

#'
#' @examples
#' datEgss <- loadEGSS(x = dat_egssBas, y = currency)
#' datAll <- loadNA(x = natAccN, y = datEgss, z = currency, toEst = 2016, t1 = "TOT_EGSS")
#' datComp <- gapFill(x = datAll)
#' resPrelim <- genConv(datComp, 20)
#' tables1(resPrelim, per = 2015, g = "AT", v = "PROD")
#' @import data.table formattable
#' @export
#'

tables1<- function(x,per,g,v){
  yyyy <- geo <- indic_pi <- . <- nace_r2 <- ceparema <- obs_value <- orig <- SUM <- CEPA1 <- NULL
  CEPA2 <- CEPA3 <- CEPA4 <- CEPA5 <- CEPA6 <- 'CEPA7-9' <- TOT_CEPA <- CREMA10 <- CREMA11 <- NULL
  CREMA13A <- CREMA13B <- CREMA13C <- CREMA14 <- CREMA12_15_16 <- TOT_CREMA <- TOTAL <- NULL 
  DTotC <- D1_9 <- D10_16 <- D1_16 <- NACE <- S1_16 <- S1_9 <- S10_16 <- STotC <- NULL
  color_tile <- color_bar <- NULL
  t00 <- x[yyyy == per & geo == g & indic_pi == v, ]
  t00 <- t00[, .(yyyy, geo, indic_pi, nace_r2, ceparema, obs_value, orig)]
  t01 <- dcast(t00, nace_r2~ceparema, fun.aggregate = mean, value.var = c("obs_value"))
  t02 <- t01[, lapply(.SD, round, 2),
             .SDcols = c("CEPA1", "CEPA2", "CEPA3", "CEPA4", "CEPA5", "CEPA6",
                       "CEPA7-9", "CREMA10", "CREMA11", "CREMA13A", "CREMA13B",
                       "CREMA13C", "CREMA14", "CREMA12_15_16", "TOT_CEPA",
                       "TOT_CREMA", "TOTAL")]
  t02[, S1_16 := rowSums(.SD), .SDcols = colnames(t02)[1:14]]
  t02[, S1_9 := rowSums(.SD), .SDcols = colnames(t02)[1:7]]
  t02[, S10_16 := rowSums(.SD), .SDcols = colnames(t02)[8:14]]
  t02[, STotC := rowSums(.SD), .SDcols = colnames(t02)[15:16]]
  t20 <- cbind(nace_r2 = t01$nace_r2, t02)
  setkey(t20, nace_r2)
  t21 <- t20[, .(NACE = nace_r2, CEPA1, CEPA2, CEPA3, CEPA4, CEPA5, CEPA6,
                `CEPA7-9`, TOT_CEPA, S1_9, CREMA10, CREMA11, CREMA13A, CREMA13B,
                CREMA13C, CREMA14, CREMA12_15_16, TOT_CREMA, S10_16,
                TOTAL, STotC, S1_16)]
  ro <- colSums(t21[c(1:20, 22), c(2:22)])
  ro <- c(NACE = 1, ro)
  t22 <- rbind(t21, as.data.table(t(ro)))
  t22 <- t22[c(1:20, 22, 21, 23), ]
  t22[23, 1] <- "SUM"

  t22[, ":=" (D1_9 = round(TOT_CEPA - S1_9, 2), D10_16 = round(TOT_CREMA - S10_16, 2),
             DTotC = round(TOTAL - STotC, 2), D1_16 = round(TOTAL - S1_16, 2))]

  diff <- data.table(NACE = "DIFF", round(t22[22, 2:22] - t22[23, 2:22], 2),
                     D1_9 = NA, D10_16 = NA, DTotC = NA, D1_16 = NA)
  t23 <- rbind(t22, diff)
  t23 <- t23[, .(NACE, CEPA1, CEPA2, CEPA3, CEPA4, CEPA5, CEPA6, `CEPA7-9`, TOT_CEPA,
                CREMA10, CREMA11, CREMA13A, CREMA13B, CREMA13C, CREMA14, CREMA12_15_16,
                TOT_CREMA, TOTAL, DTotC,  D1_9, D10_16, D1_16)]
  colnames(t23) <- c("NACE", "CEPA1", "CEPA2", "CEPA3", "CEPA4", "CEPA5", "CEPA6",
                     "CEP79","TOTCEP", "CRE10", "CRE11", "CRE13A", "CRE13B",
                     "CRE13C", "CRE14", "CRE16", "TOTCRE", "TOTAL", "DTotC", 
                     "D1_9", "D10_16", "D1_16")
  t23[23, c(19, 20, 21, 22)] <- NA
  diff_formatter <- formatter("span",
                              style = x ~ style(
                                color = ifelse(x == 0, "grey", "red"),
                                font.weight = "bold"))
  t23[is.na(t23)] <- ""
  tab23 <- formattable(t23, align = c("l", rep("r", NCOL(t23) - 1)),
                      list(`NACE` = formatter("span", style = ~
                                                style(color = "grey",
                                                      font.weight = "bold")),
                           `SUM` = formatter("span", style =
                                               ~ style(color = "black",
                                                       font.weight = "bold")),
                           `TOTCEP` = color_tile('cornsilk3', 'cornsilk3'),
                           `TOTCRE` = color_tile('cornsilk3', 'cornsilk3'),
                           #`S1_9` = color_tile('cornsilk2', 'cornsilk2'),
                           #`S10_16` = color_tile('cornsilk2', 'cornsilk2'),
                           #`STotC`= color_tile('cornsilk2', 'cornsilk2'),
                           #`S1_16` = color_tile('cornsilk2', 'cornsilk2'),
                           `TOTAL` = color_tile('darkgrey', 'darkgrey'),
                           `D1_9` =  color_bar('pink'),
                           `D10_16` = color_bar('pink'),
                           `DTotC` = color_bar('pink'),
                           `D1_16` = color_bar('pink'),
                           area(row = 23) ~ formatter("span", style =
                                                        ~ style(color = "black",
                                                                font.weight = "bold")),
                           area(row = 24) ~ diff_formatter
                      ))
  return(tab23)
}
