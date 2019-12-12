#' Tables of Nace times Ceparema for given countries, years and variables
#'
#' This function creates Tables with the same built as the original EGSS-data 
#' tables from Excel. Further, the tables consider, if a cell entry was reported
#' by the member-country or if it was estimated with the gap-filling procedure.
#' Finally, row-sums and column-sums are compared with respective row- and 
#' column-totals. These tables can be created with every level of EGSS-data set,
#' i.e. with original reported data or with some intermediate or final EGSS-data set.
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
#' tables(resPrelim, per = 2015, g = "AT", v = "PROD")
#' @import data.table
#' @import formattable
#' @export
#'

tables <- function(x, per, g, v){
  yyyy <- geo <- indic_pi <- . <- nace_r2 <- ceparema <- obs_value <- orig <- SUM <- CEPA1 <- C1 <- NULL
  CEPA2 <- C2 <- CEPA3 <- C3 <- CEPA4 <- C4 <- CEPA5 <- C5 <- CEPA6 <- C6 <- 'CEPA7-9' <- C7 <- NULL
  CREMA10 <- C10 <- CREMA11 <- C11 <- CREMA13A <- C13A <- CREMA13B <- C13B <- CREMA13C <- C13C <- NULL 
  CREMA14 <- C14 <- CREMA12_15_16 <- C12 <- TOTAL <- TC <- NULL
  t00 <- x[yyyy == per & geo == g & indic_pi == v,]
  t00 <- t00[,.(yyyy, geo, indic_pi, nace_r2, ceparema, obs_value, orig)]
  t01 <- dcast(t00, nace_r2 ~ ceparema, fun.aggregate = mean, value.var = c("obs_value"))
  t01a <- dcast(t00, nace_r2 ~ ceparema, fun.aggregate = mean, value.var = c("orig"))
  t01a <- t01a[,c(1:15,18)]
  colnames(t01a) <- c("nace_r2", paste0("C", 1:7), "C10", "C11", "C12", "C13A", "C13B", "C13C",
                      "C14", "TC")

  t02 <- t01[,lapply(.SD, round, 2),
             .SDcols = c("CEPA1", "CEPA2", "CEPA3", "CEPA4", "CEPA5", "CEPA6", "CEPA7-9",
                       "CREMA10", "CREMA11", "CREMA13A", "CREMA13B", "CREMA13C", "CREMA14",
                       "CREMA12_15_16","TOTAL")]
  t02[, SUM := rowSums(.SD), .SDcols = colnames(t02)[1:14]]

  t20 <- cbind(nace_r2 = t01$nace_r2, t02)
  setkey(t20, nace_r2)
  setkey(t01a, nace_r2)
  t21 <- merge(t20, t01a)
  t21 <- t21[,.(nace_r2, CEPA1, C1, CEPA2, C2, CEPA3, C3, CEPA4, C4, CEPA5, C5, CEPA6, C6,
                `CEPA7-9`, C7, CREMA10, C10, CREMA11, C11, CREMA13A, C13A, CREMA13B, C13B,
                CREMA13C, C13C, CREMA14, C14, CREMA12_15_16, C12, TOTAL, TC, SUM)]
  colnames(t21) <- c("NACE", "CEPA1", "C1", "CEPA2", "C2", "CEPA3", "C3", "CEPA4", "C4", "CEPA5", "C5", "CEPA6", "C6",
                     "CEPA79", "C7", "CREMA10", "C10", "CREMA11", "C11", "CREMA13A", "C13A", "CREMA13B",
                     "C13B", "CREMA13C", "C13C", "CREMA14", "C14", "CREMA12", "C12",
                     "TOTAL", "TC", "SUM")
  ro <- colSums(t21[c(1:20,22), c(2:31)])
  ro <- c(NACE = 1, ro, SUM = NA)
  t22 <- rbind(t21, as.data.table(t(ro)))
  t22 <- t22[c(1:20, 22, 21, 23),]
  t22[23, 1] <- "SUM"
  t22[23, seq(3, 31, 2)] <- NA

  t22[,":=" (DIFF = round(TOTAL-SUM, 2))]
  diff <- data.table(NACE = "DIFF", round(t22[22,2:32] - t22[23,2:32], 2), DIFF = NA)
  t23 <- rbind(t22,diff)
  t23[, ":=" (C1 = as.logical(C1), C2 = as.logical(C2), C3 = as.logical(C3), C4 = as.logical(C4), C5 = as.logical(C5),
      C6 = as.logical(C6), C7 = as.logical(C7), C10 = as.logical(C10), C11 = as.logical(C11), C12 = as.logical(C12),
      C13A = as.logical(C13A), C13B = as.logical(C13B), C13C = as.logical(C13C), C14 = as.logical(C14), 
      TC = as.logical(TC))]
  diff_formatter <- formatter("span",
                 style = x ~ style(
                 color = ifelse(x == 0, "grey", "red"),
                 font.weight = "bold"))
  t23[is.na(t23)]=''
  tab23 = formattable(t23, align = c("l", rep("r", NCOL(t23) - 1)),
                      list(`nace_r2` = formatter("span", style = ~ style(color = "grey", font.weight = "bold")),
                           `SUM` = formatter("span", style = ~ style(color = "black", font.weight = "bold")),
                           `DIFF` = diff_formatter,
                           `C1` = formatter("span",
                                               style = x ~ style(color = ifelse(x, "green", "lightgrey")),
                                               x ~ icontext(ifelse(x, "star","star-empty"))),
                           `C2` = formatter("span",
                                            style = x ~ style(color = ifelse(x, "green", "lightgrey")),
                                            x ~ icontext(ifelse(x, "star","star-empty"))),
                           `C3` = formatter("span",
                                            style = x ~ style(color = ifelse(x, "green", "lightgrey")),
                                            x ~ icontext(ifelse(x, "star","star-empty"))),
                           `C4` = formatter("span",
                                            style = x ~ style(color = ifelse(x, "green", "lightgrey")),
                                            x ~ icontext(ifelse(x, "star","star-empty"))),
                           `C5` = formatter("span",
                                            style = x ~ style(color = ifelse(x, "green", "lightgrey")),
                                            x ~ icontext(ifelse(x, "star","star-empty"))),
                           `C6` = formatter("span",
                                            style = x ~ style(color = ifelse(x, "green", "lightgrey")),
                                            x ~ icontext(ifelse(x, "star","star-empty"))),
                           `C7` = formatter("span",
                                            style = x ~ style(color = ifelse(x, "green", "lightgrey")),
                                            x ~ icontext(ifelse(x, "star","star-empty"))),
                           `C10` = formatter("span",
                                             style = x ~ style(color = ifelse(x, "green", "lightgrey")),
                                             x ~ icontext(ifelse(x, "star","star-empty"))),
                           `C11` = formatter("span",
                                             style = x ~ style(color = ifelse(x, "green", "lightgrey")),
                                             x ~ icontext(ifelse(x, "star","star-empty"))),
                           `C12` = formatter("span",
                                             style = x ~ style(color = ifelse(x, "green", "lightgrey")),
                                             x ~ icontext(ifelse(x, "star","star-empty"))),
                           `C13A` = formatter("span",
                                             style = x ~ style(color = ifelse(x, "green", "lightgrey")),
                                             x ~ icontext(ifelse(x, "star","star-empty"))),
                           `C13B` = formatter("span",
                                              style = x ~ style(color = ifelse(x, "green", "lightgrey")),
                                              x ~ icontext(ifelse(x, "star","star-empty"))),
                           `C13C` = formatter("span",
                                              style = x ~ style(color = ifelse(x, "green", "lightgrey")),
                                              x ~ icontext(ifelse(x, "star","star-empty"))),
                           `C14` = formatter("span",
                                             style = x ~ style(color = ifelse(x, "green", "lightgrey")),
                                             x ~ icontext(ifelse(x, "star","star-empty"))),
                           `TC` = formatter("span",
                                            style = x ~ style(color = ifelse(x, "green", "lightgrey")),
                                            x ~ icontext(ifelse(x, "star", "star-empty"))),
                           area(row = 23) ~ formatter("span", style = ~ style(color = "black", font.weight = "bold")),
                           area(row = 24) ~ diff_formatter
                           ))
  return(tab23)
}
