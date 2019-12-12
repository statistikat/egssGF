#' Visualisation of original together with imputed data for EGSS
#'
#' Plots for every hierarchy level desired are created with values from the x-data set together with those from the
#' y-data set. If the raw-data set (output of loadNA) is set as x-data then the plot shows which values had been
#' originally reported by the member country. These are then compared with the estimated values from the y-data set,
#' which can be any data set of raw-gap filled data or data which had been adapted by the iterative fitting procedure.
#'
#' @param x EGSS-Base data set (ouput of loadNA.R)
#' @param y EGSS-Aggregate with calibrated values (output of iterFit.R)
#' @param geoC Country codes to display, default = ALL
#' @param varC Variable code, possible values are "PROD" (default), "EMP", "VA", "EXP"
#' @param nC Nace code, default = "TOTAL"
#' @param cC Ceparema code, default = "TOTAL"
#' @return Time series plots of EGSS-data with different completion version
#'
#' @examples
#' datEgss <- loadEGSS(x = dat_egssBas, y = currency)
#' datAll <- loadNA(x = natAccN, y = datEgss, z = currency, toEst = 2016, t1 = "TOT_EGSS")
#' datCompl <- gapFill(x = datAll)
#' plotData(x = datAll, y = datCompl)
#' @import data.table
#' @import ggplot2
#' @export
plotData <- function(x, y, geoC = "ALL", varC = "PROD", nC = "TOTAL", cC = "TOTAL") {
  nace_r2 <- ty <- ceparema <- . <- yyyy <- code <- obs_value <- geo <- indic_pi <- NULL
  if(any("ALL" %in% geoC)){
    geoC <- c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "EL", "ES",
            "FI", "FR", "HR", "HU", "IE", "IT", "LT", "LU", "LV", "MT", "NL", "PL",
            "PT", "RO", "SE", "SI", "SK", "UK")
  }

  p0 <- x[nace_r2 %in% nC & ceparema %in% cC,]
  p0[,":=" (source="observed")]
  p0 <- p0[,.(yyyy,code,obs_value,source,geo,indic_pi,ceparema,nace_r2)]
  p1 <- copy(y)
  p1[,":=" (source="estimated")]
  p1 <- p1[,.(yyyy,code,obs_value,source,geo,indic_pi,ceparema,nace_r2)]

  p00 <- rbind(p1,p0)
  setkey(p00,code,yyyy,source)
  p10 <- p00[geo %in% geoC & indic_pi %in% varC & nace_r2 %in% nC &
               ceparema %in% cC,]
  linetype = rep(c('dotted', 'solid'),2)
  ggplot(data = p10, aes(x = yyyy, y = obs_value, linetype = source, color = source)) +
    theme_linedraw() +
    geom_line(alpha=0.8, na.rm = TRUE) +
    facet_wrap(~ code, scales="free") +
    geom_point(na.rm = TRUE) + 
    scale_linetype_manual(values = linetype) +
    scale_color_manual(values = c("brown1","deepskyblue4")) +
    xlab("Time") + ylab("Observation Value")

}
