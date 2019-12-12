#' Putting together all results
#'
#' Generation of final EGSS data-matrices from iterative fitted data.
#'
#' @param x EGSS data matrix with some tables which did not converge in the iterative fitting procedure
#' @param y Non-consistent tables from x which were made consistent by relaxing the constraints
#' @return list element with consistent EGSS-data for all countries and EU totals
#'
#' @examples
#' datEgss <- loadEGSS(x = dat_egssBas, y = currency)
#' datAll <- loadNA(x = natAccN, y = datEgss, z = currency, toEst = 2016, t1 = "TOT_EGSS")
#'
#' @import data.table
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @export
results <- function(x,y) {
  yyyy <- geo <- cc <- . <- code <- indic_pi <- ty <- nace_r2 <- ceparema <- obs_value <- NULL

  if(is.null(y)){
    datFin <- x
  } else{
    yInd <- y[,.(code,yyyy)]
    yInd$cc <- 1
    setkey(x,code,yyyy)
    setkey(yInd,code,yyyy)
    xBas <- merge(x,yInd,all.x=TRUE)
    xBas <- xBas[is.na(cc),]
    xBas[,":=" (cc=NULL)]
    datFin <- rbind(xBas,y)
  }
  datFin[,":=" (obs_gen = NULL, naINDIC = NULL)]
  datEU28 <- datFin[, lapply(.SD,sum), by=c("yyyy","indic_pi","ty","nace_r2","ceparema"), .SDcols=c("obs_value")]
  datEU28[,":=" (geo = "EU28", orig = FALSE, obs_status = "", obs_conf = "", obs_comment = "")]
  datEU27 <- datFin[!(geo %in% c("UK")), lapply(.SD,sum), by=c("yyyy","indic_pi","ty","nace_r2","ceparema"), .SDcols=c("obs_value")]
  datEU27[,":=" (geo = "EU27", orig = FALSE, obs_status = "", obs_conf = "", obs_comment = "")]
  out <- list(allCountries = datFin, EU28 = datEU28, EU27 = datEU27)
  return(out[])
}
