#' Iterative fitting over all marginal tables of EGSS-data
#'
#' Generation of final (consistent) EGSS data-matrices from gap-filled EGSS data.
#'
#' @param x Gap-filled EGSS data set (non consistent data)
#' @param n Number of iterations
#' @return EGSS-data set made consistent in all dimensions
#'
#' @examples
#' datEgss <- loadEGSS(x = dat_egssBas, y = currency)
#' datAll <- loadNA(x = natAccN, y = datEgss, z = currency, toEst = 2016, t1 = "TOT_EGSS")
#' datComp <- gapFill(x = datAll)
#' genConv(x = datComp, n = 10)
#'
#' @import data.table
#' @export
genConv <- function(x, n){
  nace_r2 <- ceparema <- CEPA1 <- CEPA2 <- CEPA3 <- CEPA4 <- CEPA5 <- CEPA6 <- 'CEPA7-9' <- NULL
  TOT_CEPA <- CREMA10 <- CREMA11 <- CREMA13 <- CREMA14 <- CREMA12_15_16 <- TOT_CREMA <- TOTAL <- NULL
  agg7 <- copy(x)
  start <- Sys.time()
  pb <- txtProgressBar(min = 0, max = n, style=3)
  for(ii in 1:n){
    agg1 <- iterFit(agg7,z=NULL,lc=c(6,0,0),nLevel1=LETTERS[1:21])
    agg2a <- iterFit(agg1,z=NULL,lc=c(6,0,0),cLevel1=c("TOT_CEPA", "TOT_CREMA"))
    agg2b <- iterFit(agg2a,z=c("ceparema","TOT_CEPA"),lc=c(6,0,0), cLevel=c("TOT_CEPA"),
                     cLevel1=c("CEPA1", "CEPA2", "CEPA3", "CEPA4", "CEPA5", "CEPA6", "CEPA7-9"))
    agg2c <- iterFit(agg2b,z=c("ceparema","TOT_CREMA"),lc=c(6,0,0), cLevel=c("TOT_CREMA"),
                     cLevel1=c("CREMA10", "CREMA11", "CREMA13A", "CREMA13B", "CREMA13C",
                               "CREMA14", "CREMA12_15_16"))
    agg3 <- iterFit(agg2c,z = c("ceparema","TOTAL"), nLevel=LETTERS[1:21], nLevel1=LETTERS[1:21],
                    cLevel1=c("TOT_CEPA", "TOT_CREMA"))
    agg4 <- iterFit(agg3,z = c("nace_r2","TOTAL"),lc=c(6,15,17),nLevel1=LETTERS[1:21],
                    cLevel=c("TOT_CEPA", "TOT_CREMA"), cLevel1=c("TOT_CEPA", "TOT_CREMA"))
    agg5 <- iterFit(agg4,z=c("ceparema","TOT_CEPA"), nLevel=LETTERS[1:21],nLevel1=LETTERS[1:21],
                    cLevel=c("TOT_CEPA"),
                    cLevel1=c("CEPA1", "CEPA2", "CEPA3", "CEPA4", "CEPA5", "CEPA6", "CEPA7-9"))
    agg6 <- iterFit(agg5,z=c("ceparema","TOT_CREMA"), nLevel=LETTERS[1:21],nLevel1=LETTERS[1:21],
                    cLevel=c("TOT_CREMA"),
                    cLevel1=c("CREMA10", "CREMA11", "CREMA13A", "CREMA13B", "CREMA13C", "CREMA14", "CREMA12_15_16"))
    agg7 <- iterFit(agg6,z=c("nace_r2","TOTAL"),lc=c(6,15,17),nLevel1=LETTERS[1:21],
                    cLevel=c("CEPA1", "CEPA2", "CEPA3", "CEPA4", "CEPA5", "CEPA6", "CEPA7-9","TOT_CEPA",
                             "CREMA10", "CREMA11", "CREMA13A", "CREMA13B", "CREMA13C", "CREMA14", 
                             "CREMA12_15_16","TOT_CREMA"),
                    cLevel1=c("CEPA1", "CEPA2", "CEPA3", "CEPA4", "CEPA5", "CEPA6", "CEPA7-9","TOT_CEPA",
                              "CREMA10", "CREMA11", "CREMA13A", "CREMA13B", "CREMA13C", "CREMA14", 
                              "CREMA12_15_16","TOT_CREMA"))
    setTxtProgressBar(pb, ii)
  }
  cat(" ","\n")
  print(difftime(Sys.time(), start))
  return(agg7[])
}
