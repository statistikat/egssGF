#' Evaluation of iterative Fitting
#'
#' Function to check, if the iterative fitting converges. This function checks the horizontal and vertical
#' consistency of the imputed data set.
#'
#' @param x Completed EGSS-data table
#' @return Ratios of row/column- wise differences greater than 0.5 and 0.02 in absolute value together with
#' the value, country, year and variable of the maximum deviation
#'
#' @examples
#' data("dat_egssBas")
#' data("natAcc")
#' datEgss <- loadEGSS(x = dat_egssBas)
#' datEgssNA <- loadNA(x = natAcc, y = datEgss, toEst = 2016, t1 = "TOT_EGSS")
#' datComp <- gapFill(x = datEgssNA)
#' resPrelim <- genConv(datComp, 50)
#' iterControl(resPrelim)
#'
#' @import data.table
#' @importFrom utils tail
#' @importFrom formattable formattable
#' @export
#'
#'
iterControl <- function(x){
    ceparema <- nace_r2 <- obs_value <- obsT <- diffAbs <- id <- geo <- yyyy <- indic_pi <- . <- NULL
    a <- list()
    # agg1
    a[[1]] <- x[ceparema == "TOTAL" & !(nace_r2=="TOTAL"), lapply(.SD,sum,na.rm=TRUE),
           by=c("geo", "yyyy", "indic_pi"), .SDcols = c("obs_value")]
    a[[2]] <- x[ceparema == "TOTAL" & nace_r2 == "TOTAL",
            mget(c("geo","yyyy","indic_pi","obs_value"))]
    # agg2
    a[[3]] <- x[!(substr(ceparema,1,3) =="TOT") & nace_r2=="TOTAL", lapply(.SD,sum,na.rm=TRUE),
           by=c("geo", "yyyy", "indic_pi"), .SDcols = c("obs_value")]
    a[[4]] <- x[ceparema == "TOTAL" & nace_r2 == "TOTAL",
            mget(c("geo","yyyy","indic_pi","obs_value"))]
    # TOT_CEPA and TOT_CREMA to TOTAL by NACE
    a[[5]] <- x[ceparema %in% c("TOT_CEPA","TOT_CREMA") & !(nace_r2 %in% c("TOTAL")),
           lapply(.SD,sum,na.rm=TRUE),by=c("geo","yyyy","indic_pi","nace_r2"),.SDcols=c("obs_value")]
    a[[6]] <- x[ceparema == "TOTAL" & !(nace_r2 %in% c("TOTAL")),
            mget(c("geo","yyyy","indic_pi","nace_r2","obs_value"))]
    # Cepa1 to Cepa9 to TOT_CEPA
    a[[7]] <- x[ceparema %in% c("CEPA1","CEPA2","CEPA3","CEPA4","CEPA5","CEPA6","CEPA7-9") &
             !(nace_r2 %in% c("TOTAL")), lapply(.SD,sum,na.rm=TRUE),by=c("geo","yyyy","indic_pi","nace_r2"),
              .SDcols=c("obs_value")]
    a[[8]] <- x[ceparema=="TOT_CEPA" & !(nace_r2 %in% "TOTAL"),  mget(c("geo","yyyy","indic_pi","nace_r2","obs_value"))]
    # Crema10 to Crema16 to TOT_CREMA
    a[[9]] <- x[ceparema %in% c("CREMA10","CREMA11","CREMA13","CREMA14","CREMA12_15_16") &
             !(nace_r2 %in% c("TOTAL")), lapply(.SD,sum,na.rm=TRUE),by=c("geo","yyyy","indic_pi","nace_r2"),
             .SDcols=c("obs_value")]
    a[[10]] <- x[ceparema=="TOT_CREMA" & !(nace_r2 %in% "TOTAL"),  mget(c("geo","yyyy","indic_pi","nace_r2","obs_value"))]
    # Cepa1 to Crema16 to TOTAL (ceparema)
    a[[11]] <- x[!(ceparema %in% c("TOTAL","TOT_CEPA","TOT_CREMA")) & !(nace_r2 %in% c("TOTAL")),
               lapply(.SD,sum,na.rm=TRUE),by=c("geo","yyyy","indic_pi","nace_r2"),
              .SDcols=c("obs_value")]
    a[[12]] <- x[ceparema=="TOTAL" & !(nace_r2 %in% "TOTAL"), mget(c("geo","yyyy","indic_pi","nace_r2","obs_value"))]
    # NACE A-U to TOTAL NACE by Ceparema
    a[[13]] <- x[!(ceparema %in% c("TOTAL")) & !(nace_r2 %in% c("TOTAL")),
                 lapply(.SD,sum,na.rm=TRUE),by=c("geo","yyyy","indic_pi","ceparema"),
                 .SDcols=c("obs_value")]
    a[[14]] <- x[!(ceparema=="TOTAL") & nace_r2 == "TOTAL",
            mget(c("geo","yyyy","indic_pi","ceparema","obs_value"))]

    for(ii in seq(2,14,2)){
      a[[ii]][,":="  (obsT=obs_value,obs_value=NULL)]
    }

    for(ii in 1:4){
      setkeyv(a[[ii]],c("geo","yyyy","indic_pi"))
    }
    for(ii in 5:12){
      setkeyv(a[[ii]],c("geo","yyyy","indic_pi", "nace_r2"))
    }
    for(ii in 13:14){
      setkeyv(a[[ii]],c("geo","yyyy","indic_pi", "ceparema"))
    }
    b <- NULL; xx2 <- NULL
    for(ii in seq(1,13,2)){
      xx <- merge(a[[ii]],a[[ii+1]])
      xx[,":=" (diffAbs=abs(obs_value-obsT))]
      share05 <- nrow(xx[diffAbs<0.5,])/nrow(xx)
      share001 <- nrow(xx[diffAbs<0.02,])/nrow(xx)
      setkey(xx,diffAbs)
      xx1 <- xx[diffAbs>0.5,]
      xx1 <- xx1[, id:=paste0(geo,yyyy,indic_pi)]
      nTab05 <- length(unique(xx1$id))
      maxi <- tail(xx)[6]
      maxi <- maxi[,.(geo,yyyy,indic_pi,diffAbs)]
      b1 <- data.table(diff05=share05,diff001=share001,nTab05,maxi)
      b <- rbind(b,b1)
      xx2 <- rbind(xx2,xx1[,.(yyyy,geo,indic_pi)])
    }
    out <- list(b,unique(xx2))
  return(out[])
}
