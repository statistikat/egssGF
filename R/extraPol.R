#' Extrapolate missing EGSS-data
#'
#' If EGSS-data for a certain time series (geo x indic_pi x nace_r2
#' x ty) is available for at least one period data is extrapolated backwards
#' and forwards in order to generate a complete time series from 2010 to the actual
#' period. The Forecasting method applied here are exponential smoothing state space
#' models (ETS) from the forecast package.
#'
#' If only one value is available for the  whole time series or if the variance of
#' the variable is equal to zero for the whole time series forecasts and backcasts
#' are set to this very value. In any other case extrapolation (and intrapolation)
#' is performed with ETS models from the forecast-package (see description in the vignette).
#'
#'
#' @param x EGSS-Data set created with Function loadNA()
#' @return completed list element for aggregation list
#'
#' @examples
#' data("dat_egssBas")
#' data("natAcc")
#' datEgss <- loadEGSS(x = dat_egssBas)
#' datAll <- loadNA(x = natAcc, y = datEgss, toEst = 2016)
#' testSer <- datAll[code=="AT_EMP_A00_00_000",]
#' extraPol(testSer)
#' @import data.table
#' @importFrom stats var ts predict
#' @importFrom forecast ets
#' @export
extraPol <- function(x){
 code <- NULL
 x0 <- dcast(x,yyyy~code,value.var = "obs_value_rel",fun.aggregate = sum)
 yyyy <- x0$yyyy
 x0 <- x0[,-c(1)]                   # eliminate year: check if no year missing in-between!!!!
 nObs <- nrow(x0)                   # number of Observations in EGSS plus 1 predicted value
 fcList <- lapply(x0,function(x) {
    if(all(is.na(x))){
       ff <- cbind(yyyy,obs_valueEX = rep(NA,nObs))
    } else if(length(x[!is.na(x)])==1 | var(x[!is.na(x)])==0){
       ff <- cbind(yyyy,obs_valueEX = rep(mean(x[!is.na(x)]),nObs))
    } else {
      # Case 1: EGSS-observation for period to estimate is available - interpolate Missing values and Backcasting
      # Case 2: EGSS-observation for period to estimate not available (standard): interpolate missing values,
      # Forecasting and Backcasting
          # if(is.na(x[nObs])){         # if obs to estimate is already available no forecast has to be generated
          #    x1 <- x[1:(nObs-1)]
          #    ii <- which(is.na(x1))
          #    yf <- ts(x1,start=c(sY,1),freq=1)
          #    modf <- ets(yf,model="ZZN",na.action = "na.interp")      # NA-values in-between are interpolated
          #    fitted <- as.numeric(modf$fitted)
          #    x1[ii] <- fitted[ii]
          #    fc <- cbind(time,obs_valueEX = c(x1,predict(modf)$mean[1]))
          # } else{
          #     ii <- which(is.na(x))
          #     yf <- ts(x,start=c(sY,1),freq=1)
          #     modf <- ets(yf,model="ZZN",na.action = "na.interp",lambda="auto")
          #     fitted <- as.numeric(modf$fitted)
          #     x[ii] <- fitted[ii]
          #     fc <- cbind(time,obs_valueEX = x)
          # }
          naV <- which(!is.na(x))
          firstObs <- naV[1]                                    # firstObservation which is not NA
          lastObs <- naV[length(naV)]                           # last Observation which is not NA
          if(nObs-lastObs==0){
             nFc <- 0
          } else{
             nFc <- seq(1,(nObs-lastObs))                          # How many Observations to forecast
          }
          if(firstObs-1==0){
             nBc <- 0
          } else{
             nBc <- seq(1,(firstObs-1))                            # How many Observations to backcast
          }

          x1 <- x[firstObs:lastObs]
          ii <- which(is.na(x1))
          yf <- ts(x[firstObs:lastObs],frequency = 1)
          modf <- ets(yf,model="ZZN",na.action = "na.interp")
          fc <- predict(modf)$mean[nFc]
          yb <- ts(rev(yf),frequency = 1)
          modb <- ets(yb,model="ZZN",na.action = "na.interp")
          bc <- rev(predict(modb)$mean[nBc])
          fitted <- as.numeric(modf$fitted)
          x1[ii] <- fitted[ii]
          ff <- cbind(yyyy,obs_valueEX=c(as.numeric(bc),x1,as.numeric(fc)))
       }
   #    if(!(any(x<0)) & fc<0 ){        # negative Values only for series which had already negatives in the past
   #       fc <- 0.00
   #    }
   ff <- as.data.table(ff)
})

 xComp <- rbindlist(fcList,idcol="code")
 setkey(xComp,code,yyyy)
 return(xComp[])
}

