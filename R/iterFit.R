#' Iterative fitting of extrapolated and imputed data to respective available aggregate numbers
#'
#'
#' @param x Complete EGSS-data set, i.e. data-set with no gaps. This data-set is generated with the
#'          function gapFill.R
#' @param lc Vector which allows different aggregation levels.
#'
#' possible values:
#'
#' + c(6,0,0) - Fitting total Nace-levels to EGSS-total and fitting total ceparema-levels to EGSS-total
#' + c(8,0,0) - (default) Fitting row-entries to total nace-letters
#' + c(6,15,17) - Fitting column-entries to total ceparema-codes
#' @param z NACE or ceparema for merging input data set with sub-aggregate data-set
#' @param nLevel Top-level for NACE
#' @param nLevel1 Bottom-level for NACE
#' @param cLevel Top-level for Ceparema
#' @param cLevel1 Bottom-level for Ceparema
#' @return Complete and consistent EGSS-data set regarding the respective aggregation level.
#'
#' @examples
#' data("dat_egssBas")
#' data("natAcc")
#' datEgss <- loadEGSS(x = dat_egssBas)
#' datEgssNA <- loadNA(x = natAcc, y = datEgss, toEst = 2016, t1 = "TOT_EGSS")
#' datComp <- gapFill(x = datEgssNA)
#' agg1 <- iterFit(datComp,z=NULL,lc=c(6,0,0),nLevel1=LETTERS[1:21])
#' agg2a <- iterFit(agg1,z=NULL,lc=c(6,0,0),cLevel1=c("TOT_CEPA", "TOT_CREMA"))
#' agg2b <- iterFit(agg2a,z=c("ceparema","TOT_CEPA"),lc=c(6,0,0), cLevel=c("TOT_CEPA"),
#'                 cLevel1=c("CEPA1", "CEPA2", "CEPA3", "CEPA4", "CEPA5", "CEPA6", "CEPA7-9"))
#' agg2c <- iterFit(agg2b,z=c("ceparema","TOT_CREMA"),lc=c(6,0,0), cLevel=c("TOT_CREMA"),
#'                  cLevel1=c("CREMA10", "CREMA11", "CREMA13", "CREMA14", "CREMA12_15_16"))
#' agg3 <- iterFit(agg2c,z = c("ceparema","TOTAL"), nLevel=LETTERS[1:21], nLevel1=LETTERS[1:21],
#'                 cLevel1=c("TOT_CEPA", "TOT_CREMA"))
#' agg4 <- iterFit(agg3,z = c("nace_r2","TOTAL"),lc=c(6,15,17),nLevel1=LETTERS[1:21],
#'                 cLevel=c("TOT_CEPA", "TOT_CREMA"), cLevel1=c("TOT_CEPA", "TOT_CREMA"))
#' agg5 <- iterFit(agg4,z=c("ceparema","TOT_CEPA"), nLevel=LETTERS[1:21],nLevel1=LETTERS[1:21],
#'                 cLevel=c("TOT_CEPA"),
#'                 cLevel1=c("CEPA1", "CEPA2", "CEPA3", "CEPA4", "CEPA5", "CEPA6", "CEPA7-9"))
#' agg6 <- iterFit(agg5,z=c("ceparema","TOT_CREMA"), nLevel=LETTERS[1:21],nLevel1=LETTERS[1:21],
#'                 cLevel=c("TOT_CREMA"),
#'                 cLevel1=c("CREMA10", "CREMA11", "CREMA13", "CREMA14", "CREMA12_15_16"))
#' agg7 <- iterFit(agg6,z=c("nace_r2","TOTAL"),lc=c(6,15,17),nLevel1=LETTERS[1:21],
#'          cLevel=c("CEPA1", "CEPA2", "CEPA3", "CEPA4", "CEPA5", "CEPA6", "CEPA7-9","TOT_CEPA",
#'                   "CREMA10", "CREMA11", "CREMA13", "CREMA14", "CREMA12_15_16","TOT_CREMA"),
#'          cLevel1=c("CEPA1", "CEPA2", "CEPA3", "CEPA4", "CEPA5", "CEPA6", "CEPA7-9","TOT_CEPA",
#'                  "CREMA10", "CREMA11", "CREMA13", "CREMA14", "CREMA12_15_16","TOT_CREMA"))
#' @import data.table
#' @export
#'

iterFit <- function(x, z, lc=c(8,0,0), nLevel="TOTAL", cLevel="TOTAL", nLevel1="TOTAL",
                    cLevel1="TOTAL"){
  #x <- copy(agg8)
  nace_r2 <- ceparema <- lcode <- code <- value <- orig <- . <- yyyy <- obs_value <- value_FALSE <- NULL
  value_TRUE <- N_FALSE <- N_TRUE <- sumRep <- sumEst <- factorEst <- factorRep <- korrObsV <- NULL
  obs_value.x <- obs_value.y <- v1 <- v2 <- oVal <- geo <- indic_pi <- ty <- obs_status <- NULL
  obs_conf <- obs_comment <- obs_gen <- naINDIC <- NULL
  x00 <- x[nace_r2 %in% nLevel1 & ceparema %in% cLevel1,]
  x00[,lcode:=paste0(substr(code,1,lc[1]),"_",substr(code,lc[2],lc[3]))]
  x01 <- melt(x00,id=c("yyyy","lcode","nace_r2","ceparema","orig"),measure=c("obs_value"))
  x01[,":=" (value=ifelse(value<0 & orig==FALSE,0.0001,value))]
  x01[,":=" (value=ifelse(value<0 & !(substr(lcode,4,6)=="VAD"),0.0001,value))]

  x01[,":=" (orig=ifelse(abs(value)<0.0002,FALSE,orig))]
  x01a <- x01[,lapply(.SD,sum,na.rm=TRUE),by=c("lcode","yyyy","orig"),.SDcols=c("value")]
  x01b <- x01[!is.na(value),.N,by=c("lcode","yyyy","orig")]
  x02 <- merge(x01a,x01b)
  x02a <- dcast(x02,yyyy+lcode~orig,value.var=c("value","N"))
  testAgg <- x[nace_r2 %in% nLevel & ceparema %in% cLevel,]
  testAgg[,lcode:=paste0(substr(code,1,lc[1]),"_",substr(code,lc[2],lc[3]))]
  testAgg <- testAgg[,.(yyyy,lcode,orig,obs_value)]
  setkey(x02a,yyyy,lcode)
  setkey(testAgg,yyyy,lcode)
  testAll <- merge(testAgg,x02a)

  if(ncol(testAll) < 8){
    aa <- testAll[,.(yyyy,lcode)]
    aa[, ":=" (value_TRUE = NA, N_TRUE = NA)]
    testAll <- merge(testAll,aa)
    testAll <- testAll[,.(yyyy, lcode, orig, obs_value, value_FALSE, value_TRUE, N_FALSE, N_TRUE)]
  }
  colnames(testAll) <- c("yyyy","lcode","orig","obs_value","sumEst","sumRep","nEst","nRep")

  # Calculation of Adjustment Factors
  # Assumption: obs_value reported cannot be negative exept for VA (only estimated cells can be negative)
  # Notice: obs_value cannot be NA if orig==TRUE

  testAll[, ":=" (factorEst = NA_real_, factorRep = NA_real_, korrObsV = 0)]
  #testAll[obs_value<0 & orig==FALSE, ":=" (obs_value = 0, korrObsV = 1)]
  #testAll[obs_value<0 & indic_pi %in% c("PROD","EMP","EXP"), ":=" (obs_value = 0, korrObsV = 1)] ##?? restrict to orig=TRUE?

  testAll[orig==FALSE & obs_value<sumRep, ":=" (obs_value = sumRep, korrObsV = 1)]
  testAll[orig==FALSE & !is.na(sumEst) & !(sumEst>0) & sumRep>0, ":=" (obs_value = sumRep, korrObsV = 1)]
  testAll[orig==TRUE & obs_value < 0.0002 & sumRep>0, ":=" (obs_value = sumRep, korrObsV = 1)]
  #testAll[orig==TRUE & obs_value<sumRep, ":=" (obs_value = sumRep, koffObsV = 1)]

  #testAll[orig==TRUE & obs_value==0 & sumEst>0 & !(sumRep>0), ":=" (obs_value = sumEst, korrObsV = 1)] #, orig=FALSE)]
  #testAll[orig==TRUE & obs_value==0 & sumEst>0 & sumRep>0, ":=" (obs_value = sumEst+sumRep, korrObsV = 1)]#, orig=FALSE)]

  # realiter is.na(obs_value) should not be observed (is observed, because naINDIC is na for some countries)
  #testAll[is.na(obs_value) & sumEst >= 0 & sumRep >= 0, ":=" (obs_value = sumEst+sumRep, korrObsV = 1)]
  #testAll[is.na(obs_value) & sumEst >= 0 & sumRep < 0, ":=" (obs_value = sumEst, korrObsV = 1)]
  #testAll[is.na(obs_value) & sumEst < 0 & sumRep >= 0, ":=" (obs_value = sumRep, korrObsV = 1)]
  #testAll[is.na(obs_value) & sumEst < 0 & sumRep < 0, ":=" (obs_value = 0, korrObsV = 1)]
  #testAll[is.na(obs_value) & is.na(sumEst) & sumRep >= 0, ":=" (obs_value = sumRep, korrObsV = 1)]
  #testAll[is.na(obs_value) & is.na(sumEst) & sumRep < 0, ":=" (obs_value = 0, korrObsV = 1)]
  #testAll[is.na(obs_value) & sumEst >= 0 & is.na(sumRep), ":=" (obs_value = sumEst, korrObsV = 1)]
  #testAll[is.na(obs_value) & sumEst < 0 & is.na(sumRep), ":=" (obs_value = 0, korrObsV = 1)]
  #testAll[is.na(obs_value) & is.na(sumEst) & is.na(sumRep), ":=" (obs_value = 0, korrObsV = 1)]

  #################################### obs_value reported ###################################################
  testAll[is.na(sumEst) & is.na(sumRep),  ":=" (factorEst = 0, factorRep = 0)]
  testAll[is.na(sumEst) & sumRep < 0,     ":=" (factorEst = 0, factorRep = 1)] #(factorEst = 0, factorRep = 0)]
  testAll[is.na(sumEst) & sumRep == 0,    ":=" (factorEst = 0, factorRep = 1)]
  testAll[is.na(sumEst) & sumRep > 0,     ":=" (factorEst = 0, factorRep = 1)]  ####
  testAll[is.na(sumEst) & sumRep > 0 & orig==TRUE & obs_value > 0,
                                          ":=" (factorEst = 0, factorRep = obs_value/sumRep)]
  #----------------------------------------------------------------------------------------------------------
  testAll[sumEst == 0 & is.na(sumRep),    ":=" (factorEst = 1, factorRep = 0)]
  testAll[sumEst == 0 & sumRep < 0,       ":=" (factorEst = 1, factorRep = 0)]
  testAll[sumEst == 0 & sumRep == 0,      ":=" (factorEst = 1, factorRep = 1)]
  testAll[sumEst == 0 & sumRep > 0,       ":=" (factorEst = 1, factorRep = 1)]  ####
  testAll[sumEst == 0 & sumRep > 0 & orig == TRUE & obs_value > 0,
                                          ":=" (factorEst = 1, factorRep = obs_value/sumRep)]
  #----------------------------------------------------------------------------------------------------------

  # # 1+2
  # testAll[sum(sumEst, sumRep, na.rm = TRUE) == obs_value, ":=" (factorEst = 1, factorRep = 1)]
  # # 3,4 und 5
  # testAll[sumRep == obs_value & orig == TRUE, ":=" (factorEst = 0, factorRep = 1)]
  # testAll[sumRep == obs_value & orig == FALSE, ":=" (factorEst = 1, factorRep = 1, obs_value = sum(sumRep, sumEst),
  #                                                  korrObsV = 1)]
  # # 6,7 und 8
  # testAll[sumRep > obs_value & orig == TRUE, ":=" (factorEst = 0, factorRep = obs_value/sumRep)]
  # testAll[sumRep > obs_value & orig == FALSE, ":=" (factorEst = 1, factorRep = 1, obs_value = sum(sumRep, sumEst),
  #                                                 korrObsV = 1)]
  # # 9+10
  # testAll[sumRep < obs_value, ":=" (factorEst = (obs_value-sumRep)/sumEst, factorRep = 1)]
  # # 11
  # testAll[sumEst == 0 & sumRep < obs_value & orig == TRUE, ":=" (factorEst = 1, factorRep = obs_value/sumRep)]
  # testAll[sumEst == 0 & sumRep < obs_value & orig == FALSE, ":=" (factorEst = 1, factorRep = 1, obs_value = sumRep,
  #                                                               korrObsV = 1)]
  #testAll[sumEst > 0 & sumRep > 0 & sum(sumEst,sumRep) > obs_value & orig == TRUE,
  #        ":=" (factorEst = 1, factorRep = 1, obs_value = sum(sumRep, sumEst), korrObsV = 1)]

  # eins

  ##########################################################################################################################
  testAll[!(sumEst == 0) & sumRep < 0,    ":=" (factorEst = (obs_value-sumRep)/sumEst, factorRep = 1)]  #factorRep = 0
  testAll[!(sumEst == 0) & sumRep == 0,   ":=" (factorEst = obs_value/sumEst, factorRep = 1)]
  testAll[!(sumEst == 0) & is.na(sumRep), ":=" (factorEst = obs_value/sumEst, factorRep = 0)]
  testAll[!(sumEst == 0) & sumRep > 0 & obs_value > 0 & obs_value >= sumRep,
                                          ":=" (factorEst = (obs_value-sumRep)/sumEst, factorRep = 1)]

  testAll[!(sumEst == 0) & sumRep > 0 & obs_value > 0 & obs_value < sumRep & orig == TRUE,
                                          ":=" (factorEst = 0, factorRep = obs_value/sumRep)]   ## A

  ## possibly -------------------------------------------------------------------------------------------------------
  ##testAll[sumEst > 0 & sumRep > 0 & obs_value > 0 & obs_value < sumRep & orig == TRUE,
  ##        ":=" (factorEst = obs_value/(sumEst+sumRep), factorRep = obs_value/(sumEst+sumRep))]  ## A alternativ
  ##-----------------------------------------------------------------------------------------------------------------

  # ----------------------------------------------------------------------------------------------------------------
  testAll[!(sumEst == 0) & sumRep > 0 & obs_value > 0 & obs_value < sumRep & orig == FALSE,
                                          ":=" (factorEst = 1, factorRep = 1, obs_value = sum(sumRep, sumEst),
                                                korrObsV = 1)] #####
  testAll[sumEst > 0 & sumRep > 0 & obs_value == 0,
                                          ":=" (factorEst = 1, factorRep = 1, obs_value = sum(sumRep, sumEst),
                                                korrObsV = 1)] #####
  testAll[sumEst < 0 & sumRep > 0 & obs_value == 0,
                                          ":=" (factorEst = 0, factorRep = 1)] #####
  #-----------------------------------------------------------------------------------------------------------------


  # Output of maximal Adjustment factors for manual control
  extAdjNA <- testAll[,.(yyyy,lcode,factorEst)]
  extAdjOri <- testAll[,.(yyyy,lcode,factorRep)]

  #cat("\n","\n","Maximal and minimal Adjustment Factors for estimated Values","\n")
  #print(extAdjNA[order(extAdjNA$factorEst,decreasing = TRUE)[1:5]])
  #print(extAdjNA[order(extAdjNA$factorEst,decreasing = FALSE)[1:5]])
  #cat("\n","Maximal Adjustment Factors for reported Values","\n")
  #print(extAdjOri[order(extAdjOri$factorRep,decreasing = TRUE)[1:5]])
  #print(extAdjOri[order(extAdjOri$factorRep,decreasing = FALSE)[1:5]])

  testAll0 <- testAll[,.(yyyy,lcode,factorEst,factorRep)]
  if(is.null(z)){
    testAll1 <- testAll[korrObsV==1,.(yyyy,lcode,oVal=obs_value,v1="TOTAL",v2="TOTAL")]
  } else {
    testAll1 <- testAll[korrObsV==1,.(yyyy,lcode,oVal=obs_value,v1=z[2])]
  }
  setkey(testAll0,yyyy,lcode)
  setkey(x01,yyyy,lcode)
  setkey(testAll1,yyyy,lcode)
  x10 <- merge(x01,testAll0,all.y=TRUE)
  x10[,":=" (obs_value=ifelse(orig==TRUE,value*factorRep,value*factorEst))]
  x10 <- x10[,.(yyyy,lcode,nace_r2,ceparema,obs_value)]

  y <- copy(x)
  y[,":=" (lcode=paste0(substr(code,1,lc[1]),"_",substr(code,lc[2],lc[3])))]
  setkey(x10,yyyy,lcode,nace_r2,ceparema)
  setkey(y,yyyy,lcode,nace_r2,ceparema)
  y <- merge(y,x10,all.x=TRUE)
  if(is.null(z)){
    y[,":=" (obs_value=ifelse(!is.na(obs_value.y),obs_value.y,obs_value.x),v1=nace_r2,v2=ceparema)]
    setkey(y,yyyy,lcode,v1,v2)
    setkey(testAll1,yyyy,lcode,v1,v2)
  } else{
    y[,":=" (obs_value=ifelse(!is.na(obs_value.y),obs_value.y,obs_value.x),v1=get(z[1]))]
    setkey(y,yyyy,lcode,v1)
    setkey(testAll1,yyyy,lcode,v1)
  }

  yy <- merge(y,testAll1,all.x=TRUE)
  yy[,":=" (obs_value=ifelse(is.na(oVal),obs_value,oVal))]
  #y <- y[,.(code,geo,yyyy,indic_pi,nace_r2,ceparema,ty,unit,obs_value,obs_status,obs_conf,obs_comment,obs_gen,orig,naINDIC)]
  yy <- yy[,.(code,geo,yyyy,indic_pi,nace_r2,ceparema,ty,unit,obs_value,obs_status,obs_conf,obs_comment,obs_gen,orig,naINDIC)]
  yy[, ":=" (obs_value=ifelse(obs_value==0,0.0001,obs_value))]
  return(yy[])
}
