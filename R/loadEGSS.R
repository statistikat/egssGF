#' Load and Compile EGSS-data
#'
#' Loads EGSS Base File, completes and harmonises data file over periods.
#'
#' 1. Conversion of national currency values to Euro values
#' 2. Input File has to be available in the "MDT"-flat format
#' 3. Standardisation of "ty"-Variable:
#'       * create structure for "ty" according to requirements 2013 (levels: MKT, C_REP and ES_CS)
#'       * add a Rest Category: Rest=MKT-C_REP-ES_CS
#'       * transform data structure for periods before 2013 to this very structure, i.e.
#'              + C_REP=GDA+TCI (if both available "old" C_REP to delete)
#'              + Rest=GDC+TCE
#'       * Treatment of NAs (e.g. GDA=200, TCI=NA; Sum=200 or NA)
#' 4. Transfer "Basic Structure" to all Countries and Years (34.916 rows)
#'      *  4 variables (Production,Value Added,Employment and Exports)
#'      * 43 nace-codes (Nace-2digit levels, Nace-divisions and Totals)
#'      * 29 Ceparema codes (with aggregates)
#'      * 8 ty-codes and 4 ty-codes respectively (indic_pi="EXP")
#' 5. Cells with Zeros and obs_status="L" (not applicable) are set to NA
#' 6. Creation of a variable **orig** indicating if the observation value 
#' (obs_value) has been reported (orig=TRUE) This means that obs_value is 
#' other than missing. (all zeroes are taken as reported). If the variable 
#' obs_value is missing than orig is set to FALSE.
#'
#' @param x R Data Table (EGSS Basis File in Flat Format)
#' @param y Currency transformation rates
#' @return Completed EGSS-Data Matrix
#'
#' @examples loadEGSS(x = dat_egssBas, y = currency)
#' @import data.table
#' @export
loadEGSS <- function(x, y) {
  ty <- . <- obs_value <- geo <- time <- nace_r2 <- ceparema <- indic_pi <- NULL
  obs_status <- obs_conf <- obs_comment <- obs_gen <- yyyy <- unit.x <- NULL
  obs_value.x <- obs_value.y <- obs_status.x <- obs_conf.x <- obs_comment.x <- NULL
  orig <- rate <- NULL

  # Conversion of national currency to Euro
  setkey(x, geo, time)
  setkey(y, geo, time)
  x0 <- merge(x, y, all.x = TRUE)
  x0[indic_pi == "EMP", rate := 1]
  x0[, ":=" (obs_value = ifelse(is.na(rate), obs_value, obs_value / rate))]
  #-----------------------------------------------------------------------------
  # Completion of Dataset                                                                          Start(1)
  #--------------------------------------------------------------------------------------------------------
  # (1)  Transformation of Levels "GDA" and "TCI" to "C_REP" - is NA if one of the two levels=NA.
  #      "obs_comment" is set to "generated".

  aa <- x0[any(ty %in% c("GDA","TCI")),.(obs_value=obs_value[ty == "GDA"] + obs_value[ty == "TCI"]),
            by=.(geo, time, nace_r2, ceparema, indic_pi)]
  aa[,":=" (ty="C_REP",unit="",obs_status="",obs_conf="",obs_comment="generated",obs_gen="harmon")]
  aa <- aa[,.(nace_r2,ceparema,indic_pi,ty,unit,geo,time,obs_value,obs_status,obs_conf,obs_comment,obs_gen)]

  setkey(aa,geo,time,nace_r2,ceparema,indic_pi,ty)
  setkey(x0,geo,time,nace_r2,ceparema,indic_pi,ty)
  dat <- merge(x0,aa,all.x=TRUE)
  dat[,":=" (yyyy = time, time = NULL)]
  dat <- dat[,.(geo,yyyy,nace_r2,ceparema,indic_pi,ty,unit=unit.x,obs_value=ifelse(is.na(obs_value.x),obs_value.y,obs_value.x),
                obs_status=obs_status.x,obs_conf=obs_conf.x,obs_comment=obs_comment.x,obs_gen)]

  # (2)  Generation of a Rest-Category for Variable "ty", depending on the occurence of Levels "C_REP" or "GDC" and
  #      "TCE" (in a perfect world the Rest-Category would be: Rest=GDC+TCE from start to 2012 and
  #       Rest=MKT-ES_CS-C_REP after 2012). Realiter it is first checked if ty="C_REP" appears in the relevant
  #       Category and the Rest is calculated depending on that.
  aa0 <- dat[,.(obs_value=ifelse(any(ty=="C_REP"),obs_value[ty=="MKT"]-sum(obs_value[ty %in% c("ES_CS","C_REP")]),
                                 obs_value[ty=="GDC"]+obs_value[ty=="TCE"])),
             by=.(geo,yyyy,nace_r2,ceparema,indic_pi)]
  aa0[,":=" (ty="REST",unit="",obs_status="",obs_conf="",obs_comment="generated",obs_gen="harmon")]
  aa0 <- aa0[,.(geo,yyyy,nace_r2,ceparema,indic_pi,ty,unit,obs_value,obs_status,obs_conf,obs_comment,obs_gen)]
  dat1 <- rbind(dat,aa0)

  # (3)  Deletion of all "old" ty-Levels (used from the beginning of the time series until 2012, incidentially also
  #      for years 2013 and beyond)
  dat1 <- dat1[!(ty %in% c("GDA","GDC","TCI","TCE")),]

  # Create Base-Structure for all countries and times ....................................

  base_struc0 <- expand.grid(geo=unique(x$geo),yyyy=unique(x$time),indic_pi=unique(x$indic_pi)[c(1,3,4)],
                             nace_r2=unique(x$nace_r2),ceparema=unique(x$ceparema),
                             ty=c("ANC","C_REP","ES_CS","MKT","NMKT","OWN_USE","REST","TOT_EGSS"))
  base_struc1 <- expand.grid(geo=unique(x$geo),yyyy=unique(x$time),indic_pi="EXP",nace_r2=unique(x$nace_r2),
                             ceparema=unique(x$ceparema),
                             ty=c("C_REP","ES_CS","REST","TOT_EGSS"))
  base_struc <- as.data.table(rbind(base_struc0,base_struc1))

  setkey(dat1,geo,yyyy,indic_pi,nace_r2,ceparema,ty)
  setkey(base_struc,geo,yyyy,indic_pi,nace_r2,ceparema,ty)
  dat2 <- merge(dat1,base_struc,all.y=TRUE)

  # Cells with Zeros and obs_status="L" (not applicable) are set to NA
  dat2[obs_value==0 & obs_status=="L",obs_value:=NA]
  dat2[,":=" (orig=FALSE)]
  dat2[!is.na(obs_value),orig:=TRUE]
  # Calculate proportion of actual value to top aggregate (per country and variable) for entire data-set

  ####################################################################################
  return(dat2[])
}
