#' Load and Compile National Accounts Data. Merge NA-Data with EGSS-data
#'
#' Loads Data from National Accounts for all 28 EU-Countries and the Variables 
#' Production, Value Added, Exports and Hours worked (instead of Empoyees - no 
#' full-time equivalents are available). Variables with values in national currency
#' are transformed to Euro values (conversion rates have to be delivered as input
#' variable z). In a second step the national account data set is merged with the 
#' EGSS-data set produced with loadEGSS. For all time series a relative observation
#' value is calculated relative to the "nearest" national account aggregate which 
#' is continuously greater than zero for the whole time series.
#'
#' An identification code is created in order to simlify the access to different 
#' aggregation levels. This code is a 17 digit character variable composed of 
#' (Example: AT_EMP_A00_00_000) :
#'
#' 1. two digits for country code followed by underscore (e.g. AT_,BE_,...)
#' 2. three digits for variable (e.g. EMP_, OUT_,...)
#' 3. three digits for NACE-level A00-U00: (e.g. A00_, E00_, E36_,...)
#' 4. two digits for type:
#'   + 10_: anciliary
#'   + 20_: own use
#'   + 30_: non market
#'   + 40_: market
#'   + 41_: market ressource efficient
#'   + 42_: market environmental specific services
#'   + 43_: market rest category
#' 5. three digits for Ceparema code
#'   + 110-190: Environmental Protection Cepa 1-6
#'   + 200-260: Ressource Management CrEMa 10-16 -- with subcategories 111 and 112 
#'              (without 110) and 131,132,133 (without 130)
#'
#' @param x National Accounts File for Variable Production, Exports, Value added 
#'          and Hours worked as R data table
#' @param y EGSS-Data set created with Function loadEGSS()
#' @param z Currency transformation rates
#' @param toEst Year to be predicted
#' @param t1 Type variable for which the analysis should be performed. Possible 
#'           values are TOT_EGSS (default), MKT, NMKT, ANC, C_REP, ES_CS, OWN_USE 
#'           and REST
#' @return Complete EGSS-Data Matrix enriched with National Account Data and 
#'         respective ratios of EGSS-Vaues to National Account Data
#'
#' @examples
#' data("dat_egssBas")
#' data("natAcc")
#' datEgss <- loadEGSS(x = dat_egssBas)
#' loadNA(x = natAcc, y = datEgss, toEst = 2016, t1 = "TOT_EGSS")
#' @import data.table
#' @export
loadNA <- function(x, y, z, toEst, t1 = "TOT_EGSS"){
  time <- nace_r2 <- value <- yyyy <- indic_pi <- geo <- nace1 <- ceparema <- z0 <- ty <- z1 <- z2 <- NULL
  z3 <- code <- naBASE <- na1DIG <- naINDIC <- naGEO <- rev_val <- naINDIC_av <- na1DIG_av <- naBASE_av <- NULL
  obs_value <- . <- obs_value_rel <- obs_status <- obs_conf <- obs_comment <- obs_gen <- orig <- lcode <- NULL
  x <- x[time <= toEst,]
  
  
  # Conversion of national currency to Euro
  setkey(x, geo, time)
  setkey(z, geo, time)
  x0 <- merge(x, z, all.x = TRUE)
  x0[indic_pi == "EMP", rate := 1]
  x0[, ":=" (value = ifelse(is.na(rate), value, value/rate), yyyy = time, time = NULL)]
  #--------------------------------------------------------------------------

  # distribute the National Accounts Aggregate E37-E39 to each E37, E38 and E39
  n37 <- x0[nace_r2=="E37-E39",]
  n37[,":=" (nace_r2="E37")]
  n38 <- x0[nace_r2=="E37-E39",]
  n38[,":=" (nace_r2="E38")]
  n39 <- x0[nace_r2=="E37-E39",]
  n39[,":=" (nace_r2="E39")]
  x0 <- x0[!nace_r2=="E37-E39",]
  x0 <- do.call("rbind", list(x0, n37, n38, n39))
  #----------------------------------------------------------------------------

  # Nace Totals for each Variable (PROD,VA,EMP and EXP) and Year
    #xTot <- x[,lapply(.SD,sum,na.rm=TRUE),by=c("time","geo"),.SDcols="value"]
    #xTot[,":=" (valueTOT=value,value=NULL)]
    xTot <- x0[,lapply(.SD,sum,na.rm=TRUE),by=c("yyyy","indic_pi"),.SDcols="value"]
    xTot[,":=" (naGEO=value,value=NULL)]
  # Nace Totals for each Country, Year and Variable
    xVar <- x0[nace_r2=="TOTAL",]
    xVar[,":=" (nace_r2=NULL,naINDIC=value,value=NULL)]
  # Nace-Sections (one-digit) for each Country, Year and Variable
    xNac1 <- x0[nchar(nace_r2)==1,]
    xNac1[,":=" (nace1=nace_r2,nace_r2=NULL,na1DIG=value,value=NULL)]

  # EGSS data is enriched with value added for Total (per Year and Country) 
  # Total Nace and Nace-1Digit
  y <- y[yyyy>2009 & yyyy<=toEst,]

  #setkey(y,geo,time)
  #setkey(xTot,geo,time)
  setkey(y,indic_pi,yyyy)
  setkey(xTot,indic_pi,yyyy)
  y0 <- merge(y,xTot,all.x=TRUE)

  setkey(y0,geo,yyyy,indic_pi)
  setkey(xVar,geo,yyyy,indic_pi)
  y1 <- merge(y0,xVar,all.x=TRUE)

  y1[,":=" (nace1=ifelse(nace_r2=="TOTAL",NA,substr(nace_r2,1,1)))]
  setkey(y1,geo,yyyy,indic_pi,nace1)
  setkey(xNac1,geo,yyyy,indic_pi,nace1)
  y2 <- merge(y1,xNac1,all.x=TRUE)

  setkey(y2,geo,yyyy,indic_pi,nace_r2)
  setkey(x0,geo,yyyy,indic_pi,nace_r2)
  y3 <- merge(y2,x0,all.x=TRUE)
  y3[,":=" (naBASE=value, value=NULL)]

  # Attention! - Ceparema codes CEPA112_122, CEPA812 and CREMA15A are deleted here!
  y3 <- y3[!ceparema %in% c("CEPA112_122", "CEPA812", "CREMA15A"),]

  # Creation of a code variable to identify the data-cells ........................... Coding (1)
  y3[,":=" (z0 = NA_character_, z1 = NA_character_, z2 = NA_character_, z3 = NA_character_)]
  y3[indic_pi == "EMP", z0 := "EMP"]
  y3[indic_pi == "EXP", z0 := "EXP"]
  y3[indic_pi == "VA", z0 := "VAD"]
  y3[indic_pi == "PROD", z0 := "PRO"]
  y3[ty == "TOT_EGSS", z1 := "00"]
  y3[ty == "ANC", z1 := "10"]
  y3[ty == "NMKT", z1 := "20"]
  y3[ty == "OWN_USE", z1 := "30"]
  y3[ty == "MKT", z1 := "40"]
  y3[ty == "ES_CS", z1 := "41"]
  y3[ty == "C_REP", z1 := "42"]
  y3[ty == "REST", z1 := "43"]
  y3[ceparema =="CEPA1", z2 := "110"]
  y3[ceparema =="CEPA2", z2 := "120"]
  y3[ceparema =="CEPA3", z2 := "130"]
  y3[ceparema =="CEPA4", z2 := "140"]
  y3[ceparema =="CEPA5", z2 := "150"]
  y3[ceparema =="CEPA6", z2 := "160"]
  y3[ceparema =="CEPA7", z2 := "170"]
  y3[ceparema =="CEPA7-9", z2 := "179"]
  y3[ceparema =="CEPA8", z2 := "180"]
  y3[ceparema =="CEPA9", z2 := "190"]
  y3[ceparema =="TOT_CEPA", z2 := "100"]
  y3[ceparema =="CREMA10", z2 := "201"]
  y3[ceparema =="CREMA11", z2 := "210"]
  y3[ceparema =="CREMA11A", z2 := "211"]
  y3[ceparema =="CREMA11B", z2 := "212"]
  y3[ceparema =="CREMA12", z2 := "220"]
  y3[ceparema =="CREMA13", z2 := "230"]
  y3[ceparema =="CREMA13A", z2 := "231"]
  y3[ceparema =="CREMA13B", z2 := "232"]
  y3[ceparema =="CREMA13C", z2 := "233"]
  y3[ceparema =="CREMA14", z2 := "240"]
  y3[ceparema =="CREMA15", z2 := "250"]
  y3[ceparema =="CREMA16", z2 := "260"]
  y3[ceparema =="CREMA12_15_16", z2 := "270"]
  y3[ceparema =="TOT_CREMA", z2 := "200"]
  y3[ceparema =="TOTAL", z2 := "000"]

  y3[,z3 := ifelse(nchar(nace_r2) == 1, paste0(nace_r2, "00"), substr(nace_r2, 1, 3))]
  y3[,":=" (code = paste0(geo, "_", z0, "_", z3, "_", z1, "_", z2), z0 = NULL,
            z1 = NULL, z2 = NULL, z3 = NULL)]
  y3[,":=" (lcode = substr(code, 1, 6))]
  # ........................................................................ End Coding (1)

  # Calculation of relative observation values: relative observation values for 
  # each code are based on the lowest continuous (for all years) available aggregation 
  # level for that very code from national account data.
  naRef <- y3[, lapply(.SD,function(x){
     #all(!is.na(x))
     all(x>0)
    }), by=c("code"),.SDcols=c("naBASE", "na1DIG", "naINDIC", "naGEO")]
  naRef[,":=" (naBASE_av = naBASE, na1DIG_av = na1DIG, naINDIC_av = naINDIC,
               naGEO_av = naGEO, naBASE = NULL, na1DIG = NULL, naINDIC = NULL,
               naGEO = NULL),]
  setkey(naRef, code)
  setkey(y3, code)
  y3a <- merge(y3, naRef, all.x = TRUE)
  y3a[, rev_val := naGEO]
  y3a[naINDIC_av == TRUE, rev_val := naINDIC]
  y3a[na1DIG_av == TRUE, rev_val := na1DIG]
  y3a[naBASE_av == TRUE, rev_val := naBASE]
  y3a[,":=" (obs_value_rel = ifelse(rev_val > 0, obs_value/rev_val, NA))]

  y4 <- y3a[nace_r2 %in% c(LETTERS[1:21], "TOTAL") & ty == t1 & 
              ceparema %in% c("CEPA1", "CEPA2", "CEPA3", "CEPA4", "CEPA5", "CEPA6",
                              "CEPA7-9", "CREMA10", "CREMA11", "CREMA12_15_16",
                              "CREMA13A", "CREMA13B", "CREMA13C", "CREMA14",
                              "TOT_CEPA", "TOT_CREMA", "TOTAL"),]
  y4 <- y4[,.(code, geo, yyyy, indic_pi, nace_r2, ceparema, ty, unit, obs_value,
              rev_val, obs_value_rel, obs_status, obs_conf, obs_comment, obs_gen,
              orig, lcode, naINDIC)]
  return(y4[])
  }
