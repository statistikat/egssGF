#' Screening of EGSS-data
#'
#' Visualisation of the share of missing data in the EGSS data matrix
#'
#'
#' @param x EGSS-data table created from flat file and/or adapted with various gap-filling functions.
#' @param g Country code.
#' @param per Year to be plotted (default per = "ALL").
#' @param v Variable to plot. Possible values are PROD, VA, EXP and EMP.
#' @param n2 Nace-code: Possible values A, B, C, C10-C12, C13-C15,C16-C18, C19, C20,
#'          C21, C22_C23, C24_C25, C26,C27, C28, C29_C30, C31-C33, D, E, E36,
#'          E37, E38,E39, F, G, H, I, J, K, L, M, M69_M70, M71,M72,
#'          M73-M75, N, O, P, Q, R, S, T, U,TOTAL,
#' @param cep Ceparema-code. Possible values are: CEPA1, CEPA112_122, CEPA2, CEPA3,
#'                       CEPA4, CEPA5, CEPA6, CEPA7, CEPA7-9, CEPA8, CEPA812,
#'                       CEPA9, CREMA10, CREMA11, CREMA11A, CREMA11B, CREMA12,
#'                       CREMA12_15_16, CREMA13, CREMA13A, CREMA13B, CREMA13C,
#'                       CREMA14, CREMA15, CREMA15A, CREMA16, TOT_CEPA, TOT_CREMA,
#'                       TOTAL
#' @param t1 Ty-variable to be displayed. Possible values are ANC, C_REP, ES_CS, MKT,
#'           NMKT, OWN_USE, REST, TOT_EGSS
#' @param p Kind of plot. Possible values are 1 - Country x Year; 2 - Country x Nace; 3 - Country x Ceparema;
#'          4 - Nace x Ceparema
#' @param val Visualisation of percentage of non-missing values (default - val=1) or percentage of values>0 (val=0)
#' @param ia Interactive plot (default ia=FALSE)
#'
#' @return Summery Tables, Graphs
#'
#' @examples
#' data <- loadEGSS(dat_egssBas)
#' screenData(data)
#' screenData(data,per=2015,p=2)
#' screenData(data,per=2015,p=3,t1="TOT_EGSS")
#' screenData(data,per=2015,g="AT",p=4)
#' screenData(data,per=2015,g=c("AT","BE"),p=4)
#'
#' @import data.table
#'         ggplot2
#' @importFrom plotly ggplotly
#' @export
screenData <- function(x,g=c("AT", "BE", "BG", "CY", "CZ", "DE","DK", "EE", "EL", "ES", "FI",
                             "FR", "HR", "HU", "IE", "IT", "LT","LU", "LV", "MT"," NL", "PL",
                             "PT", "RO", "SE", "SI", "SK", "UK"),
                         v=c("PROD","VA","EXP","EMP"),
                         per="ALL",
                         n2=c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M",
                              "N", "O", "P", "Q", "R", "S", "T","U"),
                         cep=c("CEPA1", "CEPA2", "CEPA3", "CEPA4", "CEPA5", "CEPA6", "CEPA7",
                               "CEPA8", "CEPA9", "CREMA10", "CREMA11", "CREMA12", "CREMA13A",
                               "CREMA13B", "CREMA13C", "CREMA14", "CREMA15", "CREMA16"),
                         t1=c("ANC", "MKT", "NMKT", "OWN_USE"),
                         p=1,val=1,ia=FALSE){
  indic_pi <- yyyy <- geo <- ty <- nace_r2 <- ceparema <- obs_value <- V1 <- NULL
  # prepare input variables ----------------------------
  if(val==1){
    col <- "deepskyblue4"
  } else col <- "olivedrab"
  if(per=="ALL"){per <- unique(x$yyyy)}
  #-----------------------------------------------------

  if(p==1){
  # Country x Years
  xCode <- "Year"
  yCode <- "Country"
  byCode <- paste0(yCode," & ",xCode)
  size <- nrow(x[indic_pi %in% v & yyyy==2015 & geo=="AT" & ty %in% t1 & nace_r2 %in% n2 &
                    ceparema %in% cep,])
  if(val==1){
    y <- x[geo %in% g & indic_pi %in% v & yyyy %in% per & ty %in% t1 & nace_r2 %in% n2 & ceparema %in% cep,
              sum(!is.na(obs_value))/size, by = c("geo","yyyy")]
  } else{
    y <- x[geo %in% g & indic_pi %in% v & yyyy %in% per & ty %in% t1 & nace_r2 %in% n2 & ceparema %in% cep,
              sum(obs_value>0,na.rm=TRUE)/size, by = c("geo","yyyy")]
  }
  p <- ggplot(y, aes(yyyy, geo)) + geom_tile(aes(fill = V1),colour = "white") +
    scale_fill_gradient(name="Share",low = "white",high = col, limits=c(0,1),guide="legend") +
    xlab(xCode) +ylab(yCode) +
    labs(title=paste0("Completed Cells by ",byCode),
         subtitle=paste0("VARIABLE: ",tolower(paste0(v,collapse=","))," - TYPE: ",tolower(paste0(t1,collapse=",")),"   --N: ",size),
         caption=paste0("NACE: ",tolower(paste0(n2,collapse=","))," - CEPA: ",tolower(paste0(cep,collapse=",")))) +
    theme_minimal()
  } else if(p==2){
  # Country x Nac2
  xCode <- "Nace-Code"
  yCode <- "Country"
  byCode <- paste0(yCode," & ",xCode)
  size <- nrow(x[indic_pi %in% v & yyyy %in% per & geo=="AT" & nace_r2=="A" & ceparema %in% cep &
                      ty %in% t1,])
  if(val==1){
    y <- x[geo %in% g & indic_pi %in% v & yyyy %in% per & nace_r2 %in% n2 & ty %in% t1 & ceparema %in% cep,
            sum(!is.na(obs_value))/size, by = c("geo","nace_r2")]
  } else{
    y <- x[geo %in% g & indic_pi %in% v & yyyy %in% per & nace_r2 %in% n2 & ty %in% t1 & ceparema %in% cep,
              sum(obs_value>0,na.rm=TRUE)/size, by = c("geo","nace_r2")]
  }
  p <- ggplot(y, aes(nace_r2, geo)) + geom_tile(aes(fill = V1),colour = "white") +
    scale_fill_gradient(name="Share",low = "white",high = col, limits=c(0,1),guide="legend") +
    xlab(xCode) +ylab(yCode) +
    labs(title=paste0("Completed Cells by ",byCode),
         subtitle=paste0("VARIABLE: ",tolower(paste0(v,collapse=","))," - TYPE: ",tolower(paste0(t1,collapse=",")),"     N: ",size),
         caption=paste0("YEAR: ",tolower(paste0(per,collapse=","))," - CEPA: ",tolower(paste0(cep,collapse=",")))) +
    theme_minimal()
  } else if(p==3){
  # Country x Ceparema
  xCode <- "Ceparema"
  yCode <- "Country"
  byCode <- paste0(yCode," & ",xCode)
  size <- nrow(x[indic_pi %in% v & yyyy %in% per & geo=="AT" & ceparema=="CEPA1" & nace_r2 %in% n2 &
                      ty %in% t1,])
  if(val==1){
    y <- x[geo %in% g & indic_pi %in% v & yyyy %in% per & nace_r2 %in% n2 & ty %in% t1 & ceparema %in% cep,
              sum(!is.na(obs_value))/size, by = c("geo","ceparema")]
  } else{
    y <- x[geo %in% g & indic_pi %in% v & yyyy %in% per & nace_r2 %in% n2 & ty %in% t1 & ceparema %in% cep,
              sum(obs_value>0,na.rm=TRUE)/size, by = c("geo","ceparema")]
  }
  p <- ggplot(y, aes(ceparema, geo)) + geom_tile(aes(fill = V1),colour = "white") +
    scale_fill_gradient(name="Share",low = "white",high = col, limits=c(0,1),guide="legend") +
    xlab(xCode) +ylab(yCode) +
    labs(title=paste0("Completed Cells by ",byCode),
         subtitle=paste0("VARIABLE: ",tolower(paste0(v,collapse=","))," - TYPE: ",tolower(paste0(t1,collapse=",")),"     N: ",size),
         caption=paste0("YEAR: ",tolower(paste0(per,collapse=","))," - NACE: ",tolower(paste0(n2,collapse=",")))) +
    theme_minimal()
  } else if(p==4){
  # Nace x Ceparema
  xCode <- "Ceparema"
  yCode <- "Nace"
  byCode <- paste0(yCode," & ",xCode)
  size <- nrow(x[indic_pi %in% v & yyyy %in% per & geo %in% g & ceparema=="CEPA1" & nace_r2 == "A" &
                      ty %in% t1,])
  if(val==1){
    y <- x[geo %in% g & indic_pi %in% v & yyyy %in% per & nace_r2 %in% n2 & ty %in% t1 & ceparema %in% cep,
              sum(!is.na(obs_value))/size, by = c("nace_r2","ceparema")]
  } else{
    y <- x[geo %in% g & indic_pi %in% v & yyyy %in% per & nace_r2 %in% n2 & ty %in% t1 & ceparema %in% cep,
              sum(obs_value>0,na.rm=TRUE)/size, by = c("nace_r2","ceparema")]
  }
  p <- ggplot(y, aes(ceparema, nace_r2)) + geom_tile(aes(fill = V1),colour = "white") +
    scale_fill_gradient(name="Share",low = "white",high = col, limits=c(0,1),guide="legend") +
    xlab(xCode) +ylab(yCode) +
    labs(title=paste0("Completed Cells by ",byCode),
         subtitle=paste0("VARIABLE: ",tolower(paste0(v,collapse=","))," - TYPE: ",tolower(paste0(t1,collapse=",")),"     N: ",size),
         caption=paste0("YEAR: ",tolower(paste0(per,collapse=","))," - COUNTRY: ",tolower(paste0(g,collapse=",")))) +
    theme_minimal()
  } else warning("No Plot selected!")
  if(ia){
    ggplotly(p)
  } else print(p)
}
