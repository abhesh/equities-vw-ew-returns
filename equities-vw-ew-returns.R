#This code constructs value-weighted and equal-weighted market returns based on monthy stock return data from January 1926 to December 2019
library(data.table)
library(readr)
#datafile.csv contains timeseries data from January 1926 to December 2019 of monthy stock return data
CRSP_stocks <-  read_csv('datafile.csv', 
                         col_types = cols(
                           PERMNO = col_integer(),
                           date = col_date("%Y%m%d"),
                           SHRCD = col_integer(),
                           EXCHCD = col_integer(),
                           PERMCO = col_integer(),
                           DLRETX = col_character(),
                           DLRET = col_character(),
                           PRC = col_double(),
                           RET = col_character(),
                           SHROUT = col_integer(),
                           RETX = col_character(),
                           vwretd = col_double(),
                           vwretx = col_double(),
                           ewretd = col_double(),
                           ewretx = col_double()
                         ))
CRSP_stocks <- data.table(CRSP_stocks)
FF_mkt <-  read_csv('FFF.csv',
                    col_names = c("yrMon","Market_minus_RF", "SMB","HML", "Rf"), skip = 4)
FF_mkt <- data.table(FF_mkt)

VW_EW_Ret <- function (CRSP_stocks) {
  
  
  #Clean and Prepare Data
  
  CRSP <- copy(CRSP_stocks[(EXCHCD == 1 | EXCHCD == 2 | EXCHCD == 3) & (SHRCD == 10 | SHRCD == 11)])
  CRSP[ , Year := year(date)]
  CRSP[ , Month := month(date)]
  CRSP[ , YrMo := Year*12 + Month]
  
  for (i in c('RET', 'DLRET')) {
    CRSP[ , paste0(i) := as.character(get(i))]
    CRSP[ get(i) %in% c('','A','C','P','S','T'), paste0(i) := NA]
    CRSP[ , paste0(i) := as.numeric(get(i))]
    CRSP[get(i) %in% c(-66, -77, -88, -99), paste0(i) := NA]
  }
  
  #Total Returns 
  CRSP[is.na(DLRET) & !is.na(RET), Return := RET]
  CRSP[!is.na(DLRET) & is.na(RET), Return := DLRET]
  CRSP[!is.na(DLRET) & !is.na(RET), Return := (1 + RET)*(1 + DLRET) - 1]
  
  #Market Cap
  CRSP[, Mkt_Cap := abs(PRC)*SHROUT]
  setorder(CRSP,PERMNO, date )
  CRSP[ , lag_Mkt_Cap := shift(Mkt_Cap), by = PERMNO]
  
  #Limit Data to that with full availability as on French's website
  CRSP[ , prev_YrMo := shift(YrMo), by = PERMNO]
  CRSP[ , Valid_lag := YrMo == (prev_YrMo + 1)]
  CRSP = CRSP[Valid_lag == T & !is.na(lag_Mkt_Cap & !is.na(Return))]
  
  #Value Weighting
  CRSP[, Value_weight := lag_Mkt_Cap/sum(lag_Mkt_Cap, na.rm = T), by = date]
  
  #Calculate output variables
  CRSP[, Stock_lag_MV := sum(lag_Mkt_Cap, na.rm = T), by = date]
  CRSP[, Stock_lag_MV := Stock_lag_MV/1000]
  CRSP[, Stock_Ew_Ret := mean(Return, na.rm = T), by = date]
  CRSP[, Stock_Vw_Ret := sum(Return*Value_weight, na.rm = T), by = date]
  
  # Output
  CRSP = unique(CRSP[((Year == 1926 & Month >=1)| (Year > 1926 & Year <= 2019)) & !is.na(Stock_Vw_Ret) & !is.na(Stock_lag_MV) & !is.na(Stock_Ew_Ret), .(Year, Month, Stock_lag_MV, Stock_Ew_Ret, Stock_Vw_Ret)], by = c('Year', 'Month'))
  setorder(CRSP, Year, Month)
  CRSP
  
}

Monthly_CRSP_Stocks <-  VW_EW_Ret(CRSP_stocks)
