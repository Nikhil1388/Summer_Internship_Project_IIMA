##------------------------------BEGINNING OF CODE----------------------------##
rm(list = ls())
gc()

# LOADING REQUIRED LIBRARIES:
library(data.table)
library(readxl)
library(magrittr)
library(zoo)
library(zeallot)
library(tidyfast)

# FUNCTIONS:
lag=function(x){if(length(x)==1){NA}else{c(NA,x[1:length(x)-1])}}
sum_avg = function(df) {
  df[, mcap:= close*outshares/1e9]
  c(sum(df$mcap, na.rm=T),mean(df$mcap,na.rm=T))
}
len_sum_avg <- function(x) list(length = length(x), sum.na = sum(x,na.rm=T), avg = mean(x, na.rm=T))
length_breakpoint <- function(x) list(length = length(x), breakpoints = quantile(x, seq(0.1, 0.9, 0.1), na.rm = T))
momentum=function(x){
  if(length(x)>-momentumj_start){c(rep(NA,-momentumj_start),apply(matrix(embed(x,-momentumj_start+1)[,(-momentumj_end+1):(-momentumj_start+1)],nrow=(length(x)+momentumj_start)),1,FUN=mean,na.rm=T))}else{c(rep(NA,length(x)))}
}
len.na<- function(x) sum(!is.na(x))
value_breakpoints = function(x) (c(quantile(x, c(value_bp1,value_bp2),na.rm=T), sum(!is.na(x))))
lag.sum=function(x,y)(if(length(x)>y){c(rep(NA,y),apply(matrix(embed(x,1+y)[,2:(1+y)],ncol=y),1,sum.na))}else{c(rep(NA,length(x)))}) 
sum.na = function(x) (sum(x,na.rm=T))

#Assigning INITIAL VARIABLES which need to be updated as per the period of the market data
c(startyear, endyear, start_cycle_year, update_start, update_end, ff_results_start, cutoffmonthforvanishing, toggle100pc, toggle_finance) %<-% c(1991,2021,1992,19910101,20211231,19930101,202012,1,1)

# Section 1 : ASSUMPTIONS LIST
{
  size_bp=0.9  # size break-point top-10 percentiles BIG, remaining SMALL
value_bp1=0.3 # value break-point 30-th percentile (GROWTH)
value_bp2=0.7 # hml breakpoint 70-th percentile (NEUTRAL) and above 70-th percentile is (VALUE)
return_bp1=0.3 # break-points for returns - 30-th percentile and below (LOSERS)
return_bp2=0.7 # break-points for returns - above 70-th percentile (WINNERS)
minportnumber=5 # Minimum number of firms that should be there in any size-value portfolio or size-momentum portfolio for consideration in the Four-factor calculations

# criteria for booking capital loss on an asset (price < 50% of face value) 
pricetofacevalue=0.5 # maximum value of the ratio of price to face value for recognizing -100% loss of capital

# Momentum return period     

momentumj_start=-11
momentumj_end=-1  

bvps_cutoffmonth = 3 # march bv is used for portfolio creation
size_cutoffmonth = 9 # september size is used for portfolio creation
op_cutoffmonth = 3 # Operating Profitability calculated using financial statements at fiscal year end(i.e. March mostly) is used for portfolio creation
inv_cutoffmonth = 3 # Investing calculated using financial statements at fiscal year end(i.e. March mostly) is used for portfolio creation
trdaysfilter=50  #minimum number of trading days required for firm to be included in the sample
}

# Section 2 : LOADING AND PREPROCESSING INPUT FILES
{
  setwd("D:/4-FactorModel/ResearchProject/Inputs")
write <- "D:/4-FactorModel/ResearchProject/Outputs/"

# Fast Reading of Large input data using fread function from data.table library
fread("Stockdata.txt", colClasses = c("numeric","character",rep("numeric",8)), header = TRUE, data.table = TRUE) -> stockdata
colnames(stockdata) <- c("cono","coname","date","close","bse_returns","bse_bvps","nse_bvps","outshares","facevalue","bookvalue")

stockdata[,c(1:5,8)] %>% .[order(cono,date)]  -> daily_data

unique(daily_data, by="cono") %>% .[,.N] -> cono1
daily_data[!is.na(close) & outshares>0 & close>=0]-> daily_data

unique(daily_data, by="cono") %>% .[,.N] -> cono2

read_excel("Sensex Close Price.xlsx", col_types = c("date","numeric"), skip = 5) %>% as.data.table -> sensex
colnames(sensex) <- c("date","sensex")
sensex[, date:= year(date)*1e4+month(date)*1e2+mday(date)]

fread("industry.txt", sep = "|", data.table = TRUE) -> industry
industry[, 4:=NULL]
colnames(industry) <- c("cono","coname","nic")

# Classifying firms as financial and non-financial firms
industry[, industry_dummy:= ifelse(substr(nic,1,2) %in% 64:66, 1,0)] %>% .[,c("cono","industry_dummy")] -> industry

# Fetching relevant monthly Book Value per share and Facevalue data
stockdata %>% .[, yearmonth:=date%/%100] %>% .[, last_day_month:= max(date), by=list(cono,yearmonth)] %>% .[date==last_day_month, c("cono","date","yearmonth","bse_bvps","nse_bvps","facevalue")] %>% .[, bvps:= ifelse(is.na(bse_bvps) & !is.na(nse_bvps), nse_bvps, bse_bvps)] %>% .[,c("bse_bvps","nse_bvps"):=NULL] -> bvps_facevalue

rm(stockdata)

bvps_facevalue[,c("cono","date","bvps")] -> bvps
bvps_facevalue[,c("cono","yearmonth","facevalue")] -> facevalue

rm(bvps_facevalue)

# Loading Financial year-ending data
fread("Annual_financial.txt", sep = "|", colClasses = c("numeric", "character", rep("numeric",2), "character", rep("numeric",12))) %>% .[,c(4,5):=NULL] -> annual_financial
colnames(annual_financial) %>% gsub("sa_","",.) -> colnames(annual_financial)
colnames(annual_financial)[1:3] <- c("cono","coname","fy_yearend")

annual_financial[,c(1:3)] -> yearend
gc()
}

# Section 2A: INITIAL DATA CLEANING================================================
{
  # Categorising the data into different financial cyclic years (01 April-31 March in India) and generating new necessary columns
  daily_data %>% .[, year:= date%/%1e4] %>% .[, cycle_year:= ifelse(((date%/%100)%%100) <= size_cutoffmonth, year-1, year)] %>% .[industry, on="cono", industry_dummy:= i.industry_dummy] %>% .[, ':=' (firstdate = min(date), lastdate = max(date)), by=cono] %>% .[, ':=' (firstyear = firstdate%/%1e4, lastyear = lastdate%/%1e4, lag_mcap_daily = lag(close*outshares), outshares = na.locf(outshares, na.rm = FALSE)), by=cono] %>% unique(., by=c("cono","date")) %>% .[order(cono,date)] -> daily_data
  rm(industry)
  gc()
}

# Section 2B: Finding last day for companies that are disappearing from the data during the period===== 
{
  # Finding last date data for separating out vanishing firms to make the Appendices table to be attached in the Report
  daily_data %>% .[(date==lastdate)] %>% .[, yearmonth:= date%/%100] %>% .[facevalue, on=c("cono","yearmonth"), facevalue:= i.facevalue] %>% .[, close_fv_ratio:= close/facevalue] -> last_date_data
}

# Section 2C: Description of disappearing firms============
{ 
  # Description of all firms which stopped trading
  last_date_data %>% .[(yearmonth<=cutoffmonthforvanishing) & (cycle_year>=start_cycle_year)] -> data_all_stop_trading
  c(no_of_firms_stopped_trading_before_cutoff_date, no_of_firms_stopped_trading_from_merger) %<-% c(data_all_stop_trading %>% nrow(), sum(grepl("Merged", data_all_stop_trading$coname, ignore.case = TRUE)))
  c(market_cap_all_stop_trading, mean_mcap_all_stop_trading) %<-% sum_avg(data_all_stop_trading)
  data_all_stop_trading[,c("cono","date","lag_mcap_daily")] -> vanishing_all_ref
  data_all_stop_trading[,merge_dummy:= grepl("Merged",coname, ignore.case = TRUE)]  
  
  # Segregating vanished firms into merged firms, non-merged firms with last day closing price to facevalue ratio > 50% and non-merged firms with last day closing price to facevalue ratio < 50%
  data_all_stop_trading %>% .[(merge_dummy)] %>% .[,c("cono","date","lag_mcap_daily")] -> vanishing_merge_ref
  data_all_stop_trading %>% .[!merge_dummy & close_fv_ratio>=pricetofacevalue] %>% .[,c("cono","date","lag_mcap_daily")] -> vanishing_nonmerge_nonfv_ref
  data_all_stop_trading %>% .[!merge_dummy & close_fv_ratio<pricetofacevalue] %>% .[,c("cono","date","lag_mcap_daily")] -> vanishing_nonmerge_fv_ref
  
  # Description of vanished firms with last date closing price less than 50% of facevalue (both merged & non-merged) and also count of disappearing merged firms with close to facevalue ratio <0.5
  last_date_data[yearmonth<=cutoffmonthforvanishing & close_fv_ratio<pricetofacevalue & cycle_year>=start_cycle_year] -> last_date_data
  c(no_of_disappearing_firms_with_price_less_than_50pc_of_face_value, no_of_firms_disappearing_merger) %<-% c(nrow(last_date_data), sum(grepl("Merged",last_date_data$coname,ignore.case = TRUE)))
  c(market_cap_disappearing, mean_mcap_disappearing) %<-%  sum_avg(last_date_data)
  
  # Filter for companies that stopped trading with Price/Value less than 50% of face value
  last_date_data[,merge_dummy:=grepl("Merged",coname,ignore.case=TRUE)][(merge_dummy==0)] -> last_date_data
  no_of_non_merger_disappearing_firms_with_price_less_than_50pc_of_face_value=nrow(last_date_data) # Only variable with value.To be linked to text R-sweave
  c(market_cap_disappearing_non_merger, mean_mcap_disappearing_non_merger) %<-%  sum_avg(last_date_data)
  
  # Count of firms disappearing year-wise
  last_date_data[,.N, by=year] %>% .[order(year)] -> yearly_table_disappearing
  colnames(yearly_table_disappearing)[2] <- "no_of_firms_disappearing"  
  gc()
}

# Section 2D: Appending the last date date of disappearing firms with daily data file============
{ 
  # Finding next trading day of those firms which were neither merged nor had close to facevalue ratio <0.5 and suddenly got disappeared for Survivorship Bias Adjustment in the Sensex Trading Calendar
  sensex[order(date)] -> sensex
  sensex %>% .[,c("date")] %>% .[,next_date:= c(date[2:nrow(sensex)],NA)] -> sensex_next
  
  # Merging those next trading day data with last day data
  last_date_data[sensex_next, on="date", next_date:= i.next_date] %>% .[, date:= next_date] -> last_date_data
  last_date_data[,':='(close = ifelse(toggle100pc==1, 0.01, close), bse_returns = ifelse(toggle100pc==1, 0.01,bse_returns))]
  last_date_data[,c("yearmonth", "facevalue", "close_fv_ratio", "next_date","mcap","merge_dummy"):= NULL]
  
  # Combining those rows in the original daily data to form a new data table for Survivorship Bias Adjustment
  rbindlist(list(daily_data, last_date_data)) -> daily_data_p1  
  rm(daily_data)
  gc()
  
  # Recalculating some column variables and removing NA's due to combining rows
  daily_data_p1 %>% .[, ':='(year = date%/%1e4, yearmonth = date%/%1e2)] %>% .[, ":="(firstdate = min(date, na.rm=T), lastdate = max(date, na.rm=T)), by=cono] %>% .[, ':='(firstyear = firstdate%/%1e4, lastyear = lastdate%/%1e4), by=cono] %>% .[order(cono,date)] %>% .[, last_day_month:= max(date,na.rm=T), by=c("cono","yearmonth")] -> daily_data_p1
  gc()
}

# Section 3:FILTERING DATA : REMOVING FINANCIAL FIRMS ===================
{ 
  if(toggle_finance==0){daily_data_p1[(!industry_dummy==1)] -> daily_data_p1}
}

# Section 3A: Creating Trading filter file to be merged with relevant files for filter before portfolio rank creation & Overall data characteristics (MCAP distribution and Trading Day distribution)==========
# Notes: Firms which are traded for less than 50 trading days in cycle year t-1 are not included in portfolio formation in year t. The following lines, populate the number of trading days data to the relevant cycle years (t) for the portfolio creation.    
{
  # Counting number of trading days of a firm in the past cycle year in order to check for liquidity and consider the firm to be included in portfolio for the upcoming cycle year
  daily_data_p1 %>% .[, nooftrdays_yr:= .N, by=c("cono","cycle_year")] %>% .[,c("cono","cycle_year","nooftrdays_yr","close","outshares")] %>% .[,':='(cycle_year_trading_filter = cycle_year+1, mcap = close*outshares/1e9)] %>% .[, avg_mcap:= mean(mcap, na.rm=T), by = c("cono","cycle_year")] %>% unique(., by=c("cono","cycle_year")) -> tr_filter
  gc()
  
  # Yearly trading filter table
  tr_filter %>% .[(nooftrdays_yr<trdaysfilter)] %>% .[, unlist(lapply(.SD, len_sum_avg )), .SDcols = c("avg_mcap"), by = cycle_year_trading_filter] %>% .[, V2:= rep(c("number_of_filtered","total_mcap_filtered","avg_mcap_filtered"),32)] %>% dcast(.,cycle_year_trading_filter~V2, value.var = "V1") %>% .[,c(1,3,4,2)] -> yearly_tr_filter
  colnames(yearly_tr_filter)[1] <- "cycle_year"
  
  # Avergae number of trading days cycle-yearwise 
  tr_filter %>% .[, unlist(lapply(.SD, len_sum_avg )), .SDcols = c("avg_mcap"), by = cycle_year] %>% .[, V2:= rep(c("number_of_firms_all","total_mcap_all","avg_mcap_all"),32)] %>% dcast(.,cycle_year~V2, value.var = "V1") %>% .[,c(1,3,4,2)]  -> yearly_all_mcap
  tr_filter %>% .[,.(average_trading_days = mean(nooftrdays_yr, na.rm=T)), by=cycle_year_trading_filter] %>% .[order(cycle_year_trading_filter)] -> yearly_all_trday
  colnames(yearly_all_trday)[1] <- "cycle_year"
  
  merge(merge(yearly_all_mcap,yearly_all_trday,by="cycle_year"), yearly_tr_filter, by="cycle_year") %>% .[(cycle_year>=start_cycle_year)] -> yearly_all
  rm(yearly_all_mcap,yearly_all_trday,yearly_tr_filter)
  
  # Trading Days count distribution monthly and yearly
  tr_filter[,unlist(lapply(.SD, length_breakpoint)), .SDcols = c("avg_mcap"), by=cycle_year] %>% .[, V2:= rep(c("total_no_of_firms","10%","20%","30%","40%","50%","60%","70%","80%","90%"),32)] %>% dcast(.,cycle_year~V2, value.var = "V1") %>% .[,c(1,11,2:10)] %>% .[(cycle_year>=start_cycle_year)]-> yearly_mcap_distribution
  tr_filter[,unlist(lapply(.SD, length_breakpoint)), .SDcols = c("nooftrdays_yr"), by=cycle_year] %>% .[, V2:=  rep(c("total_no_of_firms","10%","20%","30%","40%","50%","60%","70%","80%","90%"),32)] %>% dcast(., cycle_year~V2, value.var = "V1") %>% .[,c(1,11,2:10)] %>% .[(cycle_year>=start_cycle_year)] -> yearly_trday_distribution
  
  # Making monthly trading days filter table 
  tr_filter %>% .[, cycle_year:= cycle_year_trading_filter] %>% .[, c("cycle_year","cono","nooftrdays_yr")] -> tr_filter
  daily_data_p1 %>% .[, .(nooftrdays_month = .N), by=c("cono","yearmonth")] %>% unique(., by=c("cono","yearmonth")) %>% .[order(cono,yearmonth)] -> tr_filter_yearmonth
  gc()
}

# Section 4: CREATING MONTHLY PRICE FILE & CALCULATING MONTHLY RETURNS============

# 4.1: CREATING MONTHLY DATA FROM DAILY DATA
{
  daily_data_p1 %>% .[, daily_price_returns:= log(bse_returns)] %>% .[, tot_return:= sum(daily_price_returns, na.rm=T) , by=c("cono","yearmonth")] %>% .[, daily_price_returns:=NULL ] %>% .[(date==last_day_month)] %>% .[, ':='(start_yearmonth = min(yearmonth), end_yearmonth = max(yearmonth)), by=cono] -> monthly_data
  
  year = rep(startyear:endyear, 12)
  month = c(rep(1:12,endyear-startyear+1))
  month=month[order(month)]
  yearmonth = year*100+month
  cono=rep(unique(monthly_data$cono),length(yearmonth))
  yearmonth=rep(yearmonth,length(unique(monthly_data$cono)))
  yearmonth=yearmonth[order(yearmonth)]
  yearmonth = as.data.table(cbind(cono,yearmonth))
  
  merge(monthly_data, yearmonth, by=c("cono","yearmonth"), all=TRUE) %>% .[,':='(start_yearmonth = min(start_yearmonth, na.rm=TRUE), end_yearmonth = max(end_yearmonth, na.rm=TRUE)) ,by=cono] %>% .[, end_yearmonth:= ifelse(end_yearmonth%%100>9, 100*((end_yearmonth+100)%/%100)+09, 100*((end_yearmonth%/%100))+09)] %>% .[(yearmonth>=start_yearmonth & yearmonth<=end_yearmonth)] %>% .[,c("cono","yearmonth","close","bse_returns","tot_return","outshares","year","cycle_year")] %>% .[, year:= yearmonth%/%100] %>% .[, cycle_year:= ifelse(yearmonth%%100 <= 9, year-1, year)] -> monthly_data
  gc()
}

# 4.2: RETURN CALCULATION
{
  # Ordering Data before returns and Tagging Values close, bse_returns, outshares for NA's
  cols <- c("close","bse_returns","outshares")
  monthly_data %>% .[order(cono,yearmonth)] %>% .[, (cols) := lapply(.SD, nafill, type="locf"), .SDcols = cols, by=cono]%>% .[, ':='(mcap=close*outshares, momentum_return =momentum(tot_return)), by=cono] %>% .[, mcap_lag:= lag(mcap), by=cono] -> monthly_data  
  rm(cols)
  gc()
}

# SECTION 5: Operations on BVPS to get only FY-year-end corresponding data================
{
  # Operations on bvps file (finding yearend month data and subsetting)
  bvps %>% .[, ':='(year = date%/%1e4, month = (date%/%100)%%100, yearmonth = date%/%100)] -> bvps
  yearend %>% .[, ':='(fy_yearmonth = fy_yearend%/%100, year = fy_yearend%/%1e4)] -> yearend
  
  merge(bvps,yearend,by=c("cono","year"), all=FALSE) %>% .[order(cono)] -> bvps
  # Selecting only BVPS data for month == FYend month
  bvps %>% .[(yearmonth==fy_yearmonth)] %>% .[,c("cono","year","bvps","fy_yearmonth","month")] -> bvps_cutoff
  
  # Defining the BVPS cutoff YEAR for portfolio cycle year - Same year, if FY ends in March else the previous year and estimating the cycle year based on the financial year close, and removing unnecessary columns, removing duplicate columns and filtering out non-Negative bvps values as negative -B/M values are difficult to interpret for portfolio formulation
  bvps_cutoff %>% .[, cycle_year:= ifelse(month>bvps_cutoffmonth, year+1,year)] %>% .[,c("year","month"):=NULL] %>% unique(., by=c("cono","cycle_year")) %>% .[(bvps>=0)] -> bvps_cutoff
  
  # Adding closing price of fy_yearend to the bvps data to estimate the B/M numbers # Calculating the closest financial year data that is applicable for b/m calculation. This is done as there may be cases where a company changed its FY reporting months. For example if a company shifted its reporting fron June-1998 and reported again in Mar-1999, both these year end dates will have the same cycle year (1999). However, we should only consider the Mar-1999 values. # Subsetting only those rows with closest FY_yearmonth
  monthly_data %>% .[, c("cono","yearmonth","close")] -> closing_monthly
  merge(bvps_cutoff,closing_monthly, by.x=c("cono","fy_yearmonth"), by.y = c("cono","yearmonth"), all.x=T) %>% .[, closest_fy_yearmonth:= ifelse(length(fy_yearmonth)>1, max(fy_yearmonth), fy_yearmonth), by= c("cono","cycle_year")] %>% .[(fy_yearmonth==closest_fy_yearmonth)] %>% .[,bm:= bvps/close] %>% .[(close!=0.01& !is.na(bm))] %>% .[,c("cono","cycle_year","fy_yearmonth","bm","close")] -> bvps_cutoff
  
  # Only relevant rows and removing NA's and very large Values (created by disappearing firms with the introduction of 0.01 price at disappearance)
  fyendingmarchper=sum(bvps_cutoff$fy_yearmonth%%100==3)/nrow(bvps_cutoff)
  gc()
}

# SECTION 6: SMB/HML PORTFOLIO CREATION AND CALCULATIONS===============
{
  # Merging trading days data and removing firms with lower trading days before bm_ranks
  merge(bvps_cutoff, tr_filter, by=c("cono","cycle_year"), all.x=T) %>% .[(nooftrdays_yr>=trdaysfilter)] -> bvps_cutoff
  #B/M Portfolio (Rank-> 1=Low, 2=Medium,3=High) # Note that we had earlier ignored all company with negative B/M ratios
  bvps_cutoff %>% .[,bm_rank:= findInterval(bm, quantile(bm,c(0,value_bp1,value_bp2,1)), all.inside = TRUE, left.open = TRUE), by=cycle_year] %>% .[order(cycle_year, bm)] -> bvps_cutoff
  
  # Creating a file with only the Size cutoff Month Mcap (eg.200909) then calculating decile rank
  monthly_data %>% .[, last_yearmonth:= max(yearmonth), by=cono] %>% .[(yearmonth%%100==size_cutoffmonth & yearmonth!=last_yearmonth)] %>% .[,c("cono","yearmonth","mcap")] %>% .[, cycle_year:= yearmonth%/%100] -> mcap_cycle
  
  # SIZE RANKS
  # Applying liquidity filter (Merging trading days data and removing firms with lower trading days before size ranks )
  merge(mcap_cycle, tr_filter, by=c("cono","cycle_year"), all.x=T) %>% .[(nooftrdays_yr>=trdaysfilter)] -> mcap_cycle
  #Size Portfolio (Rank-> 1=Small, 2=Big)
  mcap_cycle %>% .[,size_rank:= findInterval(mcap, quantile(mcap,c(0,size_bp,1),na.rm=T), all.inside = TRUE, left.open = TRUE), by=cycle_year] -> mcap_cycle

  merge(mcap_cycle, bvps_cutoff %>% .[,c("cycle_year","cono","bm_rank","bm")], by=c("cono","cycle_year"), all.x=T)-> mcap_bvps
  
  mcap_bvps %>% .[, size_value_portfolio:= paste(fifelse(size_rank==2,"B","S"),fifelse(bm_rank==3,"H",fifelse(bm_rank==2,"M","L")),sep="")] %>% .[(!is.na(bm_rank)& !is.na(size_rank))] -> mcap_bvps
  mcap_bvps %>% .[,total_portfolio_mcap:= sum(mcap), by=c("cycle_year","size_value_portfolio")] %>% .[,initial_amount:= mcap/total_portfolio_mcap, by =c("cycle_year","size_value_portfolio")] %>% .[, total_portfolio_mcap:=NULL] -> mcap_bvps
  
  #SIZE DECILES CUTOFF POINTS
  mcap_cycle %>% .[, .(col1 = quantile(mcap, size_bp,na.rm=T), no_firms = sum(!is.na(mcap))), by=cycle_year] -> cutoff
  setnames(cutoff, "col1", paste(size_bp*100,"%-break-point",sep=""))
  
  # B/M DECILES CUTOFF POINTS
  bvps_cutoff %>% .[, unlist(lapply(.SD, FUN = value_breakpoints)), .SDcols = "bm", by = cycle_year] -> cutoff_bm
  cutoff_bm %>% .[, V2:= rep(c(paste(value_bp1*100,"%-break-point",sep=""),paste(value_bp2*100,"%-break-point",sep=""),"no_firms"),32)] %>% dcast(., cycle_year~V2, value.var = "V1") -> cutoff_bm
  cutoff_bm[(cycle_year<endyear)] -> cutoff_bm
  gc()
}

# SECTION 7: WML PORTFOLIO CREATION AND CALCULATION=========
{                        
  # 1. Merging monthly trading days data to monthly file, 2.Calculating rolling lagged 12 months trading days and 3. Removing firms that have lower liquidity in the lagged 12 months 
  merge(monthly_data, tr_filter_yearmonth, by=c("cono","yearmonth"), all.x=T) %>% .[order(cono,yearmonth)] %>% .[, lag_12m_nooftrdays:= lag.sum(nooftrdays_month,12), by=cono] %>% .[(lag_12m_nooftrdays>=trdaysfilter)] -> monthly_data_p1
  
  # PORTFOLIO CREATION
  monthly_data_p1 %>% .[, ':=' (size_rank_wml = findInterval(mcap_lag, quantile(mcap_lag,c(0,size_bp,1),na.rm=T), all.inside=TRUE, left.open = TRUE), momentum_rank_wml = findInterval(momentum_return, quantile(momentum_return, c(0,return_bp1,return_bp2,1), na.rm=T), all.inside = TRUE, left.open = TRUE)), by=yearmonth] %>% .[,momentum_portfolio:= paste(fifelse(size_rank_wml==2,"B","S"),fifelse(momentum_rank_wml==3,"W",fifelse(momentum_rank_wml==2,"N","L")),sep="")] %>% .[(!is.na(momentum_rank_wml)&!is.na(size_rank_wml))] -> monthly_data_p1  
  # Intial investment in the stocks - Investment of 1 rupee in each of the momentum portfolio. The investment in each stock in a particular month is equal to its Market Cap weight in the previous month/total market cap of momentum portfolio (any one out of 6) it belongs to
  monthly_data_p1 %>% .[, total_momentum_portfolio:= sum.na(mcap_lag), by=c("yearmonth","momentum_portfolio")] %>% .[,initial_amount_wml:= mcap_lag/total_momentum_portfolio, by=c("yearmonth","momentum_portfolio")] %>% .[,total_momentum_portfolio:=NULL] -> monthly_data_p1   
  
  # Monthly size cutoff point for size-momentum portfolios
  monthly_data_p1 %>% .[,.(col1 = quantile(mcap,size_bp,na.rm=T), no_firms = len.na(mcap)), by=yearmonth]  %>% .[(yearmonth<=update_end%/%100)] -> cutoff_m
  setnames(cutoff_m, "col1", paste(size_bp*100,"%-break-point",sep=""))
  
  # Monthly momentum cutoff point for size-momentum portfolios
  monthly_data_p1 %>% .[, unlist(lapply(.SD, value_breakpoints)), .SDcols = "momentum_return", by=yearmonth] -> cutoff_wml
  cutoff_wml %>% .[,V2:=rep(c(paste(return_bp1*100,"%-break-point",sep=""),paste(return_bp2*100,"%-break-point",sep=""),"no_firms"),360)] %>% dcast(., yearmonth~V2, value.var = "V1") -> cutoff_wml
  cutoff_wml %>% .[(yearmonth<=update_end%/%100)] -> cutoff_wml
  gc()
}

# SECTION 8: Daily SMB,HML and WML Calculations======
{
  # Dropping columns not relevant
  daily_data_p1[,c("year","industry_dummy","firstyear","lastyear","last_day_month","nooftrdays_yr","tot_return"):= NULL]
  
  # Dropping data frames not relevant
  rm(monthly_data,sensex_next,yearend,last_date_data,facevalue,yearmonth,closing_monthly,mcap_cycle,bvps)
  # Ordering the data before price returns
  daily_data_p1 %>% .[order(cono,date)] %>% .[,tot_return:= log(bse_returns)] -> daily_data_p1
  gc()
  
  # Looping across cycle years which is from startyear prices to -1 year from endyear 
  SMB_HML_Daily = as.data.table(NULL)
  SMB_HML_Daily_observations= as.data.table(NULL)
  WML_Daily= as.data.table(NULL)
  WML_Daily_observations= as.data.table(NULL)
  Rm= as.data.table(NULL)
  gc()
  
  for (i in startyear:endyear){
    sensex[(date%/%100>=(i*100+size_cutoffmonth+1) & date%/%100<=((i+1)*100+size_cutoffmonth)),c("date")] -> date # Finding sensex days for all trading days reference
    daily_data_p1[(cycle_year==i),c("cono")] %>% unique -> cono
    
    # Creating cono date file for all dates in the cycle year and tagging values for close
    a=nrow(date)
    b=nrow(cono)
    cono = cono[rep(seq(.N), a)]
    date = date[rep(seq(.N), b)] %>% .[order(date)]
    all_dates = cbind(cono,date)
    
    # Only selecting cycle year data from daily data
    daily_data_p1 %>% .[(cycle_year==i)] -> daily_data_i
    daily_data_i %>% .[,c("cono","firstdate","lastdate")] %>% unique(., by="cono") -> first_last_i
    
    # Merging firstdate and lastdate of each company
    merge(all_dates,first_last_i, by="cono", all.x=T) %>% .[(date>=firstdate)] -> all_dates
    #merging will all date file   # Recalculating yearmonth (to account for NA's due to merger)
    merge(all_dates, daily_data_i %>% .[,c("cono","date","yearmonth","tot_return","lag_mcap_daily")], by=c("cono","date"), all.x=T) %>% setnafill(., cols="tot_return",fill=0) %>% .[, yearmonth:=date%/%100] -> daily_data_i
    
    #Merging Portfolio information from 1.mcap_bvps file (for size-value port) & 2. monthly file (for momentum-size portfolio) to daily data
    mcap_bvps %>% .[(cycle_year==i),c("cono","size_value_portfolio","initial_amount")] -> mcap_bvps_i
    monthly_data_p1 %>% .[(cycle_year==i),c("cono","yearmonth","momentum_portfolio","initial_amount_wml")] -> monthly_data_i
    
    merge(daily_data_i,mcap_bvps_i, by="cono", all.x=T) %>% merge(.,monthly_data_i,by=c("cono","yearmonth"), all.x=T) %>% .[order(cono,date)] %>% .[,firm_cumulative_ret:= cumsum(tot_return), by="cono"] %>% .[,value_increase:= exp(firm_cumulative_ret)*initial_amount] -> daily_data_i
    
    daily_data_i %>% .[,.(value_increase= sum.na(value_increase), observations = len.na(value_increase)), by=c("size_value_portfolio","date")] %>% .[(!is.na(size_value_portfolio))] %>% .[order(size_value_portfolio,date)] %>% .[,wt_total_returns:= c(log(value_increase[1]),diff(log(value_increase))), by = "size_value_portfolio"] -> smb_hml_returns_i
    
    # SMB and HML Calculations
    smb_hml_returns_i %>% dcast(., date~size_value_portfolio, value.var = "wt_total_returns") -> smb_hml_i
    smb_hml_returns_i %>% dcast(., date~size_value_portfolio, value.var = "observations") -> smb_hml_observations_i
    
    port_check_i=ifelse(smb_hml_observations_i>=minportnumber,1,NA)
    smb_hml_i=smb_hml_i*port_check_i
    
    SMB_HML_Daily = rbindlist(list(SMB_HML_Daily,smb_hml_i), use.names = TRUE, fill = TRUE)
    SMB_HML_Daily_observations = rbindlist(list(SMB_HML_Daily_observations, smb_hml_observations_i), use.names = TRUE, fill = TRUE)
    
    # WML Calculations
    daily_data_i %>% .[order(cono,date)] %>% .[,firm_cumulative_wml:= cumsum(tot_return), by=c("cono","yearmonth")] %>% .[,value_increase_wml:= exp(firm_cumulative_wml)*initial_amount_wml] -> daily_data_i
    
    daily_data_i %>% .[,.(value_increase = sum.na(value_increase_wml), observations = len.na(value_increase_wml)), by=c("date","yearmonth","momentum_portfolio")] %>% .[(!is.na(momentum_portfolio))] %>% .[order(momentum_portfolio,date)] %>% .[,wt_total_returns:= c(log(value_increase[1]),diff(log(value_increase))), by=c("momentum_portfolio","yearmonth")] -> wml_returns_i
    
    wml_returns_i %>% dcast(.,date~momentum_portfolio,value.var = "wt_total_returns") -> wml_i
    wml_returns_i %>% dcast(.,date~momentum_portfolio,value.var = "observations") -> wml_observations_i
    
    port_check_i=ifelse(wml_observations_i>=minportnumber,1,NA)
    wml_i=wml_i*port_check_i
   
    WML_Daily = rbindlist(list(WML_Daily,wml_i), use.names = TRUE, fill = TRUE)
    WML_Daily_observations = rbindlist(list(WML_Daily_observations, wml_observations_i), use.names = TRUE, fill = TRUE)
    
    # Rm Calculation   # Removing remaining na's in lag_mcap_daily as this relates (if it exists) to first date of the company
    daily_data_i %>% .[(!is.na(size_value_portfolio)&!is.na(momentum_portfolio))] %>% .[, lag_mcap_daily:= na.locf(lag_mcap_daily, na.rm=FALSE), by="cono"] %>% .[(!is.na(lag_mcap_daily))] %>% .[, total_lag_mcap_daily:=sum(lag_mcap_daily), by="date"] %>% .[, lag_mcap_value:= lag_mcap_daily/total_lag_mcap_daily] %>% .[,c("total_lag_mcap_daily"):=NULL] %>% .[, mcap_value_increase:= lag_mcap_value*exp(tot_return)] -> daily_data_i
    
    daily_data_i %>% .[,.(mcap_value_increase = sum(mcap_value_increase)), by="date"] %>% .[,Rm:= log(mcap_value_increase)] -> Rm_value_i
    
    Rm = rbindlist(list(Rm, Rm_value_i[,c("date","Rm")]), fill = TRUE)
    rm(wml_i,smb_hml_i,daily_data_i,Rm_value_i,port_check_i)
    gc()
}
 
  # Operations to remove last 12 months for when Capital Loss is used (-100%) as those are provisional
  if(toggle100pc==1)
  {
    c(WML_Daily, SMB_HML_Daily, WML_Daily_observations, SMB_HML_Daily_observations) %<-% lapply(list(WML_Daily, SMB_HML_Daily, WML_Daily_observations, SMB_HML_Daily_observations), FUN = function(x){x[(date<=(update_end-1e4))]})[1:4] 
   }
  
  SMB_HML_Daily[, ':='(SMB = rowMeans(SMB_HML_Daily[,c("SL","SM","SH")], na.rm=T) - rowMeans(SMB_HML_Daily[,c("BL","BM","BH")], na.rm=T), HML = rowMeans(SMB_HML_Daily[,c("SH","BH")], na.rm=T) - rowMeans(SMB_HML_Daily[,c("SL","BL")], na.rm=T))]
  WML_Daily[, WML:= rowMeans(WML_Daily[,c("SW","BW")], na.rm=T) - rowMeans(WML_Daily[,c("SL","BL")], na.rm=T)]
  
  # Removing days where the 4-factors are all zero (i.e no trading. This comes up as the reference dates were sensex dates and there are some dates (3-days) where there is change in sensex but no trading data on any stock)
  merge(SMB_HML_Daily %>% .[,c("date","SMB","HML")], WML_Daily %>% .[, c("date","WML")], by="date") %>% merge(., Rm, by="date") %>% .[(Rm!=0)] -> ff_daily
  Rm %>% .[(Rm!=0)] -> Rm
  
  # Adding Rf to the daily Data
  tbill <- as.data.table(read_excel("91days_TBillRate.xlsx", 
                      col_types = c("date", "numeric")))
  colnames(tbill) <- c("date","yield_annualised")
  tbill %>% .[order(date)] %>% .[, date:= year(date)*1e4+month(date)*1e2+mday(date)] %>% merge(., ff_daily %>% .[,c("date")], by = "date", all=T) -> tbill
  tbill$count_trading = mapply(FUN = function(x){nrow(Rm %>% .[(date>=x & date<x+1e4)])}, tbill$date)
  # Replacing values of the trading count for the last one year 
  set(tbill, which(tbill$date>update_end-1e4), "count_trading", NA)
  tbill %>% .[,count_trading:= nafill(count_trading, type="locf")] %>% .[, Rf:= log(1+yield_annualised/100)/count_trading] %>% .[,Rf:=nafill(Rf, type="locf")] -> tbill
  
  merge(ff_daily, tbill[,c("date","Rf")], by="date", all.x=T) %>% .[, MRP:=Rm-Rf] %>% .[(date>=ff_results_start & date<=update_end)] -> ff_daily
  
  ff_monthly=copy(ff_daily) 
  
  vars<- c("SMB","HML","WML","Rm","Rf","MRP")
  ff_monthly[,yearmonth:= date%/%100] %>% .[, paste0(vars,"_","monthly"):= lapply(.SD, sum), .SDcols= vars, by=yearmonth] 
  ff_monthly %>% unique(., by="yearmonth") %>% .[,c("yearmonth","SMB_monthly","HML_monthly","WML_monthly","Rm_monthly","Rf_monthly","MRP_monthly")] -> ff_monthly  
  
  filename_output=paste("FourFactor_output",ifelse(toggle100pc==1,"100pc","wo100pc"),ifelse(toggle_finance==1,"financial","wofinancial"),sep="_")
  save.image(paste(write,filename_output,".RData",sep=""))
}

q()

# SECTION 9: Outputs==========
rm(list=ls())
toggle100pc=1 # Set before output (1 for Survivorship adjusted outputs)
toggle_finance=1 # Set before output (1 for including financial firms)

library(data.table)
library(magrittr)
library(openxlsx)
library(readr)
library(zeallot)
library(ggplot2)
library(ggrepel)
library(directlabels)
library(scales)
library(gdata)

# Fourfactors XL File
{
  master_folder="D:/4-FactorModel/ResearchProject/" # Please specify
  setwd(paste(master_folder,"Outputs/",sep=""))
  read=paste(master_folder,"Inputs/",sep="")
  write=paste(master_folder,"Outputs/",sep="")
  
  filename_output=paste("FourFactor_output",ifelse(toggle100pc==1,"100pc","wo100pc"),ifelse(toggle_finance==1,"financial","wofinancial"),sep="_")
  load(paste(write,filename_output,".RData",sep=""))
  gc()
  
# Ouput Excel
filename=paste("FourFactor",ifelse(toggle100pc==1,"100pc","wo100pc"),ifelse(toggle_finance==1,"financial","wofinancial"),sep="_")
fourfactors = createWorkbook(paste(write,filename,".xlsx",sep=""))

vars<- c("Size_cutoffs" = "cutoff", "Value_Cutoffs" = "cutoff_bm","WML_Size_Cutoff" = "cutoff_m", "WML_Return_Cutoff" = "cutoff_wml", "SMB_HML" = "SMB_HML_Daily", "SMB_HML_Observations" = "SMB_HML_Daily_observations", "WML" = "WML_Daily", "WML_Observations" = "WML_Daily_observations", "Rm" = "Rm", "Four_Factors_Daily" = "ff_daily", "Four_Factors_Monthly" = "ff_monthly")
for(i in 1:length(vars)) {
  addWorksheet(fourfactors, names(vars)[i])
  writeDataTable(fourfactors, sheet = names(vars)[i], eval(parse(text = vars[i])))
}
saveWorkbook(fourfactors, file = paste(write,ifelse(toggle100pc==1, "FourFactors_Survivorship_Bias_Adjusted.xlsx", "FourFactors.xlsx") ,sep=""))

#Output CSV
start_cycle_year=1992
update_end=20211231
ff_results_start=19930101 # specifies what date the four-factors results should start from

name=ifelse(toggle100pc==1,"_SurvivorshipBiasAdjusted","")
ff_results_end=ifelse(toggle100pc==1,update_end-1e4,update_end)
end_cycle_year=ifelse((update_end%/%100)%%100>size_cutoffmonth,update_end%/%1e4,(update_end%/%1e4-1))
end_cycle_year=ifelse(toggle100pc==1,end_cycle_year-1,end_cycle_year)

#Size_Break_point (size-value portfolios)
cutoff[(cycle_year>=start_cycle_year & cycle_year<=end_cycle_year), cycle_year:=cycle_year*1e4+930]
cutoff[,2] = cutoff[,2]/1e6
names(cutoff)=c("Portfolio creation date","90th Percentile market capitalisation (INR million)","Number of firms")
write_csv(cutoff,paste(write,"Size_Break_Points_for_Size_Value_Portfolios",name,".csv",sep=""))

#Value_Break_point (size-value portfolios)
cutoff_bm[(cycle_year>=start_cycle_year & cycle_year<=end_cycle_year),cycle_year:= cycle_year*1e4+930]
names(cutoff_bm)=c("Portfolio creation date","30th Percentile B/M","70th Percentile B/M","Number of firms")
write_csv(cutoff_bm,paste(write,"Value_Break_Points_for_Size_Value_Portfolios",name,".csv",sep=""))

#Size_Break_point (size-momentum portfolios)
setDT(cutoff_m, yearmonth>=(ff_results_start%/%100) & yearmonth<=ff_results_end%/%100)
cutoff_m[,2]=cutoff_m[,2]/1e6
names(cutoff_m)=c("Month","90th Percentile market capitalisation (INR million)","Number of firms")
write_csv(cutoff_m,paste(write,"Size_Break_Points_for_Size_Momentum_Portfolios",name,".csv",sep=""))

#Momentum Break Points (size-value portfolios)
cutoff_wml[(yearmonth>=(ff_results_start%/%100) & yearmonth<=ff_results_end%/%100), c(2:3):= lapply(.SD, "*", 1100), .SDcols = c(2:3)]
names(cutoff_wml)=c("Month",  "30th Percentile momentum return %",	"70th Percentile momentum return %",	"Number of firms")
write_csv(cutoff_wml,paste(write,"Momentum_Break_Points_for_Size_Momentum_Portfolios",name,".csv",sep=""))

# Portfolio Returns (daily yearly and monthly files)
SMB_HML_Daily[(!(SMB==0 & HML==0) & date>=ff_results_start & date<=ff_results_end), c(2:7) := lapply(.SD, "*", 100), .SDcols = c(2:7)][,c(8:9):=NULL]

SMB_HML_monthly = copy(SMB_HML_Daily)
SMB_HML_monthly[, yearmonth:= date%/%100] %>% .[,c(2:7):= lapply(.SD, FUN = sum.na), .SDcols = c(2:7), by="yearmonth"] %>% .[(BH==0), BH:=NA_real_] %>% .[,c("yearmonth","BH","BM","BL","SH","SM","SL")] %>% unique -> SMB_HML_monthly

SMB_HML_yearly = copy(SMB_HML_Daily)
SMB_HML_yearly[, year:=date%/%1e4] %>% .[,c(2:7):= lapply(.SD, FUN = sum.na), .SDcols = c(2:7), by="year"] %>% .[(BH==0), BH:=NA_real_] %>% .[,c("year","BH","BM","BL","SH","SM","SL")] %>% unique -> SMB_HML_yearly

names(SMB_HML_Daily)=c("Date",paste(rep("Portfolio",6),c("BV","BN","BG","SV","SN","SG"),rep("returns %",6)))
names(SMB_HML_monthly)=c("Month",paste(rep("Portfolio",6),c("BV","BN","BG","SV","SN","SG"),rep("returns %",6)))
names(SMB_HML_yearly)=c("Year",paste(rep("Portfolio",6),c("BV","BN","BG","SV","SN","SG"),rep("returns %",6)))
write_csv(SMB_HML_Daily,paste(write,"Size_Value_Portfolio_Returns_Daily",name,".csv",sep=""))
write_csv(SMB_HML_monthly,paste(write,"Size_Value_Portfolio_Returns_Monthly",name,".csv",sep=""))
write_csv(SMB_HML_yearly,paste(write,"Size_Value_Portfolio_Returns_Yearly",name,".csv",sep=""))

# Momentum Portfolio Returns
WML_Daily[(!(WML==0) & date>=ff_results_start & date<=ff_results_end),c(2:7):= lapply(.SD,"*",100), .SDcols = c(2:7)] %>% .[,c(1,4,7,2,5)] -> WML_Daily

WML_monthly = copy(WML_Daily)
WML_monthly[,yearmonth:= date%/%100] %>% .[,c(2:5):=lapply(.SD, sum, na.rm=T), .SDcols = c(2:5), by="yearmonth"] %>% .[(BL==0), BL:=NA_real_] %>% .[,c("yearmonth","BW","SW","BL","SL")] %>% unique -> WML_monthly

WML_yearly = copy(WML_Daily)
WML_yearly[,year:= date%/%1e4] %>% .[,c(2:5):=lapply(.SD, sum, na.rm=T), .SDcols = c(2:5), by="year"] %>% .[(BL==0), BL:=NA_real_] %>% .[,c("year","BW","SW","BL","SL")] %>% unique -> WML_yearly

names(WML_Daily)=c("Date",paste(rep("Portfolio",4),c("WB","WS","LB","LS"),rep("returns %",4)))
names(WML_monthly)=c("Month",paste(rep("Portfolio",4),c("WB","WS","LB","LS"),rep("returns %",4)))
names(WML_yearly)=c("Year",paste(rep("Portfolio",4),c("WB","WS","LB","LS"),rep("returns %",4)))
write_csv(WML_Daily,paste(write,"Size_Momentum_Portfolio_Returns_Daily",name,".csv",sep=""))
write_csv(WML_monthly,paste(write,"Size_Momentum_Portfolio_Returns_Monthly",name,".csv",sep=""))
write_csv(WML_yearly,paste(write,"Size_Momentum_Portfolio_Returns_Yearly",name,".csv",sep=""))

# Four Factors Returns
ff_daily[(date>=ff_results_start & date<=ff_results_end), c(2:7):= lapply(.SD,"*",100), .SDcols = c(2:7)]
ff_monthly[(yearmonth>=ff_results_start%/%100 & yearmonth<=ff_results_end%/%100), c(2:7):= lapply(.SD,"*",100), .SDcols = c(2:7)]

ff_yearly = copy(ff_monthly)
ff_yearly[,year:= yearmonth%/%100][,c(2:7):= lapply(.SD, sum, na.rm=T), .SDcols = c(2:7), by="year"][,c(8,2:7)] %>% unique -> ff_yearly

vars<- c("SMB %",	"HML %",	"WML %",	"Rm %",	"Rf %",	"Rm-Rf %")
names(ff_daily)=c("Date", vars)
names(ff_monthly)=c("Month", vars)
names(ff_yearly)=c("Year", vars)
write_csv(ff_daily,paste(write,"FourFactors_and_Market_Returns_Daily",name,".csv",sep=""))
write_csv(ff_monthly,paste(write,"FourFactors_and_Market_Returns_Monthly",name,".csv",sep=""))
write_csv(ff_yearly,paste(write,"FourFactors_and_Market_Returns_Yearly",name,".csv",sep=""))

gc()
}

#Plotting the Four-Factors over time
rm(list=ls())
gc()
toggle100pc=1 # Set these before output (1 for Survivorship adjusted outputs)
toggle_finance=1 # 

{
  master_folder="D:/4-FactorModel/ResearchProject/" # Please specify
  setwd(paste(master_folder,"Outputs/",sep=""))
  read=paste(master_folder,"Inputs/",sep="")
  write=paste(master_folder,"Outputs/",sep="")
  
  filename_output=paste("FourFactor_output",ifelse(toggle100pc==1,"100pc","wo100pc"),ifelse(toggle_finance==1,"financial","wofinancial"),sep="_")
  load(paste(write,filename_output,".RData",sep=""))  
  gc()
  
  data = copy(ff_monthly)
  data[(yearmonth)>=199301][,date:=yearmonth*100+1][, date:= as.Date(as.character(date),"%Y%m%d")][, yearmonth:= as.Date(date,"%b-%Y")]
  
  c(min_cum_return, max_cum_return) %<-% range(apply(data[,c(2:5)], 2, FUN = cumsum)*100)
  
  data1 = copy(data)
  data1[,c(1:4,7)][,c(2:5):= lapply(.SD, FUN = function(x) 100*cumsum(x)), .SDcols=c(2:5)][,yearmonth:=as.Date(as.character(yearmonth*100+01), format="%Y%m%d")] -> data1
  names(data1)[2:5] <- c("Cumulative SMB","Cumulative HML","Cumulative WML","Cumulative Rm-Rf")
  
  historical_events <- as.Date(c("2001-03-01", "2003-04-01","2007-12-01","2008-09-15"), format="%Y-%m-%d")
  factors<- 100* sapply(data[,c(2:4,7)],sum)
  four_factors <- names(data1)[2:5]
  
  data1<- as.data.frame(data1)
  ggplot(data1, aes(x=yearmonth)) +
    sapply(four_factors, FUN = function(p) {geom_line(aes(y=data1[,p], color = p), linetype=1, size=0.75)}) +
    theme(panel.background = element_rect(fill="white"),
          plot.title = element_text(color="black",face=1, size=15, hjust=1),
          plot.margin = unit(c(1,1,1,1), "cm"),
          legend.position=c(0.1,0.85),
          text = element_text(family="serif"),
          axis.text.x = element_text(angle = 90, color="#000000"),
          axis.text.y = element_text(color="#000000"),
          axis.title = element_text(color="black",size = 15, face=1, family = "serif", angle=0),
          axis.title.x = element_text(vjust=0),
          axis.line = element_line(color="black")) +
    scale_colour_manual(name="Four-factors", breaks= four_factors, values = c("Cumulative Rm-Rf" = "magenta", "Cumulative SMB" = "cyan", "Cumulative HML" = "red", "Cumulative WML" = "springgreen2")) +
    scale_x_date(position = "bottom",labels = label_date("%h-%y"), breaks = seq(from = data1[6,"yearmonth"], 
                                                                                to = data1[nrow(data1),"yearmonth"], by = "1 year")) +
    scale_y_continuous(position = "left" ,breaks = pretty_breaks((max_cum_return%/%20+1)-(min_cum_return%/%20-1)), limits = c((min_cum_return%/%10-1)*10,(max_cum_return%/%10+1)*10)) +
    geom_hline(yintercept = 0, linetype=2, color="grey10", size=0.25) +
    sapply(historical_events, FUN = function(e) geom_vline(xintercept = e, linetype=2, size=0.5,col="grey10")) +
    sapply(factors, FUN = function(t){geom_text(aes(x=data1[nrow(data1),"yearmonth"],y=t-10), col="black", size=3.5, label=paste(round(t,0),"%",""))}) +
    
    geom_text(aes(as.Date('2001-Mar-01','%Y-%b-%d'),180), size=3.5,label="Dot-Com Bubble Burst - Mar 2001", col="#000000") +
    geom_text(aes(as.Date('2003-Apr-01','%Y-%b-%d'),150), size=3.5,label="Net FII Inflows jumps - Apr2003",col="#000000") +
    geom_text(aes(as.Date('2007-Dec-01','%Y-%b-%d'),105),size=3.5,label="FII Outflows 2008",col="#000000") +
    geom_text(aes(as.Date('2008-Sep-15','%Y-%b-%d'),30), size=3.5,label="Lehman Bankruptcy - Sep 2008",col="#000000") +
    labs(title="Cumulative Four-Factor Returns", x="Months", y="Cumulative Return (%)")
  
  ggsave(ifelse(toggle100pc==1,"4-factor.pdf","4-factor_wo100pc.pdf"), device="pdf", path="D:/4-FactorModel/ResearchProject/Outputs", width = 9.11, height = 5.51, units = "in")
}

# Table for group-variance and overall variance of Mcaps for various decile points
 {
    daily_data_p1[,mcap:= outshares*close][,yearly_mcap_avg:= mean(mcap,na.rm=T)/1e9, by=c("cono","cycle_year")]
    data = daily_data_p1 %>% unique(.,by=c("cono","cycle_year")) %>% .[,c("cono","cycle_year","yearly_mcap_avg")]
    
    data[order(cycle_year)] -> data
    size_cutoff_breakpoint<- seq(0.5,0.9,0.1)
    
    ratio <- lapply(size_cutoff_breakpoint, FUN = function(i){
      ratio_i = copy(data)
      ratio_i[,port:=findInterval(yearly_mcap_avg, quantile(yearly_mcap_avg,c(0,i,1),na.rm=T),all.inside=T, left.open=T), by="cycle_year"]
      ratio_i <- ratio_i[,.(yearly_mcap_avg.FUN = sum(yearly_mcap_avg^2,na.rm=T)- (sum(yearly_mcap_avg,na.rm=T)^2)/length(na.omit(yearly_mcap_avg))), by=c("port","cycle_year")] %>% unique
      ratio_i %>% dcast(.,cycle_year~port, value.var = "yearly_mcap_avg.FUN") -> ratio_i
      
      ratio_i$sum_SS = rowSums(ratio_i[,c(2:3)],na.rm=T)
      names(ratio_i)[1:3] <- c("year","small SS", "big SS")
      ratio_i[,"break_point":= i][,c(5,1:4)] -> ratio_i
      })
    
    ratio = rbindlist(ratio)
    write_csv(ratio,"break-point-analysis_acmcap.csv")
}

# Table of disappearing firms
{
  if(toggle100pc==1){
      vanishing_all_ref[,year:=date%/%1e4]
     
      vanishing_all_ref %>% .[,.(no_of_firms_vanishing = length(date)), by="year"] -> table_vanishing

      vanishing_merge_ref[,year:=date%/%1e4]
      vanishing_merge_ref %>% .[,.(no_of_merged_firms_vanishing = length(date)),by="year"] %>% merge(table_vanishing,.,by="year",all.x=T) -> table_vanishing
      names(table_vanishing)=c("year","all_stop_trading","merged")
    
      # Finding details of inclusion in portfolio for FV>0.5
      vanishing_nonmerge_nonfv_ref[,':='(year=date%/%1e4,yearmonth=date%/%100)][,cycle_year:=ifelse((date%/%100)%%100>9,year,year-1)]
      merge(vanishing_nonmerge_nonfv_ref,monthly_data_p1[,c("yearmonth","cono","momentum_portfolio")], by=c("cono","yearmonth"), all.x=T) %>% merge(.,mcap_bvps[,c("cycle_year","cono","size_value_portfolio")],by=c("cono","cycle_year"),all.x=T) %>% .[,inclusion_dummy:= fifelse(is.na(.[,c("momentum_portfolio")]) & is.na(.[,c("momentum_portfolio")]),0,1)] -> vanishing_nonmerge_nonfv_ref
      to_table = vanishing_nonmerge_nonfv_ref[,.(date.length= length(date)),by=c("year","inclusion_dummy")] %>% dcast(.,year~inclusion_dummy, value.var="date.length") %>% .[,total:=rowSums(.[,c(2:3)],na.rm=T)]
      table_vanishing = merge(table_vanishing,to_table[,c("year","total","1")],by="year",all.x=T)
     
      vanishing_nonmerge_fv_ref[,':='(year=date%/%1e4,yearmonth=date%/%100)][,cycle_year:=ifelse((date%/%100)%%100>9,year,year-1)]
      merge(vanishing_nonmerge_fv_ref,monthly_data_p1[,c("yearmonth","cono","momentum_portfolio")],by=c("cono","yearmonth"),all.x=T) %>% merge(.,mcap_bvps[,c("cycle_year","cono","size_value_portfolio")],by=c("cono","cycle_year"),all.x=T) %>% .[,inclusion_dummy:= fifelse(is.na(.[,c("momentum_portfolio")]) & is.na(.[,c("size_value_portfolio")]), 0, 1)] -> vanishing_nonmerge_fv_ref
      to_table = vanishing_nonmerge_fv_ref[,.(date.length= length(date)),by=c("year","inclusion_dummy")] %>% dcast(.,year~inclusion_dummy,value.var="date.length") %>% .[,total:=rowSums(.[,c(2:3)],na.rm=T)]
      table_vanishing = merge(table_vanishing,to_table[,c("year","total","1")],by="year",all.x=T)
      
      names(table_vanishing)=c("year","all_stop_trading","merged","With_fv_above_0.5","included_in_port","with_fv_less_0.5","included_in_port")
}else{table_vanishing=NULL}
}
   
# Preparing Rdata file for sweave
{
  keep(startyear ,endyear ,yearly_mcap_distribution ,fyendingmarchper ,yearly_all  ,no_of_firms_stopped_trading_before_cutoff_date ,no_of_firms_stopped_trading_from_merger ,market_cap_all_stop_trading ,mean_mcap_all_stop_trading ,no_of_disappearing_firms_with_price_less_than_50pc_of_face_value ,no_of_firms_disappearing_merger , market_cap_disappearing, mean_mcap_disappearing, no_of_non_merger_disappearing_firms_with_price_less_than_50pc_of_face_value, market_cap_disappearing_non_merger, mean_mcap_disappearing_non_merger, yearly_table_disappearing ,yearly_trday_distribution ,cono1 ,cono2 , cutoff  , cutoff_bm , cutoff_m , cutoff_wml ,SMB_HML_Daily ,SMB_HML_Daily_observations ,WML_Daily ,WML_Daily_observations ,Rm ,ff_daily ,ff_monthly ,size_bp ,value_bp1 ,value_bp2 ,return_bp1 ,return_bp2 ,minportnumber ,minportnumber ,pricetofacevalue ,pricetofacevalue ,momentumj_start ,momentumj_end ,bvps_cutoffmonth  ,size_cutoffmonth,trdaysfilter, table_vanishing,write,toggle100pc,sure=T)
  tables_name=ifelse(toggle100pc==1,"FF-tables.RData","FF-tables-wo100pc.RData")
  save.image(paste(write,tables_name,sep=""))
}
################-------END OF CODE--------#################
   
