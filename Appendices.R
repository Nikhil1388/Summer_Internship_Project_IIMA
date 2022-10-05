library(data.table)
library(magrittr)
library(openxlsx)

market_cap = copy(daily_data_p1)
market_cap[,c("cono","close","outshares","cycle_year")][,yearly_mcap_avg:= mean(close*outshares,na.rm=T)/1e9, by=c("cono","cycle_year")][,c("cycle_year","cono","yearly_mcap_avg")] %>% unique(.,by=c("cono","cycle_year")) -> market_cap
market_cap[,.(c(length(na.omit(yearly_mcap_avg)),quantile(yearly_mcap_avg,c(seq(0.1,0.9,0.2)),na.rm=T),sum(yearly_mcap_avg,na.rm=T),mean(yearly_mcap_avg,na.rm=T))), by="cycle_year"] -> market_cap

variable <- c("Number of firms", "10%","30%","50%","70%","90%","Total Market cap.","Avg. Market cap.")
market_cap[,V2:=rep(variable,32)]
market_cap %>% dcast(.,cycle_year~V2, value.var = "V1") -> market_cap
market_cap[,c(1,8,2:7,9)] -> market_cap

yearly_trday_distribution <- round(yearly_trday_distribution,0)

# The main code file has to be re-run till section 2C where the table "data_all_stop_trading" has been generated to create this Appendix table.
data_all_stop_trading[,c("cono","coname","year","close_fv_ratio")] -> appendix_relevant_last_date
appendix_relevant_last_date[(close_fv_ratio)>=0.5] -> above_close_fv
appendix_relevant_last_date[(close_fv_ratio)<0.5] -> below_close_fv

above_close_fv[,merge_dummy:=grepl("Merged",coname,ignore.case=TRUE)][,':='(merged =sum(merge_dummy,na.rm=T), non_merged=.N-sum(merge_dummy,na.rm=T)) ,by="year"]
above_close_fv[,c("year","merged","non_merged")] %>% unique %>% .[order(year)] -> above_close_fv

below_close_fv[,merge_dummy:=grepl("Merged",coname,ignore.case=TRUE)][,':='(merged =sum(merge_dummy,na.rm=T), non_merged=.N-sum(merge_dummy,na.rm=T)) ,by="year"]
below_close_fv[,c("year","merged","non_merged")] %>% unique %>% .[order(year)] -> below_close_fv

merge(below_close_fv,above_close_fv,by="year") -> data_all_disappearing
appendix_relevant_last_date[,merge_dummy:= grepl("Merged",coname,ignore.case = TRUE)][,':='(Number_of_firms_that_stopped_trading= .N, stopped_trading_due_to_mergers = sum(merge_dummy,na.rm=T)), by=year][,c("year","Number_of_firms_that_stopped_trading","stopped_trading_due_to_mergers")] %>% unique %>% .[order(year)] -> appendix_relevant_last_date

merge(appendix_relevant_last_date,data_all_disappearing, by="year") -> data_all_disappearing

#The fourth table has been manually formed by merging ff_yearly(for both Survivorship Bias Adjusted and the other case).

Appendices = createWorkbook("Appendices.xlsx")
addWorksheet(Appendices, "DescriptiveStats_MarketCap")
writeDataTable(Appendices,"DescriptiveStats_MarketCap",market_cap)

addWorksheet(Appendices, "Descriptive_Stats_of_Liquidity")
writeDataTable(Appendices,"Descriptive_Stats_of_Liquidity",yearly_trday_distribution)

addWorksheet(Appendices, "DescriptiveStats_Disappearing")
writeDataTable(Appendices, "DescriptiveStats_Disappearing" ,data_all_disappearing)

saveWorkbook(Appendices, "D:/4-FactorModel/ResearchProject/Outputs/Appendices.xlsx")

read.xlsx("D:\\4-FactorModel\\Legacy ff 4 factors.xlsx", startRow = 2) -> legacy_ff
View(legacy_ff)
as.data.table(legacy_ff) ->legacy_ff
legacy_ff[,c(8:13):=NULL]

legacy_ff_yearly=copy(legacy_ff)

legacy_ff_yearly[,year:=Date%/%1e4][,c(2:7):=lapply(.SD,FUN = function(x){sum(x,na.rm=T)}), .SDcols = c(2:7), by="year"][,c(8,2:7)] %>% unique() -> legacy_ff_yearly          
legacy_ff_yearly[1:27] -> legacy_ff_yearly

legacy_ff_yearly[,c(1,7,2:4)][,c(2:5):= lapply(.SD,FUN = cumsum), .SDcols=c(2:5)] -> legacy_ff_yearly

legacy_ff_yearmonth = copy(legacy_ff)
legacy_ff_yearmonth[,yearmonth:= Date%/%100][,c(2:7):= lapply(.SD, FUN = function(x) {sum(x,na.rm=T)}), .SDcols=c(2:7), by=yearmonth][,c(8,7,2:4)] %>% unique -> legacy_ff_yearmonth
View(legacy_ff_yearmonth)

legacy_ff_yearmonth[1:315] -> legacy_ff_yearmonth
legacy_ff_yearmonth[,c(2:5):=lapply(.SD,cumsum), .SDcols=c(2:5)] -> legacy_ff_yearmonth
names(legacy_ff_yearmonth)[2:5] <- c("Cumulative Rm-Rf (%)","Cumulative SMB (%)", "Cumulative HML (%)", "Cumulative WML (%)")

melt(legacy_ff_yearmonth, id=1, measure=c(2:5)) -> legacy_ff_yearmonth

library(ggplot2)
ggplot(legacy_ff_yearmonth, aes(x=yearmonth, y=value, color=variable)) +
  geom_line()

getwd()
dcast(legacy_ff_yearmonth, yearmonth~variable, value.var = "value") -> legacy_ff_yearmonth

legacy_ff_yearly = copy(legacy_ff)
legacy_ff_yearly[,year:=Date%/%1e4][,c(2:5):= lapply(.SD, FUN = function(x) {sum(x,na.rm=T)}), .SDcols = c(2:5), by=year][,c(8,2:5)] %>% unique -> legacy_ff_yearly

legacy_ff_yearly[,c(2:5):= lapply(.SD, cumsum), .SDcols = c(2:5)] -> legacy_ff_yearly
names(legacy_ff_yearly)[2:5] <- c("Cumulative Rm-Rf (%)","Cumulative SMB (%)", "Cumulative HML (%)", "Cumulative WML (%)")
names(legacy_ff_yearly)[c(4,5,2:3)] = c("Cumulative Rm-Rf (%)","Cumulative SMB (%)", "Cumulative HML (%)", "Cumulative WML (%)")
