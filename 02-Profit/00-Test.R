library(styleer)
# 1. Pre-processing factor data
# 1.1 Read all the txt and csv files
url <- str_c(getwd(), "/data/")
file.names <- list.files(path = url, pattern = "*.txt|.csv") %>% tolower()
for (i in file.names) {
    assign(str_sub(i, start = 1L, end = -5L), fread(str_c(url, toupper(i))))
}

stk_mkt_fourfacmonth <- stk_mkt_carhartfourfactors[MarkettypeID == "P9706", .SD[, -2]]
stk_mkt_fivefacday <- stk_mkt_fivefacday[MarkettypeID == "P9714" & Portfolios == 1, .SD[, c(-1, -3)]]
stk_mkt_fivefacmonth <- stk_mkt_fivefacmonth[MarkettypeID == "P9714" & Portfolios == 1, .SD[, c(-1, -3)]]
stk_mkt_thrfacday <- stk_mkt_thrfacday[MarkettypeID == "P9706", .SD[, -1]]
stk_mkt_thrfacmonth <- stk_mkt_thrfacmonth[MarkettypeID == "P9706", .SD[, -1]]

umdday <- umdday[, setnames(.SD, 1:2, c("TradingDate", "UMD"))
    ][, TradingDate := str_replace_all(TradingDate, "/", "-") %>% as.Date("%Y-%m-%d")]

stk_mkt_fourfacday <- stk_mkt_thrfacday[umdday, on = .(TradingDate)]

# 1.2 Adding postfix to colum names of every table
obj.names <- ls()[4:9]
post.fix <- c(5, 5, 4, 4, 3, 3)

l <- list()
for (i in seq_along(obj.names)) {
    l[[i]] <- paste(colnames(eval(parse(text = obj.names[i])))[-1], post.fix[i], sep = "_")
    setnames(eval(parse(text = obj.names[i])), -1, l[[i]])
}

factor_monthly <- stk_mkt_fivefacmonth[stk_mkt_fourfacmonth[stk_mkt_thrfacmonth, on = .(TradingMonth)], on = .(TradingMonth)]
factor_daily <- stk_mkt_fivefacday[stk_mkt_fourfacday[stk_mkt_thrfacday, on = .(TradingDate)], on = .(TradingDate)]

sv(factor_monthly, svname = "factor_monthly")
sv(factor_daily, svname = "factor_daily")
rm(list = ls())
