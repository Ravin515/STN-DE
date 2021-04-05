library(styleer)
library(lfe)
url <- str_c(getwd(), "/data/Fund")
fund.value <- fbread(path = url, pattern = "*NAV.*\\.txt", encoding = "UTF-8")
fund.info <- fread(str_c(url, "/FUND_maininfo.txt"), encoding = "UTF-8")

fund.value <- fund.value[, ":="(fund.symbol = (formatC(Symbol, flag = '0', width = 6)), date = as.Date(TradingDate))
    ][order(fund.symbol, date), .SD
    ][, .(fund.symbol, date, NAV, CurrencyCode)
    ][, unique(.SD)]

fund <- fund.info[, ":="(fund.symbol = (formatC(MasterFundCode, flag = '0', width = 6)))
    ][, unique(.SD)
    ][fund.value, on = .(fund.symbol), nomatch = 0]

fund <- fund[date %between% c(as.Date("2016-06-01"), as.Date("2018-07-03")), .SD]

# 导入因子
url <- str_c(getwd(), "/data/Factors/")
file.names <- list.files(path = url, pattern = "*.txt|.csv")
for (i in file.names) {
    assign(str_sub(i, start = 1L, end = -5L), fread(str_c(url, toupper(i))))
}

fivefactor_daily <- fivefactor_daily[, trddy := as.Date(trddy)
    ][, setnames(.SD, "trddy", "date")]

ld(f.main2)
ld(f.main1)
f.main2 <- f.main2[fivefactor_daily, on = .(date), nomatch = 0]
f.main1 <- f.main1[fivefactor_daily, on = .(date), nomatch = 0]
fund <- fund[fivefactor_daily, on = .(date), nomatch = 0]

f.main2 <- f.main2[date %between% c(as.Date("2016-06-01"), as.Date("2018-07-03")), .SD
    ][order(cube.symbol, date), .SD
    ][, ret := value - shift(value, type = "lag"), by = .(cube.symbol)]

f.main1 <- f.main1[date %between% c(as.Date("2016-06-01"), as.Date("2018-07-03")), .SD
    ][order(cube.symbol, date), .SD
    ][, ret := value - shift(value, type = "lag"), by = .(cube.symbol)]

fund <- fund[date %between% c(as.Date("2016-06-01"), as.Date("2018-07-03")), .SD
    ][order(fund.symbol, date), .SD
    ][, ret := NAV - shift(NAV, type = "lag"), by = .(fund.symbol)]

f.main1[ret< 1 & post.follow == 0, lm(ret - rf ~ mkt_rf + smb + hml + umd + rmw + cma)] %>% summary()
f.main1[ret < 1 & post.follow == 1 & date - as.Date(follow.date) <= pre.period, lm(ret - rf ~ mkt_rf + smb + hml + umd + rmw + cma)] %>% summary()
f.main1[ret < 1 & post.follow == 0 | (post.follow == 1 & date - as.Date(follow.date) <= pre.period), lm(ret - rf ~ mkt_rf + smb + hml + umd + rmw + cma)] %>% summary()
fund[, lm(ret - rf ~ mkt_rf + smb + hml + umd + rmw + cma)] %>% summary()
fund[Category == "债券型基金", lm(ret - rf ~ mkt_rf + smb + hml + umd + rmw + cma)] %>% summary()
fund[Category == "股票型基金", lm(ret - rf ~ mkt_rf + smb + hml + umd + rmw + cma)] %>% summary()
fund[Category == "混合型基金", lm(ret - rf ~ mkt_rf + smb + hml + umd + rmw + cma)] %>% summary()
fund[Category == "商品期货型基金", lm(ret - rf ~ mkt_rf + smb + hml + umd + rmw + cma)] %>% summary()
fund[Category == "货币型基金", lm(ret - rf ~ mkt_rf + smb + hml + umd + rmw + cma)] %>% summary()
fund[Category == "其他", lm(ret - rf ~ mkt_rf + smb + hml + umd + rmw + cma)] %>% summary()
fund[Category == "FOF", lm(ret - rf ~ mkt_rf + smb + hml + umd + rmw + cma)] %>% summary()


