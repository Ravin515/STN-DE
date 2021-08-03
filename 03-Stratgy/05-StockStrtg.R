## 1.1 选上个月学习频率最高的10个stock，等比例购买（无交易成本）----
#f.stock.top10 <- f.rb.followers.followings[, unique(.SD)
#][, time.interval := difftime(from.created.at, to.created.at, unit = "day")
#][time.interval < 30, .SD
#][order(from.created.at, to.stock.symbol), .SD
#][, date.month := str_sub(from.created.at, start = 1L, end = 7L)
#][from.created.at > as.Date("2016-07-01"), .SD
#][, unique(.SD), .SDcols = c("from.cube.symbol", "from.created.at", "to.stock.symbol", "date.month")
#][, tag := .N, by = .(date.month, to.stock.symbol)
#][order(date.month, - tag, to.stock.symbol), .SD
#][, unique(.SD), .SDcols = c("to.stock.symbol", "tag", "date.month")
#][, stock.symbol := str_sub(to.stock.symbol, start = 3L, end = 8L)
#][stock.symbol %in% unique(Clsprc$stock.symbol), .SD
#][, .SD[1:10], by = .(date.month)
#][, date.month.lag := shift(date.month, type = "lead", n = 10)
#][, date.month.lag := fifelse(is.na(date.month.lag), "2018-07", date.month.lag)]

## 找出每只股票在前一个月最后一个交易日的price
#fs.1.lag.top10 <- Clsprc[, date.month := str_sub(date, start = 1L, end = 7L)
#][ f.stock.top10[, .(date.month, stock.symbol)], on = .(date.month, stock.symbol), nomatch = NA
#][order(date, stock.symbol), .SD
#][, .SD[(.N - 9):.N], by = .(date.month)
#][, date.month := shift(date.month, type = "lead", n = 10)
#][, date.month := fifelse(is.na(date.month), "2018-07", date.month)]

## 每只股票在持有的那个月的日度price
#fs.1.top10 <- Clsprc[, date.month := str_sub(date, start = 1L, end = 7L)
#][f.stock.top10[, .(date.month = date.month.lag, stock.symbol)], on = .(date.month, stock.symbol), nomatch = NA
#][order(date.month, date, stock.symbol), .SD]
#fs.revise.stock <- fs.1.top10[date.month %in% fs.1.top10[, .SD[.N < 10], by = .(date)
#][, unique(date.month)], .SD
#][, trdd.num := .N, by = .(stock.symbol)
#][trdd.num == min(trdd.num), unique(stock.symbol)]
#fs.revise.date<- fs.1.top10[date.month %in% fs.1.top10[, .SD[.N < 10], by = .(date)
#][, unique(date.month)], .SD
#][, unique(date)]
#fs.revise <- CJ(stock.symbol = fs.revise.stock, date = fs.revise.date)
#fs.revise <- Clsprc[, date.month := str_sub(date, start = 1L, end = 7L)
#][fs.revise, on = .(stock.symbol, date), nomatch = NA, roll = T]

#fs.1.top10<- rbindlist(list(fs.1.top10[!(stock.symbol %in% fs.revise$stock.symbol & date.month %in% fs.revise$date.month), .SD], fs.revise), use.names = T)

## 纵向拼接两个数据集
#fs.1.top10 <- rbindlist(list(fs.1.lag.top10, fs.1.top10), use.names = T)
#fs.1.top10 <- fs.1.top10[order(date, stock.symbol), .SD
#][, value := mean(Clsprc), by = .(date)]
#fs.1.monthly.top10 <- fs.1.top10[, unique(.SD), .SDcols = c('date', 'date.month', 'value')
#][, .(ret.monthly = value[.N] / value[1] - 1), by = .(date.month)]
#fs.1.daily.top10 <- fs.1.top10[, unique(.SD), .SDcols = c('date', 'date.month', 'value')
#][, .(ret.daily = value / shift(value, type = "lag") - 1, date), by = .(date.month)
#]

### 1.1.1 Newey West t-test
## 月度数据
#library(lmtest)
#fs.1.monthly.top10 <- indexmn[, .(date.month = Month, index_ret = Idxrtn)
#][fs.1.monthly.top10, on = .(date.month), nomatch = 0]
#fs.1.monthly.top10[, lm(ret.monthly - index_ret ~ 1) %>% coeftest()]

## 日度数据
#fs.1.daily.top10 <- index[, .(date, index_ret, index_value)
#][fs.1.daily.top10, on = .(date), nomatch = 0]
#fs.1.daily.top10[, lm(ret.daily - index_ret ~ 1) %>% coeftest()]

### 1.1.2 Alpha
## 月度数据
#fs.1.monthly.top10 <- fivefactor_monthly[fs.1.monthly.top10[, date.month := str_replace_all(date.month, "-", "")], on = .(date.month)]

#fs.1.monthly.top10[, lm(ret.monthly - rf ~ mkt_rf + smb + hml)] %>% summary()
#fs.1.monthly.top10[, lm(ret.monthly - rf ~ mkt_rf + smb + hml + umd)] %>% summary()
#fs.1.monthly.top10[, lm(ret.monthly - rf ~ mkt_rf + smb + hml + rmw + cma)] %>% summary()
#fs.1.monthly.top10[, lm(ret.monthly - rf ~ mkt_rf + smb + hml + umd + rmw + cma)] %>% summary()

## 日度数据
#fs.1.daily.top10 <- fivefactor_daily[fs.1.daily.top10, on = .(date)]
#fs.1.daily.top10[, lm(ret.daily - rf ~ mkt_rf + smb + hml)] %>% summary()
#fs.1.daily.top10[, lm(ret.daily - rf ~ mkt_rf + smb + hml + umd)] %>% summary()
#fs.1.daily.top10[, lm(ret.daily - rf ~ mkt_rf + smb + hml + rmw + cma)] %>% summary()
#fs.1.daily.top10[, lm(ret.daily - rf ~ mkt_rf + smb + hml + umd + rmw + cma)] %>% summary()

## 1.2 选每个月被follow最多的20只股票，等比例购买（无交易成本）----
#f.stock.top20 <- f.rb.followers.followings[, unique(.SD)
#][, time.interval := difftime(from.created.at, to.created.at, unit = "day")
#][time.interval < 30, .SD
#][order(from.created.at, to.stock.symbol), .SD
#][, date.month := str_sub(from.created.at, start = 1L, end = 7L)
#][from.created.at > as.Date("2016-07-01"), .SD
#][, unique(.SD), .SDcols = c("from.cube.symbol", "from.created.at", "to.stock.symbol", "date.month")
#][, tag := .N, by = .(date.month, to.stock.symbol)
#][order(date.month, - tag, to.stock.symbol), .SD
#][, unique(.SD), .SDcols = c("to.stock.symbol", "tag", "date.month")
#][, stock.symbol := str_sub(to.stock.symbol, start = 3L, end = 8L)
#][stock.symbol %in% unique(Clsprc$stock.symbol), .SD
#][, .SD[1:20], by = .(date.month)
#][, date.month.lag := shift(date.month, type = "lead", n = 20)
#][, date.month.lag := fifelse(is.na(date.month.lag), "2018-07", date.month.lag)]

## 找出每只股票在前一个月最后一个交易日的price
#fs.1.lag.top20 <- Clsprc[, date.month := str_sub(date, start = 1L, end = 7L)
#][f.stock.top20[, .(date.month, stock.symbol)], on = .(date.month, stock.symbol), nomatch = NA
#][order(date, stock.symbol), .SD
#][, .SD[(.N - 19):.N], by = .(date.month)
#][, date.month := shift(date.month, type = "lead", n = 20)
#][, date.month := fifelse(is.na(date.month), "2018-07", date.month)]

## 每只股票在持有的那个月的日度price
#fs.1.top20 <- Clsprc[, date.month := str_sub(date, start = 1L, end = 7L)
#][f.stock.top20[, .(date.month = date.month.lag, stock.symbol)], on = .(date.month, stock.symbol), nomatch = NA
#][order(date.month, date, stock.symbol), .SD]
#fs.revise.stock <- fs.1.top20[date.month %in% fs.1.top20[, .SD[.N < 20], by = .(date)
#][, unique(date.month)], .SD
#][, trdd.num := .N, by = .(stock.symbol)
#][trdd.num == min(trdd.num), unique(stock.symbol)]
#fs.revise.date <- fs.1.top20[date.month %in% fs.1.top20[, .SD[.N < 20], by = .(date)
#][, unique(date.month)], .SD
#][, unique(date)]
#fs.revise <- CJ(stock.symbol = fs.revise.stock, date = fs.revise.date)
#fs.revise <- Clsprc[, date.month := str_sub(date, start = 1L, end = 7L)
#][fs.revise, on = .(stock.symbol, date), nomatch = NA, roll = T]

#fs.1.top20 <- rbindlist(list(fs.1.top20[!(stock.symbol %in% fs.revise$stock.symbol & date.month %in% fs.revise$date.month), .SD], fs.revise), use.names = T)

## 纵向拼接两个数据集
#fs.1.top20<- rbindlist(list(fs.1.lag.top20, fs.1.top20), use.names = T)
#fs.1.top20 <- fs.1.top20[order(date, stock.symbol), .SD
#][, value := mean(Clsprc), by = .(date)]
#fs.1.monthly.top20 <- fs.1.top20[, unique(.SD), .SDcols = c('date', 'date.month', 'value')
#][, .(ret.monthly = value[.N] / value[1] - 1), by = .(date.month)]
#fs.1.daily.top20 <- fs.1.top20[, unique(.SD), .SDcols = c('date', 'date.month', 'value')
#][, .(ret.daily = value / shift(value, type = "lag") - 1, date), by = .(date.month)
#]

### 1.1 Newey West t-test
## 月度数据
#library(lmtest)
#fs.1.monthly.top20 <- indexmn[, .(date.month = Month, index_ret = Idxrtn)
#][fs.1.monthly.top20, on = .(date.month), nomatch = 0]
#fs.1.monthly.top20[, lm(ret.monthly - index_ret ~ 1) %>% coeftest()]

## 日度数据
#fs.1.daily.top20 <- index[, .(date, index_ret, index_value)
#][fs.1.daily.top20, on = .(date), nomatch = 0]
#fs.1.daily.top20[, lm(ret.daily - index_ret ~ 1) %>% coeftest()]

### 1.2 Alpha
## 月度数据
#fs.1.monthly.top20 <- fivefactor_monthly[fs.1.monthly.top20[, date.month := str_replace_all(date.month, "-", "")], on = .(date.month)]

#fs.1.monthly.top20[, lm(ret.monthly - rf ~ mkt_rf + smb + hml)] %>% summary()
#fs.1.monthly.top20[, lm(ret.monthly - rf ~ mkt_rf + smb + hml + umd)] %>% summary()
#fs.1.monthly.top20[, lm(ret.monthly - rf ~ mkt_rf + smb + hml + rmw + cma)] %>% summary()
#fs.1.monthly.top20[, lm(ret.monthly - rf ~ mkt_rf + smb + hml + umd + rmw + cma)] %>% summary()

## 日度数据
#fs.1.daily.top20 <- fivefactor_daily[fs.1.daily.top20, on = .(date)]
#fs.1.daily.top20[, lm(ret.daily - rf ~ mkt_rf + smb + hml)] %>% summary()
#fs.1.daily.top20[, lm(ret.daily - rf ~ mkt_rf + smb + hml + umd)] %>% summary()
#fs.1.daily.top20[, lm(ret.daily - rf ~ mkt_rf + smb + hml + rmw + cma)] %>% summary()
#fs.1.daily.top20[, lm(ret.daily - rf ~ mkt_rf + smb + hml + umd + rmw + cma)] %>% summary()

## 1.3 选每个月被follow最多的50只股票，等比例购买（无交易成本）----
#f.stock.top50 <- f.rb.followers.followings[, unique(.SD)
#][, time.interval := difftime(from.created.at, to.created.at, unit = "day")
#][time.interval < 30, .SD
#][order(from.created.at, to.stock.symbol), .SD
#][, date.month := str_sub(from.created.at, start = 1L, end = 7L)
#][from.created.at > as.Date("2016-07-01"), .SD
#][, unique(.SD), .SDcols = c("from.cube.symbol", "from.created.at", "to.stock.symbol", "date.month")
#][, tag := .N, by = .(date.month, to.stock.symbol)
#][order(date.month, - tag, to.stock.symbol), .SD
#][, unique(.SD), .SDcols = c("to.stock.symbol", "tag", "date.month")
#][, stock.symbol := str_sub(to.stock.symbol, start = 3L, end = 8L)
#][stock.symbol %in% unique(Clsprc$stock.symbol), .SD
#][, .SD[1:20], by = .(date.month)
#][, date.month.lag := shift(date.month, type = "lead", n = 20)
#][, date.month.lag := fifelse(is.na(date.month.lag), "2018-07", date.month.lag)]

## 找出每只股票在前一个月最后一个交易日的price
#fs.1.lag.top50 <- Clsprc[, date.month := str_sub(date, start = 1L, end = 7L)
#][f.stock.top50[, .(date.month, stock.symbol)], on = .(date.month, stock.symbol), nomatch = NA
#][order(date, stock.symbol), .SD
#][, .SD[(.N - 49):.N], by = .(date.month)
#][, date.month := shift(date.month, type = "lead", n = 50)
#][, date.month := fifelse(is.na(date.month), "2018-07", date.month)]

## 每只股票在持有的那个月的日度price
#fs.1.top50 <- Clsprc[, date.month := str_sub(date, start = 1L, end = 7L)
#][f.stock.top50[, .(date.month = date.month.lag, stock.symbol)], on = .(date.month, stock.symbol), nomatch = NA
#][order(date.month, date, stock.symbol), .SD]
#fs.revise.stock <- fs.1.top50[date.month %in% fs.1.top50[, .SD[.N < 50], by = .(date)
#][, unique(date.month)], .SD
#][, trdd.num := .N, by = .(stock.symbol)
#][trdd.num == min(trdd.num), unique(stock.symbol)]
#fs.revise.date <- fs.1.top50[date.month %in% fs.1.top50[, .SD[.N < 50], by = .(date)
#][, unique(date.month)], .SD
#][, unique(date)]
#fs.revise <- CJ(stock.symbol = fs.revise.stock, date = fs.revise.date)
#fs.revise <- Clsprc[, date.month := str_sub(date, start = 1L, end = 7L)
#][fs.revise, on = .(stock.symbol, date), nomatch = NA, roll = T]

#fs.1.top50 <- rbindlist(list(fs.1.top50[!(stock.symbol %in% fs.revise$stock.symbol & date.month %in% fs.revise$date.month), .SD], fs.revise), use.names = T)

## 纵向拼接两个数据集
#fs.1.top50 <- rbindlist(list(fs.1.lag.top50, fs.1.top50), use.names = T)
#fs.1.top50 <- fs.1.top50[order(date, stock.symbol), .SD
#][, value := mean(Clsprc), by = .(date)]
#fs.1.monthly.top50 <- fs.1.top50[, unique(.SD), .SDcols = c('date', 'date.month', 'value')
#][, .(ret.monthly = value[.N] / value[1] - 1), by = .(date.month)]
#fs.1.daily.top50 <- fs.1.top50[, unique(.SD), .SDcols = c('date', 'date.month', 'value')
#][, .(ret.daily = value / shift(value, type = "lag") - 1, date), by = .(date.month)
#]

### 1.1 Newey West t-test
## 月度数据
#library(lmtest)
#fs.1.monthly.top50 <- indexmn[, .(date.month = Month, index_ret = Idxrtn)
#][fs.1.monthly.top50, on = .(date.month), nomatch = 0]
#fs.1.monthly.top50[, lm(ret.monthly - index_ret ~ 1) %>% coeftest()]

## 日度数据
#fs.1.daily.top50 <- index[, .(date, index_ret, index_value)
#][fs.1.daily.top50, on = .(date), nomatch = 0]
#fs.1.daily.top50[, lm(ret.daily - index_ret ~ 1) %>% coeftest()]

### 1.2 Alpha
## 月度数据
#fs.1.monthly.top50 <- fivefactor_monthly[fs.1.monthly.top50[, date.month := str_replace_all(date.month, "-", "")], on = .(date.month)]

#fs.1.monthly.top50[, lm(ret.monthly - rf ~ mkt_rf + smb + hml)] %>% summary()
#fs.1.monthly.top50[, lm(ret.monthly - rf ~ mkt_rf + smb + hml + umd)] %>% summary()
#fs.1.monthly.top50[, lm(ret.monthly - rf ~ mkt_rf + smb + hml + rmw + cma)] %>% summary()
#fs.1.monthly.top50[, lm(ret.monthly - rf ~ mkt_rf + smb + hml + umd + rmw + cma)] %>% summary()

## 日度数据
#fs.1.daily.top50 <- fivefactor_daily[fs.1.daily.top50, on = .(date)]
#fs.1.daily.top50[, lm(ret.daily - rf ~ mkt_rf + smb + hml)] %>% summary()
#fs.1.daily.top50[, lm(ret.daily - rf ~ mkt_rf + smb + hml + umd)] %>% summary()
#fs.1.daily.top50[, lm(ret.daily - rf ~ mkt_rf + smb + hml + rmw + cma)] %>% summary()
#fs.1.daily.top50[, lm(ret.daily - rf ~ mkt_rf + smb + hml + umd + rmw + cma)] %>% summary()

## 2.1 选每个月被follow最多的10只股票，等比例购买（考虑交易成本）----
#fs.1.top10[]

## 2.3 选每个月被follow最多的20只股票，等比例购买（考虑交易成本）----
#fs.1.top20[]

## 2.4 选每个月被follow最多的50只股票，等比例购买（考虑交易成本）----
#fs.1.top50[]