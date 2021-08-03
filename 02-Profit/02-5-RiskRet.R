# 根据宇哥论文做出的结论，雪球平台整体的风险越来越高，在这里需要看follow后的整体风险对收益率的影响
library(styleer)
library(PerformanceAnalytics)
library(lfe)
library(quantreg)
library(texreg)
library(broom)
library(rmarkdown)
ld(f.main1, force = T)
ld(f.main2, force = T)
ld(f.cube.ret.sp)
ld(f.cube.rb.sp)
ld(f.user.cmt.num)
ld(stock.ratio.flat)

f.main2 <- f.main2[!is.na(value), .SD]
outlier <- f.cube.ret.sp[order(cube.symbol, date), .SD
    ][, ret := value / shift(value, type = "lag") - 1, by = .(cube.symbol)
    ][ret <= -0.1 | ret >= 0.1, unique(cube.symbol)]

# 导入各类因子----
url <- str_c(getwd(), "/data/Factors/")
file.names <- list.files(path = url, pattern = "*.txt|.csv")
for (i in file.names) {
    assign(str_sub(i, start = 1L, end = -5L), fread(str_c(url, toupper(i))))
}

# 日度指数文件----
url <- str_c(getwd(), "/data/Index")
index <- fbread(path = url, pattern = "*\\d.txt")
index <- index[, file_id := NULL
    ][Indexcd == "000300"
    ][, ':='(index_ret = Idxtrd08 / 100, index_value = Idxtrd05, date = as.Date(Idxtrd01))]

# 导入个股收盘价数据----
url <- str_c(getwd(), "/data/Clprc")
Clsprc <- fbread(path = url, pattern = "*.txt")
Clsprc[, stock.symbol := str_pad(Stkcd, 6, side = "left", pad = "0")
    ][, date := as.Date(Trddt)
    ][, ':='(file_id = NULL, Stkcd = NULL, Trddt = NULL)]

# 0. 控制变量的计算 ----
## 存续时间active.day：从一创建portfolio开始一直到portfolio结束时间点的时间（天）
## 交易频率trd.num：最近30天的交易次数的对数
## 发帖数量cmt.num：最近30天的发帖数量的对数
## 股票持有量stock.num：当天的股票持有量 （已通过stock.list进行计算）
## 市场的情况mmt：因子umd（直接在之后的factor中加入）

# 0.1 f.main1数据
cube.position <- f.main1[, .(cube.symbol, date)] # 将所有的cube的每一天的position全部取出

## 利用调仓的数据计算每天的交易数量
cube.trd.num <- f.cube.rb.sp[target.weight != prev.weight.adjusted & (!is.na(target.weight) | !is.na(prev.weight.adjusted)), .SD
    ][order(cube.symbol, created.at), .SD, .SDcols = c("cube.symbol", "created.at", "stock.symbol")
    ][, date := as.Date(created.at)
    ][, .(trd.num = .N), by = .(cube.symbol, date)
    ][cube.position, on = .(cube.symbol, date)
    ][, trd.num := fifelse(is.na(trd.num), 0, trd.num)]

## 利用f.user.cmt.num计算每天的发帖数量
cube.cmt.num <- f.user.cmt.num[cube.position, on = .(cube.symbol, date)
    ][, cmt.num := fifelse(is.na(cmt.num), 0, cmt.num)]

## 两个数据集进行合并
cube.ctrl.var <- cube.trd.num[cube.cmt.num, on = .(cube.symbol, date)]

# 与f.main1进行合并
f.main1 <- cube.ctrl.var[f.main1, on = .(cube.symbol, date)]

# 0.2 f.main2数据
cube.position <- f.main2[, .(cube.symbol, date)] # 将所有的cube的每一天的position全部取出

## 利用调仓的数据计算每天的交易数量
cube.trd.num <- f.cube.rb.sp[target.weight != prev.weight.adjusted & (!is.na(target.weight) | !is.na(prev.weight.adjusted)), .SD
    ][order(cube.symbol, created.at), .SD, .SDcols = c("cube.symbol", "created.at", "stock.symbol")
    ][, date := as.Date(created.at)
    ][, .(trd.num = .N), by = .(cube.symbol, date)
    ][cube.position, on = .(cube.symbol, date)
    ][, trd.num := fifelse(is.na(trd.num), 0, trd.num)]

## 利用f.user.cmt.num计算每天的发帖数量
cube.cmt.num <- f.user.cmt.num[cube.position, on = .(cube.symbol, date)
    ][, cmt.num := fifelse(is.na(cmt.num), 0, cmt.num)]

## 两个数据集进行合并
cube.ctrl.var <- cube.trd.num[cube.cmt.num, on = .(cube.symbol, date)]

# 与f.main1进行合并
f.main2 <- cube.ctrl.var[f.main2, on = .(cube.symbol, date)]

# 1. 看follow前后的risk对比 ----

# 1.1 计算利用过去一周平均的交易次数定义风险（放弃）----
### 整体持有时间变长，导致交易频率失效
#ld(f.cube.rb.sp.mst.1806, force = T)
#f.cube.rb.sp <- f.cube.rb.sp.mst.1806[, .(cube.symbol, created.at, stock.symbol, price, target.weight, prev.weight.adjusted)
#][order(cube.symbol, created.at), .SD]

### 将nan的数据填充并计算累积的买入和卖出的交易数量
#f.cube.rb.sp <- f.cube.rb.sp[!is.nan(target.weight) & !is.nan(prev.weight.adjusted), .SD
#][!(target.weight == 0 & prev.weight.adjusted == 0), .SD
#][, date := as.Date(created.at)
#][order(cube.symbol, created.at), .SD
#][, trd.num := 1, by = .(cube.symbol)
#][, trd.num.cum := cumsum(trd.num), by = .(cube.symbol)
#]

#order.num <- f.cube.rb.sp[, .SD[.N], by = .(cube.symbol, date)
#][, .(cube.symbol, date, trd.num.cum)
#][order(cube.symbol, date), .SD
#]
#f.main1.risk1 <- order.num[f.main1[order(cube.symbol, date)], on = .(cube.symbol, date), roll = T, rollends = c(F, T)]

#f.main1.risk1[, trd.num.7day := trd.num.cum - shift(trd.num.cum, type = "lag", n = 7L), by = .(cube.symbol)
#][, post.follow := fifelse(date >= follow.date, 1, 0), by = .(cube.symbol)
#][, ret := value/shift(value) - 1, by = .(cube.symbol)]

## 回归
#f.main1.risk1[!(cube.symbol %in% outlier) & post.follow == 0 | (post.follow == 1 & date - follow.date <= pre.period), felm(trd.num.7day ~ post.follow | cube.symbol + follow.date) %>% summary()]
#f.main1.risk1[!(cube.symbol %in% outlier) & post.follow == 0 | (post.follow == 1 & date - follow.date <= pre.period), felm(ret ~ trd.num.7day | cube.symbol + follow.date) %>% summary()]

# 1.2 利用cube30\60\90天之内的return的超额收益率波动率 ----

## 1.2.1 波动率前后比较（放弃）----
## 30 days (f.main1)
#f.main1.vol <- fivefactor_daily[, setnames(.SD, "trddy", "date")
    #][, date := as.Date(date)
    #][index[, .(date, index_value, index_ret)], on = .(date)
    #][f.main1[order(cube.symbol, date) & !is.na(value), .SD], on = .(date)
    #][, .SD[.N>30], by = .(cube.symbol)
    #][order(cube.symbol, date), .SD
    #][, ret := value/shift(value, type = "lag")-1, by = .(cube.symbol)
    #][, post.follow := fifelse(date >= follow.date, 1, 0), by = .(cube.symbol)
    #][, ret.30day := value / shift(value, type = "lag", 29) - 1, by = .(cube.symbol)
    #][, ret.30day.lag := shift(ret.30day, type = "lag", 1), by = .(cube.symbol)
    #][, vol.30day := frollapply(ret, 30, sd, na.rm = T)*30^0.5, by = .(cube.symbol)
    ##][!(cube.symbol %in% outlier), ':='(alpha = coef(lm(ret ~ index_ret))[1], beta = coef(lm(ret ~ index_ret))[2]), by = .(cube.symbol)
    ##][, ret.abnr := ret - alpha - beta * index_ret
    ##][, ret.abnr.cum := frollsum(ret.abnr, 30, na.rm = T), by = .(cube.symbol)
    ##][, ret.abnr.cum.lag := shift(ret.abnr.cum, type = "lag"), by = .(cube.symbol)
    ##][, ret.ex := (value / shift(value, 29, type = "lag") - 1) - (index_value / shift(index_value, 29, type = "lag") - 1), by = .(cube.symbol)
    ##][, ret.ex.lag := shift(ret.ex, type = "lag", n = 1), by = .(cube.symbol)
    ##][, vol.ex := frollapply(ret.ex, 30, sd, na.rm = T) * 30 ^ (0.5), by = .(cube.symbol)
    ##][, vol.ex.lag := shift(vol.ex, type = "lag", n = 1), by = .(cube.symbol)
    ##][, vol := frollapply(ret.abnr, 30, sd, na.rm = T) *30^(0.5), by = .(cube.symbol)
    #][, date.qrtr := str_c(year(date), quarter(date))
    #][, trd.num.30day := frollsum(trd.num, 30), by = .(cube.symbol) # 过去30天内的交易数量
    #][, stock.num.30day := frollmean(stock.num, 30), by = .(cube.symbol) # 过去30天内平均股票数量持有量
    #][, active.day := as.numeric(date - min(date), units = "weeks"), by = .(cube.symbol)
    #][, cmt.num.30day := frollsum(cmt.num, 30), by = .(cube.symbol) # 过去30天内的发帖数量
    #][, vol.30day.lag := shift(vol.30day, type = "lag", n = 1), by = .(cube.symbol)
    #]

## 30/60/90 days (f.main2)
#f.main2.vol <- fivefactor_daily[, setnames(.SD, "trddy", "date")
    #][, date := as.Date(date)
    #][index[, .(date, index_value, index_ret)], on = .(date)
    #][f.main2[order(cube.symbol, date) & !is.na(value), .SD], on = .(date)
    #][order(cube.symbol, date), .SD
    #][, ret := value / shift(value, type = "lag") - 1, by = .(cube.symbol)
    #][, .SD[.N > 90], by = .(cube.symbol)
    #][!(cube.symbol %in% outlier), ':='(alpha = coef(lm(ret ~ index_ret))[1], beta = coef(lm(ret ~ index_ret))[2]), by = .(cube.symbol)
    #][, ret.abnr := ret - alpha - beta * index_ret
    #][, ret.abnr.cum.30day := frollsum(ret.abnr, 30, na.rm = T), by = .(cube.symbol)
    #][, ret.abnr.cum.60day := frollsum(ret.abnr, 60, na.rm = T), by = .(cube.symbol)
    #][, ret.abnr.cum.90day := frollsum(ret.abnr, 90, na.rm = T), by = .(cube.symbol)
    #][, ret.abnr.cum.30day.lag := shift(ret.abnr.cum.30day, type = "lag"), by = .(cube.symbol)
    #][, ret.abnr.cum.60day.lag := shift(ret.abnr.cum.60day, type = "lag"), by = .(cube.symbol)
    #][, ret.abnr.cum.90day.lag := shift(ret.abnr.cum.90day, type = "lag"), by = .(cube.symbol)
    #][, vol.30day := frollapply(ret.abnr, 30, sd, na.rm = T) * 30 ^ (0.5), by = .(cube.symbol)
    #][, vol.60day := frollapply(ret.abnr, 60, sd, na.rm = T) * 60 ^ (0.5), by = .(cube.symbol)
    #][, vol.90day := frollapply(ret.abnr, 90, sd, na.rm = T) * 90 ^ (0.5), by = .(cube.symbol)
    #][, trd.num.30day := frollsum(trd.num, 30), by = .(cube.symbol) # 过去30天内的交易数量
    #][, stock.num.30day := frollmean(stock.num, 30), by = .(cube.symbol) # 过去30天内平均股票数量持有量
    #][, active.day := as.numeric(date - min(date), units = "weeks"), by = .(cube.symbol)
    #][, cmt.num.30day := frollsum(cmt.num, 30), by = .(cube.symbol) # 过去30天内的发帖数量
    #][, vol.30day.lag := shift(vol.30day, type = "lag"), by = .(cube.symbol)
    #][, vol.60day.lag := shift(vol.60day, type = "lag"), by = .(cube.symbol)
    #][, vol.90day.lag := shift(vol.90day, type = "lag"), by = .(cube.symbol)
    #][, date.qrtr := str_c(year(date), quarter(date))
    #][, oud.30day := frollmean(oud, 30, na.rm = T), by = .(cube.symbol)
    #][, ind.30day := frollmean(ind, 30, na.rm = T), by = .(cube.symbol)
    #][, ln.cntr.30day := frollmean(ln.cntr, 30, na.rm = T), by = .(cube.symbol)
    #][, oud.60day := frollmean(oud, 60, na.rm = T), by = .(cube.symbol)
    #][, ind.60day := frollmean(ind, 60, na.rm = T), by = .(cube.symbol)
    #][, ln.cntr.60day := frollmean(ln.cntr, 60, na.rm = T), by = .(cube.symbol)
    #][, oud.90day := frollmean(oud, 90, na.rm = T), by = .(cube.symbol)
    #][, ind.90day := frollmean(ind, 90, na.rm = T), by = .(cube.symbol)
    #][, ln.cntr.90day := frollmean(ln.cntr, 90, na.rm = T), by = .(cube.symbol)
    #][, ret.ex.30day := (value / shift(value, 29, type = "lag") - 1) - (index_value / shift(index_value, 29, type = "lag") - 1), by = .(cube.symbol)
    #][, ret.ex.30day.lag := shift(ret.ex.30day, type = "lag", n = 1), by = .(cube.symbol)
    #][, vol.ex.30day := frollapply(ret.ex.30day, 30, sd, na.rm = T) * 30 ^ (0.5), by = .(cube.symbol)
    #][, vol.ex.30day.lag := shift(vol.ex.30day, type = "lag", n = 1), by = .(cube.symbol)
    #][, ret.ex.60day := (value / shift(value, 59, type = "lag") - 1) - (index_value / shift(index_value, 59, type = "lag") - 1), by = .(cube.symbol)
    #][, ret.ex.60day.lag := shift(ret.ex.60day, type = "lag", n = 1), by = .(cube.symbol)
    #][, vol.ex.60day := frollapply(ret.ex.60day, 60, sd, na.rm = T) * 60 ^ (0.5), by = .(cube.symbol)
    #][, vol.ex.60day.lag := shift(vol.ex.60day, type = "lag", n = 1), by = .(cube.symbol)
    #][, ret.ex.90day := (value / shift(value, 89, type = "lag") - 1) - (index_value / shift(index_value, 89, type = "lag") - 1), by = .(cube.symbol)
    #][, ret.ex.90day.lag := shift(ret.ex.90day, type = "lag", n = 1), by = .(cube.symbol)
    #][, vol.ex.90day := frollapply(ret.ex.90day, 90, sd, na.rm = T) * 90 ^ (0.5), by = .(cube.symbol)
    #][, vol.ex.90day.lag := shift(vol.ex.90day, type = "lag", n = 1), by = .(cube.symbol)
    #]


#r1 <- f.main1.vol[!(cube.symbol %in% outlier) & (post.follow == 0 | (post.follow == 1 & date - follow.date <= pre.period)), felm(I(vol*100) ~ post.follow | cube.symbol + date.qrtr)] #%>% summary()
#r2 <- f.main1.vol[!(cube.symbol %in% outlier) & (post.follow == 0 | (post.follow == 1 & date - follow.date <= pre.period)), felm(I(vol * 100) ~ post.follow + active.day + cmt.num.30day + trd.num.30day + stock.num.30day | cube.symbol + date.qrtr)] # %>% summary()
#r3 <- f.main2.vol[!(cube.symbol %in% outlier), felm(I(vol.30day * 100) ~ log(ind.30day + 1) + log(oud.30day + 1) + I(ln.cntr.30day * 100) | cube.symbol + date.qrtr)] #%>% summary()
#r4 <- f.main2.vol[!(cube.symbol %in% outlier), felm(I(vol.30day * 100) ~ log(ind.30day + 1) + log(oud.30day + 1) + I(ln.cntr.30day * 100) + active.day + cmt.num.30day + trd.num.30day + stock.num.30day | cube.symbol + date.qrtr)] #%>% summary()
#r5 <- f.main2.vol[!(cube.symbol %in% outlier), felm(I(vol.60day * 100) ~ log(ind.60day + 1) + log(oud.60day + 1) + I(ln.cntr.60day * 100) | cube.symbol + date.qrtr)] #%>% summary()
#r6 <- f.main2.vol[!(cube.symbol %in% outlier), felm(I(vol.60day * 100) ~ log(ind.60day + 1) + log(oud.60day + 1) + I(ln.cntr.60day * 100) + active.day + cmt.num.30day + trd.num.30day + stock.num.30day | cube.symbol + date.qrtr)] #%>% summary()
#r7 <- f.main2.vol[!(cube.symbol %in% outlier), felm(I(vol.90day * 100) ~ log(ind.90day + 1) + log(oud.90day + 1) + I(ln.cntr.90day * 100) | cube.symbol + date.qrtr)] #%>% summary()
#r8 <- f.main2.vol[!(cube.symbol %in% outlier), felm(I(vol.90day * 100) ~ log(ind.90day + 1) + log(oud.90day + 1) + I(ln.cntr.90day * 100) + active.day + cmt.num.30day + trd.num.30day + stock.num.30day | cube.symbol + date.qrtr)] #%>% summary()

#list(r1, r2, r3, r4, r5, r6, r7, r8) %>%
    #htmlreg(
        #file = "5.8.html",
        #custom.header = list("Two-stage" = 1, "Two-stage" = 2, "Full-sample" = 3, "Full-sample" = 4,"Full-sample" = 5, "Full-sample" = 6, "Full-sample" = 7, "Full-sample" = 8),
##caption.above = TRUE,
        #include.rs = TRUE,
        #include.adjrs = FALSE,
        #custom.model.names = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)"),
        #custom.gof.rows = list(
##"Control Variables" = c("No", "Yes", "No", "Yes", "No", "Yes"),
                                            #"Trader FE" = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                                            #"Quarter FE" = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")
                                            #),
        #custom.coef.names = c("post-follow", "active day", "cmt-num-30day", "trd-num-30day", "stock-num-30day", "LI-30day", "LQ-30day", "PS-30day", "LI-60day", "LQ-60day", "PS-60day", "LI-90day", "LQ-90day", "PS-90day"),
##omit.coef = "active.day|cmt.num.30day|trd.num.30day|stock.num.30day",
        #reorder.coef = c(1, 6:14, 2:5),
        #caption.above = TRUE,
        #digits = 3,
        #inline.css = FALSE,
        #doctype = TRUE,
        #html.tag = TRUE,
        #head.tag = TRUE,
        #body.tag = TRUE,
        #center = FALSE,
#)

## 1.2.2 超额收益率与波动率之间的关系 ----
## two-stage
#r1 <- f.main1.vol[!(cube.symbol %in% outlier) & (post.follow == 0), felm(ret.abnr.cum ~ vol.lag + ret.abnr.cum.lag | cube.symbol + date.qrtr)] #%>% summary()
#r2 <- f.main1.vol[!(cube.symbol %in% outlier) & (post.follow == 1 & (date - follow.date <= pre.period)), felm(ret.abnr.cum ~ vol.lag + ret.abnr.cum.lag | cube.symbol + date.qrtr)] #%>% summary()
#r3 <- f.main1.vol[!(cube.symbol %in% outlier) & (post.follow == 0 | (post.follow == 1 & (date - follow.date <= pre.period))), felm(ret.abnr.cum ~ vol.lag + ret.abnr.cum.lag + post.follow + vol.lag * post.follow | cube.symbol + date.qrtr)] #%>% summary()
#r4 <- f.main1.vol[!(cube.symbol %in% outlier) & (post.follow == 0 | (post.follow == 1 & (date - follow.date <= pre.period))), felm(ret.abnr.cum ~ vol.lag + ret.abnr.cum.lag + post.follow + vol.lag * post.follow + active.day + cmt.num + trd.num + stock.num | cube.symbol + date.qrtr)] #%>% summary()
#r5 <- f.main1.vol[!(cube.symbol %in% outlier) & (post.follow == 0 | (post.follow == 1 & (date - follow.date <= pre.period))), felm(ret.abnr.cum ~ vol.lag + ret.abnr.cum.lag + post.follow + vol.lag * post.follow + post.follow + active.day + cmt.num.30day + trd.num.30day + stock.num.30day | cube.symbol + date.qrtr)] #%>% summary()
#r6 <- f.main1.vol[!(cube.symbol %in% outlier) & (post.follow == 0 | (post.follow == 1 & (date - follow.date <= pre.period))), felm(ret.abnr.cum ~ vol.lag + ret.abnr.cum.lag | cube.symbol + date.qrtr)] #%>% summary()
#r7 <- f.main1.vol[!(cube.symbol %in% outlier) & (post.follow == 0 | (post.follow == 1 & (date - follow.date <= pre.period))), felm(ret.abnr.cum ~ vol.lag + ret.abnr.cum.lag + active.day + cmt.num + trd.num + stock.num | cube.symbol + date.qrtr)] #%>% summary()
#r8 <- f.main1.vol[!(cube.symbol %in% outlier) & (post.follow == 0 | (post.follow == 1 & (date - follow.date <= pre.period))), felm(ret.abnr.cum ~ vol.lag + ret.abnr.cum.lag + active.day + cmt.num.30day + trd.num.30day + stock.num.30day | cube.symbol + date.qrtr)] #%>% summary()

#list(r1, r2, r3, r4, r5, r6, r7, r8) %>%
    #htmlreg(
        #file = "5.9.html",
        #custom.header = list("pre-follow" = 1, "post.follow" = 2, "two-stage" = 3, "two-stage" = 4, "two-stage" = 5, "two-stage" = 6, "two-stage" = 7, "two-stage" = 8),
##caption.above = TRUE,
        #include.rs = TRUE,
        #include.adjrs = FALSE,
        #custom.model.names = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)"),
        #custom.gof.rows = list(
##"Control Variables" = c("No", "Yes", "No", "Yes", "No", "Yes"),
                                            #"Trader FE" = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                                            #"Quarter FE" = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")
                                            #),
        #custom.coef.names = c("vol-30day.lag", "CAR.30day.lag", "post-follow", "vol.30day.lag * post-follow", "active day", "cmt-num", "trd-num", "stock-num", "cmt-num-30day", "trd-num-30day", "stock-num-30day"),
##omit.coef = "active.day|cmt.num.30day|trd.num.30day|stock.num.30day",
        #reorder.coef = c(1:11),
        #caption.above = TRUE,
        #digits = 4,
        #inline.css = FALSE,
        #doctype = TRUE,
        #html.tag = TRUE,
        #head.tag = TRUE,
        #body.tag = TRUE,
        #center = FALSE,
        #stars = c(0.001, 0.01, 0.05, 0.1)
            #)


## f.main2
#r1 <- f.main2.vol[!(cube.symbol %in% outlier), felm(ret.abnr.cum.30day ~ vol.30day.lag + ret.abnr.cum.30day.lag | cube.symbol + date.qrtr)] #%>% summary()
#r2 <- f.main2.vol[!(cube.symbol %in% outlier), felm(ret.abnr.cum.30day ~ vol.30day.lag + ret.abnr.cum.30day.lag + active.day + cmt.num + trd.num + stock.num | cube.symbol + date.qrtr)] #%>% summary()
#r3 <- f.main2.vol[!(cube.symbol %in% outlier), felm(ret.abnr.cum.30day ~ vol.30day.lag + ret.abnr.cum.30day.lag + active.day + cmt.num.30day + trd.num.30day + stock.num.30day | cube.symbol + date.qrtr)] #%>% summary()

#r4 <- f.main2.vol[!(cube.symbol %in% outlier), felm(ret.abnr.cum.60day ~ vol.60day.lag + ret.abnr.cum.60day.lag | cube.symbol + date.qrtr)]
#r5 <- f.main2.vol[!(cube.symbol %in% outlier), felm(ret.abnr.cum.60day ~ vol.60day.lag + ret.abnr.cum.60day.lag + active.day + cmt.num + trd.num + stock.num | cube.symbol + date.qrtr)]
#r6 <- f.main2.vol[!(cube.symbol %in% outlier), felm(ret.abnr.cum.60day ~ vol.60day.lag + ret.abnr.cum.60day.lag + active.day + cmt.num.30day + trd.num.30day + stock.num.30day | cube.symbol + date.qrtr)] #%>% summary()

#r7<- f.main2.vol[!(cube.symbol %in% outlier), felm(ret.abnr.cum.90day ~ vol.90day.lag + ret.abnr.cum.90day.lag | cube.symbol + date.qrtr)] #%>% summary()
#r8 <- f.main2.vol[!(cube.symbol %in% outlier), felm(ret.abnr.cum.90day ~ vol.90day.lag + ret.abnr.cum.90day.lag + active.day + cmt.num + trd.num + stock.num | cube.symbol + date.qrtr)]
#r9 <- f.main2.vol[!(cube.symbol %in% outlier), felm(ret.abnr.cum.90day ~ vol.90day.lag + ret.abnr.cum.90day.lag + active.day + cmt.num.30day + trd.num.30day + stock.num.30day | cube.symbol + date.qrtr)] #%>% summary()

#list(r1, r2, r3, r4, r5, r6, r7, r8, r9) %>%
    #htmlreg(
        #file = "6.10.html",
        #custom.header = list("CAR.30day" = 1:3, "CAR.60day" = 4:6, "CAR.90day" = 7:9),
##caption.above = TRUE,
        #include.rs = TRUE,
        #include.adjrs = FALSE,
        #custom.model.names = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)", "(9)"),
        #custom.gof.rows = list(
##"Control Variables" = c("No", "Yes", "No", "Yes", "No", "Yes"),
                                            #"Trader FE" = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                                            #"Quarter FE" = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")
                                            #),
        #custom.coef.names = c("vol.30day.lag", "CAR.30day.lag", "active.day", "cmt.num", "trd.num", "stock.num", "cmt.num.30day", "trd.num.30day", "stock.num.30day", "vol.60day.lag", "CAR.60day.lag", "vol.90day.lag", "CAR.90day.lag"),
##omit.coef = "active.day|cmt.num.30day|trd.num.30day|stock.num.30day",
        #reorder.coef = c(1:2, 10:13, 3:9),
        #caption.above = TRUE,
        #digits = 4,
        #inline.css = FALSE,
        #doctype = TRUE,
        #html.tag = TRUE,
        #head.tag = TRUE,
        #body.tag = TRUE,
        #center = FALSE,
        #stars = c(0.001, 0.01, 0.05, 0.1)
            #)

## 分位数回归
#rq1.vol <- f.main1.vol[!(cube.symbol %in% outlier) & (post.follow == 0 | (post.follow == 1 & (date - follow.date <= pre.period))), rq(ret.abnr.cum ~ vol.lag + ret.abnr.cum.lag + active.day + cmt.num + trd.num + stock.num + cmt.num.30day + trd.num.30day + stock.num.30day, tau = seq(0.1, 0.9, 0.1), method = "sfn", data = .SD)]

#rq2.vol.30 <- f.main2.vol[!(cube.symbol %in% outlier), rq(ret.abnr.cum.30day ~ vol.30day.lag + ret.abnr.cum.30day.lag + active.day + cmt.num + trd.num + stock.num + cmt.num.30day + trd.num.30day + stock.num.30day, tau = seq(0.1, 0.9, 0.1), method = "pfn", data = .SD)]
#rq1 <- summary(rq2.vol.30)
#rq1.coef <- lapply(rq1, function(x) {
    #x$coefficients[2, c(1, 2, 4)]
#})
#rq2.vol.60 <- f.main2.vol[!(cube.symbol %in% outlier), rq(ret.abnr.cum.60day ~ vol.60day.lag + ret.abnr.cum.60day.lag + active.day + cmt.num + trd.num + stock.num + cmt.num.30day + trd.num.30day + stock.num.30day, tau = seq(0.1, 0.9, 0.1), method = "pfn", data = .SD)]
#rq2 <- summary(rq2.vol.60)
#rq2.coef <- lapply(rq2, function(x) {
    #x$coefficients[2, c(1,2,4)]
#})

#rq2.vol.90 <- f.main2.vol[!(cube.symbol %in% outlier), rq(ret.abnr.cum.90day ~ vol.90day.lag + ret.abnr.cum.90day.lag + active.day + cmt.num + trd.num + stock.num + cmt.num.30day + trd.num.30day + stock.num.30day, tau = seq(0.1, 0.9, 0.1), method = "pfn", data = .SD)]
#rq3 <- summary(rq2.vol.90)
#rq3.coef <- lapply(rq3, function(x) {
    #x$coefficients[2, c(1, 2, 4)]
#})
#rq1.coef <- as.data.table(rq1.coef)
#rq2.coef <- as.data.table(rq2.coef)
#rq3.coef <- as.data.table(rq3.coef)
#fwrite(rq1.coef, "rq1.csv")
#fwrite(rq2.coef, "rq2.csv")
#fwrite(rq3.coef, "rq3.csv")


# 1.3 利用cube30\60\90天之内的return的对数超额收益率波动率 ----

## 1.3.1 对数波动率前后比较 （Given Up）----
## f.main1 (30 days)
#f.main1.lvol <- f.main1.vol[!(cube.symbol %in% outlier), .SD
    #][order(cube.symbol, date), .SD
    #][, ret.log := log(value) - log(shift(value, type = "lag")), by = .(cube.symbol)
    #][, index.ret.log := log(index_value) - log(shift(index_value, type = "lag")), by = .(cube.symbol)
    #][!(cube.symbol %in% outlier), ':='(alpha = coef(lm(ret.log ~ index.ret.log))[1], beta = coef(lm(ret.log ~ index.ret.log))[2]), by = .(cube.symbol)
    #][, ret.ex.log := ret.log - alpha - beta * index.ret.log, by = .(cube.symbol)
    #][, ret.ex.cum.30day := frollsum(ret.ex.log, 30, na.rm = T), by = .(cube.symbol)
    #][, ret.ex.cum.30day.lag := shift(ret.ex.cum.30day, type = "lag", n = 1), by = .(cube.symbol)
    #][, vol.30day := frollapply(ret.ex.cum.30day, 30, sd, na.rm = T) * (30 ^ 0.5), by = .(cube.symbol)
    #][, vol.30day.lag := shift(vol.30day, type = "lag", n = 1), by = .(cube.symbol)
    #]

#f.main1.lvol[(post.follow == 0 | (post.follow == 1 & (date - follow.date <= pre.period))) & !(cube.symbol %in% outlier), felm(vol.30day ~ post.follow | cube.symbol + date.qrtr)] %>% summary()
#f.main1.lvol[(post.follow == 0 | (post.follow == 1 & (date - follow.date <= pre.period))) & !(cube.symbol %in% outlier), felm(vol.30day ~ post.follow + active.day + cmt.num + trd.num + stock.num | cube.symbol + date.qrtr)] %>% summary()
#f.main1.lvol[(post.follow == 0 | (post.follow == 1 & (date - follow.date <= pre.period))) & !(cube.symbol %in% outlier), felm(vol.30day ~ post.follow + active.day + cmt.num.30day + trd.num.30day + stock.num.30day | cube.symbol + date.qrtr)] %>% summary()
#f.main1.lvol[!(cube.symbol %in% outlier), felm(vol.30day ~ post.follow | cube.symbol + date.qrtr)] %>% summary()
#f.main1.lvol[!(cube.symbol %in% outlier), felm(vol.30day ~ post.follow + active.day + cmt.num + trd.num + stock.num | cube.symbol + date.qrtr)] %>% summary()
#f.main1.lvol[!(cube.symbol %in% outlier), felm(vol.30day ~ post.follow + active.day + cmt.num.30day + trd.num.30day + stock.num.30day | cube.symbol + date.qrtr)] %>% summary()

## f.main2 (30/60/90 days)
#f.main2.lvol <- f.main2.vol[!(cube.symbol %in% outlier), .SD
    #][order(cube.symbol, date), .SD
    #][, .SD[.N > 90], by = .(cube.symbol)
    #][, ret.log := log(value) - log(shift(value, type = "lag")), by = .(cube.symbol)
    #][, index.ret.log := log(index_value) - log(shift(index_value, type = "lag")), by = .(cube.symbol)
    #][, ':='(alpha = coef(lm(ret.log ~ index.ret.log))[1], beta = coef(lm(ret.log ~ index.ret.log))[2]), by = .(cube.symbol)
    #][, ret.ex.log := ret.log - alpha - beta * index.ret.log, by = .(cube.symbol)
    #][, ret.ex.cum.30day := frollsum(ret.ex.log, 30, na.rm = T), by = .(cube.symbol)
    #][, ret.ex.cum.30day.lag := shift(ret.ex.cum.30day, type = "lag", n = 1), by = .(cube.symbol)
    #][, vol.30day := frollapply(ret.ex.log, 30, sd, na.rm = T) * (30 ^ 0.5), by = .(cube.symbol)
    #][, vol.30day.lag := shift(vol.30day, type = "lag", n = 1), by = .(cube.symbol)
    #][, ret.ex.cum.60day := frollsum(ret.ex.log, 60, na.rm = T), by = .(cube.symbol)
    #][, ret.ex.cum.60day.lag := shift(ret.ex.cum.60day, type = "lag", n = 1), by = .(cube.symbol)
    #][, vol.60day := frollapply(ret.ex.log, 60, sd, na.rm = T) * (60 ^ 0.5), by = .(cube.symbol)
    #][, vol.60day.lag := shift(vol.60day, type = "lag", n = 1), by = .(cube.symbol)
    #][, ret.ex.cum.90day := frollsum(ret.ex.log, 90, na.rm = T), by = .(cube.symbol)
    #][, ret.ex.cum.90day.lag := shift(ret.ex.cum.90day, type = "lag", n = 1), by = .(cube.symbol)
    #][, vol.90day := frollapply(ret.ex.log, 90, sd, na.rm = T) * (90 ^ 0.5), by = .(cube.symbol)
    #][, vol.90day.lag := shift(vol.90day, type = "lag", n = 1), by = .(cube.symbol)
    #]

## 1.3.2 对数波动率与收益率的关系（Given UP） ----
#f.main1.lvol[(post.follow == 0 | (post.follow == 1 & (date - follow.date <= pre.period))), felm(ret.ex.cum.30day ~ vol.30day.lag + ret.ex.cum.30day.lag | cube.symbol + date.qrtr)] %>% summary()
#f.main1.lvol[(post.follow == 0 | (post.follow == 1 & (date - follow.date <= pre.period))), felm(ret.ex.cum.30day ~ vol.30day.lag + ret.ex.cum.30day.lag + active.day + cmt.num + trd.num + stock.num | cube.symbol + date.qrtr)] %>% summary()
#f.main1.lvol[(post.follow == 0 | (post.follow == 1 & (date - follow.date <= pre.period))), felm(ret.ex.cum.30day ~ vol.30day.lag + ret.ex.cum.30day.lag + active.day + cmt.num.30day + trd.num.30day + stock.num.30day | cube.symbol + date.qrtr)] %>% summary()
#f.main2.lvol[, felm(ret.ex.cum.30day ~ vol.30day.lag + ret.ex.cum.30day.lag | cube.symbol + date.qrtr) %>% summary()]
#f.main2.lvol[, felm(ret.ex.cum.30day ~ vol.30day.lag + ret.ex.cum.30day.lag + active.day + cmt.num + trd.num + stock.num | cube.symbol + date.qrtr) %>% summary()]
#f.main2.lvol[, felm(ret.ex.cum.30day ~ vol.30day.lag + ret.ex.cum.30day.lag + active.day + cmt.num.30day + trd.num.30day + stock.num.30day | cube.symbol + date.qrtr) %>% summary()]
#f.main2.lvol[, felm(ret.ex.cum.60day ~ vol.60day.lag + ret.ex.cum.60day.lag | cube.symbol + date.qrtr) %>% summary()]
#f.main2.lvol[, felm(ret.ex.cum.60day ~ vol.60day.lag + ret.ex.cum.60day.lag + active.day + cmt.num + trd.num + stock.num | cube.symbol + date.qrtr) %>% summary()]
#f.main2.lvol[, felm(ret.ex.cum.60day ~ vol.60day.lag + ret.ex.cum.60day.lag + active.day + cmt.num.30day + trd.num.30day + stock.num.30day | cube.symbol + date.qrtr) %>% summary()]
#f.main2.lvol[, felm(ret.ex.cum.90day ~ vol.90day.lag + ret.ex.cum.90day.lag | cube.symbol + date.qrtr) %>% summary()]
#f.main2.lvol[, felm(ret.ex.cum.90day ~ vol.90day.lag + ret.ex.cum.90day.lag + active.day + cmt.num + trd.num + stock.num | cube.symbol + date.qrtr) %>% summary()]
#f.main2.lvol[, felm(ret.ex.cum.90day ~ vol.90day.lag + ret.ex.cum.90day.lag + active.day + cmt.num.30day + trd.num.30day + stock.num.30day | cube.symbol + date.qrtr) %>% summary()]

## 分位数回归
#rq1.lvol <- f.main1.lvol[!(cube.symbol %in% outlier) & (post.follow == 0 | (post.follow == 1 & (date - follow.date <= pre.period))), rq(ret.abnr.cum ~ vol.lag + ret.abnr.cum.lag + active.day + cmt.num + trd.num + stock.num + cmt.num.30day + trd.num.30day + stock.num.30day, tau = seq(0.1, 0.9, 0.1), method = "sfn", data = .SD)]

#rq2.lvol.30 <- f.main2.lvol[!(cube.symbol %in% outlier), rq(ret.abnr.cum.30day ~ vol.30day.lag + ret.abnr.cum.30day.lag + active.day + cmt.num + trd.num + stock.num + cmt.num.30day + trd.num.30day + stock.num.30day + as.factor(cube.symbol), tau = seq(0.1, 0.9, 0.1), method = "sfn", data = .SD)]


#rq2.lvol.60 <- f.main2.lvol[!(cube.symbol %in% outlier), rq(ret.abnr.cum.60day ~ vol.60day.lag + ret.abnr.cum.60day.lag + active.day + cmt.num + trd.num + stock.num + cmt.num.30day + trd.num.30day + stock.num.30day + as.factor(cube.symbol), tau = seq(0.1, 0.9, 0.1), method = "sfn", data = .SD)]

#rq2.lvol.90 <- f.main2.lvol[!(cube.symbol %in% outlier), rq(ret.abnr.cum.90day ~ vol.90day.lag + ret.abnr.cum.90day.lag + active.day + cmt.num + trd.num + stock.num + cmt.num.30day + trd.num.30day + stock.num.30day + as.factor(cube.symbol), tau = seq(0.1, 0.9, 0.1), method = "sfn", data = .SD)]

## 1.4 波动率前后比较 (cube持有的股票的历史波动率) (放弃)----
## 计算每一只股票15/30/60/90天的历史波动率
#stock.vol <- Clsprc[order(stock.symbol, date), .SD
#][, ret := Clsprc/shift(Clsprc, type = "lag"), by = .(stock.symbol)
#][, .(hist.vol.15day = frollapply(ret, 15, sd, na.rm = T), hist.vol.30day = frollapply(ret, 30, sd, na.rm = T), hist.vol.60day = frollapply(ret, 60, sd, na.rm = T), hist.vol.90day = frollapply(ret, 90, sd, na.rm = T), date), by = .(stock.symbol)]

## 分别提取f.main1和f.main2的stock.list
## 与stock.vol进行合并
#f.main1.stock <- f.main1[, .(cube.symbol, date, stock.list)
#][, .(stock.symbol.tag = unlist(stock.list)), by = .(cube.symbol, date)
#][, stock.symbol := str_sub(stock.symbol.tag, start = 3L, end = 8L)
#]
#f.main1.svol <- stock.vol[f.main1.stock, on = .(stock.symbol, date)
##][rowSums(!is.na(cube.stock.vol[, 2:5])) != 0, .SD
#][, str_c("shist.vol.", c("15", "30", "60", "90"), "day") := lapply(.SD[, 2:5], mean, na.rm = T), by = .(cube.symbol, date)
#][, unique(.SD), .SDcols = str_c("shist.vol.", c("15", "30", "60", "90"), "day"), by = .(cube.symbol, date)]

#f.main2.stock <- f.main2[, .(cube.symbol, date, stock.list)
#][, .(stock.symbol.tag = unlist(stock.list)), by = .(cube.symbol, date)
#][, stock.symbol := str_sub(stock.symbol.tag, start = 3L, end = 8L)
#]
#f.main2.svol <- stock.vol[f.main2.stock, on = .(stock.symbol, date)
##][rowMeans(!is.na(cube.stock.vol[, 4:7])) != 0, .SD
#][, str_c("shist.vol.", c("15", "30", "60", "90"), "day") := lapply(.SD[, 2:5], mean, na.rm = T), by = .(cube.symbol, date)
#][, unique(.SD), .SDcols = str_c("shist.vol.", c("15", "30", "60", "90"), "day"), by = .(cube.symbol, date)]


## 与f.main1和f.main2合并
#f.main1.shist <- f.main1.svol[f.main1, on = .(cube.symbol, date)
#][order(cube.symbol, date), .SD
#][, post.follow := fifelse(date >= follow.date, 1, 0), by = .(cube.symbol)
#][, ret := value / shift(value, type = "lag") - 1, by = .(cube.symbol)
#][, ret.roll.15day := value / shift(value, type = "lag", n = 14) - 1, by = .(cube.symbol)
#][, ret.roll.30day := value / shift(value, type = "lag", n = 29) - 1, by = .(cube.symbol)
#][, ret.roll.60day := value / shift(value, type = "lag", n = 59) - 1, by = .(cube.symbol)
#][, ret.roll.90day := value / shift(value, type = "lag", n = 89) - 1, by = .(cube.symbol)
#][, date.qrtr := str_c(year(date), quarter(date))
#][, trd.num.30day := frollsum(trd.num, 30), by = .(cube.symbol) # 过去30天内的交易数量
#][, stock.num.30day := frollmean(stock.num, 30), by = .(cube.symbol) # 过去30天内平均股票数量持有量
#][, active.day := as.numeric(date - min(date), units = "weeks"), by = .(cube.symbol)
#][, cmt.num.30day := frollsum(cmt.num, 30), by = .(cube.symbol) # 过去30天内的发帖数量
#]

#f.main2.shist <- f.main2.svol[f.main2, on = .(cube.symbol, date)
#][order(cube.symbol, date), .SD
#][, ret := value / shift(value, type = "lag") - 1, by = .(cube.symbol)
#][, ret.roll.30day := value / shift(value, type = "lag", n = 29) - 1, by = .(cube.symbol)
#][, ret.roll.60day := value / shift(value, type = "lag", n = 59) - 1, by = .(cube.symbol)
#][, ret.roll.90day := value / shift(value, type = "lag", n = 89) - 1, by = .(cube.symbol)
#][, date.qrtr := str_c(year(date), quarter(date))
#][, trd.num.30day := frollsum(trd.num, 30), by = .(cube.symbol) # 过去30天内的交易数量
#][, stock.num.30day := frollmean(stock.num, 30), by = .(cube.symbol) # 过去30天内平均股票数量持有量
#][, active.day := as.numeric(date - min(date), units = "weeks"), by = .(cube.symbol)
#][, cmt.num.30day := frollsum(cmt.num, 30), by = .(cube.symbol) # 过去30天内的发帖数量
#]



#f.main1.shist[!(cube.symbol %in% outlier) & ((post.follow == 0 & follow.date - date <= pre.period - 15) | (post.follow == 1 & date - follow.date <= pre.period)), felm(shist.vol.15day ~ post.follow | cube.symbol + date.qrtr)] %>% summary()
#f.main1.shist[!(cube.symbol %in% outlier) & ((post.follow == 0 & follow.date - date <= pre.period - 15) | (post.follow == 1 & date - follow.date <= pre.period)), felm(shist.vol.15day ~ post.follow + active.day + cmt.num + trd.num + stock.num | cube.symbol + date.qrtr)] %>% summary()
#f.main1.shist[!(cube.symbol %in% outlier) & ((post.follow == 0 & follow.date - date <= pre.period - 15) | (post.follow == 1 & date - follow.date <= pre.period)), felm(shist.vol.15day ~ post.follow + active.day + cmt.num + trd.num + stock.num + cmt.num.30day + trd.num.30day + stock.num.30day | cube.symbol + date.qrtr)] %>% summary()
#f.main1.shist[!(cube.symbol %in% outlier) & ((post.follow == 0 & follow.date - date <= pre.period - 15) | (post.follow == 1)), felm(shist.vol.15day ~ post.follow | cube.symbol + date.qrtr)] %>% summary()
#f.main1.shist[!(cube.symbol %in% outlier) & ((post.follow == 0 & follow.date - date <= pre.period - 15) | (post.follow == 1)), felm(shist.vol.15day ~ post.follow + active.day + cmt.num + trd.num + stock.num | cube.symbol + date.qrtr)] %>% summary()
#f.main1.shist[!(cube.symbol %in% outlier) & ((post.follow == 0 & follow.date - date <= pre.period - 15) | (post.follow == 1)), felm(shist.vol.15day ~ post.follow + active.day + cmt.num + trd.num + stock.num + cmt.num.30day + trd.num.30day + stock.num.30day | cube.symbol + date.qrtr)] %>% summary()
#f.main2.shist[!(cube.symbol %in% outlier), felm(shist.vol.30day ~ log(ind + 1) + log(oud + 1) + I(ln.cntr * 1000) | cube.symbol + date.qrtr)] %>% summary()
#f.main2.shist[!(cube.symbol %in% outlier), felm(shist.vol.30day ~ log(ind + 1) + log(oud + 1) + I(ln.cntr * 1000) + active.day + cmt.num + trd.num + stock.num + cmt.num.30day + trd.num.30day + stock.num.30day | cube.symbol + date.qrtr)] %>% summary()
#f.main2.shist[!(cube.symbol %in% outlier), felm(shist.vol.60day ~ log(ind + 1) + log(oud + 1) + I(ln.cntr * 1000) | cube.symbol + date.qrtr)] %>% summary()
#f.main2.shist[!(cube.symbol %in% outlier), felm(shist.vol.60day ~ log(ind + 1) + log(oud + 1) + I(ln.cntr * 1000) + active.day + cmt.num + trd.num + stock.num + cmt.num.30day + trd.num.30day + stock.num.30day | cube.symbol + date.qrtr)] %>% summary()
#f.main2.shist[!(cube.symbol %in% outlier), felm(shist.vol.90day ~ log(ind + 1) + log(oud + 1) + I(ln.cntr * 1000) | cube.symbol + date.qrtr)] %>% summary()
#f.main2.shist[!(cube.symbol %in% outlier), felm(shist.vol.90day ~ log(ind + 1) + log(oud + 1) + I(ln.cntr * 1000) + active.day + cmt.num + trd.num + stock.num + cmt.num.30day + trd.num.30day + stock.num.30day | cube.symbol + date.qrtr)] %>% summary()



#f.main1.shist[!(cube.symbol %in% outlier) & ((post.follow == 0 & follow.date - date <= pre.period - 15) | (post.follow == 1)), felm(ret.roll.15day ~ shist.vol.15day | cube.symbol + date.qrtr)] %>% summary()
#f.main1.shist[!(cube.symbol %in% outlier) & ((post.follow == 0 & follow.date - date <= pre.period - 15) | (post.follow == 1)), felm(ret.roll.15day ~ shist.vol.15day + active.day + cmt.num + trd.num + stock.num | cube.symbol + date.qrtr)] %>% summary()
#f.main1.shist[!(cube.symbol %in% outlier) & ((post.follow == 0 & follow.date - date <= pre.period - 15) | (post.follow == 1)), felm(ret.roll.15day ~ shist.vol.15day + active.day + cmt.num + trd.num + stock.num + cmt.num.30day + trd.num.30day + stock.num.30day | cube.symbol + date.qrtr)] %>% summary()
#f.main2.shist[!(cube.symbol %in% outlier), felm(ret.roll.30day ~ shist.vol.30day | cube.symbol + date.qrtr)] %>% summary()
#f.main2.shist[!(cube.symbol %in% outlier), felm(ret.roll.30day ~ shist.vol.30day + active.day + cmt.num + trd.num + stock.num + cmt.num.30day + trd.num.30day + stock.num.30day | cube.symbol + date.qrtr)] %>% summary()
#f.main2.shist[!(cube.symbol %in% outlier), felm(ret.roll.60day ~ shist.vol.60day | cube.symbol + date.qrtr)] %>% summary()
#f.main2.shist[!(cube.symbol %in% outlier), felm(ret.roll.60day ~ shist.vol.60day + active.day + cmt.num + trd.num + stock.num + cmt.num.30day + trd.num.30day + stock.num.30day | cube.symbol + date.qrtr)] %>% summary()
#f.main2.shist[!(cube.symbol %in% outlier), felm(ret.roll.90day ~ shist.vol.90day | cube.symbol + date.qrtr)] %>% summary()
#f.main2.shist[!(cube.symbol %in% outlier), felm(ret.roll.90day ~ shist.vol.90day + active.day + cmt.num + trd.num + stock.num + cmt.num.30day + trd.num.30day + stock.num.30day | cube.symbol + date.qrtr)] %>% summary()


## 2. 证明风险升高价格下降因为受到偏度影响 （GIVEN UP）----
## 利用management science文章所谓的模型进行检验
## 从follow前后来看
#f.main1.vol[!(cube.symbol %in% outlier) & (post.follow == 0 | (post.follow == 1 & (date - follow.date <= pre.period))), .(cube.sk = unique(skewness(ret.abnr.cum, na.rm = T, method = "fisher"))), by = .(cube.symbol, post.follow)
    #][, unique(.SD)
    ##][, .SD[sum(!is.na(cube.sk)) == 2], by = .(cube.symbol)
    #][, quantile(cube.sk, probs = seq(0.1, 0.9, 0.1), na.rm = T), by = .(post.follow)
    #][, qtl := rep(seq(0.1, 0.9, 0.1), times = 2)
    #][, post.follow := fifelse(post.follow == 1, "post.follow", "pre.follow")
    #][, unique(.SD)
    #][, dcast(.SD, post.follow ~  qtl, value.var = "V1" )] %>% fwrite("qtl.1.csv")

##f.main1.vol[!(cube.symbol %in% outlier) & (post.follow == 0 | (post.follow == 1 & (date - follow.date <= pre.period))), .(cube.sk = unique(skewness(log(ret.abnr.cum + 1), na.rm = T, method = "fisher"))), by = .(cube.symbol, post.follow)
    ##][, unique(.SD)
    ###][, .SD[sum(!is.na(cube.sk)) == 2], by = .(cube.symbol)
    ##][, summary(cube.sk, na.rm = T)]

#f.main1.vol[!(cube.symbol %in% outlier) & (post.follow == 0 | (post.follow == 1 & (date - follow.date <= pre.period))), .(cube.sk = unique(skewness(ret.abnr.cum, na.rm =T, method = "fisher"))), by = .(cube.symbol, post.follow)
    #][, post.follow := fifelse(post.follow == 1, "Post-follow", "Pre-follow")
    #][, unique(.SD)
    ##][, .SD[sum(!is.na(cube.sk)) == 2], by = .(cube.symbol)
    #][, ggplot(.SD, aes(x = cube.sk, colour = post.follow, fill = post.follow)) +
                            #geom_line(stat = "density", size = 1) +
                            #theme_grey() +
                            #scale_colour_manual(values = c("#CC6666", "#7777DD")) +
                            #labs(x = "偏度值", y = "Density") +
                            #scale_fill_manual(values = c("#CC6666", "#7777DD"),
                                                        #name = "Stage",
                                                        #breaks = c("Post-follow", "Pre-follow"),
                                                        #labels = c("Post-follow", "Pre-follow")) +
        #theme(
                                    #legend.title = element_blank(),
                                    #panel.border = element_rect(linetype = 1, fill = NA),
                                    #legend.position = "bottom",
                                    #legend.spacing.x = unit(0.1, 'cm'),
                                    #legend.spacing.y = unit(2, 'cm'),
                                    #legend.box = "horizontal",
                                    #legend.box.background = element_rect(size = 1, colour = "black", fill = "white"),
                                    #legend.key = element_rect(size = 0.5, colour = "black", fill = "white"),
                                    #legend.key.size = unit(0.5, 'cm')
                                    #)

        #]
#ggsave("6.2.tiff", device = "tiff", dpi = 300, width = 5.5, height = 4)

## 从整体的f.main2来看

#f.main2.vol[!(cube.symbol %in% outlier), .(cube.sk.30day = unique(skewness(ret.abnr.cum.30day, na.rm = T, method = "fisher")),
                       #cube.sk.60day = unique(skewness(ret.abnr.cum.60day, na.rm = T, method = "fisher")),
                       #cube.sk.90day = unique(skewness(ret.abnr.cum.90day, na.rm = T, method = "fisher"))
                       #), by = cube.symbol
                       #][, unique(.SD)
                       #][, lapply(.SD[, 2:4], quantile, probs = seq(0.1, 0.9, 0.1), na.rm = T)
                       #][, transpose(.SD)] %>% fwrite("qtl.2.csv")

#f.main2.vol[!(cube.symbol %in% outlier),
                     #.(cube.sk.30day = unique(skewness(log(ret.abnr.cum.30day + 1), na.rm = T, method = "fisher")),
                       #cube.sk.60day = unique(skewness(log(ret.abnr.cum.60day + 1), na.rm = T, method = "fisher")),
                       #cube.sk.90day = unique(skewness(log(ret.abnr.cum.90day + 1), na.rm = T, method = "fisher"))
                       #), by = cube.symbol
                       #][, unique(.SD)
                       #][, melt(.SD, id.vars = "cube.symbol", value.name = "skewness")
                       #][, stage := fcase(variable == "cube.sk.30day", "30日滚动超额收益率", variable == "cube.sk.60day", "60日滚动超额收益率", variable == "cube.sk.90day", "90日滚动超额收益率")
    #][, ggplot(.SD, aes(x = skewness, colour = stage, fill = stage)) +
                            #geom_line(stat = "density", size = 1) +
                            #theme_grey() +
                            #scale_colour_manual(values = c("#CC6666", "#9999CC", "#66CC99")) +
                            #labs(x = "偏度值", y = "Density") +
                            #scale_fill_manual(values = c("#CC6666", "#9999CC", "#66CC99"),
                                                        #name = "Stage",
                                                        #breaks = c("30日滚动超额收益率", "60日滚动超额收益率", "90日滚动超额收益率"),
                                                        #labels = c("30日滚动超额收益率", "60日滚动超额收益率", "90日滚动超额收益率")) +
                                                        #theme(
                                    #legend.title = element_blank(),
                                    #panel.border = element_rect(linetype = 1, fill = NA),
                                    #legend.position = "bottom",
                                    #legend.spacing.x = unit(0.1, 'cm'),
                                    #legend.spacing.y = unit(2, 'cm'),
                                    #legend.box = "horizontal",
                                    #legend.box.background = element_rect(size = 1, colour = "black", fill = "white"),
                                    #legend.key = element_rect(size = 0.5, colour = "black", fill = "white"),
                                    #legend.key.size = unit(0.5, 'cm')
                                    #)
        #]
#ggsave("6.3.tiff", device = "tiff", dpi = 300, width = 5.5, height = 4)


##f.main1.hist[!(cube.symbol %in% outlier), .SD
    ##][order(cube.symbol, date), .SD
    ##][, c("a", "b1", "b2", "b3") := coef(lm(ret - rf ~ mkt_rf + smb + hml)) %>% list() %>% transpose(), by = .(cube.symbol)
    ##][, ret.excess := ret - rf - a - b1 * mkt_rf - b2 * smb - b3 * hml
    ##][, ret.ex.cum.30day := frollsum(ret.excess, 30, na.rm = T), by = .(cube.symbol)
    ##][!(cube.symbol %in% outlier) & (post.follow == 0 | (post.follow == 1 & (date - follow.date <= pre.period))), .(cube.sk = unique(skewness(ret.ex.cum.30day, na.rm = T, method = "fisher"))), by = .(cube.symbol, post.follow)
    ##][, ggplot() +
        ##geom_line(.SD[post.follow == 0], mapping = aes(x = cube.sk), stat = "density", color = "blue", size = 1) +
        ##geom_line(.SD[post.follow == 1], mapping = aes(x = cube.sk), stat = "density", color = "red", size = 1) 
        ###geom_line(.SD, mapping = aes(x = cube.sk), stat = "density", color = "black", size = 1)
        ##]

## 通过对数超额收益率得出的结果


#f.main1.lvol[!(cube.symbol %in% outlier) & (post.follow == 0 | (post.follow == 1 & (date - follow.date <= pre.period))), .SD
    #][, .(cube.sk.30 = unique(skewness(log(ret.ex.cum.30day + 1), na.rm = T, method = "fisher")))
        ##cube.sk.60 = unique(skewness(ret.ex.cum.60day, na.rm = T, method = "fisher")),
        ##cube.sk.90 = unique(skewness(ret.ex.cum.90day, na.rm = T, method = "fisher"))
        #, by = .(cube.symbol, post.follow)
    #][, unique(.SD)
    ##][, .SD[sum(!is.na(cube.sk.30)) == 2], by = .(cube.symbol)
    #][, ggplot() +
        #geom_line(.SD[post.follow == 0], mapping = aes(x = cube.sk.30), stat = "density", color = "blue", size = 1) +
        #geom_line(.SD[post.follow == 1], mapping = aes(x = cube.sk.30), stat = "density", color = "red", size = 1) 
    #]

#f.main1.lvol[!(cube.symbol %in% outlier) & (post.follow == 0 | (post.follow == 1 & (date - follow.date <= pre.period))), .SD
    #][, .(cube.sk = unique(skewness(log(ret.ex.cum.30day + 1), na.rm = T, method = "fisher")))
    ##cube.sk.60 = unique(skewness(ret.ex.cum.60day, na.rm = T, method = "fisher")),
    ##cube.sk.90 = unique(skewness(ret.ex.cum.90day, na.rm = T, method = "fisher"))
        #, by = .(cube.symbol, post.follow)
        #][, unique(.SD)
        ##][, .SD[sum(!is.na(cube.sk.30)) == 2], by = .(cube.symbol)
        #][, quantile(cube.sk, probs = seq(0.1, 0.9, 0.1), na.rm = T), by = .(post.follow)]


#f.main2.lvol[!(cube.symbol %in% outlier), .(cube.sk.30 = unique(skewness(log(ret.ex.cum.30day + 1), na.rm = T, method = "fisher")),
        #cube.sk.60 = unique(skewness(log(ret.ex.cum.60day + 1), na.rm = T, method = "fisher")),
        #cube.sk.90 = unique(skewness(log(ret.ex.cum.90day + 1), na.rm = T, method = "fisher"))
        #), by = .(cube.symbol)
    #][, unique(.SD)
    #][, ggplot() +
        #geom_line(.SD, mapping = aes(x = cube.sk.30), stat = "density", color = "blue", size = 1) +
        #geom_line(.SD, mapping = aes(x = cube.sk.60), stat = "density", color = "red", size = 1) +
        #geom_line(.SD, mapping = aes(x = cube.sk.90), stat = "density", color = "black", size = 1)
    #]

#f.main2.lvol[!(cube.symbol %in% outlier), .(cube.sk.30day = unique(skewness(log(ret.abnr.cum.30day + 1), na.rm = T, method = "fisher")),
                       #cube.sk.60day = unique(skewness(log(ret.abnr.cum.60day + 1), na.rm = T, method = "fisher")),
                       #cube.sk.90day = unique(skewness(log(ret.abnr.cum.90day + 1), na.rm = T, method = "fisher"))
                       #), by = cube.symbol
                       #][, unique(.SD)
                       #][, lapply(.SD[, 2:4], quantile, probs = seq(0.1, 0.9, 0.1), na.rm = T)]

# 3. 异质性波动率证明该负相关关系的检验 ----
f.main1.ivol <- fivefactor_daily[, setnames(.SD, "trddy", "date")
    ][, date := as.Date(date)
    ][index[, .(date, index_value, index_ret)], on = .(date)
    ][f.main1[order(cube.symbol, date) & !is.na(value), .SD], on = .(date)
    ][, .SD[.N > 30], by = .(cube.symbol)
    ][value != 0, .SD
    ][order(cube.symbol, date), .SD
    ][, ret := value / shift(value, type = "lag") - 1, by = .(cube.symbol)
    ][, post.follow := fifelse(date >= follow.date, 1, 0), by = .(cube.symbol)
    ][!(cube.symbol %in% outlier), ':='(alpha = coef(lm(ret ~ index_ret))[1], beta = coef(lm(ret ~ index_ret))[2]), by = .(cube.symbol)
    ][, ret.abnr := ret - alpha - beta * index_ret
    ][, ret.abnr.cum := frollsum(ret.abnr, 30, na.rm = T), by = .(cube.symbol)
    ][, ret.abnr.cum.lag := shift(ret.abnr.cum, type = "lag"), by = .(cube.symbol)
    ][!is.na(ret), residual := lm(ret - rf ~ mkt_rf + smb + hml) %>% residuals(), by = .(cube.symbol)
    #][, ret.abnr := residual, by = .(cube.symbol)
    #][, ret.abnr.cum := frollsum(ret.abnr, 30, na.rm = T), by = .(cube.symbol)
    #][, ret.abnr.cum.lag := shift(ret.abnr.cum, type = "lag"), by = .(cube.symbol)
    #][, bvol := {
        #a <- vector()
        #for (i in 30:.N) {
            #a[i] <- cor(ret[(i - 29):i], index_ret[(i - 29):i])
        #}
        #a
    #}
    #, by = .(cube.symbol)
    #][, bvol.lag := shift(bvol, type = "lag", n = 1), by = .(cube.symbol)
    ][, ivol := frollapply(residual, 30, sd, na.rm = T), by = .(cube.symbol)
    ][, ivol.lag := shift(ivol, type = "lag", n = 1), by = .(cube.symbol)
    ][, date.qrtr := str_c(year(date), quarter(date))
    ][, trd.num.30day := frollsum(trd.num, 30), by = .(cube.symbol) # 过去30天内的交易数量
    ][, stock.num.30day := frollmean(stock.num, 30), by = .(cube.symbol) # 过去30天内平均股票数量持有量
    ][, active.day := as.numeric(date - min(date), units = "weeks"), by = .(cube.symbol)
    ][, cmt.num.30day := frollsum(cmt.num, 30), by = .(cube.symbol) # 过去30天内的发帖数量
    ]

#cl <- makeCluster(8)
#registerDoParallel(cl)
#f.main1.ivol[, ivol := {
    #a <- vector()
    #for (i in 30:.N) {
        #lm.sample <- .SD[(i - 29):i]
        #a[i] <- lm.sample[, lm(ret - rf ~ mkt_rf + smb + hml) %>% residuals() %>% sd()]
    #}
    #a
#}, by = .(cube.symbol)
    #]
#f.main1.ivol[, ivol.lag := shift(ivol, type = "lag", n = 1), by = .(cube.symbol)
    #][, ivol.lag := ivol.lag * 30 ^ 0.5
    #]

f.main2.ivol <- fivefactor_daily[, setnames(.SD, "trddy", "date")
    ][, date := as.Date(date)
    ][index[, .(date, index_value, index_ret)], on = .(date)
    ][f.main2[order(cube.symbol, date) & !is.na(value), .SD], on = .(date)
    ][value != 0, .SD
    ][order(cube.symbol, date), .SD
    ][, ret := value / shift(value, type = "lag") - 1, by = .(cube.symbol)
    ][, .SD[.N > 90], by = .(cube.symbol)
    ][!(cube.symbol %in% outlier), ':='(alpha = coef(lm(ret ~ index_ret))[1], beta = coef(lm(ret ~ index_ret))[2]), by = .(cube.symbol)
    ][, ret.abnr := ret - alpha - beta * index_ret
    ][, ret.abnr.cum.30day := frollsum(ret.abnr, 30, na.rm = T), by = .(cube.symbol)
    ][, ret.abnr.cum.60day := frollsum(ret.abnr, 60, na.rm = T), by = .(cube.symbol)
    ][, ret.abnr.cum.90day := frollsum(ret.abnr, 90, na.rm = T), by = .(cube.symbol)
    ][, ret.abnr.cum.30day.lag := shift(ret.abnr.cum.30day, type = "lag"), by = .(cube.symbol)
    ][, ret.abnr.cum.60day.lag := shift(ret.abnr.cum.60day, type = "lag"), by = .(cube.symbol)
    ][, ret.abnr.cum.90day.lag := shift(ret.abnr.cum.90day, type = "lag"), by = .(cube.symbol)
    ][!is.na(ret), residual := lm(ret - rf ~ mkt_rf + smb + hml) %>% residuals(), by = .(cube.symbol)
    ][, ivol.30day := frollapply(residual, 30, sd, na.rm = T) * 30 ^ 0.5, by = .(cube.symbol)
    ][, ivol.60day := frollapply(residual, 60, sd, na.rm = T) * 60 ^ 0.5, by = .(cube.symbol)
    ][, ivol.90day := frollapply(residual, 90, sd, na.rm = T) * 90 ^ 0.5, by = .(cube.symbol)
    #][, bvol.30day := {
        #a <- vector()
        #for (i in 30:.N) {
            #a[i] <- cor(ret.abnr[(i - 29):i], index_ret[(i - 29):i])
        #}
        #a
    #}
    #, by = .(cube.symbol)
    #][, bvol.30day.lag := shift(bvol.30day, type = "lag", n = 1), by = .(cube.symbol)
    #][, bvol.60day := {
        #a <- vector()
        #for (i in 60:.N) {
            #a[i] <- cor(ret.abnr[(i - 59):i], index_ret[(i - 59):i])
        #}
        #a
    #}
    #, by = .(cube.symbol)
    #][, bvol.60day.lag := shift(bvol.60day, type = "lag", n = 1), by = .(cube.symbol)
    #][, bvol.90day := {
        #a <- vector()
        #for (i in 90:.N) {
            #a[i] <- cor(ret.abnr[(i - 89):i], index_ret[(i - 89):i])
        #}
        #a
    #}
    #, by = .(cube.symbol)
    #][, bvol.90day.lag := shift(bvol.90day, type = "lag", n = 1), by = .(cube.symbol)
    ][, trd.num.30day := frollsum(trd.num, 30), by = .(cube.symbol) # 过去30天内的交易数量
    ][, stock.num.30day := frollmean(stock.num, 30), by = .(cube.symbol) # 过去30天内平均股票数量持有量
    ][, active.day := as.numeric(date - min(date), units = "weeks"), by = .(cube.symbol)
    ][, cmt.num.30day := frollsum(cmt.num, 30), by = .(cube.symbol) # 过去30天内的发帖数量
    ][, date.qrtr := str_c(year(date), quarter(date))
    ][, oud.30day := frollmean(oud, 30, na.rm = T), by = .(cube.symbol)
    ][, ind.30day := frollmean(ind, 30, na.rm = T), by = .(cube.symbol)
    ][, ln.cntr.30day := frollmean(ln.cntr, 30, na.rm = T), by = .(cube.symbol)
    ][, oud.60day := frollmean(oud, 60, na.rm = T), by = .(cube.symbol)
    ][, ind.60day := frollmean(ind, 60, na.rm = T), by = .(cube.symbol)
    ][, ln.cntr.60day := frollmean(ln.cntr, 60, na.rm = T), by = .(cube.symbol)
    ][, oud.90day := frollmean(oud, 90, na.rm = T), by = .(cube.symbol)
    ][, ind.90day := frollmean(ind, 90, na.rm = T), by = .(cube.symbol)
    ][, ln.cntr.90day := frollmean(ln.cntr, 90, na.rm = T), by = .(cube.symbol)
    ][, ivol.30day.lag := shift(ivol.30day, type = "lag"), by = .(cube.symbol)
    ][, ivol.60day.lag := shift(ivol.60day, type = "lag"), by = .(cube.symbol)
    ][, ivol.90day.lag := shift(ivol.90day, type = "lag"), by = .(cube.symbol)
    ]

r1 <- f.main2.ivol[, felm(I(ret.abnr.cum.30day * 100) ~ log(oud.30day + 1) + I(ln.cntr.30day * 100) + log(ind.30day + 1) | cube.symbol + date.qrtr)] #%>% summary()
r2 <- f.main2.ivol[, felm(I(ret.abnr.cum.30day * 100) ~ log(oud.30day + 1) + I(ln.cntr.30day * 100) + log(ind.30day + 1) + I(ret.abnr.cum.30day.lag * 100) + active.day + cmt.num.30day + trd.num.30day + stock.num.30day | cube.symbol + date.qrtr)] #%>% summary()
r3 <- f.main2.ivol[, felm(I(ret.abnr.cum.60day * 100) ~ log(oud.60day + 1) + I(ln.cntr.60day * 100) + log(ind.60day + 1) | cube.symbol + date.qrtr)] #%>% summary()
r4 <- f.main2.ivol[, felm(I(ret.abnr.cum.60day * 100) ~ log(oud.60day + 1) + I(ln.cntr.60day * 100) + log(ind.60day + 1) + I(ret.abnr.cum.60day.lag * 100) + active.day + cmt.num.30day + trd.num.30day + stock.num.30day | cube.symbol + date.qrtr)] #%>% summary()
r5 <- f.main2.ivol[, felm(I(ret.abnr.cum.90day * 100) ~ log(oud.90day + 1) + I(ln.cntr.90day * 100) + log(ind.90day + 1) | cube.symbol + date.qrtr)] #%>% summary()
r6 <- f.main2.ivol[, felm(I(ret.abnr.cum.90day * 100) ~ log(oud.90day + 1) + I(ln.cntr.90day * 100) + log(ind.90day + 1) + I(ret.abnr.cum.90day.lag*100) + active.day + cmt.num.30day + trd.num.30day + stock.num.30day | cube.symbol + date.qrtr)]

list(r1, r2, r3, r4, r5, r6) %>%
    htmlreg(
        file = "5.7.html",
        custom.header = list("CAR.30day" = 1:2, "CAR.60day" = 3:4, "CAR.90day" = 5:6),
#caption.above = TRUE,
        include.rs = TRUE,
        include.adjrs = FALSE,
        custom.model.names = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)"),
        custom.gof.rows = list(
#"Control Variables" = c("No", "Yes", "No", "Yes", "No", "Yes"),
                                            "Trader FE" = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                                            "Quarter FE" = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes")
                                            ),
        custom.coef.names = c("LI.30day", "LQ.30day", "PS.30day", "CAR.30day.lag", "active.day", "cmt.num.30day", "trd.num.30day", "stock.num.30day", "LI.60day", "LQ.60day", "PS.60day", "CAR.60day.lag", "LI.90day", "LQ.90day", "PS.90day", "CAR.90day.lag"),
#omit.coef = "active.day|cmt.num.30day|trd.num.30day|stock.num.30day",
        reorder.coef = c(1:4, 9:16, 5:8),
        caption.above = TRUE,
        digits = 3,
        inline.css = FALSE,
        doctype = TRUE,
        html.tag = TRUE,
        head.tag = TRUE,
        body.tag = TRUE,
        center = FALSE,
            )
#f.main2.ivol[, ivol.30day := {
#a <- vector()
#for (i in 30:.N) {
#lm.sample <- .SD[(i - 29):i]
#a[i] <- lm.sample[, lm(ret - rf ~ mkt_rf + smb + hml) %>% residuals() %>% sd()]
#}
#a
#}, by = .(cube.symbol)
#][, ivol.60day := {
#a <- vector()
#for (i in 60:.N) {
#lm.sample <- .SD[(i - 59):i]
#a[i] <- lm.sample[, lm(ret - rf ~ mkt_rf + smb + hml) %>% residuals() %>% sd()]
#}
#a
#}, by = .(cube.symbol)
#][, ivol.90day := {
#a <- vector()
#for (i in 90:.N) {
#lm.sample <- .SD[(i - 89):i]
#a[i] <- lm.sample[, lm(ret - rf ~ mkt_rf + smb + hml) %>% residuals() %>% sd()]
#}
#a
#}, by = .(cube.symbol)
#][, ivol.30day.lag := shift(ivol.30day, type = "lag"), by = .(cube.symbol)
#][, ivol.60day.lag := shift(ivol.60day, type = "lag"), by = .(cube.symbol)
#][, ivol.90day.lag := shift(ivol.90day, type = "lag"), by = .(cube.symbol)
#]

# 异质性风险的变化
r1 <- f.main1.ivol[!(cube.symbol %in% outlier) & (post.follow == 0 | (post.follow == 1 & date - follow.date <= pre.period)), felm(I(ivol * 100) ~ post.follow | cube.symbol + date.qrtr)] #%>% summary()
r2 <- f.main1.ivol[!(cube.symbol %in% outlier) & (post.follow == 0 | (post.follow == 1 & date - follow.date <= pre.period)), felm(I(ivol * 100) ~ post.follow + active.day + cmt.num.30day + trd.num.30day + stock.num.30day | cube.symbol + date.qrtr)] #%>% summary()
r3 <- f.main2.ivol[!(cube.symbol %in% outlier), felm(I(ivol.30day * 100) ~ log(oud.30day + 1) + I(ln.cntr.30day * 100) + log(ind.30day + 1) | cube.symbol + date.qrtr)] #%>% summary()
r4 <- f.main2.ivol[!(cube.symbol %in% outlier), felm(I(ivol.30day * 100) ~ log(oud.30day + 1) + I(ln.cntr.30day * 100) + log(ind.30day + 1) + active.day + cmt.num.30day + trd.num.30day + stock.num.30day | cube.symbol + date.qrtr)] #%>% summary()
r5 <- f.main2.ivol[!(cube.symbol %in% outlier), felm(I(ivol.60day * 100) ~ log(oud.60day + 1) + I(ln.cntr.60day * 100) + log(ind.60day + 1) | cube.symbol + date.qrtr)] #%>% summary()
r6 <- f.main2.ivol[!(cube.symbol %in% outlier), felm(I(ivol.60day * 100) ~ log(oud.60day + 1) + I(ln.cntr.60day * 100) + log(ind.60day + 1) + active.day + cmt.num.30day + trd.num.30day + stock.num.30day | cube.symbol + date.qrtr)] #%>% summary()
r7 <- f.main2.ivol[!(cube.symbol %in% outlier), felm(I(ivol.90day * 100) ~ log(oud.90day + 1) + I(ln.cntr.90day * 100) + log(ind.90day + 1) | cube.symbol + date.qrtr)] #%>% summary()
r8 <- f.main2.ivol[!(cube.symbol %in% outlier), felm(I(ivol.90day * 100) ~ log(oud.90day + 1) + I(ln.cntr.90day * 100) + log(ind.90day + 1) + active.day + cmt.num.30day + trd.num.30day + stock.num.30day | cube.symbol + date.qrtr)] #%>% summary()

list(r1, r2, r3, r4, r5, r6, r7, r8) %>%
    htmlreg(
        file = "5.9.html",
        custom.header = list("Two-stage" = 1, "Two-stage" = 2, "Full-sample" = 3, "Full-sample" = 4,"Full-sample" = 5, "Full-sample" = 6, "Full-sample" = 7, "Full-sample" = 8),
#caption.above = TRUE,
        include.rs = TRUE,
        include.adjrs = FALSE,
        custom.model.names = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)"),
        custom.gof.rows = list(
#"Control Variables" = c("No", "Yes", "No", "Yes", "No", "Yes"),
                                            "Trader FE" = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                                            "Quarter FE" = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")
                                            ),
        custom.coef.names = c("post.follow", "active.day", "cmt-num-30day", "trd-num-30day", "stock-num-30day", "LI-30day", "LQ-30day", "PS-30day", "LI-60day", "LQ-60day", "PS-60day", "LI-90day", "LQ-90day", "PS-90day"),
#omit.coef = "active.day|cmt.num.30day|trd.num.30day|stock.num.30day",
        reorder.coef = c(1, 6:14, 2:5),
        caption.above = TRUE,
        digits = 3,
        inline.css = FALSE,
        doctype = TRUE,
        html.tag = TRUE,
        head.tag = TRUE,
        body.tag = TRUE,
        center = FALSE,
            )

# 异质性风险与收益率的关系
r1 <- f.main1.ivol[!(cube.symbol %in% outlier) & (post.follow == 0), felm(I(ret.abnr.cum*100) ~ I(ivol.lag*10) + I(ret.abnr.cum.lag*100) | cube.symbol + date.qrtr)] #%>% summary()
r2 <- f.main1.ivol[!(cube.symbol %in% outlier) & (post.follow == 1 & (date - follow.date <= pre.period)), felm(I(ret.abnr.cum * 100) ~ I(ivol.lag * 10) + I(ret.abnr.cum.lag * 100) | cube.symbol + date.qrtr)] #%>% summary()
r3 <- f.main1.ivol[!(cube.symbol %in% outlier) & (post.follow == 0), felm(I(ret.abnr.cum * 100) ~ I(ivol.lag * 10) + I(ret.abnr.cum.lag * 100) + active.day + cmt.num.30day + trd.num.30day + stock.num.30day | cube.symbol + date.qrtr)] #%>% summary()
r4 <- f.main1.ivol[!(cube.symbol %in% outlier) & (post.follow == 1 & (date - follow.date <= pre.period)), felm(I(ret.abnr.cum * 100) ~ I(ivol.lag * 10) + I(ret.abnr.cum.lag * 100) + active.day + cmt.num.30day + trd.num.30day + stock.num.30day | cube.symbol + date.qrtr)]
r5 <- f.main1.ivol[!(cube.symbol %in% outlier) & (post.follow == 0 | (post.follow == 1 & (date - follow.date <= pre.period))), felm(I(ret.abnr.cum * 100) ~ I(ivol.lag * 10) + I(ret.abnr.cum.lag * 100) + post.follow + I(ivol.lag * 10) * post.follow | cube.symbol + date.qrtr)] #%>% summary()
r6 <- f.main1.ivol[!(cube.symbol %in% outlier) & (post.follow == 0 | (post.follow == 1 & (date - follow.date <= pre.period))), felm(I(ret.abnr.cum * 100) ~ I(ivol.lag * 10) + I(ret.abnr.cum.lag * 100) + post.follow + I(ivol.lag * 10) * post.follow + active.day + cmt.num.30day + trd.num.30day + stock.num.30day | cube.symbol + date.qrtr)] #%>% summary()
r7 <- f.main1.ivol[!(cube.symbol %in% outlier) & (post.follow == 0 | (post.follow == 1 & (date - follow.date <= pre.period))), felm(I(ret.abnr.cum * 100) ~ I(ivol.lag * 10) + I(ret.abnr.cum.lag * 100) | cube.symbol + date.qrtr)] #%>% summary()
r8 <- f.main1.ivol[!(cube.symbol %in% outlier) & (post.follow == 0 | (post.follow == 1 & (date - follow.date <= pre.period))), felm(I(ret.abnr.cum * 100) ~ I(ivol.lag * 10) + I(ret.abnr.cum.lag * 100) + active.day + cmt.num.30day + trd.num.30day + stock.num.30day | cube.symbol + date.qrtr)] #%>% summary()

list(r1, r2, r3, r4, r5, r6, r7, r8) %>%
    htmlreg(
        file = "5.11.html",
        custom.header = list("pre-follow" = 1, "post-follow" = 2, "pre-follow" = 3, "post-follow" = 4, "two-stage" = 5, "two-stage" = 6, "two-stage" = 7, "two-stage" = 8),
#caption.above = TRUE,
        include.rs = TRUE,
        include.adjrs = FALSE,
        custom.model.names = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)"),
        custom.gof.rows = list(
#"Control Variables" = c("No", "Yes", "No", "Yes", "No", "Yes"),
                                            "Trader FE" = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                                            "Quarter FE" = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")
                                            ),
        custom.coef.names = c("ivol.30day.lag", "CAR.30day.lag", "active.day", "cmt.num.30day", "trd.num.30day", "stock.num.30day", "post.follow", "ivol.30day.lag * post.follow"),
#omit.coef = "active.day|cmt.num.30day|trd.num.30day|stock.num.30day",
        reorder.coef = c(1:2, 7:8, 3:6),
        caption.above = TRUE,
        digits = 3,
        inline.css = FALSE,
        doctype = TRUE,
        html.tag = TRUE,
        head.tag = TRUE,
        body.tag = TRUE,
        center = FALSE,
        stars = c(0.001, 0.01, 0.05, 0.1)
            )

r1 <- f.main2.ivol[!(cube.symbol %in% outlier), felm(I(ret.abnr.cum.30day*100) ~ I(ivol.30day.lag * 10) + I(ret.abnr.cum.30day.lag * 100) | cube.symbol + date.qrtr)] #%>% summary
#f.main2.ivol[!(cube.symbol %in% outlier), felm(ret.abnr.cum.30day ~ ivol.30day.lag + ret.abnr.cum.30day.lag + active.day + cmt.num + trd.num + stock.num | cube.symbol + date.qrtr)] #%>% summary()
r2 <- f.main2.ivol[!(cube.symbol %in% outlier), felm(I(ret.abnr.cum.30day * 100) ~ I(ivol.30day.lag * 10) + I(ret.abnr.cum.30day.lag * 100) + active.day + cmt.num.30day + trd.num.30day + stock.num.30day | cube.symbol + date.qrtr)] #%>% summary()

r3 <- f.main2.ivol[!(cube.symbol %in% outlier), felm(I(ret.abnr.cum.60day * 100) ~ I(ivol.60day.lag * 10) + I(ret.abnr.cum.60day.lag * 100) | cube.symbol + date.qrtr)] #%>% summary()
#f.main2.ivol[!(cube.symbol %in% outlier), felm(ret.abnr.cum.60day ~ ivol.60day.lag + ret.abnr.cum.60day.lag + active.day + cmt.num + trd.num + stock.num | cube.symbol + date.qrtr)] #%>% summary()
r4 <- f.main2.ivol[!(cube.symbol %in% outlier), felm(I(ret.abnr.cum.60day * 100) ~ I(ivol.60day.lag * 10) + I(ret.abnr.cum.60day.lag * 100) + active.day + cmt.num.30day + trd.num.30day + stock.num.30day | cube.symbol + date.qrtr)] #%>% summary()

r5 <- f.main2.ivol[!(cube.symbol %in% outlier), felm(I(ret.abnr.cum.90day * 100) ~ I(ivol.90day.lag * 10) + I(ret.abnr.cum.90day.lag * 100) | cube.symbol + date.qrtr)] #%>% summary()
#f.main2.ivol[!(cube.symbol %in% outlier), felm(ret.abnr.cum.90day ~ ivol.90day.lag + ret.abnr.cum.90day.lag + active.day + cmt.num + trd.num + stock.num | cube.symbol + date.qrtr)] #%>% summary()
r6 <- f.main2.ivol[!(cube.symbol %in% outlier), felm(I(ret.abnr.cum.90day * 100) ~ I(ivol.90day.lag * 10) + I(ret.abnr.cum.90day.lag * 100) + active.day + cmt.num.30day + trd.num.30day + stock.num.30day | cube.symbol + date.qrtr)] #%>% summary()

list(r1, r2, r3, r4, r5, r6) %>%
    htmlreg(
        file = "5.13.html",
        #custom.header = list("CAR.30day" = 1:3, "CAR.60day" = 4:6, "CAR.90day" = 7:9),
#caption.above = TRUE,
        include.rs = TRUE,
        include.adjrs = FALSE,
        custom.model.names = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)"),
        custom.gof.rows = list(
#"Control Variables" = c("No", "Yes", "No", "Yes", "No", "Yes"),
                                            "Trader FE" = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                                            "Quarter FE" = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes")
                                            ),
        custom.coef.names = c("ivol.30day.lag", "CAR.30day.lag", "active.day", "cmt.num.30day", "trd.num.30day", "stock.num.30day", "ivol.60day.lag", "CAR.60day.lag", "ivol.90day.lag", "CAR.90day.lag"),
#omit.coef = "active.day|cmt.num.30day|trd.num.30day|stock.num.30day",
        reorder.coef = c(1:2, 7:10, 3:6),
        caption.above = TRUE,
        digits = 3,
        inline.css = FALSE,
        doctype = TRUE,
        html.tag = TRUE,
        head.tag = TRUE,
        body.tag = TRUE,
        center = FALSE,
        stars = c(0.001, 0.01, 0.05, 0.1)
            )

# 分位数回归
rq1.vol <- f.main1.ivol[!(cube.symbol %in% outlier) & (post.follow == 0 | (post.follow == 1 & (date - follow.date <= pre.period))), rq(ret.abnr.cum ~ ivol.lag + ret.abnr.cum.lag + active.day + cmt.num + trd.num + stock.num + cmt.num.30day + trd.num.30day + stock.num.30day, tau = seq(0.1, 0.9, 0.1), method = "sfn", data = .SD)]
rq0 <- summary(rq1.vol)
rq0.coef <- lapply(rq0, function(x) {
    x$coefficients[2, c(1, 2, 4)]
})

rq2.vol.30 <- f.main2.ivol[!(cube.symbol %in% outlier), rq(ret.abnr.cum.30day ~ ivol.30day.lag + ret.abnr.cum.30day.lag + active.day + cmt.num + trd.num + stock.num + cmt.num.30day + trd.num.30day + stock.num.30day, tau = seq(0.1, 0.9, 0.1), method = "pfn", data = .SD)]
rq1 <- summary(rq2.vol.30)
rq1.coef <- lapply(rq1, function(x) {
    x$coefficients[2, c(1, 2, 4)]
})
rq2.vol.60 <- f.main2.ivol[!(cube.symbol %in% outlier), rq(ret.abnr.cum.60day ~ ivol.60day.lag + ret.abnr.cum.60day.lag + active.day + cmt.num + trd.num + stock.num + cmt.num.30day + trd.num.30day + stock.num.30day, tau = seq(0.1, 0.9, 0.1), method = "pfn", data = .SD)]
rq2 <- summary(rq2.vol.60)
rq2.coef <- lapply(rq2, function(x) {
    x$coefficients[2, c(1, 2, 4)]
})

rq2.vol.90 <- f.main2.ivol[!(cube.symbol %in% outlier), rq(ret.abnr.cum.90day ~ ivol.90day.lag + ret.abnr.cum.90day.lag + active.day + cmt.num + trd.num + stock.num + cmt.num.30day + trd.num.30day + stock.num.30day, tau = seq(0.1, 0.9, 0.1), method = "pfn", data = .SD)]
rq3 <- summary(rq2.vol.90)
rq3.coef <- lapply(rq3, function(x) {
    x$coefficients[2, c(1, 2, 4)]
})

rq0.coef <- as.data.table(rq0.coef)
rq1.coef <- as.data.table(rq1.coef)
rq2.coef <- as.data.table(rq2.coef)
rq3.coef <- as.data.table(rq3.coef)

fwrite(rq0.coef, "rq0.csv")
fwrite(rq1.coef, "rq1.csv")
fwrite(rq2.coef, "rq2.csv")
fwrite(rq3.coef, "rq3.csv")


# 4. 对数异质性波动率前后比较 ----
# f.main1 (30 days)
f.main1.livol <- f.main1.ivol[!(cube.symbol %in% outlier), .SD
    ][order(cube.symbol, date), .SD
    ][, ret.log := log(value) - log(shift(value, type = "lag")), by = .(cube.symbol)
    ][, index.ret.log := log(index_value) - log(shift(index_value, type = "lag")), by = .(cube.symbol)
    ][!(cube.symbol %in% outlier), ':='(alpha = coef(lm(ret.log ~ index.ret.log))[1], beta = coef(lm(ret.log ~ index.ret.log))[2]), by = .(cube.symbol)
    ][, ret.ex.log := ret.log - alpha - beta * index.ret.log, by = .(cube.symbol)
    ][, ret.ex.cum.30day := frollsum(ret.ex.log, 30, na.rm = T), by = .(cube.symbol)
    ][, ret.ex.cum.30day.lag := shift(ret.ex.cum.30day, type = "lag", n = 1), by = .(cube.symbol)
    #][, vol.30day := frollapply(ret.ex.cum.30day, 30, sd, na.rm = T) * (30 ^ 0.5), by = .(cube.symbol)
    #][, vol.30day.lag := shift(vol.30day, type = "lag", n = 1), by = .(cube.symbol)
    ][!is.na(ret), residual.log := lm(ret.log - log(rf + 1) ~ mkt_rf + smb + hml) %>% residuals(), by = .(cube.symbol)
    ][, ivol.30day.log := frollapply(residual.log, 30, sd, na.rm = T) * 30 ^ 0.5, by = .(cube.symbol)
    ][, ivol.30day.log.lag := shift(ivol.30day.log, type = "lag"), by = .(cube.symbol)
    ]


# f.main2 (30/60/90 days)
f.main2.livol <- f.main2.ivol[!(cube.symbol %in% outlier), .SD
    ][order(cube.symbol, date), .SD
    ][, .SD[.N > 90], by = .(cube.symbol)
    ][, ret.log := log(value) - log(shift(value, type = "lag")), by = .(cube.symbol)
    ][, index.ret.log := log(index_value) - log(shift(index_value, type = "lag")), by = .(cube.symbol)
    ][, ':='(alpha = coef(lm(ret.log ~ index.ret.log))[1], beta = coef(lm(ret.log ~ index.ret.log))[2]), by = .(cube.symbol)
    ][, ret.ex.log := ret.log - alpha - beta * index.ret.log, by = .(cube.symbol)
    ][, ret.ex.cum.30day := frollsum(ret.ex.log, 30, na.rm = T), by = .(cube.symbol)
    ][, ret.ex.cum.30day.lag := shift(ret.ex.cum.30day, type = "lag", n = 1), by = .(cube.symbol)
    #][, vol.30day := frollapply(ret.ex.log, 30, sd, na.rm = T) * (30 ^ 0.5), by = .(cube.symbol)
    #][, vol.30day.lag := shift(vol.30day, type = "lag", n = 1), by = .(cube.symbol)
    ][, ret.ex.cum.60day := frollsum(ret.ex.log, 60, na.rm = T), by = .(cube.symbol)
    ][, ret.ex.cum.60day.lag := shift(ret.ex.cum.60day, type = "lag", n = 1), by = .(cube.symbol)
    #][, vol.60day := frollapply(ret.ex.log, 60, sd, na.rm = T) * (60 ^ 0.5), by = .(cube.symbol)
    #][, vol.60day.lag := shift(vol.60day, type = "lag", n = 1), by = .(cube.symbol)
    ][, ret.ex.cum.90day := frollsum(ret.ex.log, 90, na.rm = T), by = .(cube.symbol)
    ][, ret.ex.cum.90day.lag := shift(ret.ex.cum.90day, type = "lag", n = 1), by = .(cube.symbol)
    #][, vol.90day := frollapply(ret.ex.log, 90, sd, na.rm = T) * (90 ^ 0.5), by = .(cube.symbol)
    #][, vol.90day.lag := shift(vol.90day, type = "lag", n = 1), by = .(cube.symbol)
    ][!is.na(ret), residual.log := lm(ret.log - log(rf + 1) ~ mkt_rf + smb + hml) %>% residuals(), by = .(cube.symbol)
    ][, ivol.30day.log := frollapply(residual.log, 30, sd, na.rm = T) * 30 ^ 0.5, by = .(cube.symbol)
    ][, ivol.60day.log := frollapply(residual.log, 60, sd, na.rm = T) * 60 ^ 0.5, by = .(cube.symbol)
    ][, ivol.90day.log := frollapply(residual.log, 90, sd, na.rm = T) * 90 ^ 0.5, by = .(cube.symbol)
    ][, ivol.30day.log.lag := shift(ivol.30day.log, type = "lag"), by = .(cube.symbol)
    ][, ivol.60day.log.lag := shift(ivol.60day.log, type = "lag"), by = .(cube.symbol)
    ][, ivol.90day.log.lag := shift(ivol.90day.log, type = "lag"), by = .(cube.symbol)
    ]
#4.1 社交行为对对数收益率的影响
r1 <- f.main2.livol[, felm(I(ret.ex.cum.30day * 100) ~ log(oud.30day + 1) + I(ln.cntr.30day * 100) + log(ind.30day + 1) | cube.symbol + date.qrtr)] #%>% summary()
r2 <- f.main2.livol[, felm(I(ret.ex.cum.30day * 100) ~ log(oud.30day + 1) + I(ln.cntr.30day * 100) + log(ind.30day + 1) + I(ret.ex.cum.30day.lag*100) + active.day + cmt.num.30day + trd.num.30day + stock.num.30day | cube.symbol + date.qrtr)] #%>% summary()
r3 <- f.main2.livol[, felm(I(ret.ex.cum.60day * 100) ~ log(oud.60day + 1) + I(ln.cntr.60day * 100) + log(ind.60day + 1) | cube.symbol + date.qrtr)] #%>% summary()
r4 <- f.main2.livol[, felm(I(ret.ex.cum.60day * 100) ~ log(oud.60day + 1) + I(ln.cntr.60day * 100) + log(ind.60day + 1) + I(ret.ex.cum.60day.lag*100) + active.day + cmt.num.30day + trd.num.30day + stock.num.30day | cube.symbol + date.qrtr)] #%>% summary()
r5 <- f.main2.livol[, felm(I(ret.ex.cum.90day * 100) ~ log(oud.90day + 1) + I(ln.cntr.90day * 100) + log(ind.90day + 1) | cube.symbol + date.qrtr)] #%>% summary()
r6 <- f.main2.livol[, felm(I(ret.ex.cum.90day * 100) ~ log(oud.90day + 1) + I(ln.cntr.90day * 100) + log(ind.90day + 1) + I(ret.ex.cum.90day.lag*100) + active.day + cmt.num.30day + trd.num.30day + stock.num.30day | cube.symbol + date.qrtr)] #%>% summary()

list(r1, r2, r3, r4, r5, r6) %>%
    htmlreg(
        file = "5.8.html",
        custom.header = list("CAR-30day" = 1:2, "CAR-60day" = 3:4, "CAR-90day"=5:6),
#caption.above = TRUE,
        include.rs = TRUE,
        include.adjrs = FALSE,
        custom.model.names = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)"),
        custom.gof.rows = list(
#"Control Variables" = c("No", "Yes", "No", "Yes", "No", "Yes"),
                                            "Trader FE" = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                                            "Quarter FE" = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes")
                                            ),
        custom.coef.names = c("LI-30day", "LQ-30day", "PS-30day", "CAR-30day-lag", "active.day", "cmt-num-30day","trd-num-30day","stock-num-30day", "LI-60day", "LQ-60day", "PS-60day", "CAR-60day-lag", "LI-90day", "LQ-90day", "PS-90day","CAR-90day-lag"),
#omit.coef = "active.day|cmt.num.30day|trd.num.30day|stock.num.30day",
        reorder.coef = c(1:4, 9:16, 5:8),
        caption.above = TRUE,
        digits = 3,
        inline.css = FALSE,
        doctype = TRUE,
        html.tag = TRUE,
        head.tag = TRUE,
        body.tag = TRUE,
        center = FALSE,
            )

# 4.2 社交对对数收益率波动率的影响
r1 <- f.main1.livol[(post.follow == 0 | (post.follow == 1 & (date - follow.date <= pre.period))) & !(cube.symbol %in% outlier), felm(I(ivol.30day.log*100) ~ post.follow | cube.symbol + date.qrtr)] #%>% summary()
r2 <- f.main1.livol[(post.follow == 0 | (post.follow == 1 & (date - follow.date <= pre.period))) & !(cube.symbol %in% outlier), felm(I(ivol.30day.log*100) ~ post.follow + active.day + cmt.num.30day + trd.num.30day + stock.num.30day | cube.symbol + date.qrtr)] #%>% summary()
r3 <- f.main2.livol[, felm(I(ivol.30day.log*100) ~ log(oud.30day + 1) + I(ln.cntr.30day * 100) + log(ind.30day + 1) | cube.symbol + date.qrtr)] #%>% summary()
r4 <- f.main2.livol[, felm(I(ivol.30day.log*100) ~ log(oud.30day + 1) + I(ln.cntr.30day * 100) + log(ind.30day + 1) + active.day + cmt.num.30day + trd.num.30day + stock.num.30day | cube.symbol + date.qrtr)] #%>% summary()
r5 <- f.main2.livol[, felm(I(ivol.60day.log * 100) ~ log(oud.60day + 1) + I(ln.cntr.60day * 100) + log(ind.60day + 1) | cube.symbol + date.qrtr)] #%>% summary()
r6 <- f.main2.livol[, felm(I(ivol.60day.log * 100) ~ log(oud.60day + 1) + I(ln.cntr.60day * 100) + log(ind.60day + 1) + active.day + cmt.num.30day + trd.num.30day + stock.num.30day | cube.symbol + date.qrtr)] #%>% summary()
r7 <- f.main2.livol[, felm(I(ivol.90day.log * 100) ~ log(oud.90day + 1) + I(ln.cntr.90day * 100) + log(ind.90day + 1) | cube.symbol + date.qrtr)] #%>% summary()
r8 <- f.main2.livol[, felm(I(ivol.90day.log * 100) ~ log(oud.90day + 1) + I(ln.cntr.90day * 100) + log(ind.90day + 1) + active.day + cmt.num.30day + trd.num.30day + stock.num.30day | cube.symbol + date.qrtr)] #%>% summary()

list(r1, r2, r3, r4, r5, r6, r7, r8) %>%
    htmlreg(
        file = "5.10.html",
        custom.header = list("Two-stage" = 1, "Two-stage" = 2, "Full-sample" = 3, "Full-sample" = 4, "Full-sample" = 5, "Full-sample" = 6, "Full-sample" = 7, "Full-sample" = 8),
#caption.above = TRUE,
        include.rs = TRUE,
        include.adjrs = FALSE,
        custom.model.names = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)"),
        custom.gof.rows = list(
#"Control Variables" = c("No", "Yes", "No", "Yes", "No", "Yes"),
                                            "Trader FE" = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                                            "Quarter FE" = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")
                                            ),
        custom.coef.names = c("post-follow", "active day", "cmt-num-30day", "trd-num-30day", "stock-num-30day", "LI-30day", "LQ-30day", "PS-30day", "LI-60day", "LQ-60day", "PS-60day", "LI-90day", "LQ-90day", "PS-90day"),
#omit.coef = "active.day|cmt.num.30day|trd.num.30day|stock.num.30day",
        reorder.coef = c(1, 6:14, 2:5),
        caption.above = TRUE,
        digits = 3,
        inline.css = FALSE,
        doctype = TRUE,
        html.tag = TRUE,
        head.tag = TRUE,
        body.tag = TRUE,
        center = FALSE,
            )

# 4.3 对数波动率与收益率的关系 
# f.main1
r1 <- f.main1.livol[(post.follow == 0), felm(I(ret.ex.cum.30day*100) ~ I(ivol.30day.log.lag*10) + I(ret.ex.cum.30day.lag*100) | cube.symbol + date.qrtr)] #%>% summary()
r2 <- f.main1.livol[(post.follow == 1 & (date - follow.date <= pre.period)), felm(I(ret.ex.cum.30day * 100) ~ I(ivol.30day.log.lag * 10) + I(ret.ex.cum.30day.lag * 100) | cube.symbol + date.qrtr)] #%>% summary()

r3 <- f.main1.livol[(post.follow == 0), felm(I(ret.ex.cum.30day * 100) ~ I(ivol.30day.log.lag * 10) + I(ret.ex.cum.30day.lag * 100) + active.day + cmt.num.30day + trd.num.30day + stock.num.30day | cube.symbol + date.qrtr)] #%>% summary()
r4 <- f.main1.livol[(post.follow == 1 & (date - follow.date <= pre.period)), felm(I(ret.ex.cum.30day * 100) ~ I(ivol.30day.log.lag * 10) + I(ret.ex.cum.30day.lag * 100) + active.day + cmt.num.30day + trd.num.30day + stock.num.30day | cube.symbol + date.qrtr)] #%>% summary()


r5 <- f.main1.livol[(post.follow == 0 | (post.follow == 1 & (date - follow.date <= pre.period))), felm(I(ret.ex.cum.30day * 100) ~ I(ivol.30day.log.lag * 10) + I(ret.ex.cum.30day.lag * 100) + post.follow + post.follow * I(ivol.30day.log.lag*10) | cube.symbol + date.qrtr)] #%>% summary()
r6 <- f.main1.livol[(post.follow == 0 | (post.follow == 1 & (date - follow.date <= pre.period))), felm(I(ret.ex.cum.30day * 100) ~ I(ivol.30day.log.lag * 10) + I(ret.ex.cum.30day.lag * 100) + post.follow + post.follow * I(ivol.30day.log.lag*10) + active.day + cmt.num.30day + trd.num.30day + stock.num.30day | cube.symbol + date.qrtr)] #%>% summary()

r7 <- f.main1.livol[(post.follow == 0 | (post.follow == 1 & (date - follow.date <= pre.period))), felm(I(ret.ex.cum.30day * 100) ~ I(ivol.30day.log.lag * 10) + I(ret.ex.cum.30day.lag * 100) | cube.symbol + date.qrtr)] #%>% summary()
r8 <- f.main1.livol[(post.follow == 0 | (post.follow == 1 & (date - follow.date <= pre.period))), felm(I(ret.ex.cum.30day * 100) ~ I(ivol.30day.log.lag * 10) + I(ret.ex.cum.30day.lag * 100) + active.day + cmt.num.30day + trd.num.30day + stock.num.30day | cube.symbol + date.qrtr)] #%>% summary()

list(r1, r2, r3, r4, r5, r6, r7, r8) %>%
    htmlreg(
        file = "5.12.html",
        custom.header = list("pre-follow" = 1, "post-follow" = 2, "pre-follow" = 3, "post-follow" = 4, "two-stage" = 5, "two-stage" = 6, "two-stage" = 7, "two-stage" = 8),
#caption.above = TRUE,
        include.rs = TRUE,
        include.adjrs = FALSE,
        custom.model.names = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)"),
        custom.gof.rows = list(
#"Control Variables" = c("No", "Yes", "No", "Yes", "No", "Yes"),
                                            "Trader FE" = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                                            "Quarter FE" = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")
                                            ),
        custom.coef.names = c("livol.30day.lag", "CAR.30day.lag", "active.day", "cmt.num.30day", "trd.num.30day", "stock.num.30day", "post.follow", "livol.30day.lag * post.follow"),
#omit.coef = "active.day|cmt.num.30day|trd.num.30day|stock.num.30day",
        reorder.coef = c(1:2, 7:8, 3:6),
        caption.above = TRUE,
        digits = 3,
        inline.css = FALSE,
        doctype = TRUE,
        html.tag = TRUE,
        head.tag = TRUE,
        body.tag = TRUE,
        center = FALSE,
        stars = c(0.001, 0.01, 0.05, 0.1)
            )

# f.main2
r1 <- f.main2.livol[, felm(I(ret.ex.cum.30day * 100) ~ I(ivol.30day.log.lag * 10) + I(ret.ex.cum.30day.lag * 100) | cube.symbol + date.qrtr)]
#f.main2.livol[, felm(ret.ex.cum.30day ~ ivol.30day.log.lag + ret.ex.cum.30day.lag + active.day + cmt.num + trd.num + stock.num | cube.symbol + date.qrtr) %>% summary()]
r2 <- f.main2.livol[, felm(I(ret.ex.cum.30day * 100) ~ I(ivol.30day.log.lag * 10) + I(ret.ex.cum.30day.lag * 100) + active.day + cmt.num.30day + trd.num.30day + stock.num.30day | cube.symbol + date.qrtr)]
r3 <- f.main2.livol[, felm(I(ret.ex.cum.60day * 100) ~ I(ivol.60day.log.lag * 10) + I(ret.ex.cum.60day.lag * 100) | cube.symbol + date.qrtr)]
#f.main2.livol[, felm(ret.ex.cum.60day ~ vol.60day.lag + ret.ex.cum.60day.lag + active.day + cmt.num + trd.num + stock.num | cube.symbol + date.qrtr) %>% summary()]
r4 <- f.main2.livol[, felm(I(ret.ex.cum.60day * 100) ~ I(ivol.60day.log.lag * 10) + I(ret.ex.cum.60day.lag * 100) + active.day + cmt.num.30day + trd.num.30day + stock.num.30day | cube.symbol + date.qrtr)]
r5 <- f.main2.livol[, felm(I(ret.ex.cum.90day * 100) ~ I(ivol.90day.log.lag * 10) + I(ret.ex.cum.90day.lag * 100) | cube.symbol + date.qrtr)]
#f.main2.livol[, felm(ret.ex.cum.90day ~ vol.90day.lag + ret.ex.cum.90day.lag + active.day + cmt.num + trd.num + stock.num | cube.symbol + date.qrtr) %>% summary()]
r6 <- f.main2.livol[, felm(I(ret.ex.cum.90day * 100) ~ I(ivol.90day.log.lag * 10) + I(ret.ex.cum.90day.lag * 100) + active.day + cmt.num.30day + trd.num.30day + stock.num.30day | cube.symbol + date.qrtr)]

list(r1, r2, r3, r4, r5, r6) %>%
    htmlreg(
        file = "5.14.html",
#custom.header = list("CAR.30day" = 1:3, "CAR.60day" = 4:6, "CAR.90day" = 7:9),
#caption.above = TRUE,
        include.rs = TRUE,
        include.adjrs = FALSE,
        custom.model.names = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)"),
        custom.gof.rows = list(
#"Control Variables" = c("No", "Yes", "No", "Yes", "No", "Yes"),
                                            "Trader FE" = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                                            "Quarter FE" = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes")
                                            ),
        custom.coef.names = c("livol.30day.lag", "CAR.30day.lag", "active.day", "cmt.num.30day", "trd.num.30day", "stock.num.30day", "livol.60day.lag", "CAR.60day.lag", "livol.90day.lag", "CAR.90day.lag"),
#omit.coef = "active.day|cmt.num.30day|trd.num.30day|stock.num.30day",
        reorder.coef = c(1:2, 7:10, 3:6),
        caption.above = TRUE,
        digits = 3,
        inline.css = FALSE,
        doctype = TRUE,
        html.tag = TRUE,
        head.tag = TRUE,
        body.tag = TRUE,
        center = FALSE,
        stars = c(0.001, 0.01, 0.05, 0.1)
            )
# 分位数回归
rq1.lvol <- f.main1.livol[!(cube.symbol %in% outlier) & (post.follow == 0 | (post.follow == 1 & (date - follow.date <= pre.period))), rq(ret.ex.cum.30day ~ ivol.30day.log.lag + ret.ex.cum.30day.lag + active.day + cmt.num + trd.num + stock.num + cmt.num.30day + trd.num.30day + stock.num.30day, tau = seq(0.1, 0.9, 0.1), method = "pfn", data = .SD)]
rq0 <- summary(rq1.lvol)
rq0.coef <- lapply(rq0, function(x) {
    x$coefficients[2, c(1, 2, 4)]
})

rq2.lvol.30 <- f.main2.livol[!(cube.symbol %in% outlier), rq(ret.ex.cum.30day ~ ivol.30day.log.lag + ret.ex.cum.30day.lag + active.day + cmt.num + trd.num + stock.num + cmt.num.30day + trd.num.30day + stock.num.30day, tau = seq(0.1, 0.9, 0.1), method = "pfn", data = .SD)]
rq1 <- summary(rq2.lvol.30)
rq1.coef <- lapply(rq1, function(x) {
    x$coefficients[2, c(1, 2, 4)]
})

rq2.lvol.60 <- f.main2.livol[!(cube.symbol %in% outlier), rq(ret.ex.cum.60day ~ ivol.60day.log.lag + ret.ex.cum.60day.lag + active.day + cmt.num + trd.num + stock.num + cmt.num.30day + trd.num.30day + stock.num.30day, tau = seq(0.1, 0.9, 0.1), method = "pfn", data = .SD)]
rq2 <- summary(rq2.lvol.60)
rq2.coef <- lapply(rq2, function(x) {
    x$coefficients[2, c(1, 2, 4)]
})

rq2.lvol.90 <- f.main2.livol[!(cube.symbol %in% outlier), rq(ret.ex.cum.90day ~ ivol.90day.log.lag + ret.ex.cum.90day.lag + active.day + cmt.num + trd.num + stock.num + cmt.num.30day + trd.num.30day + stock.num.30day, tau = seq(0.1, 0.9, 0.1), method = "pfn", data = .SD)]
rq3 <- summary(rq2.lvol.90)
rq3.coef <- lapply(rq3, function(x) {
    x$coefficients[2, c(1, 2, 4)]
})

rq0.coef <- as.data.table(rq0.coef)
rq1.coef <- as.data.table(rq1.coef)
rq2.coef <- as.data.table(rq2.coef)
rq3.coef <- as.data.table(rq3.coef)

fwrite(rq0.coef, "rq0.csv")
fwrite(rq1.coef, "rq1.csv")
fwrite(rq2.coef, "rq2.csv")
fwrite(rq3.coef, "rq3.csv")