# 根据宇哥论文做出的结论，雪球平台整体的风险越来越高，在这里需要看follow后的整体风险对收益率的影响
library(styleer)
library(PerformanceAnalytics)
library(lfe)
ld(f.main1, force = T)
ld(f.main2, force = T)
ld(f.cube.ret.sp)
ld(stock.ratio.flat)
outlier <- f.main1[order(cube.symbol, date), .SD
    ][, ret := value / shift(value, type = "lag") - 1, by = .(cube.symbol)
    ][ret < -0.1 | ret > 0.1, unique(cube.symbol)]
outlier <- f.cube.ret.sp[order(cube.symbol, date), .SD
    ][, ret := value / shift(value, type = "lag") - 1, by = .(cube.symbol)
    ][ret <= -0.1 | ret >= 0.1, unique(cube.symbol)]

# 导入各类因子
url <- str_c(getwd(), "/data/Factors/")
file.names <- list.files(path = url, pattern = "*.txt|.csv")
for (i in file.names) {
    assign(str_sub(i, start = 1L, end = -5L), fread(str_c(url, toupper(i))))
}

# 日度指数文件
url <- str_c(getwd(), "/data/Index")
index <- fbread(path = url, pattern = "*\\d.txt")
index <- index[, file_id := NULL
    ][Indexcd == "000300"
    ][, ':='(index_ret = Idxtrd08 / 100, index_value = Idxtrd05, date = as.Date(Idxtrd01))]

# 导入个股收盘价数据
url <- str_c(getwd(), "/data/Clprc")
Clsprc <- fbread(path = url, pattern = "*.txt")
Clsprc[, stock.symbol := str_pad(Stkcd, 6, side = "left", pad = "0")
    ][, date := as.Date(Trddt)
    ][, ':='(file_id = NULL, Stkcd = NULL, Trddt = NULL, Markettype = NULL)]

# 1. 看follow前后的risk对比 ----

## 1.1 计算利用过去一周平均的交易次数定义风险（放弃）
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

# 1.2 利用cube三十天之内的return的历史波动率和异质性波动率
## 历史波动率
# 1.2.1 f.main1
# 30 days
f.main1 <- f.main1[order(cube.symbol, date), .SD
    ][, ret := value / shift(value, type = "lag") - 1, by = .(cube.symbol)
    ][, post.follow := fifelse(date >= follow.date, 1, 0), by = .(cube.symbol)
    ][, hist.vol := frollapply(ret, 30, sd, na.rm = T), by = .(cube.symbol)
    ][, hist.vol.diff := hist.vol - shift(hist.vol, type = "lag"), by = .(cube.symbol)
    ][, ret.roll.day := value / shift(value, type = "lag", n = 29) - 1, by = .(cube.symbol)
    ][, .SD[max(date) - follow.date >= pre.period], by = .(cube.symbol)
    ][, date.qrtr := str_c(year(date), quarter(date))]

f.main1[!(cube.symbol %in% outlier) & (post.follow == 0 | (post.follow == 1 & date - follow.date <= pre.period)), felm(hist.vol ~ post.follow | cube.symbol + date.qrtr) %>% summary()]
f.main1[!(cube.symbol %in% outlier), felm(hist.vol ~ post.follow | cube.symbol + date.qrtr) %>% summary()]
f.main1[!(cube.symbol %in% outlier), felm(hist.vol ~ post.follow | cube.symbol + date.qrtr) %>% summary()]

f.main1[!(cube.symbol %in% outlier) & (post.follow == 0 | (post.follow == 1 & date - follow.date <= pre.period)), felm(ret ~ post.follow | cube.symbol + date.qrtr) %>% summary()]
f.main1[!(cube.symbol %in% outlier), felm(ret.roll.day ~ post.follow | cube.symbol + date.qrtr) %>% summary()]

f.main1[!(cube.symbol %in% outlier), felm(ret.roll.day ~ hist.vol | cube.symbol + date.qrtr) %>% summary()] # 不能将two-side拿出来

# 1.2.2 f.main2
# 30 days
f.main2 <- f.main2[order(cube.symbol, date) & !is.na(value), .SD
    ][, ret := value / shift(value, type = "lag") - 1, by = .(cube.symbol)
    ][, hist.vol := frollapply(ret, 30, sd, na.rm = T), by = .(cube.symbol)
    ][, hist.vol.diff := hist.vol - shift(hist.vol, type = "lag"), by = .(cube.symbol)
    ][, ret.roll.day := value / shift(value, type = "lag", n = 29) - 1, by = .(cube.symbol)
    ][, date.qrtr := str_c(year(date), quarter(date))]
f.main2[!(cube.symbol %in% outlier), felm(hist.vol ~ log(ind + 1) + log(oud + 1) + I(ln.cntr * 1000) | cube.symbol + date) %>% summary()]
f.main2[!(cube.symbol %in% outlier), felm(ret.roll.day ~ log(ind + 1) + log(oud + 1) + I(ln.cntr * 1000) | cube.symbol + date) %>% summary()]
f.main2[!(cube.symbol %in% outlier), felm(ret.roll.day ~ hist.vol | cube.symbol + date) %>% summary()]

# 60 days
f.main2 <- f.main2[order(cube.symbol, date) & !is.na(value), .SD
    ][, ret := value / shift(value, type = "lag") - 1, by = .(cube.symbol)
    ][, hist.vol := frollapply(ret, 60, sd, na.rm = T), by = .(cube.symbol)
    ][, hist.vol.diff := hist.vol - shift(hist.vol, type = "lag"), by = .(cube.symbol)
    ][, ret.roll.day := value / shift(value, type = "lag", n = 59) - 1, by = .(cube.symbol)
    ][, date.qrtr := str_c(year(date), quarter(date))]
f.main2[!(cube.symbol %in% outlier), felm(hist.vol ~ log(ind + 1) + log(oud + 1) + I(ln.cntr * 1000) | cube.symbol + date) %>% summary()]
f.main2[!(cube.symbol %in% outlier), felm(ret.roll.day ~ log(ind + 1) + log(oud + 1) + I(ln.cntr * 1000) | cube.symbol + date) %>% summary()]
f.main2[!(cube.symbol %in% outlier), felm(ret.roll.day ~ hist.vol | cube.symbol + date) %>% summary()]

# 90 days
f.main2 <- f.main2[order(cube.symbol, date) & !is.na(value), .SD
    ][, ret := value / shift(value, type = "lag") - 1, by = .(cube.symbol)
    ][, hist.vol := frollapply(ret, 90, sd, na.rm = T), by = .(cube.symbol)
    ][, hist.vol.diff := hist.vol - shift(hist.vol, type = "lag"), by = .(cube.symbol)
    ][, ret.roll.day := value / shift(value, type = "lag", n = 89) - 1, by = .(cube.symbol)
    ][, date.qrtr := str_c(year(date), quarter(date))]
f.main2[!(cube.symbol %in% outlier), felm(hist.vol ~ log(ind + 1) + log(oud + 1) + I(ln.cntr * 1000) | cube.symbol + date) %>% summary()]
f.main2[!(cube.symbol %in% outlier), felm(ret.roll.day ~ log(ind + 1) + log(oud + 1) + I(ln.cntr * 1000) | cube.symbol + date) %>% summary()]
f.main2[!(cube.symbol %in% outlier), felm(ret.roll.day ~ hist.vol | cube.symbol + date) %>% summary()]



# 看pre-follow和post-follow的偏度（time维度）----
f.main1.index <- index[, .(index_ret, index_value, date)
    ][f.main1, on = .(date)
    ][, period := max(date) - min(date), by = .(cube.symbol)
    ][period > 90, .SD
    ][, post.follow := fifelse(date < follow.date, 0, 1), by = .(cube.symbol)
    ][, ret := value / shift(value, type = "lag") - 1, by = .(cube.symbol)
    ]
# 与沪深300指数相减
cube.pre.follow <- f.main1.index[!(cube.symbol %in% outlier) & post.follow == 0, .(ret.daily = mean(ret, na.rm = T), index_ret), by = .(date)
    ][, unique(.SD)
    ][, ret.excess := ret.daily - index_ret
    #][ret.excess %between% c(-0.01, 0.01)
    ][-444, skewness(ret.excess)]
ggplot(cube.pre.follow) + geom_line(aes(x = date, y = ret.excess))

cube.post.follow <- f.main1.index[!(cube.symbol %in% outlier) & post.follow == 1 & date - follow.date <= pre.period, .(ret.daily = mean(ret, na.rm = T), index_ret), by = .(date)
    ][, unique(.SD)
    ][, ret.excess := ret.daily - index_ret
    #][ret.excess %between% c(-0.01, 0.01)
    ][, skewness(ret.excess)]
ggplot(cube.post.follow) + geom_line(aes(x = date, y = ret.excess))

# 看pre-follow和post-follow的偏度 (cube维度) ----
f.main1.daily <- fivefactor_daily[, setnames(.SD, 1, "date")
    ][, date := as.Date(date)
    ][f.main1, on = .(date)
    ][, ret := value / shift(value, type = "lag") - 1, by = .(cube.symbol)
    ][, period := max(date) - min(date), by = .(cube.symbol)
    ][period > 90, .SD
    ][, .SD[max(date) - follow.date > pre.period], by = .(cube.symbol)
    ][, post.follow := fifelse(date < follow.date, 0, 1), by = .(cube.symbol)
    ][, start := min(date), by = .(cube.symbol)
    ][, active.day := as.numeric(date - min(date)), by = .(cube.symbol)]

coef.pre.follow <- f.main1.daily[!(cube.symbol %in% outlier) & post.follow == 0, .(lm(ret - rf ~ mkt_rf + smb + hml + umd + rmw + cma) %>% coef()), by = .(cube.symbol)
    ][, .SD[1], by = .(cube.symbol)
    ][, skewness(V1)]

coef.post.follow <- f.main1.daily[!(cube.symbol %in% outlier) & post.follow == 1 & date - follow.date <= pre.period, .(lm(ret - rf ~ mkt_rf + smb + hml + umd + rmw + cma) %>% coef()), by = .(cube.symbol)
    ][, .SD[1], by = .(cube.symbol)
    ][, skewness(V1)]

f.main1.daily[!(cube.symbol %in% outlier) & post.follow == 0 & pre.period > 30, .(lm(ret - rf ~ mkt_rf + smb + hml + umd + rmw + cma) %>% coef()), by = .(cube.symbol)
    ][, .SD[1], by = .(cube.symbol)
    ][, ggplot(.SD, aes(x = V1)) + stat_density(geom = "line", alpha = 1, colour = "cornflowerblue")]