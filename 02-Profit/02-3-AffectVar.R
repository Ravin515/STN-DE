library(styleer)
library(lfe)
library(quantreg)
library(rqpd)
url <- str_c(getwd(), "/data/Factors/")
file.names <- list.files(path = url, pattern = "*.txt|.csv")
for (i in file.names) {
    assign(str_sub(i, start = 1L, end = -5L), fread(str_c(url, toupper(i))))
}
rm(url, file.names, i)

ld(f.main1)
ld(f.main2)
ld(f.cube.ret.sp)
ld(f.hold.price.1806)

## 提取各种变量 ----
# 1. 重新计算stock.list，stock number, stock.ratio
# 设定一个叠加股票list的函数
sadd <- function(x) {
    if (length(x) == 1) {
        x
    } else {
        as.list(Reduce(c, x, accumulate = T))
    }
}
# 1.1.1 首先计算那些第一次出现的stock
start.cube.pre.plate <- f.hold.price.1806[, .SD[1], by = .(cube.symbol, stock.symbol)
    ][prev.weight.adjusted != 0, .SD
    ][, date := as.Date(created.at)
    ][, .(stock.list.start = list(stock.symbol)), by = .(cube.symbol)]

# 1.1.2 从0-1的股票，包括那些第一次在平台买，还有那些在平台清仓之后买入多次的股票
stock0_1 <- f.hold.price.1806[prev.weight.adjusted == 0, .SD
    ][, date := as.Date(created.at)
    ][order(cube.symbol, date), .SD
    ][, .(stock.symbol = list(stock.symbol)), by = .(cube.symbol, date)
    ][, .(stock.list.buy = sadd(stock.symbol), date), by = .(cube.symbol)]

# 1.1.3 从1-0的股票, 包括那些在平台买在平台清仓那些不在平台买但在平台清仓的股票
stock1_0.second <- f.hold.price.1806[, .SD[-1], by = .(cube.symbol, stock.symbol)
    ][target.weight == 0, .SD
    ][, date := as.Date(created.at)]
stock1_0.first <- f.hold.price.1806[, .SD[1], by = .(cube.symbol, stock.symbol)
    ][target.weight == 0, .SD
    ][, date := as.Date(created.at)]
stock1_0 <- rbindlist(list(stock1_0.first, stock1_0.second), fill = T)
stock1_0 <- stock1_0[order(cube.symbol, date), .SD
    ][, .(stock.symbol = list(stock.symbol)), by = .(cube.symbol, date)
    ][, .(stock.list.sell = sadd(stock.symbol), date), by = .(cube.symbol)]

# 1.1.4 将三张表合并到ret为基准的cube.symbol和date表中
stock.info <- stock0_1[stock1_0[start.cube.pre.plate[f.cube.ret.sp[, .(cube.symbol, date)], on = .(cube.symbol)], on = .(cube.symbol, date), roll = T], on = .(cube.symbol, date), roll = T]

# 1.1.5 计算三个list的补集
stock.info <- stock.info[, .(stock.list = which(table(c(unlist(stock.list.start), unlist(stock.list.buy), unlist(stock.list.sell))) %% 2 != 0) %>% names() %>% list()), by = .(cube.symbol, date)]

sv(stock.info, svname = "stock.info")
f.main1 <- stock.info[f.main1[, label := NULL], on = .(cube.symbol, date)
    ][, stock.num := lapply(stock.list, length) %>% unlist()]
sv(f.main1, svname = "f.main1")
f.main2 <- stock.info[f.main2[, label := NULL], on = .(cube.symbol, date)
    ][, stock.num := lapply(stock.list, length) %>% unlist()]
sv(f.main2, svname = "f.main2")

##1.2# 计算每天持仓比例 stock.ratio
#ld(stock.info)
#cl <- makeCluster(8)
#registerDoParallel(cl)
#stock.ratio <- f.cube.rb.sp[, .(cube.symbol, date = as.Date(created.at), created.at, stock.symbol, price, target.weight, prev.weight.adjusted)
    #][stock.info, on = .(cube.symbol, date)
    #][!is.na(created.at)
    #][order(cube.symbol, date, created.at, stock.symbol), .SD
    #][,
        #.(stock.ratio = {
            ##ratio <- list()
            #foreach(i = 1:.N) %dopar% {
                #library(data.table)
                #sl1 <- unlist(stock.list[i])
                #p.fward <- .SD[i:.N
                    #][order(stock.symbol, created.at)
                    #][stock.symbol %in% sl1, .SD[1], .SDcols = c("date", "created.at", "target.weight", "prev.weight.adjusted"), by = .(stock.symbol)]

                #sl2 <- setdiff(sl1, p.fward[["stock.symbol"]])
                #p.bward <- .SD[1:i
                    #][order(stock.symbol, created.at)
                    #][stock.symbol %in% sl2, .SD[.N], .SDcols = c("date", "created.at", "target.weight", "prev.weight.adjusted"), by = .(stock.symbol)]

                #fp <- p.fward[, .(weight = fifelse(prev.weight.adjusted > 0, prev.weight.adjusted, target.weight), stock.symbol)]
                #bp <- p.bward[, .(weight = fifelse(target.weight > 0, target.weight, prev.weight.adjusted), stock.symbol)]
                #rbindlist(list(fp, bp))
            #}
            ##ratio
            #}
        #, date, created.at)
        #, by = .(cube.symbol)
        #]
#sv(stock.ratio, svname = "stock.ratio")

#stock.ratio.flat <- stock.ratio[, .SD[.N], by = .(cube.symbol, date)
    #][, rbindlist(stock.ratio), by = .(date, cube.symbol)
    #][order(cube.symbol, date)]

#sv(stock.ratio.flat, svname = "stock.ratio.flat")

# 2. 打新。将是否打新数据加入数据集，加入一周之内是否有打新
#f.main1 <- f.cube.ret.sp[, .(cube.symbol, date, label)
    #][f.main1, on = .(cube.symbol, date)
    #][, new.stock := fifelse(label == "", 0, 1)
    #][, new.stock.7day := Reduce('+', shift(new.stock, 0:5, fill = 0)), by = .(cube.symbol)
    #][, new.stock.7day := fifelse(new.stock.7day > 1, 1, new.stock.7day)
    #]


# 3. top10变量，是否累积收益率为周、月、年的前10位，雪球本身的机制 (再议)
#f.cube.ret.sp[date > as.Date("2016-06-01"),
    #ret_accu_week := {
        #ret <- vector()
        #for (i in 7:.N) {
            #ret[i] <- value[i] / value[i - 6] - 1
        #}
        #ret
    #}, by = .(cube.symbol)
    #]

#f.cube.ret.sp[date > as.Date("2016-06-01"),
    #ret_accu_month := {
        #ret <- vector()
        #for (i in 30:.N) {
            #ret[i] <- value[i] / value[i - 29] - 1
        #}
        #ret
    #}, by = .(cube.symbol)
    #][is.infinite(ret_accu_month), ret_accu_month := NA]

#f.cube.ret.sp[date > as.Date("2016-06-01"),
    #ret_accu_year := {
        #ret <- vector()
        #for (i in 360:.N) {
            #ret[i] <- value[i] / value[i - 359] - 1
        #}
        #ret
    #}, by = .(cube.symbol)]

#f.cube.ret.sp[sapply(f.cube.ret.sp, is.infinite)] <- NA

#week_top <- f.cube.ret.sp[!is.na(ret_accu_week), .SD
    #][order(date, - ret_accu_week), .SD
    #][, tag := .N, by = .(date)
    ##][ret_accu_week < quantile(ret_accu_week, probs = 0.997), .SD
    #][, .SD[1:10], .SDcols = c("cube.symbol", "ret_accu_week"), by = .(date)
    #]

#month_top <- f.cube.ret.sp[!is.na(ret_accu_month), .SD
    #][order(date, - ret_accu_month), .SD
    #][, tag := .N, by = .(date)
    ##][ret_accu_week < quantile(ret_accu_week, probs = 0.990, na.rm = T), .SD
    #][, .SD[1:10], .SDcols = c("cube.symbol", "ret_accu_month"), by = .(date)
    #]



# 4. 交易数量
# 4.1 除了交易数量以外，将买入和卖出的数量同时拿出
#ld(f.cube.rb.mst.1803)
#f.cube.rb.sp <- f.cube.rb.mst.1803[cube.type == "SP"]
#sv(f.cube.rb.sp, svname = "f.cube.rb.sp")

ld(f.cube.rb.sp.mst.1806)
f.cube.rb.sp <- f.cube.rb.sp.mst.1806[, .(cube.symbol, created.at, stock.symbol, price, target.weight, prev.weight.adjusted)
    ][order(cube.symbol, stock.symbol, created.at), .SD]

# 4.2 将nan的数据填充并计算累积的买入和卖出的交易数量
f.cube.rb.sp<- f.cube.rb.sp[!is.nan(target.weight) & !is.nan(prev.weight.adjusted)
    ][!(target.weight == 0 & prev.weight.adjusted == 0), .SD
    ][, buy := fifelse(target.weight > prev.weight.adjusted, 1, 0)
    ][, sell := fifelse(target.weight > prev.weight.adjusted, 0, 1)
    ][, date := as.Date(created.at)
    ][order(cube.symbol, date), .SD
    ][, ':='(buy.num = cumsum(buy), sell.num = cumsum(sell)), by = .(cube.symbol)
    ]
order.num <- f.cube.rb.sp[, .SD[.N], by = .(cube.symbol, date)
    ][, .(cube.symbol, date, buy.num, sell.num)]

f.main1 <- order.num[f.main1, on = .(cube.symbol, date), roll = T, rollends = c(F, T)]
f.main1[, ':='(buy.num = fifelse(is.na(buy.num), 0, buy.num), sell.num = fifelse(is.na(sell.num), 0, sell.num))
    ][, buy.num.30day := (buy.num - shift(buy.num, type = "lag", n = 22L)) / 22, by = .(cube.symbol)
    ][, sell.num.30day := (sell.num - shift(sell.num, type = "lag", n = 22L)) / 22, by = .(cube.symbol)]

# 5. 停牌
# 5.1 中间存在一些停牌的情况，比如股票数量不为0，但ret为0
#f.main1[, stop := fifelse(stock.num != 0 & ret == 0, 1, 0)
    #][is.na(stop), stop := 0
    #][, tag := stop - shift(stop, type = "lag", n = 1L), by = .(cube.symbol)
    #][, stop.open := fifelse(tag == -1, 1, 0)
    #][, tag := NULL]

# 6. 持有的股票多样性
# 计算过去七天之内持有的不重复股票数量
f.main1[pre.period > 3, stock.num.7day := {
    a <- vector()
    for (i in 6:.N) {
        a[i] <- lapply(stock.list[(i-5):i], unlist) %>% unlist()  %>% unique() %>% length()
    }
    a
}, by = .(cube.symbol)]

# 计算过去七天内持有股票的平均数量
f.main1[pre.period > 3, stock.mean.7day := {
    a <- vector()
    for (i in 6:.N) {
        a[i] <- mean(stock.num[(i-5):i])
    }
    a
}, by = .(cube.symbol)]

f.main1.daily <- fivefactor_daily[, setnames(.SD, 1, "date")
    ][, date := as.Date(date)
    ][f.main1, on = .(date)
    ][order(cube.symbol, date), .SD
    ][, ret := value / shift(value, type = "lag") - 1, by = .(cube.symbol)
    ][is.infinite(ret), ret := 0
    ][, period := max(date) - min(date), by = .(cube.symbol)
    ][period > 90, .SD
    ][, .SD[max(date) - follow.date > pre.period], by = .(cube.symbol)
    ][, start := min(date), by = .(cube.symbol)
    ][, post.follow := fifelse(date > follow.date, 1, 0), by = .(cube.symbol)
    ][, active.day := date - min(date), by = .(cube.symbol)]

#### 回归
outlier <- f.main1.daily[ret > 0.1 | ret < -0.1, unique(cube.symbol)]

f.main1.daily[!(cube.symbol %in% outlier) & (post.follow == 0 | post.follow == 1 & (date - follow.date < pre.period)), felm(ret ~ post.follow + mkt_rf + smb + hml + umd + rmw + cma  | cube.symbol + follow.date)] %>% summary()

f.main1.daily[!(cube.symbol %in% outlier) & (post.follow == 0 | post.follow == 1 & (date - follow.date < pre.period)), felm(ret ~ post.follow + mkt_rf + smb + hml + umd + rmw + cma + I(active.day * post.follow) + active.day | cube.symbol + follow.date)] %>% summary() # 活跃天数

f.main1.daily[!(cube.symbol %in% outlier) & (post.follow == 0 | post.follow == 1 & (date - follow.date < pre.period)), felm(ret ~ post.follow + mkt_rf + smb + hml + umd + I((buy.num.30day + sell.num.30day) * post.follow) + I(buy.num.30day + sell.num.30day) | cube.symbol + follow.date)] %>% summary()

f.main1.daily[(!(cube.symbol %in% outlier) & (post.follow == 0) | post.follow == 1 & (date - follow.date < pre.period)), felm(ret ~ post.follow + mkt_rf + smb + hml + umd + buy.num.30day + I(buy.num.30day * post.follow) + sell.num.30day + I(sell.num.30day * post.follow) | cube.symbol + follow.date)] %>% summary()

#f.main1.daily[(post.follow == 0 | (post.follow == 1 & (date - follow.date < pre.period))), felm(ret ~ post.follow + mkt_rf + smb + hml + umd + new.stock + I(post.follow * new.stock) + new.stock | cube.symbol + follow.date)] %>% summary() # 当天是否有打新

#f.main1.daily[(post.follow == 0 | (post.follow == 1 & (date - follow.date < pre.period))), felm(ret ~ post.follow + mkt_rf + smb + hml + umd + new.stock.7day + I(post.follow * new.stock.7day) | cube.symbol + follow.date)] %>% summary() # 过去七天是否有打新

#f.main1.daily[ret < 1 & ret > -1 & (post.follow == 0), felm(ret ~ post.follow + mkt_rf + smb + hml + umd + new.stock + I(post.follow * new.stock) + new.stock.7day + I(post.follow * new.stock.7day) | cube.symbol + follow.date)] %>% summary()

f.main1.daily[!(cube.symbol %in% outlier) & (post.follow == 0 | (post.follow == 1 & (date - follow.date < pre.period))), felm(ret ~ post.follow + mkt_rf + smb + hml + umd + stock.num + I(stock.num * post.follow) | cube.symbol + follow.date)] %>% summary() # 当天股票持有数量

f.main1.daily[!(cube.symbol %in% outlier) & (post.follow == 0 | (post.follow == 1 & (date - follow.date < pre.period))), felm(ret ~ post.follow + mkt_rf + smb + hml + umd + stock.mean.7day + I(stock.mean.7day * post.follow) | cube.symbol + follow.date)] %>% summary() # 过去七天内持有过的股票平均数

f.main1.daily[!(cube.symbol %in% outlier) & (post.follow == 0 | (post.follow == 1 & (date - follow.date < pre.period))), felm(ret ~ post.follow + mkt_rf + smb + hml + umd + stock.num.7day + I(stock.num.7day * post.follow) | cube.symbol + follow.date)] %>% summary() # 过去七天内持有过的不重复的股票数量

f.main1.daily[!(cube.symbol %in% outlier) & (post.follow == 0 | (post.follow == 1 & (date - follow.date < pre.period))), felm(ret ~ post.follow + mkt_rf + smb + hml + umd + stock.num + I(stock.num * post.follow) + stock.mean.7day + I(stock.mean.7day * post.follow) + stock.num.7day + I(stock.num.7day * post.follow) | cube.symbol + follow.date)] %>% summary()

#f.main1.daily[ret < 1 & (post.follow == 0 | (post.follow == 1 & (date - follow.date < pre.period))), felm(ret ~ post.follow + mkt_rf + smb + hml + umd + stop.open + I(stop.open * post.follow) | cube.symbol + follow.date)] %>% summary() # 停牌后开盘第一天

f.main1.daily[!(cube.symbol %in% outlier) & (post.follow == 0 | (post.follow == 1 & (date - follow.date < pre.period))), felm(ret ~ post.follow + active.day + buy.num.30day + sell.num.30day + stock.num + stock.mean.7day + stock.num.7day | cube.symbol + follow.date)] %>% summary()