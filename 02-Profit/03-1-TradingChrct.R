library(styleer)
library(lfe)
ld(stock.ratio)
ld(stock.ratio.flat)
ld(f.main1)
ld(sample1)
ld(f.hold.price)

f.hold.price <- f.hold.price[order(cube.symbol, stock.symbol, created.at), .SD
    ][, hold.price.lag := shift(hold.price, type = "lag"), by= .(cube.symbol)]

# 0.0 已实现利润计算（realized gain）----
rb.1 <- sample1[, unique(.SD), .SDcols = c("cube.symbol", "follow.date", "pre.period")
    ][f.cube.rb.sp[, .(cube.symbol, created.at, date = as.Date(created.at), stock.symbol, prev.weight.adjusted, target.weight, price)], on = .(cube.symbol), nomatch = 0
    ][, post.follow := fifelse(follow.date > date, 0, 1)
    ][, ':='(sell = fifelse(target.weight < prev.weight.adjusted, 1, 0), buy = fifelse(target.weight > prev.weight.adjusted, 1, 0))]
sell.postfollow.1 <- rb.1[post.follow == 1 & date - follow.date < follow.date & sell == 1]
sell.postfollow.1 <- f.hold.price[, .(cube.symbol, created.at, stock.symbol, hold.price.lag)
    ][sell.postfollow.1, on = .(cube.symbol, created.at, stock.symbol)
    #][, .SD, .SDcols = -c(11:13)
    ][, realized.gain := (prev.weight.adjusted - target.weight) * (price - hold.price.lag)
    ][, realized.gain.sum := sum(realized.gain, na.rm = T), by = .(cube.symbol)]

sell.prefollow.1 <- rb.1[post.follow == 0 & sell == 1]
sell.prefollow.1 <- f.hold.price[, .(cube.symbol, created.at, stock.symbol, hold.price.lag)
    ][sell.prefollow.1, on = .(cube.symbol, created.at, stock.symbol)
    #][, .SD, .SDcols = -c(11:13)
    ][, realized.gain := (prev.weight.adjusted - target.weight) * (price - hold.price.lag)
    ][, realized.gain.sum := sum(realized.gain, na.rm = T), by = .(cube.symbol)]

sell.record.1 <- rbindlist(list(sell.postfollow.1, sell.prefollow.1))

sell.record.1[, t.test(realized.gain.sum ~ post.follow)]
sell.record.1[, felm(realized.gain ~ post.follow | cube.symbol + follow.date)] %>% summary()

# 0.1 follow前后的交易频率t检验
trd.freq <- rb.1[post.follow == 0 | (post.follow == 1 & date - follow.date < follow.date), .(trd.freq = sum(.N) / as.numeric(pre.period)), by = .(cube.symbol, post.follow)
    ][, unique(.SD)]
trd.freq[, t.test(trd.freq ~ post.follow)]


# 1. 对每笔交易之后的收益率进行分析----
ld(f.hold.price)
ld(r.cube.info.1803)
ld(f.cube.ret.sp)

r.cube.info <- r.cube.info[cube.type == "SP", .(cube.symbol, close.date)]



# 对hold.price进行处理
f.hold.price <- f.hold.price[, tag := fifelse(hold.price == 0, 1, 0)
    ][, tag := shift(tag, type = "lag", n = 1L), by = .(cube.symbol, stock.symbol)
    ][, tag := fifelse(is.na(tag), 0, tag)
    ][, tag := cumsum(tag), by = .(cube.symbol, stock.symbol)
    ][, stock.symbol.tag := str_c(stock.symbol, tag, sep = "_")
    ][, date := as.Date(created.at)
    ][, ':=' (prev.weight.adjusted.daily  = prev.weight.adjusted[1], target.weight.daily = target.weight[.N], price.daily = mean(price)), by =.(cube.symbol, stock.symbol.tag, date) #计算每天每只股票的调仓比例
    ][, .SD[.N], by = .(cube.symbol, stock.symbol.tag, date)]

# 与cube.info表进行合并
f.hold.price <- r.cube.info[f.hold.price, on = .(cube.symbol)
    ][, close.date := fifelse(is.na(close.date), max(date), close.date)]
rm(r.cube.info)

# 计算出每一只股票持有的时间长度
f.hold.price[, stock.max.date := {
    a <- vector()
    class(a) <- "Date"
        for (i in 1:.N) {
            if (target.weight[.N] == 0) {
                a[i] <- as.Date(date[.N], "%Y-%m-%d")
            }
            else {
                a[i] <- unique(close.date) %>% as.Date("%Y-%m-%d")
            }
        }
        as.Date(a, "1970-01-01")
}, by = .(cube.symbol, stock.symbol.tag)
][, stock.max.date := as.Date(stock.max.date, "1970-01-01")]

# 将每一个cube的净值拿出来，并计算lag净值，最后与f.hold.price合并
f.hold.price <- f.cube.ret.sp[, .(date, value, value.lag = shift(value, type = "lag")), by = .(cube.symbol)
    ][f.hold.price, on = .(cube.symbol, date)]
f.hold.price[, value.lag := fifelse(is.na(value.lag), value, value.lag)]

# 计算每笔交易所花金钱，或者所得金钱，再计算卖出多少股票
f.hold.price <- f.hold.price[, tag := 1:.N, by = .(cube.symbol, stock.symbol.tag)
    ][, cost := fifelse(tag == 1,
        (value.lag * target.weight / 100) / (1- target.weight / 100), 
        (value.lag * target.weight / 100 - value.lag * prev.weight.adjusted / 100) / (1 - (target.weight /100))), by = .(cube.symbol, stock.symbol.tag)
    ][, vol.change := cost / price
    ][order(cube.symbol, stock.symbol.tag, date), .SD
    ][, sell.not.clean := fifelse(target.weight < prev.weight.adjusted & target.weight != 0, 1, 0)
    ][, sell.clean := fifelse(target.weight < prev.weight.adjusted & target.weight == 0, 1, 0)
    ][, buy := fifelse(target.weight > prev.weight.adjusted, 1, 0)]

tim <- f.hold.price[, .(date = seq(min(date), unique(stock.max.date), by = "day")), by = .(cube.symbol, stock.symbol.tag)]

# 计算出所有cube的所有hold.price
f.trd.sell.buy <- f.hold.price[, unique(.SD), .SDcols = c("cube.symbol", "stock.symbol.tag", "date", "hold.price", "price.daily", "target.weight.daily", "prev.weight.adjusted.daily")
    ][f.hold.price[, .(cube.symbol, stock.symbol.tag, date, created.at, price, target.weight, prev.weight.adjusted)
    ][tim, on = .(cube.symbol, stock.symbol.tag, date)],
    on = .(cube.symbol, stock.symbol.tag, date), roll = T
    ][!(hold.price == 0 & is.na(price)), unique(.SD)]

sv(f.trd.sell.buy, svname = "f.trd.sell.buy")

ld(f.trd.sell.buy)
ld(sample1)
cube <- sample1[, unique(cube.symbol)]
f.trd.sell.buy1 <- f.trd.sell.buy[cube.symbol %in% cube, .SD
    ][, stock.symbol := str_replace_all(stock.symbol.tag, "_.+", "") %>% str_sub(start = 3L, end = -1L)]

# 导入股票每天的收盘价
url <- str_c(getwd(), "/data/Clprc")
clprc <- fbread(path = url, pattern = "*.txt")
clprc<- clprc[, file_id := NULL
    ][, ":="(stock.symbol = (formatC(Stkcd, flag = '0', width = 6)), date = as.Date(Trddt))
    ][, .SD, .SDcols = c("stock.symbol", "date", "Clsprc")]

f.trd.sell.buy1 <- clprc[f.trd.sell.buy1, on = .(stock.symbol, date)
    ][!is.na(Clsprc), .SD]

# 导入每天的沪深三百指数
url <- str_c(getwd(), "/data/Index")
index <- fbread(path = url, pattern = "*\\d.txt")
index <- index[, file_id := NULL
    ][Indexcd == "000300"
    ][, ':='(index_ret_daily = Idxtrd08 / 100, index_value = Idxtrd05, date = as.Date(Idxtrd01))
    ][, .(index_ret_daily, index_value, date)
    ]
f.trd.sell.buy1 <- index[f.trd.sell.buy1, on = .(date)]


# 计算每天的各类收益率
f.trd.sell.buy1[, hold.price.lag := shift(hold.price, type = "lag", n = 1L), by = .(cube.symbol, stock.symbol.tag) # 计算一个之后持仓价
    ][, stock.net.value := Clsprc * target.weight.daily/100
    ][target.weight.daily != 0, stock.ret.daily := stock.net.value/shift(stock.net.value, type = "lag") - 1, by = .(cube.symbol, stock.symbol.tag) # 计算每日收益率
    ][, stock.abrnm.ret := stock.ret.daily - index_ret_daily # 计算每日超额收益率
    ][, date.sequence := 1:.N, by = .(cube.symbol, stock.symbol.tag) # 将每一只股票的持有进行标记序号
    ][target.weight.daily != 0, stock.cum.ret := stock.net.value / stock.net.value[1] - 1, by = .(cube.symbol, stock.symbol.tag) # 计算每日累积收益率
    ][, index.cum.ret := index_value/index_value[1] - 1, by = .(cube.symbol, stock.symbol.tag) # 计算指数每日的累积收益率
    ][, stock.cum.abrnm.ret := stock.cum.ret - index.cum.ret] # 计算每日累积超额收益率

# 导入followdate, pre.period
f.trd.sell.buy1 <- sample1[, unique(.SD), .SDcols = c("cube.symbol", "follow.date", "pre.period")
    ][f.trd.sell.buy1, on = .(cube.symbol)]

# 标记follow前后
f.trd.sell.buy1[, post.follow := fifelse(date < follow.date, 0, 1)]
a <- f.trd.sell.buy1[post.follow == 0, .(cube.stock.abrnm.ret.daily = median(stock.ret.daily, na.rm = T)), by = .(date.sequence)]
b <- f.trd.sell.buy1[post.follow == 1 & date - as.Date(follow.date) <= pre.period, .SD
    #][, date.sequence := 1:.N, by = .(cube.symbol, stock.symbol.tag)
    ][, .(cube.stock.abrnm.ret.daily = median(stock.ret.daily, na.rm = T)), by = .(date.sequence)]

f.trd.sell.buy1[post.follow == 0, .(cube.stock.cum.abrnm.ret.daily = median(stock.ret.daily)), by = .(date.sequence)]
f.trd.sell.buy1[post.follow == 1 & date - as.Date(follow.date) <= pre.period, .(cube.stock.cum.abrnm.ret.daily = median(stock.ret.daily)), by = .(date.sequence)]

# 持有期为X轴的平均超额收益率图
ggplot() +
    geom_line(a, mapping = aes(x = date.sequence, y = cube.stock.abrnm.ret.daily), color = "blue", size = 1) +
    geom_line(b, mapping = aes(x = date.sequence, y = cube.stock.abrnm.ret.daily), color = "red", size = 1)

# 持有期为X轴的平均累积超额收益率图
ggplot() +
    geom_line(f.trd.sell.buy1[!is.na(stock.cum.abrnm.ret) & post.follow == 0, .(cube.stock.cum.abrnm.ret.daily = median(stock.cum.abrnm.ret, na.rm = T)), by = .(date.sequence)], mapping = aes(x = date.sequence, y = cube.stock.cum.abrnm.ret.daily), color = "blue", size = 1) +
    geom_line(f.trd.sell.buy1[!is.na(stock.cum.abrnm.ret) & post.follow == 1 & date - as.Date(follow.date) <= pre.period, .(cube.stock.cum.abrnm.ret.daily = median(stock.cum.abrnm.ret, na.rm = T)), by = .(date.sequence)], mapping = aes(x = date.sequence, y = cube.stock.cum.abrnm.ret.daily), color = "red", size = 1)