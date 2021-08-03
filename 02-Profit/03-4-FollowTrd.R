# 1. 加入closed price建立每个cube的每个stock仓位从建仓直到结束每天的仓位情况 ----
library(styleer)
# 1.1 f.main1中的cube情况
# 导入f.main1得到每一个cube的closed.date
ld(f.main1)
f.main1[, ret := value / shift(value, type = "lag") - 1, by = .(cube.symbol)]
f.main1.date <- f.main1[, .(closed.date = max(date), follow.date, date), by = .(cube.symbol)
    ][, unique(.SD)]

# 导入股票收盘价
clprc <- fbread(path = "./data/Clprc", pattern = "*.txt")
clprc <- clprc[, .(stock.symbol = str_pad(Stkcd, 6, side = "left", pad = "0"), date = as.Date(Trddt), Clsprc)]

# 对f.lifo进行处理
ld(f.fifo, force = T)
f.fifo <- f.fifo[, .SD[-1]]
f.fifo <- f.fifo[, date := as.Date(created.at)
    ][, stock.symbol := str_sub(stock.symbol.tag, start = 3L, end = 8L)
    ][order(cube.symbol, stock.symbol.tag, created.at), .SD
    ][, tag.hold := fifelse(target.weight[.N] != 0, 1, 0), by = .(cube.symbol, stock.symbol.tag) # 标记出最后一笔不是清仓的仓位
    ]
# f.fifo与f.main1.date里面的closed.date合并
f.fifo.main1 <- f.main1.date[, unique(.SD), .SDcols = c("cube.symbol", "closed.date", "follow.date")
    ][f.fifo, on = .(cube.symbol)
    ][cube.symbol %in% unique(f.main1$cube.symbol)]

# 创造两个date和cube的交集
## 对于仓位中最后一笔不清仓的选择这个cube交易的最后时间
period1.1 <- f.fifo.main1[tag.hold == 1, .SD
    ][, .SD[min(date) >= unique(closed.date)], by = .(cube.symbol, stock.symbol.tag)
    ][, closed.date := fifelse(max(date) == closed.date, closed.date, max(date) + 7), by = .(cube.symbol, stock.symbol.tag)
    ][, .(date = seq(min(date), unique(closed.date), by = "day"), follow.date = unique(follow.date)), by = .(cube.symbol, stock.symbol.tag)]

period1.2 <- f.fifo.main1[tag.hold == 1, .SD
    ][, .SD[min(date) < unique(closed.date)], by = .(cube.symbol, stock.symbol.tag)
    ][, .(date = seq(min(date), unique(closed.date), by = "day"), follow.date = unique(follow.date)), by = .(cube.symbol, stock.symbol.tag)]

period1 <- rbindlist(list(period1.1, period1.2), fill = T, use.names = T)
rm(period1.1, period1.2)

## 对于仓位中最后一笔清仓的选择当前清仓的时间
period2 <- f.fifo.main1[tag.hold == 0, .SD
    ][, .(date = seq(min(date), max(date), by = "day"), follow.date = unique(follow.date)), by = .(cube.symbol, stock.symbol.tag)]

period.main1 <- rbindlist(list(period1, period2), fill = T, use.names = T)

# 与clprc进行合并
position.main1 <- period.main1[str_detect(stock.symbol.tag, "SH|SZ"), .SD
    ][, stock.symbol := str_sub(stock.symbol.tag, start = 3L, end = 8L)
    ][clprc, on = .(stock.symbol, date), nomatch = NA
    ][, stock.symbol := str_sub(stock.symbol.tag, start = 1L, end = 8L)
    ][order(cube.symbol, stock.symbol.tag, date), .SD]

# 与f.fifo.main1进行合并， 并roll数据
f.fifo.main1 <- f.fifo.main1[, .SD, .SDcols = -c("stock.symbol", "tag.hold", "follow.date")
    ][position.main1, on = .(cube.symbol, stock.symbol.tag, date), roll = T]
# 添加follow.date变量
#f.fifo.main1 <- f.main1.date[, unique(.SD), .SDcols = c("cube.symbol", "follow.date")
    #][f.fifo.main1, on = .(cube.symbol)]
#f.fifo.main1[, post.follow := fifelse(date[1] < follow.date, 0, 1), by = .(cube.symbol, stock.symbol.tag)]

# 1.2 导入调仓跟随的表进行处理 ----
# f.rb.followers.followings根据f.main1的cube进行计算
ld(f.rb.followers.followings)
#f.rb.followers.followings[, uniqueN(.SD), .SDcols = c("from.cube.symbol", "from.created.at", "to.cube.symbol")]
f.rb.ff <- f.rb.followers.followings[, time.interval := difftime(from.created.at, to.created.at, units = "days")
    ][time.interval < 28, unique(.SD)
    ][, .SD[time.interval == min(time.interval)], by = .(from.cube.symbol, from.created.at, to.stock.symbol)
    ][from.cube.symbol %in% unique(f.main1$cube.symbol), .SD]

# 1.3 将f.fifo.main1的持仓在每天进行合并并展开 ----
# 将每天的每一个cube的每一个stock的交易提取最后一笔
# 所需提取的有两部分：
# 持仓的部分和最后清仓的部分

# 首先将每天每个cube的每一个stock的交易进行合并，并将最后一笔是清仓的仓位标记出来
f.fifo.position <- f.fifo.main1[order(cube.symbol, stock.symbol.tag, created.at), .SD
    ][, .SD[.N], by = .(cube.symbol, stock.symbol.tag, date)
    ][, num.is.null := fifelse(sapply(open.position, length) == 0, 1, 0)
    ][, clean.price := fifelse(sum(num.is.null) > 0, price[.N], 0), by = .(cube.symbol, stock.symbol.tag)]

# 持仓的仓位
# 将每一天的仓位进行展开
f.fifo.position.hold <- f.fifo.position[, rbindlist(open.position), by = .(cube.symbol, stock.symbol.tag, date)]
f.fifo.position.hold <- f.fifo.position.hold[order(cube.symbol, stock.symbol.tag, hold.id, date), .SD
    ][, stock.symbol := str_sub(stock.symbol.tag, start = 1L, end = 8L)]

f.fifo.position.hold <- f.fifo.position[, unique(.SD), .SDcols = c("stock.symbol", "date", "Clsprc")
    ][f.fifo.position.hold, on = .(stock.symbol, date)]

# 清仓的仓位
f.fifo.position.clean <- f.fifo.position[clean.price > 0, .SD[(.N - 1):.N], by = .(cube.symbol, stock.symbol.tag)]
f.fifo.position.clean <- f.fifo.position.clean[, .(Clsprc = unique(clean.price), date = date[.N]), by = .(cube.symbol, stock.symbol.tag)
    ][f.fifo.position.clean[, ':='(date = max(date), Clsprc = clean.price), by = .(cube.symbol, stock.symbol.tag)
        ][, rbindlist(open.position[1]), by = .(cube.symbol, stock.symbol.tag, date)]
        , on = .(cube.symbol, stock.symbol.tag, date)
    ][, stock.symbol := str_sub(stock.symbol.tag, start = 1L, end = 8L)]

# 将两种仓位合并
f.fifo.position.all <- rbindlist(list(f.fifo.position.clean, f.fifo.position.hold), fill = T, use.names = T)
f.fifo.position.all <- f.fifo.position.all[order(cube.symbol, stock.symbol.tag, hold.id, date), .SD
    ][hold.weight != 0, .SD]

# 1.4 f.fifo.position.all与f.main1.date进行合并----
f.fifo.position.all <- f.main1[, unique(.SD), .SDcols = c("cube.symbol","pre.period", "follow.date")
    ][f.fifo.position.all, on = .(cube.symbol)
    ][, post.follow.full := fifelse(date[1] >= follow.date, 1, 0), by = .(cube.symbol, stock.symbol.tag) # 标记出哪些仓位在post.follow之前或者之后
    ]

f.fifo.position.all[, ret.daily := Clsprc/shift(Clsprc, type = "lag") - 1]

# 1.5 进行统计分析 ----
library(lfe)
# 需要对两组对比的持仓构建portfolio
# 导入所有的跟随的交易对应所有的交易id
f.fifo.position.all <- f.fifo.main1[, unique(.SD), .SDcols = c("cube.symbol", "stock.symbol", "id", "created.at")
    ][f.rb.ff, on = .(cube.symbol = from.cube.symbol, stock.symbol = to.stock.symbol, created.at = from.created.at)
    ][!is.na(id), .(id, follow.trd = 1)
    ][f.fifo.position.all, on = .(id = hold.id)]
f.fifo.position.all[, follow.trd := fifelse(is.na(follow.trd), 0, 1)]

# 导入各类因子并合并
url <- str_c(getwd(), "/data/Factors/")
file.names <- list.files(path = url, pattern = "*.txt|.csv")
for (i in file.names) {
    assign(str_sub(i, start = 1L, end = -5L), fread(str_c(url, toupper(i))))
}

f.fifo.position.all <- fivefactor_daily[, setnames(.SD, 1, "date")
    ][f.fifo.position.all, on = .(date)]

# 所有投资者的copy交易从头到尾组成一个portfolio
## 提取出follow-trading的交易，建立portfolio
position.post.follow.trd <- f.fifo.position.all[follow.trd == 1, .SD
    ][order(date), .SD]
ret.post.follow.trd <- position.post.follow.trd[ret.daily > -0.1 & ret.daily < 0.1, .SD
    ][, .(ret.daily.aver = mean(ret.daily), rf, mkt_rf, smb, hml, umd, rmw, cma), by = .(date)
    ][, unique(.SD)]
ret.post.follow.trd[, felm(ret.daily.aver - rf ~ mkt_rf + smb + hml + umd + rmw + cma)] %>% summary()

# 1.5.1 首先构建平行时间的portfolio（择股）
## 首先构建所有投资者在同一时间其他未follow的交易组合
## 再者构建相同投资者在同一时间内所有的持仓的交易组合
## 最后构建相同投资者在相同时间买入的其他持仓组合在同一时间持仓的交易组合

## 提取出non-follow-trading，建立portfolio，根据全时间样本
#position.post.non.follow.trd <- f.fifo.position.all[(follow.trd == 0 & post.follow.full == 1), .SD
    #][cube.symbol %in% unique(position.post.follow.trd$cube.symbol), .SD
    #][date %in% unique(position.post.follow.trd$date), .SD
    #][order(date), .SD]

#ret.post.non.follow.trd <- position.post.non.follow.trd[ret.daily > -0.1 & ret.daily < 0.1, .SD
    #][, .(ret.daily.aver = mean(ret.daily), rf, mkt_rf, smb, hml, umd, rmw, cma), by = .(date)
    #][, unique(.SD)]
#ret.post.non.follow.trd[, felm(ret.daily.aver - rf ~ mkt_rf + smb + hml + umd + rmw + cma)] %>% summary()

## 提取出non-follow-trading，建立portfolio，根据每一个cube的时间窗口
position.post.non.follow.trd.cube <- f.fifo.position.all[follow.trd == 1 | post.follow.full == 1, .SD
    ][cube.symbol %in% unique(position.post.follow.trd$cube.symbol), .SD
    ][follow.trd == 1, ':='(follow.trd.min.date = min(date), follow.trd.max.date = max(date)), by = .(cube.symbol)
    ][, ':='(follow.trd.min.date = fifelse(is.na(follow.trd.min.date), unique(follow.trd.min.date[!is.na(follow.trd.min.date)]), follow.trd.min.date), follow.trd.max.date = fifelse(is.na(follow.trd.max.date), unique(follow.trd.max.date[!is.na(follow.trd.max.date)]), follow.trd.max.date)), by = .(cube.symbol)
    ][, .SD[date %between% c(unique(follow.trd.min.date), unique(follow.trd.max.date))], by = .(cube.symbol) # 筛选那些处于所有follow.trd时间窗口之间的仓位
    ][follow.trd == 0 & post.follow.full == 1, .SD
    ][order(date), .SD]

ret.post.non.follow.trd.cube <- position.post.non.follow.trd.cube[ret.daily > -0.1 & ret.daily < 0.1, .SD
    ][, ret.daily.cube := sum(hold.weight * ret.daily) / sum(hold.weight), by = .(cube.symbol, date) # 属于同一个cube的股票需要根据所持有的比例进行加权加总
    ][, unique(.SD), .SDcols = c("cube.symbol", "date", "ret.daily.cube", "rf", "mkt_rf", "smb", "hml", "umd", "rmw", "cma") 
    ][, .(ret.daily.aver = mean(ret.daily.cube), rf, mkt_rf, smb, hml, umd, rmw, cma), by = .(date)
    ][, unique(.SD)]
ret.post.non.follow.trd.cube[, felm(ret.daily.aver - rf ~ mkt_rf + smb + hml + umd + rmw + cma)] %>% summary()

# 同一投资者在同一时间买入的其他股票，建立portfolio
# 这里需要建立两个position，找出同一时间买入的其他股票之后，生成持有时间更短的那只股票
position.post.follow.trd.cube.date <- f.fifo.position.all[follow.trd == 1 | post.follow.full == 1, .SD
    ][cube.symbol %in% unique(position.post.follow.trd$cube.symbol), .SD
    ][order(cube.symbol, stock.symbol.tag, date), .SD
    ][, first.date := date[1], by = .(cube.symbol, stock.symbol.tag) # 标记每个仓位建仓的时间
    ][, .SD[first.date %in% .SD[follow.trd == 1, first.date]], by = .(cube.symbol) # 把每个cube中建仓时间与follow交易建仓时间相同的交易提取出来
    ][order(cube.symbol, stock.symbol.tag, date), .SD
    ][, period := .N, by = .(cube.symbol, stock.symbol.tag) # 计算每个仓位的持有时间
    ][, .SD[uniqueN(stock.symbol.tag) > 1 & length(follow.trd[follow.trd == 0]) > 0], by = .(cube.symbol, first.date) # 将每个cube中建仓时间相同的仓位提取出来
    ][, .SD[(.SD[follow.trd == 1, uniqueN(stock.symbol.tag)] - .SD[follow.trd == 0, uniqueN(stock.symbol.tag)]) %between% c(0, 5)], by = .(cube.symbol, first.date) # 挑选那些同一天的建仓时间follow.trd的仓位相比于non-follow的仓位数量相同的那些交易
    ][, min.period := min(period), by = .(cube.symbol, first.date) # 计算相同建仓时间的仓位最小的持有时间
    ][, .SD[1:unique(min.period)], by = .(cube.symbol, stock.symbol.tag)
    ]

ret.post.follow.trd.cube.date <- position.post.follow.trd.cube.date[follow.trd == 1, .SD
    ][ret.daily > -0.1 & ret.daily < 0.1, .SD
    ][, ret.daily.cube := sum(hold.weight * ret.daily) / sum(hold.weight), by = .(cube.symbol, date) # 属于同一个cube的股票需要根据所持有的比例进行加权加总
    ][, unique(.SD), .SDcols = c("cube.symbol", "date", "ret.daily.cube", "rf", "mkt_rf", "smb", "hml", "umd", "rmw", "cma")
    ][, .(ret.daily.aver = mean(ret.daily.cube), rf, mkt_rf, smb, hml, umd, rmw, cma), by = .(date)
    ][, unique(.SD)]

ret.post.follow.trd.cube.date[, felm(ret.daily.aver - rf ~ mkt_rf + smb + hml + umd + rmw + cma) %>% summary()]

ret.post.non.follow.trd.cube.date <- position.post.follow.trd.cube.date[follow.trd == 0, .SD
    ][ret.daily > -0.1 & ret.daily < 0.1, .SD
    ][, ret.daily.cube := sum(hold.weight * ret.daily) / sum(hold.weight), by = .(cube.symbol, date) # 属于同一个cube的股票需要根据所持有的比例进行加权加总
    ][, unique(.SD), .SDcols = c("cube.symbol", "date", "ret.daily.cube", "rf", "mkt_rf", "smb", "hml", "umd", "rmw", "cma")
    ][, .(ret.daily.aver = mean(ret.daily.cube), rf, mkt_rf, smb, hml, umd, rmw, cma), by = .(date)
    ][, unique(.SD)]

ret.post.non.follow.trd.cube.date[, felm(ret.daily.aver - rf ~ mkt_rf + smb + hml + umd + rmw + cma) %>% summary()]

# 1.5.2 构建非平行时间的portfolio（择时）
# 首先建立所有的pre-follow阶段的持仓
# 再建立所有cube在pre-follow阶段持有与follow.trd相同股票的仓位
# 最后建立同一个cube持有follow.trd相同股票在pre-follow阶段的持仓

# 提取出pre-follow阶段所有的交易，建立portfolio
#position.pre.follow.trd <- f.fifo.position.all[post.follow.full == 0, .SD
    #][cube.symbol %in% unique(position.post.follow.trd$cube.symbol), .SD
    ##][date %in% unique(position.post.follow.trd$date), .SD
    #][order(date), .SD]
#ret.pre.follow.trd <- position.pre.follow.trd[ret.daily > -0.1 & ret.daily < 0.1, .SD
    #][, .(ret.daily.aver = mean(ret.daily), rf, mkt_rf, smb, hml, umd, rmw, cma), by = .(date)
    #][, unique(.SD)]
#ret.pre.follow.trd[, felm(ret.daily.aver - rf ~ mkt_rf + smb + hml + umd + rmw + cma)] %>% summary()

# 提取出在pre-follow阶段持仓与follow.trd阶段持有相同股票的持仓
position.pre.follow.trd.stk <- f.fifo.position.all[post.follow.full == 0, .SD
    ][cube.symbol %in% unique(position.post.follow.trd$cube.symbol), .SD
    ][stock.symbol %in% unique(position.post.follow.trd$stock.symbol), .SD
    ][order(date), .SD]

ret.pre.follow.trd.stk <- position.pre.follow.trd.stk[ret.daily > -0.1 & ret.daily < 0.1, .SD
    ][, .(ret.daily.aver = mean(ret.daily), rf, mkt_rf, smb, hml, umd, rmw, cma), by = .(date)
    ][, unique(.SD)]

ret.pre.follow.trd.stk[, felm(ret.daily.aver - rf ~ mkt_rf + smb + hml + umd + rmw + cma)] %>% summary()

# 同一个cube持有follow.trd相同股票在pre - follow阶段的持仓
# 这里需要产生两个数据集，为了方便对比，stock和cube在follow前后都必须一一对应

## follow之前的持仓
position.pre.follow.trd.stk.cube <- f.fifo.position.all[order(cube.symbol, stock.symbol.tag, date), .SD
    ][, .SD[mean(follow.trd) > 0 & mean(follow.trd) < 1], by = .(cube.symbol, stock.symbol.tag)
    ][, tag := .N, by = .(cube.symbol, stock.symbol, follow.trd) # 统计出pre阶段和follow的不同的存续时间
    ][, tag := min(tag), by = .(cube.symbol, stock.symbol) # 找出最小的那个时间段
    ][order(cube.symbol, stock.symbol, date), .SD
    ][follow.trd == 0, .SD, by = .(cube.symbol, stock.symbol)
    ][order(date), .SD
    ]
ret.pre.follow.trd.stk.cube <- position.pre.follow.trd.stk.cube[ret.daily > -0.1 & ret.daily < 0.1, .SD
    ][, .(ret.daily.aver = mean(ret.daily), rf, mkt_rf, smb, hml, umd, rmw, cma), by = .(date)
    ][, unique(.SD)]
ret.pre.follow.trd.stk.cube[, felm(ret.daily.aver - rf ~ mkt_rf + smb + hml + umd + rmw + cma)] %>% summary()

position.post.follow.trd.stk.cube <- f.fifo.position.all[order(cube.symbol, stock.symbol.tag, date), .SD
    ][, .SD[mean(follow.trd) > 0 & mean(follow.trd) < 1], by = .(cube.symbol, stock.symbol.tag)
    ][, tag := .N, by = .(cube.symbol, stock.symbol, follow.trd) # 统计出pre阶段和follow的不同的存续时间
    ][, tag := min(tag), by = .(cube.symbol, stock.symbol) # 找出最小的那个时间段
    ][order(cube.symbol, stock.symbol, date), .SD
    ][follow.trd == 1, .SD, by = .(cube.symbol, stock.symbol)
    ][order(date), .SD
    ]
ret.post.follow.trd.stk.cube <- position.post.follow.trd.stk.cube[ret.daily > -0.1 & ret.daily < 0.1, .SD
    ][, .(ret.daily.aver = mean(ret.daily), rf, mkt_rf, smb, hml, umd, rmw, cma), by = .(date)
    ][, unique(.SD)]
ret.post.follow.trd.stk.cube[, felm(ret.daily.aver - rf ~ mkt_rf + smb + hml + umd + rmw + cma)] %>% summary()

# 1.6 Newey west t-test ----
# 1.6.1 择股portfolio对比
# ret.post.follow.trd与ret.post.non.follow.trd.cube
lm((ret.post.follow.trd[, ret.daily.aver] - ret.post.non.follow.trd.cube[, ret.daily.aver]) ~ 1) %>% lmtest::coeftest()
# ret.post.follow.trd.cube.date与ret.post.non.follow.trd.cube.date
lm((ret.post.follow.trd.cube.date[, ret.daily.aver] - ret.post.non.follow.trd.cube.date[, ret.daily.aver]) ~ 1) %>% lmtest::coeftest()

# 1.6.2 择时portfolio对比
# ret.post.follow.trd与ret.pre.follow.trd.stk
lm((ret.post.follow.trd[, ret.daily.aver] - ret.pre.follow.trd.stk[, ret.daily.aver]) ~ 1) %>% lmtest::coeftest()
# ret.pre.follow.trd.stk.cube与ret.post.follow.trd.stk.cube
lm((ret.post.follow.trd.stk.cube[, ret.daily.aver] - ret.pre.follow.trd.stk.cube[, ret.daily.aver]) ~ 1) %>% lmtest::coeftest()

# 1.7 画图 ----
# 导入沪深300日度指数
url <- str_c(getwd(), "/data/Index")
index <- fbread(path = url, pattern = "*\\d.txt")
index <- index[, file_id := NULL
    ][Indexcd == "000300"
    ][, ':='(index_ret = Idxtrd08 / 100, index_value = Idxtrd05, date = as.Date(Idxtrd01))
    ][date %in% ret.post.follow.trd[, unique(date)], .SD
    ][, times := 1 + index_ret
    ][, value.daily := cumprod(times)]

# 1.7.1 择股portfolio对比
# ret.post.follow.trd与ret.post.non.follow.trd.cube
ret.post.follow.trd[, times := 1 + ret.daily.aver
    ][, value.daily := cumprod(times)]
ret.post.non.follow.trd.cube[, times := 1 + ret.daily.aver
    ][, value.daily := cumprod(times)]
ggplot() +
    geom_line(ret.post.follow.trd, mapping = aes(x = date, y = value.daily), color = "blue", size = 1) +
    geom_line(ret.post.non.follow.trd.cube, mapping = aes(x = date, y = value.daily), color = "red", size = 1) +
    geom_line(index, mapping = aes(x = date, y = value.daily), color = "black", size = 1)

# ret.post.follow.trd.cube.date与ret.post.non.follow.trd.cube.date
ret.post.follow.trd.cube.date[, times := 1 + ret.daily.aver
    ][, value.daily := cumprod(times)]
ret.post.non.follow.trd.cube.date[, times := 1 + ret.daily.aver
    ][, value.daily := cumprod(times)]

ggplot() +
    geom_line(ret.post.follow.trd.cube.date, mapping = aes(x = date, y = value.daily), color = "blue", size = 1) +
    geom_line(ret.post.non.follow.trd.cube.date, mapping = aes(x = date, y = value.daily), color = "red", size = 1) +
    geom_line(index, mapping = aes(x = date, y = value.daily), color = "black", size = 1)

# 1.7.2 择时portfolio对比
# ret.post.follow.trd与ret.pre.follow.trd.stk
ret.post.follow.trd[, times := 1 + ret.daily.aver
    ][, value.daily := cumprod(times)]
ret.pre.follow.trd.stk[, times := 1 + ret.daily.aver
    ][, value.daily := cumprod(times)]

ggplot() +
    geom_line(ret.post.follow.trd, mapping = aes(x = date, y = value.daily), color = "blue", size = 1) +
    geom_line(ret.pre.follow.trd.stk, mapping = aes(x = date, y = value.daily), color = "red", size = 1) +
    geom_line(index, mapping = aes(x = date, y = value.daily), color = "black", size = 1)

# ret.pre.follow.trd.stk.cube与ret.post.follow.trd.stk.cube
ret.pre.follow.trd.stk.cube[, times := 1 + ret.daily.aver
    ][, value.daily := cumprod(times)]
ret.post.follow.trd.stk.cube[, times := 1 + ret.daily.aver
    ][, value.daily := cumprod(times)]

ggplot() +
    geom_line(ret.post.follow.trd.stk.cube, mapping = aes(x = date, y = value.daily), color = "blue", size = 1) +
    geom_line(ret.pre.follow.trd.stk.cube, mapping = aes(x = date, y = value.daily), color = "red", size = 1) +
    geom_line(index, mapping = aes(x = date, y = value.daily), color = "black", size = 1)