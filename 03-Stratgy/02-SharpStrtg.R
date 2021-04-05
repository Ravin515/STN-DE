library(styleer)

#  计算sharpe strategy的收益率，没有交易成本 ----
ld(f.cube.ret.sp)
f.cube.ret.sp <- f.cube.ret.sp[, date.month := as.character(date) %>% str_sub(start = 1L, end = 7L) %>% str_replace_all("-", "")
    ][order(cube.symbol, date, date.month), .SD
    ][, ret.daily := value / shift(value, type = "lag") - 1, by = .(cube.symbol)
    ]

# Sharpe Ratio Strategy 1：每个月月初计算前三个月sharpe ratio最大的五个股票，并且至少在下个月都不关闭 ----
# 计算每三个月的shape ratio
date.month <- f.cube.ret.sp[, .(date.month = unique(date.month))
    ][!(date.month %in% c("201604", "201807")), .SD
    ][order(date.month), tag := 1:.N]

f.cube.ret.sp <- date.month[f.cube.ret.sp[!(date.month %in% c("201604", "201807")), .SD
    ], on = .(date.month), nomatch = 0
    ][, tag.ret := fifelse(ret.daily >= 0.1 | ret.daily <= -0.1, 1, 0)
    ][, tag.ret := sum(tag.ret, na.rm = T), by = .(cube.symbol)
    ][tag.ret == 0, .SD] # 筛选那些ret正常的cube

# 计算至少存在两个月的cube的Sharpe Ratio
sharp.ratio <- f.cube.ret.sp[, tag2 := max(tag) - min(tag), by = .(cube.symbol)
    ][tag2 >= 3, .SD
    ][!is.na(ret.daily), .SD
    ][, {
        l <- list()
        for (i in min(tag):(max(tag) - 2)) {
            sq <- .SD[tag %between% c(i, i + 2)]
            l[[i]] <- sq[, .(sharp.ratio = (mean(ret.daily, na.rm = T) - 0.000045) / sd(ret.daily), tag = i + 3), by = .(cube.symbol)]
        }
        rbindlist(l, fill = T)
    }]

# 计算每个cube每三个月的ret观察数
obv <- f.cube.ret.sp[, tag2 := max(tag) - min(tag), by = .(cube.symbol)
    ][tag2 >= 3, .SD
    ][!is.na(ret.daily), .SD
    ][, {
        l <- list()
        for (i in min(tag):(max(tag) - 2)) {
            sq <- .SD[tag %between% c(i, i + 2)]
            l[[i]] <- sq[, .(obv = .N, tag = i + 2), by = .(cube.symbol)]
        }
        rbindlist(l, fill = T)
    }]
sharp.ratio <- obv[sharp.ratio, on = .(cube.symbol, tag)]

sharp.ratio <- sharp.ratio[order(tag, - obv, - sharp.ratio), .SD
    ][, .SD[1:10], by = .(tag)
    ][order(tag, - sharp.ratio), .SD
    ]

# 挑选出期间Sharpe Ratio最大的5个cube
sharp.ratio.top5 <- date.month[sharp.ratio, on = .(tag)
    ][order(date.month, - sharp.ratio), .SD[1:5], by = .(date.month)
    ][!is.na(date.month), .SD
    ][, ':='(tag = NULL, obv = NULL)
    ][, date.month.lag := shift(date.month, type = "lag", n = 5L)] # 为之后计算交易成本和日度数据每个月月初的收益率做准备

# 算出每个月选中的组合在上个月最后一日的value
ss.cube.lag <- sharp.ratio.top5[, .(cube.symbol, date.month.lag = fifelse(is.na(date.month.lag), as.character(as.numeric(date.month.lag[6]) - 1), date.month.lag))]
ss.value.cube.lag <- f.cube.ret.sp[date.month %in% ss.cube.lag$date.month & cube.symbol %in% ss.cube.lag$cube.symbol, .SD[.N], .SDcols = c('value', 'date'), by = .(cube.symbol, date.month)
    ][sharp.ratio.top5[, .(date.month = fifelse(is.na(date.month.lag), as.character(as.numeric(date.month.lag[6]) - 1), date.month.lag), cube.symbol)], on = .(cube.symbol, date.month)
    ][, .(date.month.lag = date.month), by = .(cube.symbol, date, value)
    ][, date.month := shift(date.month.lag, type = "lead", n = 5L)
    ][is.na(date.month), date.month := "201806"
    ][, .(value.cube = mean(value), date), by = .(date.month)
    ][, unique(.SD)]

# 竖着合并成一个整体
ss.1 <- sharp.ratio.top5[f.cube.ret.sp, on = .(cube.symbol, date.month)
    ][!is.na(sharp.ratio), .SD
    ][, .(value.cube = mean(value), date.month), by = .(date)
    ][, unique(.SD)
    ][, rbindlist(list(.SD, ss.value.cube.lag), fill = T, use.names = T)
    ][order(date.month, date), .SD]


# 1.1 Newey West t-test
# 导入指数文件
# 日度指数文件
url <- str_c(getwd(), "/data/Index")
index <- fbread(path = url, pattern = "*\\d.txt")
index <- index[, file_id := NULL
    ][Indexcd == "000300"
    ][, ':='(index_ret = Idxtrd08 / 100, index_value = Idxtrd05, date = as.Date(Idxtrd01))]

# 月度指数文件
indexmn <- fread(str_c(url, "/IDX_Idxtrdmth.txt"), encoding = "UTF-8")
indexmn <- indexmn[Indexcd == "000300"]

library(lmtest)
# 日度Newey West t-test
ss.1.nw.daily <- index[, .(index_ret, date)
    ][ss.1, on = .(date), nomatch = 0
    ][, unique(.SD)
    ][, ret.cube := value.cube / shift(value.cube, type = "lag") - 1, by = .(date.month)]
ss.1.nw.daily[, lm(ret.cube - index_ret ~ 1) %>% coeftest()]

# 月度Newey West t-test
ss.1.nw.monthly <- indexmn[, .(index_ret = Idxrtn, date.month = str_replace_all(Month, "-", ""))
    ][ss.1, on = .(date.month), nomatch = 0
    ][, unique(.SD)
    ][, ret.cube := value.cube[.N] / value.cube[1] - 1, by = .(date.month)
    ][, unique(.SD), .SDcols = c("ret.cube", "date.month", "index_ret")]
ss.1.nw.monthly[, lm(ret.cube - index_ret ~ 1) %>% coeftest()]

# 1.2 Alpha回归
# 导入日度因子
url <- str_c(getwd(), "/data/Factors/")
fivefactor_daily <- fread(str_c(url, "fivefactor_daily.csv"), encoding = "UTF-8")

# 导入月度因子
url <- str_c(getwd(), "/data/Factors/")
fivefactor_monthly <- fread(str_c(url, "fivefactor_monthly.csv"), encoding = "UTF-8")
fivefactor_monthly <- fivefactor_monthly[, setnames(.SD, "trdmn", "date.month")
    ][, date.month := as.character(date.month)]


# 日度回归
ss.1.ret.daily <- fivefactor_daily[, setnames(.SD, "trddy", "date")
    ][, date := as.Date(date)
    ][ss.1, on = .(date)
    ][, unique(.SD), .SDcols = c("value.cube", "date", "date.month", "mkt_rf", "smb", "hml", "umd", "rmw", "cma", "rf")
    ][order(date), .SD
    ][, ret.cube := value.cube/shift(value.cube, type = "lag") - 1, by = .(date.month)]

ss.1.ret.daily[, lm(ret.cube - rf ~ mkt_rf + smb + hml)] %>% summary()
ss.1.ret.daily[, lm(ret.cube - rf ~ mkt_rf + smb + hml + umd)] %>% summary()
ss.1.ret.daily[, lm(ret.cube - rf ~ mkt_rf + smb + hml + rmw + cma)] %>% summary()
ss.1.ret.daily[, lm(ret.cube - rf ~ mkt_rf + smb + hml + umd + rmw + cma)] %>% summary()

# 月度回归
ss.1.ret.monthly <- fivefactor_monthly[ss.1, on = .(date.month)
    ][order(date.month, date), .SD
    ][, ret.cube := value.cube[.N] / value.cube[1] - 1, by = .(date.month)
    ][, unique(.SD), .SDcols = c("ret.cube", "date.month", "mkt_rf", "smb", "hml", "umd", "rmw", "cma", "rf")]

ss.1.ret.monthly[, lm(ret.cube - rf ~ mkt_rf + smb + hml)] %>% summary()
ss.1.ret.monthly[, lm(ret.cube - rf ~ mkt_rf + smb + hml + umd)] %>% summary()
ss.1.ret.monthly[, lm(ret.cube - rf ~ mkt_rf + smb + hml + rmw + cma)] %>% summary()
ss.1.ret.monthly[, lm(ret.cube - rf ~ mkt_rf + smb + hml + umd + rmw + cma)] %>% summary()

# Sharpe Ratio Strategy 2：计算交易成本的 Sharpe Ratio Strategy ----
## 交易成本的计算

### 交易成本 （因为雪球数据每天的净值包含了交易的手续费，只需计算每月换手的交易成本）

cost.lag <- f.cube.ret.sp[date.month %in% ss.cube.lag$date.month & cube.symbol %in% ss.cube.lag$cube.symbol, .SD[.N], .SDcols = c('value', 'date'), by = .(cube.symbol, date.month)
    ][sharp.ratio.top5[, .(date.month =  fifelse(is.na(date.month.lag), as.character(as.numeric(date.month.lag[6]) - 1), date.month.lag), cube.symbol)], on = .(cube.symbol, date.month)
    ][, .(date.month.lag = date.month), by = .(cube.symbol, date, value)
    ][, date.month := shift(date.month.lag, type = "lead", n = 5L)
    ][is.na(date.month), date.month := "201806"
    ][, date.month.lag := NULL]

cost <- sharp.ratio.top5[, .(cube.symbol, date.month)
    ][f.cube.ret.sp, on = .(cube.symbol, date.month), nomatch = 0
    ][, .SD[c(1, .N)], by = .(cube.symbol, date.month)
    ][order(date, cube.symbol), .SD
    ][, tag := rep(1:2, times = 5), by = .(date.month)
    ][, ':='(label = NULL, tag = NULL, tag.ret = NULL, ret.daily = NULL, tag2 = NULL)]

ss.cost <- rbindlist(list(cost, cost.lag), use.names = T)

ss.cost <- ss.cost[order(date.month, cube.symbol, date), .SD
    ][, ret.per.cube := value[.N] / value[1] - 1, by = .(cube.symbol, date.month)
    ][order(date.month, date, cube.symbol), .SD
    ][, value.all.cube := mean(value), by = .(date.month, date)
    #][, ret.all.cube := .SD[date == max(date), unique(value.all.cube)] - .SD[date == min(date), unique(value.all.cube)], by = .(date.month)
    ][, ret.all.cube := mean(ret.per.cube), by = .(date.month)
    ][, .SD[(.N - 4):.N], by = .(date.month)
    ][, abs.ret.between.all.per := abs(ret.per.cube - ret.all.cube)
    ][, ret.plus.all.per := 2 + ret.per.cube + ret.all.cube]

cube.symbol.list <- ss.cost[, .(cube.symbol.list = list(unique(cube.symbol))), by = .(date.month)
    ][, cube.symbol.intersect := {
        a <- list()
        for (i in 2:.N) {
            a[[i]] <- intersect(cube.symbol.list[[i]], cube.symbol.list[[i - 1]])
        }
        a
    }][, cube.symbol.diff := {
        a <- list()
        for (i in 1:.N) {
            if (i == 1) {
                a[[i]] <- cube.symbol.list[[i]]
            } else {
                a[[i]] <- setdiff(cube.symbol.list[[i]], cube.symbol.intersect[[i]])
            }
        }
        a
    }]

ss.cost <- cube.symbol.list[ss.cost, on = .(date.month)]

ss.cost[, ev := {
    p1 <- cube.symbol.intersect %>% unlist() %>% unique()
    p2 <- cube.symbol.diff %>% unlist() %>% unique()
    ev.p1 <- .SD[cube.symbol %in% p1, sum(abs.ret.between.all.per)]
    ev.p2 <- .SD[cube.symbol %in% p2, sum(ret.plus.all.per)]
    n <- 5
    a <- (ev.p1 + ev.p2) * (n * (1 + unique(ret.all.cube))) ^ (-1)
    a
}, by = .(date.month)
][, cost := 0.0025 * (1 + ret.all.cube) * ev]

# 将cost带入monthly的数据中
ss.2.ret.monthly <- ss.cost[, unique(.SD), .SDcols = c('cost', 'date.month')
    ][ss.1.ret.monthly, on = .(date.month)
    ]
ss.2.nw.monthly <- ss.cost[, unique(.SD), .SDcols = c('cost', 'date.month')
    ][ss.1.nw.monthly, on = .(date.month)
    ]

# 将cost带入daily的数据中
ss.2.ret.daily <- ss.cost[, unique(.SD), .SDcols = c('cost', 'date.month')
    ][ss.1.ret.daily, on = .(date.month)
    ]
ss.2.nw.daily <- ss.cost[, unique(.SD), .SDcols = c('cost', 'date.month')
    ][ss.1.nw.daily, on = .(date.month)
    ]

# 2.1 Newey West t-test
ss.2.nw.daily[, lm(ret.cube - cost - index_ret ~ 1) %>% coeftest()]
ss.2.nw.monthly[, lm(ret.cube - cost - index_ret ~ 1) %>% coeftest()]

# 2.2 Alpha 回归
ss.2.ret.daily[, lm(ret.cube - cost - rf ~ mkt_rf + smb + hml)] %>% summary()
ss.2.ret.daily[, lm(ret.cube - cost - rf ~ mkt_rf + smb + hml + umd)] %>% summary()
ss.2.ret.daily[, lm(ret.cube - cost - rf ~ mkt_rf + smb + hml + rmw + cma)] %>% summary()
ss.2.ret.daily[, lm(ret.cube - cost - rf ~ mkt_rf + smb + hml + umd + rmw + cma)] %>% summary()

ss.2.ret.monthly[, lm(I(ret.cube - cost - rf) ~ mkt_rf + smb + hml)] %>% summary()
ss.2.ret.monthly[, lm(I(ret.cube - cost - rf) ~ mkt_rf + smb + hml + umd)] %>% summary()
ss.2.ret.monthly[, lm(I(ret.cube - cost - rf) ~ mkt_rf + smb + hml + rmw + cma)] %>% summary()
ss.2.ret.monthly[, lm(I(ret.cube - cost - rf) ~ mkt_rf + smb + hml + umd + rmw + cma)] %>% summary()

