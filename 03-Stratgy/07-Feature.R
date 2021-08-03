# 探究那些赚钱的cube到底有一些什么样的特点
library(styleer)

# 对样本中累积收益率最高的那些cube进行分析 ----
ld(f.cube.ret.sp)
outlier <- f.cube.ret.sp[, ret := value / shift(value, type = "lag") - 1, by = cube.symbol
    ][ret < -0.1 | ret > 0.1, unique(cube.symbol)]


# 导入指数文件----
# 日度指数文件

url <- str_c(getwd(), "/data/Index")
index <- fbread(path = url, pattern = "*\\d.txt")
index <- index[, file_id := NULL
    ][Indexcd == "000300"
    ][, ':='(index_ret = Idxtrd08 / 100, index_value = Idxtrd05, date = as.Date(Idxtrd01))]

# 月度指数文件
indexmn <- fread(str_c(url, "/IDX_Idxtrdmth.txt"), encoding = "UTF-8")
indexmn <- indexmn[Indexcd == "000300"]

# 导入各类因子----
url <- str_c(getwd(), "/data/Factors/")
file.names <- list.files(path = url, pattern = "*.txt|.csv")
for (i in file.names) {
    assign(str_sub(i, start = 1L, end = -5L), fread(str_c(url, toupper(i))))
}

# 对四个策略中选出的cube进行分析----
ld(f.cube.ret.sp, force = T)
f.cube.ret.sp <- f.cube.ret.sp[, date.month := as.character(date) %>% str_sub(start = 1L, end = 7L) %>% str_replace_all("-", "")
    ][order(cube.symbol, date, date.month), .SD]

# 1. 风险-收益率策略
rs <- index[, .(date, index_value, index_ret)
    ][f.cube.ret.sp[, .(ret = value / shift(value, type = "lag") - 1, value, date, date.month), by = .(cube.symbol)
    ], on = .(date)
    ][, tag := fifelse(ret >= 0.1 | ret <= -0.1, 1, 0)
    ][, tag := sum(tag, na.rm = T), by = .(cube.symbol)
    ][, ret.num.monthly := .N, by = .(cube.symbol, date.month)
    ][!(date.month %in% c("201606", "201604", "201807")) & tag == 0, .SD
    ][ret.num.monthly >= 10, .SD
    ][order(cube.symbol, date), .SD
    ][, alpha := coef(lm(ret ~ index_ret))[1], by = .(cube.symbol, date.month)
    ][, beta := coef(lm(ret ~ index_ret))[2], by = .(cube.symbol, date.month)
    ][, ret.abnr := ret - alpha - beta * index_ret
    ][, cum.abnr.ret := sum(ret.abnr, na.rm = T), by = .(cube.symbol, date.month)
    ][, risk := sd(ret.abnr), by = .(cube.symbol, date.month)
    ][, unique(.SD), .SDcols = c("cube.symbol", "date.month", "risk", "ret.num.monthly", "cum.abnr.ret")
    ][order(date.month, - ret.num.monthly, - risk, - cum.abnr.ret), .SD
    ][risk > 0, .SD
    ][, .SD[1:50], by = .(date.month)
    ][, date.month := shift(date.month, type = "lead", n = 50)
    ][!is.na(date.month), .SD]

# 2. Naive 策略
ns <- f.cube.ret.sp[, .(ret.ns.1 = value / value[1] - 1, ret = value / shift(value, type = "lag") - 1, value, date, date.month), by = .(cube.symbol)
    ][, tag := fifelse(ret >= 0.1 | ret <= -0.1, 1, 0)
    ][, tag := sum(tag, na.rm = T), by = .(cube.symbol)
    ][, ret.num.monthly := .N, by = .(cube.symbol, date.month)
    ][!(date.month %in% c("201606", "201604", "201807")) & tag == 0, .SD[1], by = .(cube.symbol, date.month)
    ][order(date.month, - ret.num.monthly, - ret.ns.1), .SD
    ][, .SD[1:50], by = .(date.month)
    ][, date.month.lag := shift(date.month, type = "lag", n = 50L)
    ][, date := NULL]

# 3. Sharpe Ratio策略
date.month <- f.cube.ret.sp[, ret.daily := value / shift(value, type = "lag") - 1, by = .(cube.symbol)
    ][, .(date.month = unique(date.month))
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
    ][, .SD[1:50], by = .(tag)
    ][order(tag, - sharp.ratio), .SD
    ]

# 挑选出期间Sharpe Ratio最大的50个cube
ss <- date.month[sharp.ratio, on = .(tag)
    ][order(date.month, - sharp.ratio), .SD[1:50], by = .(date.month)
    ][!is.na(date.month), .SD
    ][, ':='(tag = NULL, obv = NULL)
    ][, date.month.lag := shift(date.month, type = "lag", n = 50L)]

# 4. 收益率滚动排行策略
outlier <- f.cube.ret.sp[order(cube.symbol, date), .SD
    ][, ret.daily := value / shift(value, type = "lag") - 1, by = .(cube.symbol)
    ][ret.daily > 0.1 | ret.daily < -0.1, unique(cube.symbol)]
f.cube.top50 <- f.cube.ret.sp[order(cube.symbol, date), .SD
    ][!(cube.symbol %in% outlier), .SD
    ][, date.month := str_sub(date, start = 1L, end = 7L)
    ][, ret.monthly := value[.N] / value[1] - 1, by = .(cube.symbol, date.month)
    ][order(date.month, - ret.monthly), .SD
    ][, unique(.SD), .SDcols = c("cube.symbol", "date.month", "ret.monthly")
    ][, .SD[1:50], by = .(date.month)
    ][date.month != "2018-07", .SD]

# 每个月所需要购买的cube的列表
ts <- f.cube.top50[, date.month.lag := shift(date.month, type = "lead", n = 50)
    ][, .(cube.symbol, date.month.lag)
    ][!is.na(date.month.lag), .SD
    ][, setnames(.SD, "date.month.lag", "date.month")
    ][, date.month := str_replace_all(date.month, "-", "")]


# 0.1 首先看挑选出来的这些cube在每个月的list----
# 0.1.1 控制变量的计算 ----
## 存续时间active.day：从一创建portfolio开始一直到portfolio结束时间点的时间（天）
## 交易频率trd.num：最近30天的交易次数的对数
## 发帖数量cmt.num：最近30天的发帖数量的对数
## 股票持有量stock.num：当天的股票持有量 （已通过stock.list进行计算）
## 市场的情况mmt：因子umd（直接在之后的factor中加入）

## 利用调仓的数据计算每天的交易数量
ld(f.cube.rb.sp)
cube.trd.num <- f.cube.rb.sp[target.weight != prev.weight.adjusted & (!is.na(target.weight) | !is.na(prev.weight.adjusted)), .SD
    ][order(cube.symbol, created.at), .SD, .SDcols = c("cube.symbol", "created.at", "stock.symbol")
    ][, date := as.Date(created.at)
    ][, .(trd.num = .N), by = .(cube.symbol, date)
    ][, trd.num := fifelse(is.na(trd.num), 0, trd.num)]

## 利用f.user.cmt.num计算每天的发帖数量
ld(f.user.cmt.num)
cube.cmt.num <- f.user.cmt.num

## 利用收益率数据计算存续时间
ld(f.cube.ret.sp)
cube.active.day <- f.cube.ret.sp[, .(active.day = difftime(date, min(date), unit = "day"), date), by = .(cube.symbol)]

## 利用stock.info数据计算股票持有情况
ld(stock.info)
cube.stock.num <- stock.info[, stock.num := lapply(stock.list, length) %>% unlist()
    ][, .(cube.symbol, date, stock.num)]

## 合并四张表
ctrl.var <- cube.trd.num[cube.cmt.num, on = .(cube.symbol, date)
    ][cube.active.day, on = .(cube.symbol, date)
    ][cube.stock.num, on = .(cube.symbol, date)
    ][, trd.num := fifelse(is.na(trd.num), 0, trd.num)
    ][, cmt.num := fifelse(is.na(cmt.num), 0, cmt.num)
    ][, trd.num.30day := frollsum(trd.num, 30)
    ][, stock.num.30day := frollmean(stock.num, 30)
    ][, cmt.num.30day := frollsum(cmt.num, 30)
    ]

# 策略中每个月cube的重合度
overlap <- rs[, .(cube.symbol.list = list(cube.symbol)), by = date.month
    ][, overlap.1.month := {
        a <- vector()
        for (i in 2:.N) {
            a[i] <- intersect(cube.symbol.list[[i - 1]], cube.symbol.list[[i]]) %>% length()
        }
        a
    }][, overlap.3.month := {
        a <- vector()
        for (i in 3:.N) {
            a[i] <- intersect(cube.symbol.list[[i - 2]], cube.symbol.list[[i]]) %>% length()
        }
        a
    }][, overlap.6.month := {
        a <- vector()
        for (i in 6:.N) {
            a[i] <- intersect(cube.symbol.list[[i - 5]], cube.symbol.list[[i]]) %>% length()
        }
        a
    }][, overlap.12.month := {
        a <- vector()
        for (i in 12:.N) {
            a[i] <- intersect(cube.symbol.list[[i - 11]], cube.symbol.list[[i]]) %>% length()
        }
        a
    }][, overlap.18.month := {
        a <- vector()
        for (i in 18:.N) {
            a[i] <- intersect(cube.symbol.list[[i - 17]], cube.symbol.list[[i]]) %>% length()
        }
        a
    }][, .SD, .SDcols = -c("cube.symbol.list")]
fwrite(overlap, "overlap.csv")


# 0.2 将这些选出来的cube被选中的时间标记为窗口期----
# 上个月为0，这个月为1，下个月为2以此进行回归，看系数变化
# 并与导入的f.cube.ret.sp进行合并
window.cube.ns <- ns[, unique(.SD), .SDcols = c("date.month", "cube.symbol")
    ][, window := 1]
window.cube.ss <- ss[, unique(.SD), .SDcols = c("date.month", "cube.symbol")
    ][, window := 1]
window.cube.ts <- ts[, unique(.SD), .SDcols = c("date.month", "cube.symbol")
    ][, window := 1]
window.cube.rs <- rs[, unique(.SD), .SDcols = c("date.month", "cube.symbol")
    ][, window := 1]
# 窗口期之前一个月的数据
window.lag.cube.ns <- ns[, .(date.month = shift(date.month, type = "lag", n = 50), cube.symbol)
    ][, date.month := fifelse(is.na(date.month), "201607", date.month)
    ][, window := 0]
window.lag.cube.ss <- ss[, .(date.month = shift(date.month, type = "lag", n = 50), cube.symbol)
    ][, date.month := fifelse(is.na(date.month), "201607", date.month)
    ][, window := 0]
window.lag.cube.ts <- ts[, .(date.month = shift(date.month, type = "lag", n = 50), cube.symbol)
    ][, date.month := fifelse(is.na(date.month), "201607", date.month)
    ][, window := 0]
window.lag.cube.rs <- rs[, .(date.month = shift(date.month, type = "lag", n = 50), cube.symbol)
    ][, date.month := fifelse(is.na(date.month), "201607", date.month)
    ][, window := 0]

# 窗口期之后一个月的数据
window.lead.cube.ns <- ns[, .(date.month = shift(date.month, type = "lead", n = 50), cube.symbol)
    ][, date.month := fifelse(is.na(date.month), "201806", date.month)
    ][, window := 0]
window.lead.cube.ss <- ss[, .(date.month = shift(date.month, type = "lead", n = 50), cube.symbol)
    ][, date.month := fifelse(is.na(date.month), "201806", date.month)
    ][, window := 0]
window.lead.cube.ts <- ts[, .(date.month = shift(date.month, type = "lead", n = 50), cube.symbol)
    ][, date.month := fifelse(is.na(date.month), "201806", date.month)
    ][, window := 0]
window.lead.cube.rs <- rs[, .(date.month = shift(date.month, type = "lead", n = 50), cube.symbol)
    ][, date.month := fifelse(is.na(date.month), "201806", date.month)
    ][, window := 0]

# 合并三个部分
window.period.ns <- window.cube.ns[f.cube.ret.sp[cube.symbol %in% window.cube.ns$cube.symbol, .SD], on = .(cube.symbol, date.month)
    ][order(cube.symbol, date), .SD
    ][, ret.daily := value / shift(value, type = "lag") - 1, by = .(cube.symbol)
    ][window == 1, .SD
    ]
window.lag.period.ns <- window.lag.cube.ns[f.cube.ret.sp[cube.symbol %in% window.lag.cube.ns$cube.symbol, .SD], on = .(cube.symbol, date.month)
    ][order(cube.symbol, date), .SD
    ][, ret.daily := value / shift(value, type = "lag") - 1, by = .(cube.symbol)
    ][window == 0, .SD
    ]
window.lead.period.ns <- window.lead.cube.ns[f.cube.ret.sp[cube.symbol %in% window.lead.cube.ns$cube.symbol, .SD], on = .(cube.symbol, date.month)
    ][order(cube.symbol, date), .SD
    ][, ret.daily := value / shift(value, type = "lag") - 1, by = .(cube.symbol)
    ][window == 0, .SD
    ]

window.period.ss <- window.cube.ss[f.cube.ret.sp[cube.symbol %in% window.cube.ss$cube.symbol, .SD], on = .(cube.symbol, date.month)
    ][order(cube.symbol, date), .SD
    ][, ret.daily := value / shift(value, type = "lag") - 1, by = .(cube.symbol)
    ][window == 1, .SD
    ]
window.lag.period.ss <- window.lag.cube.ss[f.cube.ret.sp[cube.symbol %in% window.lag.cube.ss$cube.symbol, .SD], on = .(cube.symbol, date.month)
    ][order(cube.symbol, date), .SD
    ][, ret.daily := value / shift(value, type = "lag") - 1, by = .(cube.symbol)
    ][window == 0, .SD
    ]
window.lead.period.ss <- window.lead.cube.ss[f.cube.ret.sp[cube.symbol %in% window.lead.cube.ss$cube.symbol, .SD], on = .(cube.symbol, date.month)
    ][order(cube.symbol, date), .SD
    ][, ret.daily := value / shift(value, type = "lag") - 1, by = .(cube.symbol)
    ][window == 0, .SD
    ]

window.period.ts <- window.cube.ts[f.cube.ret.sp[cube.symbol %in% window.cube.ts$cube.symbol, .SD], on = .(cube.symbol, date.month)
    ][order(cube.symbol, date), .SD
    ][, ret.daily := value / shift(value, type = "lag") - 1, by = .(cube.symbol)
    ][window == 1, .SD
    ]
window.lag.period.ts <- window.lag.cube.ts[f.cube.ret.sp[cube.symbol %in% window.lag.cube.ts$cube.symbol, .SD], on = .(cube.symbol, date.month)
    ][order(cube.symbol, date), .SD
    ][, ret.daily := value / shift(value, type = "lag") - 1, by = .(cube.symbol)
    ][window == 0, .SD
    ]
window.lead.period.ts <- window.lead.cube.ts[f.cube.ret.sp[cube.symbol %in% window.lead.cube.ts$cube.symbol, .SD], on = .(cube.symbol, date.month)
    ][order(cube.symbol, date), .SD
    ][, ret.daily := value / shift(value, type = "lag") - 1, by = .(cube.symbol)
    ][window == 0, .SD
    ]

window.period.rs <- window.cube.rs[f.cube.ret.sp[cube.symbol %in% window.cube.rs$cube.symbol, .SD], on = .(cube.symbol, date.month)
    ][order(cube.symbol, date), .SD
    ][, ret.daily := value / shift(value, type = "lag") - 1, by = .(cube.symbol)
    ][window == 1, .SD
    ]
window.lag.period.rs <- window.lag.cube.rs[f.cube.ret.sp[cube.symbol %in% window.lag.cube.rs$cube.symbol, .SD], on = .(cube.symbol, date.month)
    ][order(cube.symbol, date), .SD
    ][, ret.daily := value / shift(value, type = "lag") - 1, by = .(cube.symbol)
    ][window == 0, .SD
    ]
window.lead.period.rs <- window.lead.cube.rs[f.cube.ret.sp[cube.symbol %in% window.lead.cube.rs$cube.symbol, .SD], on = .(cube.symbol, date.month)
    ][order(cube.symbol, date), .SD
    ][, ret.daily := value / shift(value, type = "lag") - 1, by = .(cube.symbol)
    ][window == 0, .SD
    ]

# 两个数据集与index合并
# ns策略
event.lead.period.ns <- rbindlist(list(window.lead.period.ns, window.period.ns), use.names = T)
event.lead.period.ns <- index[, .(index_value, date, index_ret)
    ][event.lead.period.ns, on = .(date)
    ][order(cube.symbol, date.month, date), .SD
    ][, date.qrtr := str_c(year(date), quarter(date))
    ][, ex.ret.daily := ret.daily - index_ret
    ][, ':='(ret.monthly = value[.N] / value[1] - 1, index.ret.monthly = index_value[.N]/index_value[1] - 1), by = .(cube.symbol, date.month, window)
    ][, ex.ret := ret.monthly - index.ret.monthly
    ][, vol := sd(ex.ret.daily) * .N ^ (1 / 2), by = .(cube.symbol, date.month, window)
    #][, unique(.SD), .SDcols = c("cube.symbol", "window", "date", "ex.ret.daily", "vol", "ret.daily", "date.qrtr", "date.month")
    ][, unique(.SD), .SDcols = c("cube.symbol", "window", "date", "ex.ret", "vol", "ret.monthly", "date.qrtr", "date.month", "ex.ret.daily")
    ][date.month != "201806"
    ][, tag := fifelse(window == 1, 1, 2)
    ]
    
event.lag.period.ns <- rbindlist(list(window.lag.period.ns, window.period.ns), use.names = T)
event.lag.period.ns <- index[, .(index_value, date, index_ret)
    ][event.lag.period.ns, on = .(date)
    ][order(cube.symbol, date.month, date), .SD
    ][, date.qrtr := str_c(year(date), quarter(date))
    ][, ex.ret.daily := ret.daily - index_ret
    ][, ':='(ret.monthly = value[.N] / value[1] - 1, index.ret.monthly = index_value[.N] / index_value[1] - 1), by = .(cube.symbol, date.month, window)
    ][, ex.ret := ret.monthly - index.ret.monthly
    ][, vol := sd(ex.ret.daily)*.N^(1/2), by = .(cube.symbol, date.month, window)
    #][, unique(.SD), .SDcols = c("cube.symbol", "window", "date", "ex.ret.daily", "vol", "ret.daily", "date.qrtr", "date.month")
    ][, unique(.SD), .SDcols = c("cube.symbol", "window", "date", "ex.ret", "vol", "ret.monthly", "date.qrtr", "date.month", "ex.ret.daily")
    ][date.month != "201806"
    ][, tag := fifelse(window == 1, 1, 0)
    ]

event.ns <- rbindlist(list(event.lag.period.ns, event.lead.period.ns), use.names = T) %>% unique()
event.period.monthly.ns <- ctrl.var[event.ns, on = .(cube.symbol, date)
    ][, .SD[.N], by = .(cube.symbol, date.month)]
event.period.daily.ns <- ctrl.var[event.ns, on = .(cube.symbol, date)]

# ss策略
event.lead.period.ss <- rbindlist(list(window.lead.period.ss, window.period.ss), use.names = T)
event.lead.period.ss <- index[, .(index_value, date, index_ret)
    ][event.lead.period.ss, on = .(date)
    ][order(cube.symbol, date.month, date), .SD
    ][, date.qrtr := str_c(year(date), quarter(date))
    ][, ex.ret.daily := ret.daily - index_ret
    ][, ':='(ret.monthly = value[.N] / value[1] - 1, index.ret.monthly = index_value[.N] / index_value[1] - 1), by = .(cube.symbol, date.month, window)
    ][, ex.ret := ret.monthly - index.ret.monthly
    ][, vol := sd(ex.ret.daily) * .N ^ (1 / 2), by = .(cube.symbol, date.month, window)
    #][, unique(.SD), .SDcols = c("cube.symbol", "window", "date", "ex.ret.daily", "vol", "ret.daily", "date.qrtr", "date.month")
    ][, unique(.SD), .SDcols = c("cube.symbol", "window", "date", "ex.ret", "vol", "ret.monthly", "date.qrtr", "date.month", "ex.ret.daily")
    ][date.month != "201806"
    ][, tag := fifelse(window == 1, 1, 2)
    ]

event.lag.period.ss <- rbindlist(list(window.lag.period.ss, window.period.ss), use.names = T)
event.lag.period.ss <- index[, .(index_value, date, index_ret)
    ][event.lag.period.ss, on = .(date)
    ][order(cube.symbol, date.month, date), .SD
    ][, date.qrtr := str_c(year(date), quarter(date))
    ][, ex.ret.daily := ret.daily - index_ret
    ][, ':='(ret.monthly = value[.N] / value[1] - 1, index.ret.monthly = index_value[.N] / index_value[1] - 1), by = .(cube.symbol, date.month, window)
    ][, ex.ret := ret.monthly - index.ret.monthly
    ][, vol := sd(ex.ret.daily) * .N ^ (1 / 2), by = .(cube.symbol, date.month, window)
    #][, unique(.SD), .SDcols = c("cube.symbol", "window", "date", "ex.ret.daily", "vol", "ret.daily", "date.qrtr", "date.month")
    ][, unique(.SD), .SDcols = c("cube.symbol", "window", "date", "ex.ret", "vol", "ret.monthly", "date.qrtr", "date.month", "ex.ret.daily")
    ][date.month != "201806"
    ][, tag := fifelse(window == 1, 1, 0)
    ]

event.ss <- rbindlist(list(event.lag.period.ss, event.lead.period.ss), use.names = T) %>% unique()
event.period.monthly.ss <- ctrl.var[event.ss, on = .(cube.symbol, date)
    ][, .SD[.N], by = .(cube.symbol, date.month)]
event.period.daily.ss <- ctrl.var[event.ss, on = .(cube.symbol, date)]

# ts策略
event.lead.period.ts <- rbindlist(list(window.lead.period.ts, window.period.ts), use.names = T)
event.lead.period.ts <- index[, .(index_value, date, index_ret)
    ][event.lead.period.ts, on = .(date)
    ][order(cube.symbol, date.month, date), .SD
    ][, date.qrtr := str_c(year(date), quarter(date))
    ][, ex.ret.daily := ret.daily - index_ret
    ][, ':='(ret.monthly = value[.N] / value[1] - 1, index.ret.monthly = index_value[.N] / index_value[1] - 1), by = .(cube.symbol, date.month, window)
    ][, ex.ret := ret.monthly - index.ret.monthly
    ][, vol := sd(ex.ret.daily) * .N ^ (1 / 2), by = .(cube.symbol, date.month, window)
    #][, unique(.SD), .SDcols = c("cube.symbol", "window", "date", "ex.ret.daily", "vol", "ret.daily", "date.qrtr", "date.month")
    ][, unique(.SD), .SDcols = c("cube.symbol", "window", "date", "ex.ret", "vol", "ret.monthly", "date.qrtr", "date.month", "ex.ret.daily")
    ][date.month != "201806"
    ][, tag := fifelse(window == 1, 1, 2)
    ]

event.lag.period.ts <- rbindlist(list(window.lag.period.ts, window.period.ts), use.names = T)
event.lag.period.ts <- index[, .(index_value, date, index_ret)
    ][event.lag.period.ts, on = .(date)
    ][order(cube.symbol, date.month, date), .SD
    ][, date.qrtr := str_c(year(date), quarter(date))
    ][, ex.ret.daily := ret.daily - index_ret
    ][, ':='(ret.monthly = value[.N] / value[1] - 1, index.ret.monthly = index_value[.N] / index_value[1] - 1), by = .(cube.symbol, date.month, window)
    ][, ex.ret := ret.monthly - index.ret.monthly
    ][, vol := sd(ex.ret.daily) * .N ^ (1 / 2), by = .(cube.symbol, date.month, window)
    #][, unique(.SD), .SDcols = c("cube.symbol", "window", "date", "ex.ret.daily", "vol", "ret.daily", "date.qrtr", "date.month")
    ][, unique(.SD), .SDcols = c("cube.symbol", "window", "date", "ex.ret", "vol", "ret.monthly", "date.qrtr", "date.month", "ex.ret.daily")
    ][date.month != "201806"
    ][, tag := fifelse(window == 1, 1, 0)
    ]

event.ts <- rbindlist(list(event.lag.period.ts, event.lead.period.ts), use.names = T) %>% unique()
event.period.monthly.ts <- ctrl.var[event.ts, on = .(cube.symbol, date)
    ][, .SD[.N], by = .(cube.symbol, date.month)]
event.period.daily.ts <- ctrl.var[event.ts, on = .(cube.symbol, date)]

# rs策略
event.lead.period.rs <- rbindlist(list(window.lead.period.rs, window.period.rs), use.names = T)
event.lead.period.rs <- index[, .(index_value, date, index_ret)
    ][event.lead.period.rs, on = .(date)
    ][order(cube.symbol, date.month, date), .SD
    ][, date.qrtr := str_c(year(date), quarter(date))
    ][, ex.ret.daily := ret.daily - index_ret
    ][, ':='(ret.monthly = value[.N] / value[1] - 1, index.ret.monthly = index_value[.N] / index_value[1] - 1), by = .(cube.symbol, date.month, window)
    ][, ex.ret := ret.monthly - index.ret.monthly
    ][, vol := sd(ex.ret.daily) * .N ^ (1 / 2), by = .(cube.symbol, date.month, window)
    #][, unique(.SD), .SDcols = c("cube.symbol", "window", "date", "ex.ret.daily", "vol", "ret.daily", "date.qrtr", "date.month")
    ][, unique(.SD), .SDcols = c("cube.symbol", "window", "date", "ex.ret", "vol", "ret.monthly", "date.qrtr", "date.month", "ex.ret.daily")
    ][date.month != "201806"
    ][, tag := fifelse(window == 1, 1, 2)
    ]

event.lag.period.rs <- rbindlist(list(window.lag.period.ts, window.period.rs), use.names = T)
event.lag.period.rs <- index[, .(index_value, date, index_ret)
    ][event.lag.period.rs, on = .(date)
    ][order(cube.symbol, date.month, date), .SD
    ][, date.qrtr := str_c(year(date), quarter(date))
    ][, ex.ret.daily := ret.daily - index_ret
    ][, ':='(ret.monthly = value[.N] / value[1] - 1, index.ret.monthly = index_value[.N] / index_value[1] - 1), by = .(cube.symbol, date.month, window)
    ][, ex.ret := ret.monthly - index.ret.monthly
    ][, vol := sd(ex.ret.daily) * .N ^ (1 / 2), by = .(cube.symbol, date.month, window)
    #][, unique(.SD), .SDcols = c("cube.symbol", "window", "date", "ex.ret.daily", "vol", "ret.daily", "date.qrtr", "date.month")
    ][, unique(.SD), .SDcols = c("cube.symbol", "window", "date", "ex.ret", "vol", "ret.monthly", "date.qrtr", "date.month", "ex.ret.daily")
    ][date.month != "201806"
    ][, tag := fifelse(window == 1, 1, 0)
    ]

event.rs <- rbindlist(list(event.lag.period.rs, event.lead.period.rs), use.names = T) %>% unique()
event.period.monthly.rs <- ctrl.var[event.rs, on = .(cube.symbol, date)
    ][, .SD[.N], by = .(cube.symbol, date.month)]
event.period.daily.rs <- ctrl.var[event.rs, on = .(cube.symbol, date)]

library(lfe)
r1 <- event.period.monthly.ns[, felm(ex.ret ~ I(as.factor(tag)) | cube.symbol + date.qrtr)]
r2 <- event.period.monthly.ns[, felm(ex.ret ~ I(as.factor(tag)) + active.day | cube.symbol + date.qrtr)]
r3 <- event.period.monthly.ns[, felm(ex.ret ~ I(as.factor(tag)) + active.day + cmt.num.30day + trd.num.30day + stock.num.30day | cube.symbol + date.qrtr)]
r4 <- event.period.monthly.ss[, felm(ex.ret ~ I(as.factor(tag)) | cube.symbol + date.qrtr)]
r5 <- event.period.monthly.ss[, felm(ex.ret ~ I(as.factor(tag)) + active.day | cube.symbol + date.qrtr)]
r6 <- event.period.monthly.ss[, felm(ex.ret ~ I(as.factor(tag)) + active.day + cmt.num.30day + trd.num.30day + stock.num.30day | cube.symbol + date.qrtr)]
r7 <- event.period.monthly.ts[, felm(ex.ret ~ I(as.factor(tag)) | cube.symbol + date.qrtr)]
r8 <- event.period.monthly.ts[, felm(ex.ret ~ I(as.factor(tag)) + active.day | cube.symbol + date.qrtr)]
r9 <- event.period.monthly.ts[, felm(ex.ret ~ I(as.factor(tag)) + active.day + cmt.num.30day + trd.num.30day + stock.num.30day | cube.symbol + date.qrtr)]
r10 <- event.period.monthly.rs[, felm(ex.ret ~ I(as.factor(tag)) | cube.symbol + date.qrtr)]
r11 <- event.period.monthly.rs[, felm(ex.ret ~ I(as.factor(tag)) + active.day | cube.symbol + date.qrtr)]
r12 <- event.period.monthly.rs[, felm(ex.ret ~ I(as.factor(tag)) + active.day + cmt.num.30day + trd.num.30day + stock.num.30day | cube.symbol + date.qrtr)]


library(texreg)
list(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12) %>%
    htmlreg(
        file = "stage.monthly.html",
        custom.header = list("Naive策略" = 1:3, "Sharpe Ratio策略" = 4:6, "收益率滚动排行策略" = 7:9, "风险-收益率策略" = 10:12),
#caption.above = TRUE,
        include.rs = TRUE,
        include.adjrs = FALSE,
        custom.model.names = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)", "(9)", "(10)", "(11)", "(12)"),
        custom.gof.rows = list(
        #"Control Variables" = c("No", "Yes", "No", "Yes", "No", "Yes"),
        "Trader FE" = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
        "Quarter FE" = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")
        ),
       custom.coef.names = c("window = 1", "window = 2", "active.day", "cmt.num.30day", "trd.num.30day", "stock.num.30day"),
#omit.coef = "active.day|cmt.num.30day|trd.num.30day|stock.num.30day",
#reorder.coef = c(1:2, 10:13, 3:9),
        caption.above = TRUE,
        digits = 4,
        inline.css = FALSE,
        doctype = TRUE,
        html.tag = TRUE,
        head.tag = TRUE,
        body.tag = TRUE,
        center = FALSE,
        stars = c(0.001, 0.01, 0.05, 0.1)
            )

r1 <- event.period.daily.ns[, felm(ex.ret.daily ~ as.factor(tag) | cube.symbol + date.qrtr)]
r2 <- event.period.daily.ns[, felm(ex.ret.daily ~ as.factor(tag) + active.day + cmt.num + trd.num + stock.num | cube.symbol + date.qrtr)]
r3 <- event.period.daily.ns[, felm(ex.ret.daily ~ as.factor(tag) + active.day + cmt.num.30day + trd.num.30day + stock.num.30day | cube.symbol + date.qrtr)]
r4 <- event.period.daily.ss[, felm(ex.ret.daily ~ as.factor(tag) | cube.symbol + date.qrtr)]
r5 <- event.period.daily.ss[, felm(ex.ret.daily ~ as.factor(tag) + active.day + cmt.num + trd.num + stock.num | cube.symbol + date.qrtr)]
r6 <- event.period.daily.ss[, felm(ex.ret.daily ~ as.factor(tag) + active.day + cmt.num.30day + trd.num.30day + stock.num.30day | cube.symbol + date.qrtr)]
r7 <- event.period.daily.ts[, felm(ex.ret.daily ~ as.factor(tag) | cube.symbol + date.qrtr)]
r8 <- event.period.daily.ts[, felm(ex.ret.daily ~ as.factor(tag) + active.day + cmt.num + trd.num + stock.num | cube.symbol + date.qrtr)]
r9 <- event.period.daily.ts[, felm(ex.ret.daily ~ as.factor(tag) + active.day + cmt.num.30day + trd.num.30day + stock.num.30day | cube.symbol + date.qrtr)]
r10 <- event.period.daily.rs[, felm(ex.ret.daily ~ as.factor(tag) | cube.symbol + date.qrtr)]
r11 <- event.period.daily.rs[, felm(ex.ret.daily ~ as.factor(tag) + active.day + cmt.num + trd.num + stock.num | cube.symbol + date.qrtr)]
r12 <- event.period.daily.rs[, felm(ex.ret.daily ~ as.factor(tag) + active.day + cmt.num.30day + trd.num.30day + stock.num.30day | cube.symbol + date.qrtr)]


library(texreg)
list(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12) %>%
    htmlreg(
        file = "stage.daily.html",
        custom.header = list("Naive策略" = 1:3, "Sharpe Ratio策略" = 4:6, "收益率滚动排行策略" = 7:9, "风险-收益率策略" = 10:12),
#caption.above = TRUE,
        include.rs = TRUE,
        include.adjrs = FALSE,
        custom.model.names = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)", "(9)", "(10)", "(11)", "(12)"),
        custom.gof.rows = list(
#"Control Variables" = c("No", "Yes", "No", "Yes", "No", "Yes"),
        "Trader FE" = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
        "Quarter FE" = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")
        ),
        custom.coef.names = c("window = 1", "window = 2",  "active.day", "cmt.num", "trd.num", "stock.num", "cmt.num.30day", "trd.num.30day", "stock.num.30day"),
#omit.coef = "active.day|cmt.num.30day|trd.num.30day|stock.num.30day",
#reorder.coef = c(1:2, 10:13, 3:9),
        caption.above = TRUE,
        digits = 4,
        inline.css = FALSE,
        doctype = TRUE,
        html.tag = TRUE,
        head.tag = TRUE,
        body.tag = TRUE,
        center = FALSE,
        stars = c(0.001, 0.01, 0.05, 0.1)
            )


# 1.1 看持有股票和择时的特点 ----
# 导入调仓数据，匹配portfolio中每个cube在那个时间段的调仓
ld(stock.info, force = T)
stock.info <- stock.info[, date.month := str_sub(date, start = 1L, end = 7L)
    ][, date.month := str_replace_all(date.month, "-", "") 
    ][, .(stock.symbol = unlist(stock.list)), by = .(cube.symbol, date, date.month)]

# 每天持有股票展开的列表
f.cube.stock.ns <- ns[stock.info, on = .(date.month, cube.symbol), nomatch = 0
    ][order(date.month, date, stock.symbol), .SD
    ][, stock.symbol := str_sub(stock.symbol, start = 3L, end = 8L)]

f.cube.stock.ss <- ss[stock.info, on = .(date.month, cube.symbol), nomatch = 0
    ][order(date.month, date, stock.symbol), .SD
    ][, stock.symbol := str_sub(stock.symbol, start = 3L, end = 8L)]

f.cube.stock.ts <- ts[stock.info, on = .(date.month, cube.symbol), nomatch = 0
    ][order(date.month, date, stock.symbol), .SD
    ][, stock.symbol := str_sub(stock.symbol, start = 3L, end = 8L)]

f.cube.stock.rs <- rs[stock.info, on = .(date.month, cube.symbol), nomatch = 0
    ][order(date.month, date, stock.symbol), .SD
    ][, stock.symbol := str_sub(stock.symbol, start = 3L, end = 8L)]

## 1.1.1 行业与市场分布
## 导入公司股票信息文件
#url <- str_c(getwd(), "/data/Stockinfo/")
#f.stock.list <- f.cube.stock[, unique(stock.symbol)]
#firm.info <- fread(str_c(url, "TRD_Co.txt"), encoding = "UTF-8")
#f.firm.info <- firm.info[, stock.symbol := str_pad(Stkcd, 6, "left", pad = "0")
    #][stock.symbol %in% f.stock.list, .SD]
#f.firm.info[, Markettype := fcase(Markettype == 4, "深证A", Markettype == 1, "上证A", Markettype == 16, "创业板")]

## 市场分布特点
#f.firm.info[, .N, by = Markettype]

## 行业分布特点
#f.firm.info[, .N, by = Nindnme
    #][order(-N), .SD]

# 1.1.2 市盈率与市净率指标
# 导入股票各类指标
url <- str_c(getwd(), "/data/Stockindex")
stock.index <- fbread(path = url, pattern = "*.txt", encoding = "UTF-8")
stock.index <- stock.index[, setnames(.SD, c("TradingDate", "Symbol"), c("date", "stock.symbol"))
    ][, stock.symbol := str_pad(stock.symbol, 6, "left", pad = "0")
    ][, unique(.SD)
    ][order(file_id, stock.symbol, date), .SD
    ][, ':='(PE.60day = frollmean(PE, 60, na.rm = T), PB.60day = frollmean(PB, 60, na.rm = T), PCF.60day = frollmean(PCF, 60, na.rm = T), PS.60day = frollmean(PS, 60, na.rm = T)), by = .(stock.symbol)]


#ggplot() +
#geom_line(data = f.cube.pe.pb[, unique(.SD), .SDcols = c("date", "pe.aver.stock")], mapping = aes(x = date, y = pe.aver.stock), color = "blue", size = 1, linetype = 1) +
#geom_line(data = f.cube.pe.pb[, unique(.SD), .SDcols = c("date", "pe.med.stock")], mapping = aes(x = date, y = pe.med.stock), color = "blue", size = 1, linetype = 2)
#ggplot() +
#geom_line(data = f.cube.pe.pb[, unique(.SD), .SDcols = c("date", "pb.aver.stock")], mapping = aes(x = date, y = pb.aver.stock), color = "red", size = 1, linetype = 1) +
#geom_line(data = f.cube.pe.pb[, unique(.SD), .SDcols = c("date", "pb.med.stock")], mapping = aes(x = date, y = pb.med.stock), color = "red", size = 1, linetype = 2) +
#geom_line(data = index[date %in% c(as.Date("2016-08-01"), as.Date("2018-06-30"))], mapping = aes(x = date, y = index_ret), color = "black", size = 1, linetype = 1)

# 进行PSM匹配----
# 为择股的对应样本做准备
# 导入PSM变量
url <- str_c(getwd(), "/data/PSM/")
file.names <- list.files(path = url, pattern = "*.txt|.csv")
for (i in file.names) {
    assign(str_sub(i, start = 1L, end = -5L), fread(str_c(url, toupper(i)), encoding = "UTF-8"))
}
# 高管持股比例
EN_EquityNatureAll <- EN_EquityNatureAll[, setnames(.SD, c("Symbol", "EndDate", "TopTenHoldersRate"), c("stock.symbol", "date", "top10holder"))
    ][date %between% c(as.Date("2011-01-01"), as.Date("2018-12-31")), .SD
    ][, stock.symbol := str_pad(stock.symbol, 6, "left", pad = "0")
    ][, date.year := str_sub(date, start = 1L, end = 4L)
    ][order(stock.symbol, date), .SD
    ][, .SD[.N], by = .(stock.symbol, date.year)
    ][, .(stock.symbol, date.year, top10holder)]

# 资产负债率
FI_T1 <- FI_T1[, setnames(.SD, c("Stkcd", "Accper", "F011201A"), c("stock.symbol", "date", "debt.asset"))
    ][date %between% c(as.Date("2011-01-01"), as.Date("2018-12-31")), .SD
    ][, stock.symbol := str_pad(stock.symbol, 6, "left", pad = "0")
    ][, date.year := str_sub(date, start = 1L, end = 4L)
    ][Typrep == "A", .SD
    ][order(stock.symbol, date), .SD
    ][, .SD[.N], by = .(stock.symbol, date.year)
    ][, .(stock.symbol, date.year, debt.asset)]

# 托宾q及行业代码
FI_T10 <- FI_T10[, setnames(.SD, c("Stkcd", "Accper", "F100901A", "Indcd"), c("stock.symbol", "date", "Tobinq", "ind.symbol"))
    ][date %between% c(as.Date("2011-01-01"), as.Date("2018-12-31")), .SD
    ][, stock.symbol := str_pad(stock.symbol, 6, "left", pad = "0")
    ][, date.year := str_sub(date, start = 1L, end = 4L)
    ][order(stock.symbol, date), .SD
    ][, .SD[.N], by = .(stock.symbol, date.year)
    ][, .(stock.symbol, date.year, Tobinq, ind.symbol)]

# 总资产
FS_Combas <- FS_Combas[, setnames(.SD, c("Stkcd", "Accper", "A001000000"), c("stock.symbol", "date", "total.asset"))
    ][date %between% c(as.Date("2011-01-01"), as.Date("2018-12-31")), .SD
    ][, stock.symbol := str_pad(stock.symbol, 6, "left", pad = "0")
    ][, date.year := str_sub(date, start = 1L, end = 4L)
    ][Typrep == "A", .SD
    ][order(stock.symbol, date), .SD
    ][, .SD[.N], by = .(stock.symbol, date.year)
    ][, .(stock.symbol, date.year, total.asset)]

# 将上述的数据集全部横向合并、
f.psm <- Reduce(merge, list(EN_EquityNatureAll, FI_T1, FI_T10, FS_Combas))
f.psm <- f.psm[, str_c(c("top10holder", "debt.asset", "Tobinq", "total.asset"), ".aver") := frollapply(.SD, mean, na.rm = T, n = 5), .SDcols = c("top10holder", "debt.asset", "Tobinq", "total.asset"), by = .(stock.symbol)
    ][, unique(.SD), .SDcols = c('stock.symbol', 'date.year', 'top10holder.aver', 'debt.asset.aver', 'Tobinq.aver', 'total.asset.aver', 'ind.symbol')
    ][!is.na(top10holder.aver), .SD]

# 把所有股票和所有月份进行匹配
# 标记出在当月为event的股票
# 导入收盘价
clsprc <- fbread(path = "./data/Clprc", pattern = "*.txt")
clsprc <- clsprc[, .(stock.symbol = str_pad(Stkcd, 6, side = "left", pad = "0"), date = as.Date(Trddt), Clsprc)]
f.full.sample <- CJ(stock.symbol = unique(clsprc$stock.symbol), date.month = unique(f.cube.stock$date.month))
f.full.sample <- f.cube.stock[, event := 1
    ][, unique(.SD), .SDcols = c("date.month", "stock.symbol", "event")
    ][f.full.sample[, date.year := str_sub(date.month, start = 1L, end = 4L)], on = .(stock.symbol, date.month)
    ][, event := fifelse(is.na(event), 0, event)
    ][, date.year := as.character(as.numeric(date.year) - 1)
    ]
f.psm.final <- f.psm[f.full.sample, on = .(stock.symbol, date.year)
    ][!is.na(top10holder.aver), .SD]

library(MatchIt)
psm.out <- f.psm.final[, .(result = list(matchit(event ~ top10holder.aver + debt.asset.aver + Tobinq.aver + ind.symbol + total.asset.aver, data = .SD, method = "nearest", ratio = 3) %>% match.data())), by = .(date.month)
    ][, rbindlist(result), by = .(date.month)
    ][order(date.month, subclass), .SD]


# 1.2 流动性指标----
# 导入股票每日的收盘价和每日成交量
clsp.amnt <- fbread(path = "./data/Clprc", pattern = "*.txt")
clsp.amnt <- clsp.amnt[, .(stock.symbol = str_pad(Stkcd, 6, side = "left", pad = "0"), date = as.Date(Trddt), Clsprc, Dnshrtrd)]
# 生成每天流动性指标的表
# 1.2.1 择时 
# 四大策略
f.cube.pe.pb.time.ns <- f.cube.stock.ns[, event := 1
    ][, unique(.SD), .SDcols = -c("cube.symbol", "ret.ns.1", "ret", "value", "tag", "ret.num.monthly", "date.month.lag")
    ][stock.index, on = .(stock.symbol, date)
    ][order(date, stock.symbol), .SD
    ][, ':='(event = fifelse(is.na(event), 0, event), date.month = NULL)
    ][order(stock.symbol, date), .SD
    ][, .SD[sum(event == 1) > 0], by = .(stock.symbol)
    ][, date.month := str_sub(date, start = 1L, end = 7L)
    ][, date.month := str_replace_all(date.month, "-", "")
    ][, date.qrtr := str_c(year(date), quarter(date))]

f.cube.pe.pb.time.ss <- f.cube.stock.ss[, event := 1
    ][, unique(.SD), .SDcols = -c("cube.symbol", "date.month.lag", "sharp.ratio")
    ][stock.index, on = .(stock.symbol, date)
    ][order(date, stock.symbol), .SD
    ][, ':='(event = fifelse(is.na(event), 0, event), date.month = NULL)
    ][order(stock.symbol, date), .SD
    ][, .SD[sum(event == 1) > 0], by = .(stock.symbol)
    ][, date.month := str_sub(date, start = 1L, end = 7L)
    ][, date.month := str_replace_all(date.month, "-", "")
    ][, date.qrtr := str_c(year(date), quarter(date))]

f.cube.pe.pb.time.ts <- f.cube.stock.ts[, event := 1
    ][, unique(.SD), .SDcols = -c("cube.symbol")
    ][stock.index, on = .(stock.symbol, date)
    ][order(date, stock.symbol), .SD
    ][, ':='(event = fifelse(is.na(event), 0, event), date.month = NULL)
    ][order(stock.symbol, date), .SD
    ][, .SD[sum(event == 1) > 0], by = .(stock.symbol)
    ][, date.month := str_sub(date, start = 1L, end = 7L)
    ][, date.month := str_replace_all(date.month, "-", "")
    ][, date.qrtr := str_c(year(date), quarter(date))]

f.cube.pe.pb.time.rs <- f.cube.stock.rs[, event := 1
    ][, unique(.SD), .SDcols = -c("cube.symbol", "risk", "ret.num.monthly", "cum.abnr.ret")
    ][stock.index, on = .(stock.symbol, date)
    ][order(date, stock.symbol), .SD
    ][, ':='(event = fifelse(is.na(event), 0, event), date.month = NULL)
    ][order(stock.symbol, date), .SD
    ][, .SD[sum(event == 1) > 0], by = .(stock.symbol)
    ][, date.month := str_sub(date, start = 1L, end = 7L)
    ][, date.month := str_replace_all(date.month, "-", "")
    ][, date.qrtr := str_c(year(date), quarter(date))]


# 与收盘价和每日成交量合并
f.cube.pe.pb.time.ns <- clsp.amnt[f.cube.pe.pb.time.ns, on = .(stock.symbol, date), nomatch = 0]
f.cube.pe.pb.time.ss <- clsp.amnt[f.cube.pe.pb.time.ss, on = .(stock.symbol, date), nomatch = 0]
f.cube.pe.pb.time.ts <- clsp.amnt[f.cube.pe.pb.time.ts, on = .(stock.symbol, date), nomatch = 0]
f.cube.pe.pb.time.rs<- clsp.amnt[f.cube.pe.pb.time.rs, on = .(stock.symbol, date), nomatch = 0]
# 与市场因子umd合并
f.cube.pe.pb.time.ns <- fivefactor_daily[, .(umd, date = trddy)
    ][f.cube.pe.pb.time.ns, on = .(date)]
f.cube.pe.pb.time.ss <- fivefactor_daily[, .(umd, date = trddy)
    ][f.cube.pe.pb.time.ss, on = .(date)]
f.cube.pe.pb.time.ts <- fivefactor_daily[, .(umd, date = trddy)
    ][f.cube.pe.pb.time.ts, on = .(date)]
f.cube.pe.pb.time.rs <- fivefactor_daily[, .(umd, date = trddy)
    ][f.cube.pe.pb.time.rs, on = .(date)]

#library(lfe)
#f.cube.pe.pb.time.ns[, felm(Liquidility ~ event | stock.symbol + date.qrtr)] %>% summary()
#f.cube.pe.pb.time.ns[, felm(Liquidility ~ event + Clsprc + PE + PB + umd | stock.symbol + date.qrtr)] %>% summary()
#f.cube.pe.pb.time.ss[, felm(Liquidility ~ event | stock.symbol + date.qrtr)] %>% summary()
#f.cube.pe.pb.time.ss[, felm(Liquidility ~ event + Clsprc + PE + PB + umd | stock.symbol + date.qrtr)] %>% summary()
#f.cube.pe.pb.time.ts[, felm(Liquidility ~ event | stock.symbol + date.qrtr)] %>% summary()
#f.cube.pe.pb.time.ts[, felm(Liquidility ~ event + Clsprc + PE + PB + umd | stock.symbol + date.qrtr)] %>% summary()
#f.cube.pe.pb.time.rs[, felm(Liquidility ~ event | stock.symbol + date.qrtr)] %>% summary()
#f.cube.pe.pb.time.rs[, felm(Liquidility ~ event + Clsprc + PE + PB + umd | stock.symbol + date.qrtr)] %>% summary()



# 1.2.2 择股
f.cube.pe.pb.stock.ns <- f.cube.stock.ns[, event := 1
    ][, unique(.SD), .SDcols = -c("cube.symbol", "ret.ns.1", "ret", "value", "tag", "ret.num.monthly", "date.month.lag")
    ][stock.index, on = .(stock.symbol, date)
    ][date %in% f.cube.stock.ns$date, .SD
    ][, event := fifelse(is.na(event), 0, event)
    ][, date.month := str_sub(date, start = 1L, end = 7L)
    ][, date.month := str_replace_all(date.month, "-", "")
    ][, date.qrtr := str_c(year(date), quarter(date))]

f.cube.pe.pb.stock.ss <- f.cube.stock.ss[, event := 1
    ][, unique(.SD), .SDcols = -c("cube.symbol", "date.month.lag", "sharp.ratio")
    ][stock.index, on = .(stock.symbol, date)
    ][date %in% f.cube.stock.ss$date, .SD
    ][, event := fifelse(is.na(event), 0, event)
    ][, date.month := str_sub(date, start = 1L, end = 7L)
    ][, date.month := str_replace_all(date.month, "-", "")
    ][, date.qrtr := str_c(year(date), quarter(date))]

f.cube.pe.pb.stock.ts <- f.cube.stock.ts[, event := 1
    ][, unique(.SD), .SDcols = -c("cube.symbol")
    ][stock.index, on = .(stock.symbol, date)
    ][date %in% f.cube.stock.ts$date, .SD
    ][, event := fifelse(is.na(event), 0, event)
    ][, date.month := str_sub(date, start = 1L, end = 7L)
    ][, date.month := str_replace_all(date.month, "-", "")
    ][, date.qrtr := str_c(year(date), quarter(date))]

f.cube.pe.pb.stock.rs <- f.cube.stock.rs[, event := 1
    ][, unique(.SD), .SDcols = -c("cube.symbol", "risk", "ret.num.monthly", "cum.abnr.ret")
    ][stock.index, on = .(stock.symbol, date)
    ][date %in% f.cube.stock.rs$date, .SD
    ][, event := fifelse(is.na(event), 0, event)
    ][, date.month := str_sub(date, start = 1L, end = 7L)
    ][, date.month := str_replace_all(date.month, "-", "")
    ][, date.qrtr := str_c(year(date), quarter(date))]


# 与收盘价和每日成交量合并
f.cube.pe.pb.stock.ns <- clsp.amnt[f.cube.pe.pb.stock.ns, on = .(stock.symbol, date), nomatch = 0]
f.cube.pe.pb.stock.ss <- clsp.amnt[f.cube.pe.pb.stock.ss, on = .(stock.symbol, date), nomatch = 0]
f.cube.pe.pb.stock.ts <- clsp.amnt[f.cube.pe.pb.stock.ts, on = .(stock.symbol, date), nomatch = 0]
f.cube.pe.pb.stock.rs <- clsp.amnt[f.cube.pe.pb.stock.rs, on = .(stock.symbol, date), nomatch = 0]

# 与市场因子umd合并
f.cube.pe.pb.stock.ns <- fivefactor_daily[, .(umd, date = trddy)
    ][f.cube.pe.pb.stock.ns, on = .(date)]
f.cube.pe.pb.stock.ss <- fivefactor_daily[, .(umd, date = trddy)
    ][f.cube.pe.pb.stock.ss, on = .(date)]
f.cube.pe.pb.stock.ts <- fivefactor_daily[, .(umd, date = trddy)
    ][f.cube.pe.pb.stock.ts, on = .(date)]
f.cube.pe.pb.stock.rs <- fivefactor_daily[, .(umd, date = trddy)
    ][f.cube.pe.pb.stock.rs, on = .(date)]

#f.cube.pe.pb.stock.ns[, felm(Liquidility ~ event | stock.symbol + date.qrtr)] %>% summary()
#f.cube.pe.pb.stock.ns[, felm(Liquidility ~ event + Clsprc + I(Amount / 10000) + PE + PB + umd | stock.symbol + date.qrtr)] %>% summary()
#f.cube.pe.pb.stock.ss[, felm(Liquidility ~ event | stock.symbol + date.qrtr)] %>% summary()
#f.cube.pe.pb.stock.ss[, felm(Liquidility ~ event + Clsprc + I(Amount / 10000) + PE + PB + umd | stock.symbol + date.qrtr)] %>% summary()
#f.cube.pe.pb.stock.ts[, felm(Liquidility ~ event | stock.symbol + date.qrtr)] %>% summary()
#f.cube.pe.pb.stock.ts[, felm(Liquidility ~ event + Clsprc + I(Amount / 10000) + PE + PB + umd | stock.symbol + date.qrtr)] %>% summary()
#f.cube.pe.pb.stock.rs[, felm(Liquidility ~ event | stock.symbol + date.qrtr)] %>% summary()
#f.cube.pe.pb.stock.rs[, felm(Liquidility ~ event + Clsprc + I(Amount / 10000) + PE + PB + umd | stock.symbol + date.qrtr)] %>% summary()

## 代入PSM样本
#f.cube.pe.pb.stock.psm <- psm.out[, .(stock.symbol, date.month)
    #][f.cube.pe.pb.stock, on = .(stock.symbol, date.month), nomatch = 0]

#f.cube.pe.pb.stock.psm[, felm(Liquidility ~ event | stock.symbol + date.qrtr)] %>% summary()
#f.cube.pe.pb.stock.psm[, felm(Liquidility ~ event + Clsprc + I(Amount / 10000) | stock.symbol + date.qrtr)] %>% summary()
#f.cube.pe.pb.stock.psm[, felm(Liquidility ~ event + PE + PB | stock.symbol + date.qrtr)] %>% summary()
#f.cube.pe.pb.stock.psm[, felm(Liquidility ~ event + umd | stock.symbol + date.qrtr)] %>% summary()
#f.cube.pe.pb.stock.psm[, felm(Liquidility ~ event + Clsprc + I(Amount / 10000) + PE + PB + umd | stock.symbol + date.qrtr)] %>% summary()

# 1.3 看动量的特点
library(lfe)

#f.cube.time <- f.cube.stock[, event := 1
    #][, unique(.SD), .SDcols = -c("cube.symbol", "risk", "ret.num.monthly", "cum.abnr.ret")
    #][clsp.amnt, on = .(stock.symbol, date)
    #][, ':='(event = fifelse(is.na(event), 0, event), date.month = NULL)
    #][order(stock.symbol, date), .SD
    #][, .SD[sum(event == 1) > 0], by = .(stock.symbol)]

# 1.3.1 择时能力
f.cube.time.sl.ns <- f.cube.pe.pb.time.ns[, .SD, .SDcol = -c("Clsprc")
    ][clsp.amnt, on = .(stock.symbol, date)
    ][, ':='(event = fifelse(is.na(event), 0, event))
    ][order(stock.symbol, date), .SD
    ][, .SD[sum(event == 1) > 0], by = .(stock.symbol)
    ][, mmt := {
    a <- vector()
    for (i in 60:.N) {
        a[i] <- Clsprc[i] / Clsprc[i - 59] * 100 - 100
    }
    a
}, by = .(stock.symbol)
    ][, date.month := str_sub(date, start = 1L, end = 7L)
    ][, date.month := str_replace_all(date.month, "-", "")
    ][, date.qrtr := str_c(year(date), quarter(date))
    ][, mmt.lag := shift(mmt, type = "lag"), by = .(stock.symbol)]

f.cube.time.sl.ss <- f.cube.pe.pb.time.ss[, .SD, .SDcol = -c("Clsprc")
    ][clsp.amnt, on = .(stock.symbol, date)
    ][, ':='(event = fifelse(is.na(event), 0, event))
    ][order(stock.symbol, date), .SD
    ][, .SD[sum(event == 1) > 0], by = .(stock.symbol)
    ][, mmt := {
        a <- vector()
        for (i in 60:.N) {
            a[i] <- Clsprc[i] / Clsprc[i - 59] * 100 - 100
        }
        a
    }, by = .(stock.symbol)
    ][, date.month := str_sub(date, start = 1L, end = 7L)
    ][, date.month := str_replace_all(date.month, "-", "")
    ][, date.qrtr := str_c(year(date), quarter(date))
    ][, mmt.lag := shift(mmt, type = "lag"), by = .(stock.symbol)]

f.cube.time.sl.ts <- f.cube.pe.pb.time.ts[, .SD, .SDcol = -c("Clsprc")
    ][clsp.amnt, on = .(stock.symbol, date)
    ][, ':='(event = fifelse(is.na(event), 0, event))
    ][order(stock.symbol, date), .SD
    ][, .SD[sum(event == 1) > 0], by = .(stock.symbol)
    ][, mmt := {
        a <- vector()
        for (i in 60:.N) {
            a[i] <- Clsprc[i] / Clsprc[i - 59] * 100 - 100
        }
        a
    }, by = .(stock.symbol)
    ][, date.month := str_sub(date, start = 1L, end = 7L)
    ][, date.month := str_replace_all(date.month, "-", "")
    ][, date.qrtr := str_c(year(date), quarter(date))
    ][, mmt.lag := shift(mmt, type = "lag"), by = .(stock.symbol)]

f.cube.time.sl.rs <- f.cube.pe.pb.time.rs[, .SD, .SDcol = -c("Clsprc")
    ][clsp.amnt, on = .(stock.symbol, date)
    ][, ':='(event = fifelse(is.na(event), 0, event))
    ][order(stock.symbol, date), .SD
    ][, .SD[sum(event == 1) > 0], by = .(stock.symbol)
    ][, mmt := {
        a <- vector()
        for (i in 60:.N) {
            a[i] <- Clsprc[i] / Clsprc[i - 59] * 100 - 100
        }
        a
    }, by = .(stock.symbol)
    ][, date.month := str_sub(date, start = 1L, end = 7L)
    ][, date.month := str_replace_all(date.month, "-", "")
    ][, date.qrtr := str_c(year(date), quarter(date))
    ][, mmt.lag := shift(mmt, type = "lag"), by = .(stock.symbol)]

r1 <- f.cube.time.sl.ns[, felm(mmt ~ event | stock.symbol + date.qrtr)] 
r2 <- f.cube.time.sl.ns[, felm(mmt ~ event + mmt.lag + PE + PB  + PS + umd | stock.symbol + date.qrtr)] #%>% summary()
r3 <- f.cube.time.sl.ss[, felm(mmt ~ event + mmt.lag + PE.60day + PB.60day + PS.60day + umd | stock.symbol + date.qrtr)] #%>% summary()
r4 <- f.cube.time.sl.ss[, felm(mmt ~ event | stock.symbol + date.qrtr)]
r5 <- f.cube.time.sl.ss[, felm(mmt ~ event + mmt.lag + PE + PB + PS + umd | stock.symbol + date.qrtr)] #%>% summary()
r6 <- f.cube.time.sl.ss[, felm(mmt ~ event + mmt.lag + PE.60day + PB.60day + PS.60day + umd | stock.symbol + date.qrtr)] #%>% summary()
r7 <- f.cube.time.sl.ts[, felm(mmt ~ event | stock.symbol + date.qrtr)]
r8 <- f.cube.time.sl.ts[, felm(mmt ~ event + mmt.lag + PE + PB + PS + umd | stock.symbol + date.qrtr)] #%>% summary()
r9 <- f.cube.time.sl.ts[, felm(mmt ~ event + mmt.lag + PE.60day + PB.60day + PS.60day + umd | stock.symbol + date.qrtr)] #%>% summary()
r10 <- f.cube.time.sl.rs[, felm(mmt ~ event | stock.symbol + date.qrtr)]
r11 <- f.cube.time.sl.rs[, felm(mmt ~ event + mmt.lag + PE + PB + PS + umd | stock.symbol + date.qrtr)] #%>% summary()
r12 <- f.cube.time.sl.rs[, felm(mmt ~ event + mmt.lag + PE.60day + PB.60day + PS.60day + umd | stock.symbol + date.qrtr)] #%>% summary()

library(texreg)
list(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12) %>%
    htmlreg(
        file = "mmt.time.html",
        custom.header = list("Naive策略" = 1:3, "Sharpe Ratio策略" = 4:6, "收益率滚动排行策略" = 7:9, "风险-收益率策略" = 10:12),
#caption.above = TRUE,
        include.rs = TRUE,
        include.adjrs = FALSE,
        custom.model.names = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)", "(9)", "(10)", "(11)", "(12)"),
        custom.gof.rows = list(
#"Control Variables" = c("No", "Yes", "No", "Yes", "No", "Yes"),
        "Stock FE" = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
        "Quarter FE" = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")
        ),
       custom.coef.names = c("window", "mmt.lag", "PE", "PB", "PS", "UMD", "PE.60day", "PB.60day", "PS.60day"),
#omit.coef = "active.day|cmt.num.30day|trd.num.30day|stock.num.30day",
        reorder.coef = c(1:2, 6, 3:5, 7:9),
        caption.above = TRUE,
        digits = 4,
        inline.css = FALSE,
        doctype = TRUE,
        html.tag = TRUE,
        head.tag = TRUE,
        body.tag = TRUE,
        center = FALSE,
        stars = c(0.001, 0.01, 0.05, 0.1)
            )


# 1.3.2 择股能力
full.mmt <- clsp.amnt[, .SD[.N > 60], by = .(stock.symbol)
    ][, mmt := {
    a <- vector()
    for (i in 60:.N) {
        a[i] <- Clsprc[i] / Clsprc[i - 59] * 100 - 100
    }
    a
    }, by = .(stock.symbol)
    ][, date.month := str_sub(date, start = 1L, end = 7L)
    ][, date.month := str_replace_all(date.month, "-", "")
    ][, date.qrtr := str_c(year(date), quarter(date))
    ][, mmt.lag := shift(mmt, type = "lag"), by = .(stock.symbol)]

f.cube.stock.sl.ns <- f.cube.stock.ns[, .(stock.symbol, date, date.month, event)
    ][full.mmt, on = .(stock.symbol, date, date.month)
    ][date %in% f.cube.stock.ns$date, .SD
    ][, event := fifelse(is.na(event), 0, event)]

f.cube.stock.sl.ss <- f.cube.stock.ss[, .(stock.symbol, date, date.month, event)
    ][full.mmt, on = .(stock.symbol, date, date.month)
    ][date %in% f.cube.stock.ss$date, .SD
    ][, event := fifelse(is.na(event), 0, event)]

f.cube.stock.sl.ts <- f.cube.stock.ts[, .(stock.symbol, date, date.month, event)
    ][full.mmt, on = .(stock.symbol, date, date.month)
    ][date %in% f.cube.stock.ss$date, .SD
    ][, event := fifelse(is.na(event), 0, event)]

f.cube.stock.sl.rs <- f.cube.stock.rs[, .(stock.symbol, date, date.month, event)
    ][full.mmt, on = .(stock.symbol, date, date.month)
    ][date %in% f.cube.stock.ss$date, .SD
    ][, event := fifelse(is.na(event), 0, event)]

# 与市场因子umd合并
f.cube.stock.sl.ns <- fivefactor_daily[, .(umd, date = trddy)
    ][f.cube.stock.sl.ns, on = .(date)]
f.cube.stock.sl.ss <- fivefactor_daily[, .(umd, date = trddy)
    ][f.cube.stock.sl.ss, on = .(date)]
f.cube.stock.sl.ts <- fivefactor_daily[, .(umd, date = trddy)
    ][f.cube.stock.sl.ts, on = .(date)]
f.cube.stock.sl.rs <- fivefactor_daily[, .(umd, date = trddy)
    ][f.cube.stock.sl.rs, on = .(date)]

# 与PB和PE合并
f.cube.stock.sl.ns <- stock.index[, .(stock.symbol, date, PB, PE, PCF, PS, PB.60day, PE.60day, PCF.60day, PS.60day)
    ][f.cube.stock.sl.ns, on = .(stock.symbol, date)]
f.cube.stock.sl.ss <- stock.index[, .(stock.symbol, date, PB, PE, PCF, PS, PB.60day, PE.60day, PCF.60day, PS.60day)
    ][f.cube.stock.sl.ss, on = .(stock.symbol, date)]
f.cube.stock.sl.ts <- stock.index[, .(stock.symbol, date, PB, PE, PCF, PS, PB.60day, PE.60day, PCF.60day, PS.60day)
    ][f.cube.stock.sl.ts, on = .(stock.symbol, date)]
f.cube.stock.sl.rs <- stock.index[, .(stock.symbol, date, PB, PE, PCF, PS, PB.60day, PE.60day, PCF.60day, PS.60day)
    ][f.cube.stock.sl.rs, on = .(stock.symbol, date)]


r1 <- f.cube.stock.sl.ns[, felm(mmt ~ event | stock.symbol + date.qrtr)]
r2 <- f.cube.stock.sl.ns[, felm(mmt ~ event + mmt.lag + PE + PB + PS + umd | stock.symbol + date.qrtr)]
r3 <- f.cube.stock.sl.ns[, felm(mmt ~ event + mmt.lag + PE.60day + PB.60day + PS.60day + umd | stock.symbol + date.qrtr)]

r4 <- f.cube.stock.sl.ss[, felm(mmt ~ event | stock.symbol + date.qrtr)]
r5 <- f.cube.stock.sl.ss[, felm(mmt ~ event + mmt.lag + PE + PB + PS + umd | stock.symbol + date.qrtr)]
r6 <- f.cube.stock.sl.ss[, felm(mmt ~ event + mmt.lag + PE.60day + PB.60day + PS.60day + umd | stock.symbol + date.qrtr)]

r7 <- f.cube.stock.sl.ts[, felm(mmt ~ event | stock.symbol + date.qrtr)]
r8 <- f.cube.stock.sl.ts[, felm(mmt ~ event + mmt.lag + PE + PB + PS + umd | stock.symbol + date.qrtr)]
r9 <- f.cube.stock.sl.ts[, felm(mmt ~ event + mmt.lag + PE.60day + PB.60day + PS.60day + umd | stock.symbol + date.qrtr)]

r10 <- f.cube.stock.sl.rs[, felm(mmt ~ event | stock.symbol + date.qrtr)]
r11 <- f.cube.stock.sl.rs[, felm(mmt ~ event + mmt.lag + PE + PB + PS + umd | stock.symbol + date.qrtr)]
r12 <- f.cube.stock.sl.rs[, felm(mmt ~ event + mmt.lag + PE.60day + PB.60day + PS.60day + umd | stock.symbol + date.qrtr)]

list(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12) %>%
    htmlreg(
        file = "mmt.stock.html",
        custom.header = list("Naive策略" = 1:3, "Sharpe Ratio策略" = 4:6, "收益率滚动排行策略" = 7:9, "风险-收益率策略" = 10:12),
#caption.above = TRUE,
        include.rs = TRUE,
        include.adjrs = FALSE,
        custom.model.names = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)", "(9)", "(10)", "(11)", "(12)"),
        custom.gof.rows = list(
#"Control Variables" = c("No", "Yes", "No", "Yes", "No", "Yes"),
        "Stock FE" = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
        "Quarter FE" = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")
        ),
       custom.coef.names = c("window", "mmt.lag", "PE", "PB", "PS","UMD", "PE.60day", "PB.60day", "PS.60day"),
#omit.coef = "active.day|cmt.num.30day|trd.num.30day|stock.num.30day",
        reorder.coef = c(1:2, 6, 3:5, 7:9),
        caption.above = TRUE,
        digits = 4,
        inline.css = FALSE,
        doctype = TRUE,
        html.tag = TRUE,
        head.tag = TRUE,
        body.tag = TRUE,
        center = FALSE,
        stars = c(0.001, 0.01, 0.05, 0.1)
            )

## 2. 看择时能力，占最大回撤和最大上涨的期间----
#stock <- c(f.cube.stock.ns[, unique(stock.symbol)], f.cube.stock.ss[, unique(stock.symbol)], f.cube.stock.ts[, unique(stock.symbol)], f.cube.stock.rs[, unique(stock.symbol)]) %>% unique()
    
#a <- clsp.amnt[stock.symbol %in% stock,
    ##c("retreat", "goal.date") :=
    #{
    #l <- list()
    #for (i in 1:.N) {
        #bd <- date[i]
        #cp <- Clsprc[i]
        #fd <- .SD[(i + 1):.N, .(goal.date=date, clsp = Clsprc, begin.date= bd)
        #][, retreat := 1 - clsp / cp
        #][, unique(.SD), .SDcols = c("retreat", "goal.date", "begin.date")
        #][retreat == min(retreat), .SD
        #]
        ##l[[i]] <- list(retreat = fd$retreat, goal.date = fd$date)
        #l[[i]] <- fd[1]
    #}
        ##rbindlist(l, fill = T)
        #rbindlist(l,fill = T)
#}, by = .(stock.symbol) # 最大回撤
    #]
