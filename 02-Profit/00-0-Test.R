library(styleer)
# 1. Pre-processing factor data----
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

### 看从头到尾没有进行follow的cube的行为
ld(f.main1)
ld(f.main2)
ld(f.cube.ret.sp)
ld(user.wnwk.sp)
# Caculate in-degree & out-degree
f.nwl.ind <- user.wnwk.sp[, .(date = as.Date(follow.date), cube.symbol = from.cube.symbol, to.cube.symbol)
    ][order(cube.symbol, date)
    ][, out := ifelse(!is.null(to.cube.symbol), str_c(unlist(to.cube.symbol), collapse = ","), NA), keyby = .(cube.symbol, date)
    ][, .(sp.out = unlist(str_split(out, ','))), keyby = .(cube.symbol, date)
    ][!is.na(sp.out), .SD[order(sp.out, date)]
    ][, .(indegree = list(cube.symbol)), keyby = .(sp.out, date)]
f.nwl.ind <- f.nwl.ind[, setnames(.SD, 1, "cube.symbol")]

setnames(user.wnwk.sp, 1:3, c("cube.symbol", "date", "outdegree"))
f.nwl.ints <- f.nwl.ind[user.wnwk.sp[, date := as.Date(date)], on = .(date, cube.symbol)]

f.nwl.ints[!is.null(indegree), ind := indegree %>% unlist() %>% length(), by = .(cube.symbol, date)]
f.nwl.ints[!is.null(outdegree), oud := outdegree %>% unlist() %>% length(), by = .(cube.symbol, date)]

sv(f.nwl.ints, svname = "f.nwl.ints.ind.oud")

ld(f.nwl.ints.ind.oud)
# select the non-follow sample with indegree
ind.oud <- f.nwl.ints[, sum.oud := sum(oud), by = .(cube.symbol)
    ][ind != 0 & sum.oud == 0, .SD, .SDcols = c(1:3, 5)
    ]


follow.sample <- f.main2[, .(cube.symbol = unique(cube.symbol), tag = "followsample")]
f.nonfollow <- follow.sample[f.cube.ret.sp, on = "cube.symbol"
    ][is.na(tag), .(cube.symbol, date, value)]
f.nonfollow.daily <- fivefactor_daily[, setnames(.SD, 1, "date")
    ][, date := as.Date(date)
    ][f.nonfollow, on = .(date)
    ][, ret := value / shift(value, type = "lag") - 1, by = .(cube.symbol)
    ][is.infinite(ret), ret := 0
    ][, active.day := date - min(date), by = .(cube.symbol)
    ][, start := min(date), by = .(cube.symbol)]

# Regress the sample non-follow sample with indegree
f.nonfollow.daily <- ind.oud[f.nonfollow.daily, on = .(cube.symbol, date)]
f.nonfollow.daily[!is.na(ind), lm(ret - rf ~ mkt_rf + smb + hml)] %>% summary()
f.nonfollow.daily[!is.na(ind), lm(ret - rf ~ mkt_rf + smb + hml + umd)] %>% summary()
f.nonfollow.daily[!is.na(ind), lm(ret - rf ~ mkt_rf + smb + hml + rmw + cma)] %>% summary()
f.nonfollow.daily[!is.na(ind), lm(ret - rf ~ mkt_rf + smb + hml + umd + rmw + cma)] %>% summary()

f.nonfollow.daily[!is.na(ind), rq(ret - rf ~ mkt_rf + smb + hml, tau = 0.5, method = "pfn", data = .SD)] %>% summary()
f.nonfollow.daily[!is.na(ind), rq(ret - rf ~ mkt_rf + smb + hml + umd, tau = 0.5, method = "pfn", data = .SD)] %>% summary()
f.nonfollow.daily[!is.na(ind), rq(ret - rf ~ mkt_rf + smb + hml + rmw + cma, tau = 0.5, method = "pfn", data = .SD)] %>% summary()
f.nonfollow.daily[!is.na(ind), rq(ret - rf ~ mkt_rf + smb + hml + umd + rmw + cma, tau = 0.5, method = "pfn", data = .SD)] %>% summary()

## 构造follow前后的DID，标准为follow前后的pre.period，周和月----
url <- str_c(getwd(), "/data/Index")
index <- fbread(path = url, pattern = "*\\d.txt")
index <- index[, file_id := NULL
    ][Indexcd == "000300"
    ][, ':='(index_ret = Idxtrd08 / 100, index_value = Idxtrd05, date = as.Date(Idxtrd01))]
f.main1 <- index[, .(date, index_ret, index_value)
    ][f.main1, on = .(date)]
# Regression

f.main1.day <- f.main1[is.infinite(ret), ret := 0
    #][pre.period >= 0 , .SD #去除那些清仓的记录
    #][ret != 0, .SD
    ][, abnor_ret := ret - index_ret]

f.main1.week <- f.main1[is.infinite(ret), ret := 0
    #][ret != 0, .SD
    ][post.follow == 1,
        week.num := {
            if (.N > 5) {
                a <- .N %/% 5
                b <- .N %% 5
                c(rep(1:a, each = 5), rep(1 + a, each = b))
            } else {
                1
            }
        } %>% str_c("post"),
        by = .(cube.symbol)
        ][post.follow == 0,
        week.num := {
            if (.N > 5) {
                a <- .N %/% 5
                b <- .N %% 5
                c(rep(1 + a, each = b), rep(a:1, each = 5))
            } else {
                1
            }
        } %>% str_c("pre"),
        by = .(cube.symbol)
        ][, .SD[.N], by = .(cube.symbol, week.num)
        ][, ':='(ret_week = value / shift(value, type = "lag") - 1, index_ret_week = index_value / shift(index_value, type = "lag") - 1), by = .(cube.symbol)
        ][is.infinite(ret_week), ret_week := 0
        ][, abnor_ret_week := ret_week - index_ret_week]

f.main1.month <- f.main1[is.infinite(ret), ret := 0
    #][ret != 0, .SD
    ][post.follow == 1,
        month.num := {
            if (.N > 22) {
                a <- .N %/% 22
                b <- .N %% 22
                c(rep(1:a, each = 22), rep(1 + a, each = b))
            } else {
                1
            }
        } %>% str_c("post"),
        by = .(cube.symbol)
        ][post.follow == 0,
        month.num := {
            if (.N > 22) {
                a <- .N %/% 22
                b <- .N %% 22
                c(rep(1 + a, each = b), rep(a:1, each = 22))
            } else {
                1
            }
        } %>% str_c("pre"),
        by = .(cube.symbol)
        ][,
        ][, .SD[.N], by = .(cube.symbol, month.num)
        ][, ':='(ret_month = value / shift(value, type = "lag") - 1, index_ret_month = index_value / shift(index_value, type = "lag") - 1), by = .(cube.symbol)
        ][is.infinite(ret_month), ret_month := 0
        ][, abnor_ret_month := ret_month - index_ret_month]

f.main1.day[post.follow == 0 | (post.follow == 1 & (date - follow.date < pre.period)), felm(ret ~ post.follow | cube.symbol + stock.num + follow.date)] %>% summary()
f.main1.day[post.follow == 0 | (post.follow == 1 & (date - follow.date < pre.period)), felm(ret ~ post.follow + as.numeric(trd.num / 100) + as.numeric(active.day / 365) + mmt | cube.symbol + stock.num + follow.date)] %>% summary()
f.main1.day[, felm(ret - index_ret ~ post.follow + as.numeric(trd.num / 100) + as.numeric(active.day / 365) + mmt | cube.symbol + stock.num + follow.date)] %>% summary()
f.main1.day[post.follow == 0 | (post.follow == 1 & (date - follow.date < pre.period)), rq(abnor_ret ~ post.follow + mmt + as.numeric(trd.num / 100) + as.numeric(active.day / 365), tau = seq(0.1, 0.9, by = 0.1))] %>% summary()

f.main1.week[post.follow == 0 | (post.follow == 1 & (date - follow.date < pre.period)), felm(ret_week ~ post.follow | cube.symbol + stock.num + follow.date)] %>% summary()
f.main1.week[post.follow == 0 | (post.follow == 1 & (date - follow.date < pre.period)), felm(ret_week - index_ret_week ~ post.follow + as.numeric(trd.num / 100) + as.numeric(active.day / 365) + mmt | cube.symbol + stock.num + follow.date)] %>% summary()
f.main1.week[, felm(ret_week - index_ret_week ~ post.follow + as.numeric(trd.num / 100) + as.numeric(active.day / 365) + mmt | cube.symbol + stock.num + follow.date)] %>% summary()
f.main1.week[post.follow == 0 | (post.follow == 1 & (date - follow.date < pre.period)), rq(abnor_ret_week ~ post.follow + mmt + as.numeric(trd.num / 100) + as.numeric(active.day / 365), tau = seq(0.1, 0.9, by = 0.1))] %>% summary()

f.main1.month[post.follow == 0 | (post.follow == 1 & (date - follow.date < pre.period)), felm(ret_month - index_ret_month ~ post.follow | cube.symbol + stock.num + follow.date)] %>% summary()
f.main1.month[post.follow == 0 | (post.follow == 1 & (date - follow.date < pre.period)), felm(ret_month - index_ret_month ~ post.follow + as.numeric(trd.num / 100) + as.numeric(active.day / 365) + mmt | cube.symbol + stock.num + follow.date)] %>% summary()
f.main1.month[, felm(ret_month - index_ret_month ~ post.follow + as.numeric(trd.num / 100) + as.numeric(active.day / 365) + mmt | cube.symbol + stock.num + follow.date)] %>% summary()
f.main1.month[post.follow == 0 | (post.follow == 1 & (date - follow.date < pre.period)), rq(abnor_ret_month ~ post.follow + mmt + as.numeric(trd.num / 100) + as.numeric(active.day / 365), tau = seq(0.1, 0.9, by = 0.1))] %>% summary()

# 有follow和没有follow样本的对比----
follow.sample <- f.main2[, .(cube.symbol = unique(cube.symbol), tag = "followsample")]
f.nonfollow <- follow.sample[f.cube.ret.sp, on = "cube.symbol"
    ][is.na(tag), .(cube.symbol, date, value)]
f.nonfollow.daily <- fivefactor_daily[, setnames(.SD, 1, "date")
    ][, date := as.Date(date)
    ][f.nonfollow, on = .(date)
    ][, ret := value / shift(value, type = "lag") - 1, by = .(cube.symbol)
    ][is.infinite(ret), ret := 0
    ][, active.day := date - min(date), by = .(cube.symbol)
    ][, start := min(date), by = .(cube.symbol)]

f.nonfollow.daily[ret < 1, lm(ret - rf ~ mkt_rf + smb + hml)] %>% summary()
f.nonfollow.daily[ret < 1000, lm(ret - rf ~ mkt_rf + smb + hml + umd)] %>% summary()
f.nonfollow.daily[ret < 1000, lm(ret - rf ~ mkt_rf + smb + hml + rmw + cma)] %>% summary()
f.nonfollow.daily[ret < 1000, lm(ret - rf ~ mkt_rf + smb + hml + umd + rmw + cma)] %>% summary()

f.nonfollow.daily[, rq(ret - rf ~ mkt_rf + smb + hml, tau = c(0.1, 0.5, 0.9), method = "pfn", data = .SD)] %>% summary()
f.nonfollow.daily[, rq(ret - rf ~ mkt_rf + smb + hml + umd, tau = 0.5, method = "pfn", data = .SD)] %>% summary()
f.nonfollow.daily[, rq(ret - rf ~ mkt_rf + smb + hml + rmw + cma, tau = 0.5, method = "pfn", data = .SD)] %>% summary()
f.nonfollow.daily[, rq(ret - rf ~ mkt_rf + smb + hml + umd + rmw + cma, tau = 0.5, method = "pfn", data = .SD)] %>% summary()

f.nonfollow.daily[active.day < 7, lm(ret - rf ~ mkt_rf + smb + hml)] %>% summary()
f.nonfollow.daily[active.day < 7, lm(ret - rf ~ mkt_rf + smb + hml + umd)] %>% summary()
f.nonfollow.daily[active.day < 7, lm(ret - rf ~ mkt_rf + smb + hml + rmw + cma)] %>% summary()
f.nonfollow.daily[active.day < 7, lm(ret - rf ~ mkt_rf + smb + hml + umd + rmw + cma)] %>% summary()

f.follow.daily <- fivefactor_daily[, setnames(.SD, 1, "date")
    ][, date := as.Date(date)
    ][f.main2, on = .(date)
    ][, ret := value / shift(value, type = "lag") - 1, by = .(cube.symbol)
    ][is.infinite(ret), ret := 0
    ]

f.follow.daily[ret < 1, lm(ret - rf ~ mkt_rf + smb + hml)] %>% summary()
f.follow.daily[, lm(ret - rf ~ mkt_rf + smb + hml + umd)] %>% summary()
f.follow.daily[, lm(ret - rf ~ mkt_rf + smb + hml + rmw + cma)] %>% summary()
f.follow.daily[, lm(ret - rf ~ mkt_rf + smb + hml + umd + rmw + cma)] %>% summary()

f.follow.daily[, rq(ret - rf ~ mkt_rf + smb + hml, tau = c(0.1, 0.5, 0.9), method = "pfn", data = .SD)] %>% summary()
f.follow.daily[, rq(ret - rf ~ mkt_rf + smb + hml + umd, tau = c(0.1, 0.5, 0.9), method = "pfn", data = .SD)] %>% summary()
f.follow.daily[, rq(ret - rf ~ mkt_rf + smb + hml + rmw + cma, tau = c(0.1, 0.5, 0.9), method = "pfn", data = .SD)] %>% summary()
f.follow.daily[, rq(ret - rf ~ mkt_rf + smb + hml + umd + rmw + cma, tau = c(0.1, 0.5, 0.9), method = "pfn", data = .SD)] %>% summary()

## 计算每天每个cube每只股票的持仓percent----
library(styleer)
ld(f.cube.rb.sp)
ld(stock.info)
cl <- makeCluster(8)
registerDoParallel(cl)

a <- f.cube.rb.sp[cube.symbol %in% c("SP1000047", "SP1000002"), .(cube.symbol, date = as.Date(created.at), created.at, stock.symbol, price, target.weight, prev.weight.adjusted)
    ][stock.info[cube.symbol %in% c("SP1000047", "SP1000002")], on = .(cube.symbol, date)
    ][!is.na(created.at)
    #][order(cube.symbol, stock.symbol, - created.at)
    #][, tag := 1:.N, by = .(cube.symbol, stock.symbol)
    #][order(cube.symbol, -date, stock.symbol), .SD
    #][,
        #ratio1 :=
        #{
        #ratio <- list()
        #for (i in 1:.N) {
            #d <- date[i]
            #sl <- stock.list[i] %>% unlist()
            #p <- .SD[i:.N][stock.symbol %in% sl, .SD[1], by = .(cube.symbol, stock.symbol)]
            #tw <- p[date %in% d, .(weight = target.weight, stock.symbol)] # 将当日调仓的股票target.weight拿出来
            #pw <- p[!(date %in% d), .(weight = fifelse(prev.weight.adjusted > 0, prev.weight.adjusted, target.weight), stock.symbol)] # 非当日调仓的prev.weight拿出来
            #ratio[[i]] <- rbindlist(list(tw, pw))
        #}
        #ratio       
    #}
    ][order(cube.symbol, date, stock.symbol), .SD
    ][,
    .(
          ratio =
        {
            #ratio <- list()
            foreach(i = 1:.N) %dopar% {
            library(data.table)
            d <- date[i]
            sl1 <- unlist(stock.list[i])
            p.fward <- .SD[i:.N
            ][order(stock.symbol, created.at)
            ][stock.symbol %in% sl1, .SD[1], .SDcols = c("date", "created.at", "target.weight", "prev.weight.adjusted"), by = .(stock.symbol)]

            sl2 <- setdiff(sl1, p.fward[["stock.symbol"]])
            p.bward <- .SD[1:i
            ][order(stock.symbol, created.at)
            ][stock.symbol %in% sl2, .SD[.N], .SDcols = c("date", "created.at", "target.weight","prev.weight.adjusted"), by = .(stock.symbol)]

            fp <- p.fward[, .(weight = fifelse(prev.weight.adjusted > 0, prev.weight.adjusted, target.weight), stock.symbol)] 
            bp <- p.bward[, .(weight = fifelse(prev.weight.adjusted > 0, prev.weight.adjusted, target.weight), stock.symbol)]
            rbindlist(list(fp, bp))
        }
        #ratio
    }
    , date, created.at)
    , by = .(cube.symbol)
    ]

#a <- f.cube.rb.sp[cube.symbol == "SP1000002", .(cube.symbol, date = as.Date(created.at), created.at, stock.symbol, price, target.weight, prev.weight.adjusted)
    #][order(cube.symbol, created.at), .SD]

#a.start <- f.cube.rb.sp[, .SD[1], by = .(cube.symbol, stock.symbol)
    #][prev.weight.adjusted != 0, .SD
    #][, date := as.Date(created.at)
    #][, .(stock.list.start = list(stock.symbol)), by = .(cube.symbol)
    #][cube.symbol == "SP1000002"]


#a.stock0_1 <- f.cube.rb.sp[prev.weight.adjusted == 0, .SD
    #][, date := as.Date(created.at)
    #][order(cube.symbol, date), .SD
    #][, .(stock.symbol = list(stock.symbol)), by = .(cube.symbol, date)
    #][, .(stock.list.buy = sadd(stock.symbol), date), by = .(cube.symbol)
    #][cube.symbol == "SP1000002"]

#a.stock1_0.second <- f.cube.rb.sp[, .SD[-1], by = .(cube.symbol, stock.symbol)
    #][target.weight == 0, .SD
    #][, date := as.Date(created.at)
    #][cube.symbol == "SP1000002"]
#a.stock1_0.first <- f.cube.rb.sp[, .SD[1], by = .(cube.symbol, stock.symbol)
    #][target.weight == 0, .SD
    #][, date := as.Date(created.at)
    #][cube.symbol == "SP1000002"]
#a.stock1_0 <- rbindlist(list(a.stock1_0.first, a.stock1_0.second), fill = T)
#a.stock1_0 <- a.stock1_0[order(cube.symbol, date), .SD
    #][, .(stock.symbol = list(stock.symbol)), by = .(cube.symbol, date)
    #][, .(stock.list.sell = sadd(stock.symbol), date), by = .(cube.symbol)]

#a.info <- a.stock0_1[a.stock1_0[a.start[f.cube.ret.sp[cube.symbol == "SP1000002", .(cube.symbol, date)], on = .(cube.symbol)], on = .(cube.symbol, date), roll = T], on = .(cube.symbol, date), roll = T]
#a.info <- a.info[, .(stock.list = which(table(c(unlist(stock.list.start), unlist(stock.list.buy), unlist(stock.list.sell))) %% 2 != 0) %>% names() %>% list()), by = .(cube.symbol, date)]

#b <- f.cube.rb.sp[a.info[f.cube.ret.sp[cube.symbol == "SP1000002"], on = .(cube.symbol, date)], on = .(cube.symbol, date)]

# ----
ld(f.cube.rb.sp)
ld(f.surv.flw)
# 1. 计算sell与buy的单只股票收益率
f.trd.sell.buy <- f.cube.rb.sp[, .(cube.symbol, date= as.Date(created.at), stock.symbol = str_sub(stock.symbol, start = 3L, end = 9L), price, target.weight, prev.weight.adjusted)
][f.surv.flw[, setnames(.SD, "stck", "stock.symbol")
][, .SD, .SDcols = -c("created.at", "price", "prev.weight.adjusted", "target.weight")
], on = .(cube.symbol, date, stock.symbol)]

# 1.1 计算sell时刻获得的收益率，并与post.follow回归
sell <- f.trd.sell.buy[, pre.period := follow.date - min(date), by = .(cube.symbol)
][first.half == 1 | (second.half == 1 & date - follow.date < pre.period), .SD
][, sell.ret := fifelse(issale == 1, (price - hold.price.lst) * (prev.weight.adjusted - target.weight)/100, 0), by = .(cube.symbol, stock.symbol)
]

sell[!is.infinite(sell.ret) & sell.ret != 0, felm(sell.ret ~ second.half | stock.symbol)] %>% summary()
sell[!is.infinite(sell.ret) & sell.ret != 0, felm(sell.ret ~ second.half + mmt + active.day | stock.symbol)] %>% summary()
#sell[!is.infinite(sell.ret.30day.before) & !is.na(sell.ret.30day.before) & sell.ret.30day.before != 0 & second.half == 1, t.test(sell.ret.30day.before, alternative = "less")]
#sell[!is.infinite(sell.ret.30day.before) & !is.na(sell.ret.30day.before) & sell.ret.30day.before != 0 & second.half == 1, t.test(sell.ret.30day.before, alternative = "greater")]
#sell[!is.infinite(sell.ret.30day.before) & !is.na(sell.ret.30day.before) & sell.ret.30day.before != 0 & second.half == 0, t.test(sell.ret.30day.before, alternative = "less")]
#sell[!is.infinite(sell.ret.30day.before) & !is.na(sell.ret.30day.before) & sell.ret.30day.before != 0 & second.half == 0, t.test(sell.ret.30day.before, alternative = "greater")]

# 1.2 计算buy时刻之后3-60天的收益率，并与post.follow回归
buy <- f.trd.sell.buy[, pre.period := follow.date - min(date), by = .(cube.symbol)
][first.half == 1 | (second.half == 1 & date - follow.date < pre.period), .SD
][, buy.ret.60day.after := fifelse(target.weight > prev.weight.adjusted, (shift(Clsprc, type = "lead", n = 44L) - price) * (target.weight - prev.weight.adjusted) / 100, 0), by = .(cube.symbol, stock.symbol, second.half)
][, buy.ret.30day.after := fifelse(target.weight > prev.weight.adjusted, (shift(Clsprc, type = "lead", n = 22L) - price) * (target.weight - prev.weight.adjusted) / 100, 0), by = .(cube.symbol, stock.symbol, second.half)
][, buy.ret.15day.after := fifelse(target.weight > prev.weight.adjusted, (shift(Clsprc, type = "lead", n = 11L) - price) * (target.weight - prev.weight.adjusted) / 100, 0), by = .(cube.symbol, stock.symbol, second.half)
][, buy.ret.7day.after := fifelse(target.weight > prev.weight.adjusted, (shift(Clsprc, type = "lead", n = 5L) - price) * (target.weight - prev.weight.adjusted) / 100, 0), by = .(cube.symbol, stock.symbol, second.half)
][, buy.ret.3day.after := fifelse(target.weight > prev.weight.adjusted, (shift(Clsprc, type = "lead", n = 3L) - price) * (target.weight - prev.weight.adjusted) / 100, 0), by = .(cube.symbol, stock.symbol, second.half)
][, buy.ret := fifelse(target.weight > prev.weight.adjusted, (Clsprc - price) * (target.weight - prev.weight.adjusted) / 100, 0), by = .(cube.symbol, stock.symbol, second.half)
]

buy[!is.infinite(buy.ret) & buy.ret != 0, felm(buy.ret ~ second.half | stock.symbol)] %>% summary()
buy[!is.infinite(buy.ret) & buy.ret != 0, felm(buy.ret ~ second.half + mmt + active.day | stock.symbol)] %>% summary()

buy[!is.infinite(buy.ret.3day.after) & buy.ret.3day.after != 0, felm(buy.ret.3day.after ~ second.half + mmt + active.day)] %>% summary()
buy[!is.infinite(buy.ret.3day.after) & buy.ret.3day.after != 0, felm(buy.ret.3day.after ~ second.half + mmt + active.day | stock.symbol)] %>% summary()

buy[!is.infinite(buy.ret.7day.after) & buy.ret.7day.after != 0, felm(buy.ret.7day.after ~ second.half + mmt + active.day)] %>% summary()
buy[!is.infinite(buy.ret.7day.after) & buy.ret.7day.after != 0, felm(buy.ret.7day.after ~ second.half + mmt + active.day | stock.symbol)] %>% summary()

buy[!is.infinite(buy.ret.15day.after) & buy.ret.15day.after != 0, felm(buy.ret.15day.after ~ second.half + mmt + active.day)] %>% summary()
buy[!is.infinite(buy.ret.15day.after) & buy.ret.15day.after != 0, felm(buy.ret.15day.after ~ second.half + mmt + active.day | stock.symbol)] %>% summary()

buy[!is.infinite(buy.ret.30day.after) & buy.ret.30day.after != 0, felm(buy.ret.30day.after ~ second.half + mmt + active.day)] %>% summary()
buy[!is.infinite(buy.ret.30day.after) & buy.ret.30day.after != 0, felm(buy.ret.30day.after ~ second.half + mmt + active.day | stock.symbol)] %>% summary()

buy[!is.infinite(buy.ret.60day.after) & buy.ret.60day.after != 0, felm(buy.ret.60day.after ~ second.half + mmt + active.day | stock.symbol)] %>% summary()
buy[!is.infinite(buy.ret.60day.after) & buy.ret.60day.after != 0, felm(buy.ret.60day.after ~ second.half + mmt + active.day| stock.symbol)] %>% summary()
#buy[!is.na(buy.ret.30day.after) & buy.ret.30day.after != 0 & second.half == 1, t.test(buy.ret.30day.after, alternative = "greater")]
#buy[!is.na(buy.ret.30day.after) & buy.ret.30day.after != 0 & second.half == 0, t.test(buy.ret.30day.after, alternative = "less")]

ld(f.cube.rb.sp)
ld(f.cube.ret.sp)
url <- str_c(getwd(), "/data/Clprc")
clprc <- fbread(path = url, pattern = "*.txt")
f.cube.rb.sp.test<- clprc[, file_id := NULL
    ][, ":="(stock.symbol = (formatC(Stkcd, flag = '0', width = 6)), date = as.Date(Trddt))
    ][, Clsprc.lag := shift(Clsprc, type = "lag"), by = .(stock.symbol)
    ][, .SD, .SDcols = c("stock.symbol", "date", "Clsprc", "Clsprc.lag")
    ]
f.cube.rb.sp[, ':='(stock.symbol = str_sub(stock.symbol, start = 3L, end = -1L), date = as.Date(created.at))]

f.cube.rb.sp.test <- f.cube.rb.sp.test[f.cube.rb.sp, on = .(stock.symbol, date)]
sv(f.cube.rb.sp.test, svname = "f.cube.rb.sp.test")

# LIFO与FIFO计算 ----
ld(f.flifo, force = T)
a <- f.flifo[cube.symbol %in% c("SP1000002", "SP1000012", "SP1000047"), .SD
    ]

a <- a[, id := .I #标记每一笔交易的id
    ][, sale := fifelse(target.weight < prev.weight.adjusted, 1, 0)
    ][, tag.stock := ifelse(target.weight == 0, 1, 0) # 区分每次同一只股票清仓标记
    ][, tag.stock := cumsum(tag.stock), by = .(cube.symbol, stock.symbol)
    ][, tag.stock.lag := shift(tag.stock), by = .(cube.symbol, stock.symbol)
    ][, tag.stock.lag := fifelse(is.na(tag.stock.lag), tag.stock, tag.stock.lag)
    ][, tag.stock := tag.stock.lag 
    ][, tag.stock.lag := NULL
    ][, stock.symbol.tag := str_c(stock.symbol, tag.stock, sep = "_") # 区分每次同一只股票清仓标记
    ][, change.weight := target.weight - shift(target.weight, type = "lag"), by = .(cube.symbol, stock.symbol.tag)
    ][, change.weight := fifelse(is.na(change.weight), target.weight, change.weight)
    ][, cum.change.weight := cumsum(change.weight), by = .(cube.symbol, stock.symbol)
    ][, tag.order := 1:.N, by = .(cube.symbol, stock.symbol.tag)
    ][, prev.weight := shift(target.weight, type = "lag"), by = .(cube.symbol, stock.symbol.tag)
    ][, prev.weight := fifelse(is.na(prev.weight), 0, prev.weight)
    ]
    b <- a[, .(open.position = {
        l <- list()
        for (i in 1:.N) {
            if (tag.order[i] == 1) { # 每个cube每个stock的第一笔交易
               l[[i]] <- data.table(hold.weight = target.weight[i], hold.price = price[i], hold.id = id[i])
            }
            else { # 非第一笔交易
                rd <- .SD[1:(i - 1)];
                buy.rd <- rd[sale == 0, .SD
                    ][, cum.buy.weight := cumsum(change.weight)];
                sale.rd <- rd[sale == 1, .SD
                    ][, cum.sale.weight := cumsum(change.weight)];
                if (target.weight[i] == 0) {
                # 清仓
                    l[[i]] <- data.table()
                    # 还差一个realized.gain需要计算
                }
                else if (change.weight[i] < 0 & target.weight[i] != 0) {
                # 当前为卖出交易
                    hw <- target.weight[i];
                    p <- price[i];
                    br <- buy.rd[order(-id), .SD
                        ][, cum.buy.weight1 := cumsum(change.weight)]
                    s.1 <- br[cum.buy.weight1 <= hw
                        ][, .(hold.weight = change.weight, hold.price = price, hold.id = id)];
                    s.2 <- br[cum.buy.weight1 - hw > 0, .SD[1]
                        ][, .(hold.weight = abs(hw - s.1[, sum(hold.weight)]), hold.price = price, hold.id = id)];
                    s.3 <- rbindlist(list(s.1, s.2), fill = T, use.names = T)
                    l[[i]] <- s.3
                }
                else if (change.weight[i] > 0) {
                # 当前为买入交易
                    if (sale.rd[, .N] == 0) { # 之前的卖出交易为0条
                        b.1 <- buy.rd[, .(hold.weight = change.weight, hold.price = price, hold.id = id)]
                        b.2 <- data.table(hold.weight = change.weight[i], hold.price = price[i], hold.id = id[i])
                        l[[i]] <- rbindlist(list(b.1, b.2), fill = T, use.names = T)
                    }
                    else { # 之前的卖出交易不为0条
                     # 需要将交易分为第一笔交易到最后一笔卖出交易，最后一笔卖出交易到现在这笔交易两部分
                     # 第一笔交易到卖出交易的部分
                        rd1 <- rd[id <= sale.rd[, id[.N]]];
                        buy.rd1 <- rd1[sale == 0, .SD
                            ][, cum.buy.weight := cumsum(change.weight)];
                        sale.rd1 <- rd[sale == 1, .SD
                            ][, cum.sale.weight := cumsum(change.weight)];
                        hw <- rd1[, target.weight[.N]];
                        p <- price[i];
                        br1 <- buy.rd1[order(-id), .SD
                            ][, cum.buy.weight1 := cumsum(change.weight)]
                        b.1 <- br1[cum.buy.weight1 <= hw
                            ][, .(hold.weight = change.weight, hold.price = price, hold.id = id)];
                        b.2 <- br1[cum.buy.weight1 - hw > 0, .SD[1]
                            ][, .(hold.weight = abs(hw - s.1[, sum(hold.weight)]), hold.price = price, hold.id = id)];
                       # 最后一笔卖出交易到现在这笔交易的部分
                        b.3 <- rd[id > sale.rd[, id[.N]]
                        ][, .(hold.weight = change.weight, hold.price = price, hold.id = id)];
                        b.4 <- data.table(hold.weight = change.weight[i], hold.price = price[i], hold.id = id[i])
                        l[[i]] <- rbindlist(list(b.1, b.2, b.3, b.4), fill = T, use.names = T)
                    }
                }
            }
        }
        l
    }, created.at = created.at, price, target.weight, prev.weight.adjusted, id), by = .(cube.symbol, stock.symbol.tag)]
d <- b[, rbindlist(open.position, fill = T), by = .(cube.symbol, stock.symbol.tag, created.at, price, target.weight, prev.weight.adjusted, id)]

# 计算卖出交易的那些realized gain
a <- f.lifo[1:500000, .SD
    ][, sale := fifelse(target.weight < prev.weight.adjusted, 1, 0)
    ][, .SD[.N > 1], by = .(cube.symbol, stock.symbol.tag)]
a[target.weight != prev.weight.adjusted, realized.gain := {
    l <- vector(mode = "numeric")
    for (i in 2:.N) {
        s <- sale[i]
        if (s == 1) {
            a <- open.position[[i]]
            b <- open.position[[i - 1]]
            if (length(a) == 0) {
                hw <- b[, .(rg = sum((price[i] - hold.price) * hold.weight))]
                l[i] <- unique(hw$rg)
            }
            else {
                hw <- a[b, on = .(hold.id, hold.price)
                ][, hold.weight.change := fifelse(!is.na(hold.weight), i.hold.weight - hold.weight, i.hold.weight)
                ][, .(rg = sum((price[i] - hold.price) * hold.weight.change))]
                l[i] <- unique(hw$rg)
            }
        }
        else {
            l[i] <- 0
        }
    }
    l
}, by = .(cube.symbol, stock.symbol.tag)]