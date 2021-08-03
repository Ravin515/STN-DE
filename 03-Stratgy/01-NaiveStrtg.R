library(styleer)

#  计算naive strategy的收益率，没有交易成本----
ld(f.cube.ret.sp)
f.cube.ret.sp <- f.cube.ret.sp[, date.month := as.character(date) %>% str_sub(start = 1L, end = 7L) %>% str_replace_all("-", "")
    ][order(cube.symbol, date, date.month), .SD]

# Naive strategy 1: 每个月月初，挑选出自publication以来ret最高的五个cube, 并且在接下来的一个月都不关闭，并同比例购买 ----
ns.1 <- f.cube.ret.sp[, .(ret.ns.1 = value / value[1] - 1, ret = value/ shift(value, type = "lag") - 1, value, date, date.month), by = .(cube.symbol)
    ][, tag := fifelse(ret >= 0.1|ret <= -0.1, 1, 0)
    ][, tag := sum(tag, na.rm = T), by = .(cube.symbol)
    ][, ret.num.monthly := .N, by = .(cube.symbol, date.month)
    ][!(date.month %in% c("201606", "201604", "201807")) & tag == 0, .SD[1], by = .(cube.symbol, date.month)
    ][order(date.month, - ret.num.monthly, - ret.ns.1), .SD
    ][, .SD[1:50], by = .(date.month)
    ][, date.month.lag := shift(date.month, type = "lag", n = 50L)] # 为之后计算交易成本和日度数据每个月月初的收益率做准备

# 算出每个月选中的组合在上个月最后一日的value
ns.cube.lag <- ns.1[!is.na(date.month.lag), .(cube.symbol, date.month.lag)]
ns.value.cube.lag <- f.cube.ret.sp[date.month %in% ns.cube.lag$date.month & cube.symbol %in% ns.cube.lag$cube.symbol, .SD[.N], .SDcols = c('value', 'date'), by = .(cube.symbol, date.month)
    ][ns.1[!is.na(date.month.lag), .(date.month = date.month.lag, cube.symbol)], on = .(cube.symbol, date.month)
    ][, .(date.month.lag = date.month), by = .(cube.symbol, date, value)
    ][, date.month := shift(date.month.lag, type = "lead", n = 50L)
    ][is.na(date.month), date.month := "201806"
    ][, .(value.cube = mean(value), date), by = .(date.month)
    ][, unique(.SD)]

## 月度数据
ns.1.ret.month <- ns.1[, .(cube.symbol, date.month)
    ][f.cube.ret.sp, on = .(cube.symbol, date.month), nomatch = 0
    ][, .SD[c(1, .N)], by = .(cube.symbol, date.month)
    ][order(date, cube.symbol), .SD
    ][, tag := rep(1:2, times = 50), by = .(date.month)
    ][, value.cube := mean(value), by = .(date.month, tag)
    ][, .(value.cube = unique(value.cube), date = unique(date)), by = .(date.month)
    ][, rbindlist(list(ns.value.cube.lag, .SD), use.names = T, fill = T)
    ][order(date.month, date), .SD
    ][, ret.cube := (value.cube[.N] / value.cube[1] - 1), by = .(date.month)
    ][, unique(.SD), .SDcols = c("date.month", "ret.cube")
    ][!is.na(ret.cube), .SD]

## 日度数据，需要将每个月选出的cube，计算出上个月最后一天到这个月第一天选出的cube组合的收益率

ns.1.ret.daily <- ns.1[, .(cube.symbol, date.month)
    ][f.cube.ret.sp, on = .(cube.symbol, date.month), nomatch = 0
    ][, value.cube := mean(value), by = .(date)
    ][, unique(.SD), .SDcols = c("date", "date.month", "value.cube")
    ][order(date.month, date), .SD]

ns.1.ret.daily <- rbindlist(list(ns.1.ret.daily, ns.value.cube.lag), use.names = T)
ns.1.ret.daily <- ns.1.ret.daily[order(date.month, date), .SD
    ][, ret.cube := (value.cube / shift(value.cube, type = "lag") - 1), by = .(date.month)
    ][!is.na(ret.cube), .SD]

## 1.1 Newey West t-test
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
# 月度Newey West t-test
ns.1.ret.month <- indexmn[, .(date.month = str_replace(Month, "-", ""), index_ret = Idxrtn)
    ][ns.1.ret.month, on = .(date.month), nomatch = 0]
ns.1.ret.month[, lm(ret.cube - index_ret ~ 1) %>% coeftest()]

# 日度Newey West t-test
ns.1.ret.daily <- index[, .(date, index_ret, index_value)
    ][ns.1.ret.daily, on = .(date), nomatch = 0]
ns.1.ret.daily[, lm(ret.cube - index_ret ~ 1) %>% coeftest()]

## 1.2 Alpha回归
## 月度因子
url <- str_c(getwd(), "/data/Factors/")
fivefactor_monthly <- fread(str_c(url, "fivefactor_monthly.csv"), encoding = "UTF-8")

ns.1.ret.month <- fivefactor_monthly[, setnames(.SD, "trdmn", "date.month")
    ][, date.month := as.character(date.month)
    ][ns.1.ret.month, on = .(date.month), nomatch = 0]

## 月度回归
ns.1.ret.month[, lm(ret.cube - rf ~ mkt_rf + smb + hml)] %>% summary()
ns.1.ret.month[, lm(ret.cube - rf ~ mkt_rf + smb + hml + umd)] %>% summary()
ns.1.ret.month[, lm(ret.cube - rf ~ mkt_rf + smb + hml + rmw + cma)] %>% summary()
ns.1.ret.month[, lm(ret.cube - rf ~ mkt_rf + smb + hml + umd + rmw + cma)] %>% summary()

## 日度因子
url <- str_c(getwd(), "/data/Factors/")
fivefactor_daily <- fread(str_c(url, "fivefactor_daily.csv"), encoding = "UTF-8")

ns.1.ret.daily <- fivefactor_daily[, setnames(.SD, "trddy", "date")
    ][, date := as.Date(date)
    ][ns.1.ret.daily, on = .(date), nomatch = 0]

## 日度回归
ns.1.ret.daily[, lm(ret.cube - rf ~ mkt_rf + smb + hml)] %>% summary()
ns.1.ret.daily[, lm(ret.cube - rf ~ mkt_rf + smb + hml + umd)] %>% summary()
ns.1.ret.daily[, lm(ret.cube - rf ~ mkt_rf + smb + hml + rmw + cma)] %>% summary()
ns.1.ret.daily[, lm(ret.cube - rf ~ mkt_rf + smb + hml + umd + rmw + cma)] %>% summary()


# Naive Strategy 2：加入交易成本的Naive Strategy 1----
## 交易成本的计算

### 交易成本 （因为雪球数据每天的净值包含了交易的手续费，只需计算每月换手的交易成本）
cost.lag <- f.cube.ret.sp[date.month %in% ns.cube.lag$date.month & cube.symbol %in% ns.cube.lag$cube.symbol, .SD[.N], .SDcols = c('value', 'date'), by = .(cube.symbol, date.month)
    ][ns.1[!is.na(date.month.lag), .(date.month = date.month.lag, cube.symbol)], on = .(cube.symbol, date.month)
    ][, .(date.month.lag = date.month), by = .(cube.symbol, date, value)
    ][, date.month := shift(date.month.lag, type = "lead", n = 50L)
    ][is.na(date.month), date.month := "201806"
    ][, date.month.lag := NULL]

cost <- ns.1[, .(cube.symbol, date.month)
    ][f.cube.ret.sp, on = .(cube.symbol, date.month), nomatch = 0
    ][, .SD[c(1, .N)], by = .(cube.symbol, date.month)
    ][order(date, cube.symbol), .SD
    ][, tag := rep(1:2, times = 50), by = .(date.month)
    ][, ':='(label = NULL, tag = NULL)]

ns.cost <- rbindlist(list(cost, cost.lag), use.names = T)

ns.cost <- ns.cost[order(date.month, cube.symbol, date), .SD
    ][, ret.per.cube := value[.N] / value[1] - 1, by = .(cube.symbol, date.month)
    ][order(date.month, date, cube.symbol), .SD
    ][, value.all.cube := mean(value), by = .(date.month, date)
    #][, ret.all.cube := .SD[date == max(date), unique(value.all.cube)] - .SD[date == min(date), unique(value.all.cube)], by = .(date.month)
    ][, ret.all.cube := mean(ret.per.cube), by = .(date.month)
    ][, .SD[(.N - 49):.N], by = .(date.month)
    ][, abs.ret.between.all.per := abs(ret.per.cube - ret.all.cube)
    ][, ret.plus.all.per := 2 + ret.per.cube + ret.all.cube]

cube.symbol.list <- ns.cost[, .(cube.symbol.list = list(unique(cube.symbol))), by = .(date.month)
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

ns.cost <- cube.symbol.list[ns.cost, on = .(date.month)]

ns.cost[, ev := {
    p1 <- cube.symbol.intersect %>% unlist() %>% unique()
    p2 <- cube.symbol.diff %>% unlist() %>% unique()
    ev.p1 <- .SD[cube.symbol %in% p1, sum(abs.ret.between.all.per)]
    ev.p2 <- .SD[cube.symbol %in% p2, sum(ret.plus.all.per)]
    n <- 50
    a <- (ev.p1 + ev.p2) * (n * (1 + unique(ret.all.cube))) ^ (-1)
    a
}, by = .(date.month)
][, cost := 0.0025 * (1 + ret.all.cube) * ev]

# 将cost带入monthly的数据中
ns.2.ret.month <- ns.cost[, unique(.SD), .SDcols = c('cost', 'date.month')
    ][ns.1.ret.month, on = .(date.month)
    ]

# 将cost带入daily的数据中
ns.2.ret.daily <- ns.cost[, unique(.SD), .SDcols = c('cost', 'date.month')
    ][ns.1.ret.daily, on = .(date.month)
    ]

## 2.1 Newey West t-test
library(lmtest)
# 月度Newey West t-test
ns.2.ret.month <- indexmn[, .(date.month = str_replace(Month, "-", ""), index_ret = Idxrtn)
    ][ns.2.ret.month, on = .(date.month), nomatch = 0
    ]
ns.2.ret.month[, ret.cube := ret.cube - cost
    ][, lm(ret.cube - index_ret ~ 1) %>% coeftest()]

# 日度Newey West t-test
ns.2.ret.daily <- index[, .(date, index_ret, index_value)
    ][ns.2.ret.daily, on = .(date), nomatch = 0]
ns.2.ret.daily[, tag := 1:.N, by = .(date.month)
    ][, ret.cube := fifelse(tag == 1, ret.cube - cost, ret.cube), by = .(date.month)
    ][, lm(ret.cube - index_ret ~ 1) %>% coeftest()]

# 2.2 考虑交易成本之后的回归
# 月度回归
ns.2.ret.month[, lm(I(ret.cube - rf)~ mkt_rf + smb + hml)] %>% summary()
ns.2.ret.month[, lm(I(ret.cube - rf) ~ mkt_rf + smb + hml + umd)] %>% summary()
ns.2.ret.month[, lm(I(ret.cube - rf) ~ mkt_rf + smb + hml + rmw + cma)] %>% summary()
ns.2.ret.month[, lm(I(ret.cube - rf) ~ mkt_rf + smb + hml + umd + rmw + cma)] %>% summary()

# 日度回归
ns.2.ret.daily[, lm(ret.cube - rf ~ mkt_rf + smb + hml)] %>% summary()
ns.2.ret.daily[, lm(ret.cube - rf ~ mkt_rf + smb + hml + umd)] %>% summary()
ns.2.ret.daily[, lm(ret.cube - rf ~ mkt_rf + smb + hml + rmw + cma)] %>% summary()
ns.2.ret.daily[, lm(ret.cube - rf ~ mkt_rf + smb + hml + umd + rmw + cma)] %>% summary()

# 择股择时能力
ns.1.ret.daily[, lm(ret.cube - rf ~ I(index_ret - rf) + I((index_ret - rf) ^ 2))] %>% summary()
ns.2.ret.daily[, lm(ret.cube - rf ~ I(index_ret - rf) + I((index_ret - rf) ^ 2))] %>% summary()

#### 日度交易成本----
#cost.lag.daily <- f.cube.ret.sp[date.month %in% ns.cube.lag$date.month & cube.symbol %in% ns.cube.lag$cube.symbol, .SD[.N], .SDcols = c('value', 'date'), by = .(cube.symbol, date.month)
    #][ns.1[!is.na(date.month.lag), .(date.month = date.month.lag, cube.symbol)], on = .(cube.symbol, date.month)
    #][, .(date.month.lag = date.month), by = .(cube.symbol, date, value)
    #][, date.month := shift(date.month.lag, type = "lead", n = 5L)
    #][is.na(date.month), date.month := "201806"
    #][, date.month.lag := NULL]

#cost.daily <- ns.1[, .(cube.symbol, date.month)
    #][f.cube.ret.sp, on = .(cube.symbol, date.month), nomatch = 0
    ##][, .SD[c(1, .N)], by = .(cube.symbol, date.month)
    #][order(date, cube.symbol), .SD
    ##][, tag := rep(1:2, times = 5), by = .(date.month)
    #][, ':='(label = NULL)]

#ns.cost.daily <- rbindlist(list(cost.lag.daily, cost.daily), use.names = T)
#ns.cost.daily <- ns.cost.daily[order(date.month, cube.symbol, date), .SD
    #][, ret.per.cube := value/ shift(value, type = "lag") - 1, by = .(cube.symbol, date.month) # 先计算每个cube单个的return
    #][order(date.month, date, cube.symbol), .SD
    #][, value.all.cube := mean(value), by = .(date.month, date) # 再计算所选择的cube整体的portfolio的return
    #][order(date.month, cube.symbol, date)
    #][, ret.all.cube := mean(ret.per.cube), by = .(date) # 计算所选择的cube每天的return
    ##][, .SD[(.N - 4):.N], by = .(date.month)
    #][, abs.ret.between.all.per := abs(ret.per.cube - ret.all.cube)
    #][, ret.plus.all.per := 2 + ret.per.cube + ret.all.cube]

### 计算每天的cube.symbol.list
#cube.symbol.list.daily <- ns.cost.daily[!is.na(ret.per.cube), .(cube.symbol.list = list(unique(cube.symbol))), by = .(date)
    #][, cube.symbol.intersect := {
        #a <- list()
        #for (i in 2:.N) {
            #a[[i]] <- intersect(cube.symbol.list[[i]], cube.symbol.list[[i - 1]])
        #}
        #a
    #}][, cube.symbol.diff := {
        #a <- list()
        #for (i in 1:.N) {
            #if (i == 1) {
                #a[[i]] <- cube.symbol.list[[i]]
            #} else {
                #a[[i]] <- setdiff(cube.symbol.list[[i]], cube.symbol.intersect[[i]])
            #}
        #}
        #a
    #}]

#ns.cost.daily <- cube.symbol.list.daily[ns.cost.daily[!is.na(ret.per.cube), .SD], on = .(date)]

#ns.cost.daily <- ns.cost.daily[order(date.month, date, cube.symbol), .SD
    #][, ev := {
    #p1 <- cube.symbol.intersect %>% unlist() %>% unique()
    #p2 <- cube.symbol.diff %>% unlist() %>% unique()
    #ev.p1 <- .SD[cube.symbol %in% p1, sum(abs.ret.between.all.per)]
    #ev.p2 <- .SD[cube.symbol %in% p2, sum(ret.plus.all.per)]
    #n <- 5
    #a <- (ev.p1 + ev.p2) * (n * (1 + unique(ret.all.cube))) ^ (-1)
    #a
#}, by = .(date, date.month)
#][, cost := 0.005 * (1 + ret.all.cube) * ev]

# 3. 表和图的导出----
# 3.1 Newey-West t-test
list(ns.1.ret.daily[, lm(ret.cube - index_ret ~ 1) %>% coeftest()], ns.2.ret.daily[, lm(ret.cube - index_ret ~ 1) %>% coeftest()]) %>% htmlreg(file = "NS.nw.html", custom.header = list("无交易成本" = 1, "考虑交易成本" = 2), digits = 5, custom.model.names = c("(1)", "(2)"))

# 3.2 因子回归
r1 <- ns.1.ret.daily[, lm(ret.cube - rf ~ mkt_rf + smb + hml)]
r2 <- ns.1.ret.daily[, lm(ret.cube - rf ~ mkt_rf + smb + hml + umd)]
r3 <- ns.1.ret.daily[, lm(ret.cube - rf ~ mkt_rf + smb + hml + rmw + cma)]
r4 <- ns.1.ret.daily[, lm(ret.cube - rf ~ mkt_rf + smb + hml + umd + rmw + cma)]

r5 <- ns.2.ret.daily[, lm(ret.cube - rf ~ mkt_rf + smb + hml)]
r6 <- ns.2.ret.daily[, lm(ret.cube - rf ~ mkt_rf + smb + hml + umd)]
r7 <- ns.2.ret.daily[, lm(ret.cube - rf ~ mkt_rf + smb + hml + rmw + cma)]
r8 <- ns.2.ret.daily[, lm(ret.cube - rf ~ mkt_rf + smb + hml + umd + rmw + cma)]

library(texreg)
list(r1, r2, r3, r4, r5, r6, r7, r8) %>%
    htmlreg(
        file = "ns.html",
        custom.header = list("无交易成本" = 1:4, "考虑交易成本" = 5:8),
#caption.above = TRUE,
        include.rs = TRUE,
        include.adjrs = FALSE,
        custom.model.names = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)"),
#custom.gof.rows = list(
#"Control Variables" = c("No", "Yes", "No", "Yes", "No", "Yes"),
#"Trader FE" = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
#"Quarter FE" = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")
#),
        custom.coef.names = c("alpha", "mkt_rf", "smb", "hml", "umd", "rmw", "cma"),
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

# 3.3 净值图
rs.pic <- index[date %in% ns.1.ret.daily$date, .(index_ret, date)
    ][ns.1.ret.daily[, .(ret.cube.no.fee = ret.cube, date)], on = .(date)
    ][ns.2.ret.daily[, .(ret.cube.with.fee = ret.cube, date)], on = .(date)
    ][, .(value.cube.with.fee = cumprod(ret.cube.with.fee + 1), value.cube.no.fee = cumprod(ret.cube.no.fee + 1), value.index = cumprod(index_ret + 1), date)
    ][, melt(.SD, id.vars = "date", value.name = "net.value")
    ][, variable := fcase(variable == "value.index", "沪深300", variable == "value.cube.no.fee", "Naive策略", variable == "value.cube.with.fee", "Naive策略（考虑交易成本）")
    ][, variable := factor(variable, levels = c("沪深300", "Naive策略", "Naive策略（考虑交易成本）"))]
ggplot(rs.pic, aes(x = date, y = net.value, colour = variable, fill = variable, order = variable)) +
                            geom_line(size = 1) +
                            theme_grey() +
                            scale_colour_manual(values = c("#9999CC", "#CC6666", "#66CC99")) +
                            labs(x = "交易日期", y = "净值") +
                            scale_fill_manual(values = c("#9999CC", "#CC6666", "#66CC99"),
                                                        name = "variable",
                                                        breaks = c("沪深300", "Naive策略", "Naive策略（考虑交易成本）"),
                                                        labels = c("沪深300", "Naive策略", "Naive策略（考虑交易成本）")) +
                                                        theme(
                                    axis.line = element_line(linetype = 1),
                                    legend.title = element_blank(),
                                    #panel.border = element_rect(linetype = 1, fill = NA),
                                    legend.position = "bottom",
                                    legend.spacing.x = unit(0.1, 'cm'),
                                    legend.spacing.y = unit(2, 'cm'),
                                    legend.box = "horizontal",
                                    legend.box.background = element_rect(size = 1, colour = "black", fill = "white"),
                                    legend.key = element_rect(size = 0.5, colour = "black", fill = "white"),
                                    legend.key.size = unit(0.5, 'cm')
                                    )
ggsave("NS.tiff", device = "tiff", dpi = 300, width = 6.67, height = 5)
fwrite(rs.pic, "ns.pic.csv")

# 4. 在市场形式下降的情况 ----
# 4.1 因子回归
r1 <- ns.1.ret.daily[date > as.Date("2018-01-01"), lm(ret.cube - rf ~ mkt_rf + smb + hml)]
r2 <- ns.1.ret.daily[date > as.Date("2018-01-01"), lm(ret.cube - rf ~ mkt_rf + smb + hml + umd)]
r3 <- ns.1.ret.daily[date > as.Date("2018-01-01"), lm(ret.cube - rf ~ mkt_rf + smb + hml + rmw + cma)]
r4 <- ns.1.ret.daily[date > as.Date("2018-01-01"), lm(ret.cube - rf ~ mkt_rf + smb + hml + umd + rmw + cma)]

r5 <- ns.2.ret.daily[date > as.Date("2018-01-01"), lm(ret.cube - rf ~ mkt_rf + smb + hml)]
r6 <- ns.2.ret.daily[date > as.Date("2018-01-01"), lm(ret.cube - rf ~ mkt_rf + smb + hml + umd)]
r7 <- ns.2.ret.daily[date > as.Date("2018-01-01"), lm(ret.cube - rf ~ mkt_rf + smb + hml + rmw + cma)]
r8 <- ns.2.ret.daily[date > as.Date("2018-01-01"), lm(ret.cube - rf ~ mkt_rf + smb + hml + umd + rmw + cma)]

# 4.2 净值曲线
rs.pic <- index[date > as.Date("2018-01-01"), .SD
    ][date %in% ns.1.ret.daily$date, .(index_ret, date)
    ][ns.1.ret.daily[date > as.Date("2018-01-01"), .(ret.cube.no.fee = ret.cube, date)], on = .(date)
    ][ns.2.ret.daily[date > as.Date("2018-01-01"), .(ret.cube.with.fee = ret.cube, date)], on = .(date)
    ][, .(value.cube.with.fee = cumprod(ret.cube.with.fee + 1), value.cube.no.fee = cumprod(ret.cube.no.fee + 1), value.index = cumprod(index_ret + 1), date)
    ][, melt(.SD, id.vars = "date", value.name = "net.value")
    ][, variable := fcase(variable == "value.index", "沪深300", variable == "value.cube.no.fee", "Naive策略", variable == "value.cube.with.fee", "Naive策略（考虑交易成本）")
    ][, variable := factor(variable, levels = c("沪深300", "Naive策略", "Naive策略（考虑交易成本）"))
    ]
ggplot(rs.pic, aes(x = date, y = net.value, colour = variable, fill = variable, order = variable)) +
                            geom_line(size = 1) +
                            theme_grey() +
                            scale_colour_manual(values = c("#9999CC", "#CC6666", "#66CC99")) +
                            labs(x = "交易日期", y = "净值") +
                            scale_fill_manual(values = c("#9999CC", "#CC6666", "#66CC99"),
                                                        name = "variable",
                                                        breaks = c("沪深300", "Naive策略", "Naive策略（考虑交易成本）"),
                                                        labels = c("沪深300", "Naive策略", "Naive策略（考虑交易成本）")) +
                                                        theme(
                                    axis.line = element_line(linetype = 1),
                                    legend.title = element_blank(),
#panel.border = element_rect(linetype = 1, fill = NA),
                                    legend.position = "bottom",
                                    legend.spacing.x = unit(0.1, 'cm'),
                                    legend.spacing.y = unit(2, 'cm'),
                                    legend.box = "horizontal",
                                    legend.box.background = element_rect(size = 1, colour = "black", fill = "white"),
                                    legend.key = element_rect(size = 0.5, colour = "black", fill = "white"),
                                    legend.key.size = unit(0.5, 'cm')
                                    )