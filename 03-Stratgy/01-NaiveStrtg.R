library(styleer)

#  ����naive strategy�������ʣ�û�н��׳ɱ�----
ld(f.cube.ret.sp)
f.cube.ret.sp <- f.cube.ret.sp[, date.month := as.character(date) %>% str_sub(start = 1L, end = 7L) %>% str_replace_all("-", "")
    ][order(cube.symbol, date, date.month), .SD]

# Naive strategy 1: ÿ�����³�����ѡ����publication����ret��ߵ����cube, �����ڽ�������һ���¶����رգ���ͬ�������� ----
ns.1 <- f.cube.ret.sp[, .(ret.ns.1 = value / value[1] - 1, ret = value/ shift(value, type = "lag") - 1, value, date, date.month), by = .(cube.symbol)
    ][, tag := fifelse(ret >= 0.1|ret <= -0.1, 1, 0)
    ][, tag := sum(tag, na.rm = T), by = .(cube.symbol)
    ][, ret.num.monthly := .N, by = .(cube.symbol, date.month)
    ][!(date.month %in% c("201606", "201604", "201807")) & tag == 0, .SD[1], by = .(cube.symbol, date.month)
    ][order(date.month, - ret.num.monthly, - ret.ns.1), .SD
    ][, .SD[1:50], by = .(date.month)
    ][, date.month.lag := shift(date.month, type = "lag", n = 50L)] # Ϊ֮����㽻�׳ɱ����ն�����ÿ�����³�����������׼��

# ���ÿ����ѡ�е�������ϸ������һ�յ�value
ns.cube.lag <- ns.1[!is.na(date.month.lag), .(cube.symbol, date.month.lag)]
ns.value.cube.lag <- f.cube.ret.sp[date.month %in% ns.cube.lag$date.month & cube.symbol %in% ns.cube.lag$cube.symbol, .SD[.N], .SDcols = c('value', 'date'), by = .(cube.symbol, date.month)
    ][ns.1[!is.na(date.month.lag), .(date.month = date.month.lag, cube.symbol)], on = .(cube.symbol, date.month)
    ][, .(date.month.lag = date.month), by = .(cube.symbol, date, value)
    ][, date.month := shift(date.month.lag, type = "lead", n = 50L)
    ][is.na(date.month), date.month := "201806"
    ][, .(value.cube = mean(value), date), by = .(date.month)
    ][, unique(.SD)]

## �¶�����
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

## �ն����ݣ���Ҫ��ÿ����ѡ����cube��������ϸ������һ�쵽����µ�һ��ѡ����cube��ϵ�������

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
# ����ָ���ļ�
# �ն�ָ���ļ�
url <- str_c(getwd(), "/data/Index")
index <- fbread(path = url, pattern = "*\\d.txt")
index <- index[, file_id := NULL
    ][Indexcd == "000300"
    ][, ':='(index_ret = Idxtrd08 / 100, index_value = Idxtrd05, date = as.Date(Idxtrd01))]

# �¶�ָ���ļ�
indexmn <- fread(str_c(url, "/IDX_Idxtrdmth.txt"), encoding = "UTF-8")
indexmn <- indexmn[Indexcd == "000300"]

library(lmtest)
# �¶�Newey West t-test
ns.1.ret.month <- indexmn[, .(date.month = str_replace(Month, "-", ""), index_ret = Idxrtn)
    ][ns.1.ret.month, on = .(date.month), nomatch = 0]
ns.1.ret.month[, lm(ret.cube - index_ret ~ 1) %>% coeftest()]

# �ն�Newey West t-test
ns.1.ret.daily <- index[, .(date, index_ret, index_value)
    ][ns.1.ret.daily, on = .(date), nomatch = 0]
ns.1.ret.daily[, lm(ret.cube - index_ret ~ 1) %>% coeftest()]

## 1.2 Alpha�ع�
## �¶�����
url <- str_c(getwd(), "/data/Factors/")
fivefactor_monthly <- fread(str_c(url, "fivefactor_monthly.csv"), encoding = "UTF-8")

ns.1.ret.month <- fivefactor_monthly[, setnames(.SD, "trdmn", "date.month")
    ][, date.month := as.character(date.month)
    ][ns.1.ret.month, on = .(date.month), nomatch = 0]

## �¶Ȼع�
ns.1.ret.month[, lm(ret.cube - rf ~ mkt_rf + smb + hml)] %>% summary()
ns.1.ret.month[, lm(ret.cube - rf ~ mkt_rf + smb + hml + umd)] %>% summary()
ns.1.ret.month[, lm(ret.cube - rf ~ mkt_rf + smb + hml + rmw + cma)] %>% summary()
ns.1.ret.month[, lm(ret.cube - rf ~ mkt_rf + smb + hml + umd + rmw + cma)] %>% summary()

## �ն�����
url <- str_c(getwd(), "/data/Factors/")
fivefactor_daily <- fread(str_c(url, "fivefactor_daily.csv"), encoding = "UTF-8")

ns.1.ret.daily <- fivefactor_daily[, setnames(.SD, "trddy", "date")
    ][, date := as.Date(date)
    ][ns.1.ret.daily, on = .(date), nomatch = 0]

## �նȻع�
ns.1.ret.daily[, lm(ret.cube - rf ~ mkt_rf + smb + hml)] %>% summary()
ns.1.ret.daily[, lm(ret.cube - rf ~ mkt_rf + smb + hml + umd)] %>% summary()
ns.1.ret.daily[, lm(ret.cube - rf ~ mkt_rf + smb + hml + rmw + cma)] %>% summary()
ns.1.ret.daily[, lm(ret.cube - rf ~ mkt_rf + smb + hml + umd + rmw + cma)] %>% summary()


# Naive Strategy 2�����뽻�׳ɱ���Naive Strategy 1----
## ���׳ɱ��ļ���

### ���׳ɱ� ����Ϊѩ������ÿ��ľ�ֵ�����˽��׵������ѣ�ֻ�����ÿ�»��ֵĽ��׳ɱ���
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

# ��cost����monthly��������
ns.2.ret.month <- ns.cost[, unique(.SD), .SDcols = c('cost', 'date.month')
    ][ns.1.ret.month, on = .(date.month)
    ]

# ��cost����daily��������
ns.2.ret.daily <- ns.cost[, unique(.SD), .SDcols = c('cost', 'date.month')
    ][ns.1.ret.daily, on = .(date.month)
    ]

## 2.1 Newey West t-test
library(lmtest)
# �¶�Newey West t-test
ns.2.ret.month <- indexmn[, .(date.month = str_replace(Month, "-", ""), index_ret = Idxrtn)
    ][ns.2.ret.month, on = .(date.month), nomatch = 0
    ]
ns.2.ret.month[, ret.cube := ret.cube - cost
    ][, lm(ret.cube - index_ret ~ 1) %>% coeftest()]

# �ն�Newey West t-test
ns.2.ret.daily <- index[, .(date, index_ret, index_value)
    ][ns.2.ret.daily, on = .(date), nomatch = 0]
ns.2.ret.daily[, tag := 1:.N, by = .(date.month)
    ][, ret.cube := fifelse(tag == 1, ret.cube - cost, ret.cube), by = .(date.month)
    ][, lm(ret.cube - index_ret ~ 1) %>% coeftest()]

# 2.2 ���ǽ��׳ɱ�֮��Ļع�
# �¶Ȼع�
ns.2.ret.month[, lm(I(ret.cube - rf)~ mkt_rf + smb + hml)] %>% summary()
ns.2.ret.month[, lm(I(ret.cube - rf) ~ mkt_rf + smb + hml + umd)] %>% summary()
ns.2.ret.month[, lm(I(ret.cube - rf) ~ mkt_rf + smb + hml + rmw + cma)] %>% summary()
ns.2.ret.month[, lm(I(ret.cube - rf) ~ mkt_rf + smb + hml + umd + rmw + cma)] %>% summary()

# �նȻع�
ns.2.ret.daily[, lm(ret.cube - rf ~ mkt_rf + smb + hml)] %>% summary()
ns.2.ret.daily[, lm(ret.cube - rf ~ mkt_rf + smb + hml + umd)] %>% summary()
ns.2.ret.daily[, lm(ret.cube - rf ~ mkt_rf + smb + hml + rmw + cma)] %>% summary()
ns.2.ret.daily[, lm(ret.cube - rf ~ mkt_rf + smb + hml + umd + rmw + cma)] %>% summary()

# �����ʱ����
ns.1.ret.daily[, lm(ret.cube - rf ~ I(index_ret - rf) + I((index_ret - rf) ^ 2))] %>% summary()
ns.2.ret.daily[, lm(ret.cube - rf ~ I(index_ret - rf) + I((index_ret - rf) ^ 2))] %>% summary()

#### �նȽ��׳ɱ�----
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
    #][, ret.per.cube := value/ shift(value, type = "lag") - 1, by = .(cube.symbol, date.month) # �ȼ���ÿ��cube������return
    #][order(date.month, date, cube.symbol), .SD
    #][, value.all.cube := mean(value), by = .(date.month, date) # �ټ�����ѡ���cube�����portfolio��return
    #][order(date.month, cube.symbol, date)
    #][, ret.all.cube := mean(ret.per.cube), by = .(date) # ������ѡ���cubeÿ���return
    ##][, .SD[(.N - 4):.N], by = .(date.month)
    #][, abs.ret.between.all.per := abs(ret.per.cube - ret.all.cube)
    #][, ret.plus.all.per := 2 + ret.per.cube + ret.all.cube]

### ����ÿ���cube.symbol.list
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

# 3. ����ͼ�ĵ���----
# 3.1 Newey-West t-test
list(ns.1.ret.daily[, lm(ret.cube - index_ret ~ 1) %>% coeftest()], ns.2.ret.daily[, lm(ret.cube - index_ret ~ 1) %>% coeftest()]) %>% htmlreg(file = "NS.nw.html", custom.header = list("�޽��׳ɱ�" = 1, "���ǽ��׳ɱ�" = 2), digits = 5, custom.model.names = c("(1)", "(2)"))

# 3.2 ���ӻع�
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
        custom.header = list("�޽��׳ɱ�" = 1:4, "���ǽ��׳ɱ�" = 5:8),
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

# 3.3 ��ֵͼ
rs.pic <- index[date %in% ns.1.ret.daily$date, .(index_ret, date)
    ][ns.1.ret.daily[, .(ret.cube.no.fee = ret.cube, date)], on = .(date)
    ][ns.2.ret.daily[, .(ret.cube.with.fee = ret.cube, date)], on = .(date)
    ][, .(value.cube.with.fee = cumprod(ret.cube.with.fee + 1), value.cube.no.fee = cumprod(ret.cube.no.fee + 1), value.index = cumprod(index_ret + 1), date)
    ][, melt(.SD, id.vars = "date", value.name = "net.value")
    ][, variable := fcase(variable == "value.index", "����300", variable == "value.cube.no.fee", "Naive����", variable == "value.cube.with.fee", "Naive���ԣ����ǽ��׳ɱ���")
    ][, variable := factor(variable, levels = c("����300", "Naive����", "Naive���ԣ����ǽ��׳ɱ���"))]
ggplot(rs.pic, aes(x = date, y = net.value, colour = variable, fill = variable, order = variable)) +
                            geom_line(size = 1) +
                            theme_grey() +
                            scale_colour_manual(values = c("#9999CC", "#CC6666", "#66CC99")) +
                            labs(x = "��������", y = "��ֵ") +
                            scale_fill_manual(values = c("#9999CC", "#CC6666", "#66CC99"),
                                                        name = "variable",
                                                        breaks = c("����300", "Naive����", "Naive���ԣ����ǽ��׳ɱ���"),
                                                        labels = c("����300", "Naive����", "Naive���ԣ����ǽ��׳ɱ���")) +
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

# 4. ���г���ʽ�½������ ----
# 4.1 ���ӻع�
r1 <- ns.1.ret.daily[date > as.Date("2018-01-01"), lm(ret.cube - rf ~ mkt_rf + smb + hml)]
r2 <- ns.1.ret.daily[date > as.Date("2018-01-01"), lm(ret.cube - rf ~ mkt_rf + smb + hml + umd)]
r3 <- ns.1.ret.daily[date > as.Date("2018-01-01"), lm(ret.cube - rf ~ mkt_rf + smb + hml + rmw + cma)]
r4 <- ns.1.ret.daily[date > as.Date("2018-01-01"), lm(ret.cube - rf ~ mkt_rf + smb + hml + umd + rmw + cma)]

r5 <- ns.2.ret.daily[date > as.Date("2018-01-01"), lm(ret.cube - rf ~ mkt_rf + smb + hml)]
r6 <- ns.2.ret.daily[date > as.Date("2018-01-01"), lm(ret.cube - rf ~ mkt_rf + smb + hml + umd)]
r7 <- ns.2.ret.daily[date > as.Date("2018-01-01"), lm(ret.cube - rf ~ mkt_rf + smb + hml + rmw + cma)]
r8 <- ns.2.ret.daily[date > as.Date("2018-01-01"), lm(ret.cube - rf ~ mkt_rf + smb + hml + umd + rmw + cma)]

# 4.2 ��ֵ����
rs.pic <- index[date > as.Date("2018-01-01"), .SD
    ][date %in% ns.1.ret.daily$date, .(index_ret, date)
    ][ns.1.ret.daily[date > as.Date("2018-01-01"), .(ret.cube.no.fee = ret.cube, date)], on = .(date)
    ][ns.2.ret.daily[date > as.Date("2018-01-01"), .(ret.cube.with.fee = ret.cube, date)], on = .(date)
    ][, .(value.cube.with.fee = cumprod(ret.cube.with.fee + 1), value.cube.no.fee = cumprod(ret.cube.no.fee + 1), value.index = cumprod(index_ret + 1), date)
    ][, melt(.SD, id.vars = "date", value.name = "net.value")
    ][, variable := fcase(variable == "value.index", "����300", variable == "value.cube.no.fee", "Naive����", variable == "value.cube.with.fee", "Naive���ԣ����ǽ��׳ɱ���")
    ][, variable := factor(variable, levels = c("����300", "Naive����", "Naive���ԣ����ǽ��׳ɱ���"))
    ]
ggplot(rs.pic, aes(x = date, y = net.value, colour = variable, fill = variable, order = variable)) +
                            geom_line(size = 1) +
                            theme_grey() +
                            scale_colour_manual(values = c("#9999CC", "#CC6666", "#66CC99")) +
                            labs(x = "��������", y = "��ֵ") +
                            scale_fill_manual(values = c("#9999CC", "#CC6666", "#66CC99"),
                                                        name = "variable",
                                                        breaks = c("����300", "Naive����", "Naive���ԣ����ǽ��׳ɱ���"),
                                                        labels = c("����300", "Naive����", "Naive���ԣ����ǽ��׳ɱ���")) +
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