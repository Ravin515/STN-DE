# ���������Ľ�����ҳ��ϸ�������������ǰ5��portfolio
library(styleer)
ld(f.cube.ret.sp, force = T)

# 0.1 ����������̼�����----
url <- str_c(getwd(), "/data/Clprc")
Clsprc <- fbread(path = url, pattern = "*.txt")
Clsprc[, stock.symbol := str_pad(Stkcd, 6, side = "left", pad = "0")
    ][, date := as.Date(Trddt)
    ][, ':='(file_id = NULL, Stkcd = NULL, Trddt = NULL)]

# 0.2 ����ָ���ļ�----
# �ն�ָ���ļ�
url <- str_c(getwd(), "/data/Index")
index <- fbread(path = url, pattern = "*\\d.txt")
index <- index[, file_id := NULL
    ][Indexcd == "000300"
    ][, ':='(index_ret = Idxtrd08 / 100, index_value = Idxtrd05, date = as.Date(Idxtrd01))]

# �¶�ָ���ļ�
indexmn <- fread(str_c(url, "/IDX_Idxtrdmth.txt"), encoding = "UTF-8")
indexmn <- indexmn[Indexcd == "000300"
    ][, setnames(.SD, "Month", "date.month")
    ][, setnames(.SD, "Idxrtn", "index_ret")
    ][, date.month := str_replace_all(date.month, "-", "")]

# 0.3 ���������ļ�---- 
## �¶�����
url <- str_c(getwd(), "/data/Factors/")
fivefactor_monthly <- fread(str_c(url, "fivefactor_monthly.csv"), encoding = "UTF-8")
fivefactor_monthly <- fivefactor_monthly[, trdmn := as.character(trdmn)
    ][, setnames(.SD, "trdmn", "date.month")
    ]
## �ն�����
url <- str_c(getwd(), "/data/Factors/")
fivefactor_daily <- fread(str_c(url, "fivefactor_daily.csv"), encoding = "UTF-8")
fivefactor_daily <- fivefactor_daily[, trddy := as.Date(trddy)
    ][, setnames(.SD, "trddy", "date")]

# 1.1 ѡ���ϸ�����������ߵ�50��portfolio (�����ǽ��׳ɱ�)----
outlier<- f.cube.ret.sp[order(cube.symbol, date), .SD
    ][, ret.daily := value / shift(value, type = "lag") - 1, by = .(cube.symbol)
    ][ret.daily > 0.1 | ret.daily < -0.1, unique(cube.symbol)]
f.cube.top5 <- f.cube.ret.sp[order(cube.symbol, date), .SD
    ][!(cube.symbol %in% outlier), .SD
    ][, date.month := str_sub(date, start = 1L, end = 7L)
    ][, ret.monthly := value[.N] / value[1] - 1, by = .(cube.symbol, date.month)
    ][order(date.month, - ret.monthly), .SD
    ][, unique(.SD), .SDcols = c("cube.symbol", "date.month", "ret.monthly")
    ][, .SD[1:50], by = .(date.month)
    ][date.month != "2018-07", .SD]

# ÿ��������Ҫ�����cube���б�
ts.1 <- f.cube.top5[, date.month.lag := shift(date.month, type = "lead", n = 50)
    ][, .(cube.symbol, date.month.lag)
    ][!is.na(date.month.lag), .SD
    ][, setnames(.SD, "date.month.lag", "date.month")
    ][f.cube.ret.sp[, .(cube.symbol, date, date.month = str_sub(date, start = 1L, end = 7L), value)], on = .(cube.symbol, date.month), nomatch = 0]

# ÿ�������蹺���cube���б����ϸ������һ���value
ts.1.lag <- f.cube.top5[, .(cube.symbol, date.month)
    ][f.cube.ret.sp[, .(cube.symbol, date, date.month = str_sub(date, start = 1L, end = 7L), value)], on = .(cube.symbol, date.month), nomatch = 0
    ][order(cube.symbol, date), .SD
    ][, .SD[.N], by = .(cube.symbol, date.month)
    ][, date := fifelse(date == max(date), date, max(date)), by = .(date.month)
    ][order(date.month, cube.symbol), .SD
    ][, date.month := shift(date.month, type = "lead", n = 50)
    ][!is.na(date.month), .SD]

# �ϲ�����value�����ݼ�
ts.1 <- rbindlist(list(ts.1, ts.1.lag), use.names = T)
ts.1.ptf <- ts.1[, .(value = mean(value, na.rm = T)), by = .(date.month, date)
    ][order(date.month, date), .SD]

# �¶�����
ts.1.monthly <- ts.1.ptf[, .(ret.monthly = value[.N] / value[1] - 1), by = .(date.month)
    ][, unique(.SD)
    ][, date.month := str_replace_all(date.month, "-", "")]
ts.1.monthly <- indexmn[, .(date.month, index_ret)
    ][ts.1.monthly, on = .(date.month)]
ts.1.monthly <- fivefactor_monthly[ts.1.monthly, on = .(date.month)]

# �ն�����
ts.1.daily <- ts.1.ptf[, .(ret.daily = value / shift(value, type = "lag") - 1, date, value), by = .(date.month)
    ][!is.na(ret.daily), .SD
    ][, unique(.SD)]
ts.1.daily <- index[, .(date, index_ret)
    ][ts.1.daily, on = .(date)]
ts.1.daily <- fivefactor_daily[ts.1.daily, on = .(date)]

# 1.1.1 Newey-west t-test
library(lmtest)

# �¶�����
ts.1.monthly[, lm(ret.monthly - index_ret ~ 1) %>% coeftest()]

# �ն�����
ts.1.daily[, lm(ret.daily - index_ret ~ 1) %>% coeftest()]

# 1.1.2 Alpha
# �¶�����
ts.1.monthly[, lm(ret.monthly - rf ~ mkt_rf + smb + hml)] %>% summary()
ts.1.monthly[, lm(ret.monthly - rf ~ mkt_rf + smb + hml + umd)] %>% summary()
ts.1.monthly[, lm(ret.monthly - rf ~ mkt_rf + smb + hml + rmw + cma)] %>% summary()
ts.1.monthly[, lm(ret.monthly - rf ~ mkt_rf + smb + hml + umd + rmw + cma)] %>% summary()

# �ն�����
ts.1.daily[, lm(ret.daily - rf ~ mkt_rf + smb + hml)] %>% summary()
ts.1.daily[, lm(ret.daily - rf ~ mkt_rf + smb + hml + umd)] %>% summary()
ts.1.daily[, lm(ret.daily - rf ~ mkt_rf + smb + hml + rmw + cma)] %>% summary()
ts.1.daily[, lm(ret.daily - rf ~ mkt_rf + smb + hml + umd + rmw + cma)] %>% summary()

# 1.2 ѡ���ϸ�����������ߵ�5��portfolio (���ǽ��׳ɱ�)----
### ���׳ɱ� ����Ϊѩ������ÿ��ľ�ֵ�����˽��׵������ѣ�ֻ�����ÿ�»��ֵĽ��׳ɱ���
ts.cost <- ts.1[order(date.month, cube.symbol, date), .SD
    ][!is.na(date), ret.per.cube := value[.N] / value[1] - 1, by = .(cube.symbol, date.month)
    ][order(date.month, date, cube.symbol), .SD
    ][, value.all.cube := mean(value), by = .(date.month, date)
    #][, ret.all.cube := .SD[date == max(date), unique(value.all.cube)] - .SD[date == min(date), unique(value.all.cube)], by = .(date.month)
    ][, ret.all.cube := mean(ret.per.cube), by = .(date.month)
    ][, .SD[(.N - 49):.N], by = .(date.month)
    ][, abs.ret.between.all.per := abs(ret.per.cube - ret.all.cube)
    ][, ret.plus.all.per := 2 + ret.per.cube + ret.all.cube]

cube.symbol.list <- ts.cost[, .(cube.symbol.list = list(unique(cube.symbol))), by = .(date.month)
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

ts.cost <- cube.symbol.list[ts.cost, on = .(date.month)]

ts.cost[, ev := {
    p1 <- cube.symbol.intersect %>% unlist() %>% unique()
    p2 <- cube.symbol.diff %>% unlist() %>% unique()
    ev.p1 <- .SD[cube.symbol %in% p1, sum(abs.ret.between.all.per)]
    ev.p2 <- .SD[cube.symbol %in% p2, sum(ret.plus.all.per)]
    n <- 50
    a <- (ev.p1 + ev.p2) * (n * (1 + unique(ret.all.cube))) ^ (-1)
    a
}, by = .(date.month)
][, date.month := str_replace_all(date.month, "-", "")
][, cost := 0.0025 * (1 + ret.all.cube) * ev]

# ��cost����monthly��������
ts.2.monthly <- ts.cost[, unique(.SD), .SDcols = c('cost', 'date.month')
    ][ts.1.monthly, on = .(date.month)
    ]

# ��cost����daily��������
ts.2.daily <- ts.cost[, unique(.SD), .SDcols = c('cost', 'date.month')
    ][ts.1.daily[, date.month := str_replace(date.month, "-", "")], on = .(date.month)
    ]

## 2.1.1 Newey West t-test
# �¶�Newey West t-test
ts.2.monthly[, ret.cube := ret.monthly - cost
    ][, lm(ret.cube - index_ret ~ 1) %>% coeftest()]

# �ն�Newey West t-test
ts.2.daily[, tag := 1:.N, by = .(date.month)
    ][, ret.cube := fifelse(tag == 1, ret.daily - cost, ret.daily), by = .(date.month)
    ][, lm(ret.cube - index_ret ~ 1) %>% coeftest()]

## 2.2.2 Alpha
# �¶Ȼع�
ts.2.monthly[, lm(I(ret.cube - rf) ~ mkt_rf + smb + hml)] %>% summary()
ts.2.monthly[, lm(I(ret.cube - rf) ~ mkt_rf + smb + hml + umd)] %>% summary()
ts.2.monthly[, lm(I(ret.cube - rf) ~ mkt_rf + smb + hml + rmw + cma)] %>% summary()
ts.2.monthly[, lm(I(ret.cube - rf) ~ mkt_rf + smb + hml + umd + rmw + cma)] %>% summary()

# �նȻع�
ts.2.daily[, lm(ret.cube - rf ~ mkt_rf + smb + hml)] %>% summary()
ts.2.daily[, lm(ret.cube - rf ~ mkt_rf + smb + hml + umd)] %>% summary()
ts.2.daily[, lm(ret.cube - rf ~ mkt_rf + smb + hml + rmw + cma)] %>% summary()
ts.2.daily[, lm(ret.cube - rf ~ mkt_rf + smb + hml + umd + rmw + cma)] %>% summary()

# ��ɺ���ʱ����
ts.1.daily[, lm(ret.daily - rf ~ I(index_ret - rf) + I((index_ret - rf) ^ 2))] %>% summary()
ts.2.daily[, lm(ret.cube- rf ~ I(index_ret - rf) + I((index_ret - rf) ^ 2))] %>% summary()

# 3. ����ͼ�ĵ���
# 3.1 Newey-West t-test
list(ts.1.daily[, lm(ret.daily - index_ret ~ 1) %>% coeftest()], ts.2.daily[, lm(ret.cube - index_ret ~ 1) %>% coeftest()]) %>% htmlreg(file = "TS.nw.html", custom.header = list("�޽��׳ɱ�" = 1, "���ǽ��׳ɱ�" = 2), digits = 4, custom.model.names = c("(1)", "(2)"))

# 3.2 ���ӻع�
r1 <- ts.1.daily[, lm(ret.daily - rf ~ mkt_rf + smb + hml)]
r2 <- ts.1.daily[, lm(ret.daily - rf ~ mkt_rf + smb + hml + umd)]
r3 <- ts.1.daily[, lm(ret.daily - rf ~ mkt_rf + smb + hml + rmw + cma)]
r4 <- ts.1.daily[, lm(ret.daily - rf ~ mkt_rf + smb + hml + umd + rmw + cma)]

r5 <- ts.2.daily[, lm(ret.cube - rf ~ mkt_rf + smb + hml)]
r6 <- ts.2.daily[, lm(ret.cube - rf ~ mkt_rf + smb + hml + umd)]
r7 <- ts.2.daily[, lm(ret.cube - rf ~ mkt_rf + smb + hml + rmw + cma)]
r8 <- ts.2.daily[, lm(ret.cube - rf ~ mkt_rf + smb + hml + umd + rmw + cma)]

library(texreg)
list(r1, r2, r3, r4, r5, r6, r7, r8) %>%
    htmlreg(
        file = "ts.html",
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
rs.pic <- index[date %in% ts.1.daily$date, .(index_ret, date)
    ][ts.1.daily[, .(ret.cube.no.fee = ret.daily, date)], on = .(date)
    ][ts.2.daily[, .(ret.cube.with.fee = ret.cube, date)], on = .(date)
    ][, .(value.cube.with.fee = cumprod(ret.cube.with.fee + 1), value.cube.no.fee = cumprod(ret.cube.no.fee + 1), value.index = cumprod(index_ret + 1), date)
    ][, melt(.SD, id.vars = "date", value.name = "net.value")
    ][, variable := fcase(variable == "value.index", "����300", variable == "value.cube.no.fee", "�����ʹ������в���", variable == "value.cube.with.fee", "�����ʹ������в��ԣ����ǽ��׳ɱ���")
    ][, variable := factor(variable, levels = c("����300", "�����ʹ������в���", "�����ʹ������в��ԣ����ǽ��׳ɱ���"))]
ggplot(rs.pic, aes(x = date, y = net.value, colour = variable, fill = variable)) +
                            geom_line(size = 1) +
                            theme_grey() +
                            scale_colour_manual(values = c("#9999CC", "#CC6666", "#66CC99")) +
                            labs(x = "��������", y = "��ֵ") +
                            scale_fill_manual(values = c("#9999CC", "#CC6666", "#66CC99"),
                                                        name = "Stage",
                                                        breaks = c("����300", "�����ʹ������в���", "�����ʹ������в��ԣ����ǽ��׳ɱ���"),
                                                        labels = c("����300", "�����ʹ������в���", "�����ʹ������в��ԣ����ǽ��׳ɱ���")) +
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
ggsave("TS.tiff", device = "tiff", dpi = 300, width = 6.67, height = 5)
fwrite(rs.pic, "ts.pic.csv")

# 4. �г���ʽ�½������----
# 4.1 ���ӻع�
r1 <- ts.1.daily[date > as.Date("2018-01-01"), lm(ret.daily - rf ~ mkt_rf + smb + hml)]
r2 <- ts.1.daily[date > as.Date("2018-01-01"), lm(ret.daily - rf ~ mkt_rf + smb + hml + umd)]
r3 <- ts.1.daily[date > as.Date("2018-01-01"), lm(ret.daily - rf ~ mkt_rf + smb + hml + rmw + cma)]
r4 <- ts.1.daily[date > as.Date("2018-01-01"), lm(ret.daily - rf ~ mkt_rf + smb + hml + umd + rmw + cma)]

r5 <- ts.2.daily[date > as.Date("2018-01-01"), lm(ret.cube - rf ~ mkt_rf + smb + hml)]
r6 <- ts.2.daily[date > as.Date("2018-01-01"), lm(ret.cube - rf ~ mkt_rf + smb + hml + umd)]
r7 <- ts.2.daily[date > as.Date("2018-01-01"), lm(ret.cube - rf ~ mkt_rf + smb + hml + rmw + cma)]
r8 <- ts.2.daily[date > as.Date("2018-01-01"), lm(ret.cube - rf ~ mkt_rf + smb + hml + umd + rmw + cma)]

# 4.2 ��ֵ�Ա�ͼ
rs.pic <- index[date > as.Date("2018-01-01"), .SD
    ][date %in% ts.1.daily$date, .(index_ret, date)
    ][ts.1.daily[date > as.Date("2018-01-01"), .(ret.cube.no.fee = ret.daily, date)], on = .(date)
    ][ts.2.daily[date > as.Date("2018-01-01"), .(ret.cube.with.fee = ret.cube, date)], on = .(date)
    ][, .(value.cube.with.fee = cumprod(ret.cube.with.fee + 1), value.cube.no.fee = cumprod(ret.cube.no.fee + 1), value.index = cumprod(index_ret + 1), date)
    ][, melt(.SD, id.vars = "date", value.name = "net.value")
    ][, variable := fcase(variable == "value.index", "����300", variable == "value.cube.no.fee", "�����ʹ������в���", variable == "value.cube.with.fee", "�����ʹ������в��ԣ����ǽ��׳ɱ���")
    ][, variable := factor(variable, levels = c("����300", "�����ʹ������в���", "�����ʹ������в��ԣ����ǽ��׳ɱ���"))]
ggplot(rs.pic, aes(x = date, y = net.value, colour = variable, fill = variable)) +
                            geom_line(size = 1) +
                            theme_grey() +
                            scale_colour_manual(values = c("#9999CC", "#CC6666", "#66CC99")) +
                            labs(x = "��������", y = "��ֵ") +
                            scale_fill_manual(values = c("#9999CC", "#CC6666", "#66CC99"),
                                                        name = "Stage",
                                                        breaks = c("����300", "�����ʹ������в���", "�����ʹ������в��ԣ����ǽ��׳ɱ���"),
                                                        labels = c("����300", "�����ʹ������в���", "�����ʹ������в��ԣ����ǽ��׳ɱ���")) +
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