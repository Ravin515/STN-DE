library(styleer)
library(lfe)
library(quantreg)
library(rqpd)

ld(f.main1)
ld(f.main2)

# 所有cube的第一个和最后一个交易日的截面净值分布
f.main.end <- f.main1[, .SD[.N], by = .(cube.symbol)]
f.main.start <- f.main1[, .SD[60], by = .(cube.symbol)]
ggplot(, aes(x = value)) +
    geom_line(f.main.end[value > 0 & value < 4], mapping = aes(x = value), stat = "density", color = "blue") +
    geom_line(f.main.start[value > 0 & value < 4], mapping = aes(x = value), stat = "density", color = "red") +
    scale_x_continuous(breaks =c(-1, 0:10))

ggplot() +
    geom_line(f.main1[date == as.Date("2017-01-03") & (value > -1 & value < 4)], mapping = aes(x = value), stat = "density", color = "black") +
    geom_line(f.main1[date == as.Date("2017-03-03") & (value > -1 & value < 4)], mapping = aes(x = value), stat = "density", color = "red") +
    geom_line(f.main1[date == as.Date("2017-06-01") & (value > -1 & value < 4)], mapping = aes(x = value), stat = "density", color = "blue") +
    geom_line(f.main1[date == as.Date("2017-09-01") & (value > -1 & value < 4)], mapping = aes(x = value), stat = "density", color = "yellow") +
    geom_line(f.main1[date == as.Date("2017-12-01") & (value > -1 & value < 4)], mapping = aes(x = value), stat = "density", color = "grey") +
    geom_line(f.main1[date == as.Date("2018-03-01") & (value > -1 & value < 4)], mapping = aes(x = value), stat = "density", color = "green") +
    scale_x_continuous(breaks = c(-1, 0:10))

# 收益率的图
## cube在follow 前后的对比

ggplot() +
    geom_line(f.main1[date - follow.date == -150, .SD, by = .(cube.symbol)][ret > -0.125 & ret < 0.125], mapping = aes(x = ret), stat = "density", color = "blue") +
    geom_line(f.main1[date - follow.date == 150, .SD, by = .(cube.symbol)][ret > -0.125 & ret < 0.125], mapping = aes(x = ret), stat = "density", color = "red") +
    scale_x_continuous(breaks = c(-0.5, -0.25, 0, 0.25, 0.5))

ggplot() +
    geom_line(f.main1[date - follow.date == -120, .SD, by = .(cube.symbol)][ret > -0.125 & ret < 0.125], mapping = aes(x = ret), stat = "density", color = "blue") +
    geom_line(f.main1[date - follow.date == 120, .SD, by = .(cube.symbol)][ret > -0.125 & ret < 0.125], mapping = aes(x = ret), stat = "density", color = "red") +
    scale_x_continuous(breaks = c(-0.5, -0.25, 0, 0.25, 0.5))

ggplot() +
    geom_line(f.main1[date - follow.date == -90, .SD, by = .(cube.symbol)][ret > -0.125 & ret < 0.125], mapping = aes(x = ret), stat = "density", color = "blue") +
    geom_line(f.main1[date - follow.date == 90, .SD, by = .(cube.symbol)][ret > -0.125 & ret < 0.125], mapping = aes(x = ret), stat = "density", color = "red") +
    scale_x_continuous(breaks = c(-0.5, -0.25, 0, 0.25, 0.5))

ggplot() +
    geom_line(f.main1[date - follow.date == -60, .SD, by = .(cube.symbol)][ret > -0.125 & ret < 0.125], mapping = aes(x = ret), stat = "density", color = "blue") +
    geom_line(f.main1[date - follow.date == 60, .SD, by = .(cube.symbol)][ret > -0.125 & ret < 0.125], mapping = aes(x = ret), stat = "density", color = "red") +
    scale_x_continuous(breaks = c(-0.5, -0.25, 0, 0.25, 0.5))

ggplot() +
    geom_line(f.main1[date - follow.date == -30, .SD, by = .(cube.symbol)][ret > -0.125 & ret < 0.125], mapping = aes(x = ret), stat = "density", color = "blue") +
    geom_line(f.main1[date - follow.date == 30, .SD, by = .(cube.symbol)][ret > -0.125 & ret < 0.125], mapping = aes(x = ret), stat = "density", color = "red") +
    scale_x_continuous(breaks = c(-0.5, -0.25, 0, 0.25, 0.5))

ggplot() +
    geom_line(f.main1[date - follow.date == -5, .SD, by = .(cube.symbol)][ret > -0.125 & ret < 0.125], mapping = aes(x = ret), stat = "density", color = "blue") +
    geom_line(f.main1[date - follow.date == 5, .SD, by = .(cube.symbol)][ret > -0.125 & ret < 0.125], mapping = aes(x = ret), stat = "density", color = "red") +
    scale_x_continuous(breaks = c(-0.5, -0.25, 0, 0.25, 0.5))

## 实盘整体情况
ggplot() +
    geom_line(f.main1[date == as.Date("2017-01-03") & (ret > -1 & ret < 1)], mapping = aes(x = ret), stat = "density", color = "blue") +
    geom_line(f.main1[date == as.Date("2017-06-01") & (ret > -1 & ret < 1)], mapping = aes(x = ret), stat = "density", color = "red") +
    geom_line(f.main1[date == as.Date("2018-03-01") & (ret > -1 & ret < 1)], mapping = aes(x = ret), stat = "density", color = "green") +
    scale_x_continuous(breaks = c(-0.5, -0.25, 0, 0.25, 0.5))

# 均值DID, Drawing plots
did.daily <- f.main1.day[post.follow == 0 | (post.follow == 1 & (date - follow.date < pre.period)), .(aver.ret = mean(ret, na.rm = T)), by = .(cube.symbol, post.follow)
    ][, tag := 1
    ][, tag := sum(tag), by = .(cube.symbol)
    ][tag == 2
    ][, stage := ifelse(post.follow == 0, "pre-follow", "post-follow")]
did.daily[, t.test(aver.ret ~ post.follow)]

did.weekly <- f.main1.week[post.follow == 0 | (post.follow == 1 & (date - follow.date < pre.period)), .(aver.ret = mean(ret_week, na.rm = T)), by = .(cube.symbol, post.follow)
    ][, tag := 1
    ][, tag := sum(tag), by = .(cube.symbol)
    ][tag == 2
    ][, stage := ifelse(post.follow == 0, "pre-follow", "post-follow")]
did.weekly[, t.test(aver.ret ~ post.follow)]

did.monthly <- f.main1.month[post.follow == 0 | (post.follow == 1 & (date - follow.date < pre.period)), .(aver.ret = mean(ret_month, na.rm = T)), by = .(cube.symbol, post.follow)
    ][, tag := 1
    ][, tag := sum(tag), by = .(cube.symbol)
    ][tag == 2
    ][, stage := ifelse(post.follow == 0, "pre-follow", "post-follow")]
did.monthly[, t.test(aver.ret ~ post.follow)]

d.ttest <- ggplot(did.daily[aver.ret < 0.05 & aver.ret > -0.05], aes(x = aver.ret, colour = stage, fill = stage)) +
                            geom_line(stat = "density", size = 0.5) +
                            theme_grey() +
                            scale_colour_manual(values = c("#CC6666", "#7777DD")) +
                            labs(x = "Return", y = "Density") +
                            scale_fill_manual(values = c("#CC6666", "#7777DD"),
                                                        name = "Stage",
                                                        breaks = c("post.follow", "pre.follow"),
                                                        labels = c("Post-follow", "Pre-follow")) +
                                                        theme(
                                    legend.title = element_blank(),
                                    panel.border = element_rect(linetype = 1, fill = NA),
                                    legend.position = "bottom",
                                    legend.spacing.x = unit(0.1, 'cm'),
                                    legend.spacing.y = unit(2, 'cm'),
                                    legend.box = "horizontal",
                                    legend.box.background = element_rect(size = 1, colour = "black", fill = "white"),
                                    legend.key = element_rect(size = 0.5, colour = "black", fill = "white"),
                                    legend.key.size = unit(0.5, 'cm')
                                    )
ggsave("Fig1-1.tiff", device = "tiff", dpi = 300, width = 5.5, height = 4)

d.ttest <- ggplot(did.weekly[aver.ret < 0.2 & aver.ret > -0.2], aes(x = aver.ret, colour = stage, fill = stage)) +
                            geom_line(stat = "density", size = 0.5) +
                            theme_grey() +
                            scale_colour_manual(values = c("#CC6666", "#7777DD")) +
                            labs(x = "Return", y = "Density") +
                            scale_fill_manual(values = c("#CC6666", "#7777DD"),
                                                        name = "Stage",
                                                        breaks = c("post.follow", "pre.follow"),
                                                        labels = c("Post-follow", "Pre-follow")) +
                                                        theme(
                                    legend.title = element_blank(),
                                    panel.border = element_rect(linetype = 1, fill = NA),
                                    legend.position = "bottom",
                                    legend.spacing.x = unit(0.1, 'cm'),
                                    legend.spacing.y = unit(2, 'cm'),
                                    legend.box = "horizontal",
                                    legend.box.background = element_rect(size = 1, colour = "black", fill = "white"),
                                    legend.key = element_rect(size = 0.5, colour = "black", fill = "white"),
                                    legend.key.size = unit(0.5, 'cm')
                                    )
ggsave("Fig1-2.tiff", device = "tiff", dpi = 300, width = 5.5, height = 4)

d.ttest <- ggplot(did.monthly[aver.ret < 0.5 & aver.ret > -0.5], aes(x = aver.ret, colour = stage, fill = stage)) +
                            geom_line(stat = "density", size = 0.5) +
                            theme_grey() +
                            scale_colour_manual(values = c("#CC6666", "#7777DD")) +
                            labs(x = "Return", y = "Density") +
                            scale_fill_manual(values = c("#CC6666", "#7777DD"),
                                                        name = "Stage",
                                                        breaks = c("post.follow", "pre.follow"),
                                                        labels = c("Post-follow", "Pre-follow")) +
                                                        theme(
                                    legend.title = element_blank(),
                                    panel.border = element_rect(linetype = 1, fill = NA),
                                    legend.position = "bottom",
                                    legend.spacing.x = unit(0.1, 'cm'),
                                    legend.spacing.y = unit(2, 'cm'),
                                    legend.box = "horizontal",
                                    legend.box.background = element_rect(size = 1, colour = "black", fill = "white"),
                                    legend.key = element_rect(size = 0.5, colour = "black", fill = "white"),
                                    legend.key.size = unit(0.5, 'cm')
                                    )
ggsave("Fig1-3.tiff", device = "tiff", dpi = 300, width = 5.5, height = 4)

# Regression

f.main1.day <- f.main1[is.infinite(ret), ret := 0
    #][pre.period >= 0 , .SD #去除那些清仓的记录
    #][ret != 0, .SD
    ]

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
    ][, 
    ][, .SD[.N], by = .(cube.symbol, week.num)
    ][, ret_week := value / shift(value, type = "lag") - 1, by = .(cube.symbol)
    ][is.infinite(ret_week), ret_week := 0
    ]

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
        ][, ret_month := value / shift(value, type = "lag") - 1, by = .(cube.symbol)
        ][is.infinite(ret_month), ret_month := 0
        ]

f.main1.day[post.follow == 0 | (post.follow == 1 & (date - follow.date < pre.period)), rq(ret ~ post.follow, data = .SD , tau = seq(0.2, 0.8, by = 0.2))] %>% summary()
f.main1.day[post.follow == 0 | (post.follow == 1 & (date - follow.date < pre.period)), felm(ret ~ post.follow + as.numeric(trd.num / 100) + as.numeric(active.day / 365) + mmt | cube.symbol + stock.num + follow.date)] %>% summary()
f.main1.day[, felm(ret ~ post.follow + as.numeric(trd.num / 100) + as.numeric(active.day / 365) + mmt | cube.symbol + stock.num + follow.date)] %>% summary()
f.main1.day[, felm(ret ~ post.follow)] %>% summary()

f.main1.week[post.follow == 0 | (post.follow == 1 & (date - follow.date < pre.period)), rq(ret_week ~ post.follow, tau = 0.5)] %>% summary()
f.main1.week[post.follow == 0 | (post.follow == 1 & (date - follow.date < pre.period)), felm(ret_week ~ post.follow + as.numeric(trd.num / 100) + as.numeric(active.day / 365) + mmt | cube.symbol + stock.num + follow.date)] %>% summary()
f.main1.week[, felm(ret_week ~ post.follow + as.numeric(trd.num / 100) + as.numeric(active.day / 365) + mmt | cube.symbol + stock.num + follow.date)] %>% summary()

f.main1.month[post.follow == 0 | (post.follow == 1 & (date - follow.date < pre.period)), felm(ret_month ~ post.follow )] %>% summary()
f.main1.month[post.follow == 0 | (post.follow == 1 & (date - follow.date < pre.period)), felm(ret_month ~ post.follow + as.numeric(trd.num / 100) + as.numeric(active.day / 365) + mmt | cube.symbol + stock.num + follow.date)] %>% summary()
f.main1.month[, felm(ret_month ~ post.follow + as.numeric(trd.num / 100) + as.numeric(active.day / 365) + mmt | cube.symbol + stock.num + follow.date)] %>% summary()

# social variable
f.main2.daily <- fivefactor_daily[, setnames(.SD, 1, "date")
    ][, date := as.Date(date)
    ][f.main2, on = .(date)
    ][, ret := value / shift(value, type = "lag") - 1, by = .(cube.symbol)
    ][is.infinite(ret), ret := 0
    ][ret != 0, .SD] #去除那些清仓的记录

fivefactor_weekly[, trdwk := str_c(year(trdwk), week(trdwk))]
f.main2[, trdwk := str_c(year(date), week(date))]
f.main2.weekly <- fivefactor_weekly[f.main2, on = .(trdwk)
    ][, ret := value / shift(value, type = "lag") - 1, by = .(cube.symbol)
    ][is.infinite(ret), ret := 0
    ][ret != 0, .SD # 去除那些清仓的记录
    ][, .SD[.N], by = .(cube.symbol, trdwk)
    ][, ret_week := value / shift(value, type = "lag") - 1, by = .(cube.symbol)
    ][is.infinite(ret_week), ret_week := 0
    ]

f.main2[, trdmn := as.character(date) %>% str_sub(start = 1L, end = 7L) %>% str_replace_all("-", "")]
f.main2.monthly <- fivefactor_monthly[, trdmn := as.character(trdmn)
    ][f.main2, on = .(trdmn)
    ][, ret := value / shift(value, type = "lag") - 1, by = .(cube.symbol)
    ][is.infinite(ret), ret := 0
    ][ret != 0, .SD # 去除那些清仓的记录
    ][, .SD[.N], by = .(cube.symbol, trdmn)
    ][, ret_month := value / shift(value, type = "lag") - 1, by = .(cube.symbol)
    ][is.infinite(ret_month), ret_month := 0
    ]

f.main2.daily[oud > 0 & date - follow.date < 8, felm(ret ~ log(ind + 1) + log(oud + 1) + I(ln.cntr * 100) + as.numeric(trd.num / 100) + as.numeric(active.day / 365) + mmt | cube.symbol + stock.num + follow.date)] %>% summary()
f.main2.daily[oud > 0 & date - follow.date < 16, felm(ret ~ log(ind + 1) + log(oud + 1) + I(ln.cntr * 100) + as.numeric(trd.num / 100) + as.numeric(active.day / 365) + mmt | cube.symbol + stock.num + follow.date)] %>% summary()
f.main2.daily[oud > 0 & date - follow.date < 31, felm(ret ~ log(ind + 1) + log(oud + 1) + I(ln.cntr * 100) + as.numeric(trd.num / 100) + as.numeric(active.day / 365) + mmt | cube.symbol + stock.num + follow.date)] %>% summary()
f.main2.daily[oud > 0 & date - follow.date < 60, felm(ret ~ log(ind + 1) + log(oud + 1) + I(ln.cntr * 100) + as.numeric(trd.num / 100) + as.numeric(active.day / 365) + mmt | cube.symbol + stock.num + follow.date)] %>% summary()
f.main2.daily[oud > 0 & date - follow.date < 90, felm(ret ~ log(ind + 1) + log(oud + 1) + I(ln.cntr * 100) + as.numeric(trd.num / 100) + as.numeric(active.day / 365) + mmt | cube.symbol + stock.num + follow.date)] %>% summary()
f.main2.daily[oud > 0 & date - follow.date < 120, felm(ret ~ log(ind + 1) + log(oud + 1) + I(ln.cntr * 100) + as.numeric(trd.num / 100) + as.numeric(active.day / 365) + mmt | cube.symbol + stock.num + follow.date)] %>% summary()
f.main2.daily[oud > 0 & date - follow.date < 150, felm(ret ~ log(ind + 1) + log(oud + 1) + I(ln.cntr * 100) + as.numeric(trd.num / 100) + as.numeric(active.day / 365) + mmt | cube.symbol + stock.num + follow.date)] %>% summary()
f.main2.daily[oud > 0, felm(ret ~ log(ind + 1) + log(oud + 1) + I(ln.cntr * 100) + as.numeric(trd.num / 100) + as.numeric(active.day / 365) + mmt | cube.symbol + stock.num + follow.date)] %>% summary()

f.main2.weekly[oud > 0 & date - follow.date < 8, felm(ret ~ log(ind + 1) + log(oud + 1) + I(ln.cntr * 100) + as.numeric(trd.num / 100) + as.numeric(active.day / 365) + mmt | cube.symbol + stock.num + follow.date)] %>% summary()
f.main2.weekly[oud > 0 & date - follow.date < 16, felm(ret ~ log(ind + 1) + log(oud + 1) + I(ln.cntr * 100) + as.numeric(trd.num / 100) + as.numeric(active.day / 365) + mmt | cube.symbol + stock.num + follow.date)] %>% summary()
f.main2.weekly[oud > 0 & date - follow.date < 31, felm(ret ~ log(ind + 1) + log(oud + 1) + I(ln.cntr * 100) + as.numeric(trd.num / 100) + as.numeric(active.day / 365) + mmt | cube.symbol + stock.num + follow.date)] %>% summary()
f.main2.weekly[oud > 0 & date - follow.date < 60, felm(ret ~ log(ind + 1) + log(oud + 1) + I(ln.cntr * 100) + as.numeric(trd.num / 100) + as.numeric(active.day / 365) + mmt | cube.symbol + stock.num + follow.date)] %>% summary()
f.main2.weekly[oud > 0 & date - follow.date < 90, felm(ret ~ log(ind + 1) + log(oud + 1) + I(ln.cntr * 100) + as.numeric(trd.num / 100) + as.numeric(active.day / 365) + mmt | cube.symbol + stock.num + follow.date)] %>% summary()
f.main2.weekly[oud > 0 & date - follow.date < 120, felm(ret ~ log(ind + 1) + log(oud + 1) + I(ln.cntr * 100) + as.numeric(trd.num / 100) + as.numeric(active.day / 365) + mmt | cube.symbol + stock.num + follow.date)] %>% summary()
f.main2.weekly[oud > 0 & date - follow.date < 150, felm(ret ~ log(ind + 1) + log(oud + 1) + I(ln.cntr * 100) + as.numeric(trd.num / 100) + as.numeric(active.day / 365) + mmt | cube.symbol + stock.num + follow.date)] %>% summary()
f.main2.weekly[oud > 0, felm(ret ~ log(ind + 1) + log(oud + 1) + I(ln.cntr * 100) + as.numeric(trd.num / 100) + as.numeric(active.day / 365) + mmt | cube.symbol + stock.num + follow.date)] %>% summary()

f.main2.monthly[oud > 0 & date - follow.date < 60, felm(ret ~ log(ind + 1) + log(oud + 1) + I(ln.cntr * 100) + as.numeric(trd.num / 100) + as.numeric(active.day / 365) + mmt | cube.symbol + stock.num + follow.date)] %>% summary()
f.main2.monthly[oud > 0 & date - follow.date < 90, felm(ret ~ log(ind + 1) + log(oud + 1) + I(ln.cntr * 100) + as.numeric(trd.num / 100) + as.numeric(active.day / 365) + mmt | cube.symbol + stock.num + follow.date)] %>% summary()
f.main2.monthly[oud > 0 & date - follow.date < 120, felm(ret ~ log(ind + 1) + log(oud + 1) + I(ln.cntr * 100) + as.numeric(trd.num / 100) + as.numeric(active.day / 365) + mmt | cube.symbol + stock.num + follow.date)] %>% summary()
f.main2.monthly[oud > 0 & date - follow.date < 150, felm(ret ~ log(ind + 1) + log(oud + 1) + I(ln.cntr * 100) + as.numeric(trd.num / 100) + as.numeric(active.day / 365) + mmt | cube.symbol + stock.num + follow.date)] %>% summary()
f.main2.monthly[oud > 0, felm(ret ~ log(ind + 1) + log(oud + 1) + I(ln.cntr * 100) + as.numeric(trd.num / 100) + as.numeric(active.day / 365) + mmt | cube.symbol + stock.num + follow.date)] %>% summary()