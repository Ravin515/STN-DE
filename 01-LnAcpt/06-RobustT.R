library(styleer)
ld(f.main)

# 1. Robust test 1 (Gvn Up)
# 1.1 Select individuals before following people have DE 
f.cube.early <- f.surv.early[, .(cube.symbol = unique(cube.symbol))]
f.cube.late <- f.surv.late[, .(cube.symbol = unique(cube.symbol))]
f.cube <- f.cube.early[f.cube.late, on = "cube.symbol", nomatch = 0]
f.main <- f.surv.flw[f.cube, on = "cube.symbol", nomatch = 0]
rm(f.cube.early, f.cube.late, f.cube)

f.cube.DE <- f.main[, isde := ifelse(gain == 1 & issale == 1 & hldt.ls.7 == 1&first.half ==1 , 1, 0)
    ][isde == 1, .(cube.symbol = unique(cube.symbol), de = 1)
    ]

f.cube.NDE <- f.cube.DE[f.main[, .(cube.symbol = unique(cube.symbol), nde = 1)], on = .(cube.symbol)
    ][is.na(de)
    ][, ':='(de = NULL, nde = NULL)]

f.rbst1.DE <- f.main[f.cube.DE, on = .(cube.symbol), nomatch = 0
    ][, isde := 1]
f.rbst1.NDE <- f.main[f.cube.NDE, on = .(cube.symbol), nomatch = 0
    ][, isde := 0]
rm(f.surv.early.m, f.surv.late.m, f.surv.m)

# 1.2 Survival analysis & Regress 
library(ggthemes)
gg.rbst2 <- survfit(Surv(hold.time, issale) ~ state, data = f.rbst1.DE[hold.time < 200])
d.rbst2 <- ggsurv(gg.rbst2, lty.est = c(1, 1, 3, 3), surv.col = c("#CC6666", "#CC6666", "#7777DD", "#7777DD"), plot.cens = F, xlab = "holding period (days)", ylab = "fraction of holding position", main = "BiTradeDE Sample", size.est = 1) + theme_grey()

rst.rcox1.ede <- coxph(Surv(hold.time, issale) ~ gain, data = f.rbst1.DE[first.half == 1])
rst.rcox1.lde <- coxph(Surv(hold.time, issale) ~ gain, data = f.rbst1.DE[second.half == 1])
rst.rcox1.de <- coxph(Surv(hold.time, issale) ~ I(gain * first.half) + I(gain * second.half), data = f.rbst1.DE)


rst.rbst1 <- f.rbst1.DE[, felm(issale ~ gain + second.half + I(gain * second.half)| stck + cube.symbol + hold.time)] %>% summary()
rst.rbst1.e <- f.rbst1.DE[first.half == 1, felm(issale ~  gain | stck + cube.symbol + hold.time)] %>% summary()
rst.rbst1.l <- f.rbst1.DE[second.half == 1, felm(issale ~ gain | stck + cube.symbol + hold.time)] %>% summary()

list(rst.rcox2.e, rst.rcox2.l, rst.rcox2, rst.rbst2.e, rst.rbst2.l, rst.rbst2) %>%
    stargazer(
        out = "rst.BiTradeDEsample.doc",
        type = "html",
        title = "BiTradeDE Sample",
        dep.var.caption = "Dependent Variable: Sale",
        dep.var.labels.include = F,
        covariate.labels = c("Gain", "Gain*Pre.follow", "Pro.follow", "Gain*Pro.follow"),
        omit.stat = c("LL", "lr", "wald", "max.rsq", "logrank"),
        model.names = T,
        single.row = F,
        add.lines = list(c("cube.symbol", "--", "--", "--", "Yes", "Yes", "Yes"), c("hold.time", "--", "--", "--", "Yes", "Yes", "Yes"), c("stock", "--", "--", "--", "Yes", "Yes", "Yes"))
    )

# 2. Robust test 2 (Gvn Up)
# 2.1 Calculate the ratio change of the stock in portfolio
f.prt <- f.surv.flw[hold.price.lst != 0, prt.chng := (Clsprc - hold.price.lst) / hold.price.lst
    ][issale == 1, prt.chng :=ifelse(hold.price.lst !=0, (price - hold.price.lst) / hold.price.lst, 0)
    ]

f.cube.early <- f.surv.early[, .(cube.symbol = unique(cube.symbol))]
f.cube.late <- f.surv.late[, .(cube.symbol = unique(cube.symbol))]
f.cube <- f.cube.early[f.cube.late, on = "cube.symbol", nomatch = 0]
f.rbst2 <- f.cube[f.prt, on = "cube.symbol", nomatch = 0]

# 2.2 Regress
rst.rbst2 <- f.rbst2[, felm(issale ~ I(gain * second.half) + second.half + gain | stck + cube.symbol + prt.chng + hold.time)] %>% summary()

# 3. Robust test3 (Gvn up)
# Bull market and bear market test

# 3.1 Import market risk factor and calculate
# Caculate CSI300 index (BB non-parameter method)
# Daily
bb <- data.table(tag = list.files(getwd(), pattern = "trdd"))
bb[, csv := lapply(tag, function(x) {
        fread(x, sep = ",", fill = T, encoding = "UTF-8", na.strings = "")
    }
)]
bb <- bb[, rbindlist(.SD[['csv']], fill = T, idcol = "csv")
    ][Indexcd == "300", .SD, .SDcol = c(2, 3, 5)
    ][, .(date = as.Date(Idxtrd01), index = Idxtrd05)
    ][date >= as.Date("2016-07-01") & date <= as.Date("2018-03-31")]
plot(x = bb$date, y = bb$index, type = "l")

# Weekly
bb <- data.table(tag = list.files(getwd(), pattern = "trdd"))
bb[, csv := lapply(tag, function(x) {
    fread(x, sep = ",", fill = T, encoding = "UTF-8", na.strings = "")
}
)]

bb <- bb[, rbindlist(.SD[['csv']], fill = T, idcol = "csv")
    ][Indexcd == "300", .SD, .SDcol = c(2, 3, 5)
    ][, .(date = as.Date(Idxtrd01), index = Idxtrd05)
    ][date >= as.Date("2016-07-01") & date <= as.Date("2018-03-31")
    ][, date.week := str_c(year(date), str_pad(week(date), width = 2L, side = "left", pad = "0"), sep = "-")
    ][, index := mean(index), by = .(date.week)
    ][, .(index = unique(index)), by = date.week
    ][order(date.week)]
plot(bb$index, type = "l")

# Monthly
bb <- fread("IDX_Idxtrdmth.csv", encoding = "UTF-8")
bb <- bb[Indexcd == "000300", .SD, .SDcol = c(2, 6)
    ][, month := str_remove(Month, "-") %>% as.numeric()
    ][month >= 201608 & month <= 201803]
plot(bb$Clsidx, type = "l")

#bb[, setnames(bb, 1:2, c("date", "hs300"))
    #][, date := as.Date(date, "%Y-%m-%d")
    #][, date.month := paste0(year(date), str_sub(date, start = 6L, end = 7L), "")]

#bb <- bb[date.month >= 201601 & date.month <= 201805]
#peak.valley <- bb[, month.index := mean(hs300), by = date.month
    #][, date.month := str_c(str_sub(date.month, 1, 4), "-", str_sub(date.month, 5, 6))]

#plot(unique(peak.valley$month.index), type = "l")

# Calculate monthly return (Kao Parameter method)
ret.month <- fread("TRD_Cnmont1.csv", encoding = "UTF-8")
mkt.bb <- ret.month[Markettype == 5
    ][, .(ret = as.numeric(Cmretwdos), date.month = as.Date(paste0(Trdmnt, "-01"), "%Y-%m-%d"))
    ][,
        .(ret.twl = {
        a <- vector()
        for (i in 12:.N) {
            a[i] <- mean(ret[(i - 11):i])
        }
        a
        }, date.month)
    ][date.month >= as.Date("2016-01-01") & date.month <= as.Date("2019-01-01")
    ][, date.month := str_c(str_sub(date.month, 1, 4), "-", str_sub(date.month, 6, 7))]
rm(ret.month)
plot(mkt.bb$ret.twl, type = "l")

## calculate trading number
#daily
trade.num <- f.main[, .(num = ifelse(issale == 1 & loss == 1, 1, 0), date, cube.symbol)]
trade.num <- trade.num[, trade.num := sum(num), by = .(cube.symbol, date)
    ][, trade.num := mean(trade.num), by = .(date)
    ][, .(trade.num = unique(trade.num)), by = date
    ][order(date)
    ][date >= as.Date("2016-07-01")]
plot(x = trade.num$date, y = trade.num$trade.num, type = "l")

#weekly
trade.num <- f.main[, .(num = ifelse(issale == 1 & loss == 1, 1, 0), date, cube.symbol)]
trade.num[, date.week := str_c(year(date), str_pad(week(date), width = 2L, side = "left", pad = "0"), sep = "-")]
trade.num <- trade.num[, trade.num := sum(num), by = .(cube.symbol, date.week)
    ][, trade.num := mean(trade.num), by = .(date.week)
    ][, .(trade.num = unique(trade.num)), by = date.week
    ][order(date.week)
    ][date.week != "2016-26"]
plot(trade.num$trade.num, type = "l")


# monthly
trade.num <- f.main[, .(num = ifelse(issale == 1 & loss == 1, 1, 0), date, cube.symbol)]
trade.num <- trade.num[, date.month := as.character(date) %>% str_sub(start = 1L, end = 7L)
    ][, trade.num := sum(num), by = .(cube.symbol, date.month)
    ][, date := NULL
    ][, unique(.SD)
    ][, .(trade.num = mean(trade.num)), by = .(date.month)
    ][order(date.month)
    ][!(date.month %in% c("2016-06", "2016-07"))]
plot(trade.num$trade.num, type = "l")

# 3.2 Merge two tables
plot.bb <- bb[, .(index = Idxtrd05), by = .(date.week=week(as.Date()))
    ][mkt.bb, on = .(date.month)
    ][, bb := ifelse(ret.twl > 0, "bull", "bear")]


# 3.3 Draw picture
#s1 <- data.table(xmin = "2016-03", xmax = "2016-07", ymin = -Inf, ymax = Inf, type = "bear market")
s2 <- data.table(xmin = "2016-10", xmax = "2016-12", ymin = -Inf, ymax = Inf, type = "bear market")
s3 <- data.table(xmin = "2018-02", xmax = "2018-03", ymin = -Inf, ymax = Inf, type = "bear market")

library(grid)
library(gridExtra)
p1 <- ggplot() +
    ggtitle("B.")+
    geom_line(data = trade.num, aes(x = date.week, y = trade.num, group = 1), stat = "identity", size = 0.5, linetype = 4, colour = "grey50") +
    geom_rect(data = s2, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = "grey"), alpha = 0.3) +
    scale_fill_manual(values = c("grey"), labels = c("Bear market")) +
    geom_rect(data = s3, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), alpha = 0.3, fill = "grey") +
    #scale_y_continuous(position = "right", sec.axis = sec_axis(~.)) +
    theme_bw() +
    labs(x = "Week", y = "Number of sales at loss per trader") +
        theme(
        axis.text.x = element_text(angle = 70, hjust = 1),
        axis.title.x = element_text(), #size = 20, margin =  margin(t = 20, r = 0, b = 20, l = 0)
        axis.title.y = element_text(size = 8), #(size = 20, margin = margin(t = 10, r = 0, b = 0, l = 20)),
        #axis.text = element_text(size = 20),
        #axis.title.y.right = element_text(), # size = 20, margin = margin(t = 10, r =10, b = 0, l = 10)
        ##panel.border = element_rect(linetype = 1, fill = NA),
        legend.title = element_blank(),
        legend.position = "bottom",
        #legend.direction = "horizontal",
        #legend.text = element_text(size = 16),
        ##legend.key = element_rect(size = 0.5, colour = "black", fill = "white"),
        ##legend.key.size = unit(1, 'cm'),
        #legend.spacing.x = unit(0.5, 'cm'),
        #legend.spacing.y = unit(2, 'cm'),
        #legend.box = "horizontal",
        ##legend.box.background = element_rect(size = 1, colour = "black", fill = "white")
        #axis.text.x = element_blank(),
        #axis.title.x = element_blank(),
        #plot.title = element_blank(),
        #axis.ticks.x = element_blank(),
        #plot.margin = unit(c(0, 1, 0, 2.55), "lines")
               #plot.margin = unit(c(0.1, 0.25, 0.5, 3.18), "lines")
                )
gp1 <- ggplot_gtable(ggplot_build(p1))
ggsave("Fig4-1.tiff", gp1, device = "tiff", dpi = 300, width = 5, height = 3.5)
p2 <- ggplot() +
    ggtitle("A.") +
    geom_line(data = plot.bb, aes(x = date.month, y = index, group = 1), stat = "identity", size = 0.5) +
    geom_rect(data = s2, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = "grey"), alpha = 0.3) +
    scale_fill_manual(values = c("grey"), labels = c("Bear market")) +
    geom_rect(data = s3, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), alpha = 0.3, fill = "grey") +
    theme_bw() +
    labs(x = "Month", y = "CSI 300") +
    #scale_colour_manual(values = c("#CC6666", "#7777DD"))+
    theme(
            axis.text.x = element_text(angle = 70, hjust = 1),
            axis.title.x = element_text(), #size = 24, margin = margin(t = 20, r = 0, b = 20, l = 0)
            #axis.title.y = element_text(size = 20, margin = margin(t = 0, r = 10, b = 0, l = 10)),
            #axis.text = element_text(size = 20),
            #panel.border = element_rect(linetype = 1, fill = NA),
            legend.title = element_blank(),
            legend.position = "bottom",
            #legend.direction = "horizontal",
            #legend.text = element_text(size = 24),
            #legend.key = element_rect(size = 0.5, colour = "black", fill = "white"),
            #legend.key.size = unit(1, 'cm'),
            #legend.spacing.x = unit(0.5, 'cm'),
            #legend.spacing.y = unit(2, 'cm'),
            #legend.box = "horizontal",
            #legend.box.background = element_rect(size = 1, colour = "black", fill = "white"),
                #axis.text.x = element_blank(),
                #axis.title.x = element_blank(),
                #plot.title = element_blank(),
                #axis.ticks.x = element_blank(),
                #plot.margin = unit(c(1, 3.1, 0, 1), "lines")
                #plot.margin = unit(c(4, 5.2, 0, 0), "lines")
                )
ggsave("Fig4-2.tiff", gp2, device = "tiff", dpi = 300, width = 5, height = 3.5)
gp2 <- ggplot_gtable(ggplot_build(p2))
maxWidth = unit.pmax(gp1$widths[2:3], gp2$widths[2:3])
gp1$widths[2:3] <- maxWidth
gp2$widths[2:3] <- maxWidth
gp3 <- grid.arrange(gp2, gp1)
ggsave("Fig4.tiff", gp3, device = "tiff", dpi = 300, width = 5, height = 5)

# 3.4 Regression
f.rbst3 <- mkt.bb[f.main[, date.month := format(date, "%Y-%m")], on = "date.month"]
rm(mkt.bb, ret.month)
library(alpaca)
rst.rbst3.bull <- feglm(issale ~ gain + second.half + I(gain * second.half) | cube.symbol + stck + hold.time, f.rbst3[ret.twl > 0 & (first.half == 1 | second.half == 1 & date - as.Date(follow.date) <= pre.period)], binomial("logit"))  %>% summary() # bulll
rst.rbst3.bear <- feglm(issale ~ gain + second.half + I(gain * second.half) | cube.symbol + stck + hold.time, f.rbst3[ret.twl < 0 & (first.half == 1 | second.half == 1 & date - as.Date(follow.date) <= pre.period)], binomial("logit"))  %>% summary() # bear

rst.rbst3.bull1 <- feglm(issale ~ gain + second.half + I(gain * second.half) + mmt + as.numeric(active.day / 365) + as.numeric(trd.num / 1000) | cube.symbol + stck + hold.time, f.rbst3[ret.twl > 0 & (first.half == 1 | second.half == 1 & date - as.Date(follow.date) <= pre.period)], binomial("logit"))
rst.rbst3.bear1 <- feglm(issale ~ gain + second.half + I(gain * second.half) + mmt + mmt + as.numeric(active.day / 365) + as.numeric(trd.num / 1000) | cube.symbol + stck + hold.time, f.rbst3[ret.twl < 0 & (first.half == 1 | second.half == 1 & date - as.Date(follow.date) <= pre.period)], binomial("logit"))

rst.rbst3.bull2 <- feglm(issale ~ gain + second.half + I(gain * second.half) | cube.symbol + stck + hold.time, f.rbst3[ret.twl > 0], binomial("logit"))
rst.rbst3.bear2 <- feglm(issale ~ gain + second.half + I(gain * second.half) | cube.symbol + stck + hold.time, f.rbst3[ret.twl < 0], binomial("logit"))

rst.rbst3.bull3 <- feglm(issale ~ gain + second.half + I(gain * second.half) + mmt + as.numeric(active.day / 365) + as.numeric(trd.num / 1000) | cube.symbol + stck + hold.time, f.rbst3[ret.twl > 0], binomial("logit"))
rst.rbst3.bear3 <- feglm(issale ~ gain + second.half + I(gain * second.half) + mmt + as.numeric(active.day / 365) + as.numeric(trd.num / 1000) | cube.symbol + stck + hold.time, f.rbst3[ret.twl < 0], binomial("logit"))

list(rst.rbst3.bull, rst.rbst3.bear, rst.rbst3.bull1, rst.rbst3.bear1, rst.rbst3.bull2, rst.rbst3.bear2, rst.rbst3.bull3, rst.rbst3.bear3) %>%
    stargazer(out = "rst.bullbear.doc",
        type = "html",
        title = "Bull and Bear market robust test",
        dep.var.caption = "Dependent Variable: Sale",
        dep.var.labels.include = F,
        column.labels = c("Bull", "Bear", "Bull", "Bear", "Bull", "Bear", "Bull", "Bear"),
        covariate.labels = c("Gain", "Post.follow", "Gain*Post.follow",  "Momentum", "Active days", "Trade number"),
        model.names = F,
        single.row = F,
        add.lines = list(c("Trader FE", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"), c("Hold period FE", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"), c("Stock FE", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")))

# 4. Robust test 4 (GvnUp)
# The loss price regress on holding behavior
# 4.1 Select the people who had trading before and after first follow
f.cube.early <- f.surv.early[, .(cube.symbol = unique(cube.symbol))]
f.cube.late <- f.surv.late[, .(cube.symbol = unique(cube.symbol))]
f.cube <- f.cube.early[f.cube.late, on = "cube.symbol", nomatch = 0]
f.rbst4 <- f.surv.flw[f.cube, on = "cube.symbol", nomatch = 0]
f.rbst4.early <- f.rbst4[first.half == 1]
f.rbst4.late <- f.rbst4[second.half == 1]
rm(f.cube.early, f.cube.late, f.cube)

# 4.2 Regression
rst.rbst4.e <- f.rbst4.early[, felm(issale ~ loss | cube.symbol + stck + hold.time)]
rst.rbst4.l <- f.rbst4.late[, felm(issale ~ loss | cube.symbol + stck + hold.time)]
rst.rbst4 <- f.rbst4[, felm(ishold ~ loss + I(loss * second.half) + second.half| cube.symbol + stck + hold.time)]

# 4.3 Regression
lrst.rbst2.e <- f.rbst2.early[, felm(ishold ~ loss | cube.symbol + stck + hold.time)]
lrst.rbst2.l <- f.rbst2.late[, felm(ishold ~ loss | cube.symbol + stck + hold.time)]
lrst.rbst2 <- f.rbst2[, felm(ishold ~ loss + second.half + I(loss * second.half) | cube.symbol + stck + hold.time)]

list(lrst.rbst1.e, lrst.rbst1.l, lrst.rbst1, lrst.rbst2.e, lrst.rbst2.l, lrst.rbst2) %>%
    stargazer(
        out = "rst.Loss-Holdsample.doc",
        type = "html",
        title = "Loss-Hold Sample",
        dep.var.caption = "Dependent Variable: Hold",
        column.separate = c(3, 3),
        column.labels = c("BiTrade Sample", "BiTradeDE Sample"),
        dep.var.labels.include = F,
        covariate.labels = c("Loss", "Pro.follow", "Loss*Pro.follow"),
        no.space = T,
        omit.stat = c("LL", "ser"),
        model.names = T,
        single.row = F,
        add.lines = list(c("cube.symbol", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"), c("hold.time", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"), c("stock", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"))
        )

# 5. Robust test 5 (GvnUp)
# Select tradings bought and sold during pre-follow period and as same in post-follow
# 5.1 Select the people who had trading before and after first follow and two same trading period
f.surv.early <- f.surv.flw[first.half == 1]
f.surv.late <- f.surv.flw[second.half == 1]
f.cube.early <- f.surv.early[, .(cube.symbol = unique(cube.symbol))]
f.cube.late <- f.surv.late[, .(cube.symbol = unique(cube.symbol))]
f.cube <- f.cube.early[f.cube.late, on = "cube.symbol", nomatch = 0]
f.main <- f.surv.flw[f.cube, on = "cube.symbol", nomatch = 0]

f.main[, pre.period := as.Date(follow.date) - min(date), by = .(cube.symbol)]
f.main.early <- f.main[first.half == 1]
f.main.late <- f.main[second.half == 1 & date - as.Date(follow.date) <= pre.period]
rm(f.cube.early, f.cube.late, f.cube)

# 5.2 Select those sample
f.rbst5.early <- f.main.early[buy.at < follow.date]
f.rbst5.late <- f.main.late[buy.at >= follow.date]
f.rbst5 <- rbindlist(list(f.rbst5.early, f.rbst5.late))
f.rbst5[first.half == 1, felm(issale ~ gain | stck + cube.symbol + hold.time)] %>% summary()
f.rbst5[second.half == 1, felm(issale ~ gain | stck + cube.symbol + hold.time)] %>% summary()
f.rbst5[, felm(issale ~ gain + I(gain * second.half) | stck + cube.symbol + hold.time)] %>% summary()

# 6. Robust test 6
# Delete first 3 months data and test the post-follow
styleer::ld(f.nwl.reg)
f.rbst6 <- f.nwl.reg[!(tag %in% (1:3)) & active.day >= 0
    ][, pre.period := as.Date(follow.date) - min(date), by = cube.symbol
    ][pre.period >= 0, .SD]

# Plot1 & Table 2
library(data.table)
library(stringr)
library(survival)
library(ggplot2)
library(GGally)
gg.main <- survfit(Surv(hold.time, issale) ~ state, data = f.rbst6[hold.time < 200 & (first.half == 1 | (second.half == 1 & date - as.Date(follow.date) <= pre.period))])

d.main <- ggsurv(gg.main,
                             lty.est = c(1, 1, 4, 4),
                             surv.col = c("#CC6666", "#CC6666", "#7777DD", "#7777DD"),
                             plot.cens = F,
                             xlab = "Holding period (days)",
                             ylab = "Remaining position",
                             main = "",
                             size.est = 0.5,
                             order.legend = T
                            ) +

                            #theme_grey() +
                            theme(
#axis.title.x = element_text(size = 24, margin = margin(t = 20, r = 0, b = 20, l = 0)),
#axis.title.y = element_text(size = 24, margin = margin(t = 0, r = 20, b = 0, l = 20)),
#axis.text = element_text(size = 24),
#panel.border = element_rect(linetype = 1, fill = NA),
                            legend.title = element_blank(),
                            legend.position = "bottom",
#legend.direction = "horizontal",
#legend.text = element_text(size = 24),
#legend.key = element_rect(size = 0.5, colour = "black", fill = "white"),
#legend.key.size = unit(1, 'cm'),
                            legend.spacing.x = unit(0.1, 'cm'),
                            legend.spacing.y = unit(2, 'cm'),
#legend.box = "horizontal",
#legend.box.background = element_rect(size = 1, colour = "black", fill = "white")
                        plot.margin = unit(c(0, 1, 1, 1), "lines")
                            )

ggsave("Fig2.tiff", device = "tiff", dpi = 300, width = 6, height = 6) # plos one size

rst.cox.e <- coxph(Surv(hold.time, issale == 1) ~ gain, data = f.rbst6[first.half == 1])
rst.cox.l <- coxph(Surv(hold.time, issale == 1) ~ gain, data = f.rbst6[second.half == 1 & date - as.Date(follow.date) <= pre.period])
rst.cox <- coxph(Surv(hold.time, issale == 1) ~ gain + second.half + I(gain * second.half), data = f.rbst6[first.half == 1 | (second.half == 1 & date - as.Date(follow.date) <= pre.period)])

library(stargazer)
list(rst.cox.e, rst.cox.l, rst.cox) %>%
    stargazer(
        out = "Table2R.doc",
        type = "html",
        title = "Main Regression",
        dep.var.labels.include = F,
        covariate.labels = c("Gain", "Postfollow", "Gain * Postfollow"),
        column.labels = c("Pre-follow", "Pro-follow", "Two-stage"),
        omit.stat = c("LL", "lr", "wald", "max.rsq", "logrank"),
        model.names = F,
        single.row = F
    )

# Table 3
library(alpaca)
rst.rbst.e <- feglm(issale ~ gain | stck + cube.symbol + hold.time, f.rbst6[first.half == 1], binomial("logit")) #%>% summary()
rst.rbst.l <- feglm(issale ~ gain | stck + cube.symbol + hold.time, f.rbst6[second.half == 1 & date - as.Date(follow.date) <= pre.period], binomial("logit")) #%>% summary()

rst.rbst.t0 <- feglm(issale ~ gain + second.half + I(gain * second.half) | stck + cube.symbol + hold.time, f.rbst6[first.half == 1 | (second.half == 1 & date - as.Date(follow.date) <= pre.period)], binomial("logit"))

rst.rbst.t1 <- feglm(issale ~ gain + second.half + I(gain * second.half) + mmt | cube.symbol + stck + hold.time, f.rbst6[first.half == 1 | (second.half == 1 & date - as.Date(follow.date) <= pre.period)], binomial("logit"))

rst.rbst.t2 <- feglm(issale ~ gain + second.half + I(gain * second.half) + as.numeric(active.day / 365) | cube.symbol + stck + hold.time, f.rbst6[first.half == 1 | (second.half == 1 & date - as.Date(follow.date) <= pre.period)], binomial("logit"))

rst.rbst.t3 <- feglm(issale ~ gain + second.half + I(gain * second.half) + as.numeric(trd.num / 1000) | cube.symbol + stck + hold.time, f.rbst6[first.half == 1 | (second.half == 1 & date - as.Date(follow.date) <= pre.period)], binomial("logit"))

rst.rbst.t4 <- feglm(issale ~ gain + second.half + I(gain * second.half) + mmt + as.numeric(active.day / 365) + as.numeric(trd.num / 1000) | cube.symbol + stck + hold.time, f.rbst6[first.half == 1 | (second.half == 1 & date - as.Date(follow.date) <= pre.period)], binomial("logit")) #%>% summary()

rst.rbst.f1 <- feglm(issale ~ gain + second.half + I(gain * second.half) | cube.symbol + stck + hold.time, f.rbst6, binomial("logit"))

rst.rbst.f2 <- feglm(issale ~ gain + second.half + I(gain * second.half) + mmt + as.numeric(active.day / 365) + as.numeric(trd.num / 1000) | cube.symbol + stck + hold.time, f.rbst6, binomial("logit"))

library(texreg)
list(rst.rbst.e, rst.rbst.l, rst.rbst.t0, rst.rbst.t1, rst.rbst.t2, rst.rbst.t3, rst.rbst.t4, rst.rbst.f1, rst.rbst.f2) %>%
    htmlreg(
            file = "Rbst6.doc",
#custom.header = list("Pre-follow" = 1:2, 
#"Two-stage" = 3:4, 
#"After two-stage" = 5:6, 
#"Full sample" = 7:8),
            custom.model.names = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)", "(9)"),
            custom.gof.rows = list("Trader FE" = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                                                 "Holding period FE" = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                                                 "Stock FE" = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")),
            custom.coef.names = c("Gain", "Post-follow", "Gain * Post-follow", "Momentum", "Active days", "Trade number"),
            reorder.coef = c(1, 2, 3, 4, 5, 6),
            no.margin = T,
            digits = 3
            )

# Plot 3
f.surv.early <- f.rbst6[first.half == 1]
f.surv.late <- f.rbst6[second.half == 1]

f.cube.early <- f.surv.early[, .(cube.symbol = unique(cube.symbol))]
f.cube.late <- f.surv.late[, .(cube.symbol = unique(cube.symbol))]
f.cube <- f.cube.early[f.cube.late, on = "cube.symbol", nomatch = 0]

f.rbst6 <- f.rbst6[f.cube, on = "cube.symbol", nomatch = 0]
f.rbst6.early <- f.rbst6[first.half == 1]
f.rbst6.late <- f.rbst6[second.half == 1]

# Cox harzard regression
num.early <- f.rbst6.early[, rows:= .N, by = .(cube.symbol)
    ][rows > 1, .SD][["rows"]] %>% unique() %>% cumsum()
DEbeta.e <- vector(mode = "numeric")
for (i in 1:length(num.early)) {
    if (i == 1) {
        DEbeta.e[i] <- coxph(Surv(hold.time, issale == 1) ~ gain, data = f.rbst6.early[1:num.early[1]]) %>% coef()
    } else {
        DEbeta.e[i] <- coxph(Surv(hold.time, issale == 1) ~ gain, data = f.rbst6.early[(num.early[i-1]+1):num.early[i]]) %>% coef()
    }
}

num.late <- f.rbst6.late[, rows := .N, by = .(cube.symbol)
    ][rows > 1, .SD][["rows"]] %>% unique() %>% cumsum()
DEbeta.l <- vector(mode = "numeric")
for (i in 1:length(num.late)) {
    if (i == 1) {
        DEbeta.l[i] <- coxph(Surv(hold.time, issale == 1) ~ gain, data = f.rbst6.late[1:num.late[1]]) %>% coef()
    } else {
        DEbeta.l[i] <- coxph(Surv(hold.time, issale == 1) ~ gain, data = f.rbst6.late[(num.late[i - 1] + 1):num.late[i]]) %>% coef()
    }
}

DEbeta.e <- data.table(DE = DEbeta.e)
DEbeta.e <- DEbeta.e[!is.na(DE), .SD
    ][, Stage := "pre-follow"]
DEbeta.l <- data.table(DE = DEbeta.l)
DEbeta.l <- DEbeta.l[!is.na(DE), .SD
    ][, Stage := "post-follow"]
ggDE <- rbindlist(list(DEbeta.e, DEbeta.l), fill = T)

# Logistic regression
DEbeta.e <- f.rbst6.early[, .(pre.follow = glm(issale ~ gain, family = "binomial") %>% coef()), by = .(cube.symbol)
    ][, .SD[2], by = .(cube.symbol)]
DEbeta.l <- f.rbst6.late[, .(post.follow = glm(issale ~ gain, family = "binomial") %>% coef()), by = .(cube.symbol)
    ][, .SD[2], by = .(cube.symbol)]
# OLS regression
DEbeta.e <- f.rbst6.early[, .(pre.follow = lm(issale ~ gain) %>% coef()), by = .(cube.symbol)
    ][, .SD[2], by = .(cube.symbol)]
DEbeta.l <- f.rbst6.late[, .(post.follow = lm(issale ~ gain) %>% coef()), by = .(cube.symbol)
    ][, .SD[2], by = .(cube.symbol)]
DEbeta <- DEbeta.e[DEbeta.l, on = "cube.symbol"]

rst.t.test <- DEbeta[!is.na(pre.follow) & !is.na(post.follow), t.test(post.follow, pre.follow)]
ggDE <- melt(DEbeta[!is.na(pre.follow) & !is.na(post.follow)], id.vars = "cube.symbol", measure.vars = c("pre.follow", "post.follow"))
setnames(ggDE, 2:3, c("Stage", "DE"))
ggDE[, Stage := as.character(Stage)
    ][, Stage := ifelse(Stage == "pre.follow", "pre-follow", "post-follow")]

d.ttest <- ggplot(ggDE, aes(x = DE, colour = Stage, fill = Stage)) +
                            geom_line(stat = "density", size = 0.5) +
                            theme_grey() +
                            scale_colour_manual(values = c("#CC6666", "#7777DD")) +
                            labs(x = "Disposition effect", y = "Density") +
                            scale_fill_manual(values = c("#CC6666", "#7777DD"),
                                                        name = "Stage",
                                                        breaks = c("post.follow", "pre.follow"),
                                                        labels = c("post-follow", "pre-follow")) +
                                                        theme(
#axis.title.x = element_text(size = 24, margin = margin(t = 20, r = 0, b = 20, l = 0)),
#axis.title.y = element_text(size = 24, margin = margin(t = 0, r = 20, b = 0, l = 20)),
#axis.text = element_text(size = 24),
#panel.border = element_rect(linetype = 1, fill = NA),
                                    legend.title = element_blank(),
                                    legend.position = "bottom",
#legend.direction = "horizontal",
#legend.text = element_text(size = 24),
#legend.key = element_rect(size = 0.5, colour = "black", fill = "white"),
#legend.key.size = unit(1, 'cm'),
                                    legend.spacing.x = unit(0.1, 'cm'),
                                    legend.spacing.y = unit(2, 'cm'),
#legend.box = "horizontal",
#legend.box.background = element_rect(size = 1, colour = "black", fill = "white")
                                    )
ggsave("Fig3R2.tiff", device = "tiff", dpi = 300, width = 5, height = 3.5)