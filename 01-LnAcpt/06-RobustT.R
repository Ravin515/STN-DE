library(GGally)
library(survival)
library(Matrix)
library(lfe)
library(stargazer)
library(lubridate)
ld(f.surv.flw)
f.surv.flw[, tag := ifelse(date - as.Date(follow.date) > 0, 1, 0), keyby = .(cube.symbol, stck)
    ][, first.half := ifelse(tag == 0, 1, 0)
    ][, second.half := ifelse(tag == 1, 1, 0)
    ][, loss := ifelse(gain == 1, 0, 1)
    ][, ishold := ifelse(issale == 1, 0, 1)
    ][, hldt.ls.7 := ifelse(hold.time < 7, 1, 0) # sign the two-stage and loss&hold data
    ][isgain == 'gain' & first.half == 1, state := "pre.gain"
    ][isgain == 'loss' & first.half == 1, state := "pre.loss"
    ][isgain == 'gain' & second.half == 1, state := "pro.gain"
    ][isgain == 'loss' & second.half == 1, state := "pro.loss"]

f.surv.early <- f.surv.flw[first.half == 1]
f.surv.late <- f.surv.flw[second.half == 1]

# 1. Robust test 1 
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

#rst.rcox1.ende <- coxph(Surv(hold.time, issale) ~ gain, data = f.rbst1.NDE[first.half == 1])
#rst.rcox1.lnde <- coxph(Surv(hold.time, issale) ~ gain, data = f.rbst1.NDE[second.half == 1])
#rst.rcox1.nde <- coxph(Surv(hold.time, issale) ~ I(gain * first.half) + I(gain * second.half), data = f.rbst1.NDE)

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

# 2. Robust test 2
# 2.1 Calculate the ratio change of the stock in portfolio
f.prt <- f.surv.flw[hold.price.lst != 0, prt.chng := (Clsprc - hold.price.lst) / hold.price.lst
    ][issale == 1, prt.chng :=ifelse(hold.price.lst !=0, (price - hold.price.lst) / hold.price.lst, 0)
    ]

f.cube.early <- f.surv.early[, .(cube.symbol = unique(cube.symbol))]
f.cube.late <- f.surv.late[, .(cube.symbol = unique(cube.symbol))]
f.cube <- f.cube.early[f.cube.late, on = "cube.symbol", nomatch = 0]
f.rbst3 <- f.cube[f.prt, on = "cube.symbol", nomatch = 0]

# 2.2 Regress
rst.rbst3 <- f.rbst3[, felm(issale ~ I(gain * second.half) + second.half + gain | stck + cube.symbol + prt.chng + hold.time)] %>% summary()

# 3. Robust test3
# Bull market and bear market test
# 3.1 Import market risk factor and calculate
mkt.sit <- fread("return_month.csv", encoding = "UTF-8")
mkt.sit[, mkt_rf_cum := {
    cum <- vector(length = .N, mode = "numeric")
    for (i in 13:.N) {
        cum[i] <- mean(mkt_rf[(i-12):(i-1)]) 
    }
    cum
}]
mkt.sit <- mkt.sit[trdmn >= 201606 & trdmn <= 201803]
mkt.sit[, trdmn := as.character(trdmn)
    ][, trdmn := str_c(str_sub(trdmn, 1, 4), "-", str_sub(trdmn, 5, 6))
    ][, date.month := trdmn]

# 3.2 Select the people who had trading before and after first follow
f.cube.early <- f.surv.early[, .(cube.symbol = unique(cube.symbol))]
f.cube.late <- f.surv.late[, .(cube.symbol = unique(cube.symbol))]
f.cube <- f.cube.early[f.cube.late, on = "cube.symbol", nomatch = 0]
f.main <- f.surv.flw[f.cube, on = "cube.symbol", nomatch = 0]
f.main[, date.month := format(date, "%Y-%m")]
f.main[, pre.period := as.Date(follow.date) - min(date), by = .(cube.symbol)]
f.main.early <- f.main[first.half == 1]
f.main.late <- f.main[second.half == 1 & date - as.Date(follow.date) <= pre.period]

# 3.3 Merge two tables
f.rbst3 <- mkt.sit[, .(date.month, mkt_rf, mkt_rf_cum)
    ][f.main, on = "date.month"]
f.rbst3[, tag := ifelse(mkt_rf_cum > 0, 1, 0)
    ][, tag1 := ifelse(mkt_rf > 0, 1, 0)]

# 3.4 Regression
rst.rbst3.bull <- f.rbst3[mkt_rf_cum > 0, felm(issale ~ I(gain * second.half) + gain | cube.symbol + stck + hold.time)] # %>% summary() # bull
rst.rbst3.bear <- f.rbst3[mkt_rf_cum < 0, felm(issale ~ I(gain * second.half) + gain | cube.symbol + stck + hold.time)] # %>% summary() # bear

#f.rbst3[, felm(issale ~ I(gain * tag) + tag | cube.symbol + stck + hold.time)] # %>% summary()
#f.rbst3[mkt_rf > 0, felm(issale ~ I(gain * second.half) + gain| cube.symbol + stck + hold.time)] %>% summary()
#f.rbst3[mkt_rf < 0, felm(ishold ~ I(gain * second.half) + gain| cube.symbol + stck + hold.time)] %>% summary()
#f.rbst3[, felm(issale ~ I(gain * tag1) + tag1 | cube.symbol + stck + hold.time)] %>% summary()



# 4. Robust test 4
# The loss price regress on holding behavior
# 4.1.1 Select the people who had trading before and after first follow
f.cube.early <- f.surv.early[, .(cube.symbol = unique(cube.symbol))]
f.cube.late <- f.surv.late[, .(cube.symbol = unique(cube.symbol))]
f.cube <- f.cube.early[f.cube.late, on = "cube.symbol", nomatch = 0]
f.rbst4 <- f.surv.flw[f.cube, on = "cube.symbol", nomatch = 0]
f.rbst4.early <- f.rbst4[first.half == 1]
f.rbst4.late <- f.rbst4[second.half == 1]
rm(f.cube.early, f.cube.late, f.cube)

# 4.1.2 Regression
lrst.rbst4.e <- f.rbst4.early[, felm(issale ~ loss | cube.symbol + stck + hold.time)]
lrst.rbst4.l <- f.rbst4.late[, felm(issale ~ loss | cube.symbol + stck + hold.time)]
lrst.rbst4 <- f.rbst4[, felm(issale ~ loss + I(loss * second.half) | cube.symbol + stck + hold.time)]

## 4.2.1 Select individuals before and after follow people both had DE 
#f.surv.early.m <- f.surv.early[gain == 1 & ishold == 1, .(cube.symbol = unique(cube.symbol), d.e.g = 1)
    #][f.surv.early[loss == 1 & issale == 1, .(cube.symbol = unique(cube.symbol), d.e.l = 1)]
    #, on = "cube.symbol", nomatch = 0]


#f.surv.late.m <- f.surv.late[gain == 1 & ishold == 1, .(cube.symbol = unique(cube.symbol), d.e.g = 1)
    #][f.surv.late[loss == 1 & issale == 1, .(cube.symbol = unique(cube.symbol), d.e.l = 1)]
    #, on = "cube.symbol", nomatch = 0]

#f.surv.m <- f.surv.early.m[f.surv.late.m, on = "cube.symbol", nomatch = 0]

#f.rbst2 <- f.surv.m[f.surv.flw, on = "cube.symbol", nomatch = 0]
#f.rbst2.early <- f.rbst2[first.half == 1]
#f.rbst2.late <- f.rbst2[second.half == 1]
#rm(f.surv.early.m, f.surv.late.m, f.surv.m)

# 4.2.2 Regression
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

# 5. Robust test 5
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
