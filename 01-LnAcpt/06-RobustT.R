library(GGally)
library(survival)
library(Matrix)
library(lfe)
library(stargazer)
ld(f.surv.flw)
f.surv.flw[, tag := ifelse(date - as.Date(follow.date) > 0, 1, 0), keyby = .(cube.symbol, stck)
    ][, first.half := ifelse(tag == 0, 1, 0)
    ][, second.half := ifelse(tag == 1, 1, 0)
    ][, loss := ifelse(gain == 1, 0, 1)
    ][, ishold := ifelse(issale == 1, 0, 1)] # sign the two-stage and loss&hold data

f.surv.early <- f.surv.flw[first.half == 1]
f.surv.late <- f.surv.flw[second.half == 1]

# 1. Robust test 1
# 1.1 Select the people who had trading before and after first follow
f.cube.early <- f.surv.early[, .(cube.symbol = unique(cube.symbol))]
f.cube.late <- f.surv.late[, .(cube.symbol = unique(cube.symbol))]
f.cube <- f.cube.early[f.cube.late, on = "cube.symbol", nomatch = 0]
f.rbst1 <- f.surv.flw[f.cube, on = "cube.symbol", nomatch = 0]
    #[is.na(target.weight), weight.change := 0
#][!is.na(target.weight), weight.change := target.weight - prev.weight.adjusted]
f.rbst1.early <- f.rbst1[first.half == 1]
f.rbst1.late <- f.rbst1[second.half == 1]
rm(f.cube.early, f.cube.late, f.cube)

# 1.2 Survival analysis & Regress
srvl.rbst1.e <- survfit(Surv(hold.time, issale) ~ isgain, data = f.rbst1.early[hold.time < 200])
srvl.rbst1.l <- survfit(Surv(hold.time, issale) ~ isgain, data = f.rbst1.late[hold.time < 200])
ggsurv(srvl.rbst1.e, lty.est = c(1, 2), surv.col = 1, plot.cens = F, xlab = "holding period (days)", ylab = "fraction of holding position(1st-Stage)", main = "BiTrade Sample")
ggsurv(srvl.rbst1.l, lty.est = c(1, 2), surv.col = 1, plot.cens = F, xlab = "holding period (days)", ylab = "fraction of holding position(2nd-Stage)", main = "BiTrade Sample")

rst.rcox1.e <- coxph(Surv(hold.time, issale == 1) ~ gain, data = f.rbst1.early)
rst.rcox1.l <- coxph(Surv(hold.time, issale == 1) ~ gain, data = f.rbst1.late)
rst.rcox1 <- coxph(Surv(hold.time, issale == 1) ~ I(gain * first.half) + I(gain * second.half), data = f.rbst1)

rst.rbst1.e <- f.rbst1.early[, felm(issale ~ gain | hold.time + stck + cube.symbol)] #%>% summary()
rst.rbst1.l <- f.rbst1.late[, felm(issale ~ gain | stck + cube.symbol + hold.time)] #%>% summary()
rst.rbst1 <- f.rbst1[, felm(issale ~ gain + second.half + I(gain * second.half) | stck + cube.symbol + hold.time)] #%>% summary()

list(rst.rcox1.e, rst.rcox1.l, rst.rcox1, rst.rbst1.e, rst.rbst1.l, rst.rbst1) %>% stargazer(out = "BiTrade sample.doc", type = "html", title = "BiTrade Sample", dep.var.caption = "Dependent Variable: Sale", dep.var.labels.include = F, covariate.labels = c("Gain", "Gain*Pre.follow", "Pro.follow", "Gain*Pro.follow"),
omit.stat = c("LL", "ser", "f"), model.names = T, single.row = F)

# 2.Robust test 2 
# 2.1 Select individuals before and after follow people both had DE 
f.surv.early.m<- f.surv.early[gain == 1 & ishold == 1, .(cube.symbol = unique(cube.symbol), d.e.g = 1)
    ][f.surv.early[loss == 1 & issale == 1, .(cube.symbol = unique(cube.symbol), d.e.l = 1)]
    , on = "cube.symbol", nomatch = 0]


f.surv.late.m <- f.surv.late[gain == 1 & ishold == 1, .(cube.symbol = unique(cube.symbol), d.e.g = 1)
    ][f.surv.late[loss == 1 & issale == 1, .(cube.symbol = unique(cube.symbol), d.e.l = 1)]
    , on = "cube.symbol", nomatch = 0]

f.surv.m <- f.surv.early.m[f.surv.late.m, on = "cube.symbol", nomatch = 0]

f.rbst2 <- f.surv.m[f.surv.flw, on = "cube.symbol", nomatch = 0]
f.rbst2.early <- f.rbst2[first.half == 1]
f.rbst2.late <- f.rbst2[second.half == 1]
rm(f.surv.early.m, f.surv.late.m, f.surv.m)

# 2.2 Survival analysis & Regress 
srvl.rbst2.e <- survfit(Surv(hold.time, issale) ~ isgain, data = f.rbst2.early[hold.time < 200])
srvl.rbst2.l <- survfit(Surv(hold.time, issale) ~ isgain, data = f.rbst2.late[hold.time < 200])

ggsurv(srvl.rbst2.e, lty.est = c(1, 2), surv.col = 1, plot.cens = F, xlab = "holding period (days)", ylab = "fraction of holding position(1st-Stage)", main = "BiTradeDE Sample")
ggsurv(srvl.rbst2.l, lty.est = c(1, 2), surv.col = 1, plot.cens = F, xlab = "holding period (days)", ylab = "fraction of holding position(2nd-Stage)", main = "BiTradeDE Sample")

rst.rcox2.e <- coxph(Surv(hold.time, issale) ~ gain, data = f.rbst2.early)
rst.rcox2.l <- coxph(Surv(hold.time, issale) ~ gain, data = f.rbst2.late)
rst.rcox2 <- coxph(Surv(hold.time, issale) ~ I(gain * first.half) + I(gain*second.half), data = f.rbst2)

rst.rbst2 <- f.rbst2[, felm(issale ~ gain + second.half + I(gain * second.half)| stck + cube.symbol + hold.time)] #%>% summary()
rst.rbst2.e <- f.rbst2.early[, felm(issale ~  gain | stck + cube.symbol + hold.time)] #%>% summary()
rst.rbst2.l <- f.rbst2.late[, felm(issale ~ gain | stck + cube.symbol + hold.time)] #%>% summary()

list(rst.rcox2.e, rst.rcox2.l, rst.rcox2, rst.rbst2.e, rst.rbst2.l, rst.rbst2) %>% stargazer(out = "BiTradeDE sample.doc", type = "html", title = "BiTradeDE Sample", dep.var.caption = "Dependent Variable: Sale", dep.var.labels.include = F, covariate.labels = c("Gain", "Gain*Pre.follow", "Pro.follow", "Gain*Pro.follow"),
omit.stat = c("LL", "ser", "f"), model.names = T, single.row = F)

# 3. Robust test 3
# 3.1 Calculate the ratio change of the stock in portfolio
f.prt <- f.surv.flw[hold.price.lst != 0, prt.chng := (Clsprc - hold.price.lst) / hold.price.lst
    ][issale == 1, prt.chng :=ifelse(hold.price.lst !=0, (price - hold.price.lst) / hold.price.lst, 0)
    ]

f.cube.early <- f.surv.early[, .(cube.symbol = unique(cube.symbol))]
f.cube.late <- f.surv.late[, .(cube.symbol = unique(cube.symbol))]
f.cube <- f.cube.early[f.cube.late, on = "cube.symbol", nomatch = 0]
f.rbst3 <- f.cube[f.prt, on = "cube.symbol", nomatch = 0]

# 3.2 Regress
rst.rbst3 <- f.rbst3[, felm(issale ~ I(gain * second.half) + second.half + gain | stck + cube.symbol + prt.chng + hold.time)] %>% summary()

# 4. Robust test 4
# The loss price regress on holding behavior
# 4.1.1 Select the people who had trading before and after first follow
f.cube.early <- f.surv.early[, .(cube.symbol = unique(cube.symbol))]
f.cube.late <- f.surv.late[, .(cube.symbol = unique(cube.symbol))]
f.cube <- f.cube.early[f.cube.late, on = "cube.symbol", nomatch = 0]
f.rbst1 <- f.surv.flw[f.cube, on = "cube.symbol", nomatch = 0]
f.rbst1.early <- f.rbst1[first.half == 1]
f.rbst1.late <- f.rbst1[second.half == 1]
rm(f.cube.early, f.cube.late, f.cube)

# 4.1.2 Regression
lrst.rbst1.e <- f.rbst1.early[, felm(ishold ~ loss | cube.symbol + stck + hold.time)]
lrst.rbst1.l <- f.rbst1.late[, felm(ishold ~ loss | cube.symbol + stck + hold.time)]
lrst.rbst1 <- f.rbst1[, felm(ishold ~ loss + second.half + I(loss * second.half) | cube.symbol + stck + hold.time)]

# 4.2.1 Select individuals before and after follow people both had DE 
f.surv.early.m <- f.surv.early[gain == 1 & ishold == 1, .(cube.symbol = unique(cube.symbol), d.e.g = 1)
    ][f.surv.early[loss == 1 & issale == 1, .(cube.symbol = unique(cube.symbol), d.e.l = 1)]
    , on = "cube.symbol", nomatch = 0]


f.surv.late.m <- f.surv.late[gain == 1 & ishold == 1, .(cube.symbol = unique(cube.symbol), d.e.g = 1)
    ][f.surv.late[loss == 1 & issale == 1, .(cube.symbol = unique(cube.symbol), d.e.l = 1)]
    , on = "cube.symbol", nomatch = 0]

f.surv.m <- f.surv.early.m[f.surv.late.m, on = "cube.symbol", nomatch = 0]

f.rbst2 <- f.surv.m[f.surv.flw, on = "cube.symbol", nomatch = 0]
f.rbst2.early <- f.rbst2[first.half == 1]
f.rbst2.late <- f.rbst2[second.half == 1]
rm(f.surv.early.m, f.surv.late.m, f.surv.m)

# 4.2.2 Regression
lrst.rbst2.e <- f.rbst2.early[, felm(ishold ~ loss | cube.symbol + stck + hold.time)]
lrst.rbst2.l <- f.rbst2.late[, felm(ishold ~ loss | cube.symbol + stck + hold.time)]
lrst.rbst2 <- f.rbst2[, felm(ishold ~ loss + second.half + I(loss * second.half) | cube.symbol + stck + hold.time)]

list(lrst.rbst1.e, lrst.rbst1.l, lrst.rbst1, lrst.rbst2.e, lrst.rbst2.l, lrst.rbst2) %>% stargazer(out = "Loss-Hold sample.doc", type = "html", title = "Loss-Hold Sample", dep.var.caption = "Dependent Variable: Hold", dep.var.labels.include = F, covariate.labels = c("Loss", "Pro.follow", "Loss*Pro.follow"),
omit.stat = c("LL", "ser", "f"), model.names = T, single.row = F)

