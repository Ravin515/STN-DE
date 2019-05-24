#split the trades by the first-follow date
library(GGally)
library(survival)
library(Matrix)
library(lfe)
library(texreg)
library(stargazer)
ld(f.surv.flw)
f.surv.flw[, tag := ifelse(date - as.Date(follow.date) > 0, 1, 0), keyby = .(cube.symbol, stck)
    ][, first.half := ifelse(tag == 0, 1, 0)
    ][, second.half := ifelse(tag == 1, 1, 0)
    ][, loss := ifelse(gain == 1, 0, 1)
    ][, ishold := ifelse(issale == 1, 0, 1)]

f.surv.early <- f.surv.flw[first.half == 1]
f.surv.late <- f.surv.flw[second.half == 1]

ely.survival.g <- survfit(Surv(hold.time, issale) ~ isgain, data = f.surv.early[hold.time < 175])
lat.survival.g <- survfit(Surv(hold.time, issale) ~ isgain, data = f.surv.late[hold.time < 175])

ggsurv(ely.survival.g, lty.est = c(1, 2), surv.col = 1, plot.cens = F, xlab = "holding period (days)", ylab = "fraction of holding position(1st-Stage)", main = "Pooled Sample")
ggsurv(lat.survival.g, lty.est = c(1, 2), surv.col = 1, plot.cens = F, xlab = "holding period (days)", ylab = "fraction of holding position(2nd-Stage)", main = "Pooled Sample")

# Cox proportional hazard model
# The gain performance of DE
rst.mcox.e <- coxph(Surv(hold.time, issale) ~ gain, data = f.surv.early)
rst.mcox.l <- coxph(Surv(hold.time, issale) ~ gain, data = f.surv.late)
rst.mcox <- coxph(Surv(hold.time, issale) ~  I(gain * first.half) + I(gain * second.half), data = f.surv.flw)


rst.main.e <- f.surv.early[, felm(issale ~ gain | cube.symbol + stck + hold.time)]
rst.main.l <- f.surv.late[, felm(issale ~ gain | cube.symbol + stck + hold.time)]
rst.main <- f.surv.flw[, felm(issale ~ gain + second.half + I(gain * second.half) | cube.symbol + stck + hold.time)] #%>% summary()


#list(rst.mcox.e, rst.mcox.l, rst.mcox, rst.main.e, rst.main.l, rst.main) %>% htmlreg(file = "full sample.doc", custom.model.names = c("Cox prop.harzard", "Cox prop.harzard", "Cox prop.harzard", "OLS", "OLS", "OLS"), custom.coef.names = c("gain", "gain*pre.follow", "gain*pro.follow", "pro.follow"), digits = 4)

list(rst.mcox.e, rst.mcox.l, rst.mcox) %>% stargazer(out = "Cox full sample.doc", type = "html", title = "Full Sample", dep.var.caption = "Dependent Variable: Sale", dep.var.labels.include = F,
omit.stat = c("LL", "ser", "f"), model.names = T, single.row = F)

list(rst.main.e, rst.main.l, rst.main) %>% stargazer(out = "OLS full sample.doc", type = "html", title = "Full Sample", dep.var.caption = "Dependent Variable: Sale", dep.var.labels.include = F,
omit.stat = c("LL", "ser", "f"), model.names = T, single.row = F)

list(rst.mcox.e, rst.mcox.l, rst.mcox, rst.main.e, rst.main.l, rst.main) %>% stargazer(out = "Full sample.doc", type = "html", title = "Full Sample", dep.var.caption = "Dependent Variable: Sale", dep.var.labels.include = F, covariate.labels = c("Gain", "Gain*Pre.follow", "Pro.follow", "Gain*Pro.follow"),
omit.stat = c("LL", "ser", "f"), model.names = T, single.row = F)

## The loss performance of DE
coxph(Surv(hold.time, ishold) ~ loss, data = f.surv.early)
coxph(Surv(hold.time, ishold) ~ loss, data = f.surv.late)
coxph(Surv(hold.time, ishold) ~ I(loss * first.half) + I(loss * second.half), data = f.surv.flw)

a <- f.surv.early[, felm(ishold ~ loss | cube.symbol + stck + hold.time)]
b <- f.surv.late[, felm(ishold ~ loss | cube.symbol + stck + hold.time)]
c <- f.surv.flw[, felm(ishold ~ loss + second.half + I(gain * second.half) | cube.symbol + stck + hold.time)]