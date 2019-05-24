# Survival analysis of Kaplan-Meier
library(GGally)
library(survival) 
ld(f.surv, T)
pool.survival <- survfit(Surv(hold.time, issale) ~ isgain, data = f.surv)
ggsurv(pool.survival, lty.est = c(1, 2), surv.col = 1, plot.cens = F, xlab = "holding period (days)", ylab = "fraction of holding position")

#split the trades by the mid-date 
f.surv[, tag := ifelse(date - mean.Date(date) > 0, 1, 0), keyby = .(cube.symbol, stck)
    ][, first.half := ifelse(tag ==0, 1, 0)
    ][, second.half := ifelse(tag ==1, 1, 0)
    ][, ':='(fst.hf.gain = first.half*gain, snd.hf.gain = second.half*gain)]
f.surv.early <- f.surv[tag == 0]
f.surv.late <- f.surv[tag == 1]

ely.survival <- survfit(Surv(hold.time, issale) ~ isgain, data = f.surv.early)
lat.survival <- survfit(Surv(hold.time, issale) ~ isgain, data = f.surv.late)

ggsurv(ely.survival, lty.est = c(1, 2), surv.col = 1, plot.cens = F, xlab = "holding period (days)", ylab = "fraction of holding position")

ggsurv(lat.survival, lty.est = c(1, 2), surv.col = 1, plot.cens = F, xlab = "holding period (days)", ylab = "fraction of holding position")

#z <- draw[!is.na(benefit), .(n = .N, hold.time = hold.time[.N]), keyby = .(benefit, grp)
    #][, ':='(pct = 1 - cumsum(n) / sum(n)), keyby = .(benefit)]

#z %>%
    #ggplot(aes(x = hold.time, y = pct, color = benefit)) +
    #geom_line() +
    #geom_point()

# Cox proportional hazard model
coxph(Surv(hold.time, issale) ~ gain, data = f.surv)
coxph(Surv(hold.time, issale) ~ fst.hf.gain + snd.hf.gain, data = f.surv)