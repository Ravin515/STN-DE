library(GGally)
library(survival)
library(Matrix)
library(lfe)
library(texreg)
library(stargazer)

ld(f.surv.flw)
#split the trades by the first-follow date
f.surv.flw[, tag := ifelse(date - as.Date(follow.date) > 0, 1, 0), keyby = .(cube.symbol, stck)
    ][, first.half := ifelse(tag == 0, 1, 0)
    ][, second.half := ifelse(tag == 1, 1, 0)
    ][, loss := ifelse(gain == 1, 0, 1)
    ][, ishold := ifelse(issale == 1, 0, 1)
    ][, hldt.ls.7 := ifelse(hold.time < 7, 1, 0)
    ][, hold.time.7 := ifelse(hldt.ls.7 == 1, hold.time, 8)
    ][isgain == 'gain' & first.half == 1, state := "pre-follow-gain"
    ][isgain == 'loss' & first.half == 1, state := "pre-follow-loss"
    ][isgain == 'gain' & second.half == 1, state := "post-follow-gain"
    ][isgain == 'loss' & second.half == 1, state := "post-follow-loss"]

# import momentum variable
f.mmt <- fread("Momentum.csv", encoding = "UTF-8")
setnames(f.mmt, 1:2, c("date", "mmt"))
f.mmt[, date := str_replace_all(date, "/", "-")
    ][, date := as.Date(date, "%Y-%m-%d")]
f.surv.flw <- f.mmt[f.surv.flw, on = "date", nomatch = 0]

f.surv.early <- f.surv.flw[first.half == 1]
f.surv.late <- f.surv.flw[second.half == 1]
#ely.survival.g <- survfit(Surv(hold.time, issale) ~ isgain, data = f.surv.early[hold.time < 175])
#lat.survival.g <- survfit(Surv(hold.time, issale) ~ isgain, data = f.surv.late[hold.time < 175])
#ggsurv(ely.survival.g, lty.est = c(1, 2), surv.col = 1, plot.cens = F, xlab = "holding period (days)", ylab = "fraction of holding position(1st-Stage)", main = "Pooled Sample")
#ggsurv(lat.survival.g, lty.est = c(1, 2), surv.col = 1, plot.cens = F, xlab = "holding period (days)", ylab = "fraction of holding position(2nd-Stage)", main = "Pooled Sample")

# main regression
# Select the people who had trading before and after first follow
f.cube.early <- f.surv.early[, .(cube.symbol = unique(cube.symbol))]
f.cube.late <- f.surv.late[, .(cube.symbol = unique(cube.symbol))]
f.cube <- f.cube.early[f.cube.late, on = "cube.symbol", nomatch = 0]
f.main <- f.surv.flw[f.cube, on = "cube.symbol", nomatch = 0]
#[is.na(target.weight), weight.change := 0
#][!is.na(target.weight), weight.change := target.weight - prev.weight.adjusted]

# Select two same trading period 
f.main[, pre.period := as.Date(follow.date) - min(date), by = .(cube.symbol)]
f.main.early <- f.main[first.half == 1]
f.main.late <- f.main[second.half == 1 & date - as.Date(follow.date) <= pre.period]
rm(f.cube.early, f.cube.late, f.cube)

# Survival analysis & Regress
library(ggthemes)
gg.main <- survfit(Surv(hold.time, issale) ~ state, data = f.main[hold.time < 200 & (first.half == 1 | (second.half == 1 & date - as.Date(follow.date) <= pre.period))])

d.main <- ggsurv(gg.main,
                             lty.est = c(1, 1, 4, 4),
                             surv.col = c("#CC6666", "#CC6666", "#7777DD", "#7777DD"),
                             plot.cens = F,
                             xlab = "holding period (days)",
                             ylab = "fraction of holding position",
                             main = "",
                             size.est = 1.5,
                             order.legend = T
                            ) +
                #ggplot2 :: scale_linetype_manual(
                             #name = "Phase",
                             #breaks = c("pro.loss", "pro.gain", "pre.loss", "pre.gain"),
                             #labels = c("Control", "Treatment 1", "Treatment 2", "Treatment3")) +
                #scale_colour_discrete(
                             #name = "Phase",
                             #labels = c("Control", "Treatment 1", "Treatment 2", "Treatment3")) +
                                    
                theme_grey() +
                theme(
                            axis.title.x = element_text(size = 24, margin = margin(t = 20, r = 0, b = 20, l = 0)),
                            axis.title.y = element_text(size = 24, margin = margin(t = 0, r = 20, b = 0, l = 20)),
                            axis.text = element_text(size = 24),
                            panel.border = element_rect(linetype = 1, fill = NA),
                            legend.title = element_blank(),
                            legend.position = "bottom",
                            legend.direction = "horizontal",
                            legend.text = element_text(size = 24),
                            legend.key = element_rect(size = 0.5, colour = "black", fill = "white"),
                            legend.key.size = unit(1, 'cm'),
                            legend.spacing.x = unit(0.5, 'cm'),
                            legend.spacing.y = unit(2, 'cm'),
                            legend.box = "horizontal",
                            legend.box.background = element_rect(size = 1, colour = "black", fill = "white")
                            )
         

rst.cox.e <- coxph(Surv(hold.time, issale == 1) ~ gain, data = f.main.early)
rst.cox.l <- coxph(Surv(hold.time, issale == 1) ~ gain, data = f.main.late)
rst.cox <- coxph(Surv(hold.time, issale == 1) ~ gain + second.half + I(gain * second.half), data = f.main)

list(rst.cox.e, rst.cox.l, rst.cox) %>%
    stargazer(
        out = "rst.cox.doc",
        type = "html",
        title = "Main Regression",
        dep.var.labels.include = F,
        covariate.labels = c("Gain", "Hold.week", "Gain*Hold.week", "Gain*Hold.week*Pre.follow", "Gain*Hold.week*Pro.follow"),
        column.labels = c("Pre-follow", "Pro-follow", "Full sample"),
        omit.stat = c("LL", "lr", "wald", "max.rsq", "logrank"),
        model.names = T,
        single.row = F
    )

rst.main.e <- f.main.early[, felm(issale ~ gain + I(gain * hldt.ls.7) | stck + cube.symbol + hold.time)] %>% summary()
rst.main.l <- f.main.late[, felm(issale ~ gain + I(gain * hldt.ls.7) | stck + cube.symbol + hold.time)] %>% summary()
rst.main0 <- f.main[, felm(issale ~ gain + second.half + I(gain * second.half) | cube.symbol + stck + hold.time)] %>% summary()
rst.main1 <- f.main[, felm(issale ~ gain + I(gain * hldt.ls.7) + second.half + I(gain * second.half * hldt.ls.7) | cube.symbol + stck + hold.time)]
rst.main2 <- f.main[, felm(issale ~ gain + I(gain * hldt.ls.7) + second.half + I(gain * second.half * hldt.ls.7) + mmt | cube.symbol + stck + hold.time)]

list(rst.main.e, rst.main.l, rst.main0, rst.main1, rst.main2) %>%
    stargazer(out = "rst.felm.doc",
        type = "html",
        title = "Fixed effect OLS",
        dep.var.caption = "Dependent Variable: Sale",
        dep.var.labels.include = F,
        column.labels = c("Pre-follow", "Pro-follow", "Both side", "Full Sample", "Full Sample"),
        covariate.labels = c("Gain", "Gain*Hold.week", "Pro.follow", "Gain*Hold.week*Pro.follow", "Momentum"),
        model.names = F,
        single.row = F,
        add.lines = list(c("Trader FE", "Yes", "Yes", "Yes", "Yes", "Yes"), c("Hold period FE", "Yes", "Yes", "Yes", "Yes", "Yes"), c("Stock FE", "Yes", "Yes", "Yes", "Yes", "Yes")))