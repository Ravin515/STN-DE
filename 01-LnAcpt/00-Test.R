ld(sp.rb.ef, T)

# Calculate the buy.price as the last buy price
sp.rb <- sp.rb[issale == 0, ':='(buy.at = created.at, buy.price = price), by = .(cube.symbol, stock.symbol)
    ][order(cube.symbol, stock.symbol, created.at)
    ][, ":="(buy.at = na.locf(buy.at, na.rm = F), buy.price = na.locf(buy.price, na.rm = F)), keyby = .(cube.symbol, stock.symbol)]

sp.rb <- sp.rb[!is.na(buy.at) & buy.price != price
    ][, ':='(ret = price - buy.price, hold.time = as.numeric(difftime(created.at, buy.at, units = "days"))), keyby = .(cube.symbol, stock.symbol)
    ][, isgain := ifelse(ret > 0, 1, 0)]


#Calculate the hold.time
sp.rb.ef <- sp.rb.ef[issale == 0, buy.at := created.at, by = .(cube.symbol, stock.symbol)
    ][order(cube.symbol, stock.symbol, created.at)
    ][, buy.at := na.locf(buy.at, na.rm = F), keyby = .(cube.symbol, stock.symbol)
    ][!is.na(buy.at), hold.time := as.numeric(difftime(created.at, buy.at, units = "days")), by = .(cube.symbol, stock.symbol)]

#Calculate the isgain 1 or 0 by the hold.price
sp.rb.d <- sp.rb.ef[, hold.price.lt := shift(hold.price, 1, fill = 0), keyby = .(cube.symbol, stock.symbol)]
sp.rb.d <- sp.rb.d[issale == 1, isgain := ifelse(price - hold.price.lt > 0, 2, 1), by = .(cube.symbol, stock.symbol)
    ][issale == 1, .(id, stock.name, stock.symbol, price, target.weight, proactive, prev.weight.adjusted, cube.symbol, cube.type, created.at, issale, hold.time, isgain)]

# test the hold.price function
sp.rb.test <- sp.rb.ef[cube.symbol == "SP1000002" & stock.symbol == "SZ002146", .(price, prev.weight.adjusted, target.weight)]


make_hp <- function(n, price, pwa, tw) {
    if (tw != 0) {
        hp[n] <- (hp[n - 1] * pwa + (tw - pwa) * abs(price - hp[n - 1])) / tw
    }
    else {
        hp[n] = 0
    }

    hp
}

hp <- price[1]

for (i in 2:length(price)) {
    hp <- make_hp(i, price[i], pwa[i], tw[i])
}

f.surv.flw[, uniqueN(cube.symbol)]
#calculate the DE people from early part
a <- f.surv.early[loss == 1 & ishold == 1, .N, by = cube.symbol
    ][f.surv.early[gain == 1 & issale == 1, .N, by = cube.symbol
    ], on = .(cube.symbol), nomatch = 0]

a[, uniqueN(cube.symbol)]

#calculate the DE people from late part
b <- f.surv.late[loss == 1 & ishold == 1, .N, by = cube.symbol
    ][f.surv.late[gain == 1 & issale == 1, .N, by = cube.symbol
    ], on = .(cube.symbol), nomatch = 0]

b[, uniqueN(cube.symbol)]

c <- a[b, on = .(cube.symbol), nomatch = 0]

d <- f.surv.early[, unique(cube.symbol)
    ][f.surv.late[, unique(cube.symbol)], on = "cube.symbol", nomatch = 0]

x <- f.surv.early[, unique(cube.symbol)] %>% as.data.table()
y <- f.surv.late[, unique(cube.symbol)] %>% as.data.table()
z <- x[y, on = ".", nomatch = 0]

# 07-Learning
a <- f.nwl.cntr[!is.na(out) & date == "2018-03-27"
    ][, .(sp.out = unlist(str_split(out, ','))), keyby = .(cube.symbol)
    ][, id := seq(1, .N, by = 1)
    ][, net.out := str_c(cube.symbol, sp.out, sep = ','), keyby = .(id)]
g <- graph_(cbind(a$cube.symbol, a$sp.out), from_edgelist(), directed = FALSE)
v <- page_rank(g)
value <- setDT(as.data.frame(v$vector), keep.rownames = TRUE)
setnames(value, 1:2, c("sp.out", "value"))
int <- value[a, on = .(sp.out), nomatch = NA]
ln.cntr <- int[, .(ln.cntr = mean(value)), keyby = .(cube.symbol)]

# 09-Odean
a <- f.surv.flw[cube.symbol == "SP1000002"
    ][order(date)]
sadd <- function(x, y) {
    if (length(x) == 1) {
        x
    }
    else {
        if (y == -1) {
        Reduce(setdiff, Reduce(union, x, accumulate = T))
        }
        else {
            Reduce(union, x, accumulate = T)
        }
    }
}

a[, stck.prtfl := {
    stck <- stck;
    num.dymc <- num.dymc;

    sadd <- function(n, stck, num.dymc) {
        if (num.dymc[n] == 0) {
            a <- Reduce(union, stck, accumulate = T)
        } else {
            a <- Reduce(union, stck, accumulate = T)
            a[-n]
        }
        a
    };
    list(a)
}]

b <- a[['stck.prtfl']]
d <- b[[2]]
a[ppr.gain == 1, ppr.gain.stck := sadd(stck)]



## Cleaning uploading data 
# Data for Fig3, Fig4, Table 2, Table 3
styleer::ld(f.main, force = T)
sample1 <- f.main[active.day >= 0, .SD
    ][, .(cube.symbol, date, trd.num, mmt, active.day, hold.period = hold.time, pre.period, stkcd = stck, sale = issale, gain, follow.date, post.follow = second.half, pre.follow = first.half)
    ][order(cube.symbol, date)]
styleer::sv(sample1)

# Data for Table1, Table 4
ret.mst.1806 <- readRDS("ret.mst.1806.rds")
load("f.nwl.reg.Rdata")
sample2 <- ret.mst.1806[cube.type == 'SP', .(ret = value[.N]), by = .(cube.symbol, date)
    ][, unique(.SD)
    ][f.nwl.reg, on = .(cube.symbol, date)
    ][, unique(.SD)
    ][, .(cube.symbol, date, cntra = ln.cntr, followings = oud, followers = ind, trd.num, mmt, active.day, hold.period = hold.time, pre.period, stkcd = stck, sale = issale, gain, ret)
    ][order(cube.symbol, date)
    ][, unique(.SD)
    ][active.day >= 0, .SD
    ]

styleer::sv(sample2)

## Test 
a <- f.nwl.reg[sale == 1 & hold.price != 0, issale := 0]
feglm(sale ~ gain + post.follow + I(gain * post.follow) + mmt + as.numeric(active.day / 365) + as.numeric(trd.num / 1000) | cube.symbol + stkcd + hold.period, a[pre.follow == 1 | (post.follow == 1 & date - as.Date(follow.date) <= pre.period)], binomial("logit")) %>% summary()
feglm(sale ~ gain + post.follow + I(gain * post.follow) + mmt + as.numeric(active.day / 365) + as.numeric(trd.num / 1000) | cube.symbol + stkcd + hold.period, a, binomial("logit")) %>% summary()

library(survival)
a <- a[gain == 1 & pre.follow == 1, state := "pre-follow (gain)"
    ][gain == 0 & pre.follow == 1, state := "pre-follow (loss)"
    ][gain == 1 & post.follow == 1, state := "post-follow (gain)"
    ][gain == 0 & post.follow == 1, state := "post-follow (loss)"]
gg.main <- survfit(Surv(hold.period, issale) ~ state, data = a[hold.period < 200 & (pre.follow == 1 | post.follow == 1)])
library(ggplot2)
library(GGally)
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
rst.cox.e <- coxph(Surv(hold.period, issale == 1) ~ gain, data = a[pre.follow == 1])
rst.cox.l <- coxph(Surv(hold.period, issale == 1) ~ gain, data = a[post.follow == 1])
rst.cox <- coxph(Surv(hold.period, issale == 1) ~ gain + post.follow + I(gain * post.follow), data = a[pre.follow == 1 | (post.follow == 1 & date - as.Date(follow.date) <= pre.period)])

feglm(issale ~ gain + post.follow + I(gain * post.follow) + mmt + as.numeric(active.day / 365) + as.numeric(trd.num / 1000) | cube.symbol + stck + hold.period, f.nwl.reg[pre.follow == 1 | (post.follow == 1 & date - as.Date(follow.date) <= pre.period)], binomial("logit")) %>% summary()
