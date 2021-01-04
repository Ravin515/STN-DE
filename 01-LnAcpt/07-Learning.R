library(data.table)
library(stringr)
library(igraph)
library(lubridate)
styleer::ld(user.wnwk.sp)
styleer::ld(f.surv.flw)

# 7.1 Calculate learning intensity
# Caculate in-degree & out-degree
f.nwl.ind <- user.wnwk.sp[, .(date = as.Date(follow.date), cube.symbol = from.cube.symbol, to.cube.symbol)
    ][f.surv.flw, on = .(date, cube.symbol), nomatch = NA
    ][order(cube.symbol, date)
    ][, out := ifelse(!is.null(to.cube.symbol), str_c(unlist(to.cube.symbol), collapse = ","), NA), keyby = .(cube.symbol, date)
    ][, .(sp.out = unlist(str_split(out, ','))), keyby = .(cube.symbol, date)
    ][!is.na(sp.out), .SD[order(sp.out, date)]
    ][, .(indegree = list(cube.symbol)), keyby = .(sp.out, date)]
f.nwl.ind <- f.nwl.ind[, setnames(.SD, 1, "cube.symbol")]
setnames(user.wnwk.sp, 1:3, c("cube.symbol", "date", "outdegree"))
f.nwl.ints <- f.nwl.ind[user.wnwk.sp[, date := as.Date(date)], on = .(date, cube.symbol)]
#intec <- f.nwl.ind[user.wnwk.sp[, date := as.Date(date)], on = .(date, cube.symbol), nomatch = 0
    #][, unique(cube.symbol)]
#f.surv.flw[, unique(cube.symbol)] %>% intersect(intec) %>% length()
f.nwl.ints[!is.null(indegree), ind := indegree %>% unlist() %>% length(), by = .(cube.symbol, date)]
f.nwl.ints[!is.null(outdegree), oud := outdegree %>% unlist() %>% length(), by = .(cube.symbol, date)]

sv(f.nwl.ints)
rm(user.wnwk.sp)


# 7.2 Calculate learning centrality
styleer::ld(user.wnwk.sp)
tim <- CJ(cube.symbol = unique(f.nwl.ints$cube.symbol), date = seq(as.Date('2016-06-24'), as.Date('2018-03-27'), by = "day"))


# revise some variable names and unlist 'to.cube.symbol' list
f.nwl.cntr<- user.wnwk.sp[, ":="(date = follow.date, cube.symbol = from.cube.symbol)
    ][, out := ifelse(!is.null(to.cube.symbol), str_c(unlist(to.cube.symbol), collapse = ","), NA), keyby = .(cube.symbol, date)
    ][, ":="(from.cube.symbol = NULL, follow.date = NULL, to.cube.symbol = NULL)
    ][tim, on = .(cube.symbol, date), nomatch = NA]

# flat 'sp.out' to every single observation
pg_rk <- f.nwl.cntr[!is.na(out)
    ][, .(sp.out = unlist(str_split(out, ','))), keyby = .(cube.symbol, date)
    ][, id := seq(1, .N, by = 1)
    ][, net.out := str_c(cube.symbol, sp.out, sep = ','), keyby = .(id)] # This row is no useful, just merge two different columns to a new variable. At this time, it is very necessary to create a ID key. 

# Use igraph packages to construct network matrix and calculate PR of every point
grph <- pg_rk[, .(pgrnk = graph_(cbind(cube.symbol, sp.out), from_edgelist(), directed = FALSE)), keyby = .(date)]
pgrk_grph <- grph[, .(pgrnk = page_rank(pgrnk)), keyby = .(date)]
pgrk_grph <- pgrk_grph[, .SD[1], keyby = .(date)]
p <- pgrk_grph[, setDT(as.data.frame(pgrnk), keep.rownames = T), keyby = .(date)] # This row is very important. When PR was calculated, the data structure is a list with rownames, more than that, rownames is a necessary variable to be transformed. First, the list should be transformed into data.frame with rownames, then, use setDT to convert data.frame into data.table with a variable consisted with rownames.

rm(grph, pgrk_grph)
setnames(p, 2:3, c("sp.out", "ln.cntr"))

pg_rnk <- p[pg_rk, on = .(sp.out, date), nomatch = NA]
f.nwl.cntr <- pg_rnk[, .(ln.cntr = mean(ln.cntr)), keyby = .(cube.symbol, date)]
rm(p, pg_rk, pg_rnk, tim, user.wnwk.sp)
sv(f.nwl.cntr)


f.nwl <- f.nwl.cntr[f.nwl.ints[,.(cube.symbol, date, ind, oud)], on = .(cube.symbol, date), nomatch = NA
    ][is.na(ln.cntr), ln.cntr := 0
    ][is.na(oud), oud := 0
    ][is.na(ind), ind := 0]
sv(f.nwl)


# 7.3 Power law distribution
styleer::ld(f.nwl)
styleer::ld(f.surv.flw)
# extract follow date
f.pld <- f.surv.flw[, .(follow.date = as.Date(follow.date) %>% unique()), by = .(cube.symbol)
    ][f.nwl, on = "cube.symbol"]

## Caculate how many SPs ever have ZHs
#ld(r.cube.info.1803)
#zh.sp <- r.cube.info[, .(owner.id, cube.symbol, create.date)
    #][order(owner.id, cube.symbol)
    #]

#sp <- f.pld[date == as.Date(follow.date), .(cube.symbol, oud = oud, ind = ind, follow.date = as.Date(follow.date))
    #][, unique(.SD)
    #]

#merg <- sp[zh.sp, on = .(cube.symbol)
    #][, min.date := min(create.date), by = .(owner.id)
    #][!is.na(follow.date)
    #][order(cube.symbol)]


# power-law curve
t1<- f.nwl.reg[date == as.Date("2016-09-01"), .(cube.symbol, oud = oud, ind = ind, date)
    ][, unique(.SD)
    ]
t2 <- f.nwl.reg[date == as.Date("2017-06-01"), .(cube.symbol, oud = oud , ind = ind, date)
    ][, unique(.SD)
    ]
t3 <- f.nwl.reg[date == as.Date("2018-03-01"), .(cube.symbol, oud = oud, ind = ind, date)
    ][, unique(.SD)
    ]

# in-degree
t1 <- t1[order(ind), .(ind = ind)
    ][, ind_d := .N, by = ind
    ][, unique(.SD)
    ][, ind_cd := {
        a <- vector()
        a[1] <- 1 
        for (i in 2:.N) {
            a[i] <- 1-sum(ind_d[1:i])/sum(ind_d)
        }
        a
    }][, tag := "t1"]
t2 <- t2[order(ind), .(ind = ind)
    ][, ind_d := .N, by = ind
    ][, unique(.SD)
    ][, ind_cd := {
        a <- vector()
        a[1] <- 1
        for (i in 2:.N) {
            a[i] <- 1 - sum(ind_d[1:i]) / sum(ind_d)
        }
        a
    }][, tag := "t2"]
t3 <- t3[order(ind), .(ind = ind)
    ][, ind_d := .N, by = ind
    ][, unique(.SD)
    ][, ind_cd := {
        a <- vector()
        a[1] <- 1
        for (i in 2:.N) {
            a[i] <- 1 - sum(ind_d[1:i]) / sum(ind_d)
        }
        a
    }][, tag := "t3"]
oud <- rbindlist(list(t1, t2, t3), fill = T)

ggplot(oud, aes(x = ind, y = ind_cd, color = tag, fill = tag)) +
    ggtitle("A.") +
    geom_point(size = 2) +
    #geom_smooth() +
    labs(x = "In-degree X", y = "p(x > X)") +
    scale_color_manual(values = c("red", "blue", "green"), labels = c("t1", "t2", "t3")) +
    scale_x_log10(limits = c(1, 1000)) +
    scale_y_log10(limits = c(1e-5, 1)) +
    theme(
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.position = "bottom"
        )
ggsave("Fig 4-A.tiff", device = "tiff", dpi = 300, width = 6, height = 5)

# out-degree
oud_t1 <- t1[order(oud), .(oud = oud)
    ][, oud_d := .N, by = oud
    ][, unique(.SD)
    ][, oud_cd := {
        a <- vector()
        a[1] <- 1
        for (i in 2:.N) {
            a[i] <- 1 - sum(oud_d[1:i]) / sum(oud_d)
        }
        a
    }][, tag := "t1"]
oud_t2 <- t2[order(oud), .(oud = oud)
    ][, oud_d := .N, by = oud
    ][, unique(.SD)
    ][, oud_cd := {
        a <- vector()
        a[1] <- 1
        for (i in 2:.N) {
            a[i] <- 1 - sum(oud_d[1:i]) / sum(oud_d)
        }
        a
    }][, tag := "t2"]
oud_t3 <- t3[order(oud), .(oud = oud)
    ][, oud_d := .N, by = oud
    ][, unique(.SD)
    ][, oud_cd := {
        a <- vector()
        a[1] <- 1
        for (i in 2:.N) {
            a[i] <- 1 - sum(oud_d[1:i]) / sum(oud_d)
        }
        a
    }][, tag := "t3"]
oud <- rbindlist(list(oud_t1, oud_t2, oud_t3), fill = T)
ggplot(oud, aes(x = oud, y = oud_cd, color = tag, fill = tag)) +
    ggtitle("B.") +
    geom_point(size = 3) +
    labs(x = "Out-degree X", y = "p(x > X)") +
    scale_color_manual(values = c("red", "blue", "green"), labels = c("t1", "t2", "t3")) +
    scale_x_log10(limits = c(1, 1000)) +
    scale_y_log10(limits = c(1e-5, 1)) +
    theme(
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.position = "bottom"
        )
ggsave("Fig 4-B.tiff", device = "tiff", dpi = 300, width = 6, height = 5)


#7.4 Regression
library(alpaca)
f.nwl.reg <- f.nwl[f.surv.flw, on = .(cube.symbol, date)
    ][order(cube.symbol, date)]

f.nwl.reg <- f.nwl.reg[is.na(ln.cntr), ln.cntr := 0
    ][is.na(oud), oud := 0
    ][is.na(ind), ind := 0
    ][, pre.period := as.Date(follow.date) - min(date), by = .(cube.symbol)
    ][order(date)
    ][, month := str_c(year(date), "-", str_sub(date, start = 6L, end = 7L))
    ]
tag <- f.nwl.reg[, .(month = unique(month))
    ][, tag := 1:.N]
f.nwl.reg <- tag[f.nwl.reg, on = "month"]
styleer::sv(f.nwl.reg)

# Table 1 last two variables
ind_avr <- f.nwl.reg[first.half == 1 | (second.half == 1 & date - as.Date(follow.date) <= pre.period), .(ind_avr = mean(ind)), by = .(cube.symbol, first.half)]

mean(ind_avr[first.half == 1]$ind) # 1.475
sd(ind_avr[first.half == 1]$ind) # 16.44
mean(ind_avr[first.half == 0]$ind) # 4.288
sd(ind_avr[first.half == 0]$ind) # 31.84
t.test(ind_avr[first.half == 1]$ind, ind_avr[first.half == 0]$ind) # <0.001

oud_avr <- f.nwl.reg[first.half == 1 | (second.half == 1 & date - as.Date(follow.date) <= pre.period), .(oud_avr = mean(oud)), by = .(cube.symbol, first.half)]

mean(oud_avr[first.half == 1]$oud) # 0
sd(oud_avr[first.half == 1]$oud) # 0
mean(oud_avr[first.half == 0]$oud) # 1.99
sd(oud_avr[first.half == 0]$oud) # 3.78
t.test(oud_avr[first.half == 1]$oud, oud_avr[first.half == 0]$oud) # <0.001

# Regression for ind, oud and learning quality

rst.ln.i <- feglm(issale ~ gain + I(log(ind + 1)) + I(gain * (log(ind + 1))) + mmt + as.numeric(active.day / 365) + as.numeric(trd.num / 1000) | stck + cube.symbol + hold.time, data = f.nwl.reg, binomial("logit"))

rst.ln.o <- feglm(issale ~ gain + I(log(oud + 1)) + I(gain * (log(oud + 1))) + mmt + as.numeric(active.day / 365) + as.numeric(trd.num / 1000) | stck + cube.symbol + hold.time, data = f.nwl.reg, binomial("logit"))

rst.ln.oi <- feglm(issale ~ gain + I(log(oud + 2) / log(ind + 2)) + I(gain * (log(oud + 2) / log(ind + 2))) + mmt + as.numeric(active.day / 365) + as.numeric(trd.num / 1000) | stck + cube.symbol + hold.time, data = f.nwl.reg, binomial("logit"))

rst.ln.lq <- feglm(issale ~ gain + I(ln.cntr*100) + I(gain * (ln.cntr*100)) + mmt + as.numeric(active.day / 365) + as.numeric(trd.num / 1000) | stck + cube.symbol + hold.time, data = f.nwl.reg, binomial("logit"))

rst.ln.io <- feglm(issale ~ gain + I(log(ind + 1)) + I(gain * (log(ind + 1))) + I(log(oud + 1)) + I(gain * (log(oud + 1))) + mmt + as.numeric(active.day / 365) + as.numeric(trd.num / 1000) | stck + cube.symbol + hold.time, data = f.nwl.reg, binomial("logit"))

rst.ln.f1 <- feglm(issale ~ gain + I(log(ind + 1)) + I(gain * (log(ind + 1))) + I(log(oud + 1)) + I(gain * (log(oud + 1))) + I(log(oud + 2) / log(ind + 2)) + I(gain * (log(oud + 2) / log(ind + 2))) + I(ln.cntr * 100) + I(gain * (ln.cntr * 100)) + mmt + as.numeric(active.day / 365) + as.numeric(trd.num / 1000) | stck + cube.symbol + hold.time, data = f.nwl.reg, binomial("logit"))

rst.ln.oq <- feglm(issale ~ gain + I(log(oud + 1)) + I(gain * (log(oud + 1))) + I(ln.cntr * 100) + I(gain * (ln.cntr * 100)) + mmt + as.numeric(active.day / 365) + as.numeric(trd.num / 1000) | stck + cube.symbol + hold.time, data = f.nwl.reg, binomial("logit"))

rst.ln.f2 <- feglm(issale ~ gain + I(log(ind + 1)) + I(gain * (log(ind + 1))) + I(log(oud + 1)) + I(gain * (log(oud + 1))) + I(ln.cntr * 100) + I(gain * (ln.cntr * 100)) + mmt + as.numeric(active.day / 365) + as.numeric(trd.num / 1000) | stck + cube.symbol + hold.time, data = f.nwl.reg, binomial("logit"))

library(texreg)

list(rst.ln.i, rst.ln.o, rst.ln.oi, rst.ln.lq, rst.ln.io, rst.ln.f1, rst.ln.oq, rst.ln.f2) %>%
    htmlreg(
            file = "Table4.doc",
#custom.header = list("Pre-follow" = 1:2, 
#"Two-stage" = 3:4, 
#"After two-stage" = 5:6, 
#"Full sample" = 7:8),
            custom.model.names = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)"),
            custom.gof.rows = list("Trader FE" = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                                                 "Holding period FE" = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                                                 "Stock FE" = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")),
            custom.coef.names = c("Gain", "In-degree", "Gain * In-degree", "Momentum", "Active days", "Trade number", "Out-degree", "Gain * Out-degree", "Out-degree/In-degree", "Gain * (Out-degree/In-degree)", "Centrality", "Gain * Centrality"),
            reorder.coef = c(1, 2, 3, 7, 8, 9, 10, 11, 12, 4, 5, 6),
            no.margin = T,
            digits = 3
            )

# Rolling regression window is 6 months 
re.6month <- f.nwl.reg[, {
    l <- list()
    for (t in 1:17) {
        l[[t]] <- as.list(c(feglm(issale ~ gain + I(log(oud + 1)) + I(log(ind + 1)) + I(gain * (log(oud + 1))) + I(gain * (log(ind + 1))) | stck + cube.symbol + hold.time, data = .SD[tag %in% (t:(t + 5))]) %>% coef(), tag = t))
    }
    rbindlist(l)
}]
fwrite(file = "re6month.csv", re.6month)

re.6month <- fread("re6month.csv")[!(tag %in% (1:3))
    ][, to.month := seq(from = as.Date("2017-02-01"), to = as.Date("2018-03-01"), by = "month")
    ][, to.month := str_c(year(to.month), "-", str_sub(to.month, start = 6L, end = 7L))
    ][, setnames(.SD, 2:5, c("oud", "ind", "gain_oud", "gain_ind"))
    ][, from.month := seq(from = as.Date("2016-09-01"), to = as.Date("2017-10-01"), by = "month")
    ][, from.month := str_c(year(from.month), "-", str_sub(from.month, start = 6L, end = 7L))
    ][, ':='(from.month = parse_date_time(from.month, 'ym'), to.month = parse_date_time(to.month, 'ym', locale = Sys.getlocale("LC_TIME")))]

# Out-degree beta_3 estimate
ggplot(data = re.6month, aes(x = to.month, y = gain_oud, group = 1)) +
    ggtitle("A.") +
    #geom_line(size = 1, stat = "identity", type = 2) +
    geom_smooth(span = 0.4, color = "black") +
    #xlab("Calender") +
    ylab("coefficient of out-degree") +
    #scale_x_discrete(breaks = c("2017-03", "2017-08", "2018-01")) +
    theme(
        axis.text.x = element_text(vjust = 0.5),
        axis.title.x = element_blank(),
        #axis.title.x = element_text(vjust = 0.5),
          )
ggsave(file = "Fig 4-A.tiff", device = "tiff", dpi = 300, width = 5, height = 4)

# In-degree beta_5 estimates
ggplot(data = re.6month, aes(x = to.month, y = gain_ind, group = 1)) +
    ggtitle("B.") +
    #geom_line(size = 1, stat = "identity", type = 2) +
    geom_smooth(span = 0.4,color = "black") +
    #geom_smooth(size = 1, stat = "identity",color = "black") +
    ylab("coefficient of in-degree") +
    #scale_x_discrete(breaks = c("2017-03", "2017-08", "2018-01")) +
    theme(
        axis.text.x = element_text(vjust = 0.5),
        axis.title.x = element_blank(),
        #axis.title.x = element_text(vjust = 0.5),
          )
ggsave(file = "Fig 4-B.tiff", device = "tiff", dpi = 300, width = 5, height = 4)

# Rolling regression window is 3 months
re.3month <- f.nwl.reg[, {
    l <- list()
    for (t in 1:20) {
        l[[t]] <- as.list(c(feglm(issale ~ gain + I(log(oud + 1)) + I(log(ind + 1)) + I(gain * (log(oud + 1))) + I(gain * (log(ind + 1))) | stck + cube.symbol + hold.time, data = .SD[tag %in% (t:(t + 3))]) %>% coef(), tag = t))
    }
    rbindlist(l)
}]
fwrite(file = "re3month.csv", re.3month)

