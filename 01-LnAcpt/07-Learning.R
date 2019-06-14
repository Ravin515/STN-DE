ld(user.wnwk.sp)
ld(f.surv.flw)
library(Matrix)
library(lfe)
library(igraph)
library(stargazer)
library(igraph)

# Calculate learning intensity
f.nwl.ints <- user.wnwk.sp[, ":="(date = follow.date, cube.symbol = from.cube.symbol)
    ][f.surv.flw, on = .(date, cube.symbol), nomatch = NA
    ][, f.follow.date := i.follow.date
    ][order(cube.symbol, date)
    ][, i.follow.date := NULL
    ][, x := ifelse(!is.null(to.cube.symbol), str_c(unlist(to.cube.symbol), collapse = ","), NA), keyby = .(cube.symbol, date)
    ][, ":="(from.cube.symbol = NULL, follow.date = NULL, to.cube.symbol = NULL)
    ][, ln.ints := str_count(x, ",") + 1
    ][is.na(ln.ints), out.degree := 0
    ][, tag := ifelse(date - as.Date(f.follow.date) > 0, 1, 0), keyby = .(cube.symbol, stck)
    ][, first.half := ifelse(tag == 0, 1, 0)
    ][, second.half := ifelse(tag == 1, 1, 0)
    ][, loss := ifelse(gain == 1, 0, 1)
    ][, ishold := ifelse(issale == 1, 0, 1)
    ][, hldt.ls.7 := ifelse(hold.time <7, 1, 0)]

rm(user.wnwk.sp)


# Calculate learning centrality
ld(user.wnwk.sp)
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
p <- pgrk_grph[, setDT(as.data.frame(pgrnk), keep.rownames = T), keyby = .(date)] # This row is very important. When PR was calculated, the data structure is a list with rownames, more than that, rownames is a necessary variable to be transformed. First, the list should be transformed into data.frame with rownames, then, use setDT to convert data.frame into data.dable with a variable consisted with rownames.

rm(grph, pgrk_grph)
setnames(p, 2:3, c("sp.out", "ln.cntr"))

pg_rnk <- p[pg_rk, on = .(sp.out, date), nomatch = NA]
f.nwl.cntr <- pg_rnk[, .(ln.cntr =sum(ln.cntr)), keyby = .(cube.symbol, date)]
rm(p, pg_rk, pg_rnk, tim, user.wnwk.sp)

f.nwl <- f.nwl.cntr[f.nwl.ints, on = .(cube.symbol, date), nomatch = NA
    ][is.na(ln.cntr), ln.cntr := 0]
f.nwl <- f.nwl[, str_c("ln.cntr", 1, sep = ".lag") := shift(ln.cntr, n = 1, type = 'lag'), keyby = .(cube.symbol, stck)
    ][order(cube.symbol, date)]
f.nwl[, .SD[.N], by = .(cube.symbol, date)]
save(f.nwl, file = 'f.nwl.Rdata')

# import momentum variable
ld(f.nwl)
f.mmt <- fread("Momentum.csv", encoding = "UTF-8")
setnames(f.mmt, 1:2, c("date", "mmt"))
f.mmt[, date := str_replace_all(date, "/", "-")
    ][, date := as.Date(date, "%Y-%m-%d")]
f.nwl <- f.mmt[f.nwl, on = "date", nomatch = 0]
f.nwl[is.na(x), ln.ints := 1
    ][, ln.qlty := ln.cntr * ln.ints
    ][order(cube.symbol, date)
    ][, str_c("ln.qlty", 1, sep = ".lag") := shift(ln.qlty, n = 1, type = 'lag'), keyby = .(cube.symbol, stck)]

# import cubelife to calculate trade period for per trader
ld(f.cubelife.mst.1803)
f.nwl <- f.cubelife.mst.1803[f.nwl, on = .(cube.symbol), nomatch = 0]
f.nwl[, trd.prid := as.numeric(difftime(date, start, units = "days"))/7]

# import f.hold.price to calculate trade number per day for per trader
ld(f.hold.price)
f.hold.price <- f.hold.price[order(cube.symbol, created.at)
    ][, tag := 1
    ][, date := as.character(created.at)
    ][, date := as.Date(str_extract(date, '.{1,10}'), "%Y-%m-%d")]

f.trd.d <- f.hold.price[, .(date = seq(min(date), max(date), by = "day")), by = .(cube.symbol)]

f.trd.d <- f.hold.price[, .(cube.symbol, date, tag)
    ][f.trd.d, on = .(cube.symbol, date)]
f.trd.d <- f.trd.d[is.na(tag), tag := 0
    ][, .(trd.num = sum(tag)), by = .(cube.symbol, date)]
f.trd.d[, trd.num := cumsum(trd.num)/100, by = .(cube.symbol)]

f.nwl <- f.trd.d[f.nwl, on = .(cube.symbol, date)]

rst.ln1 <- f.nwl[, felm(issale ~ gain + I(gain*ln.cntr) | cube.symbol + stck + hold.time)]
rst.ln2 <- f.nwl[, felm(issale ~ gain + I(gain * ln.cntr) + mmt| cube.symbol + stck + hold.time)]
rst.ln3 <- f.nwl[, felm(issale ~ gain + I(gain*ln.cntr)  + mmt + I(trd.prid*7/365) | cube.symbol + stck + hold.time)]
rst.ln4 <- f.nwl[, felm(issale ~ gain + I(gain * ln.cntr) + mmt + I(trd.num/10) | cube.symbol + stck + hold.time)]
rst.ln5 <- f.nwl[, felm(issale ~ gain + I(gain * ln.cntr) + mmt + I(trd.prid*7/365) + I(trd.num/10) | cube.symbol + stck + hold.time)]

rst.ln6 <- f.nwl[, felm(issale ~ gain + I(gain * ln.ints/1000) | cube.symbol + stck + hold.time)]
rst.ln7 <- f.nwl[, felm(issale ~ gain + I(gain * ln.ints/1000) + mmt | cube.symbol + stck + hold.time)]
rst.ln8 <- f.nwl[, felm(issale ~ gain + I(gain * ln.ints/1000) + mmt + I(trd.prid*7/365) | cube.symbol + stck + hold.time)]
rst.ln9 <- f.nwl[, felm(issale ~ gain + I(gain * ln.ints/1000) + mmt + I(trd.num/10) | cube.symbol + stck + hold.time)]
rst.ln10 <- f.nwl[, felm(issale ~ gain + I(gain * ln.ints/1000) + mmt + I(trd.prid*7/365) + I(trd.num/10) | cube.symbol + stck + hold.time)]

list(rst.ln1, rst.ln2, rst.ln3, rst.ln4, rst.ln5, rst.ln6, rst.ln7, rst.ln8, rst.ln9, rst.ln10) %>%
    stargazer(out = "rst.ln.doc",
        type = "html",
        t.auto = T,
        title = "Full Sample",
        dep.var.caption = "Dependent Variable: Sale",
        dep.var.labels.include = F,
        #column.separate = c(3, 3, 3),
        #column.labels = "Full Sample", 
        #covariate.labels = c("Gain", "Gain*Quality", "Gain*Quality*Hold-7-day", "Momentum", "Gain*Intensity", "Gain*Intensity*Hold-7-day"),
        omit.stat = c("LL", "ser", "rsq", "res.dev"),
        model.names = F,
        single.row = F,
        add.lines = list(c("Cube.symbol FE", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"), c("Hold period FE", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"), c("Stock FE", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")))