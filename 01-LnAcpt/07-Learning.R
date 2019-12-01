ld(user.wnwk.sp)
ld(f.surv.flw)
library(Matrix)
library(lfe)
library(igraph)
library(stargazer)

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
f.nwl.cntr <- pg_rnk[, .(ln.cntr = mean(ln.cntr)), keyby = .(cube.symbol, date)]
rm(p, pg_rk, pg_rnk, tim, user.wnwk.sp)

f.nwl <- f.nwl.cntr[f.nwl.ints, on = .(cube.symbol, date), nomatch = NA
    ][is.na(ln.cntr), ln.cntr := 0]
f.nwl <- f.nwl[, str_c("ln.cntr", 1, sep = ".lag") := shift(ln.cntr, n = 1, type = 'lag'), keyby = .(cube.symbol, stck)
    ][order(cube.symbol, date)]
f.nwl[, .SD[.N], by = .(cube.symbol, date)]
save(f.nwl, file = 'f.nwl.Rdata')

f.nwl.rbst1 <- f.nwl[f.cube, on = .(cube.symbol), nomatch = 0]
f.nwl.rbst2 <- f.nwl[f.cube.DE, on = .(cube.symbol), nomatch = 0]

#rst.ln1 <- f.nwl[ln.ints != 0, felm(issale ~ gain + hldt.ls.7 + I(gain*hldt.ls.7)+ log(ln.ints) + I(log(ln.ints) * gain*hldt.ls.7) | cube.symbol + stck + hold.time)]
#rst.ln2 <- f.nwl[ln.cntr != 0, felm(issale ~ gain + log(ln.cntr) + I(log(ln.cntr) * gain) | cube.symbol + stck + hold.time)]
#rst.ln3 <- f.nwl[ln.ints != 0, felm(issale ~ gain + log(ln.ints) + I(log(ln.ints) * gain) + log(ln.cntr) + I(log(ln.cntr) * gain) | cube.symbol + stck + hold.time)]
rst.ln4 <- a[ln.ints != 0, felm(disp ~ second.half| cube.symbol + stck + hold.time)]
rst.ln5 <- f.nwl.rbst1[, felm(issale ~ I(gain) + I(ln.cntr * 1000) + I(ln.cntr.lag1 * 1000) + I(ln.cntr * 1000 * gain) + I(ln.cntr.lag1 * 1000 * gain) | cube.symbol + stck + hold.time)]
rst.ln6 <- f.nwl.rbst1[ln.ints != 0, felm(issale ~ I(gain * hldt.ls.7) + log(ln.ints) + I(log(ln.ints) * gain * hldt.ls.7) + log(ln.cntr) + I(log(ln.cntr) * gain * hldt.ls.7) | cube.symbol + stck + hold.time)]

rst.ln7 <- f.nwl.rbst2[ln.ints != 0, felm(issale ~ gain + log(ln.ints) + I(log(ln.ints) * gain) | cube.symbol + stck + hold.time)]
rst.ln8 <- f.nwl.rbst2[, felm(issale ~ I(gain) + I(ln.cntr * 1000) + I(ln.cntr.lag1 * 1000) + I(ln.cntr * 1000 * gain) + I(ln.cntr.lag1 * 1000 * gain) | cube.symbol + stck + hold.time)]
rst.ln9 <- f.nwl.rbst2[ln.ints != 0, felm(issale ~ gain + log(ln.ints) + I(log(ln.ints) * gain) + log(ln.cntr*100) + I(log(ln.cntr*100) * gain) | cube.symbol + stck + hold.time)]

list(rst.ln1, rst.ln2, rst.ln3, rst.ln4, rst.ln5, rst.ln6, rst.ln7, rst.ln8, rst.ln9) %>%
    stargazer(out = "rst.ln.doc",
        type = "html",
        t.auto = T,
        title = "Full Sample",
        dep.var.caption = "Dependent Variable: Sale",
        dep.var.labels.include = F,
        column.separate = c(3, 3, 3),
        column.labels = c("Full Sample", "BiTrade Sample", "BiTradeDE Sample"),
        covariate.labels = c("Gain", "ln.ints", "Gain*ln.ints", "ln.cntr", "Gain*ln.cntr"),
        omit.stat = c("LL", "ser"),
        model.names = F,
        single.row = F,
        add.lines = list(c("cube.symbol", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"), c("hold.time", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"), c("stock", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")))