ld(sample2)
ld(f.cube.ret.sp)
ld(f.nwl)
ld(f.cubelife.mst.1803)

# 1. Data pre-processing
ret.filtered2 <- f.nwl[sample2[, .(cube.symbol)
    ][, unique(.SD)
    ][f.cube.ret.sp, on = .(cube.symbol), nomatch = 0]
    , on = .(cube.symbol, date), roll = T
    ][, unique(.SD)
    ][, c('ln.cntr', 'ind', 'oud') := lapply(.SD, fillna, fill = 0), .SDcol = c('ln.cntr', 'ind', 'oud'), by = .(cube.symbol)]

# 1.1 add UMD factor
mmt <- fread("C:/Users/MrStylee/source/repos/STN-DE/01-LnAcpt/Momentum.csv")
mmt <- mmt[, setnames(.SD, 1:2, c("date", "mmt"))
    ][, date := str_replace_all(date, "/", "-") %>% as.Date("%Y-%m-%d")]

f.main2 <- mmt[ret.filtered2, on = .(date)]

# 1.2  Trade number is merged to main
ld(f.cube.rb.mst.1803)
cube.rb.sp <- f.cube.rb.mst.1803[cube.type == "SP", .(cube.symbol, created.at, date = as.Date(created.at), tag = 1)
    ][, trd.num := sum(tag), by = .(cube.symbol, date)
    ][order(cube.symbol, date), .SD
    ][, trd.num := cumsum(trd.num), by = .(cube.symbol)
    ][, .SD[.N], by = .(cube.symbol, date)
    ][, ':='(created.at = NULL, tag = NULL)]
rm(f.cube.rb.mst.1803)

f.main2 <- cube.rb.sp[f.main2, on = .(cube.symbol, date), roll = T]

# 1.3 Active day is generated
f.main2[, active.day := 1:.N, by = .(cube.symbol)]

# 1.4 Adding close date per cube symbol
f.main2 <- f.cubelife.mst.1803[, .(cube.symbol, end)
    ][f.main2, on = .(cube.symbol)
    ][date < end | date == end, .SD]

# 1.5 stock list generating
ld(f.cube.rb.mst.1803)

# 1.5.1 processing every stock symbol first trading per cube
cube.rb.first <- f.cube.rb.mst.1803[cube.type == "SP", .(cube.symbol, target.weight, prev.weight.adjusted, stock.symbol, created.at)
    ][order(cube.symbol, stock.symbol, created.at), .SD
    ][, .SD[1], by = .(cube.symbol, stock.symbol)]
cube.rb.first[, buy := ifelse(prev.weight.adjusted == 0 & target.weight != 0, 1, 0)
    ][, min.trddy := min(created.at), by = .(cube.symbol)]

cube.rb.first.first <- cube.rb.first[buy == 1
    ][, date := as.Date(created.at)
    ][, ':='(min.trddy = NULL, buy = NULL)]

cube.rb.first.second <- cube.rb.first[buy == 0
    ][, date := as.Date(created.at)
    ][, ':='(adjusted.date = as.Date(min.trddy) - 1, adjusted.stock.symbol = stock.symbol, adjusted.buy = 1)]

# 1.5.2 generate a initial stock list
cube.first.stock.list <- cube.rb.first.second[, .(first.stock.list = list(adjusted.stock.symbol)), by = .(cube.symbol, adjusted.date)]

# 1.5.3 processing every stock symbol non-first trading per cube
cube.rb.next <- f.cube.rb.mst.1803[cube.type == "SP", .(cube.symbol, target.weight, prev.weight.adjusted, stock.symbol, created.at)
    ][order(cube.symbol, stock.symbol, created.at), .SD
    ][, .SD[-1], by = .(cube.symbol, stock.symbol)]
# select those trading can't effect stock list
cube.rb.next <- cube.rb.next[target.weight == 0 | prev.weight.adjusted == 0, date := as.Date(created.at)]

# 1.5.4 binding cube.first.first and cube.rb.next
cube.rb <- rbindlist(list(cube.rb.next, cube.rb.first.first))
cube.rb[, ':='(buy = ifelse(target.weight != 0, 1, 0), sell = ifelse(target.weight == 0, 1, 0))]
sadd <- function(x) {
    if (length(x) == 1) {
        x
    } else {
        as.list(Reduce(c, x, accumulate = T))
    }
}
cube.rb.buy <- cube.rb[buy == 1, .(stock.list = list(stock.symbol)), keyby = .(cube.symbol, date)
    ][, .(stock.list.buy = sadd(stock.list), date), by = .(cube.symbol)]
cube.rb.sell <- cube.rb[sell == 1, .(stock.list = list(stock.symbol)), keyby = .(cube.symbol, date)
    ][, .(stock.list.sell = sadd(stock.list), date), by = .(cube.symbol)]

# 1.5.5 combining three sets to calculate holding stock list
stock.info2 <- cube.first.stock.list[cube.rb.sell[cube.rb.buy[ret.filtered2, on = .(cube.symbol, date), roll = T], on = .(cube.symbol, date), roll = T], on = .(cube.symbol)]
stock.info2[, stock.list := {
    l <- list()
    for (i in 1:.N) {
        l[[i]] <- setdiff(union(first.stock.list[[i]], stock.list.buy[[i]]), stock.list.sell[[i]]) #%>% list()
    }
    l
}]

stock.info2 <- stock.info2[, .(cube.symbol, date, adjusted.date, first.stock.list, stock.list.sell, stock.list.buy, stock.list)]
sv(stock.info2, svname = "stock.info2")
rm(list = ls())

# Merge stock list 
ld(stock.info2)
f.main2 <- stock.info2[, .(cube.symbol, date, stock.list)
    ][f.main2, on = .(cube.symbol, date)]

f.main2[, stock.num := lapply(stock.list, length) %>% unlist()]

# Merge follow date
ld(user.wnwk.sp)
follow.date <- user.wnwk.sp[!sapply(to.cube.symbol, is.null), .(follow.date = min(follow.date)), keyby = .(from.cube.symbol)
    ][, setnames(.SD, 1, "cube.symbol")]
f.main2 <- follow.date[f.main2, on = .(cube.symbol)]

sv(f.main2, svname = "f.main2")
rm(list = ls())