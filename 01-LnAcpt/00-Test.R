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
