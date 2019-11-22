# clean SP data and order the stock&time
ld(r.cube.rb.1803, T)
sp.rb <- r.cube.rb.1803[cube.type == "SP"
    ][, issale := ifelse((target.weight < prev.weight.adjusted), 1, 0)]
rm(r.cube.rb.1803)

sp.rb <- sp.rb[, .(id, stock.name, stock.symbol, price, target.weight, proactive, prev.weight.adjusted, cube.symbol, cube.type, created.at, issale)
    ][order(stock.symbol, created.at)
    ][, unique(created.at), by = .(id, stock.name, stock.symbol, price, target.weight, proactive, prev.weight.adjusted, cube.symbol, cube.type, issale)]
setnames(sp.rb, "V1", "created.at")

#delete the first issale ==1 by cube.symbol&stock.symbol
sp.rb.ef<- sp.rb[, diff := c(0, diff(issale)), keyby = .(cube.symbol, stock.symbol)
    ][, cumsum := cumsum(abs(diff)), keyby = .(cube.symbol, stock.symbol)
    ][issale == 1 & cumsum == 0, no :=1
    ][is.na(no), .(id, stock.name, stock.symbol, price, target.weight, proactive, prev.weight.adjusted, cube.symbol, cube.type, created.at, issale)
    ][!is.na(target.weight)]

# clean the data with weird prev.weight.adjusted
prblm <- unique(sp.rb.ef[prev.weight.adjusted < 0, .(cube.symbol, stock.symbol)], by = c("cube.symbol", "stock.symbol"))
lct <- prblm[sp.rb.ef, on = .(cube.symbol, stock.symbol), nomatch = 0]
a <- lct[, .(id = id, tag = 1)
    ][sp.rb.ef, on = 'id']
sp.rb.ef <- a[is.na(tag)
    ][, tag := NULL
    ][, issale := ifelse((target.weight < prev.weight.adjusted), 1, 0)]
rm(a, lct)

# delete duplicated id
sp.rb.ef <- unique(sp.rb.ef, by = "id")

#The function of calculating the hold price
f.hold.price <- sp.rb.ef[, hold.price := {
    pwa <- prev.weight.adjusted;
    price <- price;
    tw <- target.weight;

    make_hp <- function(n, price, pwa, tw) {
        if (tw != 0) {
            hp[n] <- (hp[n - 1] * pwa + (tw - pwa) * abs(price - hp[n - 1])) / tw
        }
        else {
            hp[n] <- 0
        }
    hp
    };

    hp <- price[1];
    if (length(price) == 1) {
        hp <- price
    }
    else { 
        for (i in 2:length(price)) {
            hp <- make_hp(i, price[i], pwa[i], tw[i])
        };
    }
    list(hp)
}
    , keyby = .(cube.symbol, stock.symbol)
]

save(f.hold.price ,file = "f.hold.price.Rdata")