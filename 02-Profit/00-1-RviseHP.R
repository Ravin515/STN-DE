# 利用1806的数据，重新计算持仓价格
library(styleer)
ld(f.cube.rb.sp.mst.1806)

f.hold.price.1806 <- f.cube.rb.sp.mst.1806[order(cube.symbol, stock.symbol, created.at, id, - target.weight), unique(.SD), .SDcols = c("cube.symbol", "stock.symbol", "created.at", "price", 'id', "target.weight", "prev.weight.adjusted")
    ][!(prev.weight.adjusted == 0 & target.weight == 0), .SD
    #][, {
        ##对那些重复的记录进行去重
        #a <- vector()
        #a[1] <- 0
        #for (i in 2:.N) {
            #if (abs(target.weight[i] - target.weight[i - 1]) <= 0.05) {
                #a[i] <- i
            #}
            #else {
                #a[i] <- 0
            #}
        #}
        #.SD[-(a[a != 0])]
    #}
    ][, tag := fifelse(mean(is.na(id)) > 0 & .N > 1, 1, 0), by = .(cube.symbol, stock.symbol, created.at) # 对那些created.at有重复且id存在NA的记录进行处理
    ][, tag2 := fifelse(mean(tag == 0) < 1, 1, 0), by = .(cube.symbol, stock.symbol)] # 标记出那些有tag为1的cube.symbol和stock.symbol

# 抽取tag == 1进行清洗
f.locate1 <- f.hold.price.1806[tag == 1, .SD
    ][order(cube.symbol, stock.symbol, created.at, - target.weight), .SD
    ][, value := sum(prev.weight.adjusted - shift(target.weight, type = "lag"), na.rm = T) %>% abs(), by = .(cube.symbol, stock.symbol, created.at)
    ][, tag.locate := 1:.N, by = .(cube.symbol, stock.symbol, created.at)]

f.locate2 <- f.hold.price.1806[tag == 1, .SD
    ][order(cube.symbol, stock.symbol, created.at, target.weight), .SD
    ][, value := sum(prev.weight.adjusted - shift(target.weight, type = "lag"), na.rm = T) %>% abs(), by = .(cube.symbol, stock.symbol, created.at)
    ][, tag.locate := 1:.N, by = .(cube.symbol, stock.symbol, created.at)]

f.locate <- rbindlist(list(f.locate1, f.locate2))
f.locate <- f.locate[order(cube.symbol, stock.symbol, created.at, value, tag.locate), .SD
    ][, .SD[value == min(value)], by = .(cube.symbol, stock.symbol, created.at)
    ][, unique(.SD), by = .(cube.symbol, stock.symbol, created.at)
    ][, id.num := sum(!is.na(id)), by = .(cube.symbol, stock.symbol, created.at)]

# 抽取tag == 1 且 每个时间戳上id数为0、1和2的进行清洗
f.id2 <- f.locate[id.num == 2, .SD
    ][!is.na(id), .SD
    ][, id.num := sum(!is.na(id)), by = .(cube.symbol, stock.symbol, created.at)
    ][, value := sum(prev.weight.adjusted - shift(target.weight, type = "lag"), na.rm = T) %>% abs(), by = .(cube.symbol, stock.symbol, created.at)
    ][, .SD[value < 1 | (value > 1 & tag.locate == min(tag.locate))], by = .(cube.symbol, stock.symbol, created.at)
    ][, tag.locate := 1:.N, by = .(cube.symbol, stock.symbol, created.at)
    ][, {
        weight <- CJ(target.weight, prev.weight.adjusted, price)
        weight[abs(target.weight - prev.weight.adjusted) == max(abs(target.weight - prev.weight.adjusted)), unique(.SD)]
    }, by = .(cube.symbol, stock.symbol, created.at)
    ][, tag.locate := 1:.N, by = .(cube.symbol, stock.symbol, created.at)]

f.id1 <- f.locate[id.num == 1, .SD
    ][, {
        weight <- CJ(target.weight, prev.weight.adjusted, price)
        weight[abs(target.weight - prev.weight.adjusted) == max(abs(target.weight - prev.weight.adjusted)), unique(.SD)]
    }, by = .(cube.symbol, stock.symbol, created.at)
    ][, tag.locate := 1:.N, by = .(cube.symbol, stock.symbol, created.at)]

f.id0 <- f.locate[id.num == 0, .SD
    ][, {
    weight <- CJ(target.weight, prev.weight.adjusted, price)
    weight[abs(target.weight - prev.weight.adjusted) == max(abs(target.weight - prev.weight.adjusted)), unique(.SD)]
    }, by = .(cube.symbol, stock.symbol, created.at)
    ][, tag.locate := 1:.N, by = .(cube.symbol, stock.symbol, created.at)]

f.id <- rbindlist(list(f.id0, f.id1, f.id2), use.names = T)

# 与tag == 0的f.hold.price数据进行合并
f.hold.price.1806 <- f.hold.price.1806[tag == 0, .SD, .SDcols = -c("id", "tag", "tag2")
    ][, tag.locate := 1:.N, by = .(cube.symbol, stock.symbol, created.at)]
f.hold.price.1806 <- rbindlist(list(f.id, f.hold.price.1806), use.names = T)
f.hold.price.1806 <- f.hold.price.1806[order(cube.symbol, stock.symbol, created.at, tag.locate)
    ][, .SD[.N], by = .(cube.symbol, stock.symbol, created.at)
    ][!(is.nan(target.weight) | is.infinite(prev.weight.adjusted)), .SD]

# 删除每个cube每只股票第一笔为卖出的交易
f.hold.price.1806 <- f.hold.price.1806[, issale := fifelse(target.weight < prev.weight.adjusted, 1, 0)
    ][, diff := c(0, diff(issale)), keyby = .(cube.symbol, stock.symbol)
    ][, cumsum := cumsum(abs(diff)), keyby = .(cube.symbol, stock.symbol)
    ][issale == 1 & cumsum == 0, no := 1
    ][is.na(no), .(stock.symbol, price, target.weight, prev.weight.adjusted, cube.symbol, created.at, issale)
    ][!is.na(target.weight)]

# 清除那些prev.weight.adjusted为负的记录
f.hold.price.1806 <- f.hold.price.1806[!(target.weight < 0 | prev.weight.adjusted < 0 | price < 0), .SD]

f.hold.price.1806[, hold.price := {
    # 计算持仓价格
    pwa <- prev.weight.adjusted;
    price <- price;
    tw <- target.weight;

    make_hp <- function(n, price, pwa, tw) {
        if (tw != 0) {
            hp[n] <- (hp[n - 1] * pwa + (tw - pwa) * (price)) / tw
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
sv(f.hold.price.1806, svname = "f.hold.price.1806")

rm(list = ls())


## 加入1806的数据
#ld(r.cube.rb.mst.1806)
#f.cube.rb.sp.1806 <- r.cube.rb.mst.1806[cube.type == "SP" & lastcrawl == 1806 & created.at > as.POSIXct("2018-03-27 15:00:00")]
#f.cube.rb.sp.mst.1806 <- rbindlist(list(f.cube.rb.sp.1806, f.cube.rb.sp), use.names = T)
#sv(f.cube.rb.sp.mst.1806, svname = "f.cube.rb.sp.mst.1806")

#f.hold.price.1806 <- f.cube.rb.sp.1806[order(cube.symbol, stock.symbol, created.at, id, - target.weight), unique(.SD), .SDcols = c("cube.symbol", "stock.symbol", "created.at", "price", 'id', "target.weight", "prev.weight.adjusted")
    #][!(prev.weight.adjusted == 0 & target.weight == 0), .SD
    ##][, {
    ###对那些重复的记录进行去重
    ##a <- vector()
    ##a[1] <- 0
    ##for (i in 2:.N) {
    ##if (abs(target.weight[i] - target.weight[i - 1]) <= 0.05) {
    ##a[i] <- i
    ##}
    ##else {
    ##a[i] <- 0
    ##}
    ##}
    ##.SD[-(a[a != 0])]
    ##}
    #][, tag := fifelse(.N > 1, 1, 0), by = .(cube.symbol, stock.symbol, created.at) # 对那些created.at有重复的记录进行处理
    #]
#f.id <- f.hold.price.1806[tag == 1, .SD
    #][, .(target.weight = target.weight[.N], prev.weight.adjusted = prev.weight.adjusted[1], price = price[.N]), by = .(cube.symbol, stock.symbol, created.at)]
#f.hold.price.1806 <- rbindlist(list(f.hold.price.1806[tag == 0, .SD, .SDcols = -c('id', 'tag')], f.id), use.names = T)

#ld(f.hold.price)
#f.hold.price[, ':='(hold.price = NULL, tag.locate = NULL)]
#f.hold.price <- rbindlist(list(f.hold.price, f.hold.price.1806), use.names = T)
## 去除第一笔交易为卖出的记录
#f.hold.price <- f.hold.price[order(cube.symbol, stock.symbol, created.at), .SD
    #][, issale := fifelse(target.weight < prev.weight.adjusted, 1, 0)
    #][, diff := c(0, diff(issale)), keyby = .(cube.symbol, stock.symbol)
    #][, cumsum := cumsum(abs(diff)), keyby = .(cube.symbol, stock.symbol)
    #][issale == 1 & cumsum == 0, no := 1
    #][is.na(no), .SD
    #][!is.na(target.weight), .SD]

#f.hold.price <- f.hold.price[, .SD[, - c(8:11)]]

#f.hold.price[, hold.price := {
    ## 计算持仓价格
    #pwa <- prev.weight.adjusted;
    #price <- price;
    #tw <- target.weight;

    #make_hp <- function(n, price, pwa, tw) {
        #if (tw != 0) {
            #hp[n] <- (hp[n - 1] * pwa + (tw - pwa) * (price)) / tw
        #}
        #else {
            #hp[n] <- 0
        #}
        #hp
    #};

    #hp <- price[1];
    #if (length(price) == 1) {
        #hp <- price
    #}
    #else {
        #for (i in 2:length(price)) {
            #hp <- make_hp(i, price[i], pwa[i], tw[i])
        #};
    #}
    #list(hp)
#}
    #, keyby = .(cube.symbol, stock.symbol)
    #]
#sv(f.hold.price, svname = "f.hold.price")