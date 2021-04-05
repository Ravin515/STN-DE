library(styleer)

# 计算每一个SP的cube的交易可能复制的交易 ----
ld(user.wnwk.sp.1806)
ld(r.cube.rb.mst.1806)
# 找出follow的cube当中属于ZH的symbol
zh.cube.symbol <- user.wnwk.sp.1806[!sapply(to.cube.symbol, is.null), unlist(to.cube.symbol), by = .(from.cube.symbol, follow.date)
    ][, setnames(.SD, "V1", "to.cube.symbol")
    ][str_detect(to.cube.symbol, "ZH"), .(to.cube.symbol = unique(to.cube.symbol))]

# 存储这些ZH的调仓
f.cube.rb.zh <- r.cube.rb.mst.1806[cube.symbol %in% zh.cube.symbol[["to.cube.symbol"]], .SD]
sv(f.cube.rb.zh, svname = "f.cube.rb.zh")


ld(f.main1)
ld(f.cube.rb.zh)
#ld(f.cube.rb.sp.mst.1806)
ld(f.hold.price)
ld(user.wnwk.sp.1806)

# 对SP和ZH的调仓设定两个变量
# following和follower
# 以此判定这个cube是否在网络中是否具有这两种身份
f.cube.rb.zh[, ':='(following = 1, follower = 0)
    ][, prev.weight.adjusted := fifelse(is.na(prev.weight.adjusted), 0, prev.weight.adjusted)]
sp.cube.symbol.follower <- f.main1[, .(cube.symbol = unique(cube.symbol), follower = 1)]

sp.cube.symbol.following <- user.wnwk.sp.1806[!sapply(to.cube.symbol, is.null), unlist(to.cube.symbol), by = .(from.cube.symbol, follow.date)
    ][, setnames(.SD, "V1", "to.cube.symbol")
    ][str_detect(to.cube.symbol, "SP"), .(cube.symbol = unique(to.cube.symbol), following = 1)]

f.cube.rb.sp.follow <- sp.cube.symbol.following[sp.cube.symbol.follower[f.hold.price, on = .(cube.symbol)], on = .(cube.symbol)
    ][, following := fifelse(is.na(following), 0, following)
    ][, follower := fifelse(is.na(follower), 0, follower)
    ][following == 1 | follower == 1, .SD]

f.cube.rb.follower.following <- rbindlist(list(f.cube.rb.sp.follow, f.cube.rb.zh), fill = T)
# 将follow list和f.cube.rb.follower.following进行merge
f.cube.rb.follower.following <- user.wnwk.sp.1806[, .(cube.symbol = from.cube.symbol, date = as.Date(follow.date), to.cube.symbol)
    ][f.cube.rb.follower.following[, date := as.Date(created.at)], on = .(cube.symbol, date)]

f.cube.rb.follower.following <- f.cube.rb.follower.following[!(sapply(to.cube.symbol, is.null) & (follower == 1 & following == 0)), .SD
    ][order(cube.symbol, created.at), .SD]
# 计算关于某笔交易是否是跟随following的某笔交易
cl <- makeCluster(8)
registerDoParallel(cl)
f.rb.followers.followings <- f.cube.rb.follower.following[, {
    followers <- .SD[follower == 1& !(sapply(to.cube.symbol, is.null))]
    followings <- .SD[following == 1]
    followers[, {
        foreach(i = 1:.N, .final = rbindlist, .packages = c("data.table"), .export = c("followings")) %dopar% {
            cube <- cube.symbol[i]
            trd.obj <- stock.symbol[i]
            cube.symbol.list <- to.cube.symbol[[i]]
            trd.mmt <- created.at[i]
            followings[cube.symbol %in% cube.symbol.list & created.at < trd.mmt & stock.symbol %in% trd.obj, .(from.cube.symbol = cube, from.created.at = trd.mmt, to.cube.symbol = cube.symbol, to.stock.symbol = stock.symbol, to.created.at = created.at, to.target.weight = target.weight, to.prev.weight.adjusted = prev.weight.adjusted, to.price = price)]
        }
    }
    #, by = .(from.cube.symbol = cube.symbol, from.created.at = created.at)
    ]
}]

sv(f.rb.followers.followings, svname = "f.rb.followers.followings")
