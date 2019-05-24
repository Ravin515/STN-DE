# f.user.nwk ----
# 筛选出所有follow的组合与实盘，然后按照天进行累加。即每一行观测都表明当天所新增的组合以及累计follow的组合
# cube.type == ZHCN既包含组合，又包含实盘
ld(f.user.stock.mst.1803)
ld(f.cubelife.mst.1803)
ld(cu)

CJ <- f.cubelife.mst.1803[str_sub(cube.symbol, 1, 2) == 'SP', .(date = seq(start, end, by = "day")), keyby = .(cube.symbol)
    ][, .(cube.symbol, follow.date = date)
    ] %>% unique() # 为了下一步时间插值

# user.wnwk.sp: 只包含 SP 用户
# 每周建立一个nwk
sadd <- function(x) {
    if (length(x) == 1) {
        x
    } else {
        as.list(Reduce(union, x, accumulate = T))
    }
}
user.wnwk.sp <- f.user.stock.mst.1803[cube.type %in% c('ZHCN'), .(from.user.id = user.id, to.cube.symbol = stock.symbol, follow.date = create.date)
    ][cu[cube.type == 'SP', .(from.cube.symbol = cube.symbol, owner.id)], on = .(from.user.id = owner.id), nomatch = 0
    ][, ':='(from.user.id = NULL)
    ][to.cube.symbol != from.cube.symbol
    ][, .(to.cube.symbol = list(to.cube.symbol)), keyby = .(from.cube.symbol, follow.date)
    ][CJ, on = .(from.cube.symbol = cube.symbol, follow.date), nomatch = NA
    ][, ':='(to.cube.symbol = sadd(to.cube.symbol)), keyby = .(from.cube.symbol)
    ][order(from.cube.symbol, follow.date)]
sv(user.wnwk.sp)

ld(user.wnwk.sp, T)
ld(f.surv)
#将wnwk当中为null的行全部删除，然后与f.surv进行合并
f.fst.flw <- user.wnwk.sp[!sapply(to.cube.symbol, is.null), .(follow.date = min(follow.date)), keyby = .(from.cube.symbol)]

f.surv.flw <- f.surv[f.fst.flw, on = .(cube.symbol = from.cube.symbol), nomatch = NA
    ][!is.na(Clsprc)]

rm(f.fst.flw, f.surv, user.wnwk.sp)
sv(f.surv.flw)