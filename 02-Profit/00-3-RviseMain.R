library(styleer)
ld(f.nwl.1806)
ld(f.cube.ret.sp)
ld(f.main.1806)
ld(f.surv.flw.1806)

# ���1806����������֮����лع�����ݼ�f.main1��f.main2 ----

# ������f.main1
# ��ѡfollowǰ���н��׵�cube
f.main1 <- f.main.1806[, unique(.SD), .SDcols = c('cube.symbol', 'follow.date')
    ][f.cube.ret.sp, on = .(cube.symbol), nomatch = 0
    ][, pre.period := follow.date - min(date), by = .(cube.symbol)
    ][, .SD[pre.period < max(date) - follow.date], by = .(cube.symbol)]

sv(f.main1, svname = "f.main1")

# ������f.main2
f.main2 <- f.surv.flw.1806[, .(follow.date = as.Date(follow.date) %>% unique()), by = .(cube.symbol)
    ][f.nwl.1806, on = .(cube.symbol), nomatch = 0
    ]
f.main2 <- f.cube.ret.sp[f.main2, on = .(cube.symbol, date)]
sv(f.main2, svname = "f.main2")