# ����comment���ݼ�
library(styleer)
ld(r.user.cmt.mst.1806)
ld(r.cube.info.mst.1806)

# ���ÿ��ÿ��id������post����
f.user.cmt.num.1806 <- r.user.cmt.mst.1806[, .SD, .SDcols = c(1, 2, 4)
    ][, unique(.SD)
    ][, date := as.Date(created.at)
    ][order(user.id, created.at), .SD
    ][, .(cmt.num = .N), by = .(user.id, date)]

# �ó����е�cube.symbol��user.id
f.cube.info.sp <- r.cube.info.mst.1806[cube.type == "SP", unique(.SD), .SDcols = c("cube.symbol", "owner.id")
    ][, .(cube.symbol, user.id = owner.id)]

# �����ű����кϲ�
f.user.cmt.num <- f.cube.info.sp[f.user.cmt.num.1806, on = .(user.id), nomatch = 0
    ][order(cube.symbol, date), .SD
    ][, user.id := NULL
    ][date >= as.Date("2016-06-01"), .SD]

sv(f.user.cmt.num, svname = "f.user.cmt.num")