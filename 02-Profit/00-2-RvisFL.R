library(styleer)
# 1. ����1806���ݽ�����������ļ��� ----
ld(r.user.stock.mst.1806)
ld(r.cube.info.mst.1806)

# ��r.user.stock.mst.1806��ȡ��1806�Ĳ���
f.cubelife.mst.1806 <- r.user.stock.mst.1806[lastcrawl == 1806, .SD]
f.cubelife.mst.1806 <- f.cubelife.mst.1806[, create.at := as.POSIXct(createAt / 1000, origin = "1970-01-01 00:00:00")
    ][, create.date := as.Date(create.at)
    ][order(user.id, create.at), .SD]


cu <- r.cube.info.mst.1806[, close.date := fifelse(is.na(close.date), as.Date("2018-07-03"), close.date)
    ][lastcrawl == 1806, .SD]

CJ <- r.cube.info.mst.1806[str_sub(cube.symbol, 1, 2) == 'SP' & lastcrawl == 1806 & close.date - create.date > 0, .SD, by = .(cube.symbol)
    ][, .(date = seq(create.date, close.date, by = "day")), keyby = .(cube.symbol)
    ][, .(cube.symbol, follow.date = date)
    ] %>% unique() # Ϊ����һ��ʱ���ֵ

# user.wnwk.sp.1806: ֻ���� SP �û�
# ÿ�콨��һ��nwk
sadd <- function(x) {
    if (length(x) == 1) {
        x
    } else {
        as.list(Reduce(union, x, accumulate = T))
    }
}

user.wnwk.sp.1806 <- f.cubelife.mst.1806[exchange %in% c('ZHCN'), .(from.user.id = user.id, to.cube.symbol = code, follow.date = create.date)
    ][cu[cube.type == 'SP', .(from.cube.symbol = cube.symbol, owner.id)], on = .(from.user.id = owner.id), nomatch = 0
    ][, ':='(from.user.id = NULL)
    ][to.cube.symbol != from.cube.symbol
    ][, .(to.cube.symbol = list(to.cube.symbol)), keyby = .(from.cube.symbol, follow.date)
    ][CJ, on = .(from.cube.symbol = cube.symbol, follow.date), nomatch = NA
    ][, .SD[!is.null(unlist(to.cube.symbol))], by = .(from.cube.symbol) # ȥ�����м�¼�ж�û��follow��cube
    ][, ':='(to.cube.symbol = sadd(to.cube.symbol)), keyby = .(from.cube.symbol)
    ][order(from.cube.symbol, follow.date), .SD]
sv(user.wnwk.sp.1806, svname = "user.wnwk.sp.1806")



# 2. ����out-degree��in-degree��learning centrality----
# 2.1 ����out-degree��in-degree
ld(user.wnwk.sp.1806)
ld(f.surv.flw.1806)
f.nwl.ind <- user.wnwk.sp.1806[, .(date = as.Date(follow.date), cube.symbol = from.cube.symbol, to.cube.symbol)
    ][f.surv.flw.1806, on = .(date, cube.symbol), nomatch = NA
    ][order(cube.symbol, date)
    ][, out := ifelse(!is.null(to.cube.symbol), str_c(unlist(to.cube.symbol), collapse = ","), NA), keyby = .(cube.symbol, date)
    ][, .(sp.out = unlist(str_split(out, ','))), keyby = .(cube.symbol, date)
    ][!is.na(sp.out), .SD[order(sp.out, date)]
    ][, .(indegree = list(cube.symbol)), keyby = .(sp.out, date)]

f.nwl.ind <- f.nwl.ind[, setnames(.SD, 1, "cube.symbol")]
setnames(user.wnwk.sp.1806, 1:3, c("cube.symbol", "date", "outdegree"))
f.nwl.ints <- f.nwl.ind[user.wnwk.sp.1806[, date := as.Date(date)], on = .(date, cube.symbol)]

f.nwl.ints[!is.null(indegree), ind := lapply(indegree, length) %>% unlist(), by = .(cube.symbol)
    ][!is.null(outdegree), oud := lapply(outdegree, length) %>% unlist(), by = .(cube.symbol)]
f.nwl.ints.1806 <- f.nwl.ints
sv(f.nwl.ints.1806, svname = "f.nwl.ints.1806")

# 2.2 ����learning centrality
library(igraph)
ld(user.wnwk.sp.1806)
tim <- CJ(cube.symbol = unique(f.nwl.ints$cube.symbol), date = seq(as.Date('2016-06-24'), as.Date('2018-06-28'), by = "day"))

# �޸�һЩ����������to.cube.symbol����б�����unlist
f.nwl.cntr <- user.wnwk.sp.1806[, ":="(date = follow.date, cube.symbol = from.cube.symbol)
    ][, out := ifelse(!is.null(to.cube.symbol), str_c(unlist(to.cube.symbol), collapse = ","), NA), keyby = .(cube.symbol, date)
    ][, ":="(from.cube.symbol = NULL, follow.date = NULL, to.cube.symbol = NULL)
    ][tim, on = .(cube.symbol, date), nomatch = NA]

# �� sp.out���ÿ�������Ĺ۲��������չƽ
pg_rk <- f.nwl.cntr[!is.na(out)
    ][, .(sp.out = unlist(str_split(out, ','))), keyby = .(cube.symbol, date)
    ][, id := seq(1, .N, by = 1)
    ][, net.out := str_c(cube.symbol, sp.out, sep = ','), keyby = .(id)] #���д�����û��̫�����壬���ڴ���Ϊһ���ǳ���Ҫ��primary key���д���

# ���� igraph ������������󲢶�ÿ���ڵ����PageRank
grph <- pg_rk[sp.out != "", .(pgrnk = graph_from_data_frame(.SD, directed = T) %>% list()), .SDcols = c("cube.symbol", "sp.out"), keyby = .(date)]
pgrk_grph <- grph[, .(pgrnk = lapply(pgrnk, page_rank)), keyby = .(date)]
pgrk_grph <- pgrk_grph[, .(pgrnk = lapply(pgrnk, `[[`, "vector")), keyby = .(date)]
p <- pgrk_grph[, setDT(as.data.frame(pgrnk), keep.rownames = T), keyby = .(date)
    ][, setnames(.SD, 2:3, c("sp.out", "ln.cntr"))
    ][, unique(.SD)]

pg_rnk <- p[pg_rk, on = .(sp.out, date), nomatch = NA]
f.nwl.cntr <- pg_rnk[, .(ln.cntr = mean(ln.cntr, na.rm = T)), keyby = .(cube.symbol, date)]
f.nwl.cntr.1806 <- f.nwl.cntr

sv(f.nwl.cntr.1806, svname = "f.nwl.cntr.1806")

f.nwl.1806 <- f.nwl.cntr.1806[f.nwl.ints.1806[, .(cube.symbol, date, ind, oud)], on = .(cube.symbol, date), nomatch = NA
    ][is.na(ln.cntr), ln.cntr := 0
    ][is.na(oud), oud := 0
    ][is.na(ind), ind := 0]

sv(f.nwl.1806, svname = "f.nwl.1806")