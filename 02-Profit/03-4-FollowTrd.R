# 1. ����closed price����ÿ��cube��ÿ��stock��λ�ӽ���ֱ������ÿ��Ĳ�λ��� ----
library(styleer)
# 1.1 f.main1�е�cube���
# ����f.main1�õ�ÿһ��cube��closed.date
ld(f.main1)
f.main1[, ret := value / shift(value, type = "lag") - 1, by = .(cube.symbol)]
f.main1.date <- f.main1[, .(closed.date = max(date), follow.date, date), by = .(cube.symbol)
    ][, unique(.SD)]

# �����Ʊ���̼�
clprc <- fbread(path = "./data/Clprc", pattern = "*.txt")
clprc <- clprc[, .(stock.symbol = str_pad(Stkcd, 6, side = "left", pad = "0"), date = as.Date(Trddt), Clsprc)]

# ��f.lifo���д���
ld(f.fifo, force = T)
f.fifo <- f.fifo[, .SD[-1]]
f.fifo <- f.fifo[, date := as.Date(created.at)
    ][, stock.symbol := str_sub(stock.symbol.tag, start = 3L, end = 8L)
    ][order(cube.symbol, stock.symbol.tag, created.at), .SD
    ][, tag.hold := fifelse(target.weight[.N] != 0, 1, 0), by = .(cube.symbol, stock.symbol.tag) # ��ǳ����һ�ʲ�����ֵĲ�λ
    ]
# f.fifo��f.main1.date�����closed.date�ϲ�
f.fifo.main1 <- f.main1.date[, unique(.SD), .SDcols = c("cube.symbol", "closed.date", "follow.date")
    ][f.fifo, on = .(cube.symbol)
    ][cube.symbol %in% unique(f.main1$cube.symbol)]

# ��������date��cube�Ľ���
## ���ڲ�λ�����һ�ʲ���ֵ�ѡ�����cube���׵����ʱ��
period1.1 <- f.fifo.main1[tag.hold == 1, .SD
    ][, .SD[min(date) >= unique(closed.date)], by = .(cube.symbol, stock.symbol.tag)
    ][, closed.date := fifelse(max(date) == closed.date, closed.date, max(date) + 7), by = .(cube.symbol, stock.symbol.tag)
    ][, .(date = seq(min(date), unique(closed.date), by = "day"), follow.date = unique(follow.date)), by = .(cube.symbol, stock.symbol.tag)]

period1.2 <- f.fifo.main1[tag.hold == 1, .SD
    ][, .SD[min(date) < unique(closed.date)], by = .(cube.symbol, stock.symbol.tag)
    ][, .(date = seq(min(date), unique(closed.date), by = "day"), follow.date = unique(follow.date)), by = .(cube.symbol, stock.symbol.tag)]

period1 <- rbindlist(list(period1.1, period1.2), fill = T, use.names = T)
rm(period1.1, period1.2)

## ���ڲ�λ�����һ����ֵ�ѡ��ǰ��ֵ�ʱ��
period2 <- f.fifo.main1[tag.hold == 0, .SD
    ][, .(date = seq(min(date), max(date), by = "day"), follow.date = unique(follow.date)), by = .(cube.symbol, stock.symbol.tag)]

period.main1 <- rbindlist(list(period1, period2), fill = T, use.names = T)

# ��clprc���кϲ�
position.main1 <- period.main1[str_detect(stock.symbol.tag, "SH|SZ"), .SD
    ][, stock.symbol := str_sub(stock.symbol.tag, start = 3L, end = 8L)
    ][clprc, on = .(stock.symbol, date), nomatch = NA
    ][, stock.symbol := str_sub(stock.symbol.tag, start = 1L, end = 8L)
    ][order(cube.symbol, stock.symbol.tag, date), .SD]

# ��f.fifo.main1���кϲ��� ��roll����
f.fifo.main1 <- f.fifo.main1[, .SD, .SDcols = -c("stock.symbol", "tag.hold", "follow.date")
    ][position.main1, on = .(cube.symbol, stock.symbol.tag, date), roll = T]
# ����follow.date����
#f.fifo.main1 <- f.main1.date[, unique(.SD), .SDcols = c("cube.symbol", "follow.date")
    #][f.fifo.main1, on = .(cube.symbol)]
#f.fifo.main1[, post.follow := fifelse(date[1] < follow.date, 0, 1), by = .(cube.symbol, stock.symbol.tag)]

# 1.2 ������ָ���ı����д��� ----
# f.rb.followers.followings����f.main1��cube���м���
ld(f.rb.followers.followings)
#f.rb.followers.followings[, uniqueN(.SD), .SDcols = c("from.cube.symbol", "from.created.at", "to.cube.symbol")]
f.rb.ff <- f.rb.followers.followings[, time.interval := difftime(from.created.at, to.created.at, units = "days")
    ][time.interval < 28, unique(.SD)
    ][, .SD[time.interval == min(time.interval)], by = .(from.cube.symbol, from.created.at, to.stock.symbol)
    ][from.cube.symbol %in% unique(f.main1$cube.symbol), .SD]

# 1.3 ��f.fifo.main1�ĳֲ���ÿ����кϲ���չ�� ----
# ��ÿ���ÿһ��cube��ÿһ��stock�Ľ�����ȡ���һ��
# ������ȡ���������֣�
# �ֲֵĲ��ֺ������ֵĲ���

# ���Ƚ�ÿ��ÿ��cube��ÿһ��stock�Ľ��׽��кϲ����������һ������ֵĲ�λ��ǳ���
f.fifo.position <- f.fifo.main1[order(cube.symbol, stock.symbol.tag, created.at), .SD
    ][, .SD[.N], by = .(cube.symbol, stock.symbol.tag, date)
    ][, num.is.null := fifelse(sapply(open.position, length) == 0, 1, 0)
    ][, clean.price := fifelse(sum(num.is.null) > 0, price[.N], 0), by = .(cube.symbol, stock.symbol.tag)]

# �ֲֵĲ�λ
# ��ÿһ��Ĳ�λ����չ��
f.fifo.position.hold <- f.fifo.position[, rbindlist(open.position), by = .(cube.symbol, stock.symbol.tag, date)]
f.fifo.position.hold <- f.fifo.position.hold[order(cube.symbol, stock.symbol.tag, hold.id, date), .SD
    ][, stock.symbol := str_sub(stock.symbol.tag, start = 1L, end = 8L)]

f.fifo.position.hold <- f.fifo.position[, unique(.SD), .SDcols = c("stock.symbol", "date", "Clsprc")
    ][f.fifo.position.hold, on = .(stock.symbol, date)]

# ��ֵĲ�λ
f.fifo.position.clean <- f.fifo.position[clean.price > 0, .SD[(.N - 1):.N], by = .(cube.symbol, stock.symbol.tag)]
f.fifo.position.clean <- f.fifo.position.clean[, .(Clsprc = unique(clean.price), date = date[.N]), by = .(cube.symbol, stock.symbol.tag)
    ][f.fifo.position.clean[, ':='(date = max(date), Clsprc = clean.price), by = .(cube.symbol, stock.symbol.tag)
        ][, rbindlist(open.position[1]), by = .(cube.symbol, stock.symbol.tag, date)]
        , on = .(cube.symbol, stock.symbol.tag, date)
    ][, stock.symbol := str_sub(stock.symbol.tag, start = 1L, end = 8L)]

# �����ֲ�λ�ϲ�
f.fifo.position.all <- rbindlist(list(f.fifo.position.clean, f.fifo.position.hold), fill = T, use.names = T)
f.fifo.position.all <- f.fifo.position.all[order(cube.symbol, stock.symbol.tag, hold.id, date), .SD
    ][hold.weight != 0, .SD]

# 1.4 f.fifo.position.all��f.main1.date���кϲ�----
f.fifo.position.all <- f.main1[, unique(.SD), .SDcols = c("cube.symbol","pre.period", "follow.date")
    ][f.fifo.position.all, on = .(cube.symbol)
    ][, post.follow.full := fifelse(date[1] >= follow.date, 1, 0), by = .(cube.symbol, stock.symbol.tag) # ��ǳ���Щ��λ��post.follow֮ǰ����֮��
    ]

f.fifo.position.all[, ret.daily := Clsprc/shift(Clsprc, type = "lag") - 1]

# 1.5 ����ͳ�Ʒ��� ----
library(lfe)
# ��Ҫ������Աȵĳֲֹ���portfolio
# �������еĸ���Ľ��׶�Ӧ���еĽ���id
f.fifo.position.all <- f.fifo.main1[, unique(.SD), .SDcols = c("cube.symbol", "stock.symbol", "id", "created.at")
    ][f.rb.ff, on = .(cube.symbol = from.cube.symbol, stock.symbol = to.stock.symbol, created.at = from.created.at)
    ][!is.na(id), .(id, follow.trd = 1)
    ][f.fifo.position.all, on = .(id = hold.id)]
f.fifo.position.all[, follow.trd := fifelse(is.na(follow.trd), 0, 1)]

# ����������Ӳ��ϲ�
url <- str_c(getwd(), "/data/Factors/")
file.names <- list.files(path = url, pattern = "*.txt|.csv")
for (i in file.names) {
    assign(str_sub(i, start = 1L, end = -5L), fread(str_c(url, toupper(i))))
}

f.fifo.position.all <- fivefactor_daily[, setnames(.SD, 1, "date")
    ][f.fifo.position.all, on = .(date)]

# ����Ͷ���ߵ�copy���״�ͷ��β���һ��portfolio
## ��ȡ��follow-trading�Ľ��ף�����portfolio
position.post.follow.trd <- f.fifo.position.all[follow.trd == 1, .SD
    ][order(date), .SD]
ret.post.follow.trd <- position.post.follow.trd[ret.daily > -0.1 & ret.daily < 0.1, .SD
    ][, .(ret.daily.aver = mean(ret.daily), rf, mkt_rf, smb, hml, umd, rmw, cma), by = .(date)
    ][, unique(.SD)]
ret.post.follow.trd[, felm(ret.daily.aver - rf ~ mkt_rf + smb + hml + umd + rmw + cma)] %>% summary()

# 1.5.1 ���ȹ���ƽ��ʱ���portfolio����ɣ�
## ���ȹ�������Ͷ������ͬһʱ������δfollow�Ľ������
## ���߹�����ͬͶ������ͬһʱ�������еĳֲֵĽ������
## ��󹹽���ͬͶ��������ͬʱ������������ֲ������ͬһʱ��ֲֵĽ������

## ��ȡ��non-follow-trading������portfolio������ȫʱ������
#position.post.non.follow.trd <- f.fifo.position.all[(follow.trd == 0 & post.follow.full == 1), .SD
    #][cube.symbol %in% unique(position.post.follow.trd$cube.symbol), .SD
    #][date %in% unique(position.post.follow.trd$date), .SD
    #][order(date), .SD]

#ret.post.non.follow.trd <- position.post.non.follow.trd[ret.daily > -0.1 & ret.daily < 0.1, .SD
    #][, .(ret.daily.aver = mean(ret.daily), rf, mkt_rf, smb, hml, umd, rmw, cma), by = .(date)
    #][, unique(.SD)]
#ret.post.non.follow.trd[, felm(ret.daily.aver - rf ~ mkt_rf + smb + hml + umd + rmw + cma)] %>% summary()

## ��ȡ��non-follow-trading������portfolio������ÿһ��cube��ʱ�䴰��
position.post.non.follow.trd.cube <- f.fifo.position.all[follow.trd == 1 | post.follow.full == 1, .SD
    ][cube.symbol %in% unique(position.post.follow.trd$cube.symbol), .SD
    ][follow.trd == 1, ':='(follow.trd.min.date = min(date), follow.trd.max.date = max(date)), by = .(cube.symbol)
    ][, ':='(follow.trd.min.date = fifelse(is.na(follow.trd.min.date), unique(follow.trd.min.date[!is.na(follow.trd.min.date)]), follow.trd.min.date), follow.trd.max.date = fifelse(is.na(follow.trd.max.date), unique(follow.trd.max.date[!is.na(follow.trd.max.date)]), follow.trd.max.date)), by = .(cube.symbol)
    ][, .SD[date %between% c(unique(follow.trd.min.date), unique(follow.trd.max.date))], by = .(cube.symbol) # ɸѡ��Щ��������follow.trdʱ�䴰��֮��Ĳ�λ
    ][follow.trd == 0 & post.follow.full == 1, .SD
    ][order(date), .SD]

ret.post.non.follow.trd.cube <- position.post.non.follow.trd.cube[ret.daily > -0.1 & ret.daily < 0.1, .SD
    ][, ret.daily.cube := sum(hold.weight * ret.daily) / sum(hold.weight), by = .(cube.symbol, date) # ����ͬһ��cube�Ĺ�Ʊ��Ҫ���������еı������м�Ȩ����
    ][, unique(.SD), .SDcols = c("cube.symbol", "date", "ret.daily.cube", "rf", "mkt_rf", "smb", "hml", "umd", "rmw", "cma") 
    ][, .(ret.daily.aver = mean(ret.daily.cube), rf, mkt_rf, smb, hml, umd, rmw, cma), by = .(date)
    ][, unique(.SD)]
ret.post.non.follow.trd.cube[, felm(ret.daily.aver - rf ~ mkt_rf + smb + hml + umd + rmw + cma)] %>% summary()

# ͬһͶ������ͬһʱ�������������Ʊ������portfolio
# ������Ҫ��������position���ҳ�ͬһʱ�������������Ʊ֮�����ɳ���ʱ����̵���ֻ��Ʊ
position.post.follow.trd.cube.date <- f.fifo.position.all[follow.trd == 1 | post.follow.full == 1, .SD
    ][cube.symbol %in% unique(position.post.follow.trd$cube.symbol), .SD
    ][order(cube.symbol, stock.symbol.tag, date), .SD
    ][, first.date := date[1], by = .(cube.symbol, stock.symbol.tag) # ���ÿ����λ���ֵ�ʱ��
    ][, .SD[first.date %in% .SD[follow.trd == 1, first.date]], by = .(cube.symbol) # ��ÿ��cube�н���ʱ����follow���׽���ʱ����ͬ�Ľ�����ȡ����
    ][order(cube.symbol, stock.symbol.tag, date), .SD
    ][, period := .N, by = .(cube.symbol, stock.symbol.tag) # ����ÿ����λ�ĳ���ʱ��
    ][, .SD[uniqueN(stock.symbol.tag) > 1 & length(follow.trd[follow.trd == 0]) > 0], by = .(cube.symbol, first.date) # ��ÿ��cube�н���ʱ����ͬ�Ĳ�λ��ȡ����
    ][, .SD[(.SD[follow.trd == 1, uniqueN(stock.symbol.tag)] - .SD[follow.trd == 0, uniqueN(stock.symbol.tag)]) %between% c(0, 5)], by = .(cube.symbol, first.date) # ��ѡ��Щͬһ��Ľ���ʱ��follow.trd�Ĳ�λ�����non-follow�Ĳ�λ������ͬ����Щ����
    ][, min.period := min(period), by = .(cube.symbol, first.date) # ������ͬ����ʱ��Ĳ�λ��С�ĳ���ʱ��
    ][, .SD[1:unique(min.period)], by = .(cube.symbol, stock.symbol.tag)
    ]

ret.post.follow.trd.cube.date <- position.post.follow.trd.cube.date[follow.trd == 1, .SD
    ][ret.daily > -0.1 & ret.daily < 0.1, .SD
    ][, ret.daily.cube := sum(hold.weight * ret.daily) / sum(hold.weight), by = .(cube.symbol, date) # ����ͬһ��cube�Ĺ�Ʊ��Ҫ���������еı������м�Ȩ����
    ][, unique(.SD), .SDcols = c("cube.symbol", "date", "ret.daily.cube", "rf", "mkt_rf", "smb", "hml", "umd", "rmw", "cma")
    ][, .(ret.daily.aver = mean(ret.daily.cube), rf, mkt_rf, smb, hml, umd, rmw, cma), by = .(date)
    ][, unique(.SD)]

ret.post.follow.trd.cube.date[, felm(ret.daily.aver - rf ~ mkt_rf + smb + hml + umd + rmw + cma) %>% summary()]

ret.post.non.follow.trd.cube.date <- position.post.follow.trd.cube.date[follow.trd == 0, .SD
    ][ret.daily > -0.1 & ret.daily < 0.1, .SD
    ][, ret.daily.cube := sum(hold.weight * ret.daily) / sum(hold.weight), by = .(cube.symbol, date) # ����ͬһ��cube�Ĺ�Ʊ��Ҫ���������еı������м�Ȩ����
    ][, unique(.SD), .SDcols = c("cube.symbol", "date", "ret.daily.cube", "rf", "mkt_rf", "smb", "hml", "umd", "rmw", "cma")
    ][, .(ret.daily.aver = mean(ret.daily.cube), rf, mkt_rf, smb, hml, umd, rmw, cma), by = .(date)
    ][, unique(.SD)]

ret.post.non.follow.trd.cube.date[, felm(ret.daily.aver - rf ~ mkt_rf + smb + hml + umd + rmw + cma) %>% summary()]

# 1.5.2 ������ƽ��ʱ���portfolio����ʱ��
# ���Ƚ������е�pre-follow�׶εĳֲ�
# �ٽ�������cube��pre-follow�׶γ�����follow.trd��ͬ��Ʊ�Ĳ�λ
# �����ͬһ��cube����follow.trd��ͬ��Ʊ��pre-follow�׶εĳֲ�

# ��ȡ��pre-follow�׶����еĽ��ף�����portfolio
#position.pre.follow.trd <- f.fifo.position.all[post.follow.full == 0, .SD
    #][cube.symbol %in% unique(position.post.follow.trd$cube.symbol), .SD
    ##][date %in% unique(position.post.follow.trd$date), .SD
    #][order(date), .SD]
#ret.pre.follow.trd <- position.pre.follow.trd[ret.daily > -0.1 & ret.daily < 0.1, .SD
    #][, .(ret.daily.aver = mean(ret.daily), rf, mkt_rf, smb, hml, umd, rmw, cma), by = .(date)
    #][, unique(.SD)]
#ret.pre.follow.trd[, felm(ret.daily.aver - rf ~ mkt_rf + smb + hml + umd + rmw + cma)] %>% summary()

# ��ȡ����pre-follow�׶γֲ���follow.trd�׶γ�����ͬ��Ʊ�ĳֲ�
position.pre.follow.trd.stk <- f.fifo.position.all[post.follow.full == 0, .SD
    ][cube.symbol %in% unique(position.post.follow.trd$cube.symbol), .SD
    ][stock.symbol %in% unique(position.post.follow.trd$stock.symbol), .SD
    ][order(date), .SD]

ret.pre.follow.trd.stk <- position.pre.follow.trd.stk[ret.daily > -0.1 & ret.daily < 0.1, .SD
    ][, .(ret.daily.aver = mean(ret.daily), rf, mkt_rf, smb, hml, umd, rmw, cma), by = .(date)
    ][, unique(.SD)]

ret.pre.follow.trd.stk[, felm(ret.daily.aver - rf ~ mkt_rf + smb + hml + umd + rmw + cma)] %>% summary()

# ͬһ��cube����follow.trd��ͬ��Ʊ��pre - follow�׶εĳֲ�
# ������Ҫ�����������ݼ���Ϊ�˷���Աȣ�stock��cube��followǰ�󶼱���һһ��Ӧ

## follow֮ǰ�ĳֲ�
position.pre.follow.trd.stk.cube <- f.fifo.position.all[order(cube.symbol, stock.symbol.tag, date), .SD
    ][, .SD[mean(follow.trd) > 0 & mean(follow.trd) < 1], by = .(cube.symbol, stock.symbol.tag)
    ][, tag := .N, by = .(cube.symbol, stock.symbol, follow.trd) # ͳ�Ƴ�pre�׶κ�follow�Ĳ�ͬ�Ĵ���ʱ��
    ][, tag := min(tag), by = .(cube.symbol, stock.symbol) # �ҳ���С���Ǹ�ʱ���
    ][order(cube.symbol, stock.symbol, date), .SD
    ][follow.trd == 0, .SD, by = .(cube.symbol, stock.symbol)
    ][order(date), .SD
    ]
ret.pre.follow.trd.stk.cube <- position.pre.follow.trd.stk.cube[ret.daily > -0.1 & ret.daily < 0.1, .SD
    ][, .(ret.daily.aver = mean(ret.daily), rf, mkt_rf, smb, hml, umd, rmw, cma), by = .(date)
    ][, unique(.SD)]
ret.pre.follow.trd.stk.cube[, felm(ret.daily.aver - rf ~ mkt_rf + smb + hml + umd + rmw + cma)] %>% summary()

position.post.follow.trd.stk.cube <- f.fifo.position.all[order(cube.symbol, stock.symbol.tag, date), .SD
    ][, .SD[mean(follow.trd) > 0 & mean(follow.trd) < 1], by = .(cube.symbol, stock.symbol.tag)
    ][, tag := .N, by = .(cube.symbol, stock.symbol, follow.trd) # ͳ�Ƴ�pre�׶κ�follow�Ĳ�ͬ�Ĵ���ʱ��
    ][, tag := min(tag), by = .(cube.symbol, stock.symbol) # �ҳ���С���Ǹ�ʱ���
    ][order(cube.symbol, stock.symbol, date), .SD
    ][follow.trd == 1, .SD, by = .(cube.symbol, stock.symbol)
    ][order(date), .SD
    ]
ret.post.follow.trd.stk.cube <- position.post.follow.trd.stk.cube[ret.daily > -0.1 & ret.daily < 0.1, .SD
    ][, .(ret.daily.aver = mean(ret.daily), rf, mkt_rf, smb, hml, umd, rmw, cma), by = .(date)
    ][, unique(.SD)]
ret.post.follow.trd.stk.cube[, felm(ret.daily.aver - rf ~ mkt_rf + smb + hml + umd + rmw + cma)] %>% summary()

# 1.6 Newey west t-test ----
# 1.6.1 ���portfolio�Ա�
# ret.post.follow.trd��ret.post.non.follow.trd.cube
lm((ret.post.follow.trd[, ret.daily.aver] - ret.post.non.follow.trd.cube[, ret.daily.aver]) ~ 1) %>% lmtest::coeftest()
# ret.post.follow.trd.cube.date��ret.post.non.follow.trd.cube.date
lm((ret.post.follow.trd.cube.date[, ret.daily.aver] - ret.post.non.follow.trd.cube.date[, ret.daily.aver]) ~ 1) %>% lmtest::coeftest()

# 1.6.2 ��ʱportfolio�Ա�
# ret.post.follow.trd��ret.pre.follow.trd.stk
lm((ret.post.follow.trd[, ret.daily.aver] - ret.pre.follow.trd.stk[, ret.daily.aver]) ~ 1) %>% lmtest::coeftest()
# ret.pre.follow.trd.stk.cube��ret.post.follow.trd.stk.cube
lm((ret.post.follow.trd.stk.cube[, ret.daily.aver] - ret.pre.follow.trd.stk.cube[, ret.daily.aver]) ~ 1) %>% lmtest::coeftest()

# 1.7 ��ͼ ----
# ���뻦��300�ն�ָ��
url <- str_c(getwd(), "/data/Index")
index <- fbread(path = url, pattern = "*\\d.txt")
index <- index[, file_id := NULL
    ][Indexcd == "000300"
    ][, ':='(index_ret = Idxtrd08 / 100, index_value = Idxtrd05, date = as.Date(Idxtrd01))
    ][date %in% ret.post.follow.trd[, unique(date)], .SD
    ][, times := 1 + index_ret
    ][, value.daily := cumprod(times)]

# 1.7.1 ���portfolio�Ա�
# ret.post.follow.trd��ret.post.non.follow.trd.cube
ret.post.follow.trd[, times := 1 + ret.daily.aver
    ][, value.daily := cumprod(times)]
ret.post.non.follow.trd.cube[, times := 1 + ret.daily.aver
    ][, value.daily := cumprod(times)]
ggplot() +
    geom_line(ret.post.follow.trd, mapping = aes(x = date, y = value.daily), color = "blue", size = 1) +
    geom_line(ret.post.non.follow.trd.cube, mapping = aes(x = date, y = value.daily), color = "red", size = 1) +
    geom_line(index, mapping = aes(x = date, y = value.daily), color = "black", size = 1)

# ret.post.follow.trd.cube.date��ret.post.non.follow.trd.cube.date
ret.post.follow.trd.cube.date[, times := 1 + ret.daily.aver
    ][, value.daily := cumprod(times)]
ret.post.non.follow.trd.cube.date[, times := 1 + ret.daily.aver
    ][, value.daily := cumprod(times)]

ggplot() +
    geom_line(ret.post.follow.trd.cube.date, mapping = aes(x = date, y = value.daily), color = "blue", size = 1) +
    geom_line(ret.post.non.follow.trd.cube.date, mapping = aes(x = date, y = value.daily), color = "red", size = 1) +
    geom_line(index, mapping = aes(x = date, y = value.daily), color = "black", size = 1)

# 1.7.2 ��ʱportfolio�Ա�
# ret.post.follow.trd��ret.pre.follow.trd.stk
ret.post.follow.trd[, times := 1 + ret.daily.aver
    ][, value.daily := cumprod(times)]
ret.pre.follow.trd.stk[, times := 1 + ret.daily.aver
    ][, value.daily := cumprod(times)]

ggplot() +
    geom_line(ret.post.follow.trd, mapping = aes(x = date, y = value.daily), color = "blue", size = 1) +
    geom_line(ret.pre.follow.trd.stk, mapping = aes(x = date, y = value.daily), color = "red", size = 1) +
    geom_line(index, mapping = aes(x = date, y = value.daily), color = "black", size = 1)

# ret.pre.follow.trd.stk.cube��ret.post.follow.trd.stk.cube
ret.pre.follow.trd.stk.cube[, times := 1 + ret.daily.aver
    ][, value.daily := cumprod(times)]
ret.post.follow.trd.stk.cube[, times := 1 + ret.daily.aver
    ][, value.daily := cumprod(times)]

ggplot() +
    geom_line(ret.post.follow.trd.stk.cube, mapping = aes(x = date, y = value.daily), color = "blue", size = 1) +
    geom_line(ret.pre.follow.trd.stk.cube, mapping = aes(x = date, y = value.daily), color = "red", size = 1) +
    geom_line(index, mapping = aes(x = date, y = value.daily), color = "black", size = 1)