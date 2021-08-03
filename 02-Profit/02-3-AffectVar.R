library(styleer)
library(lfe)
library(quantreg)
library(rqpd)
url <- str_c(getwd(), "/data/Factors/")
file.names <- list.files(path = url, pattern = "*.txt|.csv")
for (i in file.names) {
    assign(str_sub(i, start = 1L, end = -5L), fread(str_c(url, toupper(i))))
}
rm(url, file.names, i)

ld(f.main1)
ld(f.main2)
ld(f.cube.ret.sp)
ld(f.hold.price.1806)

## ��ȡ���ֱ��� ----
# 1. ���¼���stock.list��stock number, stock.ratio
# �趨һ�����ӹ�Ʊlist�ĺ���
sadd <- function(x) {
    if (length(x) == 1) {
        x
    } else {
        as.list(Reduce(c, x, accumulate = T))
    }
}
# 1.1.1 ���ȼ�����Щ��һ�γ��ֵ�stock
start.cube.pre.plate <- f.hold.price.1806[, .SD[1], by = .(cube.symbol, stock.symbol)
    ][prev.weight.adjusted != 0, .SD
    ][, date := as.Date(created.at)
    ][, .(stock.list.start = list(stock.symbol)), by = .(cube.symbol)]

# 1.1.2 ��0-1�Ĺ�Ʊ��������Щ��һ����ƽ̨�򣬻�����Щ��ƽ̨���֮�������εĹ�Ʊ
stock0_1 <- f.hold.price.1806[prev.weight.adjusted == 0, .SD
    ][, date := as.Date(created.at)
    ][order(cube.symbol, date), .SD
    ][, .(stock.symbol = list(stock.symbol)), by = .(cube.symbol, date)
    ][, .(stock.list.buy = sadd(stock.symbol), date), by = .(cube.symbol)]

# 1.1.3 ��1-0�Ĺ�Ʊ, ������Щ��ƽ̨����ƽ̨�����Щ����ƽ̨����ƽ̨��ֵĹ�Ʊ
stock1_0.second <- f.hold.price.1806[, .SD[-1], by = .(cube.symbol, stock.symbol)
    ][target.weight == 0, .SD
    ][, date := as.Date(created.at)]
stock1_0.first <- f.hold.price.1806[, .SD[1], by = .(cube.symbol, stock.symbol)
    ][target.weight == 0, .SD
    ][, date := as.Date(created.at)]
stock1_0 <- rbindlist(list(stock1_0.first, stock1_0.second), fill = T)
stock1_0 <- stock1_0[order(cube.symbol, date), .SD
    ][, .(stock.symbol = list(stock.symbol)), by = .(cube.symbol, date)
    ][, .(stock.list.sell = sadd(stock.symbol), date), by = .(cube.symbol)]

# 1.1.4 �����ű��ϲ���retΪ��׼��cube.symbol��date����
stock.info <- stock0_1[stock1_0[start.cube.pre.plate[f.cube.ret.sp[, .(cube.symbol, date)], on = .(cube.symbol)], on = .(cube.symbol, date), roll = T], on = .(cube.symbol, date), roll = T]

# 1.1.5 ��������list�Ĳ���
stock.info <- stock.info[, .(stock.list = which(table(c(unlist(stock.list.start), unlist(stock.list.buy), unlist(stock.list.sell))) %% 2 != 0) %>% names() %>% list()), by = .(cube.symbol, date)]

sv(stock.info, svname = "stock.info")
f.main1 <- stock.info[f.main1[, label := NULL], on = .(cube.symbol, date)
    ][, stock.num := lapply(stock.list, length) %>% unlist()]
sv(f.main1, svname = "f.main1")
f.main2 <- stock.info[f.main2[, label := NULL], on = .(cube.symbol, date)
    ][, stock.num := lapply(stock.list, length) %>% unlist()]
sv(f.main2, svname = "f.main2")

##1.2# ����ÿ��ֱֲ��� stock.ratio
#ld(stock.info)
#cl <- makeCluster(8)
#registerDoParallel(cl)
#stock.ratio <- f.cube.rb.sp[, .(cube.symbol, date = as.Date(created.at), created.at, stock.symbol, price, target.weight, prev.weight.adjusted)
    #][stock.info, on = .(cube.symbol, date)
    #][!is.na(created.at)
    #][order(cube.symbol, date, created.at, stock.symbol), .SD
    #][,
        #.(stock.ratio = {
            ##ratio <- list()
            #foreach(i = 1:.N) %dopar% {
                #library(data.table)
                #sl1 <- unlist(stock.list[i])
                #p.fward <- .SD[i:.N
                    #][order(stock.symbol, created.at)
                    #][stock.symbol %in% sl1, .SD[1], .SDcols = c("date", "created.at", "target.weight", "prev.weight.adjusted"), by = .(stock.symbol)]

                #sl2 <- setdiff(sl1, p.fward[["stock.symbol"]])
                #p.bward <- .SD[1:i
                    #][order(stock.symbol, created.at)
                    #][stock.symbol %in% sl2, .SD[.N], .SDcols = c("date", "created.at", "target.weight", "prev.weight.adjusted"), by = .(stock.symbol)]

                #fp <- p.fward[, .(weight = fifelse(prev.weight.adjusted > 0, prev.weight.adjusted, target.weight), stock.symbol)]
                #bp <- p.bward[, .(weight = fifelse(target.weight > 0, target.weight, prev.weight.adjusted), stock.symbol)]
                #rbindlist(list(fp, bp))
            #}
            ##ratio
            #}
        #, date, created.at)
        #, by = .(cube.symbol)
        #]
#sv(stock.ratio, svname = "stock.ratio")

#stock.ratio.flat <- stock.ratio[, .SD[.N], by = .(cube.symbol, date)
    #][, rbindlist(stock.ratio), by = .(date, cube.symbol)
    #][order(cube.symbol, date)]

#sv(stock.ratio.flat, svname = "stock.ratio.flat")

# 2. ���¡����Ƿ�������ݼ������ݼ�������һ��֮���Ƿ��д���
#f.main1 <- f.cube.ret.sp[, .(cube.symbol, date, label)
    #][f.main1, on = .(cube.symbol, date)
    #][, new.stock := fifelse(label == "", 0, 1)
    #][, new.stock.7day := Reduce('+', shift(new.stock, 0:5, fill = 0)), by = .(cube.symbol)
    #][, new.stock.7day := fifelse(new.stock.7day > 1, 1, new.stock.7day)
    #]


# 3. top10�������Ƿ��ۻ�������Ϊ�ܡ��¡����ǰ10λ��ѩ�����Ļ��� (����)
#f.cube.ret.sp[date > as.Date("2016-06-01"),
    #ret_accu_week := {
        #ret <- vector()
        #for (i in 7:.N) {
            #ret[i] <- value[i] / value[i - 6] - 1
        #}
        #ret
    #}, by = .(cube.symbol)
    #]

#f.cube.ret.sp[date > as.Date("2016-06-01"),
    #ret_accu_month := {
        #ret <- vector()
        #for (i in 30:.N) {
            #ret[i] <- value[i] / value[i - 29] - 1
        #}
        #ret
    #}, by = .(cube.symbol)
    #][is.infinite(ret_accu_month), ret_accu_month := NA]

#f.cube.ret.sp[date > as.Date("2016-06-01"),
    #ret_accu_year := {
        #ret <- vector()
        #for (i in 360:.N) {
            #ret[i] <- value[i] / value[i - 359] - 1
        #}
        #ret
    #}, by = .(cube.symbol)]

#f.cube.ret.sp[sapply(f.cube.ret.sp, is.infinite)] <- NA

#week_top <- f.cube.ret.sp[!is.na(ret_accu_week), .SD
    #][order(date, - ret_accu_week), .SD
    #][, tag := .N, by = .(date)
    ##][ret_accu_week < quantile(ret_accu_week, probs = 0.997), .SD
    #][, .SD[1:10], .SDcols = c("cube.symbol", "ret_accu_week"), by = .(date)
    #]

#month_top <- f.cube.ret.sp[!is.na(ret_accu_month), .SD
    #][order(date, - ret_accu_month), .SD
    #][, tag := .N, by = .(date)
    ##][ret_accu_week < quantile(ret_accu_week, probs = 0.990, na.rm = T), .SD
    #][, .SD[1:10], .SDcols = c("cube.symbol", "ret_accu_month"), by = .(date)
    #]



# 4. ��������
# 4.1 ���˽����������⣬�����������������ͬʱ�ó�
#ld(f.cube.rb.mst.1803)
#f.cube.rb.sp <- f.cube.rb.mst.1803[cube.type == "SP"]
#sv(f.cube.rb.sp, svname = "f.cube.rb.sp")

ld(f.cube.rb.sp.mst.1806)
f.cube.rb.sp <- f.cube.rb.sp.mst.1806[, .(cube.symbol, created.at, stock.symbol, price, target.weight, prev.weight.adjusted)
    ][order(cube.symbol, stock.symbol, created.at), .SD]

# 4.2 ��nan��������䲢�����ۻ�������������Ľ�������
f.cube.rb.sp<- f.cube.rb.sp[!is.nan(target.weight) & !is.nan(prev.weight.adjusted)
    ][!(target.weight == 0 & prev.weight.adjusted == 0), .SD
    ][, buy := fifelse(target.weight > prev.weight.adjusted, 1, 0)
    ][, sell := fifelse(target.weight > prev.weight.adjusted, 0, 1)
    ][, date := as.Date(created.at)
    ][order(cube.symbol, date), .SD
    ][, ':='(buy.num = cumsum(buy), sell.num = cumsum(sell)), by = .(cube.symbol)
    ]
order.num <- f.cube.rb.sp[, .SD[.N], by = .(cube.symbol, date)
    ][, .(cube.symbol, date, buy.num, sell.num)]

f.main1 <- order.num[f.main1, on = .(cube.symbol, date), roll = T, rollends = c(F, T)]
f.main1[, ':='(buy.num = fifelse(is.na(buy.num), 0, buy.num), sell.num = fifelse(is.na(sell.num), 0, sell.num))
    ][, buy.num.30day := (buy.num - shift(buy.num, type = "lag", n = 22L)) / 22, by = .(cube.symbol)
    ][, sell.num.30day := (sell.num - shift(sell.num, type = "lag", n = 22L)) / 22, by = .(cube.symbol)]

# 5. ͣ��
# 5.1 �м����һЩͣ�Ƶ�����������Ʊ������Ϊ0����retΪ0
#f.main1[, stop := fifelse(stock.num != 0 & ret == 0, 1, 0)
    #][is.na(stop), stop := 0
    #][, tag := stop - shift(stop, type = "lag", n = 1L), by = .(cube.symbol)
    #][, stop.open := fifelse(tag == -1, 1, 0)
    #][, tag := NULL]

# 6. ���еĹ�Ʊ������
# �����ȥ����֮�ڳ��еĲ��ظ���Ʊ����
f.main1[pre.period > 3, stock.num.7day := {
    a <- vector()
    for (i in 6:.N) {
        a[i] <- lapply(stock.list[(i-5):i], unlist) %>% unlist()  %>% unique() %>% length()
    }
    a
}, by = .(cube.symbol)]

# �����ȥ�����ڳ��й�Ʊ��ƽ������
f.main1[pre.period > 3, stock.mean.7day := {
    a <- vector()
    for (i in 6:.N) {
        a[i] <- mean(stock.num[(i-5):i])
    }
    a
}, by = .(cube.symbol)]

f.main1.daily <- fivefactor_daily[, setnames(.SD, 1, "date")
    ][, date := as.Date(date)
    ][f.main1, on = .(date)
    ][order(cube.symbol, date), .SD
    ][, ret := value / shift(value, type = "lag") - 1, by = .(cube.symbol)
    ][is.infinite(ret), ret := 0
    ][, period := max(date) - min(date), by = .(cube.symbol)
    ][period > 90, .SD
    ][, .SD[max(date) - follow.date > pre.period], by = .(cube.symbol)
    ][, start := min(date), by = .(cube.symbol)
    ][, post.follow := fifelse(date > follow.date, 1, 0), by = .(cube.symbol)
    ][, active.day := date - min(date), by = .(cube.symbol)]

#### �ع�
outlier <- f.main1.daily[ret > 0.1 | ret < -0.1, unique(cube.symbol)]

f.main1.daily[!(cube.symbol %in% outlier) & (post.follow == 0 | post.follow == 1 & (date - follow.date < pre.period)), felm(ret ~ post.follow + mkt_rf + smb + hml + umd + rmw + cma  | cube.symbol + follow.date)] %>% summary()

f.main1.daily[!(cube.symbol %in% outlier) & (post.follow == 0 | post.follow == 1 & (date - follow.date < pre.period)), felm(ret ~ post.follow + mkt_rf + smb + hml + umd + rmw + cma + I(active.day * post.follow) + active.day | cube.symbol + follow.date)] %>% summary() # ��Ծ����

f.main1.daily[!(cube.symbol %in% outlier) & (post.follow == 0 | post.follow == 1 & (date - follow.date < pre.period)), felm(ret ~ post.follow + mkt_rf + smb + hml + umd + I((buy.num.30day + sell.num.30day) * post.follow) + I(buy.num.30day + sell.num.30day) | cube.symbol + follow.date)] %>% summary()

f.main1.daily[(!(cube.symbol %in% outlier) & (post.follow == 0) | post.follow == 1 & (date - follow.date < pre.period)), felm(ret ~ post.follow + mkt_rf + smb + hml + umd + buy.num.30day + I(buy.num.30day * post.follow) + sell.num.30day + I(sell.num.30day * post.follow) | cube.symbol + follow.date)] %>% summary()

#f.main1.daily[(post.follow == 0 | (post.follow == 1 & (date - follow.date < pre.period))), felm(ret ~ post.follow + mkt_rf + smb + hml + umd + new.stock + I(post.follow * new.stock) + new.stock | cube.symbol + follow.date)] %>% summary() # �����Ƿ��д���

#f.main1.daily[(post.follow == 0 | (post.follow == 1 & (date - follow.date < pre.period))), felm(ret ~ post.follow + mkt_rf + smb + hml + umd + new.stock.7day + I(post.follow * new.stock.7day) | cube.symbol + follow.date)] %>% summary() # ��ȥ�����Ƿ��д���

#f.main1.daily[ret < 1 & ret > -1 & (post.follow == 0), felm(ret ~ post.follow + mkt_rf + smb + hml + umd + new.stock + I(post.follow * new.stock) + new.stock.7day + I(post.follow * new.stock.7day) | cube.symbol + follow.date)] %>% summary()

f.main1.daily[!(cube.symbol %in% outlier) & (post.follow == 0 | (post.follow == 1 & (date - follow.date < pre.period))), felm(ret ~ post.follow + mkt_rf + smb + hml + umd + stock.num + I(stock.num * post.follow) | cube.symbol + follow.date)] %>% summary() # �����Ʊ��������

f.main1.daily[!(cube.symbol %in% outlier) & (post.follow == 0 | (post.follow == 1 & (date - follow.date < pre.period))), felm(ret ~ post.follow + mkt_rf + smb + hml + umd + stock.mean.7day + I(stock.mean.7day * post.follow) | cube.symbol + follow.date)] %>% summary() # ��ȥ�����ڳ��й��Ĺ�Ʊƽ����

f.main1.daily[!(cube.symbol %in% outlier) & (post.follow == 0 | (post.follow == 1 & (date - follow.date < pre.period))), felm(ret ~ post.follow + mkt_rf + smb + hml + umd + stock.num.7day + I(stock.num.7day * post.follow) | cube.symbol + follow.date)] %>% summary() # ��ȥ�����ڳ��й��Ĳ��ظ��Ĺ�Ʊ����

f.main1.daily[!(cube.symbol %in% outlier) & (post.follow == 0 | (post.follow == 1 & (date - follow.date < pre.period))), felm(ret ~ post.follow + mkt_rf + smb + hml + umd + stock.num + I(stock.num * post.follow) + stock.mean.7day + I(stock.mean.7day * post.follow) + stock.num.7day + I(stock.num.7day * post.follow) | cube.symbol + follow.date)] %>% summary()

#f.main1.daily[ret < 1 & (post.follow == 0 | (post.follow == 1 & (date - follow.date < pre.period))), felm(ret ~ post.follow + mkt_rf + smb + hml + umd + stop.open + I(stop.open * post.follow) | cube.symbol + follow.date)] %>% summary() # ͣ�ƺ��̵�һ��

f.main1.daily[!(cube.symbol %in% outlier) & (post.follow == 0 | (post.follow == 1 & (date - follow.date < pre.period))), felm(ret ~ post.follow + active.day + buy.num.30day + sell.num.30day + stock.num + stock.mean.7day + stock.num.7day | cube.symbol + follow.date)] %>% summary()