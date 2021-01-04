library(styleer)
url <- str_c(getwd(), "/data/Factors/")
file.names <- list.files(path = url, pattern = "*.txt|.csv")
for (i in file.names) {
    assign(str_sub(i, start = 1L, end = -5L), fread(str_c(url, toupper(i))))
}

# 2. Regression 
ld(f.main1)
ld(f.main2)
# 2.1 Pre-follow and Post-follow contrast
# 2.1.1 Daily factors
f.main1.daily <- fivefactor_daily[, setnames(.SD, 1, "date")
    ][, date := as.Date(date)
    ][f.main1, on = .(date)
    ][, ret := value / shift(value, type = "lag") - 1, by = .(cube.symbol)
    ][is.infinite(ret), ret := 0
    ][pre.period >= 0 , .SD]


f.main1.daily[post.follow == 0, lm(ret - rf ~ mkt_rf + smb + hml)] %>% summary()
f.main1.daily[post.follow == 1 & (date - follow.date < pre.period), lm(ret - rf ~ mkt_rf + smb + hml)] %>% summary()

f.main1.daily[post.follow == 0, lm(ret - rf ~ mkt_rf + smb + hml + umd)] %>% summary()
f.main1.daily[post.follow == 1 & (date - follow.date < pre.period), lm(ret - rf ~ mkt_rf + smb + hml + umd)] %>% summary()

f.main1.daily[post.follow == 0, lm(ret - rf ~ mkt_rf + smb + hml + rmw + cma)] %>% summary()
f.main1.daily[post.follow == 1 & (date - follow.date < pre.period), lm(ret - rf ~ mkt_rf + smb + hml + rmw + cma)] %>% summary()

f.main1.daily[post.follow == 1 & (date - follow.date > pre.period), lm(ret - rf ~ mkt_rf + smb + hml)] %>% summary()
f.main1.daily[post.follow == 1 & (date - follow.date > pre.period), lm(ret - rf ~ mkt_rf + smb + hml + umd)] %>% summary()
f.main1.daily[post.follow == 1 & (date - follow.date > pre.period), lm(ret - rf ~ mkt_rf + smb + hml + rmw + cma)] %>% summary()

f.main1.daily[post.follow == 1, lm(ret - rf ~ mkt_rf + smb + hml)] %>% summary()
f.main1.daily[post.follow == 1, lm(ret - rf ~ mkt_rf + smb + hml + umd)] %>% summary()
f.main1.daily[post.follow == 1, lm(ret - rf ~ mkt_rf + smb + hml + rmw + cma)] %>% summary()

f.main1.daily[, lm(ret - rf ~ mkt_rf + smb + hml)] %>% summary()
f.main1.daily[, lm(ret - rf ~ mkt_rf + smb + hml + umd)] %>% summary()
f.main1.daily[, lm(ret - rf ~ mkt_rf + smb + hml + rmw + cma)] %>% summary()


# 2.1.2 Monthly factors
f.main1[, trdmn := as.character(date) %>% str_sub(start = 1L, end = 7L) %>% str_replace_all("-", "")]
f.main1.monthly <- fivefactor_monthly[, trdmn := as.character(trdmn)
    ][f.main1, on = .(trdmn)
    ][, ret := value / shift(value, type = "lag") - 1, by = .(cube.symbol)
    ][is.infinite(ret), ret := 0
    #][ret != 0, .SD # 去除那些value一直没变的记录
    ][, .SD[.N], by = .(cube.symbol, trdmn)
    ][, ret_month := value / shift(value, type = "lag") - 1, by = .(cube.symbol)
    ][is.infinite(ret_month), ret_month := 0
    ][pre.period >= 30, .SD]

f.main1.monthly[post.follow == 0, lm(ret_month - rf ~ mkt_rf + smb + hml)] %>% summary()
f.main1.monthly[post.follow == 1 & (date - follow.date < pre.period), lm(ret_month - rf ~ mkt_rf + smb + hml)] %>% summary()
#f.main1.monthly[post.follow == 0 | (post.follow == 1 & (date - follow.date < pre.period)), lm(ret_month - rf ~ mkt_rf + smb + hml + post.follow)] %>% summary()

f.main1.monthly[post.follow == 0, lm(ret_month - rf ~ mkt_rf + smb + hml + umd)] %>% summary()
f.main1.monthly[post.follow == 1 & (date - follow.date < pre.period), lm(ret_month - rf ~ mkt_rf + smb + hml + umd)] %>% summary()
#f.main1.monthly[post.follow == 0 | (post.follow == 1 & (date - follow.date < pre.period)), lm(ret_month - rf ~ mkt_rf + smb + hml + umd + post.follow)] %>% summary()

f.main1.monthly[post.follow == 0, lm(ret_month - rf ~ mkt_rf + smb + hml + rmw + cma)] %>% summary()
f.main1.monthly[post.follow == 1 & (date - follow.date < pre.period), lm(ret_month - rf ~ mkt_rf + smb + hml + rmw + cma)] %>% summary()
#f.main1.monthly[post.follow == 0 | (post.follow == 1 & (date - follow.date < pre.period)), lm(ret_month - rf ~ mkt_rf + smb + hml + rmw + cma + post.follow)] %>% summary()

f.main1.monthly[post.follow == 1 & (date - follow.date > pre.period), lm(ret_month - rf ~ mkt_rf + smb + hml)] %>% summary()
f.main1.monthly[post.follow == 1 & (date - follow.date > pre.period), lm(ret_month - rf ~ mkt_rf + smb + hml + umd)] %>% summary()
f.main1.monthly[post.follow == 1 & (date - follow.date > pre.period), lm(ret_month - rf ~ mkt_rf + smb + hml + rmw + cma)] %>% summary()

f.main1.monthly[post.follow == 1, lm(ret_month - rf ~ mkt_rf + smb + hml)] %>% summary()
f.main1.monthly[post.follow == 1, lm(ret_month - rf ~ mkt_rf + smb + hml + umd)] %>% summary()
f.main1.monthly[post.follow == 1, lm(ret_month - rf ~ mkt_rf + smb + hml + rmw + cma)] %>% summary()

f.main1.monthly[, lm(ret_month - rf ~ mkt_rf + smb + hml)] %>% summary()
f.main1.monthly[, lm(ret_month - rf ~ mkt_rf + smb + hml + umd)] %>% summary()
f.main1.monthly[, lm(ret_month - rf ~ mkt_rf + smb + hml + rmw + cma)] %>% summary()


# 2.1.3 Weekly factors
fivefactor_weekly[, trdwk := str_c(year(trdwk), week(trdwk))]
f.main1[, trdwk := str_c(year(date), week(date))]
f.main1.weekly <- fivefactor_weekly[f.main1, on = .(trdwk)
    ][, ret := value / shift(value, type = "lag") - 1, by = .(cube.symbol)
    #][is.infinite(ret), ret := 0
    ][ret != 0, .SD # 去除那些value一直没变的记录
    ][, .SD[.N], by = .(cube.symbol, trdwk)
    ][, ret_week := value / shift(value, type = "lag") - 1, by = .(cube.symbol)
    ][is.infinite(ret_week), ret_week := 0
    ][pre.period >= 7, .SD]

f.main1.weekly[post.follow == 0, lm(ret_week - rf ~ mkt_rf + smb + hml)] %>% summary()
f.main1.weekly[post.follow == 1 & (date - follow.date < pre.period), lm(ret_week - rf ~ mkt_rf + smb + hml)] %>% summary()
f.main1.weekly[post.follow == 0 | (post.follow == 1 & (date - follow.date < pre.period)), lm(ret_week - rf ~ mkt_rf + smb + hml)] %>% summary()

f.main1.weekly[post.follow == 0, lm(ret_week - rf ~ mkt_rf + smb + hml + umd)] %>% summary()
f.main1.weekly[post.follow == 1 & (date - follow.date < pre.period), lm(ret_week - rf ~ mkt_rf + smb + hml + umd)] %>% summary()
f.main1.weekly[post.follow == 0 | (post.follow == 1 & (date - follow.date < pre.period)), lm(ret_week - rf ~ mkt_rf + smb + hml + umd)] %>% summary()

f.main1.weekly[post.follow == 0, lm(ret_week - rf ~ mkt_rf + smb + hml + rmw + cma)] %>% summary()
f.main1.weekly[post.follow == 1 & (date - follow.date < pre.period), lm(ret_week - rf ~ mkt_rf + smb + hml + rmw + cma)] %>% summary()
f.main1.weekly[post.follow == 0 | (post.follow == 1 & (date - follow.date < pre.period)), lm(ret_week - rf ~ mkt_rf + smb + hml + rmw + cma)] %>% summary()

f.main1.weekly[post.follow == 1 & (date - follow.date > pre.period), lm(ret_week - rf ~ mkt_rf + smb + hml)] %>% summary()
f.main1.weekly[post.follow == 1 & (date - follow.date > pre.period), lm(ret_week - rf ~ mkt_rf + smb + hml + umd)] %>% summary()
f.main1.weekly[post.follow == 1 & (date - follow.date > pre.period), lm(ret_week - rf ~ mkt_rf + smb + hml + rmw + cma + as.numeric(trd.num / 1000))] %>% summary()

f.main1.weekly[post.follow == 1, lm(ret_week - rf ~ mkt_rf + smb + hml)] %>% summary()
f.main1.weekly[post.follow == 1, lm(ret_week - rf ~ mkt_rf + smb + hml + umd)] %>% summary()
f.main1.weekly[post.follow == 1, lm(ret_week - rf ~ mkt_rf + smb + hml + rmw + cma)] %>% summary()

f.main1.weekly[, lm(ret_week - rf ~ mkt_rf + smb + hml)] %>% summary()
f.main1.weekly[, lm(ret_week - rf ~ mkt_rf + smb + hml + umd)] %>% summary()
f.main1.weekly[, lm(ret_week - rf ~ mkt_rf + smb + hml + rmw + cma)] %>% summary()

