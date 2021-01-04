library(styleer)
url <- str_c(getwd(), "/data/Factors/")
file.names <- list.files(path = url, pattern = "*.txt|.csv")
for (i in file.names) {
    assign(str_sub(i, start = 1L, end = -5L), fread(str_c(url, toupper(i))))
}
rm(file.names, i, url)

ld(f.main2)
f.main2[, trdmn :=  as.character(date) %>% str_sub(start = 1L, end = 7L) %>% str_replace_all("-", "")]
alpha <- fivefactor_daily[, setnames(.SD, 1, "date")
    ][, date := as.Date(date)
    ][f.main2, on = .(date)
    ][, ret := value / shift(value, type = "lag") - 1, by = .(cube.symbol)
    ][is.infinite(ret), ret := 0
    ][ret != 0, .SD #去除那些清仓的记录
    ][, trdmn := as.character(date) %>% str_sub(start = 1L, end = 7L) %>% str_replace_all("-", "")]


alpha_three_factor <- alpha[, .(lm(ret - rf ~ mkt_rf + smb + hml) %>% coef()), by = .(cube.symbol, trdmn)
    ][, .SD[1], by = .(cube.symbol, trdmn)
    ][, setnames(.SD, 3, "alpha")]

alpha_four_factor <- alpha[, .(lm(ret - rf ~ mkt_rf + smb + hml + umd) %>% coef()), by = .(cube.symbol, trdmn)
    ][, .SD[1], by = .(cube.symbol, trdmn)
    ][, setnames(.SD, 3, "alpha")]

alpha_five_factor <- alpha[, .(lm(ret - rf ~ mkt_rf + smb + hml + rmw + cma) %>% coef()), by = .(cube.symbol, trdmn)
    ][, .SD[1], by = .(cube.symbol, trdmn)
    ][, setnames(.SD, 3, "alpha")]

alpha_six_factor <- alpha[, .(lm(ret - rf ~ mkt_rf + smb + hml + umd + rmw + cma) %>% coef()), by = .(cube.symbol, trdmn)
    ][, .SD[1], by = .(cube.symbol, trdmn)
    ][, setnames(.SD, 3, "alpha")]

alpha_thrfac_reg <- alpha_three_factor[f.main2[, .SD[.N], by = .(cube.symbol, trdmn)], on = .(cube.symbol, trdmn), nomatch = 0]
alpha_foufac_reg <- alpha_four_factor[f.main2[, .SD[.N], by = .(cube.symbol, trdmn)], on = .(cube.symbol, trdmn), nomatch = 0]
alpha_fivfac_reg <- alpha_five_factor[f.main2[, .SD[.N], by = .(cube.symbol, trdmn)], on = .(cube.symbol, trdmn), nomatch = 0]
alpha_sixfac_reg <- alpha_six_factor[f.main2[, .SD[.N], by = .(cube.symbol, trdmn)], on = .(cube.symbol, trdmn), nomatch = 0]

alpha_thrfac_reg[oud > 0 & date - follow.date < 30, felm(alpha ~ log(ind + 1) + log(oud + 1) + I(ln.cntr * 100) + as.numeric(trd.num / 1000) + as.numeric(active.day / 365) + mmt | cube.symbol + follow.date + stock.num)] %>% summary()
alpha_thrfac_reg[oud > 0 & date - follow.date < 60, felm(alpha ~ log(ind + 1) + log(oud + 1) + I(ln.cntr * 100) + as.numeric(trd.num / 1000) + as.numeric(active.day / 365) + mmt | cube.symbol + follow.date + stock.num)] %>% summary()
alpha_thrfac_reg[oud > 0 & date - follow.date < 90, felm(alpha ~ log(ind + 1) + log(oud + 1) + I(ln.cntr * 100) + as.numeric(trd.num / 1000) + as.numeric(active.day / 365) + mmt | cube.symbol + follow.date + stock.num)] %>% summary()
alpha_thrfac_reg[oud > 0 & date - follow.date < 120, felm(alpha ~ log(ind + 1) + log(oud + 1) + I(ln.cntr * 100) + as.numeric(trd.num / 1000) + as.numeric(active.day / 365) + mmt | cube.symbol + follow.date + stock.num)] %>% summary()
alpha_thrfac_reg[oud > 0 & date - follow.date < 150, felm(alpha ~ log(ind + 1) + log(oud + 1) + I(ln.cntr * 100) + as.numeric(trd.num / 1000) + as.numeric(active.day / 365) + mmt | cube.symbol + follow.date + stock.num)] %>% summary()
alpha_thrfac_reg[oud > 0 & date - follow.date < 180, felm(alpha ~ log(ind + 1) + log(oud + 1) + I(ln.cntr * 100) + as.numeric(trd.num / 1000) + as.numeric(active.day / 365) + mmt | cube.symbol + follow.date + stock.num)] %>% summary()
alpha_thrfac_reg[oud > 0 & date - follow.date < 210, felm(alpha ~ log(ind + 1) + log(oud + 1) + I(ln.cntr * 100) + as.numeric(trd.num / 1000) + as.numeric(active.day / 365) + mmt | cube.symbol + follow.date + stock.num)] %>% summary()
alpha_thrfac_reg[oud > 0 & date - follow.date < 360, felm(alpha ~ log(ind + 1) + log(oud + 1) + I(ln.cntr * 100) + as.numeric(trd.num / 1000) + as.numeric(active.day / 365) + mmt | cube.symbol + follow.date + stock.num)] %>% summary()

alpha_foufac_reg[oud > 0 & date - follow.date < 30, felm(alpha ~ log(ind + 1) + log(oud + 1) + I(ln.cntr * 100) + as.numeric(trd.num / 1000) + as.numeric(active.day / 365) + mmt | cube.symbol + follow.date + stock.num)] %>% summary()
alpha_foufac_reg[oud > 0 & date - follow.date < 60, felm(alpha ~ log(ind + 1) + log(oud + 1) + I(ln.cntr * 100) + as.numeric(trd.num / 1000) + as.numeric(active.day / 365) + mmt | cube.symbol + follow.date + stock.num)] %>% summary()
alpha_foufac_reg[oud > 0 & date - follow.date < 90, felm(alpha ~ log(ind + 1) + log(oud + 1) + I(ln.cntr * 100) + as.numeric(trd.num / 1000) + as.numeric(active.day / 365) + mmt | cube.symbol + follow.date + stock.num)] %>% summary()
alpha_foufac_reg[oud > 0 & date - follow.date < 120, felm(alpha ~ log(ind + 1) + log(oud + 1) + I(ln.cntr * 100) + as.numeric(trd.num / 1000) + as.numeric(active.day / 365) + mmt | cube.symbol + follow.date + stock.num)] %>% summary()
alpha_foufac_reg[oud > 0 & date - follow.date < 150, felm(alpha ~ log(ind + 1) + log(oud + 1) + I(ln.cntr * 100) + as.numeric(trd.num / 1000) + as.numeric(active.day / 365) + mmt | cube.symbol + follow.date + stock.num)] %>% summary()
alpha_foufac_reg[oud > 0 & date - follow.date < 180, felm(alpha ~ log(ind + 1) + log(oud + 1) + I(ln.cntr * 100) + as.numeric(trd.num / 1000) + as.numeric(active.day / 365) + mmt | cube.symbol + follow.date + stock.num)] %>% summary()
alpha_foufac_reg[oud > 0 & date - follow.date < 210, felm(alpha ~ log(ind + 1) + log(oud + 1) + I(ln.cntr * 100) + as.numeric(trd.num / 1000) + as.numeric(active.day / 365) + mmt | cube.symbol + follow.date + stock.num)] %>% summary()
alpha_foufac_reg[oud > 0 & date - follow.date < 360, felm(alpha ~ log(ind + 1) + log(oud + 1) + I(ln.cntr * 100) + as.numeric(trd.num / 1000) + as.numeric(active.day / 365) + mmt | cube.symbol + follow.date + stock.num)] %>% summary()

alpha_fivfac_reg[oud > 0 & date - follow.date < 30, felm(alpha ~ log(ind + 1) + log(oud + 1) + I(ln.cntr * 100) + as.numeric(trd.num / 1000) + as.numeric(active.day / 365) + mmt | cube.symbol + follow.date + stock.num)] %>% summary()
alpha_fivfac_reg[oud > 0 & date - follow.date < 60, felm(alpha ~ log(ind + 1) + log(oud + 1) + I(ln.cntr * 100) + as.numeric(trd.num / 1000) + as.numeric(active.day / 365) + mmt | cube.symbol + follow.date + stock.num)] %>% summary()
alpha_fivfac_reg[oud > 0 & date - follow.date < 90, felm(alpha ~ log(ind + 1) + log(oud + 1) + I(ln.cntr * 100) + as.numeric(trd.num / 1000) + as.numeric(active.day / 365) + mmt | cube.symbol + follow.date + stock.num)] %>% summary()
alpha_fivfac_reg[oud > 0 & date - follow.date < 120, felm(alpha ~ log(ind + 1) + log(oud + 1) + I(ln.cntr * 100) + as.numeric(trd.num / 1000) + as.numeric(active.day / 365) + mmt | cube.symbol + follow.date + stock.num)] %>% summary()
alpha_fivfac_reg[oud > 0 & date - follow.date < 150, felm(alpha ~ log(ind + 1) + log(oud + 1) + I(ln.cntr * 100) + as.numeric(trd.num / 1000) + as.numeric(active.day / 365) + mmt | cube.symbol + follow.date + stock.num)] %>% summary()
alpha_fivfac_reg[oud > 0 & date - follow.date < 180, felm(alpha ~ log(ind + 1) + log(oud + 1) + I(ln.cntr * 100) + as.numeric(trd.num / 1000) + as.numeric(active.day / 365) + mmt | cube.symbol + follow.date + stock.num)] %>% summary()
alpha_fivfac_reg[oud > 0 & date - follow.date < 210, felm(alpha ~ log(ind + 1) + log(oud + 1) + I(ln.cntr * 100) + as.numeric(trd.num / 1000) + as.numeric(active.day / 365) + mmt | cube.symbol + follow.date + stock.num)] %>% summary()
alpha_fivfac_reg[oud > 0 & date - follow.date < 360, felm(alpha ~ log(ind + 1) + log(oud + 1) + I(ln.cntr * 100) + as.numeric(trd.num / 1000) + as.numeric(active.day / 365) + mmt | cube.symbol + follow.date + stock.num)] %>% summary()

alpha_sixfac_reg[oud > 0 & date - follow.date < 30, felm(alpha ~ log(ind + 1) + log(oud + 1) + I(ln.cntr * 100) + as.numeric(trd.num / 1000) + as.numeric(active.day / 365) + mmt | cube.symbol + follow.date + stock.num)] %>% summary()
alpha_sixfac_reg[oud > 0 & date - follow.date < 60, felm(alpha ~ log(ind + 1) + log(oud + 1) + I(ln.cntr * 100) + as.numeric(trd.num / 1000) + as.numeric(active.day / 365) + mmt | cube.symbol + follow.date + stock.num)] %>% summary()
alpha_sixfac_reg[oud > 0 & date - follow.date < 90, felm(alpha ~ log(ind + 1) + log(oud + 1) + I(ln.cntr * 100) + as.numeric(trd.num / 1000) + as.numeric(active.day / 365) + mmt | cube.symbol + follow.date + stock.num)] %>% summary()
alpha_sixfac_reg[oud > 0 & date - follow.date < 120, felm(alpha ~ log(ind + 1) + log(oud + 1) + I(ln.cntr * 100) + as.numeric(trd.num / 1000) + as.numeric(active.day / 365) + mmt | cube.symbol + follow.date + stock.num)] %>% summary()
alpha_sixfac_reg[oud > 0 & date - follow.date < 150, felm(alpha ~ log(ind + 1) + log(oud + 1) + I(ln.cntr * 100) + as.numeric(trd.num / 1000) + as.numeric(active.day / 365) + mmt | cube.symbol + follow.date + stock.num)] %>% summary()
alpha_sixfac_reg[oud > 0 & date - follow.date < 180, felm(alpha ~ log(ind + 1) + log(oud + 1) + I(ln.cntr * 100) + as.numeric(trd.num / 1000) + as.numeric(active.day / 365) + mmt | cube.symbol + follow.date + stock.num)] %>% summary()
alpha_sixfac_reg[oud > 0 & date - follow.date < 210, felm(alpha ~ log(ind + 1) + log(oud + 1) + I(ln.cntr * 100) + as.numeric(trd.num / 1000) + as.numeric(active.day / 365) + mmt | cube.symbol + follow.date + stock.num)] %>% summary()
alpha_sixfac_reg[oud > 0 & date - follow.date < 360, felm(alpha ~ log(ind + 1) + log(oud + 1) + I(ln.cntr * 100) + as.numeric(trd.num / 1000) + as.numeric(active.day / 365) + mmt | cube.symbol + follow.date + stock.num)] %>% summary()