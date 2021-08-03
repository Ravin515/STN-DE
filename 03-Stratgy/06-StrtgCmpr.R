library(styleer)
full <- fbread(path = str_c(getwd(), "./data/strategy"), pattern = "*.csv") %>% unique()
cmpr <- full[, file_id := NULL
    ][variable != "沪深300", .SD
    ]
url <- str_c(getwd(), "/data/Index")
index <- fbread(path = url, pattern = "*\\d.txt")
index <- index[, file_id := NULL
    ][Indexcd == "000300"
    ][, ':='(index_ret = Idxtrd08 / 100, net.value = Idxtrd05, date = as.IDate(Idxtrd01))]
index <- index[date %between% c(min(cmpr$date), max(cmpr$date)), .(net.value, date, variable = "沪深300")]

cmpr <- rbindlist(list(cmpr, index), use.names = T)

cmpr <- cmpr[order(variable, date), .SD
    ][, cum.ret := (net.value[.N] / net.value[1] - 1) * 100, by = .(variable) # 累积收益率
    ][, annual.ret := (cum.ret / (date[.N] - date[1])) * 365, by = .(variable) # 年化收益
    ][, max.retreat:= {
        a <- vector()
        for (i in 1:.N) {
           a[i]  <- min(net.value[(i + 1):.N] / net.value[i]-1) *(-100)
        }
        max(a, na.rm = T)
    }, by = .(variable) # 最大回撤
    ][, ret.daily := net.value/ shift(net.value, type = "lag") - 1, by = .(variable)
    ][, vol := sd(ret.daily, na.rm = T) *(365^0.5), by = .(variable) # 标准差
    ][, sharp.ratio := (annual.ret/100 - 3.5/100)/vol, by = .(variable)
    ][, unique(.SD), .SDcols = -c("date", "net.value", "ret.daily")]

fwrite(cmpr, "cmpr.csv")