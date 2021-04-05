styleer::ld(f.hold.price)
library(data.table)
library(stringr)
library(zoo)
# choose all of the A stocks from SH&SZ
f.a.stock<- f.hold.price[nchar(stock.symbol) > 5]
save(f.a.stock, file = "f.a.stock.Rdata")

load(file = "f.a.stock.Rdata")
# generate a stock.code list that download closed price 
a <- f.a.stock[nchar(stock.symbol) > 5, .(unique(stock.symbol))
    ][, stck := str_extract(V1, "([0-9]+)")]
stck <- list(a[, formatC(stck, flag = '0', width = 6)])
fwrite(stck, file = "stck.csv", col.names = F)

# Binding two download tabs
sp.rb.dl1 <- fread("C:/Users/MrStylee/source/repos/STN-DE/01-LnAcpt/Clprc/TRD_Dalyr.txt", header = T, sep = '\t', encoding = 'UTF-8')
sp.rb.dl1[, ":="(stck = (formatC(Stkcd, flag = '0', width = 6)), date = as.Date(Trddt))
    ][, ":="(Trddt = NULL, Stkcd = NULL)]
sp.rb.dl2 <- fread("C:/Users/MrStylee/source/repos/STN-DE/01-LnAcpt/Clprc/TRD_Dalyr1.txt", header = T, sep = '\t', encoding = 'UTF-8')
sp.rb.dl2[, ":="(stck = (formatC(Stkcd, flag = '0', width = 6)), date = as.Date(Trddt))
    ][, ":="(Trddt = NULL, Stkcd = NULL)]
sp.rb.dl <- rbindlist(list(sp.rb.dl1, sp.rb.dl2)) %>% as.data.table()
rm(sp.rb.dl1, sp.rb.dl2)

f.a.stock[, ':='(stck = str_extract(stock.symbol, "([0-9]+)"), date = as.Date(created.at))
    ][, issale := fifelse(prev.weight.adjusted > target.weight, 1, 0)]

# filling all of the omitting date with no trades
CJ <- f.a.stock[order(cube.symbol, stck, created.at)
    ][, .(date = seq(as.Date(min(created.at)), as.Date(max(created.at)), by = 'day')), keyby = .(cube.symbol, stck)
    ]

sp.rb.cj <- f.a.stock[CJ, on = .(cube.symbol, stck, date), nomatch = NA
    ][order(cube.symbol, stck, date)]
a <- sp.rb.dl[sp.rb.cj, on = .(date, stck), nomatch = 0]
sp.rb <- a[!is.na(Clsprc), .(Clsprc, stck, date, price, cube.symbol, created.at, issale, hold.price, target.weight, prev.weight.adjusted)]
rm(CJ, f.a.stock, sp.rb.cj, sp.rb.dl, a)

sp.rb <- sp.rb[order(cube.symbol, stck, date)
    ][, hold.price := na.locf(hold.price, na.rm = F), keyby = .(cube.symbol, stck)
    ][issale == 0, buy.at := date, by = .(cube.symbol, stck)
    ][, buy.at := na.locf(buy.at, na.rm = F), keyby = .(cube.symbol, stck)
    ][, hold.time := as.numeric(difftime(date, buy.at, units = "days")), keyby = .(cube.symbol, stck)
    ][issale != 0 | is.na(issale)
    ][is.na(issale), issale := 0]

f.surv <- sp.rb[issale != 0 | hold.price != 0
    ][, hold.price.lst := shift(hold.price, 1, fill = 0), keyby = .(cube.symbol, stck)
    ][issale == 1, isgain := ifelse(price - hold.price.lst > 0, "gain", "loss")
    ][issale == 0, isgain := ifelse(hold.price - Clsprc < 0, "gain", "loss")
    ][issale == 1, gain := ifelse(price - hold.price.lst > 0, 1, 0)
    ][issale == 0, gain := ifelse(hold.price - Clsprc < 0, 1, 0)]

rm(sp.rb)
save(f.surv, file = "f.surv.Rdata")

