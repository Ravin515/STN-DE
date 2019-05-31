ld(f.surv.flw)
f.surv.flw[, tag := ifelse(date - as.Date(follow.date) > 0, 1, 0), keyby = .(cube.symbol, stck)
    ][, first.half := ifelse(tag == 0, 1, 0)
    ][, second.half := ifelse(tag == 1, 1, 0)
    ][, loss := ifelse(gain == 1, 0, 1)
    ][, ishold := ifelse(issale == 1, 0, 1)
    ][, hldt.ls.7 := ifelse(hold.time < 7, 1, 0) # sign the two-stage and loss&hold data
    ][isgain == 'gain' & first.half == 1, state := "pre.gain"
    ][isgain == 'loss' & first.half == 1, state := "pre.loss"
    ][isgain == 'gain' & second.half == 1, state := "pro.gain"
    ][isgain == 'loss' & second.half == 1, state := "pro.loss"
    ][, rlz.gain := ifelse(gain == 1 & issale == 1, 1, 0)
    ][, ppr.gain := ifelse(gain == 1 & issale == 0, 1, 0)
    ][, rlz.loss := ifelse(loss == 1 & issale == 1, 1, 0)
    ][, ppr.loss := ifelse(loss == 1 & issale == 0, 1, 0)]

# Clean some weird weight
f.surv.flw <- f.surv.flw[order(cube.symbol, date)
    ][, ':='(lngth = 1:.N, target.weight.lag = shift(target.weight, 1, type = "lag")), keyby = .(cube.symbol, stck)
    ][, num.dymc := ifelse(lngth == 1 & prev.weight.adjusted != 0 & target.weight != 0, 1, 0)
    ][target.weight == 0, num.dymc := -1
    ][prev.weight.adjusted == 0, num.dymc := 1
    ][prev.weight.adjusted != 0 & target.weight.lag == 0, num.dymc := 1
    ][order(cube.symbol, date)
    ][, lngth := NULL
    ][is.na(num.dymc), num.dymc := 0]

# create a function that make portfolio everyday and every situation without duplicated stocks
sadd <- function(x) {
    if (length(x) == 1) {
        as.character(x)
    }
    else {
        Reduce(union, as.character(x), accumulate = T) %>% lapply(str_c, collapse = ",")
    }
}

f.surv.flw[rlz.gain == 1, rlz.gain.stck := as.character(sadd(stck)), by = .(cube.symbol)
    ][rlz.loss == 1, rlz.loss.stck := as.character(sadd(stck)), by = .(cube.symbol)
    ][ppr.gain == 1, ppr.gain.stck := as.character(sadd(stck)), by = .(cube.symbol)
    ][ppr.loss == 1, ppr.loss.stck := as.character(sadd(stck)), by = .(cube.symbol)
    ][, colnames(f.surv.flw)[29:33] := lapply(.SD[, 28:32], na.locf, na.rm = F), by = .(cube.symbol)]

# calculate rlz & ppr number
f.surv.flw[, rlz.gain.num := ifelse(is.na(rlz.gain.stck), 0, str_count(rlz.gain.stck, ",") + 1)
    ][, rlz.loss.num := ifelse(is.na(rlz.loss.stck), 0, str_count(rlz.loss.stck, ",") + 1)
    ][, ppr.gain.num := ifelse(is.na(ppr.gain.stck), 0, str_count(ppr.gain.stck, ",") + 1)
    ][, ppr.loss.num := ifelse(is.na(ppr.loss.stck), 0, str_count(ppr.loss.stck, ",") + 1)]

# PGR: Proportion of Gains Realized 
# PLR: Proportion of Losses Realized
f.surv.flw[, pgr := rlz.gain.num/(rlz.gain.num + ppr.gain.num)
    ][, plr := rlz.loss.num / (rlz.loss.num + ppr.gain.num)
    ][is.nan(pgr), pgr := 0
    ][is.nan(plr), plr := 0
    ][, disp := pgr-plr]

