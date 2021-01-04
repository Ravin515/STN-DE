library(styleer)
ld(f.cube.ret.sp)
f.cube.ret.sp <- f.cube.ret.sp[, unique(.SD), .SDcol = 1:3]
f.cube.ret.sp[date > as.Date("2016-06-01"),
    ret_7day := {
        ret <- vector()
        for (i in 7:.N) {
             ret[i] <- value[i] / value[i - 6] - 1
        }
        ret
    }, by = .(cube.symbol)]

f.cube.ret.sp[date > as.Date("2016-06-01"),
    ret_30day := {
        ret <- vector()
        for (i in 30:.N) {
            ret[i] <- value[i] / value[i - 29] - 1
        }
        ret
    }, by = .(cube.symbol)]

f.cube.ret.sp[date > as.Date("2016-06-01"),
    ret_365day := {
        ret <- vector()
        for (i in 360:.N) {
            ret[i] <- value[i] / value[i - 359] - 1
        }
        ret
    }, by = .(cube.symbol)]