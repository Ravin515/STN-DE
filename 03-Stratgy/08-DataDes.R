library(styleer)
ld(f.cube.ret.sp)
f.cube.ret.sp[, uniqueN(cube.symbol)]
outlier <- f.cube.ret.sp[order(cube.symbol, date), .SD
    ][, ret := value / shift(value, type = "lag") - 1, by = .(cube.symbol)
    ][ret > 0.1 | ret < -0.1 | label != "", unique(cube.symbol)]
inlier <- f.cube.ret.sp[!(cube.symbol %in% outlier), unique(cube.symbol)]
ld(r.cube.info.mst.1806)
#ld(f.cube.rb.sp)

# 组合的基本情况----
f.cube.info.sp <- r.cube.info.mst.1806[cube.type == "SP", .SD
    ][order(cube.symbol, lastcrawl), .SD
    ][, .SD[.N], by = .(cube.symbol)
    ][cube.symbol %in% inlier, .SD]
cube.info <- f.cube.info.sp[, .(period = fifelse(is.na(close.date), as.Date("2018-07-01") - create.date, close.date - create.date) %>% as.numeric(),
                   fans.count = fans.count,
                   desp.nums = fifelse(description == "暂无投资建议哦", 0, str_count(description)),
                   invest.tag.num = lapply(tag, length) %>% unlist()
), by = .(cube.symbol)]

tag.name <- f.cube.info.sp[, .(invest.tag = unlist(tag))
    ][, .(count = .N), by = .(invest.tag)
    ][order(count), .SD]

lapply(cube.info[period >= 60], summary)

# 调仓数据----
# 调仓次数、调仓间隔、调仓幅度
ld(f.cube.rb.sp)
cube.rb <- f.cube.rb.sp[order(cube.symbol, created.at), .SD
    ][, .(rb.count = .N, rb.intvl = difftime(created.at, shift(created.at, type = "lag"), unit = "day") %>% as.numeric() %>% mean(na.rm = T), rb.ratio = abs(target.weight - prev.weight.adjusted) %>% mean(na.rm = T)), by = .(cube.symbol)
    ][, unique(.SD)]

lapply(cube.rb[!(cube.symbol %in% outlier), unique(.SD)], summary)
lapply(cube.rb[!(cube.symbol %in% outlier), unique(.SD)], sd)
rb.pic <- f.cube.rb.sp[, .(count = .N), by = .(hour.date = str_c(str_pad(hour(created.at), 2, side = "left", pad = 0), str_pad(minute(created.at), 2, side = "left", pad = 0), sep = ":"))
    ][order(hour.date), .SD
    ][hour.date >= "09:30" & hour.date <= "15:00" & !(hour.date %between% c("11:30", "11:59")), .SD
    ][, hour.date := str_c("2013-01-01 ", hour.date) %>% as.POSIXct()]
a <- data.table(hour.date = seq(from = as.POSIXct("2013-01-01 09:30"), to = as.POSIXct("2013-01-01 15:00"), by = "10 min"), tag.10min = 1)
b <- data.table(hour.date = seq(from = as.POSIXct("2013-01-01 09:30"), to = as.POSIXct("2013-01-01 15:00"), by = "1 min"), tag.full = 1)
rb.pic <- a[rb.pic[b, on = .(hour.date)
    ][, count := fifelse(is.na(count), 0, count)], on = .(hour.date)
    ][, tag.10min := fifelse(is.na(tag.10min), 0, tag.10min)
    ][, tag.10min := cumsum(tag.10min)
    ][, count.sum := sum(count), by = .(tag.10min)
    ][tag.10min != 34, .SD[.N], by = .(tag.10min)
    ][, .(hour.date, count.sum)
    ][, hour.date := str_sub(hour.date, start = 1L, end = -4L) %>% as.POSIXct(tryFormats = "%Y-%m-%d %H:%M")]


ggplot(rb.pic) +
    geom_histogram(aes(x = hour.date, y = count.sum), color = "black", size = 0.2, stat = "identity") +
    labs(x = "时刻", y = "交易次数") +
    theme(
                                    axis.line = element_line(linetype = 1),
                                    legend.title = element_blank(),
                                    #panel.border = element_rect(linetype = 1, fill = NA),
                                    legend.position = "bottom",
                                    legend.spacing.x = unit(0.1, 'cm'),
                                    legend.spacing.y = unit(2, 'cm'),
                                    legend.box = "horizontal",
                                    legend.box.background = element_rect(size = 1, colour = "black", fill = "white"),
                                    legend.key = element_rect(size = 0.5, colour = "black", fill = "white"),
                                    legend.key.size = unit(0.5, 'cm')
                                    )
ggsave("3.3.tiff", device = "tiff", dpi = 300, width = 6.67, height = 5)

# 收益率数据----
# 导入指数文件
url <- str_c(getwd(), "/data/Index")
index <- fbread(path = url, pattern = "*\\d.txt")
index <- index[, file_id := NULL
    ][Indexcd == "000300"
    ][, ':='(index_ret = Idxtrd08 / 100, index_value = Idxtrd05, date = as.Date(Idxtrd01))]

ld(f.cube.ret.sp)
cube.ret <- f.cube.ret.sp[!(cube.symbol %in% outlier), .SD
    ][, date.month := str_sub(date, start = 1L, end = 7L)
    ][, .(diff.value = max(value - 1) - min(value - 1)), by = .(date.month)
    ][order(date.month), .SD]
# 总收益
full.ret <- f.cube.ret.sp[!(cube.symbol %in% outlier), .SD
    ][order(cube.symbol, date), .SD
    ][, .SD[as.numeric(date[.N] - date[1]) > 60], by = .(cube.symbol)
    ][, ret.all := (value[.N] - 1) * 100, by = .(cube.symbol)
    ][, unique(.SD), .SDcols = c("cube.symbol", "ret.all")]
summary(full.ret)
sd(full.ret$ret.all)
# 年化收益率
annual.ret <- f.cube.ret.sp[!(cube.symbol %in% outlier), .SD
    ][order(cube.symbol, date), .SD
    ][, ret.all := (value[.N] - 1) * 100, by = .(cube.symbol)
    ][, .SD[as.numeric(date[.N] - date[1]) > 60], by = .(cube.symbol)
    ][, ret.annual := ret.all * 365 / as.numeric(date[.N]-date[1]), by = .(cube.symbol)
    ][, unique(.SD), .SDcols = c("cube.symbol", "ret.annual")]
summary(annual.ret)
sd(annual.ret$ret.annual)

# 超额收益率
abnr.ret <- index[, .(index_value, date)
    ][f.cube.ret.sp, on = .(date)
    ][order(cube.symbol, date), .SD
    ][!(cube.symbol %in% outlier), .SD
    ][, .SD[as.numeric(date[.N] - date[1]) > 60], by = .(cube.symbol)
    ][, abnr.ret.all := (value[.N] - 1) * 100 - (index_value[.N] / index_value[1] - 1) * 100, by = .(cube.symbol)
    ][, abnr.ret.annual := (abnr.ret.all * 365 / as.numeric(date[.N] - date[1])) , by = .(cube.symbol)
    ][, unique(.SD), .SDcols = c("cube.symbol", "abnr.ret.annual", "abnr.ret.all")]
summary(abnr.ret)
lapply(abnr.ret, sd)

# 时间维度的变化
library(dplyr)
abnr.time.week <- index[, .(index_value, date)
    ][f.cube.ret.sp[date < as.Date("2018-07-01") & date >= as.Date("2016-07-01")], on = .(date)
    ][order(cube.symbol, date), .SD
    ][!(cube.symbol %in% outlier), .SD
    ][, .SD[as.numeric(date[.N] - date[1]) > 60], by = .(cube.symbol)
    ][, date.week := str_c(year(date), str_pad(week(date), 2, side = "left", pad = 0))
    ][, abnr.ret.week := (value[.N] / value[1] - 1) * 100 - (index_value[.N] / index_value[1] - 1) * 100, by = .(date.week, cube.symbol)
    ][order(date.week, - abnr.ret.week), .SD
    ][, tag := ntile(abnr.ret.week, 3), by = .(date.week)
    ][, unique(.SD), .SDcols = c("cube.symbol", "abnr.ret.week", "date.week", "tag")
    ][order(date.week, tag), .SD
    ][, abnr.ret.group := mean(abnr.ret.week), by = .(date.week, tag)
    ][, .(diff.abnr.ret = max(abnr.ret.group)- min(abnr.ret.group)), by = .(date.week)
    ][, unique(.SD), .SDcols = c("date.week", "diff.abnr.ret")
    ][index[, .(date.week = str_c(year(date), str_pad(week(date), 2, side = "left", pad = 0)), date = date)
            ][order(date), .SD[.N], by = .(date.week)], on = .(date.week), nomatch = 0
    ]

ggplot(abnr.time.week) +
    geom_histogram(mapping = aes(x = date, y = diff.abnr.ret), color = "black", size = 0.5, stat = "identity") +
    #geom_density(mapping = aes(x = date, y = diff.abnr.ret), color = "black", size = 1) +
    labs(x = "时间", y = "收益率差值（%）") +
    scale_x_date(breaks = "16 week", date_label = "%Y-%m") +
    geom_smooth(mapping = aes(x = date, y = diff.abnr.ret), method = "loess", colour = "blue") +
    theme(
                axis.line = element_line(linetype = 1),
                legend.title = element_blank(),
                #panel.border = element_rect(linetype = 1, fill = NA),
                legend.position = "bottom",
                legend.spacing.x = unit(0.1, 'cm'),
                legend.spacing.y = unit(2, 'cm'),
                legend.box = "horizontal",
                legend.box.background = element_rect(size = 1, colour = "black", fill = "white"),
                legend.key = element_rect(size = 0.5, colour = "black", fill = "white"),
                legend.key.size = unit(0.5, 'cm')
                                    )
ggsave("3.4.tiff", device = "tiff", dpi = 300, width = 6.67, height = 5)

abnr.time.month <- index[, .(index_value, date)
    ][f.cube.ret.sp[date < as.Date("2018-07-01")], on = .(date)
    ][order(cube.symbol, date), .SD
    ][!(cube.symbol %in% outlier), .SD
    ][, .SD[as.numeric(date[.N] - date[1]) > 60], by = .(cube.symbol)
    ][, date.month := str_c(year(date), str_pad(month(date), 2, side = "left", pad = 0))
    ][, abnr.ret.month := (value[.N] / value[1] - 1) * 100 - (index_value[.N] / index_value[1] - 1) * 100, by = .(date.month, cube.symbol)
    ][order(date.month, - abnr.ret.month), .SD
    ][, tag := ntile(abnr.ret.month, 3), by = .(date.month)
    ][, unique(.SD), .SDcols = c("cube.symbol", "abnr.ret.month", "date.month", "tag")
    ][order(date.month, tag), .SD
    ][, abnr.ret.group := mean(abnr.ret.month), by = .(date.month, tag)
    ][, .(diff.abnr.ret = max(abnr.ret.group) - min(abnr.ret.group)), by = .(date.month)
    ][, unique(.SD), .SDcols = c("date.month", "diff.abnr.ret")
    ][index[, .(date.month = str_c(year(date), str_pad(month(date), 2, side = "left", pad = 0)), date = date)
            ][order(date), .SD[.N], by = .(date.month)], on = .(date.month), nomatch = 0
            ]

ggplot(abnr.time.month) +
    geom_histogram(mapping = aes(x = date, y = diff.abnr.ret), color = "black", size = 0.5, stat = "identity") +
    #geom_density(mapping = aes(x = date.month, y = diff.abnr.ret), color = "black", size = 1) +
    labs(x = "时间", y = "收益率差值（%）") +
    scale_x_date(breaks = "4 month", date_label = "%Y-%m") +
    geom_smooth(mapping = aes(x = date, y = diff.abnr.ret), method = "loess", colour = "blue") +
    theme(
                axis.line = element_line(linetype = 1),
                legend.title = element_blank(),
                #panel.border = element_rect(linetype = 1, fill = NA),
                legend.position = "bottom",
                legend.spacing.x = unit(0.1, 'cm'),
                legend.spacing.y = unit(2, 'cm'),
                legend.box = "horizontal",
                legend.box.background = element_rect(size = 1, colour = "black", fill = "white"),
                legend.key = element_rect(size = 0.5, colour = "black", fill = "white"),
                legend.key.size = unit(0.5, 'cm')
                                    )
ggsave("3.5.tiff", device = "tiff", dpi = 300, width = 6.67, height = 5)