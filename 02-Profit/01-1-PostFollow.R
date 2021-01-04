library(styleer)
ld(r.cube.ret.mst.1806)
f.cube.ret.sp <- r.cube.ret.mst.1806[cube.type == "SP"]
rm(r.cube.ret.mst.1806)
f.cube.ret.sp <- f.cube.ret.sp[, unique(.SD), .SDcols = c("value", "date", "cube.symbol")
    ][, .SD[.N], by = .(cube.symbol, date)]
sv(f.cube.ret.sp, svname = "f.cube.ret.sp")

# 1.Using two-sides and full sample1
# Merge with sample1
ld(sample1)
ld(f.cube.ret.sp)
ret.filtered1 <- sample1[date < as.Date("2018-04-01"), .(follow.date, cube.symbol)
    ][, unique(.SD)
    ][f.cube.ret.sp, on = .(cube.symbol), nomatch = 0
    ][, stage := ifelse(follow.date > date, "pre-follow", "post-follow")
    ][, pre.period := difftime(as.Date(follow.date), min(date)), by = cube.symbol
    ][, post.follow := ifelse(stage == "post-follow", 1, 0)]

# 2. draw density plot
dnsty.plot1 <- ret.filtered1[stage == "pre-follow" | (stage == "post-follow" & (date - follow.date < pre.period)), .SD, by = .(cube.symbol)
    ][, .(avrg.ret = mean(value, na.rm = T)), by = .(cube.symbol, stage)
    ][!is.nan(avrg.ret)
    ][, tag := 1
    ][, tag := sum(tag), by = cube.symbol
    ][tag == 2, .(cube.symbol, stage, avrg.ret = avrg.ret - 1)
    ][, post.follow := ifelse(stage == "post-follow", 1, 0)]

dnsty.plot2 <- ret.filtered1[, .(avrg.ret = mean(value, na.rm = T)), by = .(cube.symbol, stage)
    ][!is.nan(avrg.ret)
    ][, tag := 1
    ][, tag := sum(tag), by = cube.symbol
    ][tag == 2, .(cube.symbol, stage, avrg.ret = avrg.ret - 1)
    ][, post.follow := ifelse(stage == "post-follow", 1, 0)]

d.ttest <- ggplot(dnsty.plot1[avrg.ret < 1 & avrg.ret > -1], aes(x = avrg.ret, colour = stage, fill = stage)) +
                            geom_line(stat = "density", size = 0.5) +
                            theme_grey() +
                            scale_colour_manual(values = c("#CC6666", "#7777DD")) +
                            labs(x = "Return", y = "Density") +
                            scale_fill_manual(values = c("#CC6666", "#7777DD"),
                                                        name = "Stage",
                                                        breaks = c("post.follow", "pre.follow"),
                                                        labels = c("Post-follow", "Pre-follow")) +
                                                        theme(
                                    legend.title = element_blank(),
                                    panel.border = element_rect(linetype = 1, fill = NA),
                                    legend.position = "bottom",
                                    legend.spacing.x = unit(0.1, 'cm'),
                                    legend.spacing.y = unit(2, 'cm'),
                                    legend.box = "horizontal",
                                    legend.box.background = element_rect(size = 1, colour = "black", fill = "white"),
                                    legend.key = element_rect(size = 0.5, colour = "black", fill = "white"),
                                    legend.key.size = unit(0.5, 'cm')
                                    )
ggsave("Fig1-1.tiff", device = "tiff", dpi = 300, width = 5.5, height = 4)

d.ttest <- ggplot(dnsty.plot2[avrg.ret < 1 & avrg.ret > -1], aes(x = avrg.ret, colour = stage, fill = stage)) +
                            geom_line(stat = "density", size = 0.5) +
                            theme_grey() +
                            scale_colour_manual(values = c("#CC6666", "#7777DD")) +
                            labs(x = "Return", y = "Density") +
                            scale_fill_manual(values = c("#CC6666", "#7777DD"),
                                                        name = "Stage",
                                                        breaks = c("post.follow", "pre.follow"),
                                                        labels = c("Post-follow", "Pre-follow")) +
                                                        theme(
                                    legend.title = element_blank(),
                                    panel.border = element_rect(linetype = 1, fill = NA),
                                    legend.position = "bottom",
                                    legend.spacing.x = unit(0.1, 'cm'),
                                    legend.spacing.y = unit(2, 'cm'),
                                    legend.box = "horizontal",
                                    legend.box.background = element_rect(size = 1, colour = "black", fill = "white"),
                                    legend.key = element_rect(size = 0.5, colour = "black", fill = "white"),
                                    legend.key.size = unit(0.5, 'cm')
                                    )
ggsave("Fig1-2.tiff", device = "tiff", dpi = 300, width = 5.5, height = 4)

# 3. T-test
dnsty.plot1[avrg.ret < 1 & avrg.ret > -1, t.test(avrg.ret ~ post.follow)]
dnsty.plot2[avrg.ret < 1 & avrg.ret > -1, t.test(avrg.ret ~ post.follow)]

# 4. UMD factor is merged to main
mmt <- fread("C:/Users/MrStylee/source/repos/STN-DE/01-LnAcpt/Momentum.csv")
mmt <- mmt[, setnames(.SD, 1:2, c("date", "mmt"))
    ][, date := str_replace_all(date, "/", "-") %>% as.Date("%Y-%m-%d")]

# 5. Trade number is merged to main
ld(f.cube.rb.mst.1803)
cube.rb.sp <- f.cube.rb.mst.1803[cube.type == "SP", .(cube.symbol, created.at, date = as.Date(created.at), tag = 1)
    ][, trd.num := sum(tag), by = .(cube.symbol, date)
    ][order(cube.symbol, date), .SD
    ][, trd.num := cumsum(trd.num), by = .(cube.symbol)
    ][, .SD[.N], by = .(cube.symbol, date)
    ][, ':='(created.at = NULL, tag = NULL)]
rm(f.cube.rb.mst.1803)


# 7. stock list generating
ld(f.cube.rb.mst.1803)

# 7.1 processing every stock symbol first trading per cube
cube.rb.first <- f.cube.rb.mst.1803[cube.type == "SP", .(cube.symbol, target.weight, prev.weight.adjusted, stock.symbol, created.at)
    ][order(cube.symbol, stock.symbol, created.at), .SD
    ][, .SD[1], by = .(cube.symbol, stock.symbol)]
cube.rb.first[, buy := ifelse(prev.weight.adjusted == 0 & target.weight != 0, 1, 0)
    ][, min.trddy := min(created.at), by = .(cube.symbol)]

cube.rb.first.first <- cube.rb.first[buy == 1
    ][, date := as.Date(created.at)
    ][, ':='(min.trddy = NULL, buy = NULL)]

cube.rb.first.second <- cube.rb.first[buy == 0
    ][, date := as.Date(created.at)
    ][, ':='(adjusted.date = as.Date(min.trddy) - 1, adjusted.stock.symbol = stock.symbol, adjusted.buy = 1)]

# generate a initial stock list
cube.first.stock.list <- cube.rb.first.second[, .(first.stock.list = list(adjusted.stock.symbol)), by = .(cube.symbol, adjusted.date)]

# 7.2 processing every stock symbol non-first trading per cube
cube.rb.next <- f.cube.rb.mst.1803[cube.type == "SP", .(cube.symbol, target.weight, prev.weight.adjusted, stock.symbol, created.at)
    ][order(cube.symbol, stock.symbol, created.at), .SD
    ][, .SD[-1], by = .(cube.symbol, stock.symbol)]
# select those trading can't effect stock list
cube.rb.next <- cube.rb.next[target.weight == 0 | prev.weight.adjusted == 0, date := as.Date(created.at)]

# binding cube.first.first and cube.rb.next
cube.rb <- rbindlist(list(cube.rb.next, cube.rb.first.first))
cube.rb[, ':='(buy = ifelse(target.weight != 0, 1, 0), sell = ifelse(target.weight == 0, 1, 0))]
sadd <- function(x) {
    if (length(x) == 1) {
        x
    } else {
        as.list(Reduce(c, x, accumulate = T))
    }
}
cube.rb.buy <- cube.rb[buy == 1, .(stock.list = list(stock.symbol)), keyby = .(cube.symbol, date)
    ][, .(stock.list.buy = sadd(stock.list), date), by = .(cube.symbol)]
cube.rb.sell <- cube.rb[sell == 1, .(stock.list = list(stock.symbol)), keyby = .(cube.symbol, date)
    ][, .(stock.list.sell = sadd(stock.list), date), by = .(cube.symbol)]

# combining three sets to calculate holding stock list
stock.info1 <- cube.first.stock.list[cube.rb.sell[cube.rb.buy[ret.filtered1, on = .(cube.symbol, date), roll = T], on = .(cube.symbol, date), roll = T], on = .(cube.symbol)]
stock.info1[, stock.list := {
    l <- list()
    for (i in 1:.N) {
        l[[i]] <- setdiff(union(first.stock.list[[i]], stock.list.buy[[i]]), stock.list.sell[[i]]) #%>% list()
    }
    l
}]

f.main1 <- stock.info1[, .(cube.symbol, date, adjusted.date, follow.date, value, stage, pre.period, post.follow, stock.list)]
stock.info1 <- stock.info1[, .(cube.symbol, date, adjusted.date, first.stock.list, stock.list.sell, stock.list.buy, stock.list)]
sv(stock.info1, svname = "stock.info1")

# Active day is generated
f.main1[, active.day := 1:.N, by = .(cube.symbol)]
f.main1 <- cube.rb.sp[f.main1, on = .(cube.symbol, date), roll = T]
f.main1 <- mmt[f.main1, on = .(date)]


ld(f.cubelife.mst.1803)
# Adding stock number per cube daily
f.main1[, stock.num := lapply(stock.list, length) %>% unlist()
    ][, follow.date := as.Date(follow.date)
    ][, trd.num := ifelse(is.na(trd.num), 0, trd.num)]
# add close date per cube.symbol
f.main1 <- f.cubelife.mst.1803[, .(cube.symbol, end)
    ][f.main1, on = .(cube.symbol)
    ][date < end | date == end, .SD
    ][pre.period > 0, .SD]
rm(f.cubelife.mst.1803)
# converting value to ret
f.main1[, ret := value / shift(value, type = "lag") - 1, by = .(cube.symbol)
    ][is.infinite(ret), ret := 0
    ][, trdweek := str_c(year(date), week(date))
    ][, trdmonth := str_c(year(date), month(date))]

sv(f.main1, svname = "f.main1")
rm(list = ls())

## select week return and month return
#f.main1.week <- f.main1[, .SD[.N], by = .(cube.symbol, trdweek)
    #][, ret_week := value / shift(value, type = "lag") - 1, by = .(cube.symbol)
    #][is.infinite(ret_week), ret_week := 0]

#f.main1.month <- f.main1[, .SD[.N], by = .(cube.symbol, trdmonth)
    #][, ret_month := value / shift(value, type = "lag") - 1, by = .(cube.symbol)
    #][is.infinite(ret_month), ret_month := 0]