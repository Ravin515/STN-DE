
# 对平台整体的ret进行描述性统计
library(styleer)
library(lfe)
library(quantreg)
library(rqpd)

# 导入指数文件
# 日度指数文件
url <- str_c(getwd(), "/data/Index")
index <- fbread(path = url, pattern = "*\\d.txt")
index <- index[, file_id := NULL
    ][Indexcd == "000300"
    ][, ':='(index_ret = Idxtrd08 / 100, index_value = Idxtrd05, date = as.Date(Idxtrd01))]

# 月度指数文件
indexmn <- fread(str_c(url, "/IDX_Idxtrdmth.txt"), encoding = "UTF-8")
indexmn <- indexmn[Indexcd == "000300"]

# f.main1为bothside样本
# f.main2为follow样本
# f.nonfollow为non-follow样本
# f.cube.ret.sp为全样本
ld(f.main1)
ld(f.main2)
ld(f.cube.ret.sp)

follow.sample <- f.main2[, .(cube.symbol = unique(cube.symbol), tag = "followsample")]
f.nonfollow <- follow.sample[f.cube.ret.sp, on = "cube.symbol"
    ][is.na(tag), .(cube.symbol, date, value)]

f.draw <- index[, .(date, index_ret, index_value)
    ][f.main1[!(cube.symbol %in% outlier), .SD], on = .(date)]


f.draw[, ':='(tag1 = fifelse( date == as.Date("2016-07-01") | date == as.Date("2017-01-03"), 1L, 0),
                   tag2 = fifelse( date == as.Date("2017-01-03") | date == as.Date("2017-06-30"), 1L, 0),
                   tag3 = fifelse( date == as.Date("2017-06-30") | date == as.Date("2018-01-04"), 1L, 0)
    )][, stage := fifelse(sum(tag1) == 2, "stage1", fifelse(sum(tag2) == 2, "stage2", fifelse(sum(tag3) == 2, "stage3", "no"))), by = .(cube.symbol)]
stage1 <- f.draw[(date == as.Date("2016-07-01") | date == as.Date("2017-01-03")) & stage == "stage1", .SD
    ][, ':='(ret = value / shift(value, type = "lag") - 1, index_ret = index_value / shift(index_value, type = "lag") - 1), by = .(cube.symbol)
    ][, abnor_ret := ret - index_ret
    ][!is.na(abnor_ret), .(cube.symbol, abnor_ret, stage)]
stage2 <- f.draw[(date == as.Date("2017-01-03") | date == as.Date("2017-06-30")) & stage == "stage2", .SD
    ][, ':='(ret = value / shift(value, type = "lag") - 1, index_ret = index_value / shift(index_value, type = "lag") - 1), by = .(cube.symbol)
    ][, abnor_ret := ret - index_ret
    ][!is.na(abnor_ret), .(cube.symbol, abnor_ret, stage)]
stage3 <- f.draw[(date == as.Date("2017-06-30") | date == as.Date("2018-01-04")) & stage == "stage3", .SD
    ][, ':='(ret = value / shift(value, type = "lag") - 1, index_ret = index_value / shift(index_value, type = "lag") - 1), by = .(cube.symbol)
    ][, abnor_ret := ret - index_ret
    ][!is.na(abnor_ret), .(cube.symbol, abnor_ret, stage)]

draw <- rbindlist(list(stage1, stage2, stage3), fill = T)
ggplot() +
    geom_line(draw[stage == "stage1" & abnor_ret > -1 & abnor_ret < 1], mapping = aes(x = abnor_ret), stat = "density", color = "blue", size = 1) +
    geom_line(draw[stage == "stage2" & abnor_ret > -1 & abnor_ret < 1], mapping = aes(x = abnor_ret), stat = "density", color = "red", size = 1) +
    geom_line(draw[stage == "stage3" & abnor_ret > -1 & abnor_ret < 1], mapping = aes(x = abnor_ret), stat = "density", color = "green", size = 1)
    #scale_x_continuous(breaks = c(-1, 0:10))

### 画出平台现值的时间轴的图
#ggplot() +
    #geom_line(f.main1[date == as.Date("2017-01-03") & (value > -1 & value < 4)], mapping = aes(x = value), stat = "density", color = "black") +
    #geom_line(f.main1[date == as.Date("2017-03-03") & (value > -1 & value < 4)], mapping = aes(x = value), stat = "density", color = "red") +
    #geom_line(f.main1[date == as.Date("2017-06-01") & (value > -1 & value < 4)], mapping = aes(x = value), stat = "density", color = "blue") +
    #geom_line(f.main1[date == as.Date("2017-09-01") & (value > -1 & value < 4)], mapping = aes(x = value), stat = "density", color = "yellow") +
    #geom_line(f.main1[date == as.Date("2017-12-01") & (value > -1 & value < 4)], mapping = aes(x = value), stat = "density", color = "grey") +
    #geom_line(f.main1[date == as.Date("2018-03-01") & (value > -1 & value < 4)], mapping = aes(x = value), stat = "density", color = "green") +
    #scale_x_continuous(breaks = c(-1, 0:10))

# 收益率的图
### cube在follow 前后的对比

#ggplot() +
    #geom_line(f.main1[date - follow.date == -150, .SD, by = .(cube.symbol)][ret > -0.125 & ret < 0.125], mapping = aes(x = ret), stat = "density", color = "blue") +
    #geom_line(f.main1[date - follow.date == 150, .SD, by = .(cube.symbol)][ret > -0.125 & ret < 0.125], mapping = aes(x = ret), stat = "density", color = "red") +
    #scale_x_continuous(breaks = c(-0.5, -0.25, 0, 0.25, 0.5))

#ggplot() +
    #geom_line(f.main1[date - follow.date == -120, .SD, by = .(cube.symbol)][ret > -0.125 & ret < 0.125], mapping = aes(x = ret), stat = "density", color = "blue") +
    #geom_line(f.main1[date - follow.date == 120, .SD, by = .(cube.symbol)][ret > -0.125 & ret < 0.125], mapping = aes(x = ret), stat = "density", color = "red") +
    #scale_x_continuous(breaks = c(-0.5, -0.25, 0, 0.25, 0.5))

#ggplot() +
    #geom_line(f.main1[date - follow.date == -90, .SD, by = .(cube.symbol)][ret > -0.125 & ret < 0.125], mapping = aes(x = ret), stat = "density", color = "blue") +
    #geom_line(f.main1[date - follow.date == 90, .SD, by = .(cube.symbol)][ret > -0.125 & ret < 0.125], mapping = aes(x = ret), stat = "density", color = "red") +
    #scale_x_continuous(breaks = c(-0.5, -0.25, 0, 0.25, 0.5))

#ggplot() +
    #geom_line(f.main1[date - follow.date == -60, .SD, by = .(cube.symbol)][ret > -0.125 & ret < 0.125], mapping = aes(x = ret), stat = "density", color = "blue") +
    #geom_line(f.main1[date - follow.date == 60, .SD, by = .(cube.symbol)][ret > -0.125 & ret < 0.125], mapping = aes(x = ret), stat = "density", color = "red") +
    #scale_x_continuous(breaks = c(-0.5, -0.25, 0, 0.25, 0.5))

#ggplot() +
    #geom_line(f.main1[date - follow.date == -30, .SD, by = .(cube.symbol)][ret > -0.125 & ret < 0.125], mapping = aes(x = ret), stat = "density", color = "blue") +
    #geom_line(f.main1[date - follow.date == 30, .SD, by = .(cube.symbol)][ret > -0.125 & ret < 0.125], mapping = aes(x = ret), stat = "density", color = "red") +
    #scale_x_continuous(breaks = c(-0.5, -0.25, 0, 0.25, 0.5))

#ggplot() +
    #geom_line(f.main1[date - follow.date == -5, .SD, by = .(cube.symbol)][ret > -0.125 & ret < 0.125], mapping = aes(x = ret), stat = "density", color = "blue") +
    #geom_line(f.main1[date - follow.date == 5, .SD, by = .(cube.symbol)][ret > -0.125 & ret < 0.125], mapping = aes(x = ret), stat = "density", color = "red") +
    #scale_x_continuous(breaks = c(-0.5, -0.25, 0, 0.25, 0.5))

## 实盘整体情况
#ggplot() +
    #geom_line(f.main1[date == as.Date("2017-01-03") & (ret > -0.125 & ret < 0.125)], mapping = aes(x = ret - index_ret), stat = "density", color = "black") +
    #geom_line(f.main1[date == as.Date("2017-03-03") & (ret > -0.125 & ret < 0.125)], mapping = aes(x = ret - index_ret), stat = "density", color = "red") +
    #geom_line(f.main1[date == as.Date("2017-06-01") & (ret > -0.125 & ret < 0.125)], mapping = aes(x = ret - index_ret), stat = "density", color = "blue") +
    #geom_line(f.main1[date == as.Date("2017-09-01") & (ret > -0.125 & ret < 0.125)], mapping = aes(x = ret - index_ret), stat = "density", color = "yellow") +
    #geom_line(f.main1[date == as.Date("2017-12-01") & (ret > -0.125 & ret < 0.125)], mapping = aes(x = ret - index_ret), stat = "density", color = "grey") +
    #geom_line(f.main1[date == as.Date("2018-03-01") & (ret > -0.125 & ret < 0.125)], mapping = aes(x = ret - index_ret), stat = "density", color = "green") +
    #scale_x_continuous(breaks = c(-0.5, -0.25, 0, 0.25, 0.5))

## 与沪深三百的比较
### 平台每天的平均收益率与市场收益率比较
#f.main1.day <- f.main1
#did.index.daily <- f.main1.day[date > as.Date("2016-07-01"), .(aver_ret = quantile(ret, probs = 0.5, na.rm = T), index_ret = index_ret[1]), by = .(date)]
#ggplot() +
    #geom_line(did.index.daily, mapping = aes(x = date, y = index_ret, group = 1), color = "blue", size = 1) +
    #geom_line(did.index.daily, mapping = aes(x = date, y = aver_ret, group = 2), color = "red", size = 1)

#### 算出平台每周的平均收益率与市场收益率比较
#indexwk <- index[, trdweek := str_c(year(date), week(date), sep = "-")
    #][, .SD[.N], by = .(trdweek)
    #][, index_ret_week := index_value / shift(index_value, type = "lag") - 1
    #][, .(trdweek, index_ret_week)]
#f.main1.week <- f.main1[, .SD[.N], by = .(cube.symbol, trdweek)
    #][, trdweek := str_c(year(date), week(date), sep = "-")
    #][, .(ret_week = value / shift(value, type = "lag") - 1, trdweek), by = .(cube.symbol)
    #][is.infinite(ret_week), ret_week := 0 
    #][, .(aver_ret_week = median(ret_week, na.rm = T)), by = .(trdweek)
    #]

#did.index.weekly <- indexwk[f.main1.week, on = .(trdweek)
    #][, ':='(index_ret_week = index_ret_week, aver_ret_week = aver_ret_week)
    #][!is.na(aver_ret_week)]

#ggplot(did.index.weekly[trdweek != "2016-26"]) +
    #geom_line(mapping = aes(x = trdweek, y = index_ret_week, group = 1), color = "blue", size = 1) +
    #geom_line(mapping = aes(x = trdweek, y = aver_ret_week, group = 2), color = "red", size = 1)

#### 算出平台每月的平均收益率与市场收益率比较
#indexmn[, ':='(trdmonth = Month, index_ret_month = Idxrtn)]
#f.main1.month <- f.main1[, .SD[.N], by = .(cube.symbol, trdmonth)
    #][, trdmonth := as.character(date) %>% str_sub(start = 1L, end = 7L)
    #][, .(ret_month= value / shift(value, type = "lag") - 1, trdmonth), by = .(cube.symbol)
    #][is.infinite(ret_month), ret_month := 0
    #][, .(aver_ret_month = median(ret_month, na.rm = T)), by = .(trdmonth)
    #]

#did.index.monthly <- indexmn[, .(trdmonth, index_ret_month)
    #][f.main1.month, on = .(trdmonth)
    #][, ':='(index_ret_month = index_ret_month, aver_ret_month = aver_ret_month)
    #][!is.na(aver_ret_month)]

#ggplot(did.index.monthly[trdmonth != "2016-06"]) +
    #geom_line(mapping = aes(x = trdmonth, y = index_ret_month, group = 1), color = "blue", size = 1) +
    #geom_line(mapping = aes(x = trdmonth, y = aver_ret_month, group = 2), color = "red", size = 1) +
    #theme(
        #axis.text.x = element_text(vjust = 0.5),
        #axis.title.x = element_blank(),
##axis.title.x = element_text(vjust = 0.5),
          #)
    
## 均值DID, Drawing plots
#did.daily <- f.main1.day[post.follow == 0 | (post.follow == 1 & (date - follow.date < pre.period)), .(aver.ret = mean(ret - index_ret, na.rm = T)), by = .(cube.symbol, post.follow)
    #][, tag := 1
    #][, tag := sum(tag), by = .(cube.symbol)
    #][tag == 2
    #][, stage := ifelse(post.follow == 0, "pre-follow", "post-follow")]
#did.daily[, t.test(aver.ret ~ post.follow)]

#did.weekly <- f.main1.week[post.follow == 0 | (post.follow == 1 & (date - follow.date < pre.period)), .(aver.ret = mean(ret_week - index_ret_week, na.rm = T)), by = .(cube.symbol, post.follow)
    #][, tag := 1
    #][, tag := sum(tag), by = .(cube.symbol)
    #][tag == 2
    #][, stage := ifelse(post.follow == 0, "pre-follow", "post-follow")]
#did.weekly[, t.test(aver.ret ~ post.follow)]

#did.monthly <- f.main1.month[post.follow == 0 | (post.follow == 1 & (date - follow.date < pre.period)), .(aver.ret = mean(ret_month - index_ret_month, na.rm = T)), by = .(cube.symbol, post.follow)
    #][, tag := 1
    #][, tag := sum(tag), by = .(cube.symbol)
    #][tag == 2
    #][, stage := ifelse(post.follow == 0, "pre-follow", "post-follow")]
#did.monthly[, t.test(aver.ret ~ post.follow)]

#d.ttest <- ggplot(did.daily[aver.ret < 0.05 & aver.ret > -0.05], aes(x = aver.ret, colour = stage, fill = stage)) +
                            #geom_line(stat = "density", size = 0.5) +
                            #theme_grey() +
                            #scale_colour_manual(values = c("#CC6666", "#7777DD")) +
                            #labs(x = "Return", y = "Density") +
                            #scale_fill_manual(values = c("#CC6666", "#7777DD"),
                                                        #name = "Stage",
                                                        #breaks = c("post.follow", "pre.follow"),
                                                        #labels = c("Post-follow", "Pre-follow")) +
                                                        #theme(
                                    #legend.title = element_blank(),
                                    #panel.border = element_rect(linetype = 1, fill = NA),
                                    #legend.position = "bottom",
                                    #legend.spacing.x = unit(0.1, 'cm'),
                                    #legend.spacing.y = unit(2, 'cm'),
                                    #legend.box = "horizontal",
                                    #legend.box.background = element_rect(size = 1, colour = "black", fill = "white"),
                                    #legend.key = element_rect(size = 0.5, colour = "black", fill = "white"),
                                    #legend.key.size = unit(0.5, 'cm')
                                    #)
#ggsave("Fig1-1.tiff", device = "tiff", dpi = 300, width = 5.5, height = 4)

#d.ttest <- ggplot(did.weekly[aver.ret < 0.2 & aver.ret > -0.2], aes(x = aver.ret, colour = stage, fill = stage)) +
                            #geom_line(stat = "density", size = 0.5) +
                            #theme_grey() +
                            #scale_colour_manual(values = c("#CC6666", "#7777DD")) +
                            #labs(x = "Return", y = "Density") +
                            #scale_fill_manual(values = c("#CC6666", "#7777DD"),
                                                        #name = "Stage",
                                                        #breaks = c("post.follow", "pre.follow"),
                                                        #labels = c("Post-follow", "Pre-follow")) +
                                                        #theme(
                                    #legend.title = element_blank(),
                                    #panel.border = element_rect(linetype = 1, fill = NA),
                                    #legend.position = "bottom",
                                    #legend.spacing.x = unit(0.1, 'cm'),
                                    #legend.spacing.y = unit(2, 'cm'),
                                    #legend.box = "horizontal",
                                    #legend.box.background = element_rect(size = 1, colour = "black", fill = "white"),
                                    #legend.key = element_rect(size = 0.5, colour = "black", fill = "white"),
                                    #legend.key.size = unit(0.5, 'cm')
                                    #)
#ggsave("Fig1-2.tiff", device = "tiff", dpi = 300, width = 5.5, height = 4)

#d.ttest <- ggplot(did.monthly[aver.ret < 0.5 & aver.ret > -0.5], aes(x = aver.ret, colour = stage, fill = stage)) +
                            #geom_line(stat = "density", size = 0.5) +
                            #theme_grey() +
                            #scale_colour_manual(values = c("#CC6666", "#7777DD")) +
                            #labs(x = "Return", y = "Density") +
                            #scale_fill_manual(values = c("#CC6666", "#7777DD"),
                                                        #name = "Stage",
                                                        #breaks = c("post.follow", "pre.follow"),
                                                        #labels = c("Post-follow", "Pre-follow")) +
                                                        #theme(
                                    #legend.title = element_blank(),
                                    #panel.border = element_rect(linetype = 1, fill = NA),
                                    #legend.position = "bottom",
                                    #legend.spacing.x = unit(0.1, 'cm'),
                                    #legend.spacing.y = unit(2, 'cm'),
                                    #legend.box = "horizontal",
                                    #legend.box.background = element_rect(size = 1, colour = "black", fill = "white"),
                                    #legend.key = element_rect(size = 0.5, colour = "black", fill = "white"),
                                    #legend.key.size = unit(0.5, 'cm')
                                    #)
#ggsave("Fig1-3.tiff", device = "tiff", dpi = 300, width = 5.5, height = 4)

