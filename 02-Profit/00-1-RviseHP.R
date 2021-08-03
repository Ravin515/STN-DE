# 利用1806的数据，重新计算持仓价格
library(styleer)
ld(f.cube.rb.sp.mst.1806)

f.hold.price.1806 <- f.cube.rb.sp.mst.1806[order(cube.symbol, stock.symbol, created.at, id, - target.weight), unique(.SD), .SDcols = c("cube.symbol", "stock.symbol", "created.at", "price", 'id', "target.weight", "prev.weight.adjusted")
    ][!(prev.weight.adjusted == 0 & target.weight == 0), .SD
    #][, {
        ##对那些重复的记录进行去重
        #a <- vector()
        #a[1] <- 0
        #for (i in 2:.N) {
            #if (abs(target.weight[i] - target.weight[i - 1]) <= 0.05) {
                #a[i] <- i
            #}
            #else {
                #a[i] <- 0
            #}
        #}
        #.SD[-(a[a != 0])]
    #}
    ][, tag := fifelse(mean(is.na(id)) > 0 & .N > 1, 1, 0), by = .(cube.symbol, stock.symbol, created.at) # 对那些created.at有重复且id存在NA的记录进行处理
    ][, tag2 := fifelse(mean(tag == 0) < 1, 1, 0), by = .(cube.symbol, stock.symbol)] # 标记出那些有tag为1的cube.symbol和stock.symbol

# 抽取tag == 1进行清洗
f.locate1 <- f.hold.price.1806[tag == 1, .SD
    ][order(cube.symbol, stock.symbol, created.at, - target.weight), .SD
    ][, value := sum(prev.weight.adjusted - shift(target.weight, type = "lag"), na.rm = T) %>% abs(), by = .(cube.symbol, stock.symbol, created.at)
    ][, tag.locate := 1:.N, by = .(cube.symbol, stock.symbol, created.at)]

f.locate2 <- f.hold.price.1806[tag == 1, .SD
    ][order(cube.symbol, stock.symbol, created.at, target.weight), .SD
    ][, value := sum(prev.weight.adjusted - shift(target.weight, type = "lag"), na.rm = T) %>% abs(), by = .(cube.symbol, stock.symbol, created.at)
    ][, tag.locate := 1:.N, by = .(cube.symbol, stock.symbol, created.at)]

f.locate <- rbindlist(list(f.locate1, f.locate2))
f.locate <- f.locate[order(cube.symbol, stock.symbol, created.at, value, tag.locate), .SD
    ][, .SD[value == min(value)], by = .(cube.symbol, stock.symbol, created.at)
    ][, unique(.SD), by = .(cube.symbol, stock.symbol, created.at)
    ][, id.num := sum(!is.na(id)), by = .(cube.symbol, stock.symbol, created.at)]

# 抽取tag == 1 且 每个时间戳上id数为0、1和2的进行清洗
f.id2 <- f.locate[id.num == 2, .SD
    ][!is.na(id), .SD
    ][, id.num := sum(!is.na(id)), by = .(cube.symbol, stock.symbol, created.at)
    ][, value := sum(prev.weight.adjusted - shift(target.weight, type = "lag"), na.rm = T) %>% abs(), by = .(cube.symbol, stock.symbol, created.at)
    ][, .SD[value < 1 | (value > 1 & tag.locate == min(tag.locate))], by = .(cube.symbol, stock.symbol, created.at)
    ][, tag.locate := 1:.N, by = .(cube.symbol, stock.symbol, created.at)
    ][, {
        weight <- CJ(target.weight, prev.weight.adjusted, price)
        weight[abs(target.weight - prev.weight.adjusted) == max(abs(target.weight - prev.weight.adjusted)), unique(.SD)]
    }, by = .(cube.symbol, stock.symbol, created.at)
    ][, tag.locate := 1:.N, by = .(cube.symbol, stock.symbol, created.at)]

f.id1 <- f.locate[id.num == 1, .SD
    ][, {
        weight <- CJ(target.weight, prev.weight.adjusted, price)
        weight[abs(target.weight - prev.weight.adjusted) == max(abs(target.weight - prev.weight.adjusted)), unique(.SD)]
    }, by = .(cube.symbol, stock.symbol, created.at)
    ][, tag.locate := 1:.N, by = .(cube.symbol, stock.symbol, created.at)]

f.id0 <- f.locate[id.num == 0, .SD
    ][, {
    weight <- CJ(target.weight, prev.weight.adjusted, price)
    weight[abs(target.weight - prev.weight.adjusted) == max(abs(target.weight - prev.weight.adjusted)), unique(.SD)]
    }, by = .(cube.symbol, stock.symbol, created.at)
    ][, tag.locate := 1:.N, by = .(cube.symbol, stock.symbol, created.at)]

f.id <- rbindlist(list(f.id0, f.id1, f.id2), use.names = T)

# 与tag == 0的f.hold.price数据进行合并
f.hold.price.1806 <- f.hold.price.1806[tag == 0, .SD, .SDcols = -c("id", "tag", "tag2")
    ][, tag.locate := 1:.N, by = .(cube.symbol, stock.symbol, created.at)]
f.hold.price.1806 <- rbindlist(list(f.id, f.hold.price.1806), use.names = T)
f.hold.price.1806 <- f.hold.price.1806[order(cube.symbol, stock.symbol, created.at, tag.locate)
    ][, .SD[.N], by = .(cube.symbol, stock.symbol, created.at)
    ][!(is.nan(target.weight) | is.infinite(prev.weight.adjusted)), .SD]

# 删除每个cube每只股票第一笔为卖出的交易
f.hold.price.1806 <- f.hold.price.1806[, issale := fifelse(target.weight < prev.weight.adjusted, 1, 0)
    ][, diff := c(0, diff(issale)), keyby = .(cube.symbol, stock.symbol)
    ][, cumsum := cumsum(abs(diff)), keyby = .(cube.symbol, stock.symbol)
    ][issale == 1 & cumsum == 0, no := 1
    ][is.na(no), .(stock.symbol, price, target.weight, prev.weight.adjusted, cube.symbol, created.at, issale)
    ][!is.na(target.weight)]

# 清除那些prev.weight.adjusted为负的记录
f.hold.price.1806 <- f.hold.price.1806[!(target.weight < 0 | prev.weight.adjusted < 0 | price < 0), .SD]

f.hold.price.1806[, hold.price := {
    # 计算持仓价格
    pwa <- prev.weight.adjusted;
    price <- price;
    tw <- target.weight;

    make_hp <- function(n, price, pwa, tw) {
        if (tw != 0) {
            hp[n] <- (hp[n - 1] * pwa + (tw - pwa) * (price)) / tw
        }
        else {
            hp[n] <- 0
        }
        hp
    };

    hp <- price[1];
    if (length(price) == 1) {
        hp <- price
    }
    else {
        for (i in 2:length(price)) {
            hp <- make_hp(i, price[i], pwa[i], tw[i])
        };
    }
    list(hp)
}
    , keyby = .(cube.symbol, stock.symbol)
    ]
sv(f.hold.price.1806, svname = "f.hold.price.1806")

rm(list = ls())


## 加入1806的数据
#ld(r.cube.rb.mst.1806)
#f.cube.rb.sp.1806 <- r.cube.rb.mst.1806[cube.type == "SP" & lastcrawl == 1806 & created.at > as.POSIXct("2018-03-27 15:00:00")]
#f.cube.rb.sp.mst.1806 <- rbindlist(list(f.cube.rb.sp.1806, f.cube.rb.sp), use.names = T)
#sv(f.cube.rb.sp.mst.1806, svname = "f.cube.rb.sp.mst.1806")

#f.hold.price.1806 <- f.cube.rb.sp.1806[order(cube.symbol, stock.symbol, created.at, id, - target.weight), unique(.SD), .SDcols = c("cube.symbol", "stock.symbol", "created.at", "price", 'id', "target.weight", "prev.weight.adjusted")
    #][!(prev.weight.adjusted == 0 & target.weight == 0), .SD
    ##][, {
    ###对那些重复的记录进行去重
    ##a <- vector()
    ##a[1] <- 0
    ##for (i in 2:.N) {
    ##if (abs(target.weight[i] - target.weight[i - 1]) <= 0.05) {
    ##a[i] <- i
    ##}
    ##else {
    ##a[i] <- 0
    ##}
    ##}
    ##.SD[-(a[a != 0])]
    ##}
    #][, tag := fifelse(.N > 1, 1, 0), by = .(cube.symbol, stock.symbol, created.at) # 对那些created.at有重复的记录进行处理
    #]
#f.id <- f.hold.price.1806[tag == 1, .SD
    #][, .(target.weight = target.weight[.N], prev.weight.adjusted = prev.weight.adjusted[1], price = price[.N]), by = .(cube.symbol, stock.symbol, created.at)]
#f.hold.price.1806 <- rbindlist(list(f.hold.price.1806[tag == 0, .SD, .SDcols = -c('id', 'tag')], f.id), use.names = T)

#ld(f.hold.price)
#f.hold.price[, ':='(hold.price = NULL, tag.locate = NULL)]
#f.hold.price <- rbindlist(list(f.hold.price, f.hold.price.1806), use.names = T)
## 去除第一笔交易为卖出的记录
#f.hold.price <- f.hold.price[order(cube.symbol, stock.symbol, created.at), .SD
    #][, issale := fifelse(target.weight < prev.weight.adjusted, 1, 0)
    #][, diff := c(0, diff(issale)), keyby = .(cube.symbol, stock.symbol)
    #][, cumsum := cumsum(abs(diff)), keyby = .(cube.symbol, stock.symbol)
    #][issale == 1 & cumsum == 0, no := 1
    #][is.na(no), .SD
    #][!is.na(target.weight), .SD]

#f.hold.price <- f.hold.price[, .SD[, - c(8:11)]]

#f.hold.price[, hold.price := {
    ## 计算持仓价格
    #pwa <- prev.weight.adjusted;
    #price <- price;
    #tw <- target.weight;

    #make_hp <- function(n, price, pwa, tw) {
        #if (tw != 0) {
            #hp[n] <- (hp[n - 1] * pwa + (tw - pwa) * (price)) / tw
        #}
        #else {
            #hp[n] <- 0
        #}
        #hp
    #};

    #hp <- price[1];
    #if (length(price) == 1) {
        #hp <- price
    #}
    #else {
        #for (i in 2:length(price)) {
            #hp <- make_hp(i, price[i], pwa[i], tw[i])
        #};
    #}
    #list(hp)
#}
    #, keyby = .(cube.symbol, stock.symbol)
    #]
#sv(f.hold.price, svname = "f.hold.price")

# 重新看待生存分析图----
library(styleer)
ld(f.hold.price.1806, force = T)
ld(f.main2, force = T)
ld(f.main1, force = T)
f.hold.price.1806 <- f.main2[, .(close.date = max(date)), by = .(cube.symbol)
    ][f.hold.price.1806, on = .(cube.symbol)
    ][!is.na(close.date), .SD
    ][, stock.symbol := str_sub(stock.symbol, start = 3L, end = 8L)
    ][, date := as.Date(created.at)
    ][order(cube.symbol, stock.symbol, created.at), .SD
    ][, .SD[.N], by = .(cube.symbol, stock.symbol, date)
    ][, end.date := fifelse(target.weight[.N] == 0, date[.N], unique(close.date)), by = .(cube.symbol, stock.symbol)]

date.cj <- f.hold.price.1806[, .(date = seq(date[1], unique(end.date), by = "day")), by = .(cube.symbol, stock.symbol)]
f.position <- f.hold.price.1806[, .(cube.symbol, stock.symbol, date, created.at, prev.weight.adjusted, target.weight, price)
    ][date.cj, on = .(cube.symbol, stock.symbol, date)]
f.position <- f.hold.price.1806[, .(cube.symbol, stock.symbol, date, hold.price)
    ][f.position, on = .(cube.symbol, stock.symbol, date), roll = T]

clsprc <- fbread(path = str_c(getwd(), "/data/Clprc"), pattern = "*.txt")

f.surv <- clsprc[, .(stock.symbol = str_pad(Stkcd, 6, "left", pad = "0"), Clsprc, date = Trddt)
    ][f.position, on = .(stock.symbol, date), nomatch = 0]
f.surv.clean <- f.surv[, tag := fifelse(hold.price == 0, 1, 0)
    ][, tag2 := fifelse(tag == 1 & shift(tag, type = "lag") == 0, 1, 0), by = .(cube.symbol, stock.symbol)
    ][!(tag == 1 & tag2 == 0), .SD
    ][, ":="(tag = NULL, tag2 = NULL)
    ][, issale := fifelse(hold.price == 0, 1, 0)
    ][cube.symbol %in% f.main1$cube.symbol, .SD]

f.surv.flw <- f.main1[, .(follow.date = unique(follow.date)), by = .(cube.symbol)
    ][f.surv.clean, on = .(cube.symbol)]
f.surv.flw[, pre.follow := fifelse(date < follow.date, 1, 0), by = .(cube.symbol)
    ][, post.follow := fifelse(date >= follow.date, 1, 0), by = .(cube.symbol)
    ][, gain := fcase(issale == 1 & price >= shift(hold.price, type = "lag"), 1,
                           issale == 1 & price < shift(hold.price, type = "lag"), 0,
                           issale == 0 & hold.price >= Clsprc, 0,
                           issale == 0& hold.price < Clsprc, 1)
    ][gain == 1 & pre.follow == 1, state := "盈利股票 (pre-follow)"
    ][gain == 0 & pre.follow == 1, state := "亏损股票 (pre-follow)"
    ][gain == 1 & post.follow == 1, state := "盈利股票 (post-follow)"
    ][gain == 0 & post.follow == 1, state := "亏损股票 (post-follow)"
    ][, hold.period := date - min(date), by = .(cube.symbol, stock.symbol)
    ][, pre.period := follow.date - min(date), by = .(cube.symbol)]

gg.main <- survfit(Surv(hold.period, issale) ~ state, data = f.surv.flw[hold.period < 200 & (pre.follow == 1 | (post.follow == 1 & date - as.Date(follow.date) <= pre.period))])

d.main <- ggsurv(gg.main,
                             lty.est = c(1, 1, 4, 4),
                             surv.col = c("#CC6666", "#7777DD", "#CC6666", "#7777DD"),
                             plot.cens = F,
                             xlab = "持有时间 (日)",
                             ylab = "剩余持仓数量的比例",
                             main = "",
                             size.est = 0.5,
                             order.legend = T
                            ) +
                            #geom_smooth() +
                            theme(
                                    #panel.background = element_rect(fill = "grey"),
                                    #panel.border = element_rect(linetype = 1, fill = NA),
                                    axis.line = element_line(linetype = 1),
                                    legend.title = element_blank(),
                                    legend.position = "bottom",
                                    legend.spacing.x = unit(0.1, 'cm'),
                                    legend.spacing.y = unit(2, 'cm'),
                                    legend.box = "horizontal",
                                    legend.box.background = element_rect(size = 1, colour = "black", fill = "white"),
                                    legend.key = element_rect(size = 0.5, colour = "black", fill = "white"),
                                    legend.key.size = unit(0.5, 'cm')
                                    )

ggsave("4-2.tiff", device = "tiff", dpi = 300, width = 6.67, height = 5)

# 重新回归前后处置效应图
library(styleer)
ld(sample1)
ld(sample2)
f.rbst1.early <- sample1[pre.follow == 1]
f.rbst1.late <- sample1[post.follow == 1 & date - as.Date(follow.date) <= pre.period]
DEbeta.e <- f.rbst1.early[, .(pre.follow = glm(sale ~ gain, family = binomial, maxit = 10) %>% coef()), by = .(cube.symbol)
    ][, .SD[2], by = .(cube.symbol)]
DEbeta.l <- f.rbst1.late[, .(post.follow = glm(sale ~ gain, family = binomial, maxit = 10) %>% coef()), by = .(cube.symbol)
    ][, .SD[2], by = .(cube.symbol)]
DEbeta <- DEbeta.e[DEbeta.l, on = "cube.symbol"]
ggDE <- melt(DEbeta[!is.na(pre.follow) & !is.na(post.follow)], id.vars = "cube.symbol", measure.vars = c("pre.follow", "post.follow"))
setnames(ggDE, 2:3, c("Stage", "DE"))
ggDE[, Stage := as.character(Stage)
    ][, Stage := ifelse(Stage == "pre.follow", "pre-follow", "post-follow")]
rm(DEbeta, DEbeta.e, DEbeta.l, f.rbst1.early, f.rbst1.late)

d.ttest <- ggplot(ggDE, aes(x = DE, colour = Stage, fill = Stage)) +
                            geom_line(stat = "density", size = 1) +
                            theme_grey() +
                            scale_colour_manual(values = c("#CC6666", "#7777DD")) +
                            labs(x = "处置效应", y = "比例（%）") +
                            scale_fill_manual(values = c("#CC6666", "#7777DD"),
                                                        name = "Stage",
                                                        breaks = c("post.follow", "pre.follow"),
                                                        labels = c("Post-follow", "Pre-follow")) +
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
ggsave("4-3.tiff", device = "tiff", dpi = 300, width = 6.67, height = 5)