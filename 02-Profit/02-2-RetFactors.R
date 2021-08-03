# ���Ƿ�ʵ�̵�cube��Ӯ�г�----
library(styleer)
library(quantreg)
library(lfe)
library(lmtest)
library(plm)
library(texreg)
library(broom)
library(rmarkdown)
# �����������
url <- str_c(getwd(), "/data/Factors/")
file.names <- list.files(path = url, pattern = "*.txt|.csv")
for (i in file.names) {
    assign(str_sub(i, start = 1L, end = -5L), fread(str_c(url, toupper(i))))
}
# ����������̼�����
url <- str_c(getwd(), "/data/Clprc")
Clsprc <- fbread(path = url, pattern = "*.txt")
Clsprc[, stock.symbol := str_pad(Stkcd, 6, side = "left", pad = "0")
    ][, date := as.Date(Trddt)
    ][, ':='(file_id = NULL, Stkcd = NULL, Trddt = NULL)]
# �ն�ָ���ļ�
url <- str_c(getwd(), "/data/Index")
index <- fbread(path = url, pattern = "*\\d.txt")
index <- index[, file_id := NULL
    ][Indexcd == "000300"
    ][, ':='(index_ret = Idxtrd08 / 100, index_value = Idxtrd05, date = as.Date(Idxtrd01))]

# �ֱ��ȫƽ̨������follow֮����follow֮ǰ���������з���----
ld(f.main1, force = T)
ld(f.main2, force = T)
ld(f.cube.ret.sp)
ld(f.cube.rb.sp)
ld(f.user.cmt.num)
f.main2 <- f.main2[!is.na(value), .SD]

# ���Ʊ����ļ��� ----
## ����ʱ��active.day����һ����portfolio��ʼһֱ��portfolio����ʱ����ʱ�䣨�죩
## ����Ƶ��trd.num�����30��Ľ��״����Ķ���
## ��������cmt.num�����30��ķ��������Ķ���
## ��Ʊ������stock.num������Ĺ�Ʊ������ ����ͨ��stock.list���м��㣩
## �г������mmt������umd��ֱ����֮���factor�м��룩

#1. f.main1����
cube.position <- f.main1[, .(cube.symbol, date)] # �����е�cube��ÿһ���positionȫ��ȡ��

## ���õ��ֵ����ݼ���ÿ��Ľ�������
cube.trd.num <- f.cube.rb.sp[target.weight != prev.weight.adjusted & (!is.na(target.weight) | !is.na(prev.weight.adjusted)), .SD
    ][order(cube.symbol, created.at), .SD, .SDcols = c("cube.symbol", "created.at", "stock.symbol")
    ][, date := as.Date(created.at)
    ][, .(trd.num = .N), by = .(cube.symbol, date)
    ][cube.position, on = .(cube.symbol, date)
    ][, trd.num := fifelse(is.na(trd.num), 0, trd.num)]

## ����f.user.cmt.num����ÿ��ķ�������
cube.cmt.num <- f.user.cmt.num[cube.position, on = .(cube.symbol, date)
    ][, cmt.num := fifelse(is.na(cmt.num), 0, cmt.num)]

## �������ݼ����кϲ�
cube.ctrl.var <- cube.trd.num[cube.cmt.num, on = .(cube.symbol, date)]

# ��f.main1���кϲ�
f.main1 <- cube.ctrl.var[f.main1, on = .(cube.symbol, date)]

# 2. f.main2����
cube.position <- f.main2[, .(cube.symbol, date)] # �����е�cube��ÿһ���positionȫ��ȡ��

## ���õ��ֵ����ݼ���ÿ��Ľ�������
cube.trd.num <- f.cube.rb.sp[target.weight != prev.weight.adjusted & (!is.na(target.weight) | !is.na(prev.weight.adjusted)), .SD
    ][order(cube.symbol, created.at), .SD, .SDcols = c("cube.symbol", "created.at", "stock.symbol")
    ][, date := as.Date(created.at)
    ][, .(trd.num = .N), by = .(cube.symbol, date)
    ][cube.position, on = .(cube.symbol, date)
    ][, trd.num := fifelse(is.na(trd.num), 0, trd.num)]

## ����f.user.cmt.num����ÿ��ķ�������
cube.cmt.num <- f.user.cmt.num[cube.position, on = .(cube.symbol, date)
    ][, cmt.num := fifelse(is.na(cmt.num), 0, cmt.num)]

## �������ݼ����кϲ�
cube.ctrl.var <- cube.trd.num[cube.cmt.num, on = .(cube.symbol, date)]

# ��f.main1���кϲ�
f.main2 <- cube.ctrl.var[f.main2, on = .(cube.symbol, date)]



# 1. ƽ̨ȫ����----
#cube.symbol2 <- f.main2[, unique(cube.symbol)]
f.full.daily <- fivefactor_daily[, setnames(.SD, 1, "date")
    ][, date := as.Date(date)
    ][f.cube.ret.sp, on = .(date)
    ][, ret := value / shift(value, type = "lag") - 1, by = .(cube.symbol)
    ][is.infinite(ret), ret := 0
    ][, date.qrtr := str_c(year(date), quarter(date))
    ][, tag := fifelse(cube.symbol %in% f.main2[, unique(cube.symbol)], 1, 0)]
#f.main1 <- f.main1[, period := max(date) - min(date), by = .(cube.symbol)
    #][period > 60, .SD]

#f.main2 <- f.main2[, period := max(date) - min(date), by = .(cube.symbol)
    #][period > 60, .SD]

outlier <- f.full.daily[ret <= -0.1 | ret >= 0.1, unique(cube.symbol)]

#f.full.daily <- f.main2[, .(cube.symbol, date, follow.date)
#][f.full.daily, on = .(cube.symbol, date), nomatch = 0
#][, post.follow := fifelse(is.na(follow.date), 0, fifelse(date < follow.date, 0, 1))]
alpha.full.sample <- f.full.daily[, .SD[.N > 90], by = .(cube.symbol)
    ][!(cube.symbol %in% outlier), .(coef.cube = lm(I((ret - rf)*100) ~ mkt_rf + smb + hml + umd + rmw + cma) %>% coef()), by = .(cube.symbol)
    ][, .SD[1], by = .(cube.symbol)]

#alpha.follow <- f.full.daily[, .SD[.N > 90], by = .(cube.symbol)
    #][!(cube.symbol %in% outlier) & (cube.symbol %in% f.main2$cube.symbol), .(coef.cube = lm(ret - rf ~ mkt_rf + smb + hml + umd + rmw + cma) %>% coef()), by = .(cube.symbol)
    #][, .SD[1], by = .(cube.symbol)]

#alpha.non.follow <- f.full.daily[, .SD[.N > 90], by = .(cube.symbol)
    #][!(cube.symbol %in% outlier) & !(cube.symbol %in% f.main2$cube.symbol), .(coef.cube = lm(ret - rf ~ mkt_rf + smb + hml + umd + rmw + cma) %>% coef()), by = .(cube.symbol)
    #][, .SD[1], by = .(cube.symbol)]

ggplot() +
    geom_histogram(alpha.full.sample, mapping = aes(x = coef.cube), stat = "density", color = "black", size = 1, adjust = 3) +
    labs(x = "���������ʣ�alpha��", y = "������%��") +
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
ggsave("5.1.tiff", device = "tiff", dpi = 300, width = 6.67, height = 5)

#f.full.daily[(cube.symbol %in% f.main2$cube.symbol) & !(cube.symbol %in% outlier), lm(ret - rf ~ mkt_rf + smb + hml + umd + rmw + cma)] %>% summary()
#f.full.daily[(cube.symbol %in% f.main2$cube.symbol) & !(cube.symbol %in% outlier), rq(ret - rf ~ mkt_rf + smb + hml + umd + rmw + cma, tau = 0.5, method = "pfn")] %>% summary()
#f.full.daily[(cube.symbol %in% f.main2$cube.symbol) & !(cube.symbol %in% outlier), rq(ret - rf ~ mkt_rf + smb + hml + umd + rmw + cma, tau = 0.8, method = "pfn")] %>% summary()

#f.full.daily[!(cube.symbol %in% outlier), felm(ret ~ tag | date.qrtr)] %>% summary()

r1 <- f.full.daily[!(cube.symbol %in% outlier), lm((ret - rf)*100 ~ mkt_rf + smb + hml)]
r2 <- f.full.daily[!(cube.symbol %in% outlier), lm((ret - rf)*100~ mkt_rf + smb + hml + umd)]
r3 <- f.full.daily[!(cube.symbol %in% outlier), lm((ret - rf)*100~ mkt_rf + smb + hml + rmw + cma)]
r4 <- f.full.daily[!(cube.symbol %in% outlier), lm((ret - rf)*100 ~ mkt_rf + smb + hml + umd + rmw + cma)]

#q1 <- f.full.daily[!(cube.symbol %in% outlier), rq(ret - rf ~ mkt_rf + smb + hml, tau = c(0.2), method = "pfn", data = .SD)] 
#q2 <- f.full.daily[!(cube.symbol %in% outlier), rq(ret - rf ~ mkt_rf + smb + hml + umd, tau = c(0.1, 0.5, 0.9), method = "pfn", data = .SD)] 
#q3 <- f.full.daily[!(cube.symbol %in% outlier), rq(ret - rf ~ mkt_rf + smb + hml + rmw + cma, tau = c(0.1, 0.5, 0.9), method = "pfn", data = .SD)] 
q1 <- f.full.daily[!(cube.symbol %in% outlier), rq((ret - rf)*100 ~ mkt_rf + smb + hml + umd + rmw + cma, tau = c(0.1), method = "pfn", data = .SD)]
q2 <- f.full.daily[!(cube.symbol %in% outlier), rq((ret - rf)*100 ~ mkt_rf + smb + hml + umd + rmw + cma, tau = c(0.2), method = "pfn", data = .SD)]
q3 <- f.full.daily[!(cube.symbol %in% outlier), rq((ret - rf)*100 ~ mkt_rf + smb + hml + umd + rmw + cma, tau = c(0.3), method = "pfn", data = .SD)]
q4 <- f.full.daily[!(cube.symbol %in% outlier), rq((ret - rf)*100 ~ mkt_rf + smb + hml + umd + rmw + cma, tau = c(0.4), method = "pfn", data = .SD)]
q5 <- f.full.daily[!(cube.symbol %in% outlier), rq((ret - rf)*100 ~ mkt_rf + smb + hml + umd + rmw + cma, tau = c(0.5), method = "pfn", data = .SD)]
q6 <- f.full.daily[!(cube.symbol %in% outlier), rq((ret - rf)*100 ~ mkt_rf + smb + hml + umd + rmw + cma, tau = c(0.6), method = "pfn", data = .SD)]
q7 <- f.full.daily[!(cube.symbol %in% outlier), rq((ret - rf)*100 ~ mkt_rf + smb + hml + umd + rmw + cma, tau = c(0.7), method = "pfn", data = .SD)]
q8 <- f.full.daily[!(cube.symbol %in% outlier), rq((ret - rf)*100 ~ mkt_rf + smb + hml + umd + rmw + cma, tau = c(0.8), method = "pfn", data = .SD)]
q9 <- f.full.daily[!(cube.symbol %in% outlier), rq((ret - rf)*100 ~ mkt_rf + smb + hml + umd + rmw + cma, tau = c(0.9), method = "pfn", data = .SD)]

library(texreg)
library(broom)
library(rmarkdown)
list(r1, r2, r3, r4) %>%
    htmlreg(
        file = "6.2.html",
        #custom.header = list("Pre-follow" = 1:2, 
        #"Two-stage" = 3:4, 
        #"After two-stage" = 5:6, 
        #"Full sample" = 7:8),
        caption.above = TRUE,
        include.rs = TRUE,
        include.adjrs = FALSE,
        custom.model.names = c("(1)", "(2)", "(3)", "(4)"),
        #custom.gof.rows = list("Trader FE" = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                                                 #"Holding period FE" = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                                                 #"Stock FE" = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")),
        custom.coef.names = c("alpha", "mkt_rf", "smb", "hml", "umd", "rmw", "cma"),
        reorder.coef = c(1:7),
        digits = 3,
        inline.css = FALSE,
        doctype = TRUE,
        html.tag = TRUE,
        head.tag = TRUE,
        body.tag = TRUE,
        center = FALSE,
        indentation = "",
        table.margin = 0
            )

htmlreg(list(q1, q2, q3, q4,q5,q6,q7,q8,q9),
    file = "6.3.html",
    #custom.header = list("Pre-follow" = 1:2, 
                                        #"Two-stage" = 3:4, 
                                        #"After two-stage" = 5:6, 
                                        #"Full sample" = 7:8),
    caption.above = TRUE,
    include.rs = TRUE,
    include.adjrs = FALSE,
    custom.model.names = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)", "(9)"),
    #custom.gof.rows = list("Trader FE" = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
    #"Holding period FE" = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
    #"Stock FE" = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")),
    custom.coef.names = c("alpha", "mkt_rf", "smb", "hml", "umd", "rmw", "cma"),
    #reorder.coef = c(1:7),
    digits = 3,
    inline.css = FALSE,
    doctype = TRUE,
    html.tag = TRUE,
    head.tag = TRUE,
    body.tag = TRUE,
    center = FALSE,
    indentation = "",
    table.margin = 0
        )

# 2. follow֮ǰ��֮��----
#  2.1 �ն�����----
f.main1.daily <- index[fivefactor_daily[, setnames(.SD, 1, "date")], on = .(date)
    ][, date := as.Date(date)
    ][f.main1, on = .(date)
    ][, ret := value / shift(value, type = "lag") - 1, by = .(cube.symbol)
    ][, ret.lag := shift(ret, type = "lag"), by = .(cube.symbol)
    ][, period := max(date) - min(date), by = .(cube.symbol)
    ][period > 90, .SD
    ][, .SD[max(date) - follow.date > pre.period], by = .(cube.symbol)
    ][, post.follow := fifelse(date < follow.date, 0, 1), by = .(cube.symbol)
    ][, start := min(date), by = .(cube.symbol)
    ][, active.day := as.numeric(date - min(date), units = "weeks"), by = .(cube.symbol)
    ][, date.qrtr := str_c(year(date), quarter(date))
    ][, trd.num.30day := frollsum(trd.num, 29), by = .(cube.symbol) # ��ȥ30���ڵĽ�������
    ][, stock.num.30day := frollmean(stock.num, 29), by = .(cube.symbol) # ��ȥ30����ƽ����Ʊ����������
    ][, cmt.num.30day := frollsum(cmt.num, 30), by = .(cube.symbol) # ��ȥ30���ڵķ�������
    ][!(cube.symbol %in% outlier), ':='(alpha = coef(lm(ret ~ index_ret))[1], beta = coef(lm(ret ~ index_ret))[2]), by = .(cube.symbol)
    ][, ret.abnr := ret - alpha - beta * index_ret
    ][, ret.abnr.cum := frollsum(ret.abnr, 30, na.rm = T), by = .(cube.symbol)
    ]

f.main2.daily <- index[fivefactor_daily[, setnames(.SD, 1, "date")], on = .(date)
    ][, date := as.Date(date)
    ][f.main2, on = .(date)
    ][order(cube.symbol, date), .SD
    ][, ret := value / shift(value, type = "lag") - 1, by = .(cube.symbol)
    ][, ret.lag := shift(ret, type = "lag"), by = .(cube.symbol)
    ][is.infinite(ret), ret := 0
    ][, period := max(date) - min(date), by = .(cube.symbol)
    ][, post.follow := fifelse(date < follow.date, 0, 1), by = .(cube.symbol)
    ][period > 90, .SD
    ][, .SD[max(date) - follow.date > follow.date - min(date)], by = .(cube.symbol)
    ][, start := min(date), by = .(cube.symbol)
    ][, active.day := as.numeric(date - min(date), units = "weeks"), by = .(cube.symbol)
    ][, date.qrtr := str_c(year(date), quarter(date))
    ][, trd.num.30day := frollsum(trd.num, 29), by = .(cube.symbol) # ��ȥ30���ڵĽ�������
    ][, stock.num.30day := frollmean(stock.num, 29), by = .(cube.symbol) # ��ȥ30����ƽ����Ʊ����������
    ][, cmt.num.30day := frollsum(cmt.num, 30), by = .(cube.symbol) # ��ȥ30���ڵķ�������
    ][!(cube.symbol %in% outlier), ':='(alpha = coef(lm(ret ~ index_ret))[1], beta = coef(lm(ret ~ index_ret))[2]), by = .(cube.symbol)
    ][, ret.abnr := ret - alpha - beta * index_ret
    ][, ret.abnr.30day := frollsum(ret.abnr, 30, na.rm = T), by = .(cube.symbol)
    ][, ret.abnr.60day := frollsum(ret.abnr, 60, na.rm = T), by = .(cube.symbol)
    ][, ret.abnr.90day := frollsum(ret.abnr, 90, na.rm = T), by = .(cube.symbol)
    ][, ret.abnr.30day.lag := shift(ret.abnr.30day, type = "lag"), by = .(cube.symbol)
    ][, ret.abnr.60day.lag := shift(ret.abnr.60day, type = "lag"), by = .(cube.symbol)
    ][, ret.abnr.90day.lag := shift(ret.abnr.90day, type = "lag"), by = .(cube.symbol)
    ][, oud.30day := frollmean(oud, 30, na.rm = T), by = .(cube.symbol)
    ][, ind.30day := frollmean(ind, 30, na.rm = T), by = .(cube.symbol)
    ][, ln.cntr.30day := frollmean(ln.cntr, 30, na.rm = T), by = .(cube.symbol)
    ][, oud.60day := frollmean(oud, 60, na.rm = T), by = .(cube.symbol)
    ][, ind.60day := frollmean(ind, 60, na.rm = T), by = .(cube.symbol)
    ][, ln.cntr.60day := frollmean(ln.cntr, 60, na.rm = T), by = .(cube.symbol)
    ][, oud.90day := frollmean(oud, 90, na.rm = T), by = .(cube.symbol)
    ][, ind.90day := frollmean(ind, 90, na.rm = T), by = .(cube.symbol)
    ][, ln.cntr.90day := frollmean(ln.cntr, 90, na.rm = T), by = .(cube.symbol)
    ]

# �ն����ݣ�followǰ��ret�Ƚ�
# Newey West t-test
nwttest.daily <- f.main1.daily[!(cube.symbol %in% outlier) & (post.follow == 0 | (post.follow == 1 & (date - follow.date <= pre.period)))]
a <- nwttest.daily[post.follow == 0, ret]
b <- nwttest.daily[post.follow == 1, ret]
nwttest.daily[, .(avg.ret = mean(ret, na.rm = T)), by = .(post.follow, cube.symbol)
    ][, t.test(avg.ret ~ post.follow)]
lm((b - a) ~ 1) %>% lmtest::coeftest()

# followǰ��alpha�Ƚ�
r1 <- f.main1.daily[!(cube.symbol %in% outlier) & post.follow == 0, lm((ret - rf)*100 ~ mkt_rf + smb + hml)] 
r2 <- f.main1.daily[!(cube.symbol %in% outlier) & post.follow == 0, lm((ret - rf)*100 ~ mkt_rf + smb + hml + umd)] 
r3 <- f.main1.daily[!(cube.symbol %in% outlier) & post.follow == 0, lm((ret - rf)*100 ~ mkt_rf + smb + hml + rmw + cma)] 
r4 <- f.main1.daily[!(cube.symbol %in% outlier) & post.follow == 0, lm((ret - rf)*100 ~ mkt_rf + smb + hml + umd + rmw + cma)] 


r5 <- f.main1.daily[!(cube.symbol %in% outlier) & (post.follow == 1 & (date - follow.date <= pre.period)), lm((ret - rf) * 100 ~ mkt_rf + smb + hml)] 
r6 <- f.main1.daily[!(cube.symbol %in% outlier) & (post.follow == 1 & (date - follow.date <= pre.period)), lm((ret - rf) * 100 ~ mkt_rf + smb + hml + umd)]
r7 <- f.main1.daily[!(cube.symbol %in% outlier) & (post.follow == 1 & (date - follow.date <= pre.period)), lm((ret - rf) * 100 ~ mkt_rf + smb + hml + rmw + cma)]
r8 <- f.main1.daily[!(cube.symbol %in% outlier) & (post.follow == 1 & (date - follow.date <= pre.period)), lm((ret - rf) * 100 ~ mkt_rf + smb + hml + umd + rmw + cma)]

list(r1, r2, r3, r4, r5, r6, r7, r8) %>%
    htmlreg(
        file = "6.4.html",
        custom.header = list("Pre-follow" = 1:4, "Post-follow" = 5:8),
        #caption.above = TRUE,
        include.rs = TRUE,
        include.adjrs = FALSE,
        custom.model.names = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)"),
#custom.gof.rows = list("Trader FE" = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
#"Holding period FE" = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
#"Stock FE" = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")),
        custom.coef.names = c("alpha", "mkt_rf", "smb", "hml", "umd", "rmw", "cma"),
        #reorder.coef = c(1:7),
        digits = 3,
        inline.css = FALSE,
        doctype = TRUE,
        html.tag = TRUE,
        head.tag = TRUE,
        body.tag = TRUE,
        center = FALSE,
        indentation = "",
        table.margin = 0
            )


f.main1.daily[!(cube.symbol %in% outlier) & (post.follow == 1 & (date - follow.date > pre.period)), lm(ret - rf ~ mkt_rf + smb + hml + umd + rmw + cma)] %>% summary()
f.main1.daily[!(cube.symbol %in% outlier) & (post.follow == 1), lm(ret - rf ~ mkt_rf + smb + hml + umd + rmw + cma)] %>% summary()
f.main1.daily[!(cube.symbol %in% outlier), lm(ret - rf ~ mkt_rf + smb + hml + umd + rmw + cma)] %>% summary()


# �̶�ЧӦ�ع�
r1 <- f.main1.daily[!(cube.symbol %in% outlier) & (post.follow == 0 | (post.follow == 1 & (date - follow.date <= pre.period))), felm(I(ret * 100) ~ post.follow + I(ret.lag*100) | cube.symbol + date.qrtr)]
r2 <- f.main1.daily[!(cube.symbol %in% outlier) & (post.follow == 0 | (post.follow == 1 & (date - follow.date <= pre.period))), felm(I(ret * 100) ~ post.follow + I(ret.lag * 100) + umd | cube.symbol + date.qrtr)]
r3 <- f.main1.daily[!(cube.symbol %in% outlier) & (post.follow == 0 | (post.follow == 1 & (date - follow.date <= pre.period))), felm(I(ret * 100) ~ post.follow + I(ret.lag * 100) + active.day | cube.symbol + date.qrtr)]
r4 <- f.main1.daily[!(cube.symbol %in% outlier) & (post.follow == 0 | (post.follow == 1 & (date - follow.date <= pre.period))), felm(I(ret * 100) ~ post.follow + I(ret.lag * 100) + cmt.num + trd.num + stock.num | cube.symbol + date.qrtr)]
r5 <- f.main1.daily[!(cube.symbol %in% outlier) & (post.follow == 0 | (post.follow == 1 & (date - follow.date <= pre.period))), felm(I(ret * 100) ~ post.follow + I(ret.lag * 100) + cmt.num.30day + trd.num.30day + stock.num.30day | cube.symbol + date.qrtr)]
r6 <- f.main1.daily[!(cube.symbol %in% outlier) & (post.follow == 0 | (post.follow == 1 & (date - follow.date <= pre.period))), felm(I(ret * 100) ~ post.follow + I(ret.lag * 100) + umd + active.day + cmt.num + trd.num + stock.num + cmt.num.30day + trd.num.30day + stock.num.30day | cube.symbol + date.qrtr)]
#r7 <- f.main1.daily[!(cube.symbol %in% outlier), felm(ret ~ post.follow + ret.lag | cube.symbol + date.qrtr)]
#r8 <- f.main1.daily[!(cube.symbol %in% outlier), felm(ret ~ post.follow + ret.lag + umd + active.day + cmt.num + trd.num + stock.num + cmt.num.30day + trd.num.30day + stock.num.30day | cube.symbol + date.qrtr)]

list(r1, r2, r3, r4, r5, r6) %>%
    htmlreg(
        file = "6.5.html",
        custom.header = list("Two-stage" = 1:6),
#caption.above = TRUE,
        include.rs = TRUE,
        include.adjrs = FALSE,
        custom.model.names = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)"),
        custom.gof.rows = list("Trader FE" = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                                            "Quarter FE" = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes")),
        custom.coef.names = c("post-follow", "return.lag", "umd", "active.day", "cmt.num", "trd.num", "stock.num", "cmt.num.30day", "trd.num.30day", "stock.num.30day"),
#reorder.coef = c(1:7),
        digits = 3,
        inline.css = FALSE,
        doctype = TRUE,
        html.tag = TRUE,
        head.tag = TRUE,
        body.tag = TRUE,
        center = FALSE,
        indentation = "",
        table.margin = 0,
        padding = 0.1
            )

# �ն����ݣ��������������µ�ret�Ƚ�
r1 <- f.main2.daily[!(cube.symbol %in% outlier), felm(I(ret * 100) ~ log(oud + 1) | cube.symbol + date.qrtr)]
r2 <- f.main2.daily[!(cube.symbol %in% outlier), felm(I(ret * 100) ~ I(ln.cntr * 100) | cube.symbol + date.qrtr)]
r3 <- f.main2.daily[!(cube.symbol %in% outlier), felm(I(ret * 100) ~ log(ind + 1) | cube.symbol + date.qrtr)]
r4 <- f.main2.daily[!(cube.symbol %in% outlier), felm(I(ret * 100) ~ log(oud + 1) + I(ln.cntr * 100) + log(ind + 1) | cube.symbol + date.qrtr)]
r5 <- f.main2.daily[!(cube.symbol %in% outlier), felm(I(ret * 100) ~ log(oud + 1) + I(ln.cntr * 100) + log(ind + 1) + ret.lag + umd + active.day + cmt.num + trd.num + stock.num | cube.symbol + date.qrtr)]
r6 <- f.main2.daily[!(cube.symbol %in% outlier), felm(I(ret * 100) ~ log(oud + 1) + I(ln.cntr * 100) + log(ind + 1) + ret.lag + umd + active.day + cmt.num + trd.num + stock.num + cmt.num.30day + trd.num.30day + stock.num.30day | cube.symbol + date.qrtr)]
list(r1, r2, r3, r4, r5, r6) %>%
    htmlreg(
        file = "6.6.html",
        #custom.header = list("Two-stage" = 1:6),
#caption.above = TRUE,
        include.rs = TRUE,
        include.adjrs = FALSE,
        custom.model.names = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)"),
        custom.gof.rows = list("Trader FE" = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                                            "Quarter FE" = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes")),
        custom.coef.names = c("learning intensity", "learning quality", "public scrutinization", "return.lag", "umd", "active.day", "cmt.num", "trd.num", "stock.num", "cmt.num.30day", "trd.num.30day", "stock.num.30day"),
#reorder.coef = c(1:7),
        digits = 3,
        inline.css = FALSE,
        doctype = TRUE,
        html.tag = TRUE,
        head.tag = TRUE,
        body.tag = TRUE,
        center = FALSE,
        indentation = "",
        table.margin = 0,
        padding = 0.1
            )

# �նȹ�������������
f.main1.daily[!(cube.symbol %in% outlier), felm(I(ret.abnr.cum*100) ~ post.follow | cube.symbol + date.qrtr) %>% summary()]
f.main1.daily[!(cube.symbol %in% outlier), felm(I(ret.abnr.cum * 100) ~ post.follow + active.day + cmt.num + trd.num + stock.num + cmt.num.30day + trd.num.30day + stock.num.30day | cube.symbol + date.qrtr) %>% summary()]

r1 <- f.main2.daily[!(cube.symbol %in% outlier), felm(I(ret.abnr.30day * 100) ~ log(oud.30day + 1) + I(ln.cntr.30day * 100) + log(ind.30day + 1) | cube.symbol + date.qrtr)]
r2 <- f.main2.daily[!(cube.symbol %in% outlier), felm(I(ret.abnr.30day * 100) ~ log(oud.30day + 1) + I(ln.cntr.30day * 100) + log(ind.30day + 1) + ret.abnr.30day.lag + active.day + cmt.num.30day + trd.num.30day + stock.num.30day | cube.symbol + date.qrtr)]

r3 <- f.main2.daily[!(cube.symbol %in% outlier), felm(I(ret.abnr.60day * 100) ~ log(oud.60day + 1) + I(ln.cntr.60day * 100) + log(ind.60day + 1) | cube.symbol + date.qrtr)]
r4 <- f.main2.daily[!(cube.symbol %in% outlier), felm(I(ret.abnr.60day * 100) ~ log(oud.60day + 1) + I(ln.cntr.60day * 100) + log(ind.60day + 1) + ret.abnr.60day.lag + active.day + cmt.num.30day + trd.num.30day + stock.num.30day | cube.symbol + date.qrtr)]

r5 <- f.main2.daily[!(cube.symbol %in% outlier), felm(I(ret.abnr.90day * 100) ~ log(oud.90day + 1) + I(ln.cntr.90day * 100) + log(ind.90day + 1) | cube.symbol + date.qrtr)]
r6 <- f.main2.daily[!(cube.symbol %in% outlier), felm(I(ret.abnr.90day * 100) ~ log(oud.90day + 1) + I(ln.cntr.90day * 100) + log(ind.90day + 1) + ret.abnr.90day.lag + active.day  + cmt.num.30day + trd.num.30day + stock.num.30day | cube.symbol + date.qrtr)]

list(r1, r2, r3, r4, r5, r6) %>%
    htmlreg(
        file = "6.7.html",
        custom.header = list("CAR.30day" = 1:2, "CAR.60day" = 3:4, "CAR.90day" = 5:6),
#caption.above = TRUE,
        include.rs = TRUE,
        include.adjrs = FALSE,
        custom.model.names = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)"),
        custom.gof.rows = list(
                               #"Control Variables" = c("No", "Yes", "No", "Yes", "No", "Yes"),
                                            "Trader FE" = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                                            "Quarter FE" = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes")
                                            ),
        custom.coef.names = c("LI.30day", "LQ.30day", "PS.30day", "CAR.30day.lag", "active.day", "cmt.num.30day", "trd.num.30day", "stock.num.30day", "LI.60day", "LQ.60day", "PS.60day", "CAR.60day.lag", "LI.90day", "LQ.90day", "PS.90day", "CAR.90day.lag"),
        #omit.coef = "active.day|cmt.num.30day|trd.num.30day|stock.num.30day",
        reorder.coef = c(1:4, 9:16, 5:8),
        caption.above = TRUE,
        digits = 3,
        inline.css = FALSE,
        doctype = TRUE,
        html.tag = TRUE,
        head.tag = TRUE,
        body.tag = TRUE,
        center = FALSE,
            )


# ��λ���ع�
rq1.r <- f.main1.daily[!(cube.symbol %in% outlier) & (post.follow == 0 | (post.follow == 1 & (date - follow.date <= pre.period))), rq((ret)*100 ~ post.follow + as.factor(date.qrtr), tau = seq(0.1, 0.9, 0.1), method = "sfn", data = .SD)]
rq1.er <- f.main1.daily[!(cube.symbol %in% outlier) & (post.follow == 0 | (post.follow == 1 & (date - follow.date <= pre.period))), rq((ret.abnr.cum) ~ post.follow + as.factor(date.qrtr), tau = seq(0.1, 0.9, 0.1), method = "sfn", data = .SD)]

rq2.r <- f.main2.daily[!(cube.symbol %in% outlier), rq((ret*100) ~ I(ln.cntr * 100) + log(oud + 1) + log(ind + 1)  + as.factor(date.qrtr), tau = seq(0.1, 0.9, 0.1), method = "sfn", data = .SD)]
rq2.er.30.1 <- f.main2.daily[!(cube.symbol %in% outlier), rq((ret.abnr.30day * 100) ~ I(ln.cntr.30day * 100) + log(oud.30day + 1) + log(ind.30day + 1) + as.factor(date.qrtr), tau = 0.1, method = "sfn", data = .SD)]
rq2.er.30.2 <- f.main2.daily[!(cube.symbol %in% outlier), rq((ret.abnr.30day * 100) ~ I(ln.cntr.30day * 100) + log(oud.30day + 1) + log(ind.30day + 1) + as.factor(date.qrtr), tau = 0.2, method = "sfn", data = .SD)]
rq2.er.30.3 <- f.main2.daily[!(cube.symbol %in% outlier), rq((ret.abnr.30day * 100) ~ I(ln.cntr.30day * 100) + log(oud.30day + 1) + log(ind.30day + 1) + as.factor(date.qrtr), tau = 0.3, method = "sfn", data = .SD)]
rq2.er.30.4 <- f.main2.daily[!(cube.symbol %in% outlier), rq((ret.abnr.30day * 100) ~ I(ln.cntr.30day * 100) + log(oud.30day + 1) + log(ind.30day + 1) + as.factor(date.qrtr), tau = 0.4, method = "sfn", data = .SD)]
rq2.er.30.5 <- f.main2.daily[!(cube.symbol %in% outlier), rq((ret.abnr.30day * 100) ~ I(ln.cntr.30day * 100) + log(oud.30day + 1) + log(ind.30day + 1) + as.factor(date.qrtr), tau = 0.5, method = "sfn", data = .SD)]
rq2.er.30.6 <- f.main2.daily[!(cube.symbol %in% outlier), rq((ret.abnr.30day * 100) ~ I(ln.cntr.30day * 100) + log(oud.30day + 1) + log(ind.30day + 1) + as.factor(date.qrtr), tau = 0.6, method = "sfn", data = .SD)]
rq2.er.30.7 <- f.main2.daily[!(cube.symbol %in% outlier), rq((ret.abnr.30day * 100) ~ I(ln.cntr.30day * 100) + log(oud.30day + 1) + log(ind.30day + 1) + as.factor(date.qrtr), tau = 0.7, method = "sfn", data = .SD)]
rq2.er.30.8 <- f.main2.daily[!(cube.symbol %in% outlier), rq((ret.abnr.30day * 100) ~ I(ln.cntr.30day * 100) + log(oud.30day + 1) + log(ind.30day + 1) + as.factor(date.qrtr), tau = 0.8, method = "sfn", data = .SD)]
rq2.er.30.9 <- f.main2.daily[!(cube.symbol %in% outlier), rq((ret.abnr.30day * 100) ~ I(ln.cntr.30day * 100) + log(oud.30day + 1) + log(ind.30day + 1) + as.factor(date.qrtr), tau = 0.9, method = "sfn", data = .SD)]

list(rq2.er.30.1, rq2.er.30.2, rq2.er.30.3, rq2.er.30.4, rq2.er.30.5, rq2.er.30.6, rq2.er.30.7, rq2.er.30.8, rq2.er.30.9) %>%
    htmlreg(
        file = "6.8.1.html",
        #custom.header = list("CAR.30day" = 1:2, "CAR.60day" = 3:4, "CAR.90day" = 5:6),
#caption.above = TRUE,
        include.rs = TRUE,
        include.adjrs = FALSE,
        custom.model.names = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)", "(9)"),
        #custom.gof.rows = list(
##"Control Variables" = c("No", "Yes", "No", "Yes", "No", "Yes"),
                                            #"Trader FE" = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                                            #"Quarter FE" = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes")
                                            #),
        #custom.coef.names = c("NA", "LI.30day", "LQ.30day", "PS.30day", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA"),
        #omit.coef = "date.qrtr|Intercept",
        #reorder.coef = c(1:4, 9:16, 5:8),
        caption.above = TRUE,
        digits = 3,
        inline.css = FALSE,
        doctype = TRUE,
        html.tag = TRUE,
        head.tag = TRUE,
        body.tag = TRUE,
        center = FALSE,
            )

rq2.er.60 <- f.main2.daily[!(cube.symbol %in% outlier), rq((ret.abnr.60day*100) ~ I(ln.cntr.60day * 100) + log(oud.60day + 1) + log(ind.60day + 1) + as.factor(date.qrtr), tau = seq(0.1, 0.9, 0.1), method = "sfn", data = .SD)]
rq2.er.90 <- f.main2.daily[!(cube.symbol %in% outlier), rq((ret.abnr.90day*100) ~ I(ln.cntr.90day * 100) + log(oud.90day + 1) + log(ind.90day + 1) + as.factor(date.qrtr), tau = seq(0.1, 0.9, 0.1), method = "sfn", data = .SD)]



### 2.2 �ܶ�����
#fivefactor_weekly[, trdwk := str_c(year(trdwk), week(trdwk))]
#f.main1[, trdwk := str_c(year(date), week(date))]
#f.main1.weekly <- fivefactor_weekly[f.main1, on = .(trdwk)
    #][, ret := value / shift(value, type = "lag") - 1, by = .(cube.symbol)
    #][is.infinite(ret), ret := 0
    ##][ret != 0, .SD # ȥ����Щvalueһֱû��ļ�¼
    #][, .SD[.N], by = .(cube.symbol, trdwk)
    #][, ret_week := value / shift(value, type = "lag") - 1, by = .(cube.symbol)
    #][is.infinite(ret_week), ret_week := 0
    #][pre.period >= 7, .SD
    #][, post.follow := fifelse(date < follow.date, 0, 1), by = .(cube.symbol)]

#f.main1.weekly[!(cube.symbol %in% outlier) & (post.follow == 0 | (post.follow == 1 & (date - follow.date < pre.period))), felm(ret_week ~ post.follow | cube.symbol + date)] %>% summary()

#f.main1.weekly[!(cube.symbol %in% outlier) & post.follow == 0, lm(ret_week - rf ~ mkt_rf + smb + hml)] %>% summary()
#f.main1.weekly[!(cube.symbol %in% outlier) & post.follow == 1 & (date - follow.date < pre.period), lm(ret_week - rf ~ mkt_rf + smb + hml)] %>% summary()

#f.main1.weekly[!(cube.symbol %in% outlier) & post.follow == 0, lm(ret_week - rf ~ mkt_rf + smb + hml + umd)] %>% summary()
#f.main1.weekly[!(cube.symbol %in% outlier) & post.follow == 1 & (date - follow.date < pre.period), lm(ret_week - rf ~ mkt_rf + smb + hml + umd)] %>% summary()

#f.main1.weekly[!(cube.symbol %in% outlier) & post.follow == 0, lm(ret_week - rf ~ mkt_rf + smb + hml + rmw + cma)] %>% summary()
#f.main1.weekly[!(cube.symbol %in% outlier) & post.follow == 1 & (date - follow.date < pre.period), lm(ret_week - rf ~ mkt_rf + smb + hml + rmw + cma)] %>% summary()

#f.main1.weekly[post.follow == 1 & (date - follow.date > pre.period), lm(ret_week - rf ~ mkt_rf + smb + hml)] %>% summary()
#f.main1.weekly[post.follow == 1 & (date - follow.date > pre.period), lm(ret_week - rf ~ mkt_rf + smb + hml + umd)] %>% summary()
#f.main1.weekly[post.follow == 1 & (date - follow.date > pre.period), lm(ret_week - rf ~ mkt_rf + smb + hml + rmw + cma)] %>% summary()

#f.main1.weekly[post.follow == 1, lm(ret_week - rf ~ mkt_rf + smb + hml)] %>% summary()
#f.main1.weekly[post.follow == 1, lm(ret_week - rf ~ mkt_rf + smb + hml + umd)] %>% summary()
#f.main1.weekly[post.follow == 1, lm(ret_week - rf ~ mkt_rf + smb + hml + rmw + cma)] %>% summary()

#f.main1.weekly[, lm(ret_week - rf ~ mkt_rf + smb + hml)] %>% summary()
#f.main1.weekly[, lm(ret_week - rf ~ mkt_rf + smb + hml + umd)] %>% summary()
#f.main1.weekly[, lm(ret_week - rf ~ mkt_rf + smb + hml + rmw + cma)] %>% summary()


## 2.3 �¶����� ----
# �ȼ���ÿ�½�������
trd.dnum <- Clsprc[, .(date = unique(date))
    ][, trdmn := as.character(date) %>% str_sub(start = 1L, end = 7L) %>% str_replace_all("-", "")
    ][, trd.dnum.std := .N, by = .(trdmn)
    ][, unique(.SD), .SDcols = -1]
# f.main1
f.main1[, trdmn := as.character(date) %>% str_sub(start = 1L, end = 7L) %>% str_replace_all("-", "")
    ][, start := min(date), by = .(cube.symbol)
    ][, trd.dnum := .N, by = .(cube.symbol, trdmn)]
f.main1 <- trd.dnum[f.main1, on = .(trdmn)]

f.main1.monthly <- fivefactor_monthly[, trdmn := as.character(trdmn)
    ][f.main1, on = .(trdmn)
    ][, pre.period := follow.date - min(date), by = .(cube.symbol)
    ][pre.period >= 30, .SD
    ][, ret := value / shift(value, type = "lag") - 1, by = .(cube.symbol)
    ][is.infinite(ret), ret := 0
    #][ret != 0, .SD # ȥ����Щvalueһֱû��ļ�¼
    ][, .SD[.N], by = .(cube.symbol, trdmn)
    ][, ret_month := value / shift(value, type = "lag") - 1, by = .(cube.symbol)
    ][is.infinite(ret_month), ret_month := 0
    #][, trdmn.fd := as.character(follow.date) %>% str_sub(start = 1L, end = 7L) %>% str_replace_all("-", "")
    ][, post.follow := fifelse(date > follow.date, 1, 0), by = .(cube.symbol)
    #][, .SD[-.N], by = .(cube.symbol)
    ][, active.day := as.numeric(date - start), by = .(cube.symbol)
    ][!(trdmn %in% c("201606", "201807")), .SD
    ][trd.dnum.std == trd.dnum, .SD]


# �¶����ݣ��¶�Alpha�Ƚ� 
f.main1.monthly[!(cube.symbol %in% outlier) & post.follow == 0, lm(ret_month - rf ~ mkt_rf + smb + hml + umd + rmw + cma)] %>% summary()
f.main1.monthly[!(cube.symbol %in% outlier) & post.follow == 1 & (date - follow.date <= pre.period), lm(ret_month - rf ~ mkt_rf + smb + hml + umd + rmw + cma)] %>% summary()

# Newey west t-test
nwttest.monthly <- f.main1.monthly[!(cube.symbol %in% outlier) & (post.follow == 0 | (post.follow == 1 & (date - follow.date <= pre.period)))]
a <- nwttest.monthly[post.follow == 0, ret]
b <- nwttest.monthly[post.follow == 1, ret]
nwttest.monthly[, .(avg.ret = mean(ret_month, na.rm = T)), by = .(post.follow, cube.symbol)
    ][, t.test(avg.ret ~ post.follow)]
lm((b - a) ~ 1) %>% lmtest::coeftest()

# �¶����ݣ��¶�ret�Ƚ�
f.main1.monthly[!(cube.symbol %in% outlier) & (post.follow == 0 | (post.follow == 1 & (date - follow.date <= pre.period))), felm(ret_month ~ post.follow | cube.symbol )] %>% summary()
f.main1.monthly[!(cube.symbol %in% outlier), felm(ret_month ~ mkt_rf + smb + hml + umd + rmw + cma)] %>% summary()
f.main1.monthly[!(cube.symbol %in% outlier) & (post.follow == 0 | (post.follow == 1 & (date - follow.date <= pre.period))), felm(ret_month ~ post.follow + mkt_rf + smb + hml + umd + rmw + cma | cube.symbol)] %>% summary()
f.main1.monthly[!(cube.symbol %in% outlier), felm(ret_month ~ post.follow + mkt_rf + smb + hml + umd + rmw + cma | cube.symbol)] %>% summary()


# f.main2
f.main2[, trdmn := as.character(date) %>% str_sub(start = 1L, end = 7L) %>% str_replace_all("-", "")
    ][, start := min(date), by = .(cube.symbol)]
f.main2.monthly <- fivefactor_monthly[, trdmn := as.character(trdmn)
    ][f.main2, on = .(trdmn)
    ][, ret := value / shift(value, type = "lag") - 1, by = .(cube.symbol)
    ][is.infinite(ret), ret := 0
    ][, ':='(oud_month = mean(oud, na.rm = T), ind_month = mean(ind, na.rm = T), ln.cntr_month = mean(ln.cntr, na.rm = T)), by = .(cube.symbol, trdmn)
    #][ret != 0, .SD # ȥ����Щvalueһֱû��ļ�¼
    ][, .SD[.N], by = .(cube.symbol, trdmn)
    ][, ret_month := value / shift(value, type = "lag") - 1, by = .(cube.symbol)
    ][is.infinite(ret_month), ret_month := 0
    ][, pre.period := follow.date - min(date), by = .(cube.symbol)
    ][pre.period >= 30, .SD
    ][order(cube.symbol, date, trdmn), .SD
    ][, post.follow := fifelse(date < follow.date, 0, 1), by = .(cube.symbol)
    ][, .SD[-.N], by = .(cube.symbol)
    ]

f.main2.monthly[!(cube.symbol %in% outlier), felm(ret_month ~ log(ind_month + 1) | cube.symbol + follow.date)] %>% summary()
f.main2.monthly[!(cube.symbol %in% outlier), felm(ret_month ~ log(oud_month + 1) | cube.symbol + follow.date)] %>% summary()
f.main2.monthly[!(cube.symbol %in% outlier), felm(ret_month ~ I(ln.cntr_month * 100) | cube.symbol + follow.date)] %>% summary()
f.main2.monthly[!(cube.symbol %in% outlier), felm(ret_month ~ I(ln.cntr_month * 100) + log(oud_month + 1) + log(ind_month + 1)  | cube.symbol + date)] %>% summary()
#f.main2.monthly[!(cube.symbol %in% outlier), felm(ret_month ~ I(ln.cntr * 100) + log(oud + 1) + log(ind + 1) + mkt_rf + smb + hml + umd + rmw + cma | cube.symbol + follow.date)] %>% summary()
