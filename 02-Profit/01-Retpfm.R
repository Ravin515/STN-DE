ld(r.cube.ret.1803)
# extract the SP cubes
f.ret.sp.1803 <- r.cube.ret[cube.type == "SP"]
ld(r.cube.ret.1703)
f.ret.sp.1703 <- r.cube.ret[cube_type == "SP"]
f.ret.sp.1703 <- f.ret.sp.1703[date < as.Date("2017-01-03")]
f.ret.sp.1703 <- f.ret.sp.1703[, setnames(.SD, 2:3, c("cube.symbol", "cube.type"))]

f.ret.sp <- rbindlist(list(f.ret.sp.1703, f.ret.sp.1803), fill = T)
sv(f.ret.sp)
rm(f.ret.sp.1703, f.ret.sp.1803)

ld(f.surv.flw)
#split the trades by the first-follow date
f.surv.flw[, tag := ifelse(date - as.Date(follow.date) > 0, 1, 0), keyby = .(cube.symbol, stck)
    ][, first.half := ifelse(tag == 0, 1, 0)
    ][, second.half := ifelse(tag == 1, 1, 0)
    ][, loss := ifelse(gain == 1, 0, 1)
    ][, ishold := ifelse(issale == 1, 0, 1)
    ][, hldt.ls.7 := ifelse(hold.time < 7, 1, 0)
    ][, hold.time.7 := ifelse(hldt.ls.7 == 1, hold.time, 8)
    ][isgain == 'gain' & first.half == 1, state := "pre-follow-gain"
    ][isgain == 'loss' & first.half == 1, state := "pre-follow-loss"
    ][isgain == 'gain' & second.half == 1, state := "post-follow-gain"
    ][isgain == 'loss' & second.half == 1, state := "post-follow-loss"]

f.early <- f.surv.flw[first.half == 1]
f.late <- f.surv.flw[second.half == 1]

f.cube.early <- f.early[, .(cube.symbol = unique(cube.symbol))]
f.cube.late <- f.late[, .(cube.symbol = unique(cube.symbol))]
f.cube <- f.cube.early[f.cube.late, on = "cube.symbol", nomatch = 0]
f.main <- f.surv.flw[f.cube, on = "cube.symbol", nomatch = 0]
sv(f.cube)
rm(f.cube.early, f.cube.late, f.early, f.late)

# filter the cubes that both have the trades records in pre-follow and post-follow
f.ret.cube <- f.main[, .(follow.date = unique(follow.date)), by = .(cube.symbol)]
f.ret <- f.ret.sp[f.ret.cube, on = .(cube.symbol)]

# clean the ret data
f.ret <- f.ret[!is.na(value)]
f.ret[, follow.date := as.Date(follow.date)
    ][, post.follow := ifelse(date < follow.date, 0, 1)
    ][, follow.period := ifelse(post.follow == 1, date - follow.date, follow.date - date)
    ][, avrg.ret := mean(value), by = .(cube.symbol, post.follow)]


# draw density plot
dnsty.plot <- f.ret[, .(avrg.ret = unique(avrg.ret)), by = .(cube.symbol, post.follow)]
dnsty.plot[, post.follow := ifelse(post.follow == 1, "post.follow", "pre.follow")]
dnsty.plot[, min(avrg.ret)]
ggplot(dnsty.plot, aes(x = avrg.ret, colour = post.follow, fill = post.follow)) +
    geom_line(stat = "density", size = 0.5)
    theme_grey() +
    scale_colour_manual(values = c("#CC6666", "#7777DD")) +
    labs(x = paste("\u03B2", "value", collapse = " "), y = "Density") +
    scale_fill_manual(values = c("#CC6666", "#7777DD"),
                                name = "Stage",
                                breaks = c("post.follow", "pre.follow"),
                                labels = c("post-follow", "pre-follow")) +
                                theme(
            axis.title.x = element_text(size = 24, margin = margin(t = 20, r = 0, b = 20, l = 0)),
            axis.title.y = element_text(size = 24, margin = margin(t = 0, r = 20, b = 0, l = 20)),
            axis.text = element_text(size = 24),
            panel.border = element_rect(linetype = 1, fill = NA),
            legend.title = element_blank(),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.text = element_text(size = 24),
            legend.key = element_rect(size = 0.5, colour = "black", fill = "white"),
            legend.key.size = unit(1, 'cm'),
            legend.spacing.x = unit(0.5, 'cm'),
            legend.spacing.y = unit(2, 'cm'),
            legend.box = "horizontal",
            legend.box.background = element_rect(size = 1, colour = "black", fill = "white")
                            )

# T-test
dnsty.plot[, t.test(avrg.ret ~ post.follow)]

# Regression
f.ret[, lm(avrg.ret ~ post.follow)] %>% summary()