ld(f.surv.flw)
library(Matrix)
library(lfe)
f.surv.flw[, tag := ifelse(date - as.Date(follow.date) > 0, 1, 0), keyby = .(cube.symbol, stck)
    ][, first.half := ifelse(tag == 0, 1, 0)
    ][, second.half := ifelse(tag == 1, 1, 0)
    ][, loss := ifelse(gain == 1, 0, 1)
    ][, ishold := ifelse(issale == 1, 0, 1)] # sign the two-stage and loss&hold data

# As the robust test in rbst1, selecting the individuals before and after follow both have trade-offs.
f.surv.early <- f.surv.flw[first.half == 1]
f.surv.late <- f.surv.flw[second.half == 1]

f.cube.early <- f.surv.early[, .(cube.symbol = unique(cube.symbol))]
f.cube.late <- f.surv.late[, .(cube.symbol = unique(cube.symbol))]
f.cube <- f.cube.early[f.cube.late, on = "cube.symbol", nomatch = 0]

f.rbst1 <- f.surv.flw[f.cube, on = "cube.symbol", nomatch = 0]
f.rbst1.early <- f.rbst1[first.half == 1]
f.rbst1.late <- f.rbst1[second.half == 1]

#rm(f.cube.early, f.cube.late, f.cube, f.rbst1, f.rbst1.early, f.rbst1.late)

# Calculate DE of every individual before and after following
DEbeta.e <- f.rbst1.early[, .(pre.follow = lm(issale ~ gain) %>% coef()), by = .(cube.symbol)
    ][, .SD[2], by = .(cube.symbol)]
DEbeta.l <- f.rbst1.late[, .(post.follow = lm(issale ~ gain) %>% coef()), by = .(cube.symbol)
    ][, .SD[2], by = .(cube.symbol)]
DEbeta <- DEbeta.e[DEbeta.l, on = "cube.symbol"]

# T-test between pre and pro following and plot the comparing DE
rst.t.test <- DEbeta[!is.na(pre.follow) & !is.na(post.follow), t.test(post.follow, pre.follow)]
ggDE <- melt(DEbeta[!is.na(pre.follow) & !is.na(post.follow)], id.vars = "cube.symbol", measure.vars = c("pre.follow", "post.follow"))
setnames(ggDE, 2:3, c("Stage", "DE"))
ggDE[, Stage := as.character(Stage)
    ][, Stage := ifelse(Stage=="pre.follow", "pre-follow", "post-follow")]

d.ttest <- ggplot(ggDE, aes(x = DE, colour = Stage, fill = Stage)) +
    geom_line(stat = "density", size = 2) +
    theme_grey() +
    scale_colour_manual(values = c("#CC6666", "#7777DD")) +
    labs(x = paste("\u03B2", "value", collapse = " "), y = "Density") +
    scale_fill_manual(values = c("#CC6666", "#7777DD"),
                                name = "Stage",
                                breaks = c("post.follow", "pre.follow"),
                                labels = c("post-follow", "pre-follow"))+
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
ggsave(d.ttest, device = "eps")