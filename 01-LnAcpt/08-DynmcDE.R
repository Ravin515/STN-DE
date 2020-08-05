styleer::ld(f.main)
library(Matrix)
library(lfe)

f.rbst1.early <- f.main[first.half == 1]
f.rbst1.late <- f.main[second.half == 1]

#rm(f.cube.early, f.cube.late, f.cube, f.rbst1, f.rbst1.early, f.rbst1.late)
# Cox Regression
num.early <- f.rbst1.early[, rows := .N, by = .(cube.symbol)
    ][rows > 1, .SD][["rows"]] %>% unique() %>% cumsum()
DEbeta.e <- vector(mode = "numeric")
for (i in 1:length(num.early)) {
    if (i == 1) {
        DEbeta.e[i] <- coxph(Surv(hold.time, issale == 1) ~ gain, data = f.rbst1.early[1:num.early[1]]) %>% coef()
    } else {
        DEbeta.e[i] <- coxph(Surv(hold.time, issale == 1) ~ gain, data = f.rbst1.early[(num.early[i - 1] + 1):num.early[i]]) %>% coef()
    }
}

num.late <- f.rbst1.late[, rows := .N, by = .(cube.symbol)
    ][rows > 1, .SD][["rows"]] %>% unique() %>% cumsum()
DEbeta.l <- vector(mode = "numeric")
for (i in 1:length(num.late)) {
    if (i == 1) {
        DEbeta.l[i] <- coxph(Surv(hold.time, issale == 1) ~ gain, data = f.rbst1.late[1:num.late[1]]) %>% coef()
    } else {
        DEbeta.l[i] <- coxph(Surv(hold.time, issale == 1) ~ gain, data = f.rbst1.late[(num.late[i - 1] + 1):num.late[i]]) %>% coef()
    }
}

DEbeta.e <- data.table(DE = DEbeta.e)
DEbeta.e <- DEbeta.e[!is.na(DE), .SD
    ][, Stage := "pre-follow"]
DEbeta.l <- data.table(DE = DEbeta.l)
DEbeta.l <- DEbeta.l[!is.na(DE), .SD
    ][, Stage := "post-follow"]
ggDE <- rbindlist(list(DEbeta.e, DEbeta.l), fill = T)

# Calculate DE of every individual before and after following
DEbeta.e <- f.rbst1.early[, .(pre.follow = glm(issale ~ gain, family = binomial) %>% coef()), by = .(cube.symbol)
    ][, .SD[2], by = .(cube.symbol)]
DEbeta.l <- f.rbst1.late[, .(post.follow = glm(issale ~ gain, family = binomial) %>% coef()), by = .(cube.symbol)
    ][, .SD[2], by = .(cube.symbol)]
DEbeta <- DEbeta.e[DEbeta.l, on = "cube.symbol"]

# T-test between pre and pro following and plot the comparing DE
#rst.t.test <- DEbeta[!is.na(pre.follow) & !is.na(post.follow), t.test(post.follow, pre.follow)]
ggDE <- melt(DEbeta[!is.na(pre.follow) & !is.na(post.follow)], id.vars = "cube.symbol", measure.vars = c("pre.follow", "post.follow"))
setnames(ggDE, 2:3, c("Stage", "DE"))
ggDE[, Stage := as.character(Stage)
    ][, Stage := ifelse(Stage=="pre.follow", "pre-follow", "post-follow")]

d.ttest <- ggplot(ggDE, aes(x = DE, colour = Stage, fill = Stage)) +
                            geom_line(stat = "density", size = 0.5) +
                            theme_grey() +
                            scale_colour_manual(values = c("#CC6666", "#7777DD")) +
                            labs(x = "Disposition effect", y = "Density") +
                            scale_fill_manual(values = c("#CC6666", "#7777DD"),
                                                        name = "Stage",
                                                        breaks = c("post.follow", "pre.follow"),
                                                        labels = c("Post-follow", "Pre-follow"))+
                            theme(
                                    #axis.title.x = element_text(size = 24, margin = margin(t = 20, r = 0, b = 20, l = 0)),
                                    #axis.title.y = element_text(size = 24, margin = margin(t = 0, r = 20, b = 0, l = 20)),
                                    #axis.text = element_text(size = 24),
                                    #panel.border = element_rect(linetype = 1, fill = NA),
                                    legend.title = element_blank(),
                                    legend.position = "bottom",
                                    #legend.direction = "horizontal",
                                    #legend.text = element_text(size = 24),
                                    #legend.key = element_rect(size = 0.5, colour = "black", fill = "white"),
                                    #legend.key.size = unit(1, 'cm'),
                                    legend.spacing.x = unit(0.1, 'cm'),
                                    legend.spacing.y = unit(2, 'cm'),
                                    #legend.box = "horizontal",
                                    #legend.box.background = element_rect(size = 1, colour = "black", fill = "white")
                                    )
ggsave("Fig3.tiff", device = "tiff", dpi = 300, width = 5, height = 3.5)