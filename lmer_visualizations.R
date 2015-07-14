rm(list=ls())
library(ggplot2)
library(lme4)

# attesteds

load("/Users/markmyslin/Dropbox/274 blends paper/stats/saved-models/attesteds/m18.fixed.Rdata")

m <- m18.fixed


estimate <- fixef(m)
label <- names(estimate)
 label <- c("(Intercept)", "Other source guessed", "Partial string matches", "Frequency ratio", "Source letters retained", "Percentage of source retained", "Source avg orthographic prob", "Source min orthographic prob", "NDR activation", "Source frequency", "Source is second")

label <- as.factor(label)
 
 levels(label)
 
 #label = factor(label,levels(label)[c(11,5,10,7,2,4,9,8,6,3,1)])
 label = factor(label,levels(label)[c(3,9, 4, 11, 7, 2, 5, 8, 10, 6,1)])
 




#se <- se.coef(m)$fixef
se <- coef(summary(m))[,2]

low <- estimate-2*se
high <- estimate+2*se
dat <- data.frame(estimate, label, low, high)

dat$p <- coef(summary(m))[,"z value"]
dat$sig <- ifelse(abs(dat$p)<2, "ns", "sig")

# dat <- dat[c(9,1,2,3,4,5,6,7,8,10,11),]

cbgColourPalette <- scale_colour_manual(values=c("#bcbcbc", "#000000",  "#999999", "#E69F00", "#56B4E9","#F0E442", "#CC79A7"))

 pdf(file="~/Dropbox/274 blends paper/presentations/figures/lmer-visualization/attestedlmer.pdf",
            paper="special",
            width=8,
            height=6)

ggplot(dat, aes(x=label, y=estimate, color=sig, shape=sig)) + geom_point(size=1) + geom_pointrange(aes(ymin=low, ymax=high), size=1.5, stat="identity") + coord_flip() + geom_hline(aes(yintercept=0)) + scale_y_continuous(expression(atop("Parameter estimate", bold(" ")))) + scale_x_discrete("Predictor") + theme(axis.title.y= element_text(size=12, angle=90, hjust=10), axis.title.x= element_text(size=12, hjust=0.49), axis.text.y=element_text(size=18)) +cbgColourPalette + theme(legend.position="none")

dev.off()


# novels

rm(list=ls())

load("/Users/markmyslin/Dropbox/274 blends paper/stats/saved-models/novels/m8.Rdata")

m <- m8

estimate <- fixef(m)
label <- names(estimate)

label <- c("(Intercept)", "Other source guessed", "(^) Partial string matches", "Frequency ratio", "Source letters retained", "Percentage of source retained", "(^) Source avg orthographic prob", "Source min orthographic prob", "NDR activation", "(^) Source frequency", "(^) Source occurs second")

label <- as.factor(label)
 
 levels(label)
 
 label = factor(label,levels(label)[c(7,4,8,11,2, 6,1,3,10, 9, 5)])
 


#se <- se.coef(m)$fixef
se <- coef(summary(m))[,2]

low <- estimate-2*se
high <- estimate+2*se
dat <- data.frame(estimate, label, low, high)




dat$p <- coef(summary(m))[,"z value"]
dat$sig <- ifelse(abs(dat$p)<2, "ns", "sig")

# dat <- dat[c(9,1,2,3,4,5,6,7,8,10,11),]

cbgColourPalette <- scale_colour_manual(values=c("#bcbcbc", "#000000",  "#999999", "#E69F00", "#56B4E9","#F0E442", "#CC79A7"))

 pdf(file="~/Dropbox/274 blends paper/presentations/figures/lmer-visualization/novellmer.pdf",
           paper="special",
           width=8,
           height=6)

ggplot(dat, aes(x=label, y=estimate, color=sig, shape=sig)) + geom_point(size=1) + geom_pointrange(aes(ymin=low, ymax=high), size=1.5, stat="identity") + coord_flip() + geom_hline(aes(yintercept=0)) + scale_y_continuous(expression(atop("Parameter estimate", bold(" ")))) + scale_x_discrete("Predictor") + theme(axis.title.y= element_text(size=12, angle=90, hjust=10), axis.title.x= element_text(size=12, hjust=0.49), axis.text.y=element_text(size=18)) +cbgColourPalette + theme(legend.position="none")


dev.off()
