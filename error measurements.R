## Inter- and intra-observer error for 2018 SRMP macaque study
library(reshape)
library(ggplot2)
library(ggpubr)
library(doBy)

dat <- read.csv("~/Downloads/measurements - Sheet1.csv", header = TRUE)

## average Euclidean distance within observers =================================

AKB <- dat[dat$user == "AKB",]
# isolate AJS trials

euclid <- function(x) { mean(c(dist(x))) }

AKB_11090 <- sapply(AKB[AKB$spec_id=="AMNH_11090", 4:16], euclid)
AJS_11090 <- sapply(AJS[AJS$spec_id=="AMNH_11090", 4:16], euclid)

# do the same for the other specimen 

# make a data.frame with all four vectors, one for each user and specimen
# (2 specimens x 2 users) - hint: use data.frame() to make it a
# data.frame instead of a matrix
dist <- data.frame()

# add the users and specimen ids to each row 
dist$users 

dist$spec_id 

# make sure the names of our data frame aren't funny - do you have to fix them
# to be more reasonable? 

# use melt() to reshape our data so we can analyze it better
dist2 <- melt(dist, id = c("user", "spec_id"))

# use ggplot to make a nice plot of the measurements by user, for each specimen
p_11090<- ggplot(dist2[dist2$spec_id == "AMNH_11090",], aes(x = variable, y = value, group = user)) + 
  geom_hline(color = "black", aes(yintercept = 1)) + 
  geom_point(aes(color = user), size = 3) + 
  geom_line(aes(color = user)) + theme_classic() + 
  labs(x = NULL, y = "mean distance")

p_87264<- ggplot(dist2[dist2$spec_id == "AMNH_87264",], aes(x = variable, y = value, group = user)) + 
  geom_hline(color = "black", aes(yintercept = 1)) + 
  geom_point(aes(color = user), size = 3) + 
  geom_line(aes(color = user)) + theme_classic() + 
  labs(x = NULL, y = "mean distance")

ggarrange(p_11090, p_87264, nrow = 2, labels = c("AMNH 11090", "AMNH 87264"))

# ANOVA between users =========================================================
long <- melt(dat, id = c("trial", "user", "spec_id"))

# 2-way ANOVA to assess interobserver error
inter_mod <- aov(value ~ spec_id + user, data = long)
summary.aov(inter_mod)

# interobserver comparisons across measurements ===============================

summ <- summaryBy(value ~ user + spec_id + variable, data = long, FUN = c(length, mean, sd, range))
colnames(summ)[4:8] <- c("N", "mean", "sd", "min", "max")
summ$se <- summ$sd / sqrt(summ$N)

ggplot(summ[summ$spec_id == "AMNH_87264",], aes(x = variable, y = mean, group = user, color = user)) + 
  geom_errorbar(aes(ymin= mean - se, ymax= mean + se), width = 0.2, size = 1) + 
  geom_line() + 
  geom_point(size = 2)

# PCA by observer =============================================================

pc <- prcomp(prep[,4:16], scale.=TRUE, center=TRUE)

pc.scores <- cbind(prep[,1:3], data.frame(pc$x))

# make a plot of the scores on PC1 and PC2 
ggplot(pc.scores)
