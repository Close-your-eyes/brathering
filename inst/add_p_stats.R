library(FSA)
library(ggpubr)
library(dplyr)
library(ggplot2)

data <- ToothGrowth
data$dose <- as.factor(data$dose)


dunn <- dunnTest(len ~ dose, data = data, method = "bonferroni")
dunn$res
stat.test <- dunn$res %>%
  mutate(
    group1 = sub(" -.*", "", Comparison),
    group2 = sub(".*- ", "", Comparison),
    p.adj = P.adj
  )

stat.test$y.position <- c(35, 37, 35)  # adjust based on your plot
stat.test$p.adj.signif <- stats::symnum(stat.test$p.adj, cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, Inf), symbols = c("****", "***", "**", "*", "n.s."))



p <- ggplot(data, aes(x = dose, y = len)) +
  geom_boxplot()

p + stat_pvalue_manual(
  stat.test,
  label = "p.adj.signif",
  tip.length = 0.01
)


pairwilc <- pairwise.wilcox.test(data$len, data$dose, p.adjust.method = "bonferroni")
pairwilc <- broom::tidy(pairwilc)
pairwilc$y.position <- c(35, 37, 35)  # adjust based on your plot
pairwilc$p.adj.signif <- stats::symnum(stat.test$p.adj, cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, Inf), symbols = c("****", "***", "**", "*", "n.s."))

p + stat_pvalue_manual(
  pairwilc,
  label = "p.adj.signif",
  tip.length = 0.05
)

pairwilc |> rstatix::add_y_position()

