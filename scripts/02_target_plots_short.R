# Graphs for life satisfaction (stflife)
# run scripts/01_clean.R first, working directory = repo folder
# graphs are saved to outputs/figures/target/

library(dplyr)
library(ggplot2)
library(gridExtra)
library(corrplot)

source("scripts/survey_design.R")

out_dir <- "outputs/figures/target"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

dy <- d %>% filter(!is.na(stflife))

# for hincfel, health and aesfdrk a higher number is worse
num_vars <- c(
  agea    = "Age",
  eisced  = "Education (1-7)",
  hincfel = "Income feeling (1 good - 4 bad)",
  health  = "Health (1 very good - 5 very bad)",
  sclmeet = "Meets friends (1 never - 7 daily)",
  inprdsc = "People to talk to (0-6)",
  sclact  = "Social activity (1-5)",
  hhmmb   = "Household size",
  ppltrst = "Trust in people (0-10)",
  trstprl = "Trust in parliament (0-10)",
  stfeco  = "Satisfaction with economy (0-10)",
  stfhlth = "Health services (0-10)",
  aesfdrk = "Unsafe after dark (1-4)",
  rlgdgr  = "Religiosity (0-10)"
)


# 1. Distribution with mean and median
m  <- coef(svymean(~stflife, des, na.rm = TRUE))
md <- coef(svyquantile(~stflife, des, 0.5, na.rm = TRUE))

g1 <- ggplot(dy, aes(x = stflife, y = after_stat(prop), weight = anweight)) +
  geom_bar(fill = "grey70") +
  geom_vline(xintercept = m,  colour = "red") +
  geom_vline(xintercept = md, colour = "blue", linetype = "dashed") +
  scale_x_continuous(breaks = 0:10) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Life satisfaction (0-10)", y = "Share of population",
       caption = paste0("Red line = mean (", round(m, 2),
                        "), blue dashed line = median (", md, ")")) +
  theme_bw()
ggsave(file.path(out_dir, "1_distribution.pdf"), g1, width = 6, height = 4)


# 2. Distribution by country with mean and median
mc <- data.frame(
  cntry  = factor(levels(d$cntry), levels = levels(d$cntry)),
  mean   = coef(svyby(~stflife, ~cntry, des, svymean, na.rm = TRUE)),
  median = coef(svyby(~stflife, ~cntry, des, svyquantile, quantiles = 0.5, na.rm = TRUE))
)

g2 <- ggplot(dy, aes(x = stflife, y = after_stat(prop), weight = anweight)) +
  geom_bar(fill = "grey70") +
  geom_vline(data = mc, aes(xintercept = mean), colour = "red", inherit.aes = FALSE) +
  geom_vline(data = mc, aes(xintercept = median), colour = "blue",
             linetype = "dashed", inherit.aes = FALSE) +
  facet_wrap(~cntry) +
  scale_x_continuous(breaks = 0:10) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Life satisfaction (0-10)", y = "Share of population",
       caption = "Red line = mean, blue dashed line = median") +
  theme_bw()
ggsave(file.path(out_dir, "2_stflife_distribution.pdf"), g2, width = 9, height = 3.5)


# 3. Mean + 95% CI by categorical variables
mean_plot <- function(by_var, title) {
  r <- as.data.frame(svyby(~stflife, by_var, des, svymean, na.rm = TRUE))
  names(r)[1] <- "group"
  ggplot(r, aes(x = factor(group), y = stflife)) +
    geom_pointrange(aes(ymin = stflife - 1.96 * se, ymax = stflife + 1.96 * se)) +
    labs(title = title, x = NULL, y = "Mean life satisfaction") +
    theme_bw()
}

c1 <- mean_plot(~activity_grp,    "Main activity")
c2 <- mean_plot(~partner,         "Lives with partner (0 no, 1 yes)")
c3 <- mean_plot(~russian_home,    "Russian at home (0 no, 1 yes)")
c4 <- mean_plot(~ever_unemployed, "Ever unemployed (0 no, 1 yes)")
c5 <- mean_plot(~domicil,         "Place (1 big city - 5 farm)")

ggsave(file.path(out_dir, "3_means_categorical.pdf"),
       arrangeGrob(c1, c2, c3, c4, c5, ncol = 3), width = 11, height = 6)


# 4. Mean of stflife for each value of x (binned scatterplots)
# if the points follow the red line, the relationship is linear
binned_plot <- function(v) {
  r <- as.data.frame(svyby(~stflife, as.formula(paste("~", v)), des, svymean, na.rm = TRUE))
  names(r)[1] <- "x"
  ggplot(r, aes(x = x, y = stflife)) +
    geom_pointrange(aes(ymin = stflife - 1.96 * se, ymax = stflife + 1.96 * se), size = 0.1) +
    geom_line() +
    geom_smooth(method = "lm", se = FALSE, colour = "red", linetype = "dashed") +
    labs(x = num_vars[[v]], y = "Mean life satisfaction") +
    theme_bw(base_size = 8)
}

binned_list <- lapply(names(num_vars), binned_plot)
ggsave(file.path(out_dir, "4_scatter_binned.pdf"),
       arrangeGrob(grobs = binned_list, ncol = 4), width = 10, height = 9)


# 5. Correlation matrix (Spearman, unweighted)
corr_vars <- c("stflife", names(num_vars),
               "female", "russian_home", "partner", "ever_unemployed")
M <- cor(d[, corr_vars], use = "pairwise.complete.obs", method = "spearman")

pdf(file.path(out_dir, "5_correlation_matrix.pdf"), width = 9, height = 9)
corrplot(M, method = "color", type = "upper", order = "hclust",
         addCoef.col = "black", number.cex = 0.55,
         tl.col = "black", tl.srt = 45, diag = FALSE)
dev.off()


# 6. Mean by age group, country and round
m6 <- as.data.frame(svyby(~stflife, ~cntry + age5 + round, des, svymean, na.rm = TRUE))

g6 <- ggplot(m6, aes(x = age5, y = stflife, colour = round, group = round)) +
  geom_pointrange(aes(ymin = stflife - 1.96 * se, ymax = stflife + 1.96 * se),
                  position = position_dodge(0.4), size = 0.3) +
  geom_line(position = position_dodge(0.4)) +
  facet_wrap(~cntry) +
  labs(x = "Age group", y = "Mean life satisfaction", colour = "Round") +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave(file.path(out_dir, "6_by_age_country_round.pdf"), g6, width = 8, height = 3.8)
