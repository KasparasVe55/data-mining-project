# Comparison of all models on the test set
# run 03_train_test_split.R first, working directory = repo folder
# input:  data/ess_test.rds, outputs/predictions/*_test.csv (one file per model,
#         columns idno, cntry, essround and pred_<model>)
# output: outputs/tables/model_comparison.csv (weighted test metrics)
#         outputs/tables/model_pairwise.csv (bootstrap differences between models)
#         outputs/figures/models/model_comparison.pdf
# metrics are weighted with anweight, the same as in the model scripts

library(dplyr)

dir.create("outputs/tables", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs/figures/models", showWarnings = FALSE, recursive = TRUE)


# 1. join the predictions of all models
# idno repeats between countries and rounds, so all three columns are the id
key  <- c("idno", "cntry", "essround")
test <- readRDS("data/ess_test.rds")[, c(key, "stflife", "anweight")]
test$cntry <- as.character(test$cntry)

for (file in list.files("outputs/predictions", pattern = "_test\\.csv$", full.names = TRUE)) {
  p <- read.csv(file)
  test <- merge(test, p[, c(key, grep("^pred_", names(p), value = TRUE))], by = key)
}

pred_cols <- grep("^pred_", names(test), value = TRUE)

# lasso_1se (simpler lasso, one-standard-error rule) is only a check in
# 05_lasso.R, so it is not compared here
pred_cols <- setdiff(pred_cols, "pred_lasso_1se")

# every model must predict all test rows, otherwise the comparison is not fair
stopifnot(nrow(test) == 2537, !anyNA(test[pred_cols]))
pred_cols


# 2. weighted test metrics for each model
# R2 = 1 - SSE / SST, all sums weighted with anweight
metrics <- function(y, p, w) {
  c(RMSE     = sqrt(sum(w * (y - p)^2) / sum(w)),
    MAE      = sum(w * abs(y - p)) / sum(w),
    Rsquared = 1 - sum(w * (y - p)^2) / sum(w * (y - weighted.mean(y, w))^2))
}

# rows = metrics, columns = models
score <- function(i) {
  sapply(pred_cols, function(m) metrics(test$stflife[i], test[[m]][i], test$anweight[i]))
}

estimates <- score(seq_len(nrow(test)))

performance <- data.frame(Model = sub("^pred_", "", pred_cols),
                          round(t(estimates), 4), row.names = NULL)
performance <- performance[order(performance$RMSE), ]
print(performance, row.names = FALSE)


# 3. paired bootstrap (adapted from the regression lab, with weights)
# draw the test rows with replacement, the SAME rows for all models,
# and compute the metrics again; differences between models show how
# much of the difference could be chance
B <- 9999
set.seed(2026)
n <- nrow(test)

boot_scores <- array(NA_real_, dim = c(3, length(pred_cols), B),
                     dimnames = list(rownames(estimates), pred_cols, NULL))
for (b in seq_len(B)) {
  i <- sample.int(n, size = n, replace = TRUE)
  boot_scores[, , b] <- score(i)
}

# all pairs of models, differences are Model_A minus Model_B
pairs  <- combn(seq_along(pred_cols), 2)
output <- list()

for (j in seq_len(ncol(pairs))) {
  a <- pairs[1, j]
  b <- pairs[2, j]

  for (k in rownames(estimates)) {
    difference      <- estimates[k, a] - estimates[k, b]
    boot_difference <- boot_scores[k, a, ] - boot_scores[k, b, ]

    # 95% percentile interval, if it does not include 0 the difference is real
    ci <- quantile(boot_difference, probs = c(0.025, 0.975), names = FALSE)

    # two-sided test, bootstrap differences centred at 0 (null hypothesis)
    null_difference <- boot_difference - difference
    p_value <- (1 + sum(abs(null_difference) >= abs(difference))) / (B + 1)

    output[[length(output) + 1]] <- data.frame(
      Model_A    = sub("^pred_", "", pred_cols[a]),
      Model_B    = sub("^pred_", "", pred_cols[b]),
      Metric     = k,
      Difference = difference,
      CI_lower   = ci[1],
      CI_upper   = ci[2],
      p_value    = p_value
    )
  }
}

pairwise <- do.call(rbind, output)

# Holm correction: many pairs are tested, so p-values are adjusted
# within each metric
pairwise$p_Holm <- ave(pairwise$p_value, pairwise$Metric,
                       FUN = function(p) p.adjust(p, method = "holm"))
pairwise$Significant <- pairwise$p_Holm < 0.05

print(subset(pairwise, Metric == "RMSE"), digits = 3, row.names = FALSE)

# significant differences only
subset(pairwise, Significant)


# 4. figure: test RMSE of each model with 95% bootstrap interval
rmse_ci <- t(apply(boot_scores["RMSE", , ], 1, quantile, probs = c(0.025, 0.975)))
ord     <- order(estimates["RMSE", ], decreasing = TRUE)

pdf("outputs/figures/models/model_comparison.pdf", width = 7, height = 4.5)
par(mar = c(5, 8, 2, 1))
plot(estimates["RMSE", ord], seq_along(ord), xlim = range(rmse_ci), pch = 16,
     yaxt = "n", xlab = "Test RMSE (weighted), 95% bootstrap interval", ylab = "",
     main = "Model comparison on the test set")
segments(rmse_ci[ord, 1], seq_along(ord), rmse_ci[ord, 2], seq_along(ord))
axis(2, at = seq_along(ord), labels = sub("^pred_", "", pred_cols[ord]), las = 1)
dev.off()


# 5. save
write.csv(performance, "outputs/tables/model_comparison.csv", row.names = FALSE)
write.csv(pairwise, "outputs/tables/model_pairwise.csv", row.names = FALSE)
