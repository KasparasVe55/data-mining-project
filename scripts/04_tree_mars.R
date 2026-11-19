# Regression tree and MARS for life satisfaction
# run 01_clean.R and 03_train_test_split.R first, working directory = repo folder
# input:  data/ess_train.rds (with fold column), data/ess_test.rds
# output: outputs/figures/models/tree_*.pdf, mars_*.pdf, tree_mars_obs_pred.pdf
#         outputs/tables/tree_mars_metrics.csv (test set results)
#         outputs/predictions/tree_mars_test.csv (test predictions, for comparing all models)
#         data/tree_mars_models.rds (fitted models, not uploaded)

library(dplyr)
library(caret)
library(rpart)
library(rpart.plot)
library(earth)

train <- readRDS("data/ess_train.rds")
test  <- readRDS("data/ess_test.rds")

# same predictors as in 03_train_test_split.R
predictors <- c("agea", "female", "hincfel", "activity_grp", "health", "sclmeet",
                "partner", "trstprl", "rlgdgr", "eisced", "domicil", "russian_home",
                "inprdsc", "sclact", "hhmmb", "ppltrst", "stfeco", "stfhlth",
                "aesfdrk", "ever_unemployed", "cntry", "round")

# stflife ~ agea + female + ... (only the predictors, not fold, imputed, weights)
f <- reformulate(predictors, response = "stflife")

dir.create("outputs/figures/models", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs/tables", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs/predictions", showWarnings = FALSE, recursive = TRUE)


# 1. CV folds
# use the folds from 03_train_test_split.R, so all models are tuned on the same folds
# index = rows used for fitting in each fold (all rows except that fold)
folds <- sort(unique(train$fold))
index <- lapply(folds, function(k) which(train$fold != k))
names(index) <- paste0("Fold", folds)

ctrl <- trainControl(method = "cv", index = index)


# 2. regression tree (CART)
# cp = how much the tree is pruned, chosen by CV
# warning "missing values in resampled performance measures" is ok: with the
# largest cp the tree has no splits, predicts one value and R2 can't be computed
set.seed(2026)
fit_tree <- train(f, data = train,
                  method = "rpart",
                  weights = train$anweight,
                  tuneLength = 20,
                  trControl = ctrl)

fit_tree
fit_tree$bestTune
fit_tree$finalModel                    # the tree step by step

pdf("outputs/figures/models/tree_cv.pdf", width = 7, height = 5)
print(plot(fit_tree))                  # CV error for each cp
dev.off()

pdf("outputs/figures/models/tree_plot.pdf", width = 10, height = 7)
rpart.plot(fit_tree$finalModel)
dev.off()

pdf("outputs/figures/models/tree_varimp.pdf", width = 7, height = 6)
print(plot(varImp(fit_tree), top = 15))
dev.off()


# 3. MARS
# degree = 1 no interactions, 2 = pairs of variables can interact
# nprune = number of terms kept after backward pruning
# interactions are allowed only between country/round and another predictor
# (e.g. health x Latvia), to check if the same factors matter in each country and round
# fitted without weights: earth with weights takes more than 9 min for one fit
# (hours for the tuning), without weights a few seconds. country and round are
# predictors, and the test results below are still weighted
set.seed(2026)
mars_grid <- expand.grid(degree = 1:2, nprune = 2:30)

# rule for earth: single terms always allowed, a product only if exactly
# one side is a country or round column (so not health x income, not LV x R11)
allowed_cr <- function(degree, pred, parents, namesx, first) {
  if (degree < 2) return(TRUE)
  is_cr <- grepl("^cntry|^round", namesx)
  xor(is_cr[pred], any(is_cr[parents != 0]))
}

fit_mars <- train(f, data = train,
                  method = "earth",
                  tuneGrid = mars_grid,
                  allowed = allowed_cr,
                  trControl = ctrl)

fit_mars
fit_mars$bestTune
summary(fit_mars$finalModel)           # hinge terms h(), knots = where lines bend

pdf("outputs/figures/models/mars_cv.pdf", width = 7, height = 5)
print(plot(fit_mars))                  # CV error for each degree and nprune
dev.off()

pdf("outputs/figures/models/mars_varimp.pdf", width = 7, height = 6)
print(plot(varImp(fit_mars), top = 15))
dev.off()

# effect of each variable used by MARS, others held at typical values
# (the age plot shows where satisfaction bends)
pdf("outputs/figures/models/mars_effects.pdf", width = 10, height = 8)
plotmo(fit_mars$finalModel)
dev.off()

# residual checks: model selection, residuals vs fitted, cumulative residuals, QQ plot
# no strong pattern in residuals vs fitted = ok
pdf("outputs/figures/models/mars_residuals.pdf", width = 9, height = 8)
plot(fit_mars$finalModel)
dev.off()


# 4. results on the test set
# weighted with anweight, like the report
wmetrics <- function(obs, pred, w) {
  c(RMSE     = sqrt(sum(w * (obs - pred)^2) / sum(w)),
    MAE      = sum(w * abs(obs - pred)) / sum(w),
    Rsquared = 1 - sum(w * (obs - pred)^2) /
                   sum(w * (obs - weighted.mean(obs, w))^2))
}

pred_tree <- predict(fit_tree, newdata = test)
pred_mars <- as.vector(predict(fit_mars, newdata = test))

# predictions should stay within the 0-10 scale
# the tree always does (it predicts group means), MARS lines can go above 10,
# so MARS predictions are cut to 0-10
range(pred_tree)
range(pred_mars)
sum(pred_mars > 10 | pred_mars < 0)
pred_mars <- pmin(pmax(pred_mars, 0), 10)

# observed vs predicted: one box of predictions for each observed answer 0-10
# red line = perfect prediction
pdf("outputs/figures/models/tree_mars_obs_pred.pdf", width = 10, height = 5)
par(mfrow = c(1, 2))
boxplot(pred_tree ~ test$stflife, at = sort(unique(test$stflife)),
        xlim = c(-0.5, 10.5), ylim = c(0, 10),
        xlab = "Observed", ylab = "Predicted", main = "Tree: Obs vs Pred")
abline(0, 1, col = "red", lty = 2)
boxplot(pred_mars ~ test$stflife, at = sort(unique(test$stflife)),
        xlim = c(-0.5, 10.5), ylim = c(0, 10),
        xlab = "Observed", ylab = "Predicted", main = "MARS: Obs vs Pred")
abline(0, 1, col = "red", lty = 2)
par(mfrow = c(1, 1))
dev.off()

metrics <- rbind(Tree = wmetrics(test$stflife, pred_tree, test$anweight),
                 MARS = wmetrics(test$stflife, pred_mars, test$anweight))
metrics <- data.frame(Model = rownames(metrics), round(metrics, 4), row.names = NULL)
print(metrics)

# CV results of the chosen settings, to compare with the test results
fit_tree$results[rownames(fit_tree$bestTune), ]
fit_mars$results[rownames(fit_mars$bestTune), ]


# 5. save
write.csv(metrics, "outputs/tables/tree_mars_metrics.csv", row.names = FALSE)

# test predictions with ids, so the group can join all models on the same rows
predictions <- data.frame(idno     = test$idno,
                          cntry    = test$cntry,
                          essround = test$essround,
                          stflife  = test$stflife,
                          anweight = test$anweight,
                          imputed  = test$imputed,
                          pred_tree = pred_tree,
                          pred_mars = pred_mars)
write.csv(predictions, "outputs/predictions/tree_mars_test.csv", row.names = FALSE)

saveRDS(list(tree = fit_tree, mars = fit_mars), "data/tree_mars_models.rds")
